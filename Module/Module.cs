using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.Nodes;
using Type = Osprey.Members.Type;

namespace Osprey
{
	/// <summary>
	/// Represents an Osprey module.
	/// </summary>
	public sealed partial class Module
	{
		public Module(ModulePool pool, string name, Version version)
			: this(pool, name, version, MaxFileFormatVersion, false, false)
		{ }
		private Module(ModulePool pool, string name, Version version, uint fileFormatVersion, bool imported, bool fromVersionedFile)
		{
			this.pool = pool;
			this.name = name;
			this.version = version;
			this.fileFormatVersion = fileFormatVersion;
			this.imported = imported;

			this.members = new MemberDefsAndRefs(imported);
			this.membersByFullName = new Dictionary<string, NamedMember>();
			if (!imported)
			{
				this.methodBlock = new MemoryStream(2048); // Initialize to 2k capacity
				this.methodBodies = new Dictionary<byte[], uint>(MethodBlockComparer.Instance);
				this.metadata = new Dictionary<string, string>();
			}
			else
			{
				this.explicitlyImported = !fromVersionedFile &&
					name != null &&
					pool != null &&
					pool.Compiler != null &&
					pool.Compiler.ImportsModule(name);
			}
		}

		private bool fullyLoaded = false;
		/// <summary>
		/// Gets a value determining whether the module is fully loaded.
		/// </summary>
		internal bool FullyLoaded { get { return fullyLoaded; } }

		private string name;
		/// <summary>
		/// Gets the name of the module.
		/// </summary>
		public string Name { get { return name; } }

		private Version version;
		/// <summary>
		/// Gets the version of the module.
		/// </summary>
		/// <remarks>The module that's being compiled has version 1.0.0.0 unless specified on the command line.</remarks>
		public Version Version { get { return version; } }

		private uint fileFormatVersion;
		/// <summary>
		/// Gets the file format version number used by this module.
		/// </summary>
		public uint FileFormatVersion { get { return fileFormatVersion; } }

		private bool imported;
		/// <summary>
		/// Gets a value indicating whether the module was imported from an external file.
		/// If false, this is the module that is being compiled.
		/// </summary>
		public bool Imported { get { return imported; } }

		/// <summary>
		/// If this field is true, there is a use directive in the source code that
		/// explicitly imports this module. Otherwise, this module was imported as
		/// a dependency of some other module.
		/// </summary>
		private bool explicitlyImported;

		private MemberDefsAndRefs members;
		internal MemberDefsAndRefs Members { get { return members; } }

		private Dictionary<string, NamedMember> membersByFullName;

		private MethodGroup mainMethod;
		/// <summary>
		/// Gets the main method of the module.
		/// </summary>
		public MethodGroup MainMethod { get { return mainMethod; } internal set { mainMethod = value; } }

		private Dictionary<string, string> metadata;
		internal Dictionary<string, string> Metadata
		{
			get { return metadata; }
			set
			{
				if (value == null)
					throw new ArgumentNullException("value");
				metadata = value;
			}
		}

		private string nativeLib;
		internal string NativeLib
		{
			get { return nativeLib; }
			set
			{
				if (value != null && value.Length == 0)
					throw new ArgumentException("Native library name cannot be the empty string. Use null instead.");
				nativeLib = value;
			}
		}

		private ModulePool pool;
		/// <summary>
		/// Gets the <see cref="ModulePool"/> that this module was loaded into.
		/// </summary>
		internal ModulePool Pool { get { return pool; } }

		private MemoryStream methodBlock;
		private Dictionary<byte[], uint> methodBodies;
		/// <summary>
		/// Gets the method block of the module. This is always null for imported methods.
		/// </summary>
		internal MemoryStream MethodBlock { get { return methodBlock; } }

		/// <summary>
		/// Gets a referenced module by ID.
		/// </summary>
		/// <param name="id">The ModuleRef ID.</param>
		/// <returns>The module with the specified ModuleRef ID.</returns>
		public Module GetModule(uint id)
		{
			return members.ModuleRefs[id];
		}

		/// <summary>
		/// References the specified module in the current module.
		/// </summary>
		/// <param name="module">The module to reference.</param>
		/// <returns>The token ID of the ModuleRef.</returns>
		internal uint AddModuleRef(Module module)
		{
			return members.ModuleRefs.GetId(module);
		}

		/// <summary>
		/// Gets a string ID for a string with the specified value. If the string does not exist in the module,
		/// it is added to it, and a new ID is created.
		/// </summary>
		/// <param name="value">The string to get an ID for.</param>
		/// <returns>The ID of the string.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="value"/> is null.</exception>
		/// <exception cref="InvalidOperationException">The module is imported.</exception>
		public uint GetStringId(string value)
		{
			if (imported)
				throw new InvalidOperationException("Cannot access string table of imported module.");
			if (value == null)
				throw new ArgumentNullException("value");

			return members.Strings.GetId(value);
		}

		/// <summary>
		/// Looks up a method by ID.
		/// </summary>
		/// <param name="id">The method ID to look up.</param>
		/// <returns>The method with the specified ID.</returns>
		public MethodGroup GetMethod(uint id)
		{
			var mask = id & MaskMask;
			if (mask == GlobalFuncDefMask)
				return members.GlobalFuncDefs[id];
			if (mask == MethodDefMask)
				return members.MethodDefs[id];
			if (mask == GlobalFuncRefMask)
				return members.GlobalFuncRefs[id];
			if (mask == MethodRefMask)
				return members.MethodRefs[id];

			throw new ArgumentException("Invalid ID kind: must refer to a method or global function.", "id");
		}

		/// <summary>
		/// Gets an ID for the specified method. If the method belongs to a different module, a MethodRef or GlobalFuncRef ID is returned;
		/// otherwise, a MethodDef or GlobalFuncDef ID is returned.
		/// </summary>
		/// <param name="method">The method to get an ID for.</param>
		/// <returns>An ID that represents the method.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="method"/> is null.</exception>
		/// <exception cref="InvalidOperationException">The module is imported.</exception>
		public uint GetMethodId(MethodGroup method)
		{
			if (imported)
				throw new InvalidOperationException("Cannot get method ID from an imported module.");
			if (method == null)
				throw new ArgumentNullException("method");

			if (method.Module != this)
			{
				// Make sure there's a ModuleRef for the method's module
				members.ModuleRefs.GetId(method.Module);

				if (method.Parent is Class)
				{
					// And a TypeRef for the type, too.
					GetTypeId(method.ParentAsClass);
					return method.Id = members.MethodRefs.GetId(method);
				}
				else
					return method.Id = members.GlobalFuncRefs.GetId(method);
			}
			else
			{
				if (method.Parent is Class)
					return method.Id = members.MethodDefs.GetId(method);
				else
					return method.Id = members.GlobalFuncDefs.GetId(method);
			}
		}

		/// <summary>
		/// Gets an ID for the specified type. If the type is from a different module, a TypeRef is returned;
		/// otherwise, a TypeDef is returned.
		/// </summary>
		/// <param name="type">The type to get an ID for.</param>
		/// <returns>The TypeRef or TypeDef ID for the specified type.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="type"/> is null.</exception>
		public uint GetTypeId(Type type)
		{
			if (type == null)
				throw new ArgumentNullException("type");

			if (type.Module != this)
			{
				// Make sure there's a ModuleRef for the type's module
				members.ModuleRefs.GetId(type.Module);
				return type.Id = members.TypeRefs.GetId(type);
			}
			else
				return members.TypeDefs.GetId(type);
		}

		/// <summary>
		/// Gets the type with the specified fully qualified name.
		/// </summary>
		/// <param name="fullName">The fully qualified name of the type to get.</param>
		/// <returns>A pointer to the specified type.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="fullName"/> is null.</exception>
		/// <exception cref="ArgumentException"><paramref name="fullName"/> refers to a member which is not a type.
		/// -or-
		/// The specified type could not be found.</exception>
		public Type GetType(string fullName)
		{
			if (membersByFullName.ContainsKey(fullName))
			{
				var mem = membersByFullName[fullName];
				if (mem is Type)
					return (Type)mem;
				throw new ArgumentException("The specified member is not a type.", "fullName");
			}

			throw new ArgumentException("The specified type could not be found.", "fullName");
		}

		/// <summary>
		/// Looks up a type by ID.
		/// </summary>
		/// <param name="id">The ID of the type to look up.</param>
		/// <returns>The type with the specified ID.</returns>
		public Type GetType(uint id)
		{
			var mask = id & MaskMask;
			if (mask == TypeDefMask)
				return members.TypeDefs[id];
			if (mask == TypeRefMask)
				return members.TypeRefs[id];

			throw new ArgumentException("Invalid ID kind: must be a TypeDef or TypeRef.", "id");
		}

		/// <summary>
		/// Gets an ID for the specified field. If the field comes from another module, a FieldRef ID is returned;
		/// otherwise, a FieldDef ID is returned.
		/// </summary>
		/// <param name="field">The field to get an ID for.</param>
		/// <returns>A FieldRef or FieldDef ID for the field.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="field"/> is null.</exception>
		/// <exception cref="InvalidOperationException">The module is imported.</exception>
		public uint GetFieldId(Field field)
		{
			if (imported)
				throw new InvalidOperationException("Cannot get field ID from an imported module.");
			if (field == null)
				throw new ArgumentNullException("field");

			if (field.Parent.Module != this)
			{
				// Make sure there's a ModuleRef for the field's module
				members.ModuleRefs.GetId(field.Parent.Module);
				return field.Id = members.FieldRefs.GetId(field);
			}
			else
				return field.Id = members.FieldDefs.GetId(field);
		}
		/// <summary>
		/// Gets an ID for the specified class constant.
		/// </summary>
		/// <param name="field">The field to get an ID for.</param>
		/// <returns>The FieldDef ID for the field.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="field"/> is null.</exception>
		/// <exception cref="ArgumentException"><paramref name="field"/> comes from another module.</exception>
		/// <exception cref="InvalidOperationException">The module is imported.</exception>
		public uint GetFieldId(ClassConstant field)
		{
			if (imported)
				throw new InvalidOperationException("Cannot get field ID from an imported module.");
			if (field == null)
				throw new ArgumentNullException("field");
			if (field.Parent.Module != this)
				throw new ArgumentException("Cannot get field ID of imported class constant (must be inlined).");

			return field.Id = members.FieldDefs.GetId(field);
		}
		/// <summary>
		/// Gets an ID for the specified enum field.
		/// </summary>
		/// <param name="field">The field to get an ID for.</param>
		/// <returns>The FieldDef ID for the field.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="field"/> is null.</exception>
		/// <exception cref="ArgumentException"><paramref name="field"/> comes from another module.</exception>
		/// <exception cref="InvalidOperationException">The module is imported.</exception>
		public uint GetFieldId(EnumField field)
		{
			if (imported)
				throw new InvalidOperationException("Cannot get field ID from an imported module.");
			if (field == null)
				throw new ArgumentNullException("field");
			if (field.Parent.Module != this)
				throw new ArgumentException("Cannot get field ID of imported enum field (constants must be inlined).");

			return field.Id = members.FieldDefs.GetId(field);
		}

		public Field GetField(uint id)
		{
			var mask = id & MaskMask;
			if (mask == FieldDefMask)
				return (Field)members.FieldDefs[id];
			if (mask == FieldRefMask)
				return (Field)members.FieldRefs[id];

			throw new ArgumentException("Invalid ID kind: must be a FieldDef or FieldRef.", "id");
		}

		/// <summary>
		/// Gets an ID for the specified global constant. This is always a ConstantDef ID.
		/// </summary>
		/// <param name="constant">The constant to get an ID for.</param>
		/// <returns>The ConstantDef ID for the specified constant.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="constant"/> is null.</exception>
		public uint GetConstantId(GlobalConstant constant)
		{
			if (constant == null)
				throw new ArgumentNullException("constant");

			return constant.Id = members.GlobalConstDefs.GetId(constant);
		}

		/// <summary>
		/// Appends a method body to the end of the method block,
		/// and returns the offset (within the method block) at
		/// which the method body begins.
		/// </summary>
		/// <param name="body">The method body to append.</param>
		/// <returns>The offset within the method block at which the method body begins.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="body"/> is null.</exception>
		internal uint AppendMethodBody(byte[] body)
		{
			if (body == null)
				throw new ArgumentNullException("body");

			uint offset;
			if (!methodBodies.TryGetValue(body, out offset))
			{
				offset = checked((uint)methodBlock.Length);
				methodBlock.Write(body, 0, body.Length);
				methodBodies[body] = offset;
			}

			return offset;
		}

		// Minimum supported file format version number (reading)
		internal const uint MinFileFormatVersion = 0x00000100u;
		// Maximum supported file format version number (reading and writing)
		internal const uint MaxFileFormatVersion = 0x00000100u;

		internal const uint MaskMask          = 0xff000000u;
		internal const uint GlobalConstMask   = 0x02000000u;
		internal const uint GlobalFuncDefMask = 0x04000000u;
		internal const uint TypeDefMask       = 0x10000000u;
		internal const uint FieldDefMask      = 0x12000000u;
		internal const uint MethodDefMask     = 0x14000000u;
		internal const uint StringMask        = 0x20000000u;
		internal const uint ModuleRefMask     = 0x40000000u;
		internal const uint GlobalFuncRefMask = 0x44000000u;
		internal const uint TypeRefMask       = 0x50000000u;
		internal const uint FieldRefMask      = 0x52000000u;
		internal const uint MethodRefMask     = 0x54000000u;

		internal const uint MagicNumber =
			(79u)       | // O
			(86u << 8)  | // V
			(77u << 16) | // M
			(77u << 24);  // M

		internal const string FileExtension = ".ovm";

		internal class MemberDefsAndRefs
		{
			public MemberDefsAndRefs(bool imported)
			{
				ModuleRefs = new MemberTable<Module>(ModuleRefMask);
				TypeDefs = new MemberTable<Type>(TypeDefMask);
				TypeRefs = new MemberTable<Type>(TypeRefMask);
				MethodDefs = new MemberTable<MethodGroup>(MethodDefMask);
				Strings = new MemberTable<string>(StringMask);

				if (!imported)
				{
					GlobalFuncRefs = new MemberTable<MethodGroup>(GlobalFuncRefMask);
					FieldRefs = new MemberTable<Field>(FieldRefMask);
					MethodRefs = new MemberTable<MethodGroup>(MethodRefMask);

					GlobalFuncDefs = new MemberTable<MethodGroup>(GlobalFuncDefMask);
					GlobalConstDefs = new MemberTable<GlobalConstant>(GlobalConstMask);
					FieldDefs = new MemberTable<NamedMember>(FieldDefMask);
				}
			}

			public MemberTable<Module> ModuleRefs;

			public MemberTable<Type> TypeDefs;
			public MemberTable<MethodGroup> GlobalFuncDefs;
			public MemberTable<GlobalConstant> GlobalConstDefs;
			public MemberTable<NamedMember> FieldDefs;
			public MemberTable<MethodGroup> MethodDefs;

			public MemberTable<MethodGroup> GlobalFuncRefs;
			public MemberTable<Type> TypeRefs;
			public MemberTable<Field> FieldRefs;
			public MemberTable<MethodGroup> MethodRefs;

			public MemberTable<string> Strings;
		}

		private class MethodBlockComparer : IEqualityComparer<byte[]>
		{
			public bool Equals(byte[] x, byte[] y)
			{
				if (x == null || y == null)
					return x == y;
				if (x.Length != y.Length)
					return false;

				var iMax = x.Length;
				for (var i = 0; i < iMax; i++)
					if (x[i] != y[i])
						return false;

				return true;
			}

			public int GetHashCode(byte[] obj)
			{
				var hash = 0;

				var i = 0;
				var length = obj.Length - 4;
				while (i < length)
				{
					unchecked
					{
						hash ^= obj[i];
						hash ^= 7 * obj[i + 1] << 8;
						hash ^= 13 * obj[i + 2] << 16;
						hash ^= 23 * obj[i + 3] << 24;
					}
					i += 4;
				}

				while (i < obj.Length)
				{
					unchecked
					{
						hash ^= obj[i] << (3 * i % 4);
					}
					i++;
				}

				return hash;
			}

			private static MethodBlockComparer instance;
			public static MethodBlockComparer Instance
			{
				get
				{
					return instance ?? (instance = new MethodBlockComparer());
				}
			}
		}

		[Flags]
		public enum TypeFlags : uint
		{
			Public = 0x01,
			Private = 0x02,
			Abstract = 0x04,
			Sealed = 0x08,
			Static = Abstract | Sealed,
			Primitive = 0x10 | Sealed,
		}

		[Flags]
		public enum FieldFlags : uint
		{
			Public = 0x01,
			Private = 0x02,
			Protected = 0x04,
			Instance = 0x08,
			HasValue = 0x10, // For constant values
		}

		[Flags]
		public enum MethodFlags : uint
		{
			Public = 0x01,
			Private = 0x02,
			Protected = 0x04,
			Instance = 0x08,
			Ctor = 0x10,
			Impl = 0x20,
		}

		[Flags]
		public enum OverloadFlags : uint
		{
			VarEnd = 0x01,
			Native = 0x04,
			ShortHeader = 0x08,
			Virtual = 0x10,
			Abstract = 0x20,
		}

		[Flags]
		public enum ParamFlags : ushort
		{
			ByRef = 0x01,
		}

		[Flags]
		public enum ConstantFlags : uint
		{
			Public = 0x01,
			Private = 0x02,
		}

		public enum Operator : byte
		{
			Add = 0x00,
			Subtract = 0x01,
			Or = 0x02,
			Xor = 0x03,
			Multiply = 0x04,
			Divide = 0x05,
			Modulo = 0x06,
			And = 0x07,
			Power = 0x08,
			ShiftLeft = 0x09,
			ShiftRight = 0x0A,
			// Reserved: 0x0B
			// Reserved: 0x0C
			Plus = 0x0D,
			Negate = 0x0E,
			Not = 0x0F,
			Equals = 0x10,
			Compare = 0x11,
		}
	}

	/// <summary>
	/// Contains a set of opened modules, identified by name.
	/// </summary>
	public class ModulePool : IEnumerable<Module>
	{
		public ModulePool(string libraryPath, Namespace ns)
			: this(libraryPath, ns, null)
		{ }
		internal ModulePool(string libraryPath, Namespace ns, Compiler compiler)
		{
			if (libraryPath == null)
				throw new ArgumentNullException("libraryPath");
			if (ns == null)
				ns = new Namespace();

			this.Compiler = compiler;
			this.libraryPath = libraryPath;
			this.ns = ns;
		}

		private string libraryPath;
		/// <summary>
		/// Gets the path from which library files are loaded, if they are not found in the current directory.
		/// </summary>
		public string LibraryPath { get { return libraryPath; } }

		/// <summary>
		/// The compiler that created this module pool.
		/// </summary>
		internal Compiler Compiler;

		private Namespace ns;
		/// <summary>
		/// Gets the namespace that the module pool's members end up in.
		/// </summary>
		public Namespace Namespace { get { return ns; } }

		private Module standardModule;
		/// <summary>
		/// Gets the standard module.
		/// </summary>
		public Module StandardModule
		{
			get { return standardModule; }
			internal set
			{
				if (value == null)
					throw new ArgumentNullException("value");
				if (standardModule != null)
					throw new InvalidOperationException("The standard module has already been set for this pool.");
				standardModule = value;
				// Add to pool if it isn't there already
				loadedModules[new Key(value.Name, value.Version)] = value;
				loadedModules[new Key(value.Name, null)] = value;
			}
		}

		private Dictionary<Key, Module> loadedModules = new Dictionary<Key, Module>();

		public bool HasLoaded(string name, Version version)
		{
			return loadedModules.ContainsKey(new Key(name, version));
		}

		/// <summary>
		/// Gets a module by the specified name and optionally with a specified version,
		/// loading it from file if it has not already been loaded.
		/// </summary>
		/// <param name="name">The name of the module to get.</param>
		/// <param name="version">The version of the module to load, or null to ignore the version.</param>
		/// <returns>The module with the specified name.</returns>
		public Module GetOrLoad(string name, Version version)
		{
			Module module;
			if (loadedModules.TryGetValue(new Key(name, version), out module))
				return module;

			// We look for modules in the following directories,
			// in the order given:
			//   $current/lib/$name-$version/$name.ovm
			//   $current/lib/$name-$version.ovm
			//   $current/lib/$name/$name.ovm
			//   $current/lib/$name.ovm
			//
			//   $current/$name-$version/$name.ovm
			//   $current/$name-$version.ovm
			//   $current/$name/$name.ovm
			//   $current/$name.ovm
			//
			//   $libpath/$name-$version/$name.ovm
			//   $libpath/$name-$version.ovm
			//   $libpath/$name/$name.ovm
			//   $libpath/$name.ovm
			// where
			//   $current = Environment.CurrentDirectory
			//   $libpath = this.libraryPath
			//   $version = versionStr
			// The versioned paths are excluded if version is null.
			string fileName = null;

			var versionStr = version != null ? "-" + version.ToStringInvariant(4) : null;
			var dirs = new string[] { Path.Combine(".", "lib"), ".", libraryPath };

			bool isVersionedFile = false;
			for (var i = 0; i < dirs.Length; i++)
			{
				// Versioned paths first
				// path/$name
				var baseName = Path.Combine(dirs[i], name);
				if (versionStr != null)
				{
					// path/$name-$version
					var versionedName = baseName + versionStr;

					// path/$name-$version/$name.ovm
					fileName = Path.Combine(versionedName, name + Module.FileExtension);
					if (isVersionedFile = File.Exists(fileName))
						break;

					// path/$name-$version.ovm
					fileName = versionedName + Module.FileExtension;
					if (isVersionedFile = File.Exists(fileName))
						break;
				}

				// And then unversioned paths
				// path/$name/$name.ovm
				fileName = Path.Combine(baseName, name + Module.FileExtension);
				if (File.Exists(fileName))
					break;

				// path/$name.ovm
				fileName = baseName + Module.FileExtension;
				if (File.Exists(fileName))
					break;
				fileName = null;
			}

			if (fileName == null)
				throw new ModuleLoadException(name, "Could not find a file for the specified module.");

			// Note: The module adds itself to the pool.
			module = Module.Open(this, fileName, version, isVersionedFile);

			return module;
		}

		internal void AddModule(string name, Module module, bool fromVersionedFile)
		{
			loadedModules.Add(new Key(name, module.Version), module);

			if (!fromVersionedFile)
				loadedModules.Add(new Key(name, null), module);
		}

		public IEnumerator<Module> GetEnumerator()
		{
			return loadedModules.Values.GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return loadedModules.Values.GetEnumerator();
		}

		private struct Key
		{
			public Key(string name, Version version)
			{
				Name = name;
				Version = version;
			}

			public string Name;
			public Version Version;

			public override bool Equals(object obj)
			{
				return obj is Key && Equals((Key)obj);
			}
			public bool Equals(Key other)
			{
				return this.Name == other.Name && this.Version == other.Version;
			}

			public override int GetHashCode()
			{
				int hash = 0;
				if (Name != null)
					hash = Name.GetHashCode();
				if (Version != null)
					hash ^= Version.GetHashCode();
				return hash;
			}
		}
	}
	
	public class ModuleLoadException : Exception
	{
		public ModuleLoadException(string fileName)
			: this(fileName, "The module could not be loaded.")
		{ }
		public ModuleLoadException(string fileName, string message)
			: this(fileName, message, null)
		{ }
		public ModuleLoadException(string fileName, string message, Exception innerException)
			: base(message, innerException)
		{
			this.fileName = fileName;
		}

		private string fileName;
		/// <summary>
		/// Gets the name of the module file that could not be loaded.
		/// </summary>
		public string FileName { get { return fileName; } }
	}
}