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
			: this(pool, name, version, false)
		{ }
		private Module(ModulePool pool, string name, Version version, bool imported)
		{
			this.pool = pool;
			this.name = name;
			this.version = version;
			this.imported = imported;

			this.members = new MemberDefsAndRefs(imported);
			this.membersByFullName = new Dictionary<string, NamedMember>();
			if (!imported)
			{
				this.methodBlock = new MemoryStream(2048); // Initialize to 2k capacity
				this.metadata = new Dictionary<string, string>();
			}

			this.explicitlyImported = pool != null && name != null &&
				pool.Compiler != null && pool.Compiler.ImportsModule(name);
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

			var offset = checked((uint)methodBlock.Length);

			methodBlock.Write(body, 0, body.Length);

			return offset;
		}

		internal const uint MaskMask          = 0xff000000u;
		internal const uint GlobalConstMask   = 0x02000000u;
		internal const uint GlobalFuncDefMask = 0x04000000u;
		internal const uint TypeDefMask       = 0x10000000u;
		internal const uint FieldDefMask      = 0x12000000u;
		internal const uint MethodDefMask     = 0x14000000u;
		//internal const uint PropertyDefMask = 0x18000000u;
		internal const uint StringMask        = 0x20000000u;
		internal const uint ModuleRefMask     = 0x40000000u;
		internal const uint GlobalFuncRefMask = 0x44000000u;
		internal const uint TypeRefMask       = 0x50000000u;
		internal const uint FieldRefMask      = 0x52000000u;
		internal const uint MethodRefMask     = 0x54000000u;

		internal static readonly byte[] MagicNumber = { (byte)'O', (byte)'V', (byte)'M', (byte)'M' };
		internal const int DataStart = 16; // the beginning of the "real" data in the module

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

		[DebuggerDisplay("Count = {Count}")]
		internal class MemberTable<T> : IEnumerable<KeyValuePair<uint, T>>
			where T : class
		{
			public MemberTable(uint mask)
				: this(mask, 0)
			{ }
			public MemberTable(uint mask, int capacity)
			{
				if ((mask & ~MaskMask) != 0 || (mask & MaskMask) == 0)
					throw new ArgumentException("Invalid MemberTable mask.", "mask");

				this.mask = mask;
				memberToId = new Dictionary<T, uint>(capacity);
				memberById = new List<T>(capacity);
			}

			private bool readOnly = false;
			private int version = 0;
			private uint mask;
			private Dictionary<T, uint> memberToId;
			private List<T> memberById;

			/// <summary>
			/// Gets an entry by ID.
			/// </summary>
			/// <param name="id">The ID of the entry to retrieve.</param>
			/// <returns>The entry with the specified ID.</returns>
			/// <exception cref="ArgumentException">The mask of <paramref name="id"/> does not match the mask of the entries in the table.
			/// -or-
			/// <paramref name="id"/> refers to an entry that is not in the table.</exception>
			public T this[uint id]
			{
				get
				{
					if ((id & MaskMask) != mask)
						throw new ArgumentException("Invalid member mask.", "id");

					uint value = id & ~MaskMask;
					if (value < 1 || value > memberById.Count)
						throw new ArgumentException("There is no entry with the specified ID.", "id");

					return memberById[unchecked((int)(value - 1))];
				}
			}

			/// <summary>
			/// Gets the actual number of entries in the table.
			/// </summary>
			public int Count { get { return memberById.Count; } }

			/// <summary>
			/// Gets a value indicating whether the member table is readonly.
			/// </summary>
			public bool ReadOnly { get { return readOnly; } }

			/// <summary>
			/// Returns the ID of an entry in the table, adding it to the end if it does not exist.
			/// </summary>
			/// <param name="entry">The entry to get an ID for.</param>
			/// <returns>The ID of the entry.</returns>
			/// <exception cref="ArgumentNullException"><paramref name="entry"/> is null.</exception>
			public uint GetId(T entry)
			{
				if (entry == null)
					throw new ArgumentNullException("entry");

				if (memberToId.ContainsKey(entry))
					return memberToId[entry];

				return AddInternal(entry);
			}

			/// <summary>
			/// Adds an entry to the table, and returns its ID.
			/// </summary>
			/// <param name="entry">The entry to add to the table.</param>
			/// <returns>The ID of the entry that was added.</returns>
			/// <exception cref="ArgumentNullException"><paramref name="entry"/> is null.</exception>
			/// <exception cref="ArgumentException"><paramref name="entry"/> is already in the table.</exception>
			public uint Add(T entry)
			{
				if (entry == null)
					throw new ArgumentNullException("entry");
				if (memberToId.ContainsKey(entry))
					throw new ArgumentException("The specified entry is already in the table.", "entry");

				return AddInternal(entry);
			}

			private uint AddInternal(T entry)
			{
				if (readOnly)
					throw new InvalidOperationException("Cannot modify a readonly member table.");

				var id = mask | unchecked((uint)memberById.Count + 1);

				if (entry != null)
					memberToId[entry] = id;
				memberById.Add(entry);

				version++;

				return id;
			}

			public bool ContainsId(uint id)
			{
				var index = id & ~MaskMask;
				return index >= 1 && index <= memberById.Count;
			}

			public bool ContainsEntry(T entry)
			{
				if (entry == null)
					throw new ArgumentNullException("entry");

				return memberToId.ContainsKey(entry);
			}

			/// <summary>
			/// Adds <paramref name="count"/> number of empty (null) entries.
			/// </summary>
			/// <param name="count">The number of empty entries to add.</param>
			internal void AddEmpty(int count)
			{
				for (var i = 0; i < count; i++)
					AddInternal(null);
			}

			/// <summary>
			/// Locks the member table, marking it as readonly and preventing any further modifications to it.
			/// This operation is irreversible.
			/// </summary>
			internal void Lock()
			{
				readOnly = true;
			}

			private IEnumerable<KeyValuePair<uint, T>> EnumerateEntries()
			{
				var startVersion = version;
				var count = memberById.Count;
				for (var i = 0; i < count; i++)
				{
					if (version != startVersion)
						throw new InvalidOperationException("The table has been modified since the enumeration began.");

					yield return new KeyValuePair<uint, T>(mask | unchecked((uint)i + 1), memberById[i]);
				}
			}

			public IEnumerator<KeyValuePair<uint, T>> GetEnumerator()
			{
				return EnumerateEntries().GetEnumerator();
			}

			System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
			{
				return EnumerateEntries().GetEnumerator();
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
			VarStart = 0x02,
			Native = 0x04,
			ShortHeader = 0x08,
			Virtual = 0x10,
			Abstract = 0x20,
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
			Hashop = 0x0B,
			Dollar = 0x0C,
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
				loadedModules.Add(value.Name, value);
			}
		}

		private Dictionary<string, Module> loadedModules = new Dictionary<string, Module>();

		public bool HasLoaded(string name)
		{
			return loadedModules.ContainsKey(name);
		}

		/// <summary>
		/// Gets a module by the specified name, loading it from file if it has not already been loaded.
		/// </summary>
		/// <param name="name">The name of the module to get.</param>
		/// <returns>The module with the specified name.</returns>
		public Module GetOrLoad(string name)
		{
			if (loadedModules.ContainsKey(name))
				return loadedModules[name];

			// TODO: find file name of module
			string fileName = null;

			var nameWithExtension = name + ".ovm";
			if (File.Exists(nameWithExtension))
				fileName = Path.GetFullPath(nameWithExtension);
			else if (File.Exists(fileName = Path.Combine(libraryPath, nameWithExtension)))
				fileName = Path.GetFullPath(fileName);
			else
				throw new ArgumentException("Could not find a file for the specified module.", "name");

			var module = Module.Open(this, fileName);
			//loadedModules.Add(name, module); // Note: the module adds itself.

			return module;
		}

		/// <summary>
		/// Loads a module by the specified name, if it has not already been loaded.
		/// </summary>
		/// <param name="name">The name of the module to load.</param>
		public void Load(string name)
		{
			GetOrLoad(name);
		}

		internal void AddModule(string name, Module module)
		{
			loadedModules.Add(name, module);
		}

		public IEnumerator<Module> GetEnumerator()
		{
			return loadedModules.Values.GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return loadedModules.Values.GetEnumerator();
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