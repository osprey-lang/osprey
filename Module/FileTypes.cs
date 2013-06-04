using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Osprey.Members;
using Type = Osprey.Members.Type;
using Enum = Osprey.Members.Enum;

namespace Osprey.FileFormat
{
	// Defines .NET types for types declared in the Ovum module file format specification.
	// See that document for more information on the specific types.
	// Some notes:
	//     * For all the numeric types, we use the built-in .NET types. Because it would be
	//       stupid to reimplement that.
	//     * String is represented by System.String, because it's already UTF-16, except in
	//       the string table, where it is represented by the wrapper type StringDef.
	//     * Oneof types are basically implemented as separate fields in the containing class.
	//       There's no other easy way of doing it.

	internal class ModuleWriter : BinaryWriter
	{
		public ModuleWriter(Stream output)
			: base(output, Encoding.Unicode)
		{ }

		protected void WriteCString(string value)
		{
			// Note: we convert each character to sbyte instead of byte
			// because ASCII characters do not go above 0x7F. This type
			// is zero-terminated, and the zero-terminator IS part of
			// the length!
			Write(value.Length + 1);
			for (var i = 0; i < value.Length; i++)
				Write(checked((sbyte)value[i]));
			Write((byte)0); // \0 terminator
		}

		protected void WriteOvumString(string value)
		{
			Write(value.Length);
			for (var i = 0; i < value.Length; i++)
				Write(unchecked((ushort)value[i]));
		}

		public void Write(string value, StringKind kind)
		{
			switch (kind)
			{
				case StringKind.DotNET:
					Write(value);
					break;
				case StringKind.Ovum:
					WriteOvumString(value);
					break;
				case StringKind.CString:
					WriteCString(value);
					break;
				default:
					throw new ArgumentOutOfRangeException("kind");
			}
		}

		public void Write(Version value)
		{
			Write(value.Major);
			Write(value.Minor);
			Write(value.Build);
			Write(value.Revision);
		}
	}

	internal enum StringKind
	{
		/// <summary>
		/// Writes a string using the default behaviour of <see cref="BinaryWriter"/>.
		/// </summary>
		DotNET = 0,
		/// <summary>
		/// Writes the string as an Ovum string, which has a fixed-size length prefix.
		/// </summary>
		Ovum = 1,
		/// <summary>
		/// Writes the string as a C string, which uses a single byte per character,
		/// is strictly ASCII, and has a constant-size length prefix.
		/// </summary>
		CString = 2,
	}

	internal abstract class FileType
	{
		public abstract void Emit(ModuleWriter target);
	}

	internal sealed class FileList<T> : FileType
		where T : FileType
	{
		public FileList(ModuleFile owner)
		{
			if (owner == null)
				throw new ArgumentNullException("owner"); 
			this.owner = owner;
		}

		private ModuleFile owner;

		public List<T> Items = new List<T>();

		public override void Emit(ModuleWriter target)
		{
			if (Items.Count == 0)
			{
				target.Write(0u); // size
			}
			else
			{
				var sizeOffset = target.BaseStream.Position;
				target.Seek(4, SeekOrigin.Current);

				target.Write(Items.Count); // length
				for (var i = 0; i < Items.Count; i++)
					Items[i].Emit(target); // item

				// And then backtrack to where the size needs to go
				var current = target.BaseStream.Position;
				target.BaseStream.Seek(sizeOffset, SeekOrigin.Begin);
				target.Write(checked((uint)(current - sizeOffset - 4))); // size

				target.BaseStream.Seek(current, SeekOrigin.Begin);
			}
		}
	}

	internal sealed class Table<T> : FileType
		where T : FileType
	{
		public Table(ModuleFile owner, MemberMask mask)
		{
			if (owner == null)
				throw new ArgumentNullException("owner");
			this.owner = owner;
			this.mask = mask;
		}

		private ModuleFile owner;

		private MemberMask mask;
		public MemberMask Mask { get { return mask; } }

		private List<Entry> entries = new List<Entry>();
		private Dictionary<T, uint> dataToId = new Dictionary<T, uint>();

		public int Length { get { return entries.Count; } }

		public uint AddEntry(T data)
		{
			if (dataToId.ContainsKey(data))
				return dataToId[data];

			var id = owner.GetNextId(mask);
			entries.Add(new Entry(id, data));
			dataToId.Add(data, id);

			return id;
		}

		public override void Emit(ModuleWriter target)
		{
			if (entries.Count == 0)
			{
				target.Write(0u); // size
			}
			else
			{
				var sizeOffset = target.BaseStream.Position;
				target.Seek(4, SeekOrigin.Current);

				target.Write(entries.Count); // length
				for (var i = 0; i < entries.Count; i++)
				{
					var entry = entries[i];
					target.Write(entry.id);  // id
					entry.data.Emit(target); // data
				}

				// And now we backtrack to where the size needs to go
				var current = target.BaseStream.Position;
				target.BaseStream.Seek(sizeOffset, SeekOrigin.Begin);
				target.Write(checked((uint)(current - sizeOffset - 4))); // size

				target.BaseStream.Seek(current, SeekOrigin.Begin); // Resume after contents 
			}
		}

		public struct Entry
		{
			public Entry(uint id, T data)
			{
				this.id = id;
				this.data = data;
			}

			public uint id;
			public T data;
		}
	}

	internal enum MemberMask : uint
	{
		ConstantDef = 0x02000000,
		FunctionDef = 0x04000000,
		TypeDef = 0x10000000,
		FieldDef = 0x12000000,
		MethodDef = 0x14000000,
		PropertyDef = 0x18000000,
		String = 0x20000000,
		ModuleRef = 0x40000000,
		FunctionRef = 0x44000000,
		TypeRef = 0x50000000,
		FieldRef = 0x52000000,
		MethodRef = 0x54000000,
	}

	internal sealed class ModuleFile : FileType
	{
		internal Compiler Compiler;

		public string Name;
		public Version Version;
		public Dictionary<string, string> Meta;

		public string NativeLib;

		private Dictionary<MemberMask, uint> maskCounters = new Dictionary<MemberMask, uint>();

		internal uint GetNextId(MemberMask mask)
		{
			if (maskCounters.ContainsKey(mask))
				return ++maskCounters[mask];
			return maskCounters[mask] = 1;
		}

		public Table<ModuleRef> ModuleRefs;
		public Table<FunctionRef> FunctionRefs;
		public Table<TypeRef> TypeRefs;
		public Table<FieldRef> FieldRefs;
		public Table<MethodRef> MethodRefs;

		public Table<StringDef> Strings;

		public Table<MethodDef> Functions;
		public Table<ConstantDef> Constants;
		public Table<TypeDef> Types;

		public uint MainMethod;

		// The method block is not stored here; instead, we output each byte array
		// of each method directly. There is, however, a counter that we can increment
		// to get the correct offset of the next method blob.
		// Methods are not aligned to any particular byte offset.

		private uint MethodCounter = 0;

		private uint GetMethodOffset(uint methodSize)
		{
			var counter = MethodCounter;
			counter = checked(counter + methodSize);
			return counter;
		}

		public override void Emit(ModuleWriter target)
		{
			target.Write(Module.MagicNumber, 0, 4);
			target.Write(Name, StringKind.Ovum);
			target.Write(Version);
			EmitStringMap(target, Meta);
			target.Write(NativeLib, StringKind.Ovum);

			foreach (var mask in counters)
			{
				uint counter;
				if (!maskCounters.TryGetValue(MemberMask.FunctionDef, out counter))
					counter = 0;
				target.Write(counter);
			}

			ModuleRefs.Emit(target);
			FunctionRefs.Emit(target);
			TypeRefs.Emit(target);
			FieldRefs.Emit(target);
			MethodRefs.Emit(target);

			Strings.Emit(target);

			Functions.Emit(target);
			Constants.Emit(target);
			Types.Emit(target);

			target.Equals(MainMethod);
		}

		private static void EmitStringMap(ModuleWriter target, Dictionary<string, string> map)
		{
			if (map.Count == 0)
			{
				target.Write(0u); // size
			}
			else
			{
				var sizeOffset = target.BaseStream.Position;
				target.Seek(4, SeekOrigin.Current);

				target.Write(map.Count); // length
				foreach (var kvp in map)
				{
					target.Write(kvp.Key, StringKind.Ovum); // key
					target.Write(kvp.Value, StringKind.Ovum); // value
				}

				// And now we backtrack to where the size needs to go
				var current = target.BaseStream.Position;
				target.BaseStream.Seek(sizeOffset, SeekOrigin.Begin);
				target.Write(checked((uint)(current - sizeOffset - 4)));

				target.BaseStream.Seek(current, SeekOrigin.Begin); // Resume after contents
			}
		}

		// These correspond, in order, to the "xCount" fields defined in the Ovum module format specification.
		// See that document for more details.
		private static readonly MemberMask[] counters =
		{
			MemberMask.FunctionDef,
			MemberMask.TypeDef,
			MemberMask.ConstantDef,
			MemberMask.FieldDef,
			MemberMask.MethodDef,
		};
	}

	internal sealed class ModuleRef : FileType
	{
		public ModuleRef(string name, Version minVersion)
		{
			if (name == null)
				throw new ArgumentNullException("name");
			if (minVersion == null)
				throw new ArgumentNullException("minVersion");
			this.Name = name;
			this.MinVersion = minVersion;
		}

		public string Name;
		public Version MinVersion;

		public override void Emit(ModuleWriter target)
		{
			target.Write(Name, StringKind.Ovum);
			target.Write(MinVersion);
		}
	}

	internal sealed class FunctionRef : FileType
	{
		public FunctionRef(string name, uint declModule)
		{
			this.Name = name;
			this.DeclModule = declModule;
		}

		public string Name;
		public uint DeclModule;

		public override void Emit(ModuleWriter target)
		{
			target.Write(Name, StringKind.Ovum);
			target.Write(DeclModule);
		}
	}

	internal sealed class TypeRef : FileType
	{
		public TypeRef(string name, uint declModule)
		{
			this.Name = name;
			this.DeclModule = declModule;
		}

		public string Name;
		public uint DeclModule;

		public override void Emit(ModuleWriter target)
		{
			target.Write(Name, StringKind.Ovum);
			target.Write(DeclModule);
		}
	}

	internal sealed class FieldRef : FileType
	{
		public FieldRef(string name, uint declType)
		{
			this.Name = name;
			this.DeclType = declType;
		}

		public string Name;
		public uint DeclType;

		public override void Emit(ModuleWriter target)
		{
			target.Write(Name, StringKind.Ovum);
			target.Write(DeclType);
		}
	}

	internal sealed class MethodRef : FileType
	{
		public MethodRef(string name, uint declType)
		{
			this.Name = name;
			this.DeclType = declType;
		}

		public string Name;
		public uint DeclType;

		public override void Emit(ModuleWriter target)
		{
			target.Write(Name, StringKind.Ovum);
			target.Write(DeclType);
		}
	}

	internal sealed class StringDef : FileType
	{
		public StringDef(string value)
		{
			if (value == null)
				throw new ArgumentNullException("value");
			this.Value = value;
		}

		public string Value;

		public override void Emit(ModuleWriter target)
		{
			target.Write(Value, StringKind.Ovum);
		}

		public override int GetHashCode()
		{
			return Value.GetHashCode();
		}

		public override bool Equals(object obj)
		{
			if (obj is StringDef)
				return this.Value.Equals(((StringDef)obj).Value);
			return base.Equals(obj);
		}

		public bool Equals(StringDef other)
		{
			return other != null && this.Value == other.Value;
		}
	}

	internal sealed class MethodDef : FileType
	{
		public MethodDef(MethodFlags flags, string name, ModuleFile owner)
		{
			this.Flags = flags;
			this.Name = name;
			this.Overloads = new FileList<Overload>(owner);
		}

		public MethodFlags Flags;
		public string Name;
		public FileList<Overload> Overloads;

		public override void Emit(ModuleWriter target)
		{
			target.Write(unchecked((byte)Flags));
			target.Write(Name, StringKind.Ovum);
			Overloads.Emit(target);
		}
	}

	internal enum MethodFlags : byte
	{
		Public = 0x01,
		Private = 0x02,
		Protected = 0x04,
		Instance = 0x08,
		Ctor = 0x10,
		Impl = 0x20,
	}

	internal sealed class Overload : FileType
	{
		public OverloadFlags Flags;

		public string[] ParamNames;

		public MethodHeader Header; // Only used if OverloadFlags.ShortHeader is not specified

		public EntryPoint Entry;

		public override void Emit(ModuleWriter target)
		{
			target.Write((byte)Flags);

			target.Write(checked((ushort)ParamNames.Length));
			foreach (var name in ParamNames)
				target.Write(name, StringKind.Ovum);

			if ((Flags & OverloadFlags.ShortHeader) == 0)
				Header.Emit(target);

			Entry.Emit(target);
		}
	}

	internal sealed class MethodHeader : FileType
	{
		public MethodHeader(ushort optionalParamCount, ushort localCount, ushort maxStack, ModuleFile owner)
		{
			this.OptionalParamCount = optionalParamCount;
			this.LocalCount = localCount;
			this.MaxStack = maxStack;
			this.Tries = new FileList<FileTryBlock>(owner);
		}

		public ushort OptionalParamCount;
		public ushort LocalCount;
		public ushort MaxStack;
		public FileList<FileTryBlock> Tries;
	}

	internal sealed class FileTryBlock : FileType
	{
		// Note: TryBlockKind is intentionally identical to the TryKind defined
		// in the file format specification. If the format changes, update TryBlockKind.
		public TryBlockKind Kind;

		public uint TryStart;
		public uint TryEnd;

		public FileFinallyBlock Finally;
		public FileList<FileCatchBlock> Catches;

		public override void Emit(ModuleWriter target)
		{
			target.Write(checked((byte)Kind));
			target.Write(TryStart);
			target.Write(TryEnd);

			if (Kind == TryBlockKind.TryCatch)
				Catches.Emit(target);
			else if (Kind == TryBlockKind.TryFinally)
				Finally.Emit(target);
		}
	}

	internal sealed class FileFinallyBlock : FileType
	{
		public uint FinallyStart;
		public uint FinallyEnd;

		public override void Emit(ModuleWriter target)
		{
			target.Write(FinallyStart);
			target.Write(FinallyEnd);
		}
	}

	internal sealed class FileCatchBlock : FileType
	{
		public uint CatchStart;
		public uint CatchEnd;
		public uint CaughtType;

		public override void Emit(ModuleWriter target)
		{
			target.Write(CatchStart);
			target.Write(CatchEnd);
			target.Write(CaughtType);
		}
	}

	internal sealed class EntryPoint : FileType
	{
		public EntryPoint(uint offset, uint length)
		{
			this.Offset = offset;
			this.Length = length;
		}
		public EntryPoint(string name)
		{
			this.Name = name;
		}

		// Managed
		public uint Offset;
		public uint Length;

		// Native
		public string Name;

		public override void Emit(ModuleWriter target)
		{
			if (Name != null)
				target.Write(Name, StringKind.CString);
			else
			{
				target.Write(Offset);
				target.Write(Length);
			}
		}
	}

	internal sealed class ConstantDef : FileType
	{
		public ConstantFlags Flags;
		public string Name;
		public uint Type;
		public ulong Value;

		public override void Emit(ModuleWriter target)
		{
			target.Write((byte)Flags);
			target.Write(Name, StringKind.Ovum);
			target.Write(Type);
			target.Write(Value);
		}
	}

	internal enum ConstantFlags : byte
	{
		Public = 0x01,
		Private = 0x02,
	}

	internal sealed class TypeDef : FileType
	{
		public TypeDef(TypeFlags flags, string name, uint baseType, uint sharedType, ModuleFile owner)
		{
			this.Flags = flags;
			this.Name = name;
			this.BaseType = baseType;
			this.SharedType = sharedType;

			this.Fields = new Table<FieldDef>(owner, MemberMask.FieldDef);
			this.Methods = new Table<MethodDef>(owner, MemberMask.MethodDef);
			this.Properties = new Table<PropertyDef>(owner, MemberMask.PropertyDef);

			this.Operators = new FileList<OperatorDef>(owner);
		}

		public TypeFlags Flags;
		public string Name;
		public uint BaseType;
		public uint SharedType;

		public Table<FieldDef> Fields;
		public Table<MethodDef> Methods;
		public Table<PropertyDef> Properties;

		public FileList<OperatorDef> Operators;

		public override void Emit(ModuleWriter target)
		{
			target.Write((byte)Flags);
			target.Write(Name, StringKind.Ovum);
			target.Write(BaseType);
			target.Write(SharedType);

			Fields.Emit(target);
			Methods.Emit(target);
			Properties.Emit(target);

			Operators.Emit(target);
		}

		public static TypeDef FromType(Type type, ModuleFile owner)
		{
			if (type is Enum)
				return FromEnum((Enum)type, owner);
			return FromClass((Class)type, owner);
		}

		private static TypeDef FromClass(Class c, ModuleFile owner)
		{
			TypeFlags flags = c.Access == AccessLevel.Private ? TypeFlags.Private : TypeFlags.Public;
			
			if (c.IsPrimitive)
				flags |= TypeFlags.Primitive;
			else if (c.IsStatic)
				flags |= TypeFlags.Static;
			else if (c.IsAbstract)
				flags |= TypeFlags.Abstract;
			else if (!c.IsInheritable)
				flags |= TypeFlags.Sealed;

			var output = new TypeDef(flags, c.FullName,
				c.BaseType == null ? 0 : c.BaseType.Id,
				c.SharedType == null ? 0 : c.SharedType.Id,
				owner);

			var properties = new List<Property>();

			foreach (var member in c.members.Values.OrderBy(GetMemberName, StringComparer.InvariantCultureIgnoreCase))
			{
				if (member is Field)
					output.Fields.AddEntry(FieldDef.FromField((Field)member));
				else if (member is ClassConstant)
					output.Fields.AddEntry(FieldDef.FromConstant((ClassConstant)member, owner));
				else if (member is MethodGroup)
					output.Methods.AddEntry(MethodDef.FromMethodGroup((MethodGroup)member));
				else if (member is Property)
					properties.Add((Property)member);
			}

			if (c.Indexer != null)
				output.Properties.AddEntry(PropertyDef.FromIndexer(c.Indexer));
			foreach (var prop in properties)
				output.Properties.AddEntry(PropertyDef.FromProperty(prop));

			foreach (var op in c.operators)
				if (op != null)
					output.Operators.Items.Add(OperatorDef.FromOverload(op));

			return output;
		}

		private static TypeDef FromEnum(Enum e, ModuleFile owner)
		{
			var output = new TypeDef(e.Access == AccessLevel.Public ? TypeFlags.Public : TypeFlags.Private,
				e.FullName, e.BaseType.Id, 0, owner);

			foreach (var member in e.members.Values.OrderBy(GetMemberName, StringComparer.InvariantCultureIgnoreCase))
				output.Fields.AddEntry(FieldDef.FromEnumField(member));

			return output;
		}

		private static string GetMemberName(NamedMember member)
		{
			return member.Name;
		}
	}

	internal class FieldDef : FileType
	{
		public FieldDef(FieldFlags flags, string name)
		{
			this.Flags = flags;
			this.Name = name;
		}

		public FieldFlags Flags;
		public string Name;

		// If the field is constant
		public uint Type;
		public ulong Value;

		public override void Emit(ModuleWriter target)
		{
			target.Write((byte)Flags);
			target.Write(Name, StringKind.Ovum);

			if ((Flags & FieldFlags.HasValue) == FieldFlags.HasValue)
			{
				target.Write(Type);
				target.Write(Value);
			}
		}

		public static FieldDef FromField(Field field)
		{
			FieldFlags flags = !field.IsStatic ? FieldFlags.Instance : 0;

			if (field.Access == AccessLevel.Public)
				flags |= FieldFlags.Public;
			else if (field.Access == AccessLevel.Private)
				flags |= FieldFlags.Private;
			else
				flags |= FieldFlags.Protected;

			return new FieldDef(flags, field.Name);
		}

		public static FieldDef FromConstant(ClassConstant constant, ModuleFile owner)
		{
			FieldFlags flags = FieldFlags.HasValue;

			if (constant.Access == AccessLevel.Public)
				flags |= FieldFlags.Public;
			else if (constant.Access == AccessLevel.Private)
				flags |= FieldFlags.Private;
			else
				flags |= FieldFlags.Protected;

			var constValue = constant.Value;

			uint typeId = constValue.Type == ConstantValueType.Null ? 0 : constValue.GetTypeObject(owner.Compiler).Id;

			ulong value;
			if (constValue.Type == ConstantValueType.String)
				value = owner.Strings.AddEntry(new StringDef(constValue.StringValue));
			else
				value = constValue.GetRawValue();

			return new FieldDef(flags, constant.Name)
			{
				Type = typeId,
				Value = value,
			};
		}

		public static FieldDef FromEnumField(EnumField field)
		{
			var value = field.Value.EnumValue;

			return new FieldDef(FieldFlags.Public | FieldFlags.HasValue, field.Name)
			{
				Type = value.Type.Id,
				Value = unchecked((ulong)value.Value)
			};
		}
	}

	internal class PropertyDef : FileType
	{
		public PropertyDef(uint getter, uint setter)
		{
			this.Getter = getter;
			this.Setter = setter;
		}

		public string Name;
		public uint Getter;
		public uint Setter;

		public override void Emit(ModuleWriter target)
		{
			target.Write(Name, StringKind.Ovum);
			target.Write(Getter);
			target.Write(Setter);
		}

		public static PropertyDef FromProperty(Property prop)
		{
			return new PropertyDef(prop.GetterId, prop.SetterId);
		}

		public static PropertyDef FromIndexer(Indexer indexer)
		{
			return new PropertyDef(indexer.GetterId, indexer.SetterId);
		}
	}

	internal class OperatorDef : FileType
	{
		public Operator Operator;
		public uint Method;

		public override void Emit(ModuleWriter target)
		{
			target.Write((byte)Operator);
			target.Write(Method);
		}

		public static OperatorDef FromOverload(OperatorOverload op)
		{
			return new OperatorDef()
			{
				Operator = unchecked((Operator)op.Index),
				Method = op.Method.Group.Id,
			};
		}
	}

	internal enum Operator : byte
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