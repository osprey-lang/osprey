using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace Osprey.ModuleFile.Raw
{
	[StructLayout(LayoutKind.Explicit)]
	public struct ModuleHeaderStruct : IFixedSizeObject
	{
		[FieldOffset(0)]
		public uint Magic;
		[FieldOffset(4)]
		public uint FormatVersion;

		[FieldOffset(16)]
		public ModuleVersionStruct Version;
		[FieldOffset(28)]
		public Rva<StringStruct> Name;

		[FieldOffset(32)]
		public Rva<StringTableHeaderStruct> Strings;

		[FieldOffset(36)]
		public Rva<StringStruct> NativeLib;

		[FieldOffset(40)]
		public Rva<RefTableHeaderStruct> References;

		[FieldOffset(44)]
		public Rva<StringMapHeaderStruct> Metadata;

		[FieldOffset(48)]
		public uint MainMethod;

		[FieldOffset(52)]
		public int TypeCount;
		[FieldOffset(56)]
		public RvaToArray<TypeDefStruct> Types;

		[FieldOffset(60)]
		public int FieldCount;
		[FieldOffset(64)]
		public RvaToArray<FieldDefStruct> Fields;

		[FieldOffset(68)]
		public int MethodCount;
		[FieldOffset(72)]
		public RvaToArray<MethodDefStruct> Methods;

		[FieldOffset(76)]
		public int FunctionCount;
		[FieldOffset(80)]
		public RvaToArray<MethodDefStruct> Functions;

		[FieldOffset(84)]
		public int ConstantCount;
		[FieldOffset(88)]
		public RvaToArray<ConstantDefStruct> Constants;

		// Not supported
		[FieldOffset(92)]
		public uint Annotations;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct ModuleVersionStruct : IFixedSizeObject
	{
		public uint Major;
		public uint Minor;
		public uint Patch;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct StringStruct
	{
		public int Length;
		public const uint CharactersOffset = 4;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct ByteStringStruct
	{
		public int Length;
		public const uint CharactersOffset = 4;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct StringTableHeaderStruct
	{
		public int Length;
		public const uint StringsOffset = 4;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct StringMapHeaderStruct
	{
		public int Length;
		public const uint EntriesOffset = 4;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct StringMapEntryStruct : IFixedSizeObject
	{
		public Rva<StringStruct> Key;
		public Rva<StringStruct> Value;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct RefTableHeaderStruct : IFixedSizeObject
	{
		public int ModuleRefCount;
		public RvaToArray<ModuleRefStruct> ModuleRefs;

		public int TypeRefCount;
		public RvaToArray<TypeRefStruct> TypeRefs;

		public int FieldRefCount;
		public RvaToArray<FieldRefStruct> FieldRefs;

		public int MethodRefCount;
		public RvaToArray<MethodRefStruct> MethodRefs;

		public int FunctionRefCount;
		public RvaToArray<FunctionRefStruct> FunctionRefs;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct ModuleRefStruct : IFixedSizeObject
	{
		public MetadataToken Name;
		public VersionConstraint VersionConstraint;
		public ModuleVersionStruct Version;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct TypeRefStruct : IFixedSizeObject
	{
		public MetadataToken DeclModule;
		public TypeRefFlags Flags;
		public MetadataToken Name;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct FieldRefStruct : IFixedSizeObject
	{
		public MetadataToken DeclType;
		public FieldRefFlags Flags;
		public MetadataToken Name;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct MethodRefStruct : IFixedSizeObject
	{
		public MetadataToken DeclType;
		public MethodRefFlags Flags;
		public MetadataToken Name;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct FunctionRefStruct : IFixedSizeObject
	{
		public MetadataToken DeclModule;
		public FunctionRefFlags Flags;
		public MetadataToken Name;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct TypeDefStruct : IFixedSizeObject
	{
		public TypeFlags Flags;
		public MetadataToken Name;
		public MetadataToken BaseType;
		public MetadataToken SharedType;

		// Not supported
		public uint Annotations;

		public Rva<ByteStringStruct> Initer;

		public int FieldCount;
		public MetadataToken FirstField;

		public int MethodCount;
		public MetadataToken FirstMethod;

		public int PropertyCount;
		public RvaToArray<PropertyDefStruct> Properties;

		public int OperatorCount;
		public RvaToArray<OperatorDefStruct> Operators;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct FieldDefStruct : IFixedSizeObject
	{
		public FieldFlags Flags;
		public MetadataToken Name;
		public MetadataToken DeclType;

		// Not supported
		public uint Annotations;

		public Rva<ConstantValueStruct> Value;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct PropertyDefStruct : IFixedSizeObject
	{
		public MetadataToken Name;
		public MetadataToken Getter;
		public MetadataToken Setter;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct OperatorDefStruct : IFixedSizeObject
	{
		public Operator Operator;
		public MetadataToken Method;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct MethodDefStruct : IFixedSizeObject
	{
		public MethodFlags Flags;
		public MetadataToken Name;
		public MetadataToken DeclType;

		public int OverloadCount;
		public RvaToArray<OverloadDefStruct> Overloads;
	}

	[StructLayout(LayoutKind.Explicit)]
	public struct OverloadDefStruct : IFixedSizeObject
	{
		[FieldOffset(0)]
		public OverloadFlags Flags;

		// Not supported
		[FieldOffset(4)]
		public uint Annotations;

		[FieldOffset(8)]
		public int ParamCount;
		[FieldOffset(12)]
		public RvaToArray<ParameterStruct> Params;

		[FieldOffset(16)]
		public Rva<MethodBodyStruct> ShortHeader;
		[FieldOffset(16)]
		public Rva<MethodHeaderStruct> LongHeader;
		[FieldOffset(16)]
		public Rva<NativeMethodHeaderStruct> NativeHeader;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct ParameterStruct : IFixedSizeObject
	{
		public ParamFlags Flags;
		public MetadataToken Name;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct MethodHeaderStruct
	{
		public uint LocalCount;
		public uint MaxStack;

		public int TryBlockCount;
		public RvaToArray<TryBlockStruct> TryBlocks;

		public MethodBodyStruct Body;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct MethodBodyStruct
	{
		public uint Size;

		public byte[] ReadBytes(/* TODO */)
		{
			throw new NotImplementedException();
		}
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct NativeMethodHeaderStruct
	{
		public uint LocalCount;
		public ByteStringStruct EntryPointName;
	}

	[StructLayout(LayoutKind.Explicit)]
	public struct TryBlockStruct : IFixedSizeObject
	{
		[FieldOffset(0)]
		public TryKind Kind;
		[FieldOffset(4)]
		public uint TryStart;
		[FieldOffset(8)]
		public uint TryEnd;

		[FieldOffset(12)]
		public CatchClausesStruct CatchClauses;
		[FieldOffset(12)]
		public FinallyClauseStruct FinallyClause;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct CatchClausesStruct : IFixedSizeObject
	{
		public int Count;
		public RvaToArray<CatchClauseStruct> Clauses;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct CatchClauseStruct : IFixedSizeObject
	{
		public MetadataToken CaughtType;
		public uint CatchStart;
		public uint CatchEnd;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct FinallyClauseStruct : IFixedSizeObject
	{
		public uint FinallyStart;
		public uint FinallyEnd;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct ConstantDefStruct : IFixedSizeObject
	{
		public ConstantFlags Flags;
		public MetadataToken Name;

		// Not supported
		public uint Annotations;

		public Rva<ConstantValueStruct> Value;
	}

	[StructLayout(LayoutKind.Explicit)]
	public struct ConstantValueStruct : IFixedSizeObject
	{
		[FieldOffset(0)]
		public MetadataToken Type;

		[FieldOffset(8)]
		public ulong Value;
		[FieldOffset(8)]
		public MetadataToken StringValue;
	}

	// Debug symbol structs - used for writing only

	[StructLayout(LayoutKind.Sequential)]
	public struct DebugSymbolsHeaderStruct
	{
		public uint MagicNumber;
		public uint Metadata;
		public Rva<SourceFileListStruct> SourceFiles;
		public int MethodSymbolCount;
		// method symbols follow the count
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct SourceFileListStruct
	{
		public int FileCount;
		// source files follow the count
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct MethodSymbolsStruct
	{
		public MetadataToken MemberToken;
		public uint Metadata;
		public int OverloadCount;
		// overloads follow the count
	}

	public struct OverloadSymbolsStruct
	{
		public uint Metadata;
		public int SymbolCount;
		// debug symbols follow the count
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct DebugSymbolStruct : IFixedSizeObject
	{
		public uint StartOffset;
		public uint EndOffset;

		public int SourceFile;

		public SourceLocationStruct StartLocation;
		public SourceLocationStruct EndLocation;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct SourceLocationStruct : IFixedSizeObject
	{
		public int LineNumber;
		public int Column;
	}
}
