using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile.Raw
{
	public enum MemberKind : uint
	{
		ConstantDef = 0x02000000,
		FunctionDef = 0x04000000,
		TypeDef     = 0x10000000,
		FieldDef    = 0x12000000,
		MethodDef   = 0x14000000,
		String      = 0x20000000,
		ModuleRef   = 0x40000000,
		FunctionRef = 0x44000000,
		TypeRef     = 0x50000000,
		FieldRef    = 0x52000000,
		MethodRef   = 0x54000000,
	}

	public enum VersionConstraint : uint
	{
		Exact      = 0x00000000,
		FixedMinor = 0x00000001,
		FixedMajor = 0x00000002,
	}

	[Flags]
	public enum TypeRefFlags : uint
	{
		None = 0,
	}

	[Flags]
	public enum FieldRefFlags : uint
	{
		None = 0,
	}

	[Flags]
	public enum MethodRefFlags : uint
	{
		None = 0,
	}

	[Flags]
	public enum FunctionRefFlags : uint
	{
		None = 0,
	}

	[Flags]
	public enum TypeFlags : uint
	{
		Public    = 0x00000001,
		Internal  = 0x00000002,
		Abstract  = 0x00000100,
		Sealed    = 0x00000200,
		Static    = 0x00000300,
		Impl      = 0x00001000,
		Primitive = 0x00002000,
	}

	public enum Operator : uint
	{
		Add        =  0, // +
		Subtract   =  1, // -
		Or         =  2, // |
		Xor        =  3, // ^
		Multiply   =  4, // *
		Divide     =  5, // /
		Modulo     =  6, // %
		And        =  7, // &
		Power      =  8, // **
		ShiftLeft  =  9, // <<
		ShiftRight = 10, // >>
		Plus       = 11, // +x
		Negate     = 12, // -x
		Not        = 13, // ~
		Equals     = 14, // ==
		Compare    = 15, // <=>
	}

	[Flags]
	public enum FieldFlags : uint
	{
		Public    = 0x00000001,
		Internal  = 0x00000002,
		Protected = 0x00000004,
		Private   = 0x00000008,
		Instance  = 0x00000100,
		HasValue  = 0x00000200,
		Impl      = 0x00001000,
	}

	[Flags]
	public enum MethodFlags : uint
	{
		Public    = 0x00000001,
		Internal  = 0x00000002,
		Protected = 0x00000004,
		Private   = 0x00000008,
		Instance  = 0x00000100,
		Ctor      = 0x00000200,
		Impl      = 0x00001000,
	}

	[Flags]
	public enum OverloadFlags : uint
	{
		Variadic    = 0x00000001,
		Virtual     = 0x00000100,
		Abstract    = 0x00000200,
		Override    = 0x00000400,
		Native      = 0x00001000,
		ShortHeader = 0x00002000,
	}

	[Flags]
	public enum ParamFlags : uint
	{
		ByRef    = 0x00000001,
		Optional = 0x00000002,
	}

	[Flags]
	public enum ConstantFlags : uint
	{
		Public   = 0x00000001,
		Internal = 0x00000002,
	}

	public enum TryKind : uint
	{
		Catch   = 0x00000001,
		Finally = 0x00000002,
		Fault   = 0x00000003,
	}
}
