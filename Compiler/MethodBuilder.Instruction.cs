using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.Nodes;
using CI = System.Globalization.CultureInfo;
using Enum = Osprey.Members.Enum;

namespace Osprey.Instructions
{
	/// <summary>
	/// Represents a single instruction in a method. It may take one or more arguments, and may modify the stack.
	/// </summary>
#if DEBUG
	[DebuggerDisplay("{DebugString,nq}")]
#endif
	public abstract class Instruction
	{
		private List<Label> labels = null;
		/// <summary>
		/// Associates a label with the instruction.
		/// </summary>
		/// <param name="label">The <see cref="Label"/> to associate with the instruction.</param>
		internal void AddLabel(Label label)
		{
			if (labels == null)
				labels = new List<Label>();
			if (labels.Contains(label))
				return;
			labels.Add(label);
		}

		public bool HasLabel(Label label)
		{
			if (labels == null)
				return false;

			return labels.Contains(label);
		}

		private MethodBuilder parent;
		/// <summary>
		/// Gets the <see cref="MethodBuilder"/> that contains the instruction.
		/// </summary>
		public MethodBuilder Parent { get { return parent; } internal set { parent = value; } }

		internal int Index;
		internal int ByteOffset;

		internal InstructionRestrictions Restrictions;

		private SourceLocation location;
		/// <summary>
		/// Gets the source location that generated this instruction.
		/// </summary>
		public SourceLocation Location { get { return location; } internal set { location = value; } }

		/// <summary>
		/// Gets the stack change performed by the instruction.
		/// A negative value indicates removal of items from the stack,
		/// a positive value indicates addition.
		/// Zero indicates a net change of nothing.
		/// </summary>
		public abstract StackChange StackChange { get; }

		/// <summary>
		/// Gets the size of the instruction, in bytes.
		/// </summary>
		/// <returns>The size of the instruction, in bytes.</returns>
		public abstract int GetSize();

		/// <summary>
		/// Gets an array of bytes that make up the instruction.
		/// </summary>
		/// <returns>The bytes that make up the instruction, including any arguments it may take.</returns>
		public abstract byte[] GetBytes();

		internal bool HasRestriction(InstructionRestrictions restriction)
		{
			return (Restrictions & restriction) == restriction;
		}

		internal void AddRestriction(InstructionRestrictions restriction)
		{
			Restrictions |= restriction;
		}

#if DEBUG
		internal string DebugString
		{
			get
			{
				var str = this.ToString();
				if (location != null)
					str = string.Format("{0} {{at {1}}}", str, location);
				return str;
			}
		}
#endif
	}

	[Flags]
	internal enum InstructionRestrictions
	{
		/// <summary>
		/// The instruction has no restrictions.
		/// </summary>
		None = 0,
		/// <summary>
		/// Other instructions are not allowed to fall through to this one.
		/// </summary>
		CannotFallThroughTo = 1,
	}

	public enum Opcode : byte
	{
		Nop = 0x00,
		Dup = 0x01,
		Pop = 0x02,
		// Arguments
		Ldarg_0 = 0x03,
		Ldarg_1 = 0x04,
		Ldarg_2 = 0x05,
		Ldarg_3 = 0x06,
		Ldarg_s = 0x07,
		Ldarg = 0x08,
		Starg_s = 0x09,
		Starg = 0x0a,
		// Locals
		Ldloc_0 = 0x0b,
		Ldloc_1 = 0x0c,
		Ldloc_2 = 0x0d,
		Ldloc_3 = 0x0e,
		Stloc_0 = 0x0f,
		Stloc_1 = 0x10,
		Stloc_2 = 0x11,
		Stloc_3 = 0x12,
		Ldloc_s = 0x13,
		Ldloc = 0x14,
		Stloc_s = 0x15,
		Stloc = 0x16,
		// Values and object initialisation
		Ldnull = 0x17,
		Ldfalse = 0x18,
		Ldtrue = 0x19,
		Ldc_i_m1 = 0x1a,
		Ldc_i_0 = 0x1b,
		Ldc_i_1 = 0x1c,
		Ldc_i_2 = 0x1d,
		Ldc_i_3 = 0x1e,
		Ldc_i_4 = 0x1f,
		Ldc_i_5 = 0x20,
		Ldc_i_6 = 0x21,
		Ldc_i_7 = 0x22,
		Ldc_i_8 = 0x23,
		Ldc_i_s = 0x24,
		Ldc_i_m = 0x25,
		Ldc_i = 0x26,
		Ldc_u = 0x27,
		Ldc_r = 0x28,
		Ldstr = 0x29,
		Ldargc = 0x2a,
		Ldenum_s = 0x2b,
		Ldenum = 0x2c,
		Newobj_s = 0x2d,
		Newobj = 0x2e,
		// Invocation
		Call_0 = 0x2f,
		Call_1 = 0x30,
		Call_2 = 0x31,
		Call_3 = 0x32,
		Call_s = 0x33,
		Call = 0x34,
		Scall_s = 0x35,
		Scall = 0x36,
		Apply = 0x37,
		Sapply = 0x38,
		Callmem_s = 0x7f,
		Callmem = 0x80,
		// Control flow
		Retnull = 0x39,
		Ret = 0x3a,
		Br_s = 0x3b,
		Brnull_s = 0x3c,
		Brinst_s = 0x3d,
		Brfalse_s = 0x3e,
		Brtrue_s = 0x3f,
		Brref_s = 0x40,
		Brnref_s = 0x41,
		Brtype_s = 0x42,
		Br = 0x43,
		Brnull = 0x44,
		Brinst = 0x45,
		Brfalse = 0x46,
		Brtrue = 0x47,
		Brref = 0x48,
		Brnref = 0x49,
		Brtype = 0x4a,
		Switch_s = 0x4b,
		Switch = 0x4c,
		// Operators
		Add = 0x4d,
		Sub = 0x4e,
		Or = 0x4f,
		Xor = 0x50,
		Mul = 0x51,
		Div = 0x52,
		Mod = 0x53,
		And = 0x54,
		Pow = 0x55,
		Shl = 0x56,
		Shr = 0x57,
		Hashop = 0x58,
		Dollar = 0x59,
		Plus = 0x5a,
		Neg = 0x5b,
		Not = 0x5c,
		Eq = 0x5d,
		Cmp = 0x5e,
		Lt = 0x5f,
		Gt = 0x60,
		Lte = 0x61,
		Gte = 0x62,
		Concat = 0x63,
		// Misc. data
		List_0 = 0x64,
		List_s = 0x65,
		List = 0x66,
		Hash_0 = 0x67,
		Hash_s = 0x68,
		Hash = 0x69,
		Lditer = 0x6a,
		Ldtype = 0x6b,
		// Fields
		Ldfld = 0x6c,
		Stfld = 0x6d,
		Ldsfld = 0x6e,
		Stsfld = 0x6f,
		// Named member access
		Ldmem = 0x70,
		Stmem = 0x71,
		// Indexers
		Ldidx_1 = 0x72,
		Ldidx_s = 0x73,
		Ldidx = 0x74,
		Stidx_1 = 0x75,
		Stidx_s = 0x76,
		Stidx = 0x77,
		// Global/static functions
		Ldsfn = 0x78,
		// Type tokens
		Ldtypetkn = 0x79,
		// Exception handling
		Throw = 0x7a,
		Rethrow = 0x7b,
		Leave_s = 0x7c,
		Leave = 0x7d,
		Endfinally = 0x7e,
	}

	public struct StackChange
	{
		public StackChange(int removed, int added)
		{
			Removed = removed;
			Added = added;
		}

		public readonly int Removed;
		public readonly int Added;

		public override string ToString()
		{
			return string.Format("-{0:D} +{1:D}", Removed, Added);
		}
		
		public static int operator +(StackChange change, int stackCount)
		{
			if (stackCount < change.Removed)
				throw new InvalidOperationException("Not enough values on stack.");
			return stackCount - change.Removed + change.Added;
		}

		public static int operator +(int stackCount, StackChange change)
		{
			return change + stackCount;
		}

		public static readonly StackChange Empty = new StackChange();
		public static readonly StackChange AddOne = new StackChange(0, 1);
	}

	/// <summary>
	/// Encapsulates a simple instruction that has a fixed size and takes no arguments.
	/// </summary>
	public sealed class SimpleInstruction : Instruction
	{
		/// <summary>
		/// Initializes a new <see cref="SimpleInstruction"/> with the specified opcode.
		/// </summary>
		/// <param name="opcode">The opcode to assign to the instruction.</param>
		/// <remarks>The opcode is NOT validated. Do not pass in opcodes for instructions that require any arguments.</remarks>
		public SimpleInstruction(Opcode opcode)
		{
			this.opcode = opcode;
		}

		private Opcode opcode;
		/// <summary>
		/// Gets the opcode associated with the instruction.
		/// </summary>
		public Opcode Opcode { get { return opcode; } }

		public override StackChange StackChange
		{
			get
			{
				switch (opcode)
				{
					case Opcode.Nop:
						return StackChange.Empty;
					case Opcode.Dup:
						return new StackChange(1, 2);
					case Opcode.Pop:
						return new StackChange(1, 0);
					case Opcode.Ldargc:
						return new StackChange(0, 1);
					case Opcode.Ret:
						return new StackChange(1, 0);
					case Opcode.Retnull:
						return StackChange.Empty;
					case Opcode.Apply:
					case Opcode.Add:
					case Opcode.Sub:
					case Opcode.Or:
					case Opcode.Xor:
					case Opcode.Mul:
					case Opcode.Div:
					case Opcode.Mod:
					case Opcode.And:
					case Opcode.Pow:
					case Opcode.Shl:
					case Opcode.Shr:
					case Opcode.Hashop:
					case Opcode.Dollar:
					case Opcode.Eq:
					case Opcode.Cmp:
					case Opcode.Lt:
					case Opcode.Gt:
					case Opcode.Lte:
					case Opcode.Gte:
					case Opcode.Concat:
						return new StackChange(2, 1);
					case Opcode.Plus:
					case Opcode.Neg:
					case Opcode.Not:
						return new StackChange(1, 1);
					case Opcode.Lditer:
					case Opcode.Ldtype:
						return new StackChange(1, 1);
					case Opcode.Throw:
						return new StackChange(1, 0);
					case Opcode.Rethrow:
					case Opcode.Endfinally:
						return StackChange.Empty;
				}
				throw new InvalidOperationException("Invalid opcode for simple instruction.");
			}
		}

		public override int GetSize() { return 1; }

		public override byte[] GetBytes()
		{
			return new byte[] { (byte)opcode };
		}

		public override string ToString()
		{
			return OpcodeToString(this.opcode);
		}

		public static SimpleInstruction FromOperator(BinaryOperator op)
		{
			Opcode opcode;
			switch (op)
			{
				case BinaryOperator.Equality: opcode = Opcode.Eq; break;
				case BinaryOperator.LessThan: opcode = Opcode.Lt; break;
				case BinaryOperator.LessEqual: opcode = Opcode.Lte; break;
				case BinaryOperator.GreaterThan: opcode = Opcode.Gt; break;
				case BinaryOperator.GreaterEqual: opcode = Opcode.Gte; break;
				case BinaryOperator.Hash: opcode = Opcode.Hashop; break;
				case BinaryOperator.Dollar: opcode = Opcode.Dollar; break;
				case BinaryOperator.ShiftLeft: opcode = Opcode.Shl; break;
				case BinaryOperator.ShiftRight: opcode = Opcode.Shr; break;
				case BinaryOperator.Addition: opcode = Opcode.Add; break;
				case BinaryOperator.Subtraction: opcode = Opcode.Sub; break;
				case BinaryOperator.BitwiseOr: opcode = Opcode.Or; break;
				case BinaryOperator.BitwiseXor: opcode = Opcode.Xor; break;
				case BinaryOperator.Multiplication: opcode = Opcode.Mul; break;
				case BinaryOperator.Division: opcode = Opcode.Div; break;
				case BinaryOperator.Modulo: opcode = Opcode.Mod; break;
				case BinaryOperator.BitwiseAnd: opcode = Opcode.And; break;
				case BinaryOperator.Exponentiation: opcode = Opcode.Pow; break;
				case BinaryOperator.FunctionApplication: opcode = Opcode.Apply; break;
				case BinaryOperator.Concatenation: opcode = Opcode.Concat; break;
				case BinaryOperator.Comparison: opcode = Opcode.Cmp; break;
				default: throw new ArgumentException("Invalid operator for SimpleInstruction.FromOperator.");
			}
			return new SimpleInstruction(opcode);
		}

		public static SimpleInstruction FromOperator(UnaryOperator op)
		{
			Opcode opcode;
			switch (op)
			{
				case UnaryOperator.Plus: opcode = Opcode.Plus; break;
				case UnaryOperator.Minus: opcode = Opcode.Neg; break;
				case UnaryOperator.BitwiseNot: opcode = Opcode.Not; break;
				default: throw new ArgumentException("Invalid operator for SimpleInstruction.FromOperator.");
			}
			return new SimpleInstruction(opcode);
		}

		public static string OpcodeToString(Opcode opcode)
		{
			switch (opcode)
			{
				case Opcode.Nop: return "nop";
				case Opcode.Dup: return "dup";
				case Opcode.Pop: return "pop";
				case Opcode.Ldargc: return "ldargc";
				case Opcode.Apply: return "apply";
				case Opcode.Ret: return "ret";
				case Opcode.Retnull: return "retnull";
				case Opcode.Add: return "add";
				case Opcode.Sub: return "sub";
				case Opcode.Or: return "or";
				case Opcode.Xor: return "xor";
				case Opcode.Mul: return "mul";
				case Opcode.Div: return "div";
				case Opcode.Mod: return "mod";
				case Opcode.And: return "and";
				case Opcode.Pow: return "pow";
				case Opcode.Shl: return "shl";
				case Opcode.Shr: return "shr";
				case Opcode.Hashop: return "hashop";
				case Opcode.Dollar: return "dollar";
				case Opcode.Plus: return "plus";
				case Opcode.Neg: return "neg";
				case Opcode.Not: return "not";
				case Opcode.Eq: return "eq";
				case Opcode.Cmp: return "cmp";
				case Opcode.Lt: return "lt";
				case Opcode.Gt: return "gt";
				case Opcode.Lte: return "lte";
				case Opcode.Gte: return "gte";
				case Opcode.Concat: return "concat";
				case Opcode.Lditer: return "lditer";
				case Opcode.Ldtype: return "ldtype";
				case Opcode.Throw: return "throw";
				case Opcode.Rethrow: return "rethrow";
				case Opcode.Endfinally: return "endfinally";
			}
			throw new ArgumentOutOfRangeException("opcode", "Invalid opcode for simple instruction.");
		}
	}

	/// <summary>
	/// Represents a branching instruction.
	/// </summary>
	public sealed class Branch : Instruction
	{
		private Branch(BranchCondition cond, Label target)
		{
			this.cond = cond;
			this.target = target;
		}

		private BranchCondition cond;
		/// <summary>
		/// Gets the type of the branching instruction.
		/// </summary>
		public BranchCondition Condition { get { return cond; } }

		private Label target;
		/// <summary>
		/// Gets the target of the branch.
		/// </summary>
		public Label Target { get { return target; } }

		internal bool isShortJump = true;
		internal int targetOffset = 0;
		internal Instruction targetInstr = null;

		private uint type;
		/// <summary>
		/// Gets the type to branch on. Returns 0 if <see cref="Condition"/> is not <see cref="BranchCondition.TypeEquals"/>.
		/// </summary>
		public uint Type { get { return type; } }

		public override StackChange StackChange
		{
			get
			{
				switch (cond)
				{
					case BranchCondition.Unconditional:
						return StackChange.Empty;
					case BranchCondition.IfNull:
					case BranchCondition.IfNotNull:
					case BranchCondition.IfFalse:
					case BranchCondition.IfTrue:
						return new StackChange(1, 0);
					case BranchCondition.RefEquals:
					case BranchCondition.RefNotEquals:
						return new StackChange(2, 0);
					case BranchCondition.TypeEquals:
						return new StackChange(1, 0);
					case BranchCondition.Leave:
						return StackChange.Empty;
					default:
						throw new InvalidOperationException();
				}
			}
		}

		public override int GetSize()
		{
			int size = 1;
			if (cond == BranchCondition.TypeEquals)
				size += 4; // for the type ID
			// And the jump target needs space too!
			size += isShortJump ? 1 : 4;

			return size;
		}

		public override byte[] GetBytes()
		{
			if (targetInstr == null)
				throw new InvalidOperationException("Cannot get the bytes of an uninitialised branch instruction.");
			if (isShortJump && (targetOffset < sbyte.MinValue || targetOffset > sbyte.MaxValue))
				throw new InvalidOperationException("Short branch instruction with out-of-range jump offset.");

			byte opcode;
			if (cond == BranchCondition.Leave)
				opcode = (byte)(isShortJump ? Opcode.Leave_s : Opcode.Leave);
			else
				opcode = (byte)((isShortJump ? ShortBranchBase : LongBranchBase) + (byte)cond);

			var size = isShortJump ? 2 : 5;
			if (cond == BranchCondition.TypeEquals)
				size += 4;

			var output = new byte[size];
			output[0] = opcode;
			if (cond == BranchCondition.TypeEquals)
			{
				output.CopyBytes(type, 1);

				if (isShortJump)
					output[5] = unchecked((byte)checked((sbyte)targetOffset));
				else
					output.CopyBytes(targetOffset, 5);
			}
			else
			{
				if (isShortJump)
					output[1] = unchecked((byte)checked((sbyte)targetOffset));
				else
					output.CopyBytes(targetOffset, 1);
			}

			return output;
		}

		public override string ToString()
		{
			var sb = new StringBuilder(16);
			switch (cond)
			{
				case BranchCondition.Unconditional:
					sb.Append("br");
					break;
				case BranchCondition.IfNull:
					sb.Append("brnull");
					break;
				case BranchCondition.IfNotNull:
					sb.Append("brinst");
					break;
				case BranchCondition.IfFalse:
					sb.Append("brfalse");
					break;
				case BranchCondition.IfTrue:
					sb.Append("brtrue");
					break;
				case BranchCondition.RefEquals:
					sb.Append("brref");
					break;
				case BranchCondition.RefNotEquals:
					sb.Append("brnref");
					break;
				case BranchCondition.TypeEquals:
					sb.Append("brtype");
					break;
				case BranchCondition.Leave:
					sb.Append("leave");
					break;
			}

			if (isShortJump)
				sb.Append(".s ");
			else
				sb.Append(' ');

			if (cond == BranchCondition.TypeEquals)
			{
				sb.Append(type.ToString("X8"));
				sb.Append(' ');
			}

			sb.Append("OV_");
			sb.Append((this.ByteOffset + this.GetSize() + targetOffset).ToString("X4"));

			return sb.ToString();
		}

		public static Branch Always(Label target)
		{
			return new Branch(BranchCondition.Unconditional, target);
		}
		public static Branch IfNull(Label target)
		{
			return new Branch(BranchCondition.IfNull, target);
		}
		public static Branch IfNotNull(Label target)
		{
			return new Branch(BranchCondition.IfNotNull, target);
		}
		public static Branch IfFalse(Label target)
		{
			return new Branch(BranchCondition.IfFalse, target);
		}
		public static Branch IfTrue(Label target)
		{
			return new Branch(BranchCondition.IfTrue, target);
		}
		public static Branch RefEquals(Label target)
		{
			return new Branch(BranchCondition.RefEquals, target);
		}
		public static Branch RefNotEquals(Label target)
		{
			return new Branch(BranchCondition.RefNotEquals, target);
		}
		public static Branch TypeEquals(Label target, uint type)
		{
			return new Branch(BranchCondition.TypeEquals, target) { type = type };
		}
		public static Branch Leave(Label target)
		{
			return new Branch(BranchCondition.Leave, target);
		}

		private const byte ShortBranchBase = (byte)Opcode.Br_s; // Short branch instruction base (sbyte argument)
		private const byte LongBranchBase = (byte)Opcode.Br; // Long branch instruction base (int argument)
	}

	public enum BranchCondition
	{
		Unconditional = 0,
		IfNull = 1,
		IfNotNull = 2,
		IfFalse = 3,
		IfTrue = 4,
		RefEquals = 5,
		RefNotEquals = 6,
		TypeEquals = 7,
		// Superspecial value! Emits a leave/leave.s instruction instead of a normal branch! OMG!
		// (This is also an unconditional branch.)
		Leave = 8,
	}

	public sealed class Switch : Instruction
	{
		public Switch(List<Label> targets)
		{
			if (targets == null)
				throw new ArgumentNullException("targets");

			this.targets = new List<SwitchTarget>(targets.Count);
			for (var i = 0; i < targets.Count; i++)
				this.targets.Add(new SwitchTarget { Label = targets[i] });
		}

		internal Switch()
		{
			this.targets = new List<SwitchTarget>();
		}

		internal List<SwitchTarget> targets;

		/// <summary>
		/// Gets the number of targets the switch has.
		/// </summary>
		public int TargetCount { get { return targets.Count; } }

		internal bool isShortJump = true;

		public override StackChange StackChange { get { return new StackChange(1, 0); } }

		/// <summary>
		/// Gets an array of the targets of the switch instruction.
		/// </summary>
		public Label[] GetTargets()
		{
			return targets.Select(t => t.Label).ToArray();
		}

		internal void SetTargets(IEnumerable<Label> targets)
		{
			if (targets == null)
				throw new ArgumentNullException("targets");

			this.targets = new List<SwitchTarget>(
				targets.Select(t => new SwitchTarget { Label = t })
			);
		}

		public override int GetSize()
		{
			return 1 +
				/* table size */ sizeof(ushort) +
				(isShortJump ? sizeof(sbyte) : sizeof(int)) * targets.Count;
		}

		public override byte[] GetBytes()
		{
			var output = new byte[GetSize()];
			output[0] = (byte)(isShortJump ? Opcode.Switch_s : Opcode.Switch);

			output.CopyBytes((ushort)targets.Count, 1);

			for (var i = 0; i < targets.Count; i++)
			{
				var target = targets[i];
				if (isShortJump)
					output[3 + i] = unchecked((byte)checked((sbyte)target.Offset));
				else
					output.CopyBytes(target.Offset, 3 + 4 * i);
			}

			return output;
		}

		public override string ToString()
		{
			var endOffset = this.ByteOffset + GetSize();
			var sb = new StringBuilder(32);

			if (isShortJump)
				sb.Append("switch.s { ");
			else
				sb.Append("switch { ");

			for (var i = 0; i < targets.Count; i++)
			{
				var target = targets[i];
				if (i > 0)
					sb.Append(", ");
				sb.Append("OV_");
				sb.Append((endOffset + target.Offset).ToString("X4"));
			}

			sb.Append(" }");

			return sb.ToString();
		}

		internal struct SwitchTarget
		{
			public Label Label;
			public int Offset;
			public Instruction Instr;
		}
	}

	public sealed class LoadLocal : Instruction
	{
		/// <summary>
		/// Initializes a new <see cref="LoadLocal"/> instance.
		/// </summary>
		/// <param name="local">The local variable that the instruction refers to.</param>
		public LoadLocal(LocalVariable local)
		{
			if (local == null)
				throw new ArgumentNullException("local");

			this.local = local;
		}

		private LocalVariable local;
		/// <summary>
		/// Gets the local variable that the instruction refers to.
		/// </summary>
		public LocalVariable Local { get { return local; } }

		/// <summary>
		/// Gets a value indicating whether the local is an argument.
		/// </summary>
		public bool IsArgument { get { return local.IsParam; } }

		public override StackChange StackChange { get { return new StackChange(0, 1); } }

		public override int GetSize()
		{
			if (local.Index <= MaxIndexForSingleByte)
				return 1;
			if (local.Index <= byte.MaxValue)
				return 2;
			if (local.Index <= ushort.MaxValue)
				return 3;
			throw new InvalidOperationException(string.Format("Local index cannot be greater than {0}.", ushort.MaxValue));
		}

		public override byte[] GetBytes()
		{
			if (local.Index <= MaxIndexForSingleByte)
				return new byte[] { (byte)((local.IsParam ? LdargBase : LdlocBase) + local.Index) };
			if (local.Index <= byte.MaxValue)
				return new byte[]
				{
					local.IsParam ? LdargShort : LdlocShort,
					(byte)local.Index,
				};
			if (local.Index <= ushort.MaxValue)
			{
				byte argLow = (byte)(local.Index & 0x00ff);
				byte argHigh = (byte)((local.Index & 0xff00) >> 8);
				return new byte[]
				{
					local.IsParam ? LdargLong : LdlocLong,
					argLow, argHigh,
				};
			}
			throw new InvalidOperationException(string.Format("Local index cannot be greater than {0}.", ushort.MaxValue));
		}

		public override string ToString()
		{
			var sb = new StringBuilder(10);

			sb.Append(local.IsParam ? "ldarg" : "ldloc");

			if (local.Index <= MaxIndexForSingleByte)
				sb.Append('.');
			else if (local.Index <= byte.MaxValue)
				sb.Append(".s ");
			else
				sb.Append(' ');

			sb.Append(local.Index);

			if (local.Name != null)
			{
				sb.Append(" [");
				sb.Append(local.Name);
				sb.Append(']');
			}

			return sb.ToString();
		}

		// If the local index is between 0 and 3, there is a single-byte instruction.
		private const int MaxIndexForSingleByte = 3;

		private const byte LdargBase = (byte)Opcode.Ldarg_0;
		private const byte LdargShort = (byte)Opcode.Ldarg_s;
		private const byte LdargLong = (byte)Opcode.Ldarg;

		private const byte LdlocBase = (byte)Opcode.Ldloc_0;
		private const byte LdlocShort = (byte)Opcode.Ldloc_s;
		private const byte LdlocLong = (byte)Opcode.Ldloc;
	}

	public sealed class StoreLocal : Instruction
	{
		public StoreLocal(LocalVariable local)
		{
			if (local == null)
				throw new ArgumentNullException("local");

			this.local = local;
		}

		private LocalVariable local;
		/// <summary>
		/// Gets the local variable that the instruction refers to.
		/// </summary>
		public LocalVariable Local { get { return local; } }

		/// <summary>
		/// Gets a value indicating whether the local is an argument.
		/// </summary>
		public bool IsArgument { get { return local.IsParam; } }

		public override StackChange StackChange { get { return new StackChange(1, 0); } }

		public override int GetSize()
		{
			if (!local.IsParam && local.Index <= MaxIndexForSingleByte)
				return 1;
			if (local.Index <= byte.MaxValue)
				return 2;
			if (local.Index <= ushort.MaxValue)
				return 3;
			throw new InvalidOperationException(string.Format("Local index cannot be greater than {0}.", ushort.MaxValue));
		}

		public override byte[] GetBytes()
		{
			if (!local.IsParam && local.Index <= MaxIndexForSingleByte)
				return new byte[] { (byte)(StlocBase + local.Index) };
			if (local.Index <= byte.MaxValue)
				return new byte[] { local.IsParam ? StargShort : StlocShort, (byte)local.Index };
			if (local.Index <= ushort.MaxValue)
			{
				byte argLow = (byte)(local.Index & 0x00ff);
				byte argHigh = (byte)((local.Index & 0xff00) >> 8);
				return new byte[]
				{
					local.IsParam ? StargLong : StlocLong,
					argLow, argHigh,
				};
			}
			throw new InvalidOperationException(string.Format("Local index cannot be greater than {0}.", ushort.MaxValue));
		}

		public override string ToString()
		{
			var sb = new StringBuilder(10);

			sb.Append(local.IsParam ? "starg" : "stloc");

			if (!local.IsParam && local.Index <= MaxIndexForSingleByte)
				sb.Append('.');
			else if (local.Index <= byte.MaxValue)
				sb.Append(".s ");
			else
				sb.Append(' ');

			sb.Append(local.Index);

			if (local.Name != null)
			{
				sb.Append(" [");
				sb.Append(local.Name);
				sb.Append(']');
			}

			return sb.ToString();
		}

		private const int MaxIndexForSingleByte = 3;

		private const byte StargShort = (byte)Opcode.Starg_s;
		private const byte StargLong = (byte)Opcode.Starg;

		private const byte StlocBase = (byte)Opcode.Stloc_0;
		private const byte StlocShort = (byte)Opcode.Stloc_s;
		private const byte StlocLong = (byte)Opcode.Stloc;
	}

	public sealed class LoadConstant : Instruction
	{
		private LoadConstant(ConstantKind kind)
		{
			this.kind = kind;
		}

		private ConstantKind kind;
		/// <summary>
		/// Gets a description of the constant that the instruction loads.
		/// </summary>
		public ConstantKind Kind { get { return kind; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize() { return 1; }

		public override byte[] GetBytes()
		{
			Opcode opc;
			switch (kind)
			{
				case ConstantKind.Null: opc = Opcode.Ldnull; break;
				case ConstantKind.False: opc = Opcode.Ldfalse; break;
				case ConstantKind.True: opc = Opcode.Ldtrue; break;
				default: throw new InvalidOperationException("LoadConstant.Kind has an invalid value.");
			}
			return new byte[] { (byte)opc };
		}

		public override string ToString()
		{
			switch (kind)
			{
				case ConstantKind.Null:
					return "ldnull";
				case ConstantKind.False:
					return "ldfalse";
				case ConstantKind.True:
					return "ldtrue";
			}
			throw new InvalidOperationException("LoadConstant.kind has an invalid value.");
		}

		public static LoadConstant Null()
		{
			return new LoadConstant(ConstantKind.Null);
		}

		public static LoadConstant False()
		{
			return new LoadConstant(ConstantKind.False);
		}

		public static LoadConstant True()
		{
			return new LoadConstant(ConstantKind.True);
		}

		public enum ConstantKind
		{
			Null,
			False,
			True,
		}
	}

	public sealed class LoadConstantInt : Instruction
	{
		public LoadConstantInt(long value)
		{
			this.value = value;
		}

		private long value;
		/// <summary>
		/// Gets the constant value that the instruction loads.
		/// </summary>
		public long Value { get { return value; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize()
		{
			// Note: I use sizeof() for semantic reasons!
			if (value >= MinSingleByteValue && value <= MaxSingleByteValue)
				return 1;
			if (value >= sbyte.MinValue && value <= sbyte.MaxValue)
				return 1 + sizeof(sbyte);
			if (value >= int.MinValue && value <= int.MaxValue)
				return 1 + sizeof(int);
			return 1 + sizeof(long);
		}

		public override byte[] GetBytes()
		{
			if (value >= MinSingleByteValue && value <= MaxSingleByteValue)
				return new byte[] { (byte)(Ldc_iBase + value) };
			if (value >= sbyte.MinValue && value <= sbyte.MaxValue)
				return new byte[]
				{
					(byte)Opcode.Ldc_i_s,
					unchecked((byte)((sbyte)value)),
				};
			if (value >= int.MinValue && value <= int.MaxValue)
			{
				var output = new byte[5];
				output[0] = (byte)Opcode.Ldc_i_m;

				output.CopyBytes((int)value, 1);

				return output;
			}

			{
				var output = new byte[9];
				output[0] = (byte)Opcode.Ldc_i;

				output.CopyBytes(value, 1);

				return output;
			}
		}

		public override string ToString()
		{
			var sb = new StringBuilder(10);
			sb.Append("ldc.i");

			if (value == -1)
				sb.Append(".m1");
			else
			{
				var size = GetSize();
				if (size == 1)
					sb.Append('.');
				else if (size == 2)
					sb.Append(".s 0x");
				else if (size == 5)
					sb.Append(".m 0x");
				else if (size == 9)
					sb.Append(" 0x");
				sb.Append(value.ToString("x"));
			}

			return sb.ToString();
		}

		private const byte Ldc_iBase = (byte)Opcode.Ldc_i_0;
		private const long MinSingleByteValue = -1;
		private const long MaxSingleByteValue = 8;
	}

	public sealed class LoadConstantUInt : Instruction
	{
		public LoadConstantUInt(ulong value)
		{
			this.value = value;
		}

		private ulong value;
		/// <summary>
		/// Gets the constant value that the instruction loads.
		/// </summary>
		public ulong Value { get { return value; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize() { return 1 + sizeof(ulong); }

		public override byte[] GetBytes()
		{
			var output = new byte[9];
			output[0] = (byte)Opcode.Ldc_u;

			output.CopyBytes(value, 1);

			return output;
		}

		public override string ToString()
		{
			return "ldc.u 0x" + value.ToString("x");
		}
	}

	public sealed class LoadConstantReal : Instruction
	{
		public LoadConstantReal(double value)
		{
			this.value = value;
		}

		private double value;
		/// <summary>
		/// Gets the constant value that the instruction loads.
		/// </summary>
		public double Value { get { return value; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize() { return 1 + sizeof(double); }

		public override byte[] GetBytes()
		{
			var output = new byte[9];
			output[0] = (byte)Opcode.Ldc_r;

			output.CopyBytes(value, 1);

			return output;
		}

		public override string ToString()
		{
			return "ldc.r " + value.ToStringInvariant();
		}
	}

	public sealed class LoadString : Instruction
	{
		public LoadString(uint stringRef)
		{
			this.stringRef = stringRef;
		}

		private uint stringRef;
		/// <summary>
		/// Gets the string that is loaded by the instruction.
		/// </summary>
		public uint StringRef { get { return stringRef; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize() { return 5; }

		public override byte[] GetBytes()
		{
			var output = new byte[5];
			output[0] = (byte)Opcode.Ldstr;

			output.CopyBytes(stringRef, 1);

			return output;
		}

		public override string ToString()
		{
			return string.Format("ldstr {0:X8}", stringRef);
		}
	}

	public sealed class LoadEnum : Instruction
	{
		public LoadEnum(uint type, long value)
		{
			this.type = type;
			this.value = value;
		}

		private uint type;
		/// <summary>
		/// Gets the enum type that the instruction loads a value of.
		/// </summary>
		public uint Type { get { return type; } }

		private long value;
		/// <summary>
		/// Gets the underlying integral value that the instruction loads.
		/// </summary>
		public long Value { get { return value; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize()
		{
			if (value >= int.MinValue && value <= int.MaxValue)
				return BaseSize + sizeof(int);

			return BaseSize + sizeof(long);
		}

		public override byte[] GetBytes()
		{
			byte[] output;
			if (value >= int.MinValue && value <= int.MaxValue)
			{
				output = new byte[BaseSize + sizeof(int)];
				output[0] = (byte)Opcode.Ldenum_s;

				output.CopyBytes((int)value, 5);
			}
			else
			{
				output = new byte[BaseSize + sizeof(long)];
				output[0] = (byte)Opcode.Ldenum;

				output.CopyBytes(value, 5);
			}

			output.CopyBytes(type, 1);

			return output;
		}

		public override string ToString()
		{
			var sb = new StringBuilder(32);
			if (value >= int.MinValue && value <= int.MaxValue)
				sb.Append("ldenum.s ");
			else
				sb.Append("ldenum ");

			sb.Append(type.ToString("X8"));
			if (Parent != null)
			{
				sb.Append(" [");
				sb.Append(Parent.Module.GetType(type).FullName);
				sb.Append(']');
			}
			sb.Append(" 0x");
			sb.Append(value.ToString("x"));

			return sb.ToString();
		}

		private const int BaseSize = 1 + 4 /* type ID */;
	}

	public sealed class NewObject : Instruction
	{
		public NewObject(uint type, int argCount)
		{
			this.type = type;
			this.argCount = argCount;
		}

		private uint type;
		/// <summary>
		/// Gets the ID of the type that the instruction represents a creation of.
		/// </summary>
		public uint Type { get { return type; } }

		private int argCount;
		/// <summary>
		/// Gets the number of arguments that are passed to the constructor,
		/// not including the new instance.
		/// </summary>
		public int ArgCount { get { return argCount; } }

		public override StackChange StackChange
		{
			get { return new StackChange(argCount, 1); }
		}

		public override int GetSize()
		{
			if (argCount <= byte.MaxValue)
				return 1 + 4 + 1; // instr + sizeof(uint) + sizeof(byte)
			return 1 + 4 + 2; // instr + sizeof(uint) + sizeof(ushort)
		}

		public override byte[] GetBytes()
		{
			// newobj(.s)  u4:type  [ub|u2]:argCount
			byte[] output;
			if (argCount <= byte.MaxValue)
			{
				output = new byte[1 + 4 + 1];
				output[0] = (byte)Opcode.Newobj_s;
				output[5] = (byte)argCount;
			}
			else
			{
				output = new byte[1 + 4 + 2];
				output[0] = (byte)Opcode.Newobj;
				output.CopyBytes((ushort)argCount, 5);
			}
			output.CopyBytes(type, 1); // Copy type ID

			return output;
		}

		public override string ToString()
		{
			var sb = new StringBuilder(10);

			sb.Append("newobj");
			if (argCount <= byte.MaxValue)
				sb.Append(".s ");
			else
				sb.Append(' ');

			sb.Append(type.ToString("X8"));
			if (Parent != null)
			{
				sb.Append(" [");
				sb.Append(Parent.Module.GetType(type).FullName);
				sb.Append(']');
			}
			sb.Append(' ');
			sb.Append(argCount);

			return sb.ToString();
		}
	}

	public sealed class Call : Instruction
	{
		public Call(int argCount)
		{
			this.argCount = argCount;
		}

		private int argCount;
		/// <summary>
		/// Gets the number of arguments that the call instruction passes to the method.
		/// </summary>
		public int ArgCount { get { return argCount; } }

		public override StackChange StackChange { get { return new StackChange(removed: argCount + 1, added: 1); } }

		public override int GetSize()
		{
			if (argCount <= MaxArgCountForSingleByte)
				return 1;
			if (argCount <= byte.MaxValue)
				return 2;
			return 3;
		}

		public override byte[] GetBytes()
		{
			if (argCount <= MaxArgCountForSingleByte)
				return new byte[] { (byte)((int)Opcode.Call_0 + argCount) };
			if (argCount <= byte.MaxValue)
				return new byte[]
				{
					(byte)Opcode.Call_s,
					(byte)argCount,
				};
			if (argCount <= ushort.MaxValue)
			{
				var argcLow = (byte)(argCount & 0x00ff);
				var argcHigh = (byte)((argCount & 0xff00) >> 8);
				return new byte[]
				{
					(byte)Opcode.Call,
					argcLow, argcHigh,
				};
			}
			throw new InvalidOperationException("Call.ArgCount is out of range.");
		}

		public override string ToString()
		{
			var sb = new StringBuilder(10);

			sb.Append("call");
			if (argCount <= MaxArgCountForSingleByte)
				sb.Append('.');
			else if (argCount <= byte.MaxValue)
				sb.Append(".s ");
			else
				sb.Append(' ');
			sb.Append(argCount);

			return sb.ToString();
		}

		private const int MaxArgCountForSingleByte = 3;
	}

	public sealed class StaticCall : Instruction
	{
		public StaticCall(uint method, int argCount)
		{
			this.method = method;
			this.argCount = argCount;
		}

		private uint method;
		/// <summary>
		/// Gets the token ID of the method that the instruction invokes.
		/// </summary>
		public uint Method { get { return method; } }

		private int argCount;
		/// <summary>
		/// Gets the number of arguments that the call instruction passes to the method.
		/// </summary>
		public int ArgCount { get { return argCount; } }

		/// <summary>
		/// Gets the effective argument count of this instruction. If the static call applies to an instance method,
		/// then this will be <see cref="ArgCount"/> plus one; otherwise, it will be the same as <see cref="ArgCount"/>.
		/// </summary>
		internal int FinalArgCount
		{
			get
			{
				if (this.Parent == null)
					throw new InvalidOperationException("Cannot get the final argument count of a static call instruction without a parent.");
				return argCount +
					(this.Parent.Module.GetMethod(this.method).IsStatic ? 0 : 1);
			}
		}

		public override StackChange StackChange
		{
			get
			{
				return new StackChange(removed: FinalArgCount, // arguments
					added: 1); // return value
			}
		}

		public override int GetSize()
		{
			return 1 + 4 /* method ID */ +
				(FinalArgCount <= byte.MaxValue ? 1 : 2);
		}

		public override byte[] GetBytes()
		{
			byte[] output;

			var finalArgc = FinalArgCount;
			if (finalArgc <= byte.MaxValue)
			{
				output = new byte[6];
				output[0] = (byte)Opcode.Scall_s;

				output[5] = (byte)finalArgc;
			}
			else
			{
				output = new byte[7];
				output[0] = (byte)Opcode.Scall;

				var argcLow = (byte)(finalArgc & 0x00ff);
				var argcHigh = (byte)((finalArgc & 0xff00) >> 8);
				output[5] = argcLow;
				output[6] = argcHigh;
			}

			output.CopyBytes(method, 1);

			return output;
		}

		public override string ToString()
		{
			var sb = new StringBuilder(16);

			sb.Append("scall");
			if (argCount <= byte.MaxValue)
				sb.Append(".s ");
			else
				sb.Append(' ');
			sb.Append(method.ToString("X8"));
			if (Parent != null)
			{
				sb.Append(" [");
				sb.Append(Parent.Module.GetMethod(method).FullName);
				sb.Append(']');
			}
			sb.Append(' ');
			sb.Append(FinalArgCount);

			return sb.ToString();
		}
	}

	public sealed class StaticApply : Instruction
	{
		public StaticApply(uint method)
		{
			this.method = method;
		}

		private uint method;
		/// <summary>
		/// Gets the method that the static application invokes.
		/// </summary>
		public uint Method { get { return method; } }

		public override StackChange StackChange { get { return new StackChange(1, 1); } }

		public override int GetSize() { return 5; }

		public override byte[] GetBytes()
		{
			var output = new byte[5];
			output[0] = (byte)Opcode.Sapply;

			output.CopyBytes(method, 1);

			return output;
		}

		public override string ToString()
		{
			if (Parent != null)
				return string.Format("sapply {0:X8} [{1}]", method, Parent.Module.GetMethod(method).FullName);
			return "sapply " + method.ToString("x8");
		}
	}

	public sealed class CallMember : Instruction
	{
		public CallMember(uint memberName, int argCount)
		{
			this.memberName = memberName;
			this.argCount = argCount;
		}

		private uint memberName;
		/// <summary>
		/// Gets the string ID of the name of the member to be invoked.
		/// </summary>
		public uint MemberName { get { return memberName; } }

		private int argCount;
		/// <summary>
		/// Gets the number of arguments to invoke the member with.
		/// </summary>
		public int ArgCount { get { return argCount; } }

		public override StackChange StackChange
		{
			get
			{
				return new StackChange(removed: argCount + 1, // arguments + instance
					added: 1); // return value
			}
		}

		public override int GetSize()
		{
			return 1 + 4 /*member ID*/ +
				(argCount <= byte.MaxValue ? 1 : 2);
		}

		public override byte[] GetBytes()
		{
			var output = new byte[GetSize()];

			if (argCount <= byte.MaxValue)
			{
				output[0] = (byte)Opcode.Callmem_s;
				output[5] = (byte)argCount;
			}
			else
			{
				output[1] = (byte)Opcode.Callmem;
				output.CopyBytes((ushort)argCount, 5);
			}

			output.CopyBytes(memberName, 1);

			return output;
		}

		public override string ToString()
		{
			var output = new StringBuilder(32);

			output.Append("callmem");
			if (argCount <= byte.MaxValue)
				output.Append(".s");

			output.Append(' ');

			output.Append(memberName.ToString("X8"));
			if (Parent != null)
				output.AppendFormat("[{0}]", Parent.Module.Members.Strings[memberName]);

			output.Append(' ');
			output.Append(argCount);

			return output.ToString();
		}
	}

	public sealed class CreateList : Instruction
	{
		public CreateList(uint length)
		{
			this.length = length;
		}

		private uint length;
		/// <summary>
		/// Gets the length of the list to be created.
		/// </summary>
		public uint Length { get { return length; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize()
		{
			return length == 0 ? 1 :
				length <= byte.MaxValue ? 2 :
				5; // 1 + sizeof(uint)
		}

		public override byte[] GetBytes()
		{
			if (length == 0)
				return new byte[] { (byte)Opcode.List_0 };
			else if (length <= byte.MaxValue)
				return new byte[] { (byte)Opcode.List_s, (byte)length };
			else
			{
				var output = new byte[5];
				output[0] = (byte)Opcode.List;

				output.CopyBytes(length, 1);

				return output;
			}
		}

		public override string ToString()
		{
			var sb = new StringBuilder(10);
			sb.Append("list");

			if (length == 0)
				sb.Append('.');
			else if (length <= byte.MaxValue)
				sb.Append(".s ");
			else
				sb.Append(' ');

			sb.Append(length);

			return sb.ToString();
		}
	}

	public sealed class CreateHash : Instruction
	{
		public CreateHash(uint length)
		{
			this.length = length;
		}

		private uint length;
		/// <summary>
		/// Gets the length of the hash to be created.
		/// </summary>
		public uint Length { get { return length; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize()
		{
			return length == 0 ? 1 :
				length <= byte.MaxValue ? 2 :
				5; // 1 + sizeof(uint)
		}

		public override byte[] GetBytes()
		{
			if (length == 0)
				return new byte[] { (byte)Opcode.Hash_0 };
			else if (length <= byte.MaxValue)
				return new byte[] { (byte)Opcode.Hash_s, (byte)length };
			else
			{
				var output = new byte[5];
				output[0] = (byte)Opcode.Hash;

				output.CopyBytes(length, 1);

				return output;
			}
		}

		public override string ToString()
		{
			var sb = new StringBuilder(10);
			sb.Append("hash");

			if (length == 0)
				sb.Append('.');
			else if (length <= byte.MaxValue)
				sb.Append(".s ");
			else
				sb.Append(' ');

			sb.Append(length);

			return sb.ToString();
		}
	}

	public sealed class LoadField : Instruction
	{
		private LoadField(uint field, bool isStatic)
		{
			this.field = field;
			this.isStatic = isStatic;
		}

		private uint field;
		/// <summary>
		/// Gets the ID 
		/// </summary>
		public uint Field { get { return field; } }

		private bool isStatic;

		public override StackChange StackChange { get { return isStatic ? new StackChange(0, 1) : new StackChange(1, 1); } }

		public override int GetSize()
		{
			return 1 + 4;
		}

		public override byte[] GetBytes()
		{
			var output = new byte[5];
			output[0] = isStatic ? (byte)Opcode.Ldsfld : (byte)Opcode.Ldfld;
			output.CopyBytes(field, 1);
			return output;
		}

		public override string ToString()
		{
			if (Parent != null)
			{
				if (isStatic)
					return string.Format("ldsfld {0:X8} [{1}]", field, Parent.Module.GetField(field).FullName);
				else
					return string.Format("ldfld {0:X8} [{1}]", field, Parent.Module.GetField(field).FullName);
			}
			if (isStatic)
				return string.Format("ldsfld {0:X8}", field);
			else
				return string.Format("ldfld {0:X8}", field);
		}

		public static LoadField Create(Module module, Field field)
		{
			return new LoadField(module.GetFieldId(field), field.IsStatic);
		}
	}

	public sealed class StoreField : Instruction
	{
		private StoreField(uint field, bool isStatic)
		{
			this.field = field;
			this.isStatic = isStatic;
		}

		private uint field;
		/// <summary>
		/// Gets the ID 
		/// </summary>
		public uint Field { get { return field; } }

		private bool isStatic;

		public override StackChange StackChange { get { return isStatic ? new StackChange(1, 0) : new StackChange(2, 0); } }

		public override int GetSize()
		{
			return 1 + 4;
		}

		public override byte[] GetBytes()
		{
			var output = new byte[5];
			output[0] = isStatic ? (byte)Opcode.Stsfld : (byte)Opcode.Stfld;
			output.CopyBytes(field, 1);
			return output;
		}

		public override string ToString()
		{
			if (Parent != null)
			{
				if (isStatic)
					return string.Format("stsfld {0:X8} [{1}]", field, Parent.Module.GetField(field).FullName);
				else
					return string.Format("stfld {0:X8} [{1}]", field, Parent.Module.GetField(field).FullName);
			}
			if (isStatic)
				return string.Format("stsfld {0:X8}", field);
			else
				return string.Format("stfld {0:X8}", field);
		}

		public static StoreField Create(Module module, Field field)
		{
			return new StoreField(module.GetFieldId(field), field.IsStatic);
		}
	}

	public sealed class LoadMember : Instruction
	{
		public LoadMember(uint name)
		{
			this.name = name;
		}

		private uint name;
		/// <summary>
		/// Gets the string ID of the name of the member to be loaded.
		/// </summary>
		public uint Name { get { return name; } }

		public override StackChange StackChange { get { return new StackChange(1, 1); } }

		public override int GetSize() { return 5; }

		public override byte[] GetBytes()
		{
			var output = new byte[5];
			output[0] = (byte)Opcode.Ldmem;

			output.CopyBytes(name, 1);

			return output;
		}

		public override string ToString()
		{
			if (Parent != null)
				return string.Format("ldmem {0:X8} [{1}]", name, Parent.Module.Members.Strings[name]);
			return string.Format("ldmem {0:X8}", name);
		}
	}

	public sealed class StoreMember : Instruction
	{
		public StoreMember(uint name)
		{
			this.name = name;
		}

		private uint name;
		/// <summary>
		/// Gets the string ID of the name of the member to be written to.
		/// </summary>
		public uint Name { get { return name; } }

		public override StackChange StackChange { get { return new StackChange(2, 0); } }

		public override int GetSize() { return 5; }

		public override byte[] GetBytes()
		{
			var output = new byte[5];
			output[0] = (byte)Opcode.Stmem;

			output.CopyBytes(name, 1);

			return output;
		}

		public override string ToString()
		{
			if (Parent != null)
				return string.Format("stmem {0:X8} [{1}]", name, Parent.Module.Members.Strings[name]);
			return string.Format("stmem {0:X8}", name);
		}
	}

	public sealed class LoadIndexer : Instruction
	{
		public LoadIndexer(int argCount)
		{
			if (argCount < 1)
				throw new ArgumentOutOfRangeException("argCount");
			this.argCount = argCount;
		}

		private int argCount;
		/// <summary>
		/// Gets the number of arguments that the instruction invokes the indexer with.
		/// </summary>
		public int ArgCount { get { return argCount; } }

		public override StackChange StackChange { get { return new StackChange(1 + argCount, 1); } }

		public override int GetSize()
		{
			if (argCount == 1)
				return 1;
			if (argCount <= byte.MaxValue)
				return 2;
			return 3;
		}

		public override byte[] GetBytes()
		{
			if (argCount == 1)
				return new byte[] { (byte)Opcode.Ldidx_1 };
			if (argCount <= byte.MaxValue)
				return new byte[] { (byte)Opcode.Ldidx_s, (byte)argCount };

			var argcLow = (byte)(argCount & 0x00ff);
			var argcHigh = (byte)((argCount & 0xff00) >> 8);
			return new byte[]
			{
				(byte)Opcode.Ldidx,
				argcLow, argcHigh
			};
		}

		public override string ToString()
		{
			var sb = new StringBuilder(10);
			sb.Append("ldidx");

			if (argCount == 1)
				sb.Append('.');
			else if (argCount <= byte.MaxValue)
				sb.Append(".s ");
			else
				sb.Append(' ');
			sb.Append(argCount);

			return sb.ToString();
		}
	}

	public sealed class StoreIndexer : Instruction
	{
		public StoreIndexer(int argCount)
		{
			if (argCount < 1)
				throw new ArgumentOutOfRangeException("argCount");
			this.argCount = argCount;
		}

		private int argCount;
		/// <summary>
		/// Gets the number of arguments that the instruction invokes the indexer with.
		/// </summary>
		public int ArgCount { get { return argCount; } }

		public override StackChange StackChange { get { return new StackChange(2 + argCount, 0); } }

		public override int GetSize()
		{
			if (argCount == 1)
				return 1;
			if (argCount <= byte.MaxValue)
				return 2;
			return 3;
		}

		public override byte[] GetBytes()
		{
			if (argCount == 1)
				return new byte[] { (byte)Opcode.Stidx_1 };
			if (argCount <= byte.MaxValue)
				return new byte[] { (byte)Opcode.Stidx_s, (byte)argCount };

			var argcLow = (byte)(argCount & 0x00ff);
			var argcHigh = (byte)((argCount & 0xff00) >> 8);
			return new byte[]
			{
				(byte)Opcode.Stidx,
				argcLow, argcHigh
			};
		}

		public override string ToString()
		{
			var sb = new StringBuilder(10);
			sb.Append("stidx");

			if (argCount == 1)
				sb.Append('.');
			else if (argCount <= byte.MaxValue)
				sb.Append(".s ");
			else
				sb.Append(' ');
			sb.Append(argCount);

			return sb.ToString();
		}
	}

	public sealed class LoadTypeToken : Instruction
	{
		public LoadTypeToken(uint type)
		{
			this.type = type;
		}

		private uint type;
		/// <summary>
		/// Gets the ID of the type to load a type token for.
		/// </summary>
		public uint Type { get { return type; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize() { return 5; }

		public override byte[] GetBytes()
		{
			var output = new byte[5];
			output[0] = (byte)Opcode.Ldtypetkn;
			output.CopyBytes(type, 1);
			return output;
		}

		public override string ToString()
		{
			if (Parent != null)
				return string.Format("ldtypetkn {0:X8} [{1}]", type, Parent.Module.GetType(type).FullName);
			return string.Format("ldtypetkn {0:X8}", type);
		}
	}

	public sealed class LoadStaticMethod : Instruction
	{
		public LoadStaticMethod(uint method)
		{
			this.method = method;
		}

		private uint method;
		/// <summary>
		/// Gets the ID of the method to be loaded.
		/// </summary>
		public uint Method { get { return method; } }

		public override StackChange StackChange { get { return StackChange.AddOne; } }

		public override int GetSize()
		{
			return 1 + 4;
		}

		public override byte[] GetBytes()
		{
			var output = new byte[5];

			output[0] = (byte)Opcode.Ldsfn;
			output.CopyBytes(method, 1);

			return output;
		}

		public override string ToString()
		{
			if (Parent != null)
				return string.Format("ldsfn {0:X8} [{1}]", method, Parent.Module.GetMethod(method).FullName);
			return string.Format("ldsfn {0:X8}", method);
		}
	}
}