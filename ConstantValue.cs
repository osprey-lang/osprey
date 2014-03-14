using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using Osprey.Nodes;
using Enum = Osprey.Members.Enum;
using Type = Osprey.Members.Type;

namespace Osprey
{
	/// <summary>
	/// Represents a constant value, to be used in constant expressions.
	/// </summary>
	/// <remarks>The parameterless default constructor should not be called directly, as it
	/// produces an uninitialized <see cref="ConstantValue"/>, equivalent to the null value.
	/// Instead, the field <see cref="ConstantValue.Null"/> should be used to get a null constant,
	/// and <see cref="ConstantValue.CreateBoolean"/>, <see cref="ConstantValue.CreateInt"/>,
	/// <see cref="ConstantValue.CreateUInt"/>, <see cref="ConstantValue.CreateReal"/>,
	/// <see cref="ConstantValue.CreateString"/> and <see cref="ConstantValue.CreateEnumValue"/>
	/// are called to produce specific constants.
	/// </remarks>
	public struct ConstantValue
	{
		public static ConstantValue CreateBoolean(bool value)
		{
			var cv = new ConstantValue();
			cv.type = ConstantValueType.Boolean;
			cv.num.BooleanValue = value;
			return cv;
		}
		public static ConstantValue CreateInt(long value)
		{
			var cv = new ConstantValue();
			cv.type = ConstantValueType.Int;
			cv.num.IntValue = value;
			return cv;
		}
		public static ConstantValue CreateUInt(ulong value)
		{
			var cv = new ConstantValue();
			cv.type = ConstantValueType.UInt;
			cv.num.UIntValue = value;
			return cv;
		}
		public static ConstantValue CreateReal(double value)
		{
			var cv = new ConstantValue();
			cv.type = ConstantValueType.Real;
			cv.num.RealValue = value;
			return cv;
		}
		public static ConstantValue CreateString(string value)
		{
			if (value == null)
				throw new ArgumentNullException("value");
			var cv = new ConstantValue();
			cv.type = ConstantValueType.String;
			cv.stringValue = value;
			return cv;
		}
		public static ConstantValue CreateChar(int value)
		{
			var cv = new ConstantValue();
			cv.type = ConstantValueType.Char;
			cv.num.IntValue = value;
			return cv;
		}
		public static ConstantValue CreateEnumValue(long value, Enum type)
		{
			if (type == null)
				throw new ArgumentNullException("type");

			var cv = new ConstantValue();
			cv.type = ConstantValueType.Enum;
			cv.num.IntValue = value;
			cv.enumType = type;
			return cv;
		}

		private ConstantValueType type;
		/// <summary>
		/// Gets the type of the constant value.
		/// </summary>
		public ConstantValueType Type { get { return type; } }

		private ConstantNumericValue num;
		/// <summary>
		/// Gets the Boolean value associated with the constant.
		/// </summary>
		/// <exception cref="InvalidOperationException">The constant is not of type Boolean.</exception>
		public bool BooleanValue
		{
			get
			{
				if (type != ConstantValueType.Boolean)
					throw new InvalidOperationException();
				return num.BooleanValue;
			}
		}
		/// <summary>
		/// Gets the Int value associated with the constant.
		/// </summary>
		/// <exception cref="InvalidOperationException">The constant is not of type Int.</exception>
		public long IntValue
		{
			get
			{
				if (type != ConstantValueType.Int)
					throw new InvalidOperationException();
				return num.IntValue;
			}
		}
		/// <summary>
		/// Gets the UInt value associated with the constant.
		/// </summary>
		/// <exception cref="InvalidOperationException">The constant is not of type UInt.</exception>
		public ulong UIntValue
		{
			get
			{
				if (type != ConstantValueType.UInt)
					throw new InvalidOperationException();
				return num.UIntValue;
			}
		}
		/// <summary>
		/// Gets the Real value associated with the constant.
		/// </summary>
		/// <exception cref="InvalidOperationException">The constant is not of type Real.</exception>
		public double RealValue
		{
			get
			{
				if (type != ConstantValueType.Real)
					throw new InvalidOperationException();
				return num.RealValue;
			}
		}

		// Note: this will get turned into a StringRef during bytecode emission.
		private string stringValue;
		/// <summary>
		/// Gets the String value associated with the constant.
		/// </summary>
		/// <exception cref="InvalidOperationException">The constant is not of type String.</exception>
		public string StringValue
		{
			get
			{
				if (type != ConstantValueType.String)
					throw new InvalidOperationException();
				return stringValue;
			}
		}

		public int CharValue
		{
			get
			{
				if (type != ConstantValueType.Char)
					throw new InvalidOperationException();
				return unchecked((int)num.IntValue);
			}
		}

		private Enum enumType;
		/// <summary>
		/// Gets the enum value associated with the constant.
		/// </summary>
		/// <exception cref="InvalidOperationException">The constant is not of type Enum.</exception>
		public ConstantEnumValue EnumValue
		{
			get
			{
				if (type != ConstantValueType.Enum)
					throw new InvalidOperationException();
				return new ConstantEnumValue(num.IntValue, enumType);
			}
		}

		public bool IsNumeric
		{
			get
			{
				return type == ConstantValueType.Int ||
					type == ConstantValueType.UInt ||
					type == ConstantValueType.Real;
			}
		}

		public bool IsTrue
		{
			get
			{
				switch (type)
				{
					case ConstantValueType.Null:
						return false;
					case ConstantValueType.Boolean:
						return num.BooleanValue;
					case ConstantValueType.Int:
					case ConstantValueType.Char:
					case ConstantValueType.Enum:
						return num.IntValue != 0;
					case ConstantValueType.UInt:
						return num.UIntValue != 0;
					case ConstantValueType.Real:
						return num.RealValue != 0.0;
				}
				return true; // all other values are true!
			}
		}

		public bool IsString
		{
			get { return this.type == ConstantValueType.String; }
		}

		public bool IsStringLike
		{
			get { return this.type == ConstantValueType.String || this.type == ConstantValueType.Char; }
		}

		internal double ToReal()
		{
			if (type == ConstantValueType.Real)
				return num.RealValue;
			if (type == ConstantValueType.Int)
				return (double)num.IntValue;
			if (type == ConstantValueType.UInt)
				return (double)num.UIntValue;
			throw new NotSupportedException();
		}

		internal long ToInt()
		{
			if (type == ConstantValueType.Int)
				return num.IntValue;
			if (type == ConstantValueType.Real)
				return checked((long)num.RealValue);
			if (type == ConstantValueType.UInt)
				return checked((long)num.UIntValue);
			throw new NotSupportedException();
		}

		internal ulong ToUInt()
		{
			if (type == ConstantValueType.UInt)
				return num.UIntValue;
			if (type == ConstantValueType.Int)
				return checked((ulong)num.IntValue);
			if (type == ConstantValueType.Real)
				return checked((ulong)num.RealValue);
			throw new NotSupportedException();
		}

		public Type GetTypeObject(Compiler compiler)
		{
			switch (type)
			{
				case ConstantValueType.Null:
					return null;
				case ConstantValueType.Boolean:
					return compiler.BooleanType;
				case ConstantValueType.Int:
					return compiler.IntType;
				case ConstantValueType.UInt:
					return compiler.UIntType;
				case ConstantValueType.Real:
					return compiler.RealType;
				case ConstantValueType.String:
					return compiler.StringType;
				case ConstantValueType.Char:
					return compiler.CharType;
				case ConstantValueType.Enum:
					return enumType;
				default:
					throw new InvalidOperationException("Invalid type for ConstantValue.");
			}
		}

		internal ulong GetRawValue()
		{
			if (type == ConstantValueType.String)
				throw new InvalidOperationException();

			return num.UIntValue;
		}

		public override int GetHashCode()
		{
			switch (type)
			{
				case ConstantValueType.Boolean:
					return num.BooleanValue.GetHashCode();
				case ConstantValueType.Int:
					return num.IntValue.GetHashCode();
				case ConstantValueType.UInt:
					return num.UIntValue.GetHashCode();
				case ConstantValueType.Real:
					return num.RealValue.GetHashCode();
				case ConstantValueType.String:
					return stringValue.GetHashCode();
				case ConstantValueType.Enum:
					return enumType.GetHashCode() ^
						num.IntValue.GetHashCode();
				default: // Null or unknown/invalid
					return 0;
			}
		}

		public override bool Equals(object obj)
		{
			if (obj is ConstantValue)
				return this == (ConstantValue)obj;
			return base.Equals(obj);
		}

		public bool Equals(ConstantValue other)
		{
			return this == other;
		}

		public bool SupportsOperator(BinaryOperator op, ConstantValue right)
		{
			switch (op)
			{
				case BinaryOperator.Addition:
				case BinaryOperator.Subtraction:
				case BinaryOperator.Multiplication:
				case BinaryOperator.Division:
				case BinaryOperator.Modulo:
				case BinaryOperator.Exponentiation:
					return this.IsNumeric && right.IsNumeric;

				case BinaryOperator.BitwiseOr:
				case BinaryOperator.BitwiseXor:
				case BinaryOperator.BitwiseAnd:
					return (this.type == ConstantValueType.Int || this.type == ConstantValueType.UInt) &&
						(right.type == ConstantValueType.Int || right.type == ConstantValueType.UInt)
						||
						this.type == ConstantValueType.Enum && right.type == ConstantValueType.Enum &&
						this.enumType.IsSet && this.enumType == right.enumType;
				case BinaryOperator.ShiftLeft:
				case BinaryOperator.ShiftRight:
					return (this.type == ConstantValueType.Int || this.type == ConstantValueType.UInt) &&
						right.IsNumeric;

				case BinaryOperator.Equality:
				case BinaryOperator.Inequality:
				case BinaryOperator.ReferenceEquality:
					// All constant types support the ==, != and refeq operators, but not necessarily with
					// all combinations of operands. However, in these cases, the operator returns false
					// (or true for != or not refeq).
					return true;

				case BinaryOperator.LessThan:
				case BinaryOperator.LessEqual:
				case BinaryOperator.GreaterThan:
				case BinaryOperator.GreaterEqual:
				case BinaryOperator.Comparison:
					// Note: <, <=, > and >= are all implemented in terms of <=>
					if (this.IsNumeric)
						return right.IsNumeric;
					if (this.type == ConstantValueType.Boolean)
						return right.type == ConstantValueType.Boolean;
					if (this.type == ConstantValueType.String || this.type == ConstantValueType.Char)
						return right.type == ConstantValueType.String || right.type == ConstantValueType.Char;
					if (this.type == ConstantValueType.Enum)
						return right.type == ConstantValueType.Enum && this.enumType == right.enumType;
					return false;

				case BinaryOperator.Or:
				case BinaryOperator.Xor:
				case BinaryOperator.And:
					return true;

				case BinaryOperator.Concatenation:
					return this.type == ConstantValueType.String && right.type == ConstantValueType.String;
			}

			return false;
		}

		public bool SupportsOperator(UnaryOperator op)
		{
			switch (op)
			{
				case UnaryOperator.Plus:
					return IsNumeric ||
						type == ConstantValueType.Boolean ||
						type == ConstantValueType.Char ||
						type == ConstantValueType.Enum;
				case UnaryOperator.Minus:
					return type == ConstantValueType.Int || type == ConstantValueType.Real;
				case UnaryOperator.BitwiseNot:
					return type == ConstantValueType.Int || type == ConstantValueType.UInt ||
						type == ConstantValueType.Enum && enumType.IsSet;
				case UnaryOperator.Not:
					return true;
			}

			return false;
		}

		public ConstantValue ExecuteOperator(BinaryOperator op, ConstantValue right)
		{
			switch (op)
			{
				case BinaryOperator.Addition:
					return this + right;
				case BinaryOperator.Subtraction:
					return this - right;
				case BinaryOperator.Multiplication:
					return this * right;
				case BinaryOperator.Division:
					return this / right;
				case BinaryOperator.Modulo:
					return this % right;
				case BinaryOperator.Exponentiation:
					return Power(this, right);
				case BinaryOperator.BitwiseOr:
					return BitwiseOr(this, right);
				case BinaryOperator.BitwiseXor:
					return BitwiseXor(this, right);
				case BinaryOperator.BitwiseAnd:
					return BitwiseAnd(this, right);
				case BinaryOperator.ShiftLeft:
					return ShiftLeft(this, right);
				case BinaryOperator.ShiftRight:
					return ShiftRight(this, right);
				case BinaryOperator.Or:
					return Or(this, right);
				case BinaryOperator.Xor:
					return Xor(this, right);
				case BinaryOperator.And:
					return And(this, right);
				case BinaryOperator.Concatenation:
					return Concat(this, right);
				case BinaryOperator.Equality:
					return CreateBoolean(this == right);
				case BinaryOperator.Inequality:
					return CreateBoolean(this != right);
				case BinaryOperator.ReferenceEquality:
					return RefEquals(this, right);
				case BinaryOperator.Comparison:
					return CreateInt(Compare(this, right));
				case BinaryOperator.LessThan:
					return CreateBoolean(Compare(this, right) < 0);
				case BinaryOperator.LessEqual:
					return CreateBoolean(Compare(this, right) <= 0);
				case BinaryOperator.GreaterThan:
					return CreateBoolean(Compare(this, right) > 0);
				case BinaryOperator.GreaterEqual:
					return CreateBoolean(Compare(this, right) >= 0);
			}

			throw new NotSupportedException();
		}

		public static ConstantValue operator +(ConstantValue left, ConstantValue right)
		{
			if (left.IsNumeric && right.IsNumeric)
			{
				if (left.type == ConstantValueType.Real || right.type == ConstantValueType.Real)
					return CreateReal(left.ToReal() + right.ToReal());

				if (left.type == ConstantValueType.Int)
				{
					if (right.type == ConstantValueType.UInt)
						return CreateInt(checked(left.IntValue + (long)right.UIntValue));
					return CreateInt(checked(left.IntValue + right.IntValue));
				}
				else // left == UInt
				{
					if (right.type == ConstantValueType.Int)
						return CreateUInt(checked(left.UIntValue + (ulong)right.IntValue));
					return CreateUInt(checked(left.UIntValue + right.UIntValue));
				}
			}

			throw new NotSupportedException();
		}
		public static ConstantValue operator -(ConstantValue left, ConstantValue right)
		{
			if (left.IsNumeric && right.IsNumeric)
			{
				if (left.type == ConstantValueType.Real || right.type == ConstantValueType.Real)
					return CreateReal(left.ToReal() - right.ToReal());

				if (left.type == ConstantValueType.Int)
				{
					if (right.type == ConstantValueType.UInt)
						return CreateInt(checked(left.IntValue - (long)right.UIntValue));
					return CreateInt(checked(left.IntValue - right.IntValue));
				}
				else // left == UInt
				{
					if (right.type == ConstantValueType.Int)
						return CreateUInt(checked(left.UIntValue - (ulong)right.IntValue));
					return CreateUInt(checked(left.UIntValue - right.UIntValue));
				}
			}

			throw new NotSupportedException();
		}

		public static ConstantValue operator *(ConstantValue left, ConstantValue right)
		{
			if (left.IsNumeric && right.IsNumeric)
			{
				if (left.type == ConstantValueType.Real || right.type == ConstantValueType.Real)
					return CreateReal(left.ToReal() * right.ToReal());

				if (left.type == ConstantValueType.Int)
				{
					if (right.type == ConstantValueType.UInt)
						return CreateInt(checked(left.IntValue * (long)right.UIntValue));
					return CreateInt(checked(left.IntValue * right.IntValue));
				}
				else // left == UInt
				{
					if (right.type == ConstantValueType.Int)
						return CreateUInt(checked(left.UIntValue * (ulong)right.IntValue));
					return CreateUInt(checked(left.UIntValue * right.UIntValue));
				}
			}

			throw new NotSupportedException();
		}
		public static ConstantValue operator /(ConstantValue left, ConstantValue right)
		{
			if (left.IsNumeric && right.IsNumeric)
			{
				if (left.type == ConstantValueType.Real || right.type == ConstantValueType.Real)
					return CreateReal(left.ToReal() / right.ToReal());

				if (left.type == ConstantValueType.Int)
				{
					if (right.type == ConstantValueType.UInt)
						return CreateInt(checked(left.IntValue / (long)right.UIntValue));
					return CreateInt(checked(left.IntValue / right.IntValue));
				}
				else // left == UInt
				{
					if (right.type == ConstantValueType.Int)
						return CreateUInt(checked(left.UIntValue / (ulong)right.IntValue));
					return CreateUInt(checked(left.UIntValue / right.UIntValue));
				}
			}

			throw new NotSupportedException();
		}
		public static ConstantValue operator %(ConstantValue left, ConstantValue right)
		{
			if (left.IsNumeric && right.IsNumeric)
			{
				if (left.type == ConstantValueType.Real || right.type == ConstantValueType.Real)
					return CreateReal(left.ToReal() % right.ToReal());

				// left union right subsetOf {Int, UInt}
				if (left.type == ConstantValueType.Int)
				{
					if (right.type == ConstantValueType.UInt)
						return CreateInt(checked(left.IntValue % (long)right.UIntValue));
					return CreateInt(checked(left.IntValue % right.IntValue));
				}
				else // left == UInt
				{
					if (right.type == ConstantValueType.Int)
						return CreateUInt(checked(left.UIntValue % (ulong)right.IntValue));
					return CreateUInt(checked(left.UIntValue % right.UIntValue));
				}
			}

			throw new NotSupportedException();
		}

		public static ConstantValue Power(ConstantValue left, ConstantValue right)
		{
			if (left.IsNumeric && right.IsNumeric)
			{
				if (left.type == ConstantValueType.Real || right.type == ConstantValueType.Real)
					return CreateReal(Math.Pow(left.ToReal(), right.ToReal()));
				if (left.type == ConstantValueType.Int)
					return CreateInt(Power(left.num.IntValue, right.ToInt()));
				if (left.type == ConstantValueType.UInt)
					return CreateUInt(Power(left.num.UIntValue, right.ToUInt()));
			}

			throw new NotSupportedException();
		}
		private static long Power(long a, long b)
		{
			long result = 1;

			checked
			{
				while (b > 0)
				{
					if ((b & 1) != 0)
						result *= a;
					b >>= 1;
					if (b > 0)
						a *= a;
				}
			}

			return result;
		}
		private static ulong Power(ulong a, ulong b)
		{
			ulong result = 1;

			checked
			{
				while (b > 0)
				{
					if ((b & 1) != 0)
						result *= a;
					b >>= 1;
					if (b > 0)
						a *= a;
				}
			}

			return result;
		}

		// Note: the bitwise operators are not operators because then that
		// would implicitly overload && and || as well, which we do not want.
		public static ConstantValue BitwiseOr(ConstantValue left, ConstantValue right)
		{
			if ((left.type == ConstantValueType.Int || left.type == ConstantValueType.UInt) &&
				(right.type == ConstantValueType.Int || right.type == ConstantValueType.UInt))
			{
				ulong value = left.num.UIntValue | right.num.UIntValue;
				if (left.type == ConstantValueType.Int)
					return CreateInt(unchecked((long)value));
				else
					return CreateUInt(value);
			}
			else if (left.type == ConstantValueType.Enum && right.type == ConstantValueType.Enum &&
				left.enumType.IsSet && left.enumType == right.enumType)
				return CreateEnumValue(left.num.IntValue | right.num.IntValue, left.enumType);

			throw new NotSupportedException();
		}
		public static ConstantValue BitwiseXor(ConstantValue left, ConstantValue right)
		{
			if ((left.type == ConstantValueType.Int || left.type == ConstantValueType.UInt) &&
				(right.type == ConstantValueType.Int || right.type == ConstantValueType.UInt))
			{
				ulong value = left.num.UIntValue ^ right.num.UIntValue;
				if (left.type == ConstantValueType.Int)
					return CreateInt(unchecked((long)value));
				else
					return CreateUInt(value);
			}
			else if (left.type == ConstantValueType.Enum && right.type == ConstantValueType.Enum &&
				left.enumType.IsSet && left.enumType == right.enumType)
				return CreateEnumValue(left.num.IntValue ^ right.num.IntValue, left.enumType);

			throw new NotSupportedException();
		}
		public static ConstantValue BitwiseAnd(ConstantValue left, ConstantValue right)
		{
			if ((left.type == ConstantValueType.Int || left.type == ConstantValueType.UInt) &&
				(right.type == ConstantValueType.Int || right.type == ConstantValueType.UInt))
			{
				ulong value = left.num.UIntValue & right.num.UIntValue;
				if (left.type == ConstantValueType.Int)
					return CreateInt(unchecked((long)value));
				else
					return CreateUInt(value);
			}
			else if (left.type == ConstantValueType.Enum && right.type == ConstantValueType.Enum &&
				left.enumType.IsSet && left.enumType == right.enumType)
				return CreateEnumValue(left.num.IntValue & right.num.IntValue, left.enumType);

			throw new NotSupportedException();
		}

		// Note: ShiftLeft and ShiftRight cannot be operators, because
		// C# requires the right operand to be of type int.
		public static ConstantValue ShiftLeft(ConstantValue left, ConstantValue right)
		{
			if (left.type == ConstantValueType.Int || left.type == ConstantValueType.UInt)
			{
				var amount = right.ToInt(); // always needed
				if (amount < 0)
					throw new ArgumentOutOfRangeException("Negative shift count is not allowed.");

				if (left.type == ConstantValueType.Int)
					return CreateInt(amount >= 64 ? 0 : left.IntValue << (int)amount);
				else // UInt
					return CreateUInt(amount >= 64 ? 0 : left.UIntValue << (int)amount);
			}
			throw new NotSupportedException();
		}
		public static ConstantValue ShiftRight(ConstantValue left, ConstantValue right)
		{
			if (left.type == ConstantValueType.Int || left.type == ConstantValueType.UInt)
			{
				var amount = right.ToInt(); // always needed
				if (amount < 0)
					throw new ArgumentOutOfRangeException("Negative shift count is not allowed.");

				if (left.type == ConstantValueType.Int)
					return CreateInt(amount >= 64 ? (left.IntValue < 0 ? -1 : 0) : left.IntValue >> (int)amount);
				else // UInt
					return CreateUInt(amount >= 64 ? 0 : left.UIntValue >> (int)amount);
			}
			throw new NotSupportedException();
		}

		public static ConstantValue Or(ConstantValue left, ConstantValue right)
		{
			return CreateBoolean(left.IsTrue || right.IsTrue);
		}
		public static ConstantValue Xor(ConstantValue left, ConstantValue right)
		{
			return CreateBoolean(left.IsTrue != right.IsTrue);
		}
		public static ConstantValue And(ConstantValue left, ConstantValue right)
		{
			return CreateBoolean(left.IsTrue && right.IsTrue);
		}

		public static ConstantValue Concat(ConstantValue left, ConstantValue right)
		{
			if (left.IsString && right.IsString)
				return ConstantValue.CreateString(left.stringValue + right.stringValue);

			throw new NotSupportedException();
		}

		public static bool operator ==(ConstantValue left, ConstantValue right)
		{
			if (left.IsNumeric && right.IsNumeric)
			{
				if (left.Type == ConstantValueType.Real ||
					right.Type == ConstantValueType.Real)
				{
					var leftReal = left.ToReal();
					var rightReal = right.ToReal();
					return double.IsNaN(leftReal) && double.IsNaN(rightReal) ||
						leftReal == rightReal;
				}

				if (left.Type == ConstantValueType.Int)
				{
					if (right.Type == ConstantValueType.Int)
						return left.num.IntValue == right.num.IntValue;
					return left.num.IntValue >= 0 &&
						unchecked((ulong)left.num.IntValue) == right.num.UIntValue;
				}
				else // left == UInt
				{
					if (right.Type == ConstantValueType.UInt)
						return left.num.UIntValue == right.num.UIntValue;
					return right.num.IntValue >= 0 &&
						left.num.UIntValue == unchecked((ulong)right.num.IntValue);
				}
			}

			if (left.type != right.type)
				return false;
			// left.type == right.type below here

			if (left.type == ConstantValueType.Null)
				return true; // both are null
			if (left.type == ConstantValueType.Enum)
				return left.enumType == right.enumType &&
					left.num.IntValue == right.num.IntValue;
			if (left.type == ConstantValueType.Boolean)
				return left.BooleanValue == right.BooleanValue;
			if (left.type == ConstantValueType.String)
				return left.StringValue == right.StringValue;
			return false;
		}
		public static bool operator !=(ConstantValue left, ConstantValue right)
		{
			return !(left == right);
		}

		public static int Compare(ConstantValue left, ConstantValue right)
		{
			if (left.IsNumeric && right.IsNumeric)
				return CompareNumeric(left, right);

			if (left.IsStringLike && right.IsStringLike)
				return CompareStringLike(left, right);

			if (left.type != right.type ||
				left.type == ConstantValueType.Null ||
				left.type == ConstantValueType.Enum && left.enumType != right.enumType)
				throw new NotSupportedException();

			// left.type == right.type here, and if type == Enum,
			// left.enumType == right.enumType

			switch (left.type)
			{
				case ConstantValueType.Boolean:
					return left.BooleanValue.CompareTo(right.BooleanValue);
				case ConstantValueType.Enum:
					return left.num.IntValue.CompareTo(right.num.IntValue);
				default:
					throw new InvalidOperationException("Compiler bug: fell through the switch.");
			}
		}

		private static int CompareNumeric(ConstantValue left, ConstantValue right)
		{
			double leftReal, rightReal;

			switch (left.type)
			{
				case ConstantValueType.Int:
					if (right.type == ConstantValueType.Real)
					{
						leftReal = (double)left.num.IntValue;
						rightReal = right.num.RealValue;
						break;
					}
					if (right.type == ConstantValueType.UInt)
					{
						if (left.num.IntValue < 0 ||
							right.num.UIntValue > long.MaxValue)
							return -1;
					}
					return left.num.IntValue.CompareTo(right.num.IntValue);
				case ConstantValueType.UInt:
					if (right.type == ConstantValueType.Real)
					{
						leftReal = (double)left.num.UIntValue;
						rightReal = right.num.RealValue;
						break;
					}
					if (right.type == ConstantValueType.Int)
					{
						if (right.num.IntValue < 0 ||
							left.num.UIntValue > long.MaxValue)
							return 1;
					}
					return left.num.UIntValue.CompareTo(right.num.UIntValue);
				case ConstantValueType.Real:
					leftReal = left.num.RealValue;
					rightReal = right.num.RealValue;
					break;
				default:
					throw new InvalidOperationException("Compiler bug: fell through the switch.");
			}

			// .NET orders doubles in the following order:
			//    NaN < -∞ < -0.0 == +0.0 < +∞
			// This is fully compatible with Osprey's ordering.
			return leftReal.CompareTo(rightReal);
		}

		private static int CompareStringLike(ConstantValue left, ConstantValue right)
		{
			string a = left.IsString ? left.stringValue : char.ConvertFromUtf32(unchecked((int)left.num.IntValue));
			string b = right.IsString ? right.stringValue : char.ConvertFromUtf32(unchecked((int)right.num.IntValue));

			// Have to reimplement Ovum's string comparison, because .NET has nothing like it
			int aLen = a.Length, bLen = b.Length;

			int ai = 0, bi = 0;

			unchecked
			{
				while (ai < aLen && bi < bLen)
				{
					int ac = a[ai++];
					if (ai < aLen && char.IsSurrogatePair((char)ac, a[ai]))
						ac = char.ConvertToUtf32((char)ac, a[ai++]);

					int bc = b[bi++];
					if (bi < bLen && char.IsSurrogatePair((char)bc, b[bi]))
						bc = char.ConvertToUtf32((char)bc, b[bi++]);

					if (ac != bc)
						return ac - bc;
				}

				return a.Length - b.Length;
			}
		}

		public static ConstantValue RefEquals(ConstantValue left, ConstantValue right)
		{
			if (left.type != right.type)
				return False; // Not possible, man

			// left.type == right.type here
			bool result;
			switch (left.type)
			{
				case ConstantValueType.Null:
					result = true; // Both are null
					break;
				case ConstantValueType.Boolean:
					result = left.BooleanValue == right.BooleanValue;
					break;
				case ConstantValueType.Int:
					result = left.IntValue == right.IntValue;
					break;
				case ConstantValueType.UInt:
					result = left.UIntValue == right.UIntValue;
					break;
				case ConstantValueType.Real:
					result = left.RealValue == right.RealValue;
					break;
				case ConstantValueType.Enum:
					result = left.enumType == right.enumType &&
						left.num.IntValue == right.num.IntValue;
					break;
				case ConstantValueType.String:
					result = left.StringValue == right.StringValue;
					break;
				case ConstantValueType.Char:
					result = left.num.IntValue == right.num.IntValue;
					break;
				default:
					result = false; // should never actually happen
					break;
			}

			return CreateBoolean(result);
		}

		public ConstantValue ExecuteOperator(UnaryOperator op)
		{
			switch (op)
			{
				case UnaryOperator.Plus:
					return +this;
				case UnaryOperator.Minus:
					return -this;
				case UnaryOperator.BitwiseNot:
					return ~this;
				case UnaryOperator.Not:
					return CreateBoolean(!this.IsTrue);
			}

			throw new NotSupportedException();
		}

		public static ConstantValue operator +(ConstantValue arg)
		{
			if (arg.IsNumeric)
				return arg; // no modification necessary
			if (arg.type == ConstantValueType.Boolean)
				return CreateInt(arg.BooleanValue ? 1 : 0);
			if (arg.type == ConstantValueType.Char ||
				arg.type == ConstantValueType.Enum)
				return CreateInt(arg.num.IntValue);

			throw new NotSupportedException();
		}
		public static ConstantValue operator -(ConstantValue arg)
		{
			if (arg.type == ConstantValueType.Int)
				return CreateInt(checked(-arg.IntValue));
			if (arg.type == ConstantValueType.Real)
				return CreateReal(-arg.RealValue);

			throw new NotSupportedException();
		}
		public static ConstantValue operator ~(ConstantValue arg)
		{
			if (arg.type == ConstantValueType.Int)
				return CreateInt(~arg.IntValue);
			if (arg.type == ConstantValueType.UInt)
				return CreateUInt(~arg.UIntValue);
			if (arg.type == ConstantValueType.Enum && arg.enumType.IsSet)
				return CreateEnumValue(~arg.num.IntValue, arg.enumType);

			throw new NotSupportedException();
		}

		/// <summary>Represents a null value.</summary>
		public static readonly ConstantValue Null = new ConstantValue();

		public static readonly ConstantValue False = CreateBoolean(false);
		public static readonly ConstantValue True = CreateBoolean(true);

		[StructLayout(LayoutKind.Explicit)]
		private struct ConstantNumericValue
		{
			[FieldOffset(0)]
			public bool BooleanValue;
			[FieldOffset(0)]
			public long IntValue;
			[FieldOffset(0)]
			public ulong UIntValue;
			[FieldOffset(0)]
			public double RealValue;
		}
	}

	public struct ConstantEnumValue
	{
		public ConstantEnumValue(long value, Enum type)
		{
			if (type == null)
				throw new ArgumentNullException("type");

			this.value = value;
			this.type = type;
		}

		private long value;
		/// <summary>
		/// Gets the enum value that the constant represents.
		/// </summary>
		public long Value { get { return value; } }

		private Enum type;
		/// <summary>
		/// Gets the type that the enum value is of.
		/// </summary>
		public Enum Type { get { return type; } }
	}

	/// <summary>
	/// Describes the type of a constant value.
	/// </summary>
	public enum ConstantValueType
	{
		/// <summary>The constant is null.</summary>
		Null = 0,
		/// <summary>The constant is of type Boolean.</summary>
		Boolean,
		/// <summary>The constant is of type Int.</summary>
		Int,
		/// <summary>The constant is of type UInt.</summary>
		UInt,
		/// <summary>The constant is of type Real.</summary>
		Real,
		/// <summary>The constant is of type String.</summary>
		String,
		/// <summary>The constant is of type Char.</summary>
		Char,
		/// <summary>The constant is of an enum type.</summary>
		Enum,
	}
}