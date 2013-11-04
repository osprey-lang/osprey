using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using Osprey.Instructions;
using Osprey.Members;
using CI = System.Globalization.CultureInfo;
using Enum = Osprey.Members.Enum;
using Type = Osprey.Members.Type;

// This file contains all the Expression node types.

namespace Osprey.Nodes
{
	public abstract class Expression : ParseNode
	{
		/// <summary>
		/// Determines whether the expression can be inlined without risking side effects or double evaluation.
		/// Expressions that can be inlined typically compile to a single instruction.
		/// </summary>
		/// <returns>True if the expression can be inlined; otherwise, false.</returns>
		public bool CanSafelyInline
		{
			get
			{
				return this is ThisAccess || // this is BaseAccess || // 'base' is not supposed to occur on its own
					this is ConstantExpression ||
					this is StaticMethodAccess ||
					this is GetArgumentCount;
			}
		}

		/// <summary>
		/// Determines whether this expression is a constant expression representing the null value.
		/// </summary>
		/// <returns>True if this expression is a constant expression with the value null; otherwise, false.</returns>
		public bool IsNull
		{
			get
			{
				if (this is ConstantExpression)
				{
					var constExpr = (ConstantExpression)this;
					return constExpr.Value.Type == ConstantValueType.Null;
				}
				return false;
			}
		}

		/// <summary>
		/// Reduces the expression to a <see cref="ConstantExpression"/> if possible.
		/// If the expression cannot be reduced, this method returns the current node,
		/// which is also the default implementation.
		/// </summary>
		/// <returns>An <see cref="Expression"/> that may be a <see cref="ConstantExpression"/>.</returns>
		public virtual Expression FoldConstant()
		{
			return this;
		}

		public Expression ResolveNames(IDeclarationSpace context, FileNamespace document, bool allowNamespace, bool allowType)
		{
			var result = ResolveNames(context, document);
			EnsureAllowed(result, allowNamespace, allowType);
			return result;
		}

		/// <summary>
		/// Resolves the names in the expression. If the expression modifies itself,
		/// it should return 'this'. If it creates a new expression, that expression
		/// should be returned.
		/// </summary>
		/// <param name="context">The context in which to resolve names. This is usually a block,
		/// but initializers for fields and global variables do not occur inside blocks.</param>
		/// <param name="document">The <see cref="FileNamespace"/> of the document in which the expression occurs.</param>
		/// <returns>An expression that represents the current expression with all names resolved.</returns>
		/// <exception cref="UndefinedNameException">A name in this expression could not be resolved.</exception>
		/// <exception cref="AmbiguousNameException">A name in this expression was ambiguous between several names.</exception>
		/// <remarks>The default implementation just returns this.</remarks>
		public virtual Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			return this;
		}

		/// <summary>
		/// Determines whether the type of the expression is known at compile-time.
		/// </summary>
		/// <param name="compiler">The <see cref="Compiler"/> which the node belongs to.</param>
		/// <returns>True if the type of the expression is known; otherwise, false.</returns>
		public virtual bool IsTypeKnown(Compiler compiler) { return false; }

		/// <summary>
		/// Returns the compile-time type of this expression. If the type is not known,
		/// or if the type is the Osprey null value, this method returns null.
		/// </summary>
		/// <param name="compiler">The <see cref="Compiler"/> from which to load type objects.</param>
		/// <returns>The compile-time type of this expression.</returns>
		public virtual Type GetKnownType(Compiler compiler) { return null; }

		protected static void EnsureAllowed(Expression expr, bool allowNamespace, bool allowType)
		{
			if (!allowType && expr is TypeAccess)
				throw new CompileTimeException(expr, "Type name is not permitted in this context.");
			if (!allowNamespace && expr is NamespaceAccess)
				throw new CompileTimeException(expr, "Namespace name is not permitted in this context.");
		}

		public virtual Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			return this;
		}

		public abstract void Compile(Compiler compiler, MethodBuilder method);

		public virtual void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			// Compile as "normal" expression and test truthiness of result
			Compile(compiler, method); // Evaluate expression
			if (negated)
				method.Append(Branch.IfTrue(falseLabel)); // not true == false
			else
				method.Append(Branch.IfFalse(falseLabel)); // false == false (yes, really!)
			// Result (on stack) is guaranteed to have the correct truthiness
		}

		internal Expression At(Expression other)
		{
			this.StartIndex = other.StartIndex;
			this.EndIndex = other.EndIndex;
			this.Document = other.Document;
			return this;
		}
	}

	public class ConstantExpression : Expression
	{
		protected ConstantExpression()
		{ } // Does not initialise the value
		public ConstantExpression(ConstantValue value)
		{
			this.value = value;
		}

		protected ConstantValue? value;
		/// <summary>The value of the constant expression.</summary>
		public virtual ConstantValue Value
		{
			get
			{
				if (!value.HasValue)
					throw new InvalidOperationException("This constant expression does not have a constant value yet.");
				return value.Value;
			}
			internal set
			{
				this.value = value;
			}
		}

		// The type of a constant expression is always known.
		public override bool IsTypeKnown(Compiler compiler) { return value.HasValue; }

		public override Type GetKnownType(Compiler compiler)
		{
			if (value.HasValue)
				return value.Value.GetTypeObject(compiler);
			else
				return null;
		}

		public override string ToString(int indent)
		{
			if (value.HasValue)
			{
				switch (Value.Type)
				{
					case ConstantValueType.Null:
						return "{ConstantExpression}null";
					case ConstantValueType.Boolean:
						return Value.BooleanValue ? "{ConstantExpression}true" : "{ConstantExpression}false";
					case ConstantValueType.Int:
						return "{ConstantExpression}" + Value.IntValue.ToString(CI.InvariantCulture);
					case ConstantValueType.UInt:
						return "{ConstantExpression}" + Value.UIntValue.ToString(CI.InvariantCulture);
					case ConstantValueType.Real:
						return "{ConstantExpression}" + Value.RealValue.ToString(CI.InvariantCulture);
					case ConstantValueType.String:
						return "{ConstantExpression}\"" + Value.StringValue + "\"";
					case ConstantValueType.Enum:
						return "{ConstantExpression enum value}";
				}
				return string.Format("{ConstantExpression, Type={2}}",
					StartIndex, EndIndex, Value.Type);
			}
			return string.Format("{ConstantExpression without value}",
				StartIndex, EndIndex);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			var constValue = Value;
			switch (constValue.Type)
			{
				case ConstantValueType.Null:
					method.Append(LoadConstant.Null());
					break;
				case ConstantValueType.Boolean:
					method.Append(constValue.IsTrue ? LoadConstant.True() : LoadConstant.False());
					break;
				case ConstantValueType.Int:
					method.Append(new LoadConstantInt(constValue.IntValue));
					break;
				case ConstantValueType.UInt:
					method.Append(new LoadConstantUInt(constValue.UIntValue));
					break;
				case ConstantValueType.Real:
					method.Append(new LoadConstantReal(constValue.RealValue));
					break;
				case ConstantValueType.String:
					method.Append(new LoadString(method.Module.GetStringId(constValue.StringValue)));
					break;
				case ConstantValueType.Enum:
					{
						var enumVal = constValue.EnumValue;
						method.Append(new LoadEnum(method.Module.GetTypeId(enumVal.Type), enumVal.Value));
					}
					break;
				default:
					throw new InvalidOperationException("Invalid type for constant expression.");
			}
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			//   T    N     BF
			//   0    0     1		if !Value and !negated, then branch to false
			//   0    1     0		if !Value and negated, then fall through
			//   1    0     0		if Value and !negated, then fall through
			//   1    1     1		if Value and negated, then branch to false
			// where
			//   T = Value.IsTrue
			//   N = negated
			//   BF = branchToFalse
			if (Value.IsTrue == negated)
				method.Append(Branch.Always(falseLabel));
			// else, fall through
		}
	}

	/// <summary>
	/// Used by expressions whose result is temporarily stored in a local variable,
	/// such as is the case with list and hash creation expressions. When a local
	/// variable is initialized with such an expression, the result can be stored
	/// directly in that variable, removing the need for a temporary local.
	/// 
	/// Note that this is safe ONLY in the variable initializer, since that is the
	/// only place where the variable is guaranteed not to be referred to.
	/// </summary>
	public interface ILocalResultExpression
	{
		void SetTargetVariable(LocalVariable target);
	}

	public abstract class AssignableExpression : Expression
	{
		internal bool IsAssignment = false;

		public abstract void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value, bool useValue);

		public abstract void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op);

		public abstract LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method);

		public abstract void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals);

		public abstract void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals);
	}

	public class BinaryOperatorExpression : Expression
	{
		public BinaryOperatorExpression(Expression left, Expression right, BinaryOperator op)
		{
			Left = left;
			Right = right;
			Operator = op;
		}

		/// <summary>The left operand.</summary>
		public Expression Left;
		/// <summary>The right operand.</summary>
		public Expression Right;
		/// <summary>The binary operator that combines the left and right operands.</summary>
		public BinaryOperator Operator;

		public override bool IsTypeKnown(Compiler compiler)
		{
			switch (Operator)
			{
				case BinaryOperator.Or:
				case BinaryOperator.Xor:
				case BinaryOperator.And:
				case BinaryOperator.Equality:
				case BinaryOperator.Inequality:
				case BinaryOperator.LessThan:
				case BinaryOperator.LessEqual:
				case BinaryOperator.GreaterThan:
				case BinaryOperator.GreaterEqual:
				case BinaryOperator.Comparison:
				case BinaryOperator.ReferenceEquality:
					return true;
				default:
					return false;
			}
		}

		public override Type GetKnownType(Compiler compiler)
		{
			switch (Operator)
			{
				case BinaryOperator.Or:
				case BinaryOperator.Xor:
				case BinaryOperator.And:
				case BinaryOperator.Equality:
				case BinaryOperator.Inequality:
				case BinaryOperator.LessThan:
				case BinaryOperator.LessEqual:
				case BinaryOperator.GreaterThan:
				case BinaryOperator.GreaterEqual:
				case BinaryOperator.ReferenceEquality:
					return compiler.BooleanType;
				case BinaryOperator.Comparison:
					return compiler.IntType;
				default:
					return null;
			}
		}

		public override string ToString(int indent)
		{
			return String.Format("{0} {1} {2}", Left.ToString(indent), GetOperatorToken(Operator), Right.ToString(indent));
		}

		internal static string GetOperatorToken(BinaryOperator op)
		{
			switch (op)
			{
				case BinaryOperator.NullCoalescing: return "??";
				case BinaryOperator.NullOr: return "?!";
				case BinaryOperator.Or: return "or";
				case BinaryOperator.Xor: return "xor";
				case BinaryOperator.And: return "and";
				case BinaryOperator.Equality: return "==";
				case BinaryOperator.Inequality: return "!=";
				case BinaryOperator.LessThan: return "<";
				case BinaryOperator.LessEqual: return "<=";
				case BinaryOperator.GreaterThan: return ">";
				case BinaryOperator.GreaterEqual: return ">=";
				case BinaryOperator.Hash: return "#";
				case BinaryOperator.Dollar: return "$";
				case BinaryOperator.ShiftLeft: return "<<";
				case BinaryOperator.ShiftRight: return ">>";
				case BinaryOperator.Addition: return "+";
				case BinaryOperator.Subtraction: return "-";
				case BinaryOperator.BitwiseOr: return "|";
				case BinaryOperator.BitwiseXor: return "^";
				case BinaryOperator.Multiplication: return "*";
				case BinaryOperator.Division: return "/";
				case BinaryOperator.Modulo: return "%";
				case BinaryOperator.BitwiseAnd: return "&";
				case BinaryOperator.Exponentiation: return "**";
				case BinaryOperator.FunctionApplication: return "->";
				case BinaryOperator.Concatenation: return "::";
				case BinaryOperator.Comparison: return "<=>";
				default: throw new ArgumentException("Invalid BinaryOperator value.");
			}
		}

		public override Expression FoldConstant()
		{
			FoldOperands();

			if (Left is ConstantExpression && Right is ConstantExpression)
			{
				var constLeft = ((ConstantExpression)Left).Value;
				var constRight = ((ConstantExpression)Right).Value;
				if (constLeft.SupportsOperator(Operator, constRight))
					try
					{
						return new ConstantExpression(constLeft.ExecuteOperator(Operator, constRight)).At(this);
					}
					catch (OverflowException e)
					{
						throw new CompileTimeException(this,
							"The expression cannot be evaluated because it results in an arithmetic overflow.",
							e);
					}
					catch (DivideByZeroException e)
					{
						throw new CompileTimeException(this,
							"The expression cannot be evaluated because it results in a division by zero.",
							e);
					}
			}
			return this; // Could not reduce
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			ResolveOperands(context, document);
			return base.ResolveNames(context, document);
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			TransformOperands(currentBlock, forGenerator);
			return base.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			Left.Compile(compiler, method);  // Evaluate the left operand
			Right.Compile(compiler, method); // And then the right
			method.Append(SimpleInstruction.FromOperator(Operator)); // And finally the operator
		}

		protected void FoldOperands()
		{
			Left = Left.FoldConstant();
			Right = Right.FoldConstant();
		}

		protected void ResolveOperands(IDeclarationSpace context, FileNamespace document)
		{
			Left = Left.ResolveNames(context, document);
			Right = Right.ResolveNames(context, document);
		}

		protected void TransformOperands(BlockSpace currentBlock, bool forGenerator)
		{
			Left = Left.TransformClosureLocals(currentBlock, forGenerator);
			Right = Right.TransformClosureLocals(currentBlock, forGenerator);
		}
	}

	public sealed class NullCoalescingExpression : BinaryOperatorExpression
	{
		public NullCoalescingExpression(Expression left, Expression right)
			: base(left, right, BinaryOperator.NullCoalescing)
		{ }

		public override Expression FoldConstant()
		{
			FoldOperands();

			// null ?? Right => Right
			// non-null ?? Right => Left

			if (Left.IsNull)
				return Right; // Not necessarily a /constant/ reduction, but hey
			else if (Left is ConstantExpression || Right.IsNull)
				// Left is a constant non-null expression, or right is always null.
				// a ?? null => a; if a is null, rhs is evaluated and produces null
				// If left were a constant null expression, we would have returned above.
				return Left;
			return this; // Could not reduce
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			// null    ?? x => x
			// notnull ?? _ => notnull
			Left.Compile(compiler, method); // Evaluate the left operand

			var endLabel = new Label("??-end");
			method.Append(new SimpleInstruction(Opcode.Dup)); // Duplicate left operand
			method.Append(Branch.IfNotNull(endLabel)); // Branch to end if not null
			method.Append(new SimpleInstruction(Opcode.Pop)); // Null value on stack, pop it
			Right.Compile(compiler, method); // Evaluate the right operand

			method.Append(endLabel); // Result on stack is either left or right operand
		}
	}

	public sealed class NullOrExpression : BinaryOperatorExpression
	{
		public NullOrExpression(Expression left, Expression right)
			: base(left, right, BinaryOperator.NullOr)
		{ }

		public override Expression FoldConstant()
		{
			FoldOperands();

			// null ?! Right => null
			// non-null ?! Right => Right
			if (Left.IsNull)
				return new ConstantExpression(ConstantValue.Null)
				{
					StartIndex = this.StartIndex,
					EndIndex = this.EndIndex,
				};
			else if (Left is ConstantExpression)
				// At this point, Left is a constant non-null expression.
				// If it were a constant null expression, we would have returned above.
				return Right;
			return base.FoldConstant();
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			// null    ?! _ => null
			// notnull ?! x => x
			Left.Compile(compiler, method); // Evaluate the left operand

			var endLabel = new Label("?!-end");
			method.Append(new SimpleInstruction(Opcode.Dup)); // Duplicate left operand
			method.Append(Branch.IfNull(endLabel)); // Branch to end if null
			method.Append(new SimpleInstruction(Opcode.Pop)); // Non-null value on stack; pop it
			Right.Compile(compiler, method); // Evaluate the right operand

			method.Append(endLabel); // Result on stack is either null or right operand
		}
	}

	public abstract class ConditionalBinaryExpression : BinaryOperatorExpression
	{
		protected ConditionalBinaryExpression(Expression left, Expression right, BinaryOperator op)
			: base(left, right, op)
		{ }

		public abstract override Expression FoldConstant();

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			var endLabel = new Label();
			var falseLabel = new Label();

			CompileBoolean(compiler, falseLabel, false, method); // Compile as a boolean expression
			// Fall through for true
			method.Append(LoadConstant.True()); // Load true
			method.Append(Branch.Always(endLabel)); // Branch to false
			method.Append(falseLabel);
			method.Append(LoadConstant.False()); // Load false
			method.Append(endLabel);
		}

		public abstract override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method);
	}

	public sealed class ConditionalOrExpression : ConditionalBinaryExpression
	{
		public ConditionalOrExpression(Expression left, Expression right)
			: base(left, right, BinaryOperator.Or)
		{ }

		public override Expression FoldConstant()
		{
			FoldOperands();

			if (Left is ConstantExpression)
			{
				var constLeft = ((ConstantExpression)Left).Value;
				if (Right is ConstantExpression)
				{
					var constRight = ((ConstantExpression)Right).Value;
					return new ConstantExpression(constLeft.ExecuteOperator(BinaryOperator.Or, constRight))
					{
						StartIndex = this.StartIndex,
						EndIndex = this.EndIndex,
					};
				}
				if (constLeft.IsTrue)
					return new ConstantExpression(ConstantValue.True)
					{
						StartIndex = this.StartIndex,
						EndIndex = this.EndIndex,
					};
			}

			return this;
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			if (!negated)
			{
				//   A    B    A or B
				//   0    0    0
				//   0    1    1
				//   1    _    1
				//   1    _    1
				//
				// Here, we want to "fall through" if A is FALSE, because then
				// B needs to be evaluated. Otherwise, the result is true.

				var trueLabel = new Label("or-true");
				// Fall through if false; otherwise, branch to trueLabel
				Left.CompileBoolean(compiler, trueLabel, true /* fall through if ¬A */, method);
				// If fallen through, B determines result.
				Right.CompileBoolean(compiler, falseLabel, false /* fall through if B */, method);
				// Fall through for true:
				method.Append(trueLabel);
			}
			else
			{
				//   A    B    ¬A and ¬B (= ¬(A or B))
				//   0    0    1
				//   0    1    0
				//   1    _    0
				//   1    _    0
				//
				// Here, we want to fall through if A is FALSE, because then
				// B needs to be evaluated. Otherwise, the result is false.

				// Fall through if false; otherwise, branch to falseLabel
				Left.CompileBoolean(compiler, falseLabel, true /* fall through if ¬A */, method);
				// If fallen through, B determines result.
				// If ¬B, then true. If B, then false.
				Right.CompileBoolean(compiler, falseLabel, true /* fall through if ¬B */, method);
			}
		}
	}

	public sealed class ConditionalXorExpression : ConditionalBinaryExpression
	{
		public ConditionalXorExpression(Expression left, Expression right)
			: base(left, right, BinaryOperator.Xor)
		{ }

		public override Expression FoldConstant()
		{
			FoldOperands();

			if (Left is ConstantExpression)
			{
				var constLeft = ((ConstantExpression)Left).Value;
				if (Right is ConstantExpression)
				{
					var constRight = ((ConstantExpression)Right).Value;
					return new ConstantExpression(constLeft.ExecuteOperator(BinaryOperator.Xor, constRight))
					{
						StartIndex = this.StartIndex,
						EndIndex = this.EndIndex,
					};
				}

				// false xor false => false
				// false xor true  => true
				// true  xor false => true
				// true  xor true  => false
				// In other words:
				// false xor Right => not not Right
				// true  xor Right => not Right

				// The "not" nodes will be reduced to simpler code in the bytecode.

				var output = new UnaryExpression(Right, UnaryOperator.Not)
					{
						StartIndex = this.StartIndex,
						EndIndex = this.EndIndex,
					};
				if (constLeft.IsTrue)
					output = new UnaryExpression(output, UnaryOperator.Not)
						{
							StartIndex = this.StartIndex,
							EndIndex = this.EndIndex,
						};
				return output;
			}

			return this;
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			Left.Compile(compiler, method);

			if (!negated)
			{
				// a xor b is true if bool(a) != bool(b)
				// this means we can effectively do this:
				/*
					if (a) {
						// if a is true, then b must be false
						if (b) goto falseLabel;
					} else if (!b) {
						// at this point, we know a and b are false,
						// so we branch to falseLabel.
						goto falseLabel;
					}
					// fall through for true
				*/
				// it doesn't matter which order we test the truthiness of
				// a and b in, because xor doesn't short circuit. For simplicity,
				// since 'right' is on the top of the stack, we do the above
				// with a = right and b = left.
				var elseLabel = new Label();
				Right.CompileBoolean(compiler, elseLabel, false, method); // if (right)
				method.Append(Branch.IfTrue(falseLabel)); // if (left) goto falseLabel;
				method.Append(elseLabel); // else
				method.Append(Branch.IfFalse(falseLabel)); // if (!left) goto falseLabel;
				// fall through for true
			}
			else
			{
				// not (a xor b) is true if bool(a) == bool(b)
				// here, it's the same idea as above, except like this:
				/*
					if (a) {
						// if a is true, then b must also be true
						if (!b) goto falseLabel;
					} else if (b) {
						// at this point, we know a is false and b is true,
						// so we branch to falseLabel.
						goto falseLabel;
					}
				*/
				var elseLabel = new Label();
				Right.CompileBoolean(compiler, elseLabel, false, method); // if (right)
				method.Append(Branch.IfFalse(falseLabel)); // if (!b) goto falseLabel;
				method.Append(elseLabel);
				method.Append(Branch.IfTrue(falseLabel)); // if (b) goto falseLabel;
				// fall through for true
			}
		}
	}

	public sealed class ConditionalAndExpression : ConditionalBinaryExpression
	{
		public ConditionalAndExpression(Expression left, Expression right)
			: base(left, right, BinaryOperator.And)
		{ }

		public override Expression FoldConstant()
		{
			FoldOperands();

			if (Left is ConstantExpression)
			{
				var constLeft = ((ConstantExpression)Left).Value;
				if (Right is ConstantExpression)
				{
					var constRight = ((ConstantExpression)Right).Value;
					return new ConstantExpression(constLeft.ExecuteOperator(BinaryOperator.And, constRight))
					{
						StartIndex = this.StartIndex,
						EndIndex = this.EndIndex,
					};
				}
				if (!constLeft.IsTrue)
					return new ConstantExpression(ConstantValue.False)
					{
						StartIndex = this.StartIndex,
						EndIndex = this.EndIndex,
					};
			}

			return this;
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			if (!negated)
			{
				//   A    B    A and B
				//   0   NE    0
				//   0   NE    0
				//   1    0    0
				//   1    1    1
				//
				// Here, we want to fall through if A is TRUE, because then the output
				// depends on B. Otherwise, the result is false.

				// Fall through if true; otherwise, branch to falseLabel.
				Left.CompileBoolean(compiler, falseLabel, false /* fall through if A */, method);
				// If fallen through, B determines result.
				// If B, then true. If ¬B, then false.
				Right.CompileBoolean(compiler, falseLabel, false /* fall through if B */, method);
			}
			else
			{
				//   A    B    ¬A or ¬B (= ¬(A and B))
				//   0   NE    1
				//   0   NE    1
				//   1    0    1
				//   1    1    0
				//
				// Here, we want to fall through if A is TRUE, because then
				// B needs to be evaluated. Otherwise, the result is true.

				var trueLabel = new Label("and-true");
				// Fall through if true; otherwise, branch to trueLabel.
				Left.CompileBoolean(compiler, trueLabel, false /* fall through if A */, method);
				// If fallen through, B determines result.
				// If ¬B, then true. If B, then false.
				Right.CompileBoolean(compiler, falseLabel, true /* fall through if ¬B */, method);
				// Fall through for true:
				method.Append(trueLabel);
			}
		}
	}

	public sealed class ReferenceTestExpression : ConditionalBinaryExpression
	{
		public ReferenceTestExpression(Expression left, bool negated, Expression right)
			: base(left, right, BinaryOperator.ReferenceEquality)
		{
			Negated = negated;
		}

		/// <summary>Indicates whether the conditional expression is negated.</summary>
		public bool Negated = false;

		public override string ToString(int indent)
		{
			return Left.ToString(indent) + (Negated ? " not refeq " : " refeq ") + Right.ToString(indent);
		}

		public override Expression FoldConstant()
		{
			if (Left is ConstantExpression && Right is ConstantExpression)
			{
				var constLeft = ((ConstantExpression)Left).Value;
				var constRight = ((ConstantExpression)Right).Value;

				var result = constLeft.ExecuteOperator(BinaryOperator.ReferenceEquality, constRight);
				if (Negated)
					result = result.ExecuteOperator(UnaryOperator.Not);

				return new ConstantExpression(result)
				{
					StartIndex = this.StartIndex,
					EndIndex = this.EndIndex,
				};
			}

			return this;
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			Left.Compile(compiler, method); // Evaluate left
			Right.Compile(compiler, method); // Evaluate right

			negated ^= this.Negated;

			if (negated)
				method.Append(Branch.RefEquals(falseLabel));
			else
				method.Append(Branch.RefNotEquals(falseLabel));

			// Fall through for true
		}
	}

	public sealed class TypeTestExpression : Expression
	{
		// Note: 'type' may also be null.
		public TypeTestExpression(Expression expr, bool negated, TypeName type)
		{
			Expression = expr;
			Negated = negated;
			Type = type;
		}

		/// <summary>Indicates whether the type test is negated ("is not").</summary>
		public bool Negated;
		/// <summary>The expression whose type to test.</summary>
		public Expression Expression;
		/// <summary>The type to test against.</summary>
		public TypeName Type;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.BooleanType;
		}

		public override string ToString(int indent)
		{
			string format = Negated ? "{0} is not {1}" : "{0} is {1}";
			return string.Format(format, Expression.ToString(indent), Type == null ? "null" : Type.ToString(indent));
		}

		public override Expression FoldConstant()
		{
			if (Expression is ConstantExpression)
			{
				var constExpr = ((ConstantExpression)Expression).Value;

				var typeName = Type == null ? null : Type.Type.FullName;

				bool result;
				switch (constExpr.Type)
				{
					case ConstantValueType.Null:
						result = typeName == null;
						break;
					case ConstantValueType.Boolean:
						result = typeName == "aves.Boolean";
						break;
					case ConstantValueType.Int:
						result = typeName == "aves.Int";
						break;
					case ConstantValueType.UInt:
						result = typeName == "aves.UInt";
						break;
					case ConstantValueType.Real:
						result = typeName == "aves.Real";
						break;
					case ConstantValueType.String:
						result = typeName == "aves.String";
						break;
					case ConstantValueType.Enum:
						result = constExpr.EnumValue.Type == Type.Type;
						break;
					default:
						result = false; // should never happenen
						break;
				}
				return new ConstantExpression(ConstantValue.CreateBoolean(result))
				{
					StartIndex = this.StartIndex,
					EndIndex = this.EndIndex,
				};
			}

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Expression = Expression.ResolveNames(context, document);
			if (Type != null)
				context.GetContainingNamespace().ResolveTypeName(Type, document); // wanted for its side-effects
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Expression = Expression.TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			// Same implementation as ConditionalBinaryExpression.Compile

			var endLabel = new Label();
			var falseLabel = new Label();

			CompileBoolean(compiler, falseLabel, false, method); // Compile as a boolean expression
			// Fall through for true
			method.Append(LoadConstant.True()); // Load true
			method.Append(Branch.Always(endLabel)); // Branch to false
			method.Append(falseLabel);
			method.Append(LoadConstant.False()); // Load false
			method.Append(endLabel);
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			Expression.Compile(compiler, method); // Evaluate the expression (to the left of 'is')

			negated ^= this.Negated;

			var type = Type == null ? null : Type.Type;
			if (type == null || type == compiler.ObjectType)
			{
				// The expression
				//   blah is Object
				// is exactly equivalent to
				//   blah is not null
				// because every type inherits from Object.
				if (type == compiler.ObjectType)
					negated = !negated; // Reverse the polarity.

				// Null test!
				if (negated) // expr is not null => branch to falseLabel if null
					method.Append(Branch.IfNull(falseLabel));
				else // expr is null => branch to falseLabel if not null
					method.Append(Branch.IfNotNull(falseLabel));
			}
			else
			{
				var typeId = method.Module.GetTypeId(type);
				if (negated) // expr is not Type => branch to falseLabel if type matches
					method.Append(Branch.TypeEquals(falseLabel, typeId));
				else // expr is Type => branch to falseLabel if type does /not/ match
				{
					// While there is a brtype instruction, there is no corresponding brntype instruction.
					// So we have to do some extra fiddling for that.
					var trueLabel = new Label("istype-true");
					method.Append(Branch.TypeEquals(trueLabel, typeId));
					// Fall through for false:
					method.Append(Branch.Always(falseLabel));
					method.Append(trueLabel);
				}
			}
		}
	}

	public enum BinaryOperator
	{
		NullCoalescing, NullOr, Or, Xor, And, Equality, Inequality,
		LessThan, LessEqual, GreaterThan, GreaterEqual, Hash, Dollar,
		ShiftLeft, ShiftRight, Addition, Subtraction, BitwiseOr, BitwiseXor,
		Multiplication, Division, Modulo, BitwiseAnd, Exponentiation,
		FunctionApplication, Concatenation, Comparison, ReferenceEquality,
	}

	public sealed class UnaryExpression : Expression
	{
		public UnaryExpression(Expression inner, UnaryOperator op)
		{
			Inner = inner;
			Operator = op;
		}

		/// <summary>The operand of the unary operator.</summary>
		public Expression Inner;
		/// <summary>The operator itself.</summary>
		public UnaryOperator Operator;

		public override bool IsTypeKnown(Compiler compiler) { return Operator == UnaryOperator.Not; }

		public override Type GetKnownType(Compiler compiler)
		{
			return Operator == UnaryOperator.Not ? compiler.BooleanType : null;
		}

		public override string ToString(int indent)
		{
			if (Operator == UnaryOperator.Plus)
				return "+" + Inner.ToString(indent);
			else if (Operator == UnaryOperator.Minus)
				return "-" + Inner.ToString(indent);
			else if (Operator == UnaryOperator.BitwiseNot)
				return "~" + Inner.ToString(indent);
			else if (Operator == UnaryOperator.Not)
				return "not " + Inner.ToString(indent);
			else
				throw new InvalidOperationException("Invalid operator type in UnaryExpression");
		}

		public override Expression FoldConstant()
		{
			Inner = Inner.FoldConstant();

			if (Inner is ConstantExpression)
			{
				var constInner = ((ConstantExpression)Inner).Value;
				if (constInner.SupportsOperator(Operator))
					try
					{
						return new ConstantExpression(constInner.ExecuteOperator(Operator)).At(this);
					}
					catch (OverflowException e)
					{
						throw new CompileTimeException(this,
							"The expression cannot be evaluated because it results in an arithmetic overflow.",
							e);
					}
			}

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Inner = Inner.ResolveNames(context, document);
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Inner = Inner.TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Operator == UnaryOperator.Not)
			{
				// Same implementation as ConditionalBinaryExpression.Compile
				var endLabel = new Label();
				var falseLabel = new Label();

				Inner.CompileBoolean(compiler, falseLabel, true, method);
				// Fall through for true
				method.Append(LoadConstant.True()); // Load true
				method.Append(Branch.Always(endLabel)); // Branch to false
				method.Append(falseLabel);
				method.Append(LoadConstant.False()); // Load false
				method.Append(endLabel);
			}
			else
			{
				Inner.Compile(compiler, method);
				method.Append(SimpleInstruction.FromOperator(Operator));
			}
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			if (Operator == UnaryOperator.Not)
				Inner.CompileBoolean(compiler, falseLabel, !negated, method);
			else
				base.CompileBoolean(compiler, falseLabel, negated, method);
		}
	}

	public enum UnaryOperator { Plus, Minus, BitwiseNot, Not }

	public sealed class ConditionalExpression : Expression
	{
		public ConditionalExpression(Expression cond, Expression truePart, Expression falsePart)
		{
			Condition = cond;
			TruePart = truePart;
			FalsePart = falsePart;
		}

		/// <summary>The condition expression (the first operand).</summary>
		public Expression Condition;
		/// <summary>The "true" part (the second operand).</summary>
		public Expression TruePart;
		/// <summary>The "false" part (the third operand).</summary>
		public Expression FalsePart;

		public override bool IsTypeKnown(Compiler compiler)
		{
			return TruePart.IsTypeKnown(compiler) && FalsePart.IsTypeKnown(compiler) &&
				TruePart.GetKnownType(compiler) == FalsePart.GetKnownType(compiler);
		}

		public override Type GetKnownType(Compiler compiler)
		{
			if (IsTypeKnown(compiler))
				return TruePart.GetKnownType(compiler);

			return null;
		}

		public override string ToString(int indent)
		{
			return String.Format("{0} ? {1} : {2}", Condition.ToString(indent), TruePart.ToString(indent), FalsePart.ToString(indent));
		}

		public override Expression FoldConstant()
		{
			Condition = Condition.FoldConstant();
			TruePart = TruePart.FoldConstant();
			FalsePart = FalsePart.FoldConstant();

			if (Condition is ConstantExpression)
			{
				var constCond = ((ConstantExpression)Condition).Value;

				if (constCond.IsTrue)
					return TruePart;
				else
					return FalsePart;
			}

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Condition = Condition.ResolveNames(context, document);
			TruePart = TruePart.ResolveNames(context, document);
			FalsePart = FalsePart.ResolveNames(context, document);
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Condition = Condition.TransformClosureLocals(currentBlock, forGenerator);
			TruePart = TruePart.TransformClosureLocals(currentBlock, forGenerator);
			FalsePart = FalsePart.TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			var endLabel = new Label("?:-end");
			var falseLabel = new Label("?:-false");

			Condition.CompileBoolean(compiler, falseLabel, false, method); // Evaluate condition
			// Fall through for true
			TruePart.Compile(compiler, method); // Evaluate true part
			method.Append(Branch.Always(endLabel)); // Branch to end

			method.Append(falseLabel);
			FalsePart.Compile(compiler, method); // Evaluate false part
			method.Append(endLabel);
		}
	}

	// Simple assignments only; that is, one value to one storage location.
	public sealed class AssignmentExpression : Expression
	{
		public AssignmentExpression(Expression target, Expression value)
		{
			Target = target;
			Value = value;
		}

		/// <summary>The target of the assignment.</summary>
		public Expression Target;
		/// <summary>The value to be assigned.</summary>
		public Expression Value;

		/// <summary>
		/// If true, the assignment is an expression on its own; otherwise,
		/// the assignment occurs inside another expression, so the rhs value
		/// must be put on the stack after assignment.
		/// </summary>
		/// <remarks>
		/// This is not needed for parallel assignments or compound assignments,
		/// because they are always statements.
		/// </remarks>
		internal bool IgnoreValue;

		public override bool IsTypeKnown(Compiler compiler)
		{
			return Value.IsTypeKnown(compiler);
		}

		public override Type GetKnownType(Compiler compiler)
		{
			return Value.GetKnownType(compiler);
		}

		public override string ToString(int indent)
		{
			return Target.ToString(indent) + " = " + Value.ToString(indent + 1);
		}

		public override Expression FoldConstant()
		{
			Target = Target.FoldConstant();
			Value = Value.FoldConstant();
			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Target = Target.ResolveNames(context, document);
			EnsureAssignable(Target);
			Value = Value.ResolveNames(context, document);
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Target = Target.TransformClosureLocals(currentBlock, forGenerator);
			Value = Value.TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			((AssignableExpression)Target).CompileSimpleAssignment(compiler, method, Value, !IgnoreValue);
		}

		internal static void EnsureAssignable(Expression expr)
		{
			if (!(expr is AssignableExpression))
				throw new CompileTimeException(expr, "This expression cannot be assigned to.");

			((AssignableExpression)expr).IsAssignment = true;

			if (expr is LocalVariableAccess)
			{
				var variable = ((LocalVariableAccess)expr).Variable;
				if (variable.VariableKind == VariableKind.IterationVariable)
					throw new CompileTimeException(expr,
						string.Format("The variable '{0}' is an iteration variable and cannot be reassigned.",
							variable.Name));
			}
			else if (expr is InstanceMemberAccess)
			{
				var member = ((InstanceMemberAccess)expr).Member;
				// Note: fields are always OK
				switch (member.Kind)
				{
					case MemberKind.Constant:
						// This should never happen, should it? Constants are static.
						throw new CompileTimeException(expr,
							string.Format("The constant '{0}.{1}' cannot be reassigned.",
								((ClassMember)member).Parent.FullName, member.Name));
					case MemberKind.MethodGroup:
						throw new CompileTimeException(expr,
							string.Format("Cannot assign to the method '{0}.{1}'.",
								((MethodGroup)member).ParentAsClass.FullName, member.Name));
					case MemberKind.Property:
						{
							var prop = (Property)member;
							if (prop.PropertyKind == PropertyKind.ReadOnly)
								throw new CompileTimeException(expr,
									string.Format("The property '{0}.{1}' cannot be assigned to; it is read-only.",
										prop.Parent.FullName, prop.Name));
							break; // Property is OK
						}
				}
			}
			else if (expr is StaticPropertyAccess)
			{
				var prop = ((StaticPropertyAccess)expr).Property;
				if (prop.PropertyKind == PropertyKind.ReadOnly)
					throw new CompileTimeException(expr,
						string.Format("The property '{0}.{1}' cannot be assigned to; it is read-only.",
							prop.Parent.FullName, prop.Name));
			}
		}
	}

	public sealed class SimpleNameExpression : Expression
	{
		public SimpleNameExpression(string name)
		{
			Name = name;
		}

		/// <summary>The name.</summary>
		public string Name;

		public override string ToString(int indent)
		{
			return Name;
		}

		public override Expression FoldConstant()
		{
			throw new InvalidOperationException("SimpleNameExpression encountered during constant reduction.");
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			bool hasInstance;
			var containingClass = context.GetContainingClass(out hasInstance);

			var member = context.ResolveName(Name, containingClass);
			if (member == null)
				// It might be a global variable, let's try that
				member = document.ResolveName(Name, containingClass);
			if (member == null)
				throw new UndefinedNameException(this, Name);

			var block = context as BlockSpace;

			Expression result;
			switch (member.Kind)
			{
				case MemberKind.Namespace:
					result = new NamespaceAccess((Namespace)member);
					break;
				case MemberKind.GlobalConstant:
					result = new GlobalConstantAccess((GlobalConstant)member);
					break;
				case MemberKind.GlobalVariable:
					{
						var variable = (GlobalVariable)member;
						if (block != null && block.Method != document.Compiler.MainMethod)
							variable.Capture();
						result = new GlobalVariableAccess(variable);
					}
					break;
				case MemberKind.Class:
				case MemberKind.Enum:
					result = new TypeAccess((Type)member);
					break;
				case MemberKind.Field:
					{
						var field = ((Field)member);
						if (field.IsStatic)
						{
							result = new StaticFieldAccess(field);
							break;
						}
						if (!hasInstance)
						{
							if (context.IsInFieldInitializer())
								throw new InstanceMemberAccessException(this, field, "Cannot refer to instance members in a field initializer.");
							throw new InstanceMemberAccessException(this, field);
						}

						var inner = new ThisAccess();
						inner.ResolveNames(context, document);

						result = new InstanceMemberAccess(inner, field.Parent, field);
					}
					break;
				case MemberKind.Constant:
					result = new ClassConstantAccess((ClassConstant)member);
					break;
				case MemberKind.EnumField:
					result = new EnumFieldAccess((EnumField)member, true);
					break;
				case MemberKind.MethodGroup:
					{
						var method = ((MethodGroup)member);
						if (method.IsStatic)
						{
							result = new StaticMethodAccess(method);
							break;
						}
						if (!hasInstance)
						{
							if (context.IsInFieldInitializer())
								throw new InstanceMemberAccessException(this, method, "Cannot refer to instance members in a field initializer.");
							throw new InstanceMemberAccessException(this, method);
						}

						var inner = new ThisAccess();
						inner.ResolveNames(context, document);

						result = new InstanceMemberAccess(inner, (Class)method.Parent, method);
					}
					break;
				case MemberKind.Property:
					{
						var prop = ((Property)member);
						if (prop.IsStatic)
							return new StaticPropertyAccess(prop);
						if (!hasInstance)
						{
							if (context.IsInFieldInitializer())
								throw new InstanceMemberAccessException(this, prop, "Cannot refer to instance members in a field initializer.");
							throw new InstanceMemberAccessException(this, prop);
						}

						if (block != null && block.Method is LocalMethod)
							((LocalMethod)block.Method).Function.CapturesThis = true;

						var inner = new ThisAccess();
						inner.ResolveNames(context, document);

						result = new InstanceMemberAccess(inner, prop.Parent, prop);
					}
					break;
				case MemberKind.Variable:
					{
						// Note: local variables only occur within blocks, so we don't actually
						// need to test whether 'block' is null.

						var variable = (Variable)member;
						if (this.EndIndex < variable.Node.EndIndex &&
							(variable.VariableKind != VariableKind.IterationVariable ||
							this.EndIndex > variable.Node.StartIndex))
							throw new CompileTimeException(this,
								string.Format("The variable '{0}' cannot be accessed before its declaration.", Name));
						var varNode = variable.Node as VariableDeclarator;
						if (varNode != null && varNode.Initializer != null &&
							this.EndIndex >= varNode.Initializer.StartIndex &&
							this.StartIndex < varNode.Initializer.EndIndex)
							throw new CompileTimeException(this,
								string.Format("The variable '{0}' cannot be accessed in its initializer.", Name));

						// If the variable comes from another method, then we must capture it,
						// but only if the current method is a local method. The actual trans-
						// formation takes place elsewhere.
						LocalAccessKind kind;
						if (variable.Parent.Method != block.Method && block.Method is LocalMethod)
						{
							var function = ((LocalMethod)block.Method).Function;
							function.Capture(variable);
							kind = function.Parent == variable.Parent ?
								LocalAccessKind.CapturingSameScope : // the function is in the same block as the variable
								LocalAccessKind.CapturingOtherScope; // the function and the variable are in different blocks
						}
						else
							kind = LocalAccessKind.NonCapturing;

						result = new LocalVariableAccess(variable, kind);
					}
					break;
				case MemberKind.LocalConstant:
					{
						var constant = (LocalConstant)member;
						if (this.EndIndex < constant.Node.EndIndex)
							throw new CompileTimeException(this,
								string.Format("The constant '{0}' cannot be accessed before its declaration.", Name));
						// Note: constants do not need to be captured; they're always inlined.
						result = new LocalConstantAccess(constant);
					}
					break;
				case MemberKind.LocalFunction:
					{
						var function = (LocalFunction)member;

						if (this.EndIndex < function.Node.EndIndex)
							throw new CompileTimeException(this,
								string.Format("The function '{0}' cannot be accessed before its declaration.", Name));

						if (block.Method is LocalMethod)
						{
							var currentFunction = ((LocalMethod)block.Method).Function;
							if (currentFunction != function) // recursive call
							{
								if (function.HasCaptures)
									// If the other function has captured variables, then we need
									// to capture the function's scope. Otherwise those variables
									// won't be  available when invoking the other function.
									// (Note: if the function also captures 'this', it will be in
									// the closure class for that function's block.)
									currentFunction.Capture(function);
								else if (function.CapturesThis)
									// If the other function captures 'this', then the current function
									// needs to capture 'this' as well. Otherwise it won't be available
									// when invoking the other function.
									currentFunction.CapturesThis = true;
							}
						}
						// Local functions don't need to be captured. Or rather, they aren't captured
						// in the same way that local variables are. This local function may be turned
						// into an instance or static method of the containing class, if it does not
						// capture anything other than 'this'.

						result = new LocalFunctionAccess(function);
					}
					break;
				case MemberKind.Ambiguous:
					throw new AmbiguousNameException(this, (AmbiguousMember)member,
						string.Format("The name '{0}' is ambiguous between the following members: {1}",
							Name, ((AmbiguousMember)member).GetMemberNamesJoined()));
				default:
					throw new InvalidOperationException("SimpleNameExpression resolved to an invalid member kind.");
			}

			result.At(this);
			return result;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			throw new InvalidOperationException("SimpleNameExpression encountered when transforming closure locals.");
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException("SimpleNameExpression encountered when emitting bytecode.");
		}
	}

	/// <summary>
	/// Represents a qualified name.
	/// It basically contains a list of dot-separated strings.
	/// </summary>
	public class QualifiedName : ParseNode
	{
		public QualifiedName(string value)
		{
			Parts = new List<string>(value.Split('.'));
		}
		public QualifiedName(List<string> parts)
		{
			Parts = parts;
		}

		/// <summary>The strings that the qualified name consists of.</summary>
		public List<string> Parts;

		public override string ToString(int indent)
		{
			return Parts.JoinString(".");
		}
	}

	/// <summary>
	/// Represents a type name, which is basically a QualifiedName optionally prefixed by "global.".
	/// </summary>
	public sealed class TypeName : QualifiedName
	{
		public TypeName(string value, bool global)
			: base(value)
		{
			IsGlobal = global;
		}
		public TypeName(List<string> parts, bool global)
			: base(parts)
		{
			IsGlobal = global;
		}

		/// <summary>Determines whether the type name is fully qualified ("global.").</summary>
		public bool IsGlobal;

		/// <summary>The <see cref="Type"/> instance that this <see cref="TypeName"/> resolves to.</summary>
		public Type Type;

		public override string ToString(int indent)
		{
			if (Type != null)
				return "‹type " + Type.FullName + "›";
			return IsGlobal ? "global." + base.ToString(indent) : base.ToString(indent);
		}
	}

	public sealed class LambdaExpression : Expression
	{
		// 'body' can be either a Block or an ExpressionStatement.
		// Note that when compiling, the body is always a Block, which simplifies name resolution.
		public LambdaExpression(List<Parameter> parameters, Splat splat, Statement body)
		{
			Parameters = parameters;
			Splat = splat;
			Body = body;
		}

		/// <summary>The parameters of the lambda.</summary>
		public List<Parameter> Parameters;

		/// <summary>The location of the splat, if any.</summary>
		public Splat Splat;

		/// <summary>The body of the lambda expression.
		/// This can be either a <see cref="Block"/> or an <see cref="ExpressionStatement"/>.</summary>
		public Statement Body;

		internal string NameHint;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder("@");

			if (Parameters.Count == 1 && Splat == Splat.None)
			{
				sb.Append(Parameters[0]);
				sb.Append(' ');
			}
			else if (Parameters.Count > 0)
			{
				if (Splat == Splat.Beginning)
					sb.Append("(...");
				else
					sb.Append("(");

				sb.Append(Parameters.JoinString(", ", indent));

				if (Splat == Splat.End)
					sb.Append("...) ");
				else
					sb.Append(") ");
			}

			if (Body is ExpressionStatement)
				sb.AppendFormat("= {0}", ((ExpressionStatement)Body).Expression.ToString(indent));
			else
				sb.Append(Body.ToString(indent));

			return sb.ToString();
		}

		public override Expression FoldConstant()
		{
			throw new InvalidOperationException(UnmovedLambda);
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			var block = context as BlockSpace;

			string funcName;
			if (block != null)
				funcName = block.GetLambdaName(NameHint);
			else
			{
				bool _;
				var @class = context.GetContainingClass(out _);
				if (@class != null)
					funcName = @class.GetLambdaName(NameHint);
				else
				{
					var ns = context.GetContainingNamespace();
					funcName = ns.GetLambdaName(NameHint);
				}
			}

			var funcDecl = new LocalFunctionDeclaration(funcName, Parameters, Splat, (Block)Body)
			{
				StartIndex = this.StartIndex,
				EndIndex = this.EndIndex,
			};
			var func = new LocalFunction(funcName, funcDecl, block ?? context);

			if (block != null)
				block.DeclareLocalFunction(func);
			else
				document.Compiler.AddMethodWithLocalFunctions(func.Method);

			funcDecl.Body.DeclareNames(context as BlockSpace);
			funcDecl.ResolveNames(context, document);

			if (func.Method.IsGenerator)
				document.Compiler.AddGeneratorMethod(func.Method);

			// And then we replace the lambda with a lambda function access! Simple!
			return new LambdaFunctionAccess(func).At(this);
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			throw new InvalidOperationException(UnmovedLambda);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException(UnmovedLambda);
		}

		internal const string UnmovedLambda =
			"Lambda expressions are supposed to be moved to local functions during name resolution.";
	}

	public sealed class LambdaMemberExpression : Expression
	{
		public LambdaMemberExpression() { }

		/// <summary>
		/// The safe-access-chain associated with the lambda.
		/// This list always begins with a <see cref="SafeMemberAccess"/>.
		/// </summary>
		public List<SafeNode> SafeAccessChain = new List<SafeNode>();

		internal string NameHint;

		public override string ToString(int indent)
		{
			return "@" + SafeAccessChain.JoinString();
		}

		public override Expression FoldConstant()
		{
			throw new InvalidOperationException(LambdaExpression.UnmovedLambda);
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			var block = context as BlockSpace;

			string paramName, funcName;
			if (block != null)
			{
				paramName = block.GetLambdaParam();
				funcName = block.GetLambdaName(NameHint);
			}
			else
			{
				bool _;
				var @class = context.GetContainingClass(out _);
				if (@class != null)
				{
					paramName = @class.GetLambdaParam();
					funcName = @class.GetLambdaName(NameHint);
				}
				else
				{
					var ns = context.GetContainingNamespace();
					paramName = ns.GetLambdaParam();
					funcName = ns.GetLambdaName(NameHint);
				}
			}

			// Okay, so, it's time to generate a function for this lambda.
			var paramDecl = new Parameter(paramName, null) { StartIndex = this.StartIndex, EndIndex = this.StartIndex + 1 };
			var param = new Variable(paramName, paramDecl);

			var returnValue = new SafeAccess(new LocalVariableAccess(param, LocalAccessKind.NonCapturing));
			returnValue.Chain = SafeAccessChain; // Overwrite the chain; no need to copy things

			var returnStmt = new ReturnStatement(new List<Expression>());
			returnStmt.ReturnValues.Add(returnValue);

			var bodyBlock = new Block();
			bodyBlock.Statements.Add(returnStmt);

			var parameters = new List<Parameter> { paramDecl };
			var funcDecl = new LocalFunctionDeclaration(funcName, parameters, Splat.None, bodyBlock)
			{
				StartIndex = this.StartIndex,
				EndIndex = this.EndIndex,
			};
			var func = new LocalFunction(funcName, funcDecl, block ?? context);

			if (block != null)
				block.DeclareLocalFunction(func);
			else
				document.Compiler.AddMethodWithLocalFunctions(func.Method);

			funcDecl.ResolveNames(context, document);

			return new LambdaFunctionAccess(func).At(this);
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			throw new InvalidOperationException(LambdaExpression.UnmovedLambda);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException(LambdaExpression.UnmovedLambda);
		}
	}

	public sealed class LambdaOperatorExpression : Expression
	{
		public LambdaOperatorExpression(LambdaOperator op)
		{
			Operator = op;
		}

		public LambdaOperator Operator;

		public override string ToString(int indent)
		{
			return "@" + LambdaOperatorToString(Operator);
		}

		private static string LambdaOperatorToString(LambdaOperator op)
		{
			switch (op)
			{
				case LambdaOperator.Plus: return "+";
				case LambdaOperator.Minus: return "-";
				case LambdaOperator.BitwiseOr: return "|";
				case LambdaOperator.BitwiseXor: return "^";
				case LambdaOperator.Multiplication: return "*";
				case LambdaOperator.Division: return "/";
				case LambdaOperator.Modulo: return "%";
				case LambdaOperator.BitwiseAnd: return "&";
				case LambdaOperator.Exponentiation: return "**";
				case LambdaOperator.Hash: return "#";
				case LambdaOperator.Dollar: return "$";
				case LambdaOperator.ShiftLeft: return "<<";
				case LambdaOperator.ShiftRight: return ">>";
				case LambdaOperator.Equality: return "==";
				case LambdaOperator.Inequality: return "!=";
				case LambdaOperator.Comparison: return "<=>";
				case LambdaOperator.Less: return "<";
				case LambdaOperator.Greater: return ">";
				case LambdaOperator.LessEquals: return "<=";
				case LambdaOperator.GreaterEquals: return ">=";
				case LambdaOperator.BitwiseNot: return "~";
				case LambdaOperator.FuncApplication: return "->";
				case LambdaOperator.Concatenation: return "::";
				case LambdaOperator.Not: return "not";
				case LambdaOperator.Or: return "or";
				case LambdaOperator.Xor: return "xor";
				case LambdaOperator.And: return "and";
				default: throw new ArgumentOutOfRangeException("op");
			}
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			return new StaticMethodAccess(document.Compiler.GetLambdaOperatorMethod(Operator)).At(this);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException(LambdaExpression.UnmovedLambda);
		}
	}

	public enum LambdaOperator
	{
		Plus, Minus, BitwiseOr, BitwiseXor, Multiplication, Division,
		Modulo, BitwiseAnd, Exponentiation, Hash, Dollar, ShiftLeft, ShiftRight,
		Equality, Inequality, Comparison, Less, Greater, LessEquals, GreaterEquals,
		BitwiseNot, FuncApplication, Concatenation, Not, Or, Xor, And,
	}

	#region Primary expressions

	public sealed class ParenthesizedExpression : Expression
	{
		public ParenthesizedExpression(Expression inner)
		{
			Inner = inner;
		}

		/// <summary>The expression contained within the parentheses.</summary>
		public Expression Inner;

		public override bool IsTypeKnown(Compiler compiler)
		{
			return Inner.IsTypeKnown(compiler);
		}

		public override Type GetKnownType(Compiler compiler)
		{
			return Inner.GetKnownType(compiler);
		}

		public override string ToString(int indent)
		{
			return "(" + Inner.ToString(indent) + ")";
		}

		public override Expression FoldConstant()
		{
			throw new InvalidOperationException("Parenthesized expressions are not supposed to exist after name resolution!");
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			return Inner.ResolveNames(context, document, false, false); // always!
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			throw new InvalidOperationException("Parenthesized expressions are not supposed to exist after name resolution!");
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException("Parenthesized expressions are not supposed to exist after name resolution!");
		}
	}

	public sealed class MemberAccess : AssignableExpression
	{
		public MemberAccess(Expression inner, string member)
		{
			Inner = inner;
			Member = member;
		}

		/// <summary>The expression whose member to access.</summary>
		public Expression Inner;

		/// <summary>The name of the member to access.</summary>
		public string Member;

		internal bool IsInvocation = false;

		public override string ToString(int indent)
		{
			return Inner.ToString(indent) + "." + Member;
		}

		public override Expression FoldConstant()
		{
			Inner = Inner.FoldConstant();
			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Inner = Inner.ResolveNames(context, document, true, true);
			if (Inner is NamespaceAccess)
			{
				var ns = ((NamespaceAccess)Inner).Namespace;
				if (ns.ContainsMember(Member))
					return GetNamespaceMemberAccess(ns.GetMember(Member)).At(this);

				throw new UndefinedNameException(this, Member,
					string.Format("The namespace '{0}' does not contain a definition for '{1}'.",
						ns.FullName, Member));
			}
			if (Inner is TypeAccess)
			{
				var type = ((TypeAccess)Inner).Type;
				if (type.ContainsMember(Member))
				{
					bool _;
					return GetTypeMemberAccess(type.GetMember(Member,
						instType: null, fromType: context.GetContainingClass(out _)),
						context).At(this);
				}

				throw new UndefinedNameException(this, Member,
					string.Format("The type '{0}' does not contain a definition for '{1}'.",
					type.FullName, Member));
			}
			if (Inner is ThisAccess || Inner is BaseAccess)
			{
				bool hasInstance;
				var @class = context.GetContainingClass(out hasInstance);
				var thisType = @class;
				if (Inner is BaseAccess)
					@class = (Class)@class.BaseType;

				NamedMember inaccessibleMember;
				var member = @class.GetMember(Member, instType: thisType, fromType: thisType,
					inaccessibleMember: out inaccessibleMember);

				if (inaccessibleMember != null)
					throw new CompileTimeException(this, string.Format("The member '{0}' is not accessible in this context.",
						inaccessibleMember.FullName));
				if (member == null)
					throw new UndefinedNameException(this, Member,
						string.Format("The type '{0}' does not contain a definition for '{1}'.",
							@class.FullName, Member));

				if (member is ClassMember && ((ClassMember)member).IsStatic ||
					member.Kind == MemberKind.MethodGroup && ((MethodGroup)member).IsStatic)
					throw new StaticMemberAccessException(this, member,
						string.Format("The member '{0}.{1}' is static and cannot be accessed through an instance.",
							@class.FullName, Member));

				if (Inner is BaseAccess)
					if (member.Kind == MemberKind.Property && ((Property)member).IsAbstract)
						throw new CompileTimeException(this,
							string.Format("The member '{0}.{1}' is abstract and cannot be accessed through 'base'.",
								@class.FullName, Member));
					else if (!IsInvocation && member.Kind == MemberKind.MethodGroup &&
						((MethodGroup)member).Any(o => o.IsAbstract))
						throw new CompileTimeException(this,
							string.Format("The method '{0}.{1}' cannot be accessed through 'base' as a value because it contains one or more abstract overloads.",
								@class.FullName, Member));

				return new InstanceMemberAccess(Inner,
					member is ClassMember ? ((ClassMember)member).Parent : ((MethodGroup)member).ParentAsClass,
					member).At(this);
			}
			if (Inner.IsTypeKnown(document.Compiler))
			{
				var knownType = Inner.GetKnownType(document.Compiler);

				if (knownType == null)
					throw new CompileTimeException(this, "Cannot access any members on the null value.");

				bool hasInstance;
				var @class = context.GetContainingClass(out hasInstance);

				NamedMember inaccessibleMember;
				var member = knownType.GetMember(Member, instType: knownType, fromType: @class,
					inaccessibleMember: out inaccessibleMember);

				if (inaccessibleMember != null)
					throw new CompileTimeException(this, string.Format("The member '{0}' is not accessible in this context.",
						inaccessibleMember.FullName));
				if (member == null)
					throw new UndefinedNameException(this, Member,
						string.Format("The type '{0}' does not contain a definition for '{1}'.",
							knownType.FullName, Member));

				if (member is ClassMember && ((ClassMember)member).IsStatic ||
					member.Kind == MemberKind.MethodGroup && ((MethodGroup)member).IsStatic)
					throw new StaticMemberAccessException(this, member,
						string.Format("The member '{0}.{1}' is static and cannot be accessed through an instance.",
							knownType.FullName, Member));

				Class declType; // The declaring type of the member
				if (member is ClassMember)
					declType = ((ClassMember)member).Parent;
				else if (member.Kind == MemberKind.MethodGroup)
					declType = ((MethodGroup)member).ParentAsClass;
				else
					throw new InvalidOperationException("Invalid member kind: a type member must be a ClassMember or a MethodGroup.");

				return new InstanceMemberAccess(Inner, declType, member).At(this);
			}
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Inner = Inner.TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (IsAssignment)
				throw new InvalidOperationException("MemberAccess assignment not compiled specially by AssignmentExpression.");

			Inner.Compile(compiler, method); // Evaluate the expression
			method.Append(new LoadMember(method.Module.GetStringId(Member))); // Load the member
		}

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value, bool useValue)
		{
			Inner.Compile(compiler, method); // Evaluate the instance

			LocalVariable valueLocal = null;
			value.Compile(compiler, method); // Evaluate the value
			if (useValue && !value.CanSafelyInline)
			{
				valueLocal = method.GetAnonymousLocal();
				method.Append(new SimpleInstruction(Opcode.Dup));
				method.Append(new StoreLocal(valueLocal));
			}

			method.Append(new StoreMember(method.Module.GetStringId(Member))); // Store the value in the member

			if (useValue)
				if (value.CanSafelyInline)
					value.Compile(compiler, method);
				else
				{
					method.Append(new LoadLocal(valueLocal));
					valueLocal.Done();
				}
		}

		public override void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op)
		{
			Inner.Compile(compiler, method); // Evaluate the instance

			method.Append(new SimpleInstruction(Opcode.Dup)); // Duplicate the instance
			method.Append(new LoadMember(method.Module.GetStringId(Member))); // Load the member

			value.Compile(compiler, method); // Evaluate the value
			method.Append(SimpleInstruction.FromOperator(op)); // instance.member op value -> finalValue

			// Stack: instance, finalValue
			method.Append(new StoreMember(method.Module.GetStringId(Member))); // instance.member = finalValue
		}

		public override LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method)
		{
			if (Inner.CanSafelyInline)
				return null;
			else
			{
				var instanceLocal = method.GetAnonymousLocal();

				Inner.Compile(compiler, method); // Evaluate the instance expression
				method.Append(new StoreLocal(instanceLocal));  // Store the instance in a local

				return new LocalVariable[] { instanceLocal };
			}
		}

		public override void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			if (Inner.CanSafelyInline)
				// If the inner expression can be inlined, evaluate it now
				Inner.Compile(compiler, method);
			else
				// Otherwise, locals contains the instance local, so load that instead
				method.Append(new LoadLocal(locals[0]));
		}

		public override void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			method.Append(new StoreMember(method.Module.GetStringId(Member))); // Store the member :D
		}

		private Expression GetNamespaceMemberAccess(NamedMember member)
		{
			switch (member.Kind)
			{
				case MemberKind.Namespace:
					return new NamespaceAccess((Namespace)member);
				case MemberKind.Class:
				case MemberKind.Enum:
					return new TypeAccess((Type)member);
				case MemberKind.MethodGroup:
					return new StaticMethodAccess((MethodGroup)member);
				case MemberKind.GlobalConstant:
					return new GlobalConstantAccess((GlobalConstant)member);
				case MemberKind.Ambiguous:
					throw new AmbiguousNameException(this, (AmbiguousMember)member,
						string.Format("The name '{0}' is ambiguous between the following members: {1}",
							member.Name, ((AmbiguousMember)member).GetMemberNamesJoined()));
			}

			throw new ArgumentException("Namespace member is of an invalid kind (must be namespace, type, function or const).");
		}

		private Expression GetTypeMemberAccess(NamedMember member, IDeclarationSpace context)
		{
			switch (member.Kind)
			{
				case MemberKind.Field:
					{
						var field = (Field)member;
						if (field.IsStatic)
							return new StaticFieldAccess(field);
						throw new InstanceMemberAccessException(this, member);
					}
				case MemberKind.Property:
					{
						var prop = (Property)member;
						if (prop.IsStatic)
							return new StaticPropertyAccess(prop);
						throw new InstanceMemberAccessException(this, member);
					}
				case MemberKind.MethodGroup:
					{
						var method = (MethodGroup)member;
						if (method.IsStatic)
							return new StaticMethodAccess(method);
						throw new InstanceMemberAccessException(this, member);
					}
				case MemberKind.Constant:
					{
						var constant = (ClassConstant)member;
						return new ClassConstantAccess(constant);
					}
				case MemberKind.EnumField:
					{
						var field = (EnumField)member;
						return new EnumFieldAccess(field,
							context is Enum && ((Enum)context) == field.Parent);
					}
			}

			throw new ArgumentException("Member of type is of an invalid kind (must be field, property, method or const).");
		}
	}

	public sealed class ThisAccess : Expression
	{
		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			throw new InvalidOperationException("Don't call GetKnownType on ThisAccess. Compile specially instead.");
		}

		public override string ToString(int indent)
		{
			return "this";
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			bool hasInstance;
			context.GetContainingClass(out hasInstance);
			if (!hasInstance)
			{
				if (context.IsInFieldInitializer())
					throw new CompileTimeException(this, "Cannot refer to 'this' in a field initializer.");

				throw new CompileTimeException(this, "'this' can only be used in an instance method.");
			}

			var block = context as BlockSpace;
			if (block != null && block.Method is LocalMethod)
				((LocalMethod)block.Method).Function.CapturesThis = true;

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			if (forGenerator)
			{
				var genClass = currentBlock.Method.GeneratorClass;
				return new InstanceMemberAccess(new ThisAccess(), genClass, genClass.ThisField);
			}
			else
			{
				var method = currentBlock.Method;
				if (method is LocalMethod)
				{
					var localFunc = ((LocalMethod)method).Function;
					if (localFunc.CompilationStrategy == LocalFunctionCompilationStrategy.ClosureMethod)
					{
						// We're inside a closure class, oh my god! We've already made sure
						// the closure class captures 'this', so we know there's a 'this' field.
						// So we just load this.'<>this'.

						var closure = localFunc.Parent.ClosureClass;

						return new InstanceMemberAccess(new ThisAccess(), closure, closure.ThisField);
					}
				}
			}
			return this; // Don't change nothin'.
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.Append(new LoadLocal(method.GetParameter(0))); // ldarg.0 == this
		}
	}

	public sealed class BaseAccess : Expression
	{
		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			throw new InvalidOperationException("Don't call GetKnownType on ThisAccess. Compile specially instead.");
		}

		public override string ToString(int indent)
		{
			return "base";
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			bool hasInstance;
			var @class = context.GetContainingClass(out hasInstance);
			if (!hasInstance)
				throw new CompileTimeException(this, "'base' can only be used in an instance method.");
			if (@class.BaseType == null)
				throw new CompileTimeException(this, "'base' cannot be used inside aves.Object.");

			var block = context as BlockSpace;
			if (block != null && block.Method is LocalMethod)
				((LocalMethod)block.Method).Function.CapturesThis = true;

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator) 
		{
			if (forGenerator)
			{
				var genClass = currentBlock.Method.GeneratorClass;
				return new InstanceMemberAccess(new ThisAccess(), genClass, genClass.ThisField);
			}
			else
			{
				var method = currentBlock.Method;
				if (method is LocalMethod)
				{
					var localFunc = ((LocalMethod)method).Function;
					if (localFunc.CompilationStrategy == LocalFunctionCompilationStrategy.ClosureMethod)
					{
						// We're inside a closure class, oh my god! We've already made sure
						// the closure class captures 'this', so we know there's a 'this' field.
						// So we just load this.'<>this'.

						var closure = localFunc.Parent.ClosureClass;

						return new InstanceMemberAccess(new ThisAccess(), closure, closure.ThisField);
					}
				}
			}
			return this; // Don't change nothin'.
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.Append(new LoadLocal(method.GetParameter(0))); // ldarg.0 == this
		}
	}

	public sealed class GlobalAccess : Expression
	{
		public GlobalAccess(string name)
		{
			Name = name;
		}

		/// <summary>The identifier to the right of the dot.</summary>
		public string Name;

		public override string ToString(int indent)
		{
			return "global." + Name;
		}

		public override Expression FoldConstant()
		{
			throw new InvalidOperationException("GlobalAccess nodes are supposed to be gone after name resolution.");
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			var member = document.ResolveName(Name, null, true);
			if (member == null)
				throw new UndefinedNameException(this, Name);

			switch (member.Kind)
			{
				case MemberKind.Namespace:
					return new NamespaceAccess((Namespace)member).At(this);
				case MemberKind.GlobalConstant:
					return new GlobalConstantAccess((GlobalConstant)member).At(this);
				case MemberKind.GlobalVariable:
					{
						var variable = (GlobalVariable)member;
						var block = context as BlockSpace;
						if (block != null && block.Method != document.Compiler.MainMethod)
							variable.Capture();
						return new GlobalVariableAccess(variable).At(this);
					}
				case MemberKind.Class:
				case MemberKind.Enum:
					return new TypeAccess((Type)member).At(this);
				case MemberKind.MethodGroup:
					return new StaticMethodAccess((MethodGroup)member).At(this);
				case MemberKind.Ambiguous:
					throw new AmbiguousNameException(this, (AmbiguousMember)member,
						string.Format("The name '{0}' is ambiguous between the following members: {1}",
							Name, ((AmbiguousMember)member).GetMemberNamesJoined()));
				default:
					throw new Exception("Invalid global member kind: must be namespace, type, function, const or var.");
			}
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException("GlobalAccess nodes are supposed to be gone after name resolution.");
		}
	}

	public sealed class IndexerAccess : AssignableExpression
	{
		public IndexerAccess(Expression inner, List<Expression> arguments)
		{
			Inner = inner;
			Arguments = arguments;
		}

		/// <summary>The expression whose indexer is being accessed.</summary>
		public Expression Inner;
		/// <summary>The arguments for the indexer.</summary>
		public List<Expression> Arguments;

		public override string ToString(int indent)
		{
			return String.Format("{0}[{1}]", Inner.ToString(indent), Arguments.JoinString(", ", indent));
		}

		public override Expression FoldConstant()
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Inner = Inner.ResolveNames(context, document, false, false);
			
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document, false, false);

			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (IsAssignment)
				throw new InvalidOperationException("IndexerAccess assignment not compiled specially by AssignmentExpression.");

			Inner.Compile(compiler, method);
			foreach (var arg in Arguments)
				arg.Compile(compiler, method);

			method.Append(new LoadIndexer(Arguments.Count));
		}

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value, bool useValue)
		{
			Inner.Compile(compiler, method);

			foreach (var arg in Arguments)
				arg.Compile(compiler, method);

			value.Compile(compiler, method);
			LocalVariable valueLocal = null;
			if (useValue && !value.CanSafelyInline)
			{
				valueLocal = method.GetAnonymousLocal();
				method.Append(new SimpleInstruction(Opcode.Dup));
				method.Append(new StoreLocal(valueLocal));
			}

			method.Append(new StoreIndexer(Arguments.Count));

			if (useValue)
				if (value.CanSafelyInline)
					value.Compile(compiler, method);
				else
				{
					method.Append(new LoadLocal(valueLocal));
					valueLocal.Done();
				}
		}

		public override void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op)
		{
			LocalVariable instLocal = null;
			var argLocals = new LocalVariable[Arguments.Count];

			Inner.Compile(compiler, method); // Evaluate the instance
			if (!Inner.CanSafelyInline)
			{
				instLocal = method.GetAnonymousLocal();
				method.Append(new SimpleInstruction(Opcode.Dup)); // Duplicate it
				method.Append(new StoreLocal(instLocal)); // Store it temporarily
			}

			for (var i = 0; i < Arguments.Count; i++)
			{
				var arg = Arguments[i];
				arg.Compile(compiler, method); // Evaluate the argument
				if (!arg.CanSafelyInline)
				{
					argLocals[i] = method.GetAnonymousLocal();
					method.Append(new SimpleInstruction(Opcode.Dup)); // Duplicate the argument
					method.Append(new StoreLocal(argLocals[i]));
				}
			}

			method.Append(new LoadIndexer(Arguments.Count)); // inner[args...]

			value.Compile(compiler, method); // Evaluate the value
			method.Append(SimpleInstruction.FromOperator(op)); // inner[args...] op value = finalValue

			var finalValueLocal = method.GetAnonymousLocal();
			method.Append(new StoreLocal(finalValueLocal)); // Store the final value in a local

			if (instLocal == null)
				Inner.Compile(compiler, method);
			else
			{
				method.Append(new LoadLocal(instLocal));
				instLocal.Done();
			}

			for (var i = 0; i < Arguments.Count; i++)
				if (argLocals[i] == null)
					Arguments[i].Compile(compiler, method);
				else
				{
					method.Append(new LoadLocal(argLocals[i]));
					argLocals[i].Done();
				}

			method.Append(new LoadLocal(finalValueLocal));
			method.Append(new StoreIndexer(Arguments.Count));

			finalValueLocal.Done();
		}

		public override LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method)
		{
			List<LocalVariable> locals = new List<LocalVariable>();

			if (!Inner.CanSafelyInline)
			{
				var innerLocal = method.GetAnonymousLocal();

				Inner.Compile(compiler, method);
				method.Append(new StoreLocal(innerLocal));

				locals.Add(innerLocal);
			}

			foreach (var arg in Arguments)
				if (!arg.CanSafelyInline)
				{
					var argLocal = method.GetAnonymousLocal();

					arg.Compile(compiler, method);
					method.Append(new StoreLocal(argLocal));

					locals.Add(argLocal);
				}

			return locals.Count == 0 ? null : locals.ToArray();
		}

		public override void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			int i = 0;

			if (Inner.CanSafelyInline)
				Inner.Compile(compiler, method);
			else
				method.Append(new LoadLocal(locals[i++]));

			foreach (var arg in Arguments)
				if (arg.CanSafelyInline)
					arg.Compile(compiler, method);
				else
					method.Append(new LoadLocal(locals[i++]));
		}

		public override void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			method.Append(new StoreIndexer(Arguments.Count));
		}
	}

	public sealed class ObjectCreationExpression : Expression
	{
		public ObjectCreationExpression(TypeName type, List<Expression> arguments)
		{
			Type = type;
			Arguments = arguments;
		}

		/// <summary>The type that is being created.</summary>
		public TypeName Type;

		/// <summary>The arguments passed to the constructor.</summary>
		public List<Expression> Arguments;

		internal Method Constructor;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return Type.Type; // Should be resolved at this point.
		}

		public override string ToString(int indent)
		{
			return String.Format("new {0}({1})",
				Type != null ? Type.ToString(indent) : Constructor.Group.ParentAsClass.FullName,
				Arguments.JoinString(", ", indent));
		}

		public override Expression FoldConstant()
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			var type = context.GetContainingNamespace().ResolveTypeName(Type, document);
			bool _;
			Constructor = type.FindConstructor(Type, Arguments.Count,
				instClass: type as Class, fromClass: context.GetContainingClass(out _));

			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document, false, false);

			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			foreach (var arg in Arguments)
				arg.Compile(compiler, method);

			var type = Type != null ? Type.Type : Constructor.Group.ParentAsClass;
			method.Append(new NewObject(method.Module.GetTypeId(type), Arguments.Count));
		}
	}

	public sealed class TypeofExpression : Expression
	{
		public TypeofExpression(Expression inner)
		{
			Inner = inner;
		}

		/// <summary>The expression whose type is being determined.</summary>
		public Expression Inner;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.TypeType;
		}

		public override string ToString(int indent)
		{
			return "typeof(" + Inner.ToString(indent) + ")";
		}

		public override Expression FoldConstant()
		{
			Inner = Inner.FoldConstant();
			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Inner = Inner.ResolveNames(context, document, allowNamespace: false, allowType: true);
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Inner = Inner.TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Inner is TypeAccess)
			{
				var type = ((TypeAccess)Inner).Type;
				method.Append(new LoadTypeToken(method.Module.GetTypeId(type)));
			}
			else
			{
				Inner.Compile(compiler, method);
				method.Append(new SimpleInstruction(Opcode.Ldtype));
			}
		}
	}

	public sealed class IteratorLookup : Expression
	{
		public IteratorLookup(Expression inner)
		{
			Inner = inner;
		}

		/// <summary>The expression whose iterator is being looked up.</summary>
		public Expression Inner;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.IteratorType;
		}

		public override string ToString(int indent)
		{
			return Inner.ToString(indent) + ".iter";
		}

		public override Expression FoldConstant()
		{
			Inner = Inner.FoldConstant();
			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Inner = Inner.ResolveNames(context, document, false, false);
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Inner = Inner.TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			Inner.Compile(compiler, method);
			method.Append(new SimpleInstruction(Opcode.Lditer));
		}
	}

	public abstract class Literal : ConstantExpression
	{
		protected Literal(string str, ConstantValue value)
			: base(value)
		{
			TokenValue = str;
		}

		public string TokenValue;

		public override string ToString(int indent)
		{
			return TokenValue;
		}
	}

	#region Literals

	public sealed class NullLiteral : Literal
	{
		public NullLiteral()
			: base("null", ConstantValue.Null)
		{ }
	}

	public sealed class BooleanLiteral : Literal
	{
		public BooleanLiteral(string token)
			: base(token, ConstantValue.CreateBoolean(token == "true"))
		{ }
	}

	/// <summary>Represents an integer literal (signed or unsigned).</summary>
	public sealed class IntegerLiteral : Literal
	{
		public IntegerLiteral(Token token)
			: base(token.Value, ParseToken(token))
		{ }

		internal static ConstantValue ParseToken(Token token)
		{
			var tokVal = token.Value;

			var isHex = tokVal.StartsWith("0x", true, CI.InvariantCulture);
			if (isHex)
				tokVal = tokVal.Substring(2); // strip hex specifier

			if (tokVal.IndexOf('_') != -1)
				tokVal = tokVal.Replace("_", ""); // underscores are irrelevant!

			var isUnsigned = tokVal.EndsWith("u", StringComparison.InvariantCultureIgnoreCase);
			if (isUnsigned)
				tokVal = tokVal.Substring(0, tokVal.Length - 1); // strip the u/U

			var last = tokVal[tokVal.Length - 1];
			long multiplier = 1;
			switch (last)
			{
				case 'k':
				case 'K':
					multiplier = 1024L;
					break;
				case 'm':
				case 'M':
					multiplier = 1024L * 1024L;
					break;
				case 'g':
				case 'G':
					multiplier = 1024L * 1024L * 1024L;
					break;
				case 't':
				case 'T':
					multiplier = 1024L * 1024L * 1024L * 1024L;
					break;
			}
			if (multiplier != 1)
				tokVal = tokVal.Substring(0, tokVal.Length - 1); // strip multiplier

			// Note: we only want to catch OverflowException below, because that means the literal is too big.
			// We definitely do NOT want to catch FormatException, because then there's a bug in the tokenizer.
			// Same with ArgumentNullException and ArgumentException.

			if (isUnsigned) // this is the only way to get unsigned ints!
			{
				ulong value;
				try
				{
					if (isHex)
						value = (ulong)multiplier * ulong.Parse(tokVal, NumberStyles.AllowHexSpecifier, CI.InvariantCulture);
					else
						value = (ulong)multiplier * ulong.Parse(tokVal, CI.InvariantCulture);
				}
				catch (OverflowException e)
				{
					throw new ParseException(token, string.Format("The integer value '{0}' cannot be represented as a UInt.", tokVal), e);
				}
				return ConstantValue.CreateUInt(value);
			}
			else // signed int
			{
				long value;
				try
				{
					if (isHex)
						value = multiplier * long.Parse(tokVal, NumberStyles.AllowHexSpecifier, CI.InvariantCulture);
					else
						value = multiplier * long.Parse(tokVal, CI.InvariantCulture);
					if (value < 0)
						throw new OverflowException();
				}
				catch (OverflowException e)
				{
					throw new ParseException(token, string.Format("The integer value '{0}' cannot be represented as an Int.", tokVal), e);
				}
				return ConstantValue.CreateInt(value);
			}
		}
	}

	public sealed class RealLiteral : Literal
	{
		public RealLiteral(Token token)
			: base(token.Value, ParseToken(token))
		{ }

		public double RealValue { get { return Value.RealValue; } }

		private static ConstantValue ParseToken(Token token)
		{
			string tokVal = token.Value;

			double value;

			if (tokVal.IndexOf('_') != -1)
				tokVal = tokVal.Replace("_", "");

			try
			{
				value = double.Parse(tokVal,
					NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent,
					CI.InvariantCulture);
			}
			catch (OverflowException e)
			{
				throw new ParseException(token, string.Format("The value '{0}' cannot be represented as a Real.", tokVal), e);
			}

			return ConstantValue.CreateReal(value);
		}
	}

	public sealed class StringLiteral : Literal
	{
		public StringLiteral(StringToken token)
			: base(token.Value, ConstantValue.CreateString(token.RealValue))
		{ }

		public string StringValue { get { return Value.StringValue; } }
	}

	public sealed class RegexLiteral : Literal
	{
		public RegexLiteral(string token)
			: base(token, ConstantValue.Null)
		{
			throw new NotImplementedException();
		}
	}

	#endregion

	public sealed class InvocationExpression : Expression
	{
		public InvocationExpression(Expression inner, List<Expression> args)
		{
			Inner = inner;
			Arguments = args;
		}

		/// <summary>The expression that is being invoked.</summary>
		public Expression Inner;

		/// <summary>The argument passed to the function.</summary>
		public List<Expression> Arguments;

		public override string ToString(int indent)
		{
			return String.Format("{0}({1})", Inner.ToString(indent), Arguments.JoinString(", ", indent));
		}

		public override Expression FoldConstant()
		{
			Inner = Inner.FoldConstant();

			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Inner = Inner.ResolveNames(context, document, false, false);

			if (Inner is InstanceMemberAccess)
			{
				var access = (InstanceMemberAccess)Inner;

				if (access.Member.Kind == MemberKind.MethodGroup)
				{
					var method = (MethodGroup)access.Member;
					var overload = method.FindOverload(Arguments.Count, true);
					if (overload == null)
						throw new CompileTimeException(Inner,
							string.Format("The method '{0}.{1}' does not contain an overload that takes {2} arguments.",
								method.ParentAsClass.FullName, method.Name, Arguments.Count));
					if (access.Inner is BaseAccess && overload.IsAbstract)
						throw new CompileTimeException(Inner,
							string.Format("The method '{0}.{1}' is abstract and cannot be called through 'base'.",
								method.ParentAsClass.FullName, method.Name));
				}
			}
			else if (Inner is StaticMethodAccess)
			{
				var access = (StaticMethodAccess)Inner;
				var overload = access.Method.FindOverload(Arguments.Count, true);
				if (overload == null)
					throw new CompileTimeException(Inner, string.Format("The method '{0}' does not take {1} arguments.",
						access.Method.FullName, Arguments.Count));
			}
			else if (Inner is LocalFunctionAccess)
			{
				var access = (LocalFunctionAccess)Inner;
				if (!access.Function.Method.Accepts(Arguments.Count))
					throw new CompileTimeException(Inner, string.Format("The local function '{0}' does not take {1} arguments.",
						access.Function.Name, Arguments.Count));
			}
			else if (Inner.IsTypeKnown(document.Compiler))
			{
				bool hasInstance;
				Class @class;

				if (Inner is ThisAccess || Inner is BaseAccess)
				{
					@class = context.GetContainingClass(out hasInstance);
				}
				else
				{
					var type = Inner.GetKnownType(document.Compiler);
					if (type == null)
						throw new CompileTimeException(Inner, "Cannot invoke the null value.");
					if (type is Enum)
						throw new CompileTimeException(Inner, "Enum values cannot be invoked.");

					@class = (Class)type;
				}

				NamedMember inaccessibleMember;
				var invocators = @class.GetMember(".call",
					instType: Inner is BaseAccess ? @class.BaseType : @class,
					fromType: @class,
					inaccessibleMember: out inaccessibleMember);
				if (inaccessibleMember != null)
					throw new CompileTimeException(Inner, string.Format("The member '{0}' is not accessible in this context.",
						inaccessibleMember.FullName));
				if (invocators == null || invocators.Kind != MemberKind.MethodGroup)
					throw new CompileTimeException(Inner, string.Format("The class '{0}' does not define an invocator.",
						@class.FullName));

				var invocator = ((MethodGroup)invocators).FindOverload(Arguments.Count, true);
				if (invocator == null)
					throw new CompileTimeException(Inner, string.Format("The class '{0}' does not define an invocator that takes {1} arguments.",
						@class.FullName, Arguments.Count));

				Inner = new InstanceMemberAccess(Inner, ((MethodGroup)invocators).ParentAsClass, invocators);
			}

			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document, false, false);

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Inner = Inner.TransformClosureLocals(currentBlock, forGenerator);

			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].TransformClosureLocals(currentBlock, forGenerator);

			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Inner is InstanceMemberAccess)
			{
				var access = (InstanceMemberAccess)Inner;
				if (access.Member.Kind == MemberKind.MethodGroup)
				{
					var methodGroup = (MethodGroup)access.Member;
					var overload = methodGroup.FindOverload(Arguments.Count, true);
					if (!overload.IsVirtualCall || access.IsBaseAccess)
					{
						access.Inner.Compile(compiler, method); // Evaluate the instance
						CompileArguments(compiler, method); // Evaluate each argument
						method.Append(new StaticCall(method.Module.GetMethodId(methodGroup), Arguments.Count)); // Call it!
						return; // nothing more to do here
					}
				}

				CompileAsMemberCall(compiler, method, access.Inner, access.Member.Name);
				return;
			}
			else if (Inner is StaticMethodAccess)
			{
				// Note: static invocations can never be virtual.
				var methodGroup = ((StaticMethodAccess)Inner).Method;

				CompileArguments(compiler, method); // Evaluate each argument
				method.Append(new StaticCall(method.Module.GetMethodId(methodGroup), Arguments.Count)); // Call it!
				return;
			}
			else if (Inner is MemberAccess)
			{
				// "Normal" member access without a known instance type,
				// compile to CallMember instruction!
				var access = (MemberAccess)Inner;

				CompileAsMemberCall(compiler, method, access.Inner, access.Member);
				return;
			}

			// Default compilation
			Inner.Compile(compiler, method); // Evaluate the invokable expression
			CompileArguments(compiler, method); // Evaluate each argument
			method.Append(new Call(Arguments.Count)); // !ti llaC
		}

		private void CompileAsMemberCall(Compiler compiler, MethodBuilder method, Expression instance, string memberName)
		{
			instance.Compile(compiler, method); // Evaluate the instance expression
			CompileArguments(compiler, method); // Evaluate each argument
			method.Append(new CallMember(method.Module.GetStringId(memberName), Arguments.Count)); // Call the member!
			return;
		}

		private void CompileArguments(Compiler compiler, MethodBuilder method)
		{
			foreach (var arg in Arguments)
				arg.Compile(compiler, method);
		}
	}

	public abstract class ListExpression : Expression
	{
		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.ListType;
		}
	}

	public sealed class ListLiteralExpression : ListExpression, ILocalResultExpression
	{
		public ListLiteralExpression(List<Expression> values)
		{
			Values = values;
		}

		/// <summary>The values contained in the list.</summary>
		public List<Expression> Values;

		private LocalVariable target;

		public override string ToString(int indent)
		{
			return "[" + Values.JoinString(", ", indent) + "]";
		}

		public override Expression FoldConstant()
		{
			for (var i = 0; i < Values.Count; i++)
				Values[i] = Values[i].FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			for (var i = 0; i < Values.Count; i++)
				Values[i] = Values[i].ResolveNames(context, document, false, false);
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Values.Count; i++)
				Values[i] = Values[i].TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Values.Count == 0)
			{
				method.Append(new CreateList(0));
				if (target != null)
					method.Append(new StoreLocal(target));
			}
			else
			{
				// Create the list
				var listLocal = target ?? method.GetAnonymousLocal();

				method.Append(new CreateList(unchecked((uint)Values.Count))); // Create the list
				method.Append(new StoreLocal(listLocal)); // Store it in the local

				// Get a MethodGroup for aves.List.add
				var listAdd = (MethodGroup)compiler.ListType.GetMember("add");

				for (var i = 0; i < Values.Count; i++)
				{
					method.Append(new LoadLocal(listLocal)); // Load the list
					Values[i].Compile(compiler, method); // Evaluate the value
					method.Append(new StaticCall(method.Module.GetMethodId(listAdd), 1)); // Call listLocal.add(value)
					method.Append(new SimpleInstruction(Opcode.Pop)); // Pop the result of the call
				}

				if (listLocal.IsAnonymous)
				{
					method.Append(new LoadLocal(listLocal)); // Load the list (result of expression)
					listLocal.Done();
				}
			}
		}

		public void SetTargetVariable(LocalVariable target)
		{
			this.target = target;
		}
	}

	public sealed class RangeExpression : ListExpression, ILocalResultExpression
	{
		public RangeExpression(Expression low, Expression high, Expression step)
		{
			Low = low;
			High = high;
			Step = step;
		}

		/// <summary>The low value of the range.</summary>
		public Expression Low;
		/// <summary>The high value of the range.</summary>
		public Expression High;
		/// <summary>The distance between each item in the range.</summary>
		public Expression Step;

		private LocalVariable target;

		public override string ToString(int indent)
		{
			return "[" + Low.ToString(indent) + " to " + High.ToString(indent) +
				(Step == null ? "]" : ", " + Step.ToString(indent) + "]");
		}

		public override Expression FoldConstant()
		{
			Low = Low.FoldConstant();
			High = High.FoldConstant();
			Step = Step == null ? new ConstantExpression(ConstantValue.CreateInt(1)) : Step.FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Low = Low.ResolveNames(context, document, false, false);
			High = High.ResolveNames(context, document, false, false);
			if (Step != null)
				Step = Step.ResolveNames(context, document, false, false);
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Low = Low.TransformClosureLocals(currentBlock, forGenerator);
			High = High.TransformClosureLocals(currentBlock, forGenerator);
			if (Step != null)
				Step = Step.TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			// [low to high, step]
			var highInlined = High.CanSafelyInline;
			var stepInlined = Step.CanSafelyInline;

			LocalVariable highLoc = null;
			if (!highInlined)
			{
				// initialize highLoc
				highLoc = method.GetAnonymousLocal();
				High.Compile(compiler, method); // Evaluate the high expression
				method.Append(new StoreLocal(highLoc)); // Store it in highLoc
			}

			LocalVariable stepLoc = null;
			if (!stepInlined)
			{
				// initialize stepLoc
				stepLoc = method.GetAnonymousLocal();
				Step.Compile(compiler, method); // Evaluate the step expression
				method.Append(new StoreLocal(stepLoc)); // Store it in stepLoc
			}

			// Create the list
			var listLoc = target ?? method.GetAnonymousLocal();
			method.Append(new CreateList(4));
			method.Append(new StoreLocal(listLoc));

			// Get a MethodGroup for aves.List.add
			var listAdd = (MethodGroup)compiler.ListType.GetMember("add");

			// Create the main loop

			var counter = method.GetAnonymousLocal();
			// Put low value in counter
			Low.Compile(compiler, method); // Evaluate low expression
			method.Append(new StoreLocal(counter)); // Put the low value in the counter

			var loopCond = new Label("range-start");
			var loopStart = new Label("range-end");

			method.Append(Branch.Always(loopCond)); // Jump to loop condition
			{ // Loop body
				method.Append(loopStart);
				method.Append(new LoadLocal(listLoc)); // Load the list
				method.Append(new LoadLocal(counter)); // Load counter
				method.Append(new StaticCall(method.Module.GetMethodId(listAdd), 1)); // Call list.add(counter)
				method.Append(new SimpleInstruction(Opcode.Pop));
			}
			{ // Loop increment
				method.Append(new LoadLocal(counter)); // Load counter
				if (stepInlined) // Load step value
					Step.Compile(compiler, method);
				else
					method.Append(new LoadLocal(stepLoc));
				method.Append(new SimpleInstruction(Opcode.Add)); // Add counter + step value

				method.Append(new StoreLocal(counter)); // counter += step
			}
			{ // Loop condition
				method.Append(loopCond);

				method.Append(new LoadLocal(counter)); // Load counter
				if (highInlined) // Load high value
					High.Compile(compiler, method);
				else
					method.Append(new LoadLocal(highLoc));

				method.Append(new SimpleInstruction(Opcode.Lte)); // counter <= highLoc?
				method.Append(Branch.IfTrue(loopStart)); // If so, branch to start of body
				// If false, fall through to the end
			}

			if (listLoc.IsAnonymous)
			{
				method.Append(new LoadLocal(listLoc)); // Load the list (result of expression)
				listLoc.Done();
			}

			counter.Done();
			if (!highInlined) highLoc.Done();
			if (!stepInlined) stepLoc.Done();
		}

		public void SetTargetVariable(LocalVariable target)
		{
			this.target = target;
		}
	}

	public sealed class ListComprehension : ListExpression, ILocalResultExpression
	{
		public ListComprehension(bool isGenerator, List<Expression> expr, List<ListCompIterator> iterators)
		{
			IsGenerator = isGenerator;
			Expressions = expr;
			Iterators = iterators;
		}

		/// <summary>Indicates whether the comprehension is a generator ("yield ...").</summary>
		public bool IsGenerator;

		/// <summary>The expressions used to generate values for the comprehension.</summary>
		public List<Expression> Expressions;

		/// <summary>The iterators ("for ... in" bits) associated with the comprehension.</summary>
		public List<ListCompIterator> Iterators;

		internal BlockSpace ImplicitBlock;

		private LocalVariable target;

		public override string ToString(int indent)
		{
			return (IsGenerator ? "[yield " : "[") + Expressions.JoinString(", ", indent) + " " +
				Iterators.JoinString(" ", indent) + "]";
		}

		public override Expression FoldConstant()
		{
			for (var i = 0; i < Expressions.Count; i++)
				Expressions[i] = Expressions[i].FoldConstant();

			for (var i = 0; i < Iterators.Count; i++)
				Iterators[i].FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			if (ImplicitBlock == null)
			{
				ImplicitBlock = new BlockSpace(new Block(), context as BlockSpace);
				foreach (var iter in Iterators)
					iter.DeclareNames(ImplicitBlock);
			}

			for (var i = 0; i < Expressions.Count; i++)
				Expressions[i] = Expressions[i].ResolveNames(ImplicitBlock, document, false, false);

			for (var i = 0; i < Iterators.Count; i++)
				Iterators[i].ResolveNames(ImplicitBlock, document);

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Expressions.Count; i++)
				Expressions[i] = Expressions[i].TransformClosureLocals(ImplicitBlock, forGenerator);

			for (var i = 0; i < Iterators.Count; i++)
				Iterators[i].TransformClosureLocals(ImplicitBlock, forGenerator);

			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			var listLoc = target ?? method.GetAnonymousLocal();
			// Initialize the list local
			method.Append(new CreateList(0));
			method.Append(new StoreLocal(listLoc));

			Iterators[0].Compile(compiler, method, 0, this, listLoc);

			if (listLoc.IsAnonymous)
			{
				method.Append(new LoadLocal(listLoc)); // Load the result of the expression
				listLoc.Done();
			}
		}

		public void SetTargetVariable(LocalVariable target)
		{
			this.target = target;
		}
	}

	public sealed class ListCompIterator : ParseNode
	{
		public ListCompIterator(List<string> variables, Expression expr)
		{
			VariableNames = variables;
			Expression = expr;
		}

		/// <summary>The variables to be iterated over.</summary>
		public List<string> VariableNames;

		/// <summary>The expression that makes up the list to iterate over.</summary>
		public Expression Expression;

		internal Variable[] Variables;

		public override string ToString(int indent)
		{
			return "for " + VariableNames.JoinString(", ") + " in " + Expression.ToString(indent);
		}

		public void FoldConstant()
		{
			Expression = Expression.FoldConstant();
		}

		public void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Expression = Expression.ResolveNames(context, document, false, false);
		}

		public void DeclareNames(BlockSpace parent)
		{
			Variables = new Variable[VariableNames.Count];
			for (var i = 0; i < VariableNames.Count; i++)
			{
				var variable = new Variable(VariableNames[i],
					new VariableDeclarator(VariableNames[i], null)
					{
						StartIndex = Expression.EndIndex,
						EndIndex = Expression.EndIndex,
					}, VariableKind.IterationVariable);
				parent.DeclareVariable(variable);
				Variables[i] = variable;
			}
		}

		public void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Expression = Expression.TransformClosureLocals(currentBlock, forGenerator);
		}

		public void Compile(Compiler compiler, MethodBuilder method, int index, ListComprehension parent, LocalVariable listLoc)
		{
			if (Expression is RangeExpression)
				CompileRange(compiler, method, index, parent, listLoc);
			else
				CompileIterator(compiler, method, index, parent, listLoc);
		}

		private void CompileRange(Compiler compiler, MethodBuilder method, int index, ListComprehension parent, LocalVariable listLoc)
		{
			var range = (RangeExpression)Expression;
			var highInlined = range.High.CanSafelyInline;
			var stepInlined = range.Step.CanSafelyInline;

			var counter = method.GetLocal(VariableNames[0]);

			range.Low.Compile(compiler, method);
			method.Append(new StoreLocal(counter)); // Store low in counter

			LocalVariable highLoc = null;
			if (!highInlined)
			{
				highLoc = method.GetAnonymousLocal();
				range.High.Compile(compiler, method);
				method.Append(new StoreLocal(highLoc));
			}

			LocalVariable stepLoc = null;
			if (!stepInlined)
			{
				stepLoc = method.GetAnonymousLocal();
				range.Step.Compile(compiler, method);
				method.Append(new StoreLocal(stepLoc));
			}

			var loopCond = new Label("listcomp-cond");
			var loopEnd = new Label("listcomp-end");

			{ // Condition
				method.Append(loopCond);

				method.Append(new LoadLocal(counter)); // Load counter
				if (highInlined) // Load high
					range.High.Compile(compiler, method);
				else
					method.Append(new LoadLocal(highLoc));
				method.Append(new SimpleInstruction(Opcode.Lte)); // counter <= high

				method.Append(Branch.IfFalse(loopEnd)); // End loop if not counter <= high
			}
			{ // Body
				if (index == parent.Iterators.Count - 1)
					CompileBody(compiler, method, parent, listLoc);
				else
					parent.Iterators[index + 1].Compile(compiler, method, index + 1, parent, listLoc);
			}
			{ // Increment
				method.Append(new LoadLocal(counter)); // Load counter
				if (stepInlined) // Load step
					range.Step.Compile(compiler, method);
				else
					method.Append(new LoadLocal(stepLoc));
				method.Append(new SimpleInstruction(Opcode.Add)); // Add counter + step
				method.Append(new StoreLocal(counter)); // counter += step

				method.Append(Branch.Always(loopCond)); // Jump to condition
			}
			method.Append(loopEnd);
		}

		private void CompileIterator(Compiler compiler, MethodBuilder method, int index, ListComprehension parent, LocalVariable listLoc)
		{
			var iterLoc = method.GetAnonymousLocal();

			Expression.Compile(compiler, method);
			method.Append(new SimpleInstruction(Opcode.Lditer));
			method.Append(new StoreLocal(iterLoc));

			var loopCond = new Label("listcomp-cond");
			var loopEnd = new Label("listcomp-end");

			{ // Condition
				method.Append(loopCond);

				method.Append(new LoadLocal(iterLoc)); // Load iterator
				method.Append(new CallMember(method.Module.GetStringId("moveNext"), 0)); // Call iterLoc.moveNext()

				method.Append(Branch.IfFalse(loopEnd));
			}
			{ // Initialize loop variable(s)
				method.Append(new LoadLocal(iterLoc));
				method.Append(new LoadMember(method.Module.GetStringId("current")));

				if (VariableNames.Count == 1)
					method.Append(new StoreLocal(method.GetLocal(VariableNames[0])));
				else
					compiler.Unpack(method, VariableNames);
			}
			{ // Body

				if (index == parent.Iterators.Count - 1)
					CompileBody(compiler, method, parent, listLoc);
				else
					parent.Iterators[index + 1].Compile(compiler, method, index + 1, parent, listLoc);

				method.Append(Branch.Always(loopCond));
			}

			method.Append(loopEnd);

			iterLoc.Done();
		}

		private void CompileBody(Compiler compiler, MethodBuilder method, ListComprehension parent, LocalVariable listLoc)
		{
			var listAdd = (MethodGroup)compiler.ListType.GetMember("add");

			parent.ImplicitBlock.Node.Compile(compiler, method); // Closure initializers, if any

			// Add each expression to the list
			foreach (var expr in parent.Expressions)
			{
				method.Append(new LoadLocal(listLoc));
				expr.Compile(compiler, method);
				method.Append(new StaticCall(method.Module.GetMethodId(listAdd), 1));
				method.Append(new SimpleInstruction(Opcode.Pop));
			}
		}
	}

	public sealed class HashLiteralExpression : Expression, ILocalResultExpression
	{
		public HashLiteralExpression(List<HashMember> members)
		{
			Members = members;
		}

		/// <summary>The members of the hash.</summary>
		public List<HashMember> Members;

		private LocalVariable target;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.HashType;
		}

		public override string ToString(int indent)
		{
			if (Members.Count == 0)
				return "{}";
			else
				return "{" + Members.JoinString(", ", indent) + "}";
		}

		public override Expression FoldConstant()
		{
			foreach (var member in Members)
			{
				member.Key = member.Key.FoldConstant();
				member.Value = member.Value.FoldConstant();
			}
			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			foreach (var member in Members)
			{
				member.Key = member.Key.ResolveNames(context, document, false, false);
				member.Value = member.Value.ResolveNames(context, document, false, false);
			}
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			foreach (var member in Members)
			{
				member.Key = member.Key.TransformClosureLocals(currentBlock, forGenerator);
				member.Value = member.Value.TransformClosureLocals(currentBlock, forGenerator);
			}
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Members.Count == 0)
			{
				method.Append(new CreateHash(0));
				if (target != null)
					method.Append(new StoreLocal(target));
			}
			else
			{
				// Create the hash
				var hashLocal = target ?? method.GetAnonymousLocal();

				method.Append(new CreateHash(unchecked((uint)Members.Count))); // Create the hash
				method.Append(new StoreLocal(hashLocal)); // Store it in the local

				// Get a MethodGroup for aves.Hash.add
				var hashAdd = (MethodGroup)compiler.HashType.GetMember("add");

				foreach (var member in Members)
				{
					method.Append(new LoadLocal(hashLocal)); // Load the hash
					member.Key.Compile(compiler, method); // Evaluate the key
					member.Value.Compile(compiler, method); // Evaluate the value
					method.Append(new StaticCall(method.Module.GetMethodId(hashAdd), 2)); // Call hashLocal.add(key, value)
					method.Append(new SimpleInstruction(Opcode.Pop));
				}

				if (hashLocal.IsAnonymous)
				{
					method.Append(new LoadLocal(hashLocal)); // Load the hash (result of expression)
					hashLocal.Done();
				}
			}
		}

		public void SetTargetVariable(LocalVariable target)
		{
			this.target = target;
		}
	}

	public sealed class HashMember : ParseNode
	{
		// The key can be an identifier, a Literal (Type == String or Int),
		// or a ParenthesizedExpression
		public HashMember(Expression key, Expression value)
		{
			Key = key;
			Value = value;
		}

		/// <summary>The key of the hash member.</summary>
		public Expression Key;

		/// <summary>The value of the hash member.</summary>
		public Expression Value;

		public override string ToString(int indent)
		{
			return Key.ToString(indent) + ": " + Value.ToString(indent);
		}
	}

	public sealed class SafeAccess : Expression
	{
		public SafeAccess(Expression inner)
		{
			Inner = inner;
		}
		/// <summary>The expression that starts the safe access.</summary>
		public Expression Inner;

		/// <summary>The safe access chain that follows the inner expression.
		/// This list is never empty; it always begins with a <see cref="SafeMemberAccess"/> or
		/// <see cref="SafeInvocation"/> with <see cref="SafeMemberAccess.IsSafe"/> set to true.</summary>
		public List<SafeNode> Chain = new List<SafeNode>();

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append(Inner.ToString(indent));
			for (var i = 0; i < Chain.Count; i++)
				sb.Append(Chain[i].ToString(indent + 1));
			return sb.ToString();
		}

		public override Expression FoldConstant()
		{
			Inner = Inner.FoldConstant();
			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Inner = Inner.ResolveNames(context, document, false, false);

			foreach (var link in Chain)
				link.ResolveNames(context, document);

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Inner = Inner.TransformClosureLocals(currentBlock, forGenerator);

			foreach (var link in Chain)
				link.TransformClosureLocals(currentBlock, forGenerator);

			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			// emit instance expression first
			Inner.Compile(compiler, method);

			// instance is on the stack
			var endLabel = new Label("safe-chain-end");

			foreach (var node in Chain)
			{
				if (node.IsSafe)
				{
					method.Append(new SimpleInstruction(Opcode.Dup)); // duplicate result of last expression
					method.Append(Branch.IfNull(endLabel)); // branch to end if null
				}

				if (node is SafeMemberAccess)
				{
					var memberAccess = (SafeMemberAccess)node;
					method.Append(new LoadMember(method.Module.GetStringId(memberAccess.Member)));
				}
				else if (node is SafeInvocation)
				{
					var invocation = (SafeInvocation)node;
					foreach (var arg in invocation.Arguments)
						arg.Compile(compiler, method);
					method.Append(new Call(invocation.Arguments.Count));
				}
				else if (node is SafeIndexerAccess)
				{
					var indexer = (SafeIndexerAccess)node;
					foreach (var arg in indexer.Arguments)
						arg.Compile(compiler, method);
					method.Append(new LoadIndexer(indexer.Arguments.Count));
				}
				else if (node is SafeIteratorLookup)
				{
					var iterLookup = (SafeIteratorLookup)node;
					method.Append(new SimpleInstruction(Opcode.Lditer));
				}
			}

			// The endLabel could only ever be reached by either
			// fully evaluating the safe access chain, or by reaching
			// null before a safe node.

			method.Append(endLabel);
		}
	}

	public abstract class SafeNode : ParseNode
	{
		public SafeNode(bool isSafe)
		{
			IsSafe = isSafe;
		}

		/// <summary>Whether the node is a safe access ("?.", "?(...)" or "?[...]").</summary>
		public bool IsSafe;

		public virtual void FoldConstant() { }

		public virtual void ResolveNames(IDeclarationSpace context, FileNamespace document) { }

		public virtual void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator) { }
	}

	public sealed class SafeMemberAccess : SafeNode
	{
		public SafeMemberAccess(string member, bool isSafe)
			: base(isSafe)
		{
			Member = member;
		}

		/// <summary>The name of the member to access.</summary>
		public string Member;

		public override string ToString(int indent)
		{
			return (IsSafe ? "?." : ".") + Member;
		}
	}

	public sealed class SafeInvocation : SafeNode
	{
		public SafeInvocation(List<Expression> args, bool isSafe)
			: base(isSafe)
		{
			Arguments = args;
		}

		/// <summary>The arguments passed into the call.</summary>
		public List<Expression> Arguments;

		public override string ToString(int indent)
		{
			return (IsSafe ? "?(" : "(") + Arguments.JoinString(", ", indent + 1) + ")";
		}

		public override void FoldConstant()
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].TransformClosureLocals(currentBlock, forGenerator);
		}
	}

	public sealed class SafeIndexerAccess : SafeNode
	{
		public SafeIndexerAccess(List<Expression> args, bool isSafe)
			: base(isSafe)
		{
			Arguments = args;
		}

		/// <summary>The arguments passed to the indexer.</summary>
		public List<Expression> Arguments;

		public override string ToString(int indent)
		{
			return (IsSafe ? "?[" : "[") + Arguments.JoinString(", ", indent + 1) + "]";
		}

		public override void FoldConstant()
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].TransformClosureLocals(currentBlock, forGenerator);
		}
	}

	public sealed class SafeIteratorLookup : SafeNode
	{
		public SafeIteratorLookup(bool isSafe)
			: base(isSafe)
		{ }

		public override string ToString(int indent)
		{
			return (IsSafe ? "?." : ".") + "iter";
		}
	}

	#endregion
}