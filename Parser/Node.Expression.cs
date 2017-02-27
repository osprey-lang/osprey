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
				var lva = this as LocalVariableAccess;
				if (lva != null)
					return lva.Variable.CanSafelyInline;

				return this is ThisAccess || // but not BaseAccess; 'base' is not supposed to occur on its own
					this is ConstantExpression ||
					this is StaticMethodAccess;
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
		/// <param name="context">The context in which to resolve names. This is usually a block;
		/// the exception is initializers for fields, which occur inside class bodies.</param>
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
		internal Expression At(int startIndex, int endIndex, Document document)
		{
			this.StartIndex = startIndex;
			this.EndIndex = endIndex;
			this.Document = document;
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
						return "{const null}";
					case ConstantValueType.Boolean:
						return Value.BooleanValue ? "{const true}" : "{const false}";
					case ConstantValueType.Int:
						return "{const " + Value.IntValue.ToStringInvariant() + "}";
					case ConstantValueType.UInt:
						return "{const " + Value.UIntValue.ToStringInvariant() + "u}";
					case ConstantValueType.Real:
						return "{const " + Value.RealValue.ToStringInvariant() + "r}";
					case ConstantValueType.String:
						return "{const \"" + Value.StringValue + "\"}";
					case ConstantValueType.Char:
						return "{const U+" + Value.CharValue.ToString("X4") + "'}";
					case ConstantValueType.Enum:
						return "{const enum value}";
				}
				return string.Format("{const, Type={0}}", Value.Type);
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
				case ConstantValueType.Char:
					// Use ldenum for char values as well;
					// ldenum really just loads a primitive value
					method.Append(new LoadEnum(method.Module.GetTypeId(compiler.CharType), constValue.CharValue));
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

	/// <summary>
	/// Represents an expression that might be able to be assigned to. Derived classes are not
	/// guaranteed to represent actual assignable expressions. Rather, deriving from this class
	/// is an indication that the expression *might* be assignable after name resolution, unlike
	/// other expressions, which are never assignable.
	/// </summary>
	public abstract class AssignableExpression : Expression
	{
		private bool isAssignment = false;
		public bool IsAssignment
		{
			get { return isAssignment; }
			set { isAssignment = value; }
		}

		public sealed override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			return ResolveNames(context, document, ExpressionAccessKind.Read);
		}

		public abstract Expression ResolveNames(IDeclarationSpace context, FileNamespace document, ExpressionAccessKind kind);

		public abstract void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value);

		public abstract void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op);

		public abstract LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method);

		public abstract void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals);

		public abstract void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals);
	}

	/// <summary>
	/// Determines whether an expression is read, written, or both.
	/// </summary>
	[Flags]
	public enum ExpressionAccessKind
	{
		/// <summary>The expression is read from.</summary>
		Read = 1,
		/// <summary>The expression is assigned to.</summary>
		Write = 2,
		/// <summary>The expression is both read from and written to, e.g. in a compound assignment.</summary>
		ReadWrite = Read | Write,
	}

	public sealed class RefExpression : Expression
	{
		public RefExpression(Expression inner)
		{
			Inner = inner;
		}

		/// <summary>The expression that this expression takes a reference to.</summary>
		public Expression Inner;

		public override string ToString(int indent)
		{
			return "ref " + Inner.ToString(indent);
		}

		public override Expression FoldConstant()
		{
			Inner = Inner.FoldConstant();
			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Inner = Inner.ResolveNames(context, document, false, false);

			if (Inner is LocalVariableAccess)
			{
				var access = (LocalVariableAccess)Inner;
				access.Variable.EnsureAssignable(Inner);
				// Passing a variable by ref also counts as assigning to it
				access.Variable.AssignmentCount++;
			}
			else
			{
				var instMemAccess = Inner as InstanceMemberAccess;
				if ((instMemAccess == null || instMemAccess.Member.Kind != MemberKind.Field) &&
					!(Inner is StaticFieldAccess) &&
					!(Inner is MemberAccess))
					throw new CompileTimeException(Inner,
						"A ref argument must be a variable, instance field, static field, or member access.");
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
			if (Inner is LocalVariableAccess)
			{
				var variable = ((LocalVariableAccess)Inner).Variable;

				var local = variable.IsParameter ? method.GetParameter(variable.Name) : method.GetLocal(variable.Name);
				method.Append(new LoadLocalReference(local));
			}
			else if (Inner is InstanceMemberAccess)
			{
				var access = (InstanceMemberAccess)Inner;
				var field = (Field)access.Member;

				access.Inner.Compile(compiler, method);
				method.Append(LoadFieldReference.Create(method.Module, field));
			}
			else if (Inner is StaticFieldAccess)
			{
				var field = ((StaticFieldAccess)Inner).Field;
				method.Append(LoadFieldReference.Create(method.Module, field));
			}
			else if (Inner is MemberAccess)
			{
				var access = (MemberAccess)Inner;

				access.Inner.Compile(compiler, method);
				method.Append(new LoadMemberReference(method.Module.GetStringId(access.Member)));
			}
			else
				throw new InvalidOperationException();
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			throw new InvalidOperationException("Ref expressions should never occur outside an argument list.");
		}
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
				default: throw new ArgumentOutOfRangeException("op", "Invalid BinaryOperator value.");
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

			if (Operator == BinaryOperator.FunctionApplication)
			{
				if (Right.IsTypeKnown(document.Compiler) &&
					!Right.GetKnownType(document.Compiler).InheritsFrom(document.Compiler.ListType))
					throw new CompileTimeException(Right,
						"The the right-hand operand in a function application must be of type aves.List.");
			}

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			TransformOperands(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Operator == BinaryOperator.FunctionApplication)
				CompileFunctionApplication(compiler, method);
			else
			{
				Left.Compile(compiler, method);  // Evaluate the left operand
				Right.Compile(compiler, method); // And then the right
				method.Append(SimpleInstruction.FromOperator(Operator)); // And finally the operator
			}
		}

		private void CompileFunctionApplication(Compiler compiler, MethodBuilder method)
		{
			if (Left is StaticMethodAccess)
			{
				var access = (StaticMethodAccess)Left;
				Right.Compile(compiler, method); // Load the arguments list
				method.Append(new StaticApply(method.Module.GetMethodId(access.Method))); // Apply!
			}
			else
			{
				Left.Compile(compiler, method);  // Evaluate the left operand
				Right.Compile(compiler, method); // And then the right
				method.Append(SimpleInstruction.FromOperator(BinaryOperator.FunctionApplication));
			}
		}

		protected void FoldOperands()
		{
			Left = Left.FoldConstant();
			Right = Right.FoldConstant();
		}

		protected void ResolveOperands(IDeclarationSpace context, FileNamespace document)
		{
			Left = Left.ResolveNames(context, document, false, false);
			Right = Right.ResolveNames(context, document, false, false);
		}

		protected void TransformOperands(BlockSpace currentBlock, bool forGenerator)
		{
			Left = Left.TransformClosureLocals(currentBlock, forGenerator);
			Right = Right.TransformClosureLocals(currentBlock, forGenerator);
		}
	}

	public sealed class ConcatenationExpression : BinaryOperatorExpression
	{
		public ConcatenationExpression(Expression left, Expression right)
			: base(left, right, BinaryOperator.Concatenation)
		{ }

		public override bool IsTypeKnown(Compiler compiler)
		{
			if (Left.IsTypeKnown(compiler) && Right.IsTypeKnown(compiler))
			{
				var leftType = Left.GetKnownType(compiler);
				var rightType = Right.GetKnownType(compiler);

				if (leftType == compiler.ListType || rightType == compiler.ListType ||
					leftType == compiler.HashType || rightType == compiler.HashType)
					return leftType == rightType;
				else
					return true;
			}
			return false;
		}

		public override Type GetKnownType(Compiler compiler)
		{
			if (Left.IsTypeKnown(compiler) && Right.IsTypeKnown(compiler))
			{
				var leftType = Left.GetKnownType(compiler);
				var rightType = Right.GetKnownType(compiler);

				if (leftType == compiler.ListType || rightType == compiler.ListType)
				{
					if (leftType == rightType)
						return compiler.ListType;
				}
				else if (leftType == compiler.HashType || rightType == compiler.HashType)
				{
					if (leftType == rightType)
						return compiler.HashType;
				}
				else
					return compiler.StringType;
			}
			return null;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			base.ResolveOperands(context, document);

			var compiler = document.Compiler;
			if (Left.IsTypeKnown(compiler) && Right.IsTypeKnown(compiler))
			{
				var leftType = Left.GetKnownType(compiler);
				var rightType = Right.GetKnownType(compiler);

				if (leftType == compiler.ListType || rightType == compiler.ListType)
				{
					if (leftType != rightType)
						throw new CompileTimeException(leftType == compiler.ListType ? Right : Left,
							"If one operand of '::' is a list, the other must be as well.");
				}
				if (leftType == compiler.HashType || rightType == compiler.HashType)
				{
					if (leftType != rightType)
						throw new CompileTimeException(leftType == compiler.HashType ? Right : Left,
							"If one operand of '::' is a hash, the other must be as well.");
				}
			}

			return this;
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

			// null     ?? Right => Right
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

			// null     ?! Right => null
			// non-null ?! Right => Right
			if (Left.IsNull)
				return new ConstantExpression(ConstantValue.Null).At(this);
			else if (Left is ConstantExpression)
				// At this point, Left is a constant non-null expression.
				// If it were a constant null expression, we would have returned above.
				return Right;
			return this;
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
					return new ConstantExpression(constLeft.ExecuteOperator(BinaryOperator.Or, constRight)).At(this);
				}
				if (constLeft.IsTrue)
					return new ConstantExpression(ConstantValue.True).At(this);
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
				//   1    NE   1
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
				//   1    NE   0
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
					return new ConstantExpression(constLeft.ExecuteOperator(BinaryOperator.Xor, constRight)).At(this);
				}

				// false xor false => false
				// false xor true  => true
				// true  xor false => true
				// true  xor true  => false
				// In other words:
				// false xor Right => not not Right
				// true  xor Right => not Right

				// The "not" nodes will be reduced to simpler code in the bytecode.

				var output = new UnaryExpression(Right, UnaryOperator.Not).At(this);
				if (constLeft.IsTrue)
					output = new UnaryExpression(output, UnaryOperator.Not).At(this);
				return output;
			}

			return this;
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			Left.Compile(compiler, method);

			var elseLabel = new Label();
			var trueLabel = new Label();
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
				Right.CompileBoolean(compiler, elseLabel, false, method); // if (right)
				method.Append(Branch.IfTrue(falseLabel)); // if (left) goto falseLabel;
				method.Append(Branch.Always(trueLabel)); // jump past else

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
				Right.CompileBoolean(compiler, elseLabel, false, method); // if (right)
				method.Append(Branch.IfFalse(falseLabel)); // if (!b) goto falseLabel;
				method.Append(Branch.Always(trueLabel)); // jump past else

				method.Append(elseLabel); // else
				method.Append(Branch.IfTrue(falseLabel)); // if (b) goto falseLabel;
				// fall through for true
			}
			method.Append(trueLabel);
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
					return new ConstantExpression(constLeft.ExecuteOperator(BinaryOperator.And, constRight)).At(this);
				}
				if (!constLeft.IsTrue)
					return new ConstantExpression(ConstantValue.False).At(this);
			}

			return this;
		}

		public override void CompileBoolean(Compiler compiler, Label falseLabel, bool negated, MethodBuilder method)
		{
			if (!negated)
			{
				//   A    B    A and B
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
			FoldOperands();

			if (Left is ConstantExpression && Right is ConstantExpression)
			{
				var constLeft = ((ConstantExpression)Left).Value;
				var constRight = ((ConstantExpression)Right).Value;

				var result = constLeft.ExecuteOperator(BinaryOperator.ReferenceEquality, constRight);
				if (Negated)
					result = result.ExecuteOperator(UnaryOperator.Not);

				return new ConstantExpression(result).At(this);
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

		private Compiler compiler; // For FoldConstant

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
			Expression = Expression.FoldConstant();

			if (Expression is ConstantExpression)
			{
				var constValue = ((ConstantExpression)Expression).Value;

				bool result;
				if (Type == null)
					result = constValue.Type == ConstantValueType.Null;
				else
					result = constValue.GetTypeObject(compiler).InheritsFrom(Type.Type);

				return new ConstantExpression(ConstantValue.CreateBoolean(result)).At(this);
			}

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			compiler = document.Compiler;

			Expression = Expression.ResolveNames(context, document, false, false);
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
		LessThan, LessEqual, GreaterThan, GreaterEqual,
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
			Condition = Condition.ResolveNames(context, document, false, false);
			TruePart = TruePart.ResolveNames(context, document, false, false);
			FalsePart = FalsePart.ResolveNames(context, document, false, false);
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

	public sealed class SimpleNameExpression : AssignableExpression
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
			throw new InvalidOperationException(NotResolved);
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document, ExpressionAccessKind access)
		{
			bool hasInstance;
			var containingClass = context.GetContainingClass(out hasInstance);

			var member = context.ResolveName(Name, containingClass);
			if (member == null)
				// It might be a global member, let's try that
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
						inner.ResolveNames(context, document, false, false);

						result = new InstanceMemberAccess(inner, field.Parent, field);
					}
					break;
				case MemberKind.Constant:
					result = new ClassConstantAccess((ClassConstant)member);
					break;
				case MemberKind.EnumField:
					{
						var field = (EnumField)member;
						result = new EnumFieldAccess(field, context.IsInEnumBody(field.Parent));
					}
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
								throw new InstanceMemberAccessException(this, method,
									"Cannot refer to instance members in a field initializer.");
							throw new InstanceMemberAccessException(this, method);
						}
						if (access.IsWrite())
							throw new CompileTimeException(this, string.Format("Cannot assign to the method '{0}'.", method.FullName));

						result = new InstanceMemberAccess(
							new ThisAccess().At(this).ResolveNames(context, document, false, false),
							(Class)method.Parent, method
						);
					}
					break;
				case MemberKind.Property:
					{
						var prop = ((Property)member);
						if (access.IsRead())
							prop.EnsureReadable(this);
						if (access.IsWrite())
							prop.EnsureWritable(this);

						if (prop.IsStatic)
							return new StaticPropertyAccess(prop);
						if (!hasInstance)
						{
							if (context.IsInFieldInitializer())
								throw new InstanceMemberAccessException(this, prop, "Cannot refer to instance members in a field initializer.");
							throw new InstanceMemberAccessException(this, prop);
						}

						result = new InstanceMemberAccess(
							new ThisAccess().At(this).ResolveNames(context, document, false, false),
							prop.Parent, prop
						);
					}
					break;
				case MemberKind.Variable:
					{
						// Note: local variables only occur within blocks, so we don't actually
						// need to test whether 'block' is null.
						
						var variable = (Variable)member;
						variable.EnsureAccessibleFrom(this);
						if (access.IsWrite())
							variable.EnsureAssignable(this);

						// If the variable comes from another method, then we must capture it,
						// but only if the current method is a local method. The actual trans-
						// formation takes place elsewhere.
						var localMethod = variable.Parent.ContainingMember != block.ContainingMember ?
							block.ContainingMember as LocalMethod : null;
						if (localMethod != null)
							localMethod.Function.Capture(variable, this);

						result = new LocalVariableAccess(variable, block, access);
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

						if (block.ContainingMember is LocalMethod)
						{
							var currentFunction = ((LocalMethod)block.ContainingMember).Function;
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
			throw new InvalidOperationException(NotResolved);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException(NotResolved);
		}

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value)
		{
			throw new InvalidOperationException(NotResolved);
		}

		public override void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op)
		{
			throw new InvalidOperationException(NotResolved);
		}

		public override LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException(NotResolved);
		}

		public override void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			throw new InvalidOperationException(NotResolved);
		}

		public override void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			throw new InvalidOperationException(NotResolved);
		}

		private const string NotResolved = "SimpleNameExpression found after name resolution.";
	}

	/// <summary>
	/// Represents a qualified name.
	/// It basically contains a list of dot-separated strings.
	/// </summary>
	public class QualifiedName : ParseNode
	{
		public QualifiedName(string value)
		{
			Parts = value.Split(Compiler.Dot);
		}
		public QualifiedName(string[] parts)
		{
			Parts = parts.ToArray();
		}

		/// <summary>The strings that the qualified name consists of.</summary>
		public string[] Parts;

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
		public TypeName(string[] parts, bool global)
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
		public LambdaExpression(Parameter[] parameters, Splat splat, Statement body)
		{
			Parameters = parameters;
			Splat = splat;
			Body = body;
		}

		/// <summary>The parameters of the lambda.</summary>
		public Parameter[] Parameters;

		/// <summary>The location of the splat, if any.</summary>
		public Splat Splat;

		/// <summary>The body of the lambda expression.
		/// This can be either a <see cref="Block"/> or an <see cref="ExpressionStatement"/>.</summary>
		public Statement Body;

		internal string NameHint;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder("@");

			if (Parameters.Length == 1 && Splat == Splat.None)
			{
				sb.Append(Parameters[0]);
				sb.Append(' ');
			}
			else if (Parameters.Length > 0)
			{
				sb.Append("(");

				sb.Append(Parameters.JoinString(", ", indent));

				if (Splat == Splat.End)
					sb.Append("...) ");
				else
					sb.Append(") ");
			}

			if (Body is ExpressionStatement)
				sb.AppendFormat("=> {0}", ((ExpressionStatement)Body).Expression.ToString(indent));
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
			// This is null when the local function is a lambda expression
			// in a field initializer
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

			if (block != null && block.ContainingMember is Method)
				block.DeclareLocalFunction(func);
			else
				document.Compiler.AddMethodWithLocalFunctions(func.Method);

			funcDecl.Body.DeclareNames(context as BlockSpace);
			funcDecl.ResolveNames(context, document, true);

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

			var returnStmt = new ReturnStatement(returnValue);

			var bodyBlock = new Block(returnStmt);

			var funcDecl = new LocalFunctionDeclaration(funcName, new[] { paramDecl }, Splat.None, bodyBlock)
			{
				StartIndex = this.StartIndex,
				EndIndex = this.EndIndex,
			};
			var func = new LocalFunction(funcName, funcDecl, block ?? context);

			if (block != null && block.ContainingMember is Method)
				block.DeclareLocalFunction(func);
			else
				document.Compiler.AddMethodWithLocalFunctions(func.Method);

			funcDecl.ResolveNames(context, document, true);

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

	public sealed class UseInExpression : Expression
	{
		public UseInExpression(VariableDeclarator[] variables, Expression inner)
		{
			this.Variables = variables;
			this.Inner = inner;
		}

		/// <summary>The variables declared for use in this expression.</summary>
		private VariableDeclarator[] Variables;
		/// <summary>The expression following the 'in' keyword; the result expression.</summary>
		private Expression Inner;

		// The block that contains the variable declarations
		private Block VariableBlock;

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
			var sb = new StringBuilder("use ");

			var needSep = false;
			foreach (var decl in Variables)
			{
				if (needSep)
					sb.Append(", ");
				else
					needSep = true;
				sb.Append(decl.ToString(indent + 1));
			}

			sb.Append(" in ");
			sb.Append(Inner.ToString(indent + 1));
			return sb.ToString();
		}

		public override Expression FoldConstant()
		{
			foreach (var decl in Variables)
				decl.FoldConstant(false);
			Inner = Inner.FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			// Note: Expression has no DeclareNames method, so we declare the names here instead.
			// The context is guaranteed to be a BlockSpace (inside a member with a body or another
			// use-in expression) or Class (inside a field initializer).
			var parentBlock = context as BlockSpace;

			var vars = Variables;
			VariableBlock = new Block(
				new LocalVariableDeclaration(false, vars)
				{
					StartIndex = vars[0].StartIndex,
					EndIndex = vars[vars.Length - 1].EndIndex,
					Document = this.Document
				}
			);
			if (parentBlock == null) // top-level use-in in field initializer
			{
				bool _;
				VariableBlock.DeclSpace = new BlockSpace(VariableBlock, context.GetContainingClass(out _));
			}
			VariableBlock.DeclareNames(parentBlock);

			foreach (var decl in vars)
				decl.ResolveNames(VariableBlock.DeclSpace, document);
			Inner = Inner.ResolveNames(VariableBlock.DeclSpace, document);

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			var varBlock = VariableBlock.DeclSpace;
			foreach (var decl in Variables)
				decl.Initializer = decl.Initializer.TransformClosureLocals(varBlock, forGenerator);
			Inner = Inner.TransformClosureLocals(varBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			// Compile the implicit block first. This assigns all the variables,
			// initializes closure classes, the whole shebang. It even leaves
			// the stack empty for us.
			VariableBlock.Compile(compiler, method);

			// And now the resulting expression
			Inner.Compile(compiler, method);
		}
	}

	public enum LambdaOperator
	{
		Plus, Minus, BitwiseOr, BitwiseXor, Multiplication, Division,
		Modulo, BitwiseAnd, Exponentiation, ShiftLeft, ShiftRight,
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

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document, ExpressionAccessKind access)
		{
			Inner = Inner.ResolveNames(context, document, true, true);
			if (Inner is NamespaceAccess)
			{
				return GetNamespaceMemberAccess(((NamespaceAccess)Inner).Namespace);
			}
			if (Inner is TypeAccess)
			{
				bool _;
				return GetTypeMemberAccess(context,
					type: ((TypeAccess)Inner).Type,
					fromType: context.GetContainingClass(out _),
					access: access);
			}
			if (Inner.IsTypeKnown(document.Compiler))
			{
				bool _;
				var fromType = context.GetContainingClass(out _);

				bool throughBase = false;
				Class instType;
				if (Inner is ThisAccess)
					instType = fromType;
				else if (throughBase = (Inner is BaseAccess))
					instType = fromType.BaseType as Class;
				else
				{
					var type = Inner.GetKnownType(document.Compiler);
					if (type == null)
						throw new CompileTimeException(Inner, "Cannot access any members on the null value.");

					instType = (Class)type;
				}

				NamedMember inaccessibleMember;
				var member = instType.GetMember(Member,
					instType: instType,
					fromType: fromType,
					inaccessibleMember: out inaccessibleMember);
				if (inaccessibleMember != null)
					throw new CompileTimeException(this, string.Format("The member '{0}' is not accessible in this context.",
						inaccessibleMember.FullName));

				if (member == null)
					throw new UndefinedNameException(this, Member,
						string.Format("The type '{0}' does not contain a definition for '{1}'.",
							instType.FullName, Member));

				if (member is ClassMember)
				{
					var classMember = (ClassMember)member;
					if (classMember.IsStatic)
						throw new StaticMemberAccessException(this, member);
					if (classMember.Kind == MemberKind.Property)
					{
						var prop = (Property)classMember;
						if (access.IsRead())
							prop.EnsureReadable(this);
						if (access.IsWrite())
							prop.EnsureWritable(this);
						// Abstract property through base is checked below
					}
				}
				else if (member.Kind == MemberKind.MethodGroup)
				{
					var methodGroup = (MethodGroup)member;
					if (methodGroup.IsStatic)
						throw new StaticMemberAccessException(this, member);
					if (throughBase && !IsInvocation && methodGroup.Any(o => o.IsAbstract))
						throw new CompileTimeException(this,
							string.Format("The method '{0}' cannot be accessed as a value through 'base', only invoked, because it contains one or more abstract overloads.",
								methodGroup.FullName));
				}

				if (throughBase)
					if (member.Kind == MemberKind.Property && ((Property)member).IsAbstract)
						throw new CompileTimeException(this,
							string.Format("The property '{0}' is abstract and cannot be accessed through 'base'.",
								member.FullName));
					else if (!IsInvocation && member.Kind == MemberKind.MethodGroup &&
						((MethodGroup)member).Any(o => o.IsAbstract))
						throw new CompileTimeException(this,
							string.Format("The method '{0}' cannot be accessed through 'base' as a value because it contains one or more abstract overloads.",
								member.FullName));

				Class declType; // The declaring type of the member
				if (member.Kind == MemberKind.MethodGroup)
					declType = ((MethodGroup)member).ParentAsClass;
				else
					declType = ((ClassMember)member).Parent;

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

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value)
		{
			Inner.Compile(compiler, method); // Evaluate the instance

			value.Compile(compiler, method); // Evaluate the value

			method.Append(new StoreMember(method.Module.GetStringId(Member))); // Store the value in the member
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

		private Expression GetNamespaceMemberAccess(Namespace ns)
		{
			if (ns.ContainsMember(Member))
			{
				var member = ns.GetMember(Member);

				Expression result;
				switch (member.Kind)
				{
					case MemberKind.Namespace:
						result = new NamespaceAccess((Namespace)member);
						break;
					case MemberKind.Class:
					case MemberKind.Enum:
						result = new TypeAccess((Type)member);
						break;
					case MemberKind.MethodGroup:
						result = new StaticMethodAccess((MethodGroup)member);
						break;
					case MemberKind.GlobalConstant:
						result = new GlobalConstantAccess((GlobalConstant)member);
						break;
					case MemberKind.Ambiguous:
						throw new AmbiguousNameException(this, (AmbiguousMember)member,
							string.Format("The name '{0}' is ambiguous between the following members: {1}",
								member.Name, ((AmbiguousMember)member).GetMemberNamesJoined()));
					default:
						throw new InvalidOperationException("Namespace member is of an invalid kind.");
				}

				return result.At(this);
			}

			throw new UndefinedNameException(this, Member,
				string.Format("The namespace '{0}' does not contain a definition for '{1}'.",
					ns.FullName, Member));
		}

		private Expression GetTypeMemberAccess(IDeclarationSpace context, Type type, Type fromType, ExpressionAccessKind access)
		{
			var member = type.GetMember(Member,
				instType: null,
				fromType: fromType);
			if (member != null)
			{
				Expression result;
				switch (member.Kind)
				{
					case MemberKind.Field:
						{
							var field = (Field)member;
							if (!field.IsStatic)
								throw new InstanceMemberAccessException(this, member);
							result = new StaticFieldAccess(field);
						}
						break;
					case MemberKind.Property:
						{
							var prop = (Property)member;
							if (!prop.IsStatic)
								throw new InstanceMemberAccessException(this, member);
							if (access.IsRead())
								prop.EnsureReadable(this);
							if (access.IsWrite())
								prop.EnsureWritable(this);
							result = new StaticPropertyAccess(prop);
						}
						break;
					case MemberKind.MethodGroup:
						{
							var method = (MethodGroup)member;
							if (!method.IsStatic)
								throw new InstanceMemberAccessException(this, member);
							result = new StaticMethodAccess(method);
						}
						break;
					case MemberKind.Constant:
						result = new ClassConstantAccess((ClassConstant)member);
						break;
					case MemberKind.EnumField:
						{
							var field = (EnumField)member;
							result = new EnumFieldAccess(field, context.IsInEnumBody(field.Parent));
						}
						break;
					default:
						throw new InvalidOperationException("Member of type is of an invalid kind.");
				}

				return result.At(this);
			}

			throw new UndefinedNameException(this, Member,
				string.Format("The type '{0}' does not contain a definition for '{1}'.",
				type.FullName, Member));
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
			var localMethod = block != null ? block.ContainingMember as LocalMethod : null;
			if (block != null && localMethod != null)
				localMethod.Function.CapturesThis = true;

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
				var localMethod = currentBlock.Method as LocalMethod;
				if (localMethod != null)
				{
					var localFunc = ((LocalMethod)localMethod).Function;
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
			throw new InvalidOperationException("Don't call GetKnownType on BaseAccess. Compile specially instead.");
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
			{
				if (context.IsInFieldInitializer())
					throw new CompileTimeException(this, "Cannot refer to 'base' in a field initializer.");
				throw new CompileTimeException(this, "'base' can only be used in an instance method.");
			}
			if (@class.BaseType == null)
				throw new CompileTimeException(this, "'base' cannot be used inside aves.Object.");

			var block = context as BlockSpace;
			var localMethod = block != null ? block.Method as LocalMethod : null;
			if (block != null && localMethod != null)
				localMethod.Function.CapturesThis = true;

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
				var localMethod = currentBlock.Method as LocalMethod;
				if (localMethod != null)
				{
					var localFunc = localMethod.Function;
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
		public IndexerAccess(Expression inner, Expression[] arguments)
		{
			Inner = inner;
			Arguments = arguments;
		}

		/// <summary>The expression whose indexer is being accessed.</summary>
		public Expression Inner;
		/// <summary>The arguments for the indexer.</summary>
		public Expression[] Arguments;

		// This field is only set if the indexer is being accessed non-virtually
		// through a known type.
		private Indexer indexer;

		public override string ToString(int indent)
		{
			return String.Format("{0}[{1}]", Inner.ToString(indent), Arguments.JoinString(", ", indent));
		}

		public override Expression FoldConstant()
		{
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document, ExpressionAccessKind access)
		{
			Inner = Inner.ResolveNames(context, document, false, false);

			if (Inner.IsTypeKnown(document.Compiler))
			{
				bool _;
				var fromType = context.GetContainingClass(out _);

				bool throughBase = false;
				Class instType;
				if (Inner is ThisAccess)
					instType = fromType;
				else if (throughBase = (Inner is BaseAccess))
					instType = fromType.BaseType as Class;
				else
				{
					var type = Inner.GetKnownType(document.Compiler);
					if (type == null)
						throw new CompileTimeException(Inner, "Cannot index into the null value.");
					if (type is Enum)
						throw new CompileTimeException(Inner, "Enum values cannot be indexed into.");

					instType = (Class)type;
				}

				NamedMember inaccessibleMember;
				var indexerMember = instType.GetMember(Indexer.MemberName,
					instType: instType,
					fromType: fromType,
					inaccessibleMember: out inaccessibleMember);
				if (inaccessibleMember != null)
					throw new CompileTimeException(Inner, string.Format("The member '{0}' is not accessible in this context.",
						inaccessibleMember.FullName));

				var argCount = Arguments.Length;
				var indexer = indexerMember.Kind == MemberKind.IndexerMember ?
					((IndexerMember)indexerMember).GetIndexer(argCount) :
					null;
				if (indexer == null)
					throw new CompileTimeException(Inner, string.Format("The type '{0}' does not define an indexer that takes {1} arguments.",
						instType.FullName, argCount.ToStringInvariant()));

				if (access.IsRead())
					indexer.EnsureReadable(this);
				if (access.IsWrite())
					indexer.EnsureWritable(this);

				// Cannot index into an abstract indexer with base[...]
				if (throughBase && indexer.IsAbstract)
					throw new CompileTimeException(Inner,
						string.Format("The {0}-argument indexer of class '{2}' is abstract and cannot be accessed through 'base'.",
							argCount, indexer.Parent.FullName));

				if (throughBase || (!indexer.IsOverridable && !indexer.IsAbstract))
					this.indexer = indexer;
			}

			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document, false, false);

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Inner = Inner.TransformClosureLocals(currentBlock, forGenerator);
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (IsAssignment)
				throw new InvalidOperationException("IndexerAccess assignment not compiled specially by AssignmentExpression.");

			Inner.Compile(compiler, method);
			foreach (var arg in Arguments)
				arg.Compile(compiler, method);

			if (indexer != null)
			{
				var getterMethod = indexer.IndexerGetter.Method;
				method.Append(new StaticCall(method.Module.GetMethodId(getterMethod.Group), Arguments.Length));
			}
			else
				method.Append(new LoadIndexer(Arguments.Length));
		}

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value)
		{
			Inner.Compile(compiler, method);

			foreach (var arg in Arguments)
				arg.Compile(compiler, method);

			value.Compile(compiler, method);

			AppendStoreInstruction(method);
		}

		public override void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op)
		{
			LocalVariable instLocal = null;
			var argLocals = new LocalVariable[Arguments.Length];

			Inner.Compile(compiler, method); // Evaluate the instance
			if (!Inner.CanSafelyInline)
			{
				instLocal = method.GetAnonymousLocal();
				method.Append(new SimpleInstruction(Opcode.Dup)); // Duplicate it
				method.Append(new StoreLocal(instLocal)); // Store it temporarily
			}

			for (var i = 0; i < Arguments.Length; i++)
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

			method.Append(new LoadIndexer(Arguments.Length)); // inner[args...]

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

			for (var i = 0; i < Arguments.Length; i++)
				if (argLocals[i] == null)
					Arguments[i].Compile(compiler, method);
				else
				{
					method.Append(new LoadLocal(argLocals[i]));
					argLocals[i].Done();
				}

			method.Append(new LoadLocal(finalValueLocal));
			AppendStoreInstruction(method);

			finalValueLocal.Done();
		}

		public override LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method)
		{
			var locals = new TempList<LocalVariable>();

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
			AppendStoreInstruction(method);
		}

		private void AppendStoreInstruction(MethodBuilder method)
		{
			if (indexer != null)
			{
				var setterMethod = indexer.IndexerSetter.Method;
				method.Append(new StaticCall(method.Module.GetMethodId(setterMethod.Group), Arguments.Length + 1));
				method.Append(new SimpleInstruction(Opcode.Pop));
			}
			else
				method.Append(new StoreIndexer(Arguments.Length));
		}
	}

	public sealed class ObjectCreationExpression : Expression
	{
		public ObjectCreationExpression(TypeName type, Expression[] arguments)
			: this(type, arguments, arguments.HasRefArguments())
		{ }
		public ObjectCreationExpression(TypeName type, Expression[] arguments, bool hasRefArgs)
		{
			Type = type;
			Arguments = arguments;
			HasRefArgs = hasRefArgs;
		}

		/// <summary>The type that is being created.</summary>
		public TypeName Type;

		/// <summary>The arguments passed to the constructor.</summary>
		public Expression[] Arguments;

		public bool HasRefArgs;

		/// <summary>The object initializer, or null if it is absent.</summary>
		public ObjectInitializer Initializer;

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
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].FoldConstant();

			if (Initializer != null)
				Initializer.FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			var type = context.GetContainingNamespace().ResolveTypeName(Type, document);
			var @class = type as Class;
			if (@class != null && (@class.IsAbstract || @class.IsStatic))
				throw new CompileTimeException(this,
					string.Format("Cannot construct an instance of '{0}': the type is abstract or static.",
						@class.FullName));

			bool _;
			Constructor = type.FindConstructor(Type, Arguments.Length,
				instClass: @class, fromClass: context.GetContainingClass(out _));

			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document, false, false);

			if (HasRefArgs || Constructor.HasRefParams)
				Constructor.VerifyArgumentRefness(Arguments);

			if (Initializer != null)
				Initializer.ResolveNames(context, document, @class);

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].TransformClosureLocals(currentBlock, forGenerator);

			if (Initializer != null)
				Initializer.TransformClosureLocals(currentBlock, forGenerator);

			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			foreach (var arg in Arguments)
				arg.Compile(compiler, method);

			var type = Type != null ? Type.Type : Constructor.Group.ParentAsClass;
			method.Append(new NewObject(method.Module.GetTypeId(type), Arguments.Length));

			if (Initializer != null)
				Initializer.Compile(compiler, method);
		}
	}

	public sealed class ObjectInitializer : ParseNode
	{
		public ObjectInitializer(MemberInitializer[] members)
		{
			Members = members;
		}

		public MemberInitializer[] Members;

		public override string ToString(int indent)
		{
			if (Members.Length == 0)
				return "with { }";

			return "with { " + Members.JoinString(", ", indent + 1) + " }";
		}

		public void FoldConstant()
		{
			foreach (var init in Members)
				init.FoldConstant();
		}

		public void ResolveNames(IDeclarationSpace context, FileNamespace document, Class objectType)
		{
			foreach (var init in Members)
				init.ResolveNames(context, document, objectType);
		}

		public void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			foreach (var init in Members)
				init.TransformClosureLocals(currentBlock, forGenerator);
		}

		public void Compile(Compiler compiler, MethodBuilder method)
		{
			// Instance is on the stack
			var instLoc = method.GetAnonymousLocal();

			method.Append(new StoreLocal(instLoc)); // Store the instance in the local

			foreach (var init in Members)
			{
				method.Append(new LoadLocal(instLoc)); // Load instance
				init.Compile(compiler, method); // Compile initializer
			}

			method.Append(new LoadLocal(instLoc)); // Load instance as result
			instLoc.Done();
		}
	}

	public sealed class MemberInitializer : ParseNode
	{
		public MemberInitializer(string name, Expression value)
		{
			Name = name;
			Value = value;
		}

		/// <summary>The name of the member that is being initialized.</summary>
		private string Name;

		/// <summary>The value that the member is initialized to.</summary>
		private Expression Value;

		private ClassMember Member;

		public override string ToString(int indent)
		{
			return Name + ": " + Value.ToString(indent + 1);
		}

		public void FoldConstant()
		{
			Value = Value.FoldConstant();
		}

		public void ResolveNames(IDeclarationSpace context, FileNamespace document, Class objectType)
		{
			Value = Value.ResolveNames(context, document, false, false);

			bool _;
			NamedMember inaccessibleMember;
			var namedMember = objectType.GetMember(Name,
				instType: objectType,
				fromType: context.GetContainingClass(out _),
				inaccessibleMember: out inaccessibleMember);

			if (namedMember == null)
			{
				if (inaccessibleMember != null)
					throw new CompileTimeException(this,
						string.Format("The member '{0}.{1}' is not accessible from this context.",
							objectType.FullName, Name));

				throw new UndefinedNameException(this, Name,
					string.Format("The type '{0}' does not contain a definition for '{1}'.",
						objectType.FullName, Name));
			}

			var member = namedMember as ClassMember;

			// The member must be an instance member, and it must be either a field or a writable property.
			// The null test is because MethodGroup does not inherit from ClassMember.
			if (member == null || member.IsStatic ||
				member.Kind != MemberKind.Field &&
				(member.Kind != MemberKind.Property || ((Property)member).Setter == null))
				throw new CompileTimeException(this,
					"The member name must refer to an instance field or writable instance property.");

			Member = member;
		}

		public void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Value = Value.TransformClosureLocals(currentBlock, forGenerator);
		}

		public void Compile(Compiler compiler, MethodBuilder method)
		{
			// Instance is on the stack

			Value.Compile(compiler, method); // First evaluate the expression

			if (Member.Kind == MemberKind.Field)
				// Store value in field
				method.Append(StoreField.Create(method.Module, (Field)Member));
			else
			{
				// Must be a writable instance property, so call the property setter
				var setter = ((Property)Member).Setter;
				method.Append(new StaticCall(method.Module.GetMethodId(setter.Method.Group), 1));
				method.Append(new SimpleInstruction(Opcode.Pop));
			}
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

			var isHex = tokVal.StartsWith("0x", StringComparison.InvariantCultureIgnoreCase);
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
						value = checked((ulong)multiplier * ulong.Parse(tokVal, NumberStyles.AllowHexSpecifier, CI.InvariantCulture));
					else
						value = checked((ulong)multiplier * ulong.Parse(tokVal, CI.InvariantCulture));
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
						value = checked(multiplier * long.Parse(tokVal, NumberStyles.AllowHexSpecifier, CI.InvariantCulture));
					else
						value = checked(multiplier * long.Parse(tokVal, CI.InvariantCulture));
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

		internal static ConstantValue ParseToken(Token token)
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
			: base(token.Value, ConstantValue.CreateString(token.LiteralValue))
		{ }

		public string StringValue { get { return Value.StringValue; } }
	}

	public sealed class CharacterLiteral : Literal
	{
		public CharacterLiteral(CharToken token)
			: base(token.Value, ConstantValue.CreateChar(token.Codepoint))
		{ }

		public int Codepoint { get { return Value.CharValue; } }
	}

	#endregion

	public sealed class InvocationExpression : Expression
	{
		public InvocationExpression(Expression inner, Expression[] args)
			: this(inner, args, args.HasRefArguments())
		{ }
		public InvocationExpression(Expression inner, Expression[] args, bool hasRefArgs)
		{
			Inner = inner;
			Arguments = args;
			HasRefArgs = hasRefArgs;
		}

		/// <summary>The expression that is being invoked.</summary>
		public Expression Inner;

		/// <summary>The argument passed to the function.</summary>
		public Expression[] Arguments;

		/// <summary>Whether the invocation uses ref arguments.</summary>
		public bool HasRefArgs;

		public override string ToString(int indent)
		{
			return String.Format("{0}({1})", Inner.ToString(indent), Arguments.JoinString(", ", indent));
		}

		public override Expression FoldConstant()
		{
			Inner = Inner.FoldConstant();

			for (var i = 0; i < Arguments.Length; i++)
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
					var overload = method.FindOverload(Arguments.Length, true);
					if (overload == null)
						throw new CompileTimeException(Inner,
							string.Format("The method '{0}.{1}' does not contain an overload that takes {2} arguments.",
								method.ParentAsClass.FullName, method.Name, Arguments.Length));
					if (access.Inner is BaseAccess && overload.IsAbstract)
						throw new CompileTimeException(Inner,
							string.Format("The method '{0}.{1}' is abstract and cannot be called through 'base'.",
								method.ParentAsClass.FullName, method.Name));
					if (HasRefArgs || overload.HasRefParams)
						overload.VerifyArgumentRefness(Arguments);
				}
			}
			else if (Inner is StaticMethodAccess)
			{
				var access = (StaticMethodAccess)Inner;
				var overload = access.Method.FindOverload(Arguments.Length, true);
				if (overload == null)
					throw new CompileTimeException(Inner, string.Format("The method '{0}' does not take {1} arguments.",
						access.Method.FullName, Arguments.Length));
				if (HasRefArgs || overload.HasRefParams)
					overload.VerifyArgumentRefness(Arguments);
			}
			else if (Inner is LocalFunctionAccess)
			{
				var access = (LocalFunctionAccess)Inner;
				var method = access.Function.Method;
				if (!method.Accepts(Arguments.Length))
					throw new CompileTimeException(Inner, string.Format("The local function '{0}' does not take {1} arguments.",
						access.Function.Name, Arguments.Length));
				if (HasRefArgs || method.HasRefParams)
					method.VerifyArgumentRefness(Arguments);
			}
			else if (Inner.IsTypeKnown(document.Compiler))
			{
				bool _;
				var fromType = context.GetContainingClass(out _);

				Class instType;
				if (Inner is ThisAccess)
					instType = fromType;
				else if (Inner is BaseAccess)
					instType = fromType.BaseType as Class;
				else
				{
					var type = Inner.GetKnownType(document.Compiler);
					if (type == null)
						throw new CompileTimeException(Inner, "Cannot invoke the null value.");
					if (type is Enum)
						throw new CompileTimeException(Inner, "Enum values cannot be invoked.");

					instType = (Class)type;
				}

				NamedMember inaccessibleMember;
				var invocators = instType.GetMember(Class.InvocatorName,
					instType: instType,
					fromType: fromType,
					inaccessibleMember: out inaccessibleMember);
				if (inaccessibleMember != null)
					throw new CompileTimeException(Inner, string.Format("The member '{0}' is not accessible in this context.",
						inaccessibleMember.FullName));
				if (invocators == null || invocators.Kind != MemberKind.MethodGroup)
					throw new CompileTimeException(Inner, string.Format("The class '{0}' does not define an invocator.",
						instType.FullName));

				var invocator = ((MethodGroup)invocators).FindOverload(Arguments.Length, true);
				if (invocator == null)
					throw new CompileTimeException(Inner, string.Format("The class '{0}' does not define an invocator that takes {1} arguments.",
						instType.FullName, Arguments.Length));

				if (HasRefArgs || invocator.HasRefParams)
					invocator.VerifyArgumentRefness(Arguments);

				Inner = new InstanceMemberAccess(Inner, ((MethodGroup)invocators).ParentAsClass, invocators);
			}

			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document, false, false);

			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Inner = Inner.TransformClosureLocals(currentBlock, forGenerator);

			for (var i = 0; i < Arguments.Length; i++)
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
					var overload = methodGroup.FindOverload(Arguments.Length, true);
					if (!overload.IsVirtualCall || access.IsBaseAccess)
					{
						access.Inner.Compile(compiler, method); // Evaluate the instance
						CompileArguments(compiler, method); // Evaluate each argument
						method.Append(new StaticCall(method.Module.GetMethodId(methodGroup), Arguments.Length)); // Call it!
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
				method.Append(new StaticCall(method.Module.GetMethodId(methodGroup), Arguments.Length)); // Call it!
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
			method.Append(new Call(Arguments.Length)); // !ti llaC
		}

		private void CompileAsMemberCall(Compiler compiler, MethodBuilder method, Expression instance, string memberName)
		{
			instance.Compile(compiler, method); // Evaluate the instance expression
			CompileArguments(compiler, method); // Evaluate each argument
			method.Append(new CallMember(method.Module.GetStringId(memberName), Arguments.Length)); // Call the member!
			return;
		}

		private void CompileArguments(Compiler compiler, MethodBuilder method)
		{
			foreach (var arg in Arguments)
				arg.Compile(compiler, method);
		}
	}

	public sealed class ListLiteralExpression : Expression, ILocalResultExpression
	{
		public ListLiteralExpression(Expression[] values)
		{
			Values = values;
		}

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.ListType;
		}

		/// <summary>The values contained in the list.</summary>
		public Expression[] Values;

		private LocalVariable target;

		public override string ToString(int indent)
		{
			return "[" + Values.JoinString(", ", indent) + "]";
		}

		public override Expression FoldConstant()
		{
			for (var i = 0; i < Values.Length; i++)
				Values[i] = Values[i].FoldConstant();

			return this;
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			for (var i = 0; i < Values.Length; i++)
				Values[i] = Values[i].ResolveNames(context, document, false, false);
			return this;
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Values.Length; i++)
				Values[i] = Values[i].TransformClosureLocals(currentBlock, forGenerator);
			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Values.Length == 0)
			{
				method.Append(new CreateList(0));
				if (target != null)
					method.Append(new StoreLocal(target));
			}
			else
			{
				// Create the list
				var listLocal = target ?? method.GetAnonymousLocal();

				method.Append(new CreateList(unchecked((uint)Values.Length))); // Create the list
				method.Append(new StoreLocal(listLocal)); // Store it in the local

				// Get a MethodGroup for aves.List.add
				var listAdd = (MethodGroup)compiler.ListType.GetMember("add");

				for (var i = 0; i < Values.Length; i++)
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

	public sealed class HashLiteralExpression : Expression, ILocalResultExpression
	{
		public HashLiteralExpression(HashMember[] members)
		{
			Members = members;
		}

		/// <summary>The members of the hash.</summary>
		public HashMember[] Members;

		private LocalVariable target;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.HashType;
		}

		public override string ToString(int indent)
		{
			if (Members.Length == 0)
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
			if (Members.Length == 0)
			{
				method.Append(new CreateHash(0));
				if (target != null)
					method.Append(new StoreLocal(target));
			}
			else
			{
				// Create the hash
				var hashLocal = target ?? method.GetAnonymousLocal();

				method.Append(new CreateHash(unchecked((uint)Members.Length))); // Create the hash
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

			for (var i = 0; i < Chain.Count; )
			{
				var link = Chain[i];
				if (link.IsSafe)
				{
					method.Append(new SimpleInstruction(Opcode.Dup)); // duplicate result of last expression
					method.Append(Branch.IfNull(endLabel)); // branch to end if null
				}

				i += link.Compile(compiler, method, this, i);
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

		public abstract int Compile(Compiler compiler, MethodBuilder method, SafeAccess parent, int index);
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

		public override int Compile(Compiler compiler, MethodBuilder method, SafeAccess parent, int index)
		{
			var memberId = method.Module.GetStringId(Member);
			// If the next SafeNode in the chain is an unsafe-invocation,
			// then we can compile this into a callmem instruction.
			var next = index < parent.Chain.Count - 1 ? parent.Chain[index + 1] : null;
			if (next != null && !next.IsSafe && next is SafeInvocation)
			{
				var nextCall = (SafeInvocation)next;
				foreach (var arg in nextCall.Arguments)
					arg.Compile(compiler, method); // First, load each argument
				// Then call the member
				method.Append(new CallMember(memberId, nextCall.Arguments.Length));
				return 2; // skip this and the next
			}

			method.Append(new LoadMember(memberId));
			return 1; // skip only this
		}
	}

	public sealed class SafeInvocation : SafeNode
	{
		public SafeInvocation(Expression[] args, bool isSafe)
			: base(isSafe)
		{
			Arguments = args;
		}

		/// <summary>The arguments passed into the call.</summary>
		public Expression[] Arguments;

		public override string ToString(int indent)
		{
			return (IsSafe ? "?(" : "(") + Arguments.JoinString(", ", indent + 1) + ")";
		}

		public override void FoldConstant()
		{
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document, false, false);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].TransformClosureLocals(currentBlock, forGenerator);
		}

		public override int Compile(Compiler compiler, MethodBuilder method, SafeAccess parent, int index)
		{
			foreach (var arg in Arguments)
				arg.Compile(compiler, method);
			method.Append(new Call(Arguments.Length));
			return 1;
		}
	}

	public sealed class SafeIndexerAccess : SafeNode
	{
		public SafeIndexerAccess(Expression[] args, bool isSafe)
			: base(isSafe)
		{
			Arguments = args;
		}

		/// <summary>The arguments passed to the indexer.</summary>
		public Expression[] Arguments;

		public override string ToString(int indent)
		{
			return (IsSafe ? "?[" : "[") + Arguments.JoinString(", ", indent + 1) + "]";
		}

		public override void FoldConstant()
		{
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document, false, false);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Arguments.Length; i++)
				Arguments[i] = Arguments[i].TransformClosureLocals(currentBlock, forGenerator);
		}

		public override int Compile(Compiler compiler, MethodBuilder method, SafeAccess parent, int index)
		{
			foreach (var arg in Arguments)
				arg.Compile(compiler, method);
			method.Append(new LoadIndexer(Arguments.Length));
			return 1;
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

		public override int Compile(Compiler compiler, MethodBuilder method, SafeAccess parent, int index)
		{
			method.Append(new SimpleInstruction(Opcode.Lditer));
			return 1;
		}
	}

	#endregion
}