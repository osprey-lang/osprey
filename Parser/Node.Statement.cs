using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.Instructions;
using Type = Osprey.Members.Type;

namespace Osprey.Nodes
{
	public abstract class Statement : ParseNode
	{
		/// <summary>
		/// Determines whether the statement ever explicitly returns on any branch of execution.
		/// </summary>
		public virtual bool CanReturn { get { return false; } }
		/// <summary>
		/// Determines whether the statement ever yields a value on any branch of execution.
		/// </summary>
		public virtual bool CanYield { get { return false; } }

		/// <summary>
		/// Determines whether the end of the statement is reachable,
		/// assuming the statement itself is reachable.
		/// </summary>
		/// <remarks>
		/// This property is only examined if the statement itself is reachable.
		/// Hence, statements are free to assume that they are reachable if queried
		/// for the reachability of their end point.
		/// </remarks>
		public virtual bool IsEndReachable { get { return true; } }
		
		public virtual void FoldConstant() { }

		public virtual void ResolveNames(IDeclarationSpace context, FileNamespace document) { }

		public virtual void DeclareNames(BlockSpace parent) { }

		public virtual void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator) { }

		public virtual void Compile(Compiler compiler, MethodBuilder method) { }
	}

	public sealed class EmptyStatement : Statement
	{
		public override string ToString(int indent)
		{
			return new string('\t', indent) + ";";
		}
	}

	public class Block : Statement
	{
		internal Block()
			: this(new List<Statement>())
		{ }
		public Block(List<Statement> statements)
		{
			Statements = statements;
		}

		internal Block(params Statement[] statements)
			: this(statements.ToList())
		{ }

		/// <summary>The statemenets contained within the block.</summary>
		public List<Statement> Statements;

		/// <summary>The declaration space associated with the block.</summary>
		internal BlockSpace DeclSpace;

		private bool? canReturn = null;
		public override bool CanReturn
		{
			get
			{
				if (!canReturn.HasValue)
				{
					canReturn = false;
					foreach (var stmt in Statements)
					{
						if (stmt.CanReturn)
							canReturn = true;
						if (!stmt.IsEndReachable)
							break; // don't examine any other statements
					}
				}
				return canReturn.Value;
			}
		}

		private bool? canYield = null;
		public override bool CanYield
		{
			get
			{
				if (!canYield.HasValue)
				{
					canYield = false;
					foreach (var stmt in Statements)
					{
						if (stmt.CanYield)
							canYield = true;
						if (!stmt.IsEndReachable)
							break; // don't examine any other statements
					}
				}
				return canYield.Value;
			}
		}

		private bool? isEndReachable = null;
		public override bool IsEndReachable
		{
			get
			{
				if (!isEndReachable.HasValue)
				{
					isEndReachable = true;
					foreach (var stmt in Statements)
						if (!stmt.IsEndReachable)
						{
							isEndReachable = false;
							break;
						}
				}
				return isEndReachable.Value;
			}
		}

		internal List<AssignmentExpression> Initializer;

		public override string ToString(int indent)
		{
			if (Statements.Count == 0 && Initializer == null)
				return "{ }";
			var sb = new StringBuilder("{\r\n"); // opening curly on same line

			if (Initializer != null)
			{
				sb.Append('\t', indent + 1);
				sb.AppendLine("init {");
				foreach (var stmt in Initializer)
				{
					sb.Append('\t', indent + 2);
					sb.Append(stmt.ToString(indent + 2));
					sb.AppendLine(";");
				}
				sb.Append('\t', indent + 1);
				sb.AppendLine("}");
			}

			foreach (var stmt in Statements)
				sb.AppendLine(stmt.ToString(indent + 1));

			sb.Append('\t', indent);
			sb.Append('}');
			return sb.ToString();
		}

		public override void FoldConstant()
		{
			foreach (var stmt in Statements)
				stmt.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			foreach (var stmt in Statements)
				stmt.ResolveNames(this.DeclSpace, document);
		}

		public override void DeclareNames(BlockSpace parent)
		{
			if (DeclSpace == null)
				DeclSpace = new BlockSpace(this, parent);

			foreach (var stmt in Statements)
				stmt.DeclareNames(DeclSpace);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			if (forGenerator && CanYield)
			{
				// We need to put all the locals declared here into a field.
				var genClass = DeclSpace.Method.GeneratorClass;
				foreach (var member in DeclSpace.members.Values)
				{
					var variable = member as Variable;
					if (variable != null && (!variable.IsCaptured || variable.IsParameter ||
						variable.VariableKind == VariableKind.IterationVariable))
					{
						// If the variable is not already captured, we need to capture it now,
						// putting it in the generator class. If it IS captured, then it must
						// already be in a closure class. Except if it's a parameter, in which
						// case we still need to declare a field for it, so that the method that
						// creates the generator instance can assign the parameter value to the
						// field inside the generator class.
						genClass.DeclareVariableField(variable);
						variable.Capture();
					}
				}

				if (DeclSpace.ClosureVariable != null)
				{
					var field = genClass.DeclareAnonField(DeclSpace);
					DeclSpace.ClosureVariable.CaptureField = field;
					DeclSpace.ClosureVariable.Capture();

					// Rather than transform the entire initializer, we let it remain mostly
					// the way it already is, including the bit where it initializes the closure
					// class instance into a local variable. Instead, we just copy that value
					// to the correct field afterwards.
					Initializer.Add(new AssignmentExpression(
						new InstanceMemberAccess(new ThisAccess(), genClass, field) { IsAssignment = true },
						new LocalVariableAccess(DeclSpace.ClosureVariable, LocalAccessKind.ClosureLocal)
					) { IgnoreValue = true });
					// This leads to simpler and smaller bytecode.

					// However, we still need to make sure that any captured parameters are
					// actually copied from the correct location (which will be a field in
					// the iterator class now, rather than an actual parameter).
					for (var i = 0; i < Initializer.Count; i++)
					{
						var expr = Initializer[i];
						if (expr.Value is LocalVariableAccess)
						{
							var variable = ((LocalVariableAccess)expr.Value).Variable;
							if (variable.IsParameter || variable.VariableKind == VariableKind.IterationVariable)
								expr.Value = new InstanceMemberAccess(new ThisAccess(), genClass, variable.CaptureField);
						}
					}
				}
			}

			foreach (var stmt in Statements)
			{
				stmt.TransformClosureLocals(DeclSpace, forGenerator);
				if (!stmt.IsEndReachable)
					break;
			}
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (DeclSpace.ClosureVariable != null)
				DeclSpace.ClosureLocal = method.GetAnonymousLocal();

			if (Initializer != null)
				foreach (var expr in Initializer)
					expr.Compile(compiler, method);

			var iMax = Statements.Count - 1;
			for (var i = 0; i <= iMax; i++)
			{
				var stmt = Statements[i];
				stmt.Compile(compiler, method);

				if (!stmt.IsEndReachable)
				{
					if (i < iMax)
						compiler.Warning("Unreachable code detected.");
					break;
				}
			}

			if (DeclSpace.ClosureLocal != null)
				DeclSpace.ClosureLocal.Done();
		}
	}

	public abstract class LocalDeclaration : Statement
	{ }

	// var a = 1, b = 2, c = 3;
	// const x = 24, y = 25, z = 26;
	public sealed class SimpleLocalVariableDeclaration : LocalDeclaration
	{
		public SimpleLocalVariableDeclaration(bool isConst, List<VariableDeclarator> declarators)
		{
			IsConst = isConst;
			Declarators = declarators;
		}

		/// <summary>Indicates whether the declaration is for a set of constant values.</summary>
		public bool IsConst;
		/// <summary>Indicates whether the declaration is for a global variable.</summary>
		public bool IsGlobal;

		/// <summary>The variables declared in this declaration.</summary>
		public List<VariableDeclarator> Declarators;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			sb.Append(IsConst ? "const " : "var ");
			sb.Append(Declarators.JoinString(", ", indent + 1));
			sb.Append(";");
			return sb.ToString();
		}

		public override void FoldConstant()
		{
			foreach (var decl in Declarators)
				if (decl.Value != null)
					decl.FoldConstant(IsConst);
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			foreach (var decl in Declarators)
				decl.ResolveNames(context, document);
		}

		public override void DeclareNames(BlockSpace parent)
		{
			//if (IsGlobal)
				//return;

			foreach (var decl in Declarators)
			{
				if (IsConst)
					parent.DeclareConstant(new LocalConstant(decl.Name, decl));
				else
				{
					if (!IsGlobal)
						decl.Variable = new Variable(decl.Name, decl);
					parent.DeclareVariable(decl.Variable);
				}
			}
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			foreach (var decl in Declarators)
				if (decl.Value != null)
					decl.Value = decl.Value.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (IsConst)
				return; // Don't do anything!

			foreach (var decl in Declarators)
				if (decl.Value != null)
				{
					if (IsGlobal && decl.Variable.IsCaptured)
					{
						decl.Value.Compile(compiler, method); // Evaluate expression
						method.Append(StoreField.Create(method.Module, decl.Variable.CaptureField)); // Store value in field
					}
					else if (decl.Variable.IsCaptured)
					{
						var block = decl.Variable.Parent;
						// Assign to the closure field
						if (decl.Variable.CaptureField.Parent is GeneratorClass)
							method.Append(new LoadLocal(method.GetParameter(0)));
						else
							method.Append(new LoadLocal(block.ClosureLocal)); // Load closure local
						decl.Value.Compile(compiler, method); // Evaluate expression
						method.Append(StoreField.Create(method.Module, decl.Variable.CaptureField)); // Store value in field
					}
					else
					{
						var variable = method.GetLocal(decl.Name);

						var value = decl.Value;
						if (value is ILocalResultExpression)
							((ILocalResultExpression)value).SetTargetVariable(variable);

						value.Compile(compiler, method); // Evaluate expression

						if (!(value is ILocalResultExpression))
							method.Append(new StoreLocal(variable));
					}
				}
		}
	}

	public sealed class VariableDeclarator : ParseNode
	{
		public VariableDeclarator(string name, Expression value)
		{
			Name = name;
			Value = value;
		}

		/// <summary>The name of the variable being declared.</summary>
		public string Name;

		/// <summary>The value of the assignment. May be null, except in constant declarations.</summary>
		public Expression Value;

		internal Variable Variable;

		public override string ToString(int indent)
		{
			return Value == null ? Name : Name + " = " + Value.ToString(indent);
		}

		public void FoldConstant(bool isConst)
		{
			if (Value == null)
				return;
			Value = Value.FoldConstant();
			if (isConst && !(Value is ConstantExpression))
				throw new CompileTimeException(this, "The expression could be reduced to a constant value.");
		}

		public void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			if (Value != null)
			{
				// If you have something like
				//    var myLambda = @(a, b) { a.method(b); };
				// then the lambda expression will be compiled to a method named
				//    <λ>myLambda@{1}
				// where {1} is replaced with an internal counter.
				// It is thought that this might aid in debugging.
				if (Value is LambdaExpression)
					((LambdaExpression)Value).NameHint = this.Name;
				else if (Value is LambdaMemberExpression)
					((LambdaMemberExpression)Value).NameHint = this.Name;
				Value = Value.ResolveNames(context, document, false, false);
			}
		}
	}

	// var (a, b, c) = expr;
	public sealed class ParallelLocalVariableDeclaration : LocalDeclaration
	{
		public ParallelLocalVariableDeclaration(List<string> names, Expression value)
		{
			Names = names;
			Value = value;
		}

		/// <summary>The names that are being declared.</summary>
		public List<string> Names;

		/// <summary>The expression that is being assigned.</summary>
		public Expression Value;

		internal Variable[] Variables;

		/// <summary>Indicates whether the declaration is for one or more global variables.</summary>
		public bool IsGlobal;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + "var (" + Names.JoinString(", ") + ") = " + Value.ToString(indent) + ";";
		}

		public override void FoldConstant()
		{
			Value = Value.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Value = Value.ResolveNames(context, document);
		}

		public override void DeclareNames(BlockSpace parent)
		{
			if (!IsGlobal)
				Variables = new Variable[Names.Count];

			for (var i = 0; i < Names.Count; i++)
			{
				if (!IsGlobal)
				{
					var name = Names[i];
					Variables[i] = new Variable(name, new VariableDeclarator(name, null)
					{
						StartIndex = Value.StartIndex,
						EndIndex = Value.EndIndex,
					});
				}
				parent.DeclareVariable(Variables[i]);
			}
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Value = Value.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			var assigners = new Compiler.UnpackAssigner[Variables.Length];

			for (var i = 0; i < Variables.Length; i++)
			{
				if (IsGlobal && Variables[i].IsCaptured)
				{
					var field = Variables[i].CaptureField;
					assigners[i] = (_method, isAssignment) =>
					{
						if (isAssignment)
							_method.Append(StoreField.Create(_method.Module, field));
					};
				}
				else if (Variables[i].IsCaptured)
				{
					var variable = Variables[i];
					assigners[i] = (_method, isAssignment) =>
					{
						if (!isAssignment) // Load closure class instance
						{
							if (variable.CaptureField.Parent is GeneratorClass)
								_method.Append(new LoadLocal(_method.GetParameter(0)));
							else
								_method.Append(new LoadLocal(variable.Parent.ClosureLocal));
						}
						else // Store the value in the capture field
							_method.Append(StoreField.Create(_method.Module, variable.CaptureField));
					};
				}
				else
				{
					var variable = Variables[i];
					assigners[i] = (_method, isAssignment) =>
					{
						if (isAssignment)
							_method.Append(new StoreLocal(_method.GetLocal(variable.Name)));
					};
				}
			}

			Value.Compile(compiler, method);
			compiler.Unpack(method, assigners);
		}
	}

	public sealed class LocalFunctionDeclaration : LocalDeclaration
	{
		public LocalFunctionDeclaration(string name, List<Parameter> parameters, Splat splat, Block body)
		{
			Name = name;
			Parameters = parameters;
			Splat = splat;
			Body = body;
		}

		/// <summary>The name of the function.</summary>
		public string Name;

		/// <summary>The parameters of the function.</summary>
		public List<Parameter> Parameters;

		/// <summary>The location of the splat in the function's parameters.</summary>
		public Splat Splat;

		/// <summary>The function body.</summary>
		public Block Body;

		/// <summary>The declaration space associated with the function.</summary>
		internal LocalFunction DeclSpace;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			sb.Append("function ");
			sb.Append(Name);
			if (Splat == Splat.Beginning)
				sb.Append("(...");
			else
				sb.Append("(");
			sb.Append(Parameters.JoinString(", "));
			if (Splat == Splat.End)
				sb.Append("...) ");
			else
				sb.Append(") ");
			sb.Append(Body.ToString(indent));
			return sb.ToString();
		}

		public override void FoldConstant()
		{
			foreach (var param in Parameters)
				param.FoldConstant();
			Body.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			foreach (var param in Parameters)
				param.ResolveNames(context, document);
			Body.ResolveNames(context, document);

			if (DeclSpace != null && DeclSpace.Method.IsGenerator)
				document.Compiler.AddGeneratorMethod(DeclSpace.Method);
		}

		public override void DeclareNames(BlockSpace parent)
		{
			if (DeclSpace == null)
				DeclSpace = new LocalFunction(Name, this, parent);

			parent.DeclareLocalFunction(DeclSpace);
			Body.DeclareNames(parent);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			// Generator transformations only affect the generator method,
			// not methods nested within it.
			if (forGenerator)
				return;

			// Note: we don't need to transform the parameters here.
			// Their default values, if any, are guaranteed to be constant
			// expressions, [] or {}, so they can't reference any locals anyway.
			// Except local constants, but they get inlined.
			Body.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method) { }
	}

	public enum Splat { None, Beginning, End }

	public sealed class ExpressionStatement : Statement
	{
		public ExpressionStatement(Expression expr)
		{
			if (expr == null) throw new ArgumentNullException("expr");
			Expression = expr;
		}

		/// <summary>The expression associated with the statement.</summary>
		public Expression Expression;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + Expression.ToString(indent) + ";";
		}

		public override void FoldConstant()
		{
			Expression = Expression.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Expression = Expression.ResolveNames(context, document);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Expression = Expression.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			Expression.Compile(compiler, method);
			if (!(Expression is AssignmentExpression))
				method.Append(new SimpleInstruction(Opcode.Pop));
		}
	}

	/// <summary>
	/// A statement that contains a "body" statement.
	/// Used by all control structures.
	/// </summary>
	public abstract class CompoundStatement : Statement
	{
		protected CompoundStatement(Statement body)
		{
			Body = body;
		}

		/// <summary>The body of the statement.</summary>
		public Statement Body;

		internal BlockSpace BodyBlock { get { return ((Block)Body).DeclSpace; } }

		public override bool CanReturn { get { return Body.CanReturn; } }

		public override bool CanYield { get { return Body.CanYield; } }

		public override bool IsEndReachable { get { return Body.IsEndReachable; } }

		public override void FoldConstant()
		{
			Body.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Body.ResolveNames(context, document);
		}

		public override void DeclareNames(BlockSpace parent)
		{
			Body.DeclareNames(parent);
			BodyBlock.Owner = this;
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Body.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			Body.Compile(compiler, method);
		}
	}

	public sealed class EmbeddedStatement : CompoundStatement
	{
		// Used in control structures; followed by a single embedded-statement.
		public EmbeddedStatement(Statement body)
			: base(body)
		{ }

		public override string ToString(int indent)
		{
			return ":\r\n" + Body.ToString(indent + 1); // always on same line as "owner"
		}
	}

	public sealed class IfStatement : CompoundStatement
	{
		public IfStatement(Expression cond, Statement body, ElseClause @else)
			: base(body)
		{
			Condition = cond;
			Else = @else;
		}

		/// <summary>The condition of the if statement.</summary>
		public Expression Condition;

		/// <summary>The else clause of the if statement, or null if there is none.</summary>
		public ElseClause Else;
		
		public override bool CanReturn
		{
			get
			{
				if (Condition is ConstantExpression)
				{
					var condValue = ((ConstantExpression)Condition).Value;
					if (condValue.IsTrue)
						return Body.CanReturn;
					else if (Else != null)
						return Else.CanReturn;
					return false;
				}
				return Body.CanReturn || (Else != null && Else.CanReturn);
			}
		}

		public override bool CanYield
		{
			get
			{
				if (Condition is ConstantExpression)
				{
					var condValue = ((ConstantExpression)Condition).Value;
					if (condValue.IsTrue)
						return Body.CanYield;
					else if (Else != null)
						return Else.CanYield;
					return false;
				}
				return Body.CanYield || (Else != null && Else.CanYield);
			}
		}

		public override bool IsEndReachable
		{
			get
			{
				if (Condition is ConstantExpression)
				{
					var cond = ((ConstantExpression)Condition).Value;
					return cond.IsTrue ? Body.IsEndReachable : Else == null || Else.IsEndReachable;
				}
				else
				{
					return Else == null || Body.IsEndReachable || Else.IsEndReachable;
				}
			}
		}

		public override string ToString(int indent)
		{
			var sb = new StringBuilder(new string('\t', indent) + "if ");
			sb.Append(Condition.ToString(indent + 1));
			sb.Append(" ");
			sb.Append(Body.ToString(indent));
			if (Else != null)
			{
				if (Body is EmbeddedStatement)
				{
					sb.Append("\r\n");
					sb.Append('\t', indent);
				}
				else
					sb.Append(' ');
				sb.Append(Else.ToString(indent));
			}
			return sb.ToString();
		}

		public override void FoldConstant()
		{
			Condition = Condition.FoldConstant();
			base.FoldConstant(); // Body
			if (Else != null)
				Else.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Condition = Condition.ResolveNames(context, document);
			base.ResolveNames(context, document);
			if (Else != null)
				Else.ResolveNames(context, document);
		}

		public override void DeclareNames(BlockSpace parent)
		{
			base.DeclareNames(parent); // Body
			if (Else != null)
				Else.DeclareNames(parent);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Condition = Condition.TransformClosureLocals(currentBlock, forGenerator);
			base.TransformClosureLocals(currentBlock, forGenerator);
			if (Else != null)
				Else.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Condition is ConstantExpression)
			{
				// If the condition is constant, then we don't have to emit any bytecode
				// for the branch that won't be taken. Note that stmt.Else may be null.
				var cond = ((ConstantExpression)Condition).Value;
				if (cond.IsTrue)
					Body.Compile(compiler, method); // If body
				else if (Else != null)
					Else.Compile(compiler, method); // Optional else clause
			}
			else
			{
				var elseLabel = new Label("else");
				Condition.CompileBoolean(compiler, elseLabel, false, method); // Evaluate the condition

				Body.Compile(compiler, method); // Evaluate the if body

				if (Else != null)
				{
					Label endLabel = null;
					if (Body.IsEndReachable)
					{
						endLabel = new Label("if-else-end");
						method.Append(Branch.Always(endLabel)); // Jump to end of if statement
					}

					method.Append(elseLabel);
					Else.Compile(compiler, method); // Evaluate the else clause

					if (Body.IsEndReachable)
						method.Append(endLabel); // End of if-else
				}
				else
					method.Append(elseLabel);
			}
		}
	}

	public sealed class ElseClause : CompoundStatement
	{
		public ElseClause(Statement body)
			: base(body)
		{ }

		public override string ToString(int indent)
		{
			var sb = new StringBuilder("else");
			if (Body is Block)
			{
				sb.Append(" ");
				sb.Append(Body.ToString(indent));
			}
			else if (Body is IfStatement)
			{
				sb.Append(" ");
				sb.Append(Body.ToString(indent).TrimStart('\t'));
			}
			else
			{
				sb.Append("\r\n");
				sb.Append(Body.ToString(indent + 1));
			}
			return sb.ToString();
		}
	}

	#region Iteration statements

	public abstract class IterationStatement : CompoundStatement
	{
		protected IterationStatement(string label, Statement body)
			: base(body)
		{
			Label = label;
		}

		/// <summary>The label associated with the iteration statement, or null if there is none.</summary>
		public string Label;

		/// <summary>The number of reachable 'break' statements that refer to this loop.</summary>
		internal int BreakCount;
		/// <summary>The number of reachable 'next' statements that refer to this loop.</summary>
		internal int NextCount;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (Label == null ? "" : Label + ": ");
		}

		public override void DeclareNames(BlockSpace parent)
		{
			base.DeclareNames(parent); // Body
			if (Label != null)
				BodyBlock.SetLabel(Label);
		}

		public abstract override void Compile(Compiler compiler, MethodBuilder method);

		internal static bool FindLoopState(MethodBuilder method, string label, out LoopState result)
		{
			if (label == null)
				return method.FindState<LoopState>(state => true, out result);
			else
				return method.FindState<LoopState>(state => state.LoopLabel == label, out result);
		}

		internal class LoopState
		{
			public LoopState(string loopLabel, Label nextLabel, Label breakLabel)
			{
				LoopLabel = loopLabel;
				NextLabel = nextLabel;
				BreakLabel = breakLabel;
			}

			/// <summary>
			/// The label of the loop.
			/// </summary>
			public readonly string LoopLabel;
			/// <summary>
			/// The label to branch to in a 'next' statement.
			/// </summary>
			public readonly Label NextLabel;
			/// <summary>
			/// The label to branch to in a 'break' statement.
			/// </summary>
			public readonly Label BreakLabel;
		}
	}

	public sealed class ForStatement : IterationStatement
	{
		public ForStatement(string label, List<string> variables, Expression list, Statement body, ElseClause @else)
			: base(label, body)
		{
			VariableNames = variables;
			List = list;
			Else = @else;
		}

		/// <summary>The variables associated with the for statement.</summary>
		public List<string> VariableNames;

		/// <summary>The expression that describes the list to iterate over.</summary>
		public Expression List;

		/// <summary>The else clause associated with the for loop, or null if there is none.</summary>
		public ElseClause Else;

		internal Variable[] Variables;

		/// <summary>
		/// Temporary/anon fields used in generators.
		/// </summary>
		/// <remarks>
		/// If this loop has a <see cref="RangeExpression"/> as its list, then this array contains two fields:
		/// one for the high end of the range, one for the step. Either of these may be null, if the corresponding
		/// expression can be safely inlined.
		/// Otherwise, this array only contains a single field, namely the Iterator that the loop loops over.
		/// </remarks>
		private Field[] tempFields;

		public override bool CanReturn
		{
			get
			{
				return Body.CanReturn || Else != null && Else.CanReturn;
			}
		}

		public override bool CanYield
		{
			get
			{
				return Body.CanYield || Else != null && Else.CanYield;
			}
		}

		public override bool IsEndReachable
		{
			get
			{
				return Else == null ||
					BreakCount > 0 ||
					Body.IsEndReachable ||
					Else.IsEndReachable;
			}
		}

		public override string ToString(int indent)
		{
			var sb = new StringBuilder(base.ToString(indent) + "for ");
			sb.Append(VariableNames.JoinString(", "));
			sb.Append(" in ");
			sb.Append(List.ToString(indent + 1));
			sb.Append(' ');
			sb.Append(Body.ToString(indent));
			if (Else != null)
			{
				if (Body is EmbeddedStatement)
				{
					sb.Append("\r\n");
					sb.Append('\t', indent);
				}
				else
					sb.Append(' ');
				sb.Append(Else.ToString(indent));
			}
			return sb.ToString();
		}

		public override void FoldConstant()
		{
			List = List.FoldConstant();
			if (List.IsNull)
				throw new CompileTimeException(List, "Cannot iterate over a null value.");
			base.FoldConstant(); // Body
			if (Else != null)
				Else.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			List = List.ResolveNames(context, document);
			base.ResolveNames(context, document); // Body
			if (Else != null)
				Else.ResolveNames(context, document);
		}

		public override void DeclareNames(BlockSpace parent)
		{
			base.DeclareNames(parent); // Body, label

			var body = BodyBlock;

			// Note: we need to set the VariableDeclarator indexes to ensure
			// that the loop variables cannot be used in the List expression.
			// Otherwise, you could do stuff like this:
			//     for a, b, c in [a, b, c] { ... }
			// And that's clearly wrong.

			Variables = new Variable[VariableNames.Count];
			for (var i = 0; i < VariableNames.Count; i++)
			{
				Variables[i] = new Variable(VariableNames[i],
					new VariableDeclarator(VariableNames[i], null)
					{
						StartIndex = Body.StartIndex,
						EndIndex = Body.StartIndex,
					}, VariableKind.IterationVariable);
				body.DeclareVariable(Variables[i]);
			}

			if (Else != null)
				Else.DeclareNames(parent);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			if (forGenerator && Body.CanYield)
			{
				var genClass = currentBlock.Method.GeneratorClass;
				if (List is RangeExpression)
				{
					var range = (RangeExpression)List;
					tempFields = new Field[2];
					if (!range.High.CanSafelyInline)
						tempFields[0] = genClass.DeclareAnonField(BodyBlock);
					if (!range.Step.CanSafelyInline)
						tempFields[1] = genClass.DeclareAnonField(BodyBlock);
				}
				else
				{
					tempFields = new Field[] { genClass.DeclareAnonField(BodyBlock) };
				}
			}

			List = List.TransformClosureLocals(currentBlock, forGenerator);
			base.TransformClosureLocals(currentBlock, forGenerator); // Body
			if (Else != null)
				Else.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Else == null)
			{
				if (List is RangeExpression)
					CompileWithRange(compiler, method);
				else
					CompileWithIterator(compiler, method);
			}
			else
			{
				if (List is RangeExpression)
					CompileWithRangeAndElse(compiler, method);
				else
					CompileWithIteratorAndElse(compiler, method);
			}
		}

		private void CompileWithRange(Compiler compiler, MethodBuilder method)
		{
			/* The loop form
			 *   for x in [n to m, s] { ... }
			 * is equivalent to:
			 *   var x = n;
			 *   var high = m; // evaluate only once
			 *   var step = s; //     --- " ---
			 *   while x <= high {
			 *       ...
			 *       x += step;
			 *   }
			 * And note that another part of the compiler makes sure there's
			 * only one variable to the left of 'in'. In other words, the
			 * following is invalid and checked elsewhere:
			 *   for a, b in [1 to 10] { }
			 */

			var rangeState = new RangeState
			{
				Counter = Variables[0],
				Range = (RangeExpression)List,
			};
			if (tempFields != null)
			{
				rangeState.HighField = tempFields[0];
				rangeState.StepField = tempFields[1];
			}

			rangeState.Init(compiler, method);

			var loopCond = new Label("for-cond");
			var loopNext = new Label("for-next");
			var loopEnd = new Label("for-end");

			var loopState = new LoopState(Label, loopNext, loopEnd);
			method.PushState(loopState); // Enter loop

			// Output order: condition, body, increment
			{ // Loop condition
				method.Append(loopCond);

				rangeState.LoadCounter(compiler, method); // Load the counter
				rangeState.LoadHighValue(compiler, method); // Load the high value

				method.Append(new SimpleInstruction(Opcode.Lte)); // counter <= high
				method.Append(Branch.IfFalse(loopEnd)); // If false, branch to end
				// If true, run loop body
			}
			{ // Loop body
				Body.Compile(compiler, method);
			}
			{ // Loop increment
				method.Append(loopNext); // 'next' target

				rangeState.Increment(compiler, method); // counter += step

				method.Append(Branch.Always(loopCond)); // Jump to condition (to re-test the counter)
			}
			if (this.IsEndReachable)
				method.Append(loopEnd); // end of loop; also 'break' target

			method.PopState(expected: loopState); // Exit loop

			rangeState.Done();
		}

		private void CompileWithIterator(Compiler compiler, MethodBuilder method)
		{
			/* The loop form
			 *   for x in xs { ... }
			 * is equivalent to:
			 *   var _iter = iter(xs);
			 *   while _iter.moveNext() {
			 *     var x = _iter.current;
			 *     ...
			 *   }
			 */

			var iterState = new IterState
			{
				Variables = Variables,
				IterField = tempFields != null ? tempFields[0] : null,
			};

			// First, evaluate the 'xs' expression and fetch an Iterator for it
			iterState.Init(compiler, method, List);

			var loopCond = new Label("for-start");
			var loopEnd = new Label("for-end");

			var loopState = new LoopState(Label, loopCond, loopEnd);
			method.PushState(loopState); // Enter the loop

			{ // Loop condition
				method.Append(loopCond); // 'next' target
				iterState.LoadIterator(compiler, method); // Load iterator
				method.Append(new LoadMember(method.Module.GetStringId("moveNext"))); // Load iterator.moveNext
				method.Append(new Call(0)); // Call iterator.moveNext()
				method.Append(Branch.IfFalse(loopEnd)); // If iterator.moveNext() is false, end the loop
				// Otherwise, evaluate the loop body
			}
			{ // Loop body
				iterState.UpdateCurrent(compiler, method); // Update the current iteration variables, based on iterator.current

				Body.Compile(compiler, method);
				method.Append(Branch.Always(loopCond)); // Move to next value
			}
			if (this.IsEndReachable)
				method.Append(loopEnd); // End of loop; also 'break' target

			method.PopState(expected: loopState); // Exit the loop

			iterState.Done();
		}

		private void CompileWithRangeAndElse(Compiler compiler, MethodBuilder method)
		{
			/* The loop form
			 *   for x in [n to m, s] { ... } else { ... }
			 * is equivalent to:
			 *   var x = n;
			 *   var high = m; // evaluate only once
			 *   if (x <= high) {
			 *       var step = s; //     --- " ---
			 *       do {
			 *           ...
			 *           x += step;
			 *       } while x <= high;
			 *   } else {
			 *       ...
			 *   }
			 */
			var rangeState = new RangeState
			{
				Counter = Variables[0],
				Range = (RangeExpression)List,
			};
			if (tempFields != null)
			{
				rangeState.HighField = tempFields[0];
				rangeState.StepField = tempFields[1];
			}

			rangeState.Init(compiler, method);

			var loopBody = new Label("for-body");
			var loopNext = new Label("for-next");
			var loopEnd = new Label("for-end");
			var elseLabel = new Label("for-else");

			rangeState.LoadCounter(compiler, method); // Load the counter
			rangeState.LoadHighValue(compiler, method); // Load high value
			method.Append(new SimpleInstruction(Opcode.Lte)); // counter <= high

			method.Append(Branch.IfFalse(elseLabel)); // branch to else if not (counter <= high)

			{ // Main body
				var loopState = new LoopState(Label, loopNext, loopEnd);
				method.PushState(loopState); // Enter the loop

				{ // Loop body
					method.Append(loopBody);
					Body.Compile(compiler, method);
				}
				{ // Loop increment
					method.Append(loopNext);

					rangeState.Increment(compiler, method);
				}
				{ // Loop condition
					rangeState.LoadCounter(compiler, method); // Load counter
					rangeState.LoadHighValue(compiler, method);// Load high

					method.Append(new SimpleInstruction(Opcode.Lte)); // counter <= high

					method.Append(Branch.IfTrue(loopBody)); // Run another iteration if counter <= high
				}
				method.Append(Branch.Always(loopEnd)); // Jump past the else

				method.PopState(expected: loopState); // Exit the loop

				rangeState.Done();
			}

			{ // Else
				method.Append(elseLabel);
				Else.Compile(compiler, method);
			}

			if (this.IsEndReachable)
				method.Append(loopEnd);
		}

		private void CompileWithIteratorAndElse(Compiler compiler, MethodBuilder method)
		{
			/* The loop form
			 *   for x in xs { ... } else { ... }
			 * is equivalent to:
			 *   var _iter = iter(xs);
			 *   if (_iter.moveNext()) {
			 *       do {
			 *           var x = _iter.current;
			 *           ...
			 *       } while _iter.moveNext();
			 *   } else {
			 *       ...
			 *   }
			 */

			var iterState = new IterState
			{
				Variables = Variables,
				IterField = tempFields != null ? tempFields[0] : null,
			};

			// First, evaluate the 'xs' expression
			iterState.Init(compiler, method, List);

			var loopBody = new Label("for-body");
			var loopNext = new Label("for-next");
			var loopEnd = new Label("for-end");
			var elseLabel = new Label("for-else");

			iterState.LoadIterator(compiler, method); // Load iterator
			method.Append(new LoadMember(method.Module.GetStringId("moveNext")));
			method.Append(new Call(0)); // Call iterator.moveNext()
			method.Append(Branch.IfFalse(elseLabel)); // Branch to else if false

			{ // Main body
				var loopState = new LoopState(Label, loopNext, loopEnd);
				method.PushState(loopState);

				{ // Body
					method.Append(loopBody);

					iterState.UpdateCurrent(compiler, method); // Update iteration variables

					Body.Compile(compiler, method);
				}
				{ // Loop increment
					method.Append(loopNext);

					iterState.LoadIterator(compiler, method); // Load iterator
					method.Append(new LoadMember(method.Module.GetStringId("moveNext")));
					method.Append(new Call(0)); // Call iterLoc.moveNext()
					method.Append(Branch.IfTrue(loopBody)); // Run another iteration if iterLoc.moveNext()
				}

				method.PopState(expected: loopState);

				method.Append(Branch.Always(loopEnd)); // Jump past else
			}

			{ // Else
				method.Append(elseLabel);
				Else.Compile(compiler, method);
			}

			if (this.IsEndReachable)
				method.Append(loopEnd);

			iterState.Done();
		}

		private class RangeState
		{
			public Variable Counter;
			public LocalVariable CounterLoc;

			public RangeExpression Range;

			public LocalVariable HighLoc;
			public LocalVariable StepLoc;
			public Field HighField;
			public Field StepField;

			public void Init(Compiler compiler, MethodBuilder method)
			{
				if (Counter.IsCaptured && Counter.CaptureField.Parent is GeneratorClass)
				{
					method.Append(new LoadLocal(method.GetParameter(0)));
					Range.Low.Compile(compiler, method);
					method.Append(StoreField.Create(method.Module, Counter.CaptureField));
				}
				else
				{
					CounterLoc = method.GetLocal(Counter.Name);
					Range.Low.Compile(compiler, method);
					method.Append(new StoreLocal(CounterLoc));
				}

				if (!Range.High.CanSafelyInline)
				{
					if (HighField != null)
					{
						method.Append(new LoadLocal(method.GetParameter(0)));
						Range.High.Compile(compiler, method);
						method.Append(StoreField.Create(method.Module, HighField));
					}
					else
					{
						HighLoc = method.GetAnonymousLocal();
						Range.High.Compile(compiler, method);
						method.Append(new StoreLocal(HighLoc));
					}
				}

				if (!Range.Step.CanSafelyInline)
				{
					if (StepField != null)
					{
						method.Append(new LoadLocal(method.GetParameter(0)));
						Range.Step.Compile(compiler, method);
						method.Append(StoreField.Create(method.Module, StepField));
					}
					else
					{
						StepLoc = method.GetAnonymousLocal();
						Range.Step.Compile(compiler, method);
						method.Append(new StoreLocal(StepLoc));
					}
				}
			}

			public void Done()
			{
				if (HighLoc != null)
					HighLoc.Done();
				if (StepLoc != null)
					StepLoc.Done();
			}

			public void LoadCounter(Compiler compiler, MethodBuilder method)
			{
				if (CounterLoc != null)
					method.Append(new LoadLocal(CounterLoc));
				else
				{
					method.Append(new LoadLocal(method.GetParameter(0)));
					method.Append(LoadField.Create(method.Module, Counter.CaptureField));
				}
			}

			public void Increment(Compiler compiler, MethodBuilder method)
			{
				if (CounterLoc == null)
					method.Append(new LoadLocal(method.GetParameter(0)));

				LoadCounter(compiler, method); // Load the counter
				LoadStepValue(compiler, method); // Load step value
				method.Append(new SimpleInstruction(Opcode.Add)); // counter + step

				// counter += step
				if (CounterLoc != null)
					method.Append(new StoreLocal(CounterLoc));
				else
					method.Append(StoreField.Create(method.Module, Counter.CaptureField));
			}

			public void LoadHighValue(Compiler compiler, MethodBuilder method)
			{
				if (Range.High.CanSafelyInline)
					Range.High.Compile(compiler, method);
				else if (HighField != null)
				{
					method.Append(new LoadLocal(method.GetParameter(0)));
					method.Append(LoadField.Create(method.Module, HighField));
				}
				else
					method.Append(new LoadLocal(HighLoc));
			}

			public void LoadStepValue(Compiler compiler, MethodBuilder method)
			{
				if (Range.Step.CanSafelyInline)
					Range.Step.Compile(compiler, method);
				else if (StepField != null)
				{
					method.Append(new LoadLocal(method.GetParameter(0)));
					method.Append(LoadField.Create(method.Module, StepField));
				}
				else
					method.Append(new LoadLocal(StepLoc));
			}
		}

		private class IterState
		{
			public Variable[] Variables;
			public Compiler.UnpackAssigner[] VariableAssigners;

			public LocalVariable IterLoc;
			public Field IterField;

			public void Init(Compiler compiler, MethodBuilder method, Expression inner)
			{
				if (Variables.Length > 1)
				{
					VariableAssigners = new Compiler.UnpackAssigner[Variables.Length];
					for (var i = 0; i < Variables.Length; i++)
					{
						if (Variables[i].IsCaptured && Variables[i].CaptureField.Parent is GeneratorClass)
						{
							var field = Variables[i].CaptureField;
							VariableAssigners[i] = (_method, isAssignment) =>
							{
								if (!isAssignment)
									_method.Append(new LoadLocal(_method.GetParameter(0)));
								else
									_method.Append(StoreField.Create(_method.Module, field));
							};
						}
						else
						{
							var local = method.GetLocal(Variables[i].Name);
							VariableAssigners[i] = (_method, isAssignment) =>
							{
								if (isAssignment)
									_method.Append(new StoreLocal(local));
							};
						}
					}
				}

				if (IterField != null)
				{
					method.Append(new LoadLocal(method.GetParameter(0)));
					inner.Compile(compiler, method);
					method.Append(new SimpleInstruction(Opcode.Lditer));
					method.Append(StoreField.Create(method.Module, IterField));
				}
				else
				{
					IterLoc = method.GetAnonymousLocal();
					inner.Compile(compiler, method);
					method.Append(new SimpleInstruction(Opcode.Lditer));
					method.Append(new StoreLocal(IterLoc));
				}
			}

			public void Done()
			{
				if (IterLoc != null)
					IterLoc.Done();
			}

			public void LoadIterator(Compiler compiler, MethodBuilder method)
			{
				if (IterField != null)
				{
					method.Append(new LoadLocal(method.GetParameter(0)));
					method.Append(LoadField.Create(method.Module, IterField));
				}
				else
					method.Append(new LoadLocal(IterLoc));
			}

			public void UpdateCurrent(Compiler compiler, MethodBuilder method)
			{
				if (Variables.Length > 1)
				{
					LoadIterator(compiler, method);
					method.Append(new LoadMember(method.Module.GetStringId("current")));

					compiler.Unpack(method, VariableAssigners);
				}
				else
				{
					var target = Variables[0];
					var isGenField = target.IsCaptured && target.CaptureField.Parent is GeneratorClass;
					if (isGenField)
						method.Append(new LoadLocal(method.GetParameter(0)));

					LoadIterator(compiler, method);
					method.Append(new LoadMember(method.Module.GetStringId("current")));

					if (isGenField)
						method.Append(StoreField.Create(method.Module, target.CaptureField));
					else
					{
						var loc = method.GetLocal(target.Name);
						method.Append(new StoreLocal(loc));
					}
				}
			}
		}
	}

	public sealed class WhileStatement : IterationStatement
	{
		public WhileStatement(string label, Expression cond, Statement body)
			: base(label, body)
		{
			Condition = cond;
		}

		/// <summary>Gets the condition for the loop.</summary>
		public Expression Condition;

		public override bool CanReturn
		{
			get
			{
				if (Condition is ConstantExpression &&
					!((ConstantExpression)Condition).Value.IsTrue)
					return false;
				return base.CanReturn;
			}
		}

		public override bool CanYield
		{
			get
			{
				if (Condition is ConstantExpression &&
					   !((ConstantExpression)Condition).Value.IsTrue)
					return false;
				return base.CanYield;
			}
		}

		public override bool IsEndReachable
		{
			get
			{
				var cond = Condition as ConstantExpression;
				return BreakCount > 0 || cond == null || !cond.Value.IsTrue;
			}
		}

		public override string ToString(int indent)
		{
			var sb = new StringBuilder(base.ToString(indent) + "while ");
			sb.Append(Condition.ToString(indent + 1));
			sb.Append(" ");
			sb.Append(Body.ToString(indent));
			return sb.ToString();
		}

		public override void FoldConstant()
		{
			Condition = Condition.FoldConstant();
			base.FoldConstant(); // Body
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Condition = Condition.ResolveNames(context, document);
			base.ResolveNames(context, document); // Body
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Condition = Condition.TransformClosureLocals(currentBlock, forGenerator);
			base.TransformClosureLocals(currentBlock, forGenerator); // Body
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Condition is ConstantExpression &&
				!((ConstantExpression)Condition).Value.IsTrue)
				return;

			var loopBody = new Label("while-body");
			var loopCond = new Label("while-cond");
			var loopEnd = new Label("while-end");

			var loopState = new LoopState(Label, loopCond, loopEnd);
			method.PushState(loopState); // Enter the loop

			if (!(Condition is ConstantExpression))
				method.Append(Branch.Always(loopCond)); // Branch to condition if non-constant
			{ // Body
				method.Append(loopBody);
				Body.Compile(compiler, method); // Evaluate the body
			}
			{ // Condition
				method.Append(loopCond);
				Condition.CompileBoolean(compiler, loopBody, true, method);
				// Fall through for false
			}
			if (this.IsEndReachable)
				method.Append(loopEnd);

			method.PopState(expected: loopState); // Exit the loop
		}
	}

	public sealed class DoWhileStatement : IterationStatement
	{
		public DoWhileStatement(string label, Block body, Expression cond)
			: base(label, body)
		{
			Condition = cond;
		}

		/// <summary>The condition for the loop.</summary>
		public Expression Condition;

		public override bool IsEndReachable
		{
			get
			{
				var cond = Condition as ConstantExpression;
				return BreakCount > 0 ||
					IsConditionReachable && (cond == null || !cond.Value.IsTrue);
			}
		}

		private bool IsConditionReachable
		{
			get
			{
				return Body.IsEndReachable || NextCount > 0;
			}
		}

		public override string ToString(int indent)
		{
			if (Condition != null)
				return base.ToString(indent) + "do " + Body.ToString(indent) + " while " + Condition.ToString(indent + 1) + ";";
			else
				return base.ToString(indent) + "do " + Body.ToString(indent) + ";";
		}

		public override void FoldConstant()
		{
			base.FoldConstant(); // Body
			if (Condition != null)
				Condition = Condition.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			base.ResolveNames(context, document); // Body
			if (Condition != null)
				Condition = Condition.ResolveNames(context, document);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			base.TransformClosureLocals(currentBlock, forGenerator); // Body
			if (Condition != null)
				Condition = Condition.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			var loopStart = Condition == null ? null : new Label("do-while-start");
			var loopCond = Condition == null ? null : new Label("do-while-cond");
			var loopEnd = new Label("do-while-end");

			var loopState = new LoopState(Label, loopCond, loopEnd);
			method.PushState(loopState);

			if (loopStart != null)
				method.Append(loopStart);

			Body.Compile(compiler, method); // Körper

			if (Condition != null && IsConditionReachable)
			{ // Condition
				method.Append(loopCond);

				Condition.CompileBoolean(compiler, loopStart, true, method);
				// Fall through for false (negated: true)
			}
			if (this.IsEndReachable)
				method.Append(loopEnd);

			method.PopState(expected: loopState);
		}
	}

	#endregion

	#region Try statement

	public sealed class TryStatement : CompoundStatement
	{
		// Note: catches may not be null, but it can be empty.
		public TryStatement(Block body, List<CatchClause> catches, FinallyClause fin)
			: base(body)
		{
			if (catches == null)
				throw new ArgumentNullException("catches");

			Catches = catches;
			Finally = fin;
		}

		/// <summary>The catch clauses associated with the try statement.</summary>
		public List<CatchClause> Catches;

		/// <summary>The finally clause associated with the try statement, or null if there is none.</summary>
		public FinallyClause Finally;

		// Note: there is always either one CatchClause or a FinallyClause.
		// It is not possible to have a TryStatement with no CatchClauses and
		// no FinallyClause.

		private bool? canReturn = null;
		public override bool CanReturn
		{
			get
			{
				// Note: finally clauses are not able to return, which is checked
				// at another step during compilation.
				if (!canReturn.HasValue)
					canReturn = Body.CanReturn || (Catches.Count > 0 && Catches.Any(stmt => stmt.CanReturn));
				return canReturn.Value;
			}
		}

		private bool? isEndReachable = null;
		public override bool IsEndReachable
		{
			get
			{
				if (!isEndReachable.HasValue)
					isEndReachable = (Body.IsEndReachable ||
						Catches.Count == 0 ||
						Catches.Any(c => c.IsEndReachable))
						&&
						(Finally == null || Finally.IsEndReachable);

				return isEndReachable.Value;
			}
		}

		public override string ToString(int indent)
		{
			var sb = new StringBuilder(new string('\t', indent) + "try ");
			sb.Append(Body.ToString(indent));
			foreach (var c in Catches)
				sb.Append(" " + c.ToString(indent));
			if (Finally != null)
				sb.Append(" " + Finally.ToString(indent));
			return sb.ToString();
		}

		public override void FoldConstant()
		{
			base.FoldConstant(); // Body
			foreach (var @catch in Catches)
				@catch.FoldConstant();
			if (Finally != null)
				Finally.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			base.ResolveNames(context, document); // Body
			foreach (var @catch in Catches)
				@catch.ResolveNames(context, document);
			if (Finally != null)
				Finally.ResolveNames(context, document);
		}

		public override void DeclareNames(BlockSpace parent)
		{
			base.DeclareNames(parent); // Body

			var body = ((Block)Body).DeclSpace;

			foreach (var clause in Catches)
				clause.DeclareNames(body);

			if (Finally != null)
				Finally.DeclareNames(body);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			base.TransformClosureLocals(currentBlock, forGenerator); // Body

			for (var i = 0; i < Catches.Count; i++)
				Catches[i].TransformClosureLocals(currentBlock, forGenerator);

			if (Finally != null)
				Finally.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			TryBlock tryFinally = null;
			TryBlock tryCatch = null;

			if (Finally != null)
				tryFinally = method.BeginTry();

			if (Catches.Count > 0)
				tryCatch = method.BeginTry();

			TryState state = null;
			if (CanReturn)
			{
				if (FindTryState(method, out state))
					state = null; // Already in a try block
				else
				{
					var returnLabel = new Label("try-return");
					var returnLocal = method.GetAnonymousLocal();
					state = new TryState(returnLabel, returnLocal);
					method.PushState(state); // Enter the try block, if we aren't in one already
				}
			}

			var endLabel = Body.IsEndReachable || Catches.Any(c => c.IsEndReachable) ? new Label("try-end") : null;

			Body.Compile(compiler, method); // Evaluate the try body
			if (Body.IsEndReachable)
				method.Append(Branch.Leave(endLabel)); // Leave the try block

			if (Catches.Count > 0)
			{
				tryCatch.EndBlock(); // End the try-catch block's try part

				foreach (var @catch in Catches)
				{
					var caughtType = @catch is SpecificCatchClause ?
						((SpecificCatchClause)@catch).Type.Type :
						compiler.ObjectType;
					var catchBlock = tryCatch.AddCatch(method.Module.GetTypeId(caughtType));

					@catch.Compile(compiler, method); // Evaluate the catch body
					if (@catch.IsEndReachable)
						method.Append(Branch.Leave(endLabel)); // Leave the catch block

					catchBlock.EndBlock();
				}
			}

			if (Finally != null)
			{
				tryFinally.EndBlock(); // End the try-finally block's try part

				var finallyBlock = tryFinally.AddFinally();

				Finally.Compile(compiler, method); // Evaluate the finally body
				method.Append(new SimpleInstruction(Opcode.Endfinally)); // End the finally block

				finallyBlock.EndBlock();
			}

			if (state != null)
			{
				method.PopState(expected: state); // Exit the try block

				method.Append(state.ReturnLabel);
				method.Append(new LoadLocal(state.ReturnLocal)); // Load the return value
				method.Append(new SimpleInstruction(Opcode.Ret)); // Return it

				state.ReturnLocal.Done(); // Done!
			}

			if (endLabel != null)
				method.Append(endLabel);
		}

		internal static bool FindTryState(MethodBuilder method, out TryState result)
		{
			return method.FindState<TryState>(state => true, out result);
		}

		internal class TryState
		{
			public TryState(Label returnLabel, LocalVariable returnLocal)
			{
				ReturnLabel = returnLabel;
				ReturnLocal = returnLocal;
			}

			public readonly Label ReturnLabel;
			public readonly LocalVariable ReturnLocal;
		}
	}

	public class CatchClause : CompoundStatement
	{
		public CatchClause(Block body)
			: base(body)
		{ }

		public override string ToString(int indent)
		{
			return "catch " + Body.ToString(indent);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.Append(new SimpleInstruction(Opcode.Pop));
			Body.Compile(compiler, method);
		}
	}

	public sealed class SpecificCatchClause : CatchClause
	{
		public SpecificCatchClause(TypeName type, string variable, Block body)
			: base(body)
		{
			Type = type;
			Variable = variable;
		}

		/// <summary>The type of the exception to be caught.</summary>
		public TypeName Type;

		/// <summary>The variable in which to store the exception. May be null.</summary>
		public string Variable;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder("catch ");
			sb.Append(Type.ToString(indent));
			if (Variable != null)
			{
				sb.Append(" in ");
				sb.Append(Variable);
			}
			sb.Append(" ");
			sb.Append(Body.ToString(indent));
			return sb.ToString();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			context.GetContainingNamespace().ResolveTypeName(Type, document);
			if (!Type.Type.InheritsFrom(document.Compiler.ErrorType))
				throw new CompileTimeException(Type, "The type in a catch clause must inherit from aves.Error.");
			base.ResolveNames(context, document); // Body
		}

		public override void DeclareNames(BlockSpace parent)
		{
			base.DeclareNames(parent); // Body

			if (Variable != null)
				BodyBlock.DeclareVariable(new Variable(Variable,
					new VariableDeclarator(Variable, null)
					{
						StartIndex = Body.StartIndex,
						EndIndex = Body.StartIndex,
					}, VariableKind.CatchVariable));
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Variable != null)
			{
				var errorLocal = method.GetLocal(Variable);
				method.Append(new StoreLocal(errorLocal));
			}
			else
				method.Append(new SimpleInstruction(Opcode.Pop));

			Body.Compile(compiler, method);
		}
	}

	public sealed class FinallyClause : CompoundStatement
	{
		public FinallyClause(Block body)
			: base(body)
		{ }

		public override string ToString(int indent)
		{
			return "finally " + Body.ToString(indent);
		}
	}

	#endregion

	#region Jump statements

	public sealed class ReturnStatement : Statement
	{
		public ReturnStatement()
			: this(new List<Expression>())
		{ }
		public ReturnStatement(List<Expression> returnValues)
		{
			if (returnValues == null)
				throw new ArgumentNullException("returnValues");
			ReturnValues = returnValues;
		}

		internal ReturnStatement(Expression returnValue)
			: this(new List<Expression> { returnValue })
		{ }

		/// <summary>The return values of the return statement.</summary>
		public List<Expression> ReturnValues;

		private Field stateField;

		public override bool CanReturn { get { return true; } }

		public override bool IsEndReachable { get { return false; } }

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (ReturnValues == null ? "return;" :
				"return " + ReturnValues.JoinString(", ", indent) + ";");
		}

		public override void FoldConstant()
		{
			for (var i =0; i < ReturnValues.Count; i++)
				ReturnValues[i] = ReturnValues[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			var block = context as BlockSpace;
			if (block != null)
			{
				if (ReturnValues.Count > 0 && block.Method is ClassMemberMethod)
				{
					var member = ((ClassMemberMethod)block.Method).Owner;
					if (member.Kind == MemberKind.Constructor ||
						member.Kind == MemberKind.PropertySetter)
						throw new CompileTimeException(this, "Constructors and property setters cannot return any values.");
				}
				else if (block.Method == document.Compiler.MainMethod)
					throw new CompileTimeException(this, "Cannot return from the top level of a script.");

				if (block.IsInsideFinally())
					throw new CompileTimeException(this, "Cannot return inside a finally clause.");

				block.Method.AddReturn(this);
			}

			for (var i = 0; i < ReturnValues.Count; i++)
				ReturnValues[i] = ReturnValues[i].ResolveNames(context, document);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < ReturnValues.Count; i++)
				ReturnValues[i] = ReturnValues[i].TransformClosureLocals(currentBlock, forGenerator);

			if (forGenerator)
				stateField = currentBlock.Method.GeneratorClass.StateField;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			var inGenerator = stateField != null;

			if (inGenerator)
			{
				method.Append(new LoadLocal(method.GetParameter(0))); // Load this
				method.Append(new LoadConstantInt(0)); // Load the end-of-iteration state
				method.Append(StoreField.Create(method.Module, stateField)); // Update this.'<>state'
			}

			TryStatement.TryState tryState;
			if (TryStatement.FindTryState(method, out tryState))
			{
				// If we're in a try block, we assign to the return local
				// and then leave the block.
				if (inGenerator)
					method.Append(LoadConstant.False());
				else if (ReturnValues.Count == 0)
					method.Append(LoadConstant.Null());
				else
					ReturnValues[0].Compile(compiler, method);
				method.Append(new StoreLocal(tryState.ReturnLocal));

				method.Append(Branch.Leave(tryState.ReturnLabel));
			}
			else if (inGenerator)
			{
				method.Append(LoadConstant.False());
				method.Append(new SimpleInstruction(Opcode.Ret));
			}
			else if (ReturnValues.Count == 0 || ReturnValues[0].IsNull)
			{
				method.Append(new SimpleInstruction(Opcode.Retnull));
			}
			else
			{
				ReturnValues[0].Compile(compiler, method);
				method.Append(new SimpleInstruction(Opcode.Ret));
			}
		}
	}

	public sealed class YieldStatement : Statement
	{
		public YieldStatement(List<Expression> returnValues)
		{
			ReturnValues = returnValues;
		}

		/// <summary>The return values of the yield statement.</summary>
		public List<Expression> ReturnValues;

		public override bool CanYield { get { return true; } }

		internal Method ParentMethod;
		internal Label StateLabel;
		private GeneratorClass genClass;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + "yield " + ReturnValues.JoinString(", ", indent) + ";";
		}

		public override void FoldConstant()
		{
			for (var i = 0; i < ReturnValues.Count; i++)
				ReturnValues[i] = ReturnValues[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			var block = context as BlockSpace;
			if (block == null)
				throw new CompileTimeException(this, "'yield' encountered outside block.");

			if (block.Method is ClassMemberMethod)
			{
				var member = ((ClassMemberMethod)block.Method).Owner;
				if (member.Kind == MemberKind.Constructor ||
					member.Kind == MemberKind.PropertySetter ||
					member.Kind == MemberKind.Operator)
					throw new CompileTimeException(this, "Constructors, property setters and operators cannot be generator methods.");
			}
			else if (block.Method == document.Compiler.MainMethod)
				throw new CompileTimeException(this, "Cannot yield from the top level of a script.");

			if (block.IsInsideProtectedBlock())
				throw new CompileTimeException(this, "Cannot yield inside a try, catch or finally.");

			block.Method.AddYield(this);

			for (var i = 0; i < ReturnValues.Count; i++)
				ReturnValues[i] = ReturnValues[i].ResolveNames(context, document);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < ReturnValues.Count; i++)
				ReturnValues[i] = ReturnValues[i].TransformClosureLocals(currentBlock, forGenerator);

			if (forGenerator)
			{
				genClass = currentBlock.Method.GeneratorClass;
				ParentMethod = genClass.MoveNextMethod;
			}
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			// Note: parser wraps multiple yield values in a ListLiteralExpression.
			// Hence, ReturnValues only ever contains a single value.

			ParentMethod.Yields.Add(this); // Mark this yield as reachable

			method.Append(new LoadLocal(method.GetParameter(0))); // Load 'this'
			ReturnValues[0].Compile(compiler, method); // Evaluate yield value
			method.Append(StoreField.Create(method.Module, genClass.CurrentValueField)); // Update the current value field

			var generatorState = ParentMethod.Yields.Count - 1;
			StateLabel = new Label(string.Format("yield-{0}-continue", generatorState));

			method.Append(new LoadLocal(method.GetParameter(0))); // Load 'this'
			method.Append(new LoadConstantInt(generatorState)); // Load the state associated with this yield
			method.Append(StoreField.Create(method.Module, genClass.StateField)); // Update the state field

			method.Append(LoadConstant.True()); // Load true
			method.Append(new SimpleInstruction(Opcode.Ret)); // Return true

			method.Append(StateLabel); // Continue after this yield
		}
	}

	public sealed class NextStatement : Statement
	{
		public NextStatement(string label)
		{
			Label = label;
		}

		/// <summary>The loop label associated with the next statement.</summary>
		public string Label;

		private BlockSpace parent;
		private bool isLeave = false;

		public override bool IsEndReachable { get { return false; } }

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (Label == null ? "next;" : "next " + Label + ";");
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			// Verify that there is a loop to refer to.
			if (context is BlockSpace)
			{
				parent = (BlockSpace)context;
				var loop = parent.FindLoop(this, Label, out isLeave);

				if (loop is DoWhileStatement && ((DoWhileStatement)loop).Condition == null)
					throw new CompileTimeException(this, "A 'next' statement may not refer to a 'do {...};' statement. Use 'break' instead.");

				loop.NextCount++;
			}
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			IterationStatement.LoopState loop;
			IterationStatement.FindLoopState(method, Label, out loop);
			if (isLeave)
				method.Append(Branch.Leave(loop.NextLabel));
			else
				method.Append(Branch.Always(loop.NextLabel));
		}
	}

	public sealed class BreakStatement : Statement
	{
		public BreakStatement(string label)
		{
			Label = label;
		}

		/// <summary>The loop label associated with the next statement.</summary>
		public string Label;

		private BlockSpace parent;
		private bool isLeave = false;

		public override bool IsEndReachable { get { return false; } }

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (Label == null ? "break;" : "break " + Label + ";");
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			if (context is BlockSpace)
			{
				parent = (BlockSpace)context;
				var loop = parent.FindLoop(this, Label, out isLeave);
				loop.BreakCount++;
			}
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			IterationStatement.LoopState loop;
			IterationStatement.FindLoopState(method, Label, out loop);
			if (isLeave)
				method.Append(Branch.Leave(loop.BreakLabel));
			else
				method.Append(Branch.Always(loop.BreakLabel));
		}
	}

	public sealed class ThrowStatement : Statement
	{
		public ThrowStatement(Expression value)
		{
			Value = value;
		}

		/// <summary>The value associated with the throw statement.</summary>
		public Expression Value;

		public override bool IsEndReachable { get { return false; } }

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (Value == null ? "throw;" : "throw " + Value.ToString(indent + 1) + ";");
		}

		public override void FoldConstant()
		{
			if (Value != null)
				Value = Value.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			var block = (BlockSpace)context;

			if (Value == null)
			{
				bool crossesFinally;
				if (block.IsInsideCatch(out crossesFinally))
				{
					if (crossesFinally)
						throw new CompileTimeException(this,
							"A throw with no argument cannot occur inside a finally nested within a catch clause.");
				}
				else
					throw new CompileTimeException(this, "A throw with no argument can only occur inside a catch clause.");
			}

			if (Value != null)
				Value = Value.ResolveNames(context, document);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			if (Value != null)
				Value = Value.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Value == null)
				method.Append(new SimpleInstruction(Opcode.Rethrow));
			else
			{
				Value.Compile(compiler, method);
				method.Append(new SimpleInstruction(Opcode.Throw));
			}
		}
	}

	#endregion

	public sealed class CompoundAssignment : Statement
	{
		public CompoundAssignment(Expression target, Expression value, BinaryOperator op)
		{
			Target = target;
			Operator = op;
			Value = value;
		}

		/// <summary>The target of the assignment.</summary>
		public Expression Target;

		/// <summary>The operator associated with the compound assignment.</summary>
		public BinaryOperator Operator;

		/// <summary>The value of the assignment.</summary>
		public Expression Value;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + Target.ToString(indent) + " " +
				BinaryOperatorExpression.GetOperatorToken(Operator) + "= " +
				Value.ToString(indent + 1) + ";";
		}

		public override void FoldConstant()
		{
			// The target isn't really meant to be reduced to a single constant expression,
			// but sub expressions might be!
			// Like, if you're assigning to a(b, c).d or something. (Why would you do that?)
			Target = Target.FoldConstant();
			Value = Value.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			Target = Target.ResolveNames(context, document);
			AssignmentExpression.EnsureAssignable(Target);
			Value = Value.ResolveNames(context, document);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Target = Target.TransformClosureLocals(currentBlock, forGenerator);
			Value = Value.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			((AssignableExpression)Target).CompileCompoundAssignment(compiler, method, Value, Operator);
		}
	}

	public sealed class ParallelAssignment : Statement
	{
		// 'targets' will have at least two values, otherwise it's not a parallel assignment.
		public ParallelAssignment(List<Expression> targets, List<Expression> values)
		{
			Targets = targets;
			Values = values;
		}

		/// <summary>The targets of the assignment.</summary>
		public List<Expression> Targets;

		/// <summary>The values of the assignment.</summary>
		public List<Expression> Values;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + Targets.JoinString(", ", indent) + " = " + Values.JoinString(", ", indent + 1) + ";";
		}

		public override void FoldConstant()
		{
			for (var i = 0; i < Targets.Count; i++)
				Targets[i] = Targets[i].FoldConstant();
			for (var i = 0; i < Values.Count; i++)
				Values[i] = Values[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			for (var i = 0; i < Targets.Count; i++)
			{
				var result = Targets[i].ResolveNames(context, document);
				AssignmentExpression.EnsureAssignable(result);
				Targets[i] = result;
			}

			for (var i = 0; i < Values.Count; i++)
				Values[i] = Values[i].ResolveNames(context, document);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Targets.Count; i++)
				Targets[i] = Targets[i].TransformClosureLocals(currentBlock, forGenerator);

			for (var i = 0; i < Values.Count; i++)
				Values[i] = Values[i].TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (Values.Count == 1)
				CompileOneToMany(compiler, method);
			else
				CompileManyToMany(compiler, method);
		}

		private void CompileOneToMany(Compiler compiler, MethodBuilder method)
		{
			if (AreAllTargetsLocalAccess())
			{
				var _method = method;
				var unpackTargets = Targets.Select(t => ((LocalVariableAccess)t).GetLocal(_method));

				Values[0].Compile(compiler, method);
				compiler.Unpack(method, unpackTargets);
			}
			else
			{
				var count = Targets.Count;
				var unpackTargets = new LocalVariable[count];
				var targetLocals = new LocalVariable[count][];

				for (var i = 0; i < count; i++)
					if (Targets[i] is LocalVariableAccess)
						unpackTargets[i] = ((LocalVariableAccess)Targets[i]).GetLocal(method);
					else
					{
						targetLocals[i] = ((AssignableExpression)Targets[i]).CompileParallelFirstEvaluation(compiler, method);
						unpackTargets[i] = method.GetAnonymousLocal();
					}

				// Unpack the List into the appropriate targets.
				// If any target is a local variable, we unpack straight into it.
				Values[0].Compile(compiler, method); // Evaluate the List value, leaving it on the stack
				compiler.Unpack(method, unpackTargets);

				for (var i = 0; i < count; i++)
					if (!(Targets[i] is LocalVariableAccess))
					{
						var target = (AssignableExpression)Targets[i];
						var currentTargetLocals = targetLocals[i];

						// Load the instance, if any
						target.CompileParallelLoadInstance(compiler, method, currentTargetLocals);
						// Then the value
						method.Append(new LoadLocal(unpackTargets[i]));
						// Evaluate the store
						target.CompileParallelAssignment(compiler, method, currentTargetLocals);

						if (currentTargetLocals != null)
							foreach (var local in currentTargetLocals)
								if (local != null && local.IsAnonymous)
									local.Done();
					}
			}
		}

		private void CompileManyToMany(Compiler compiler, MethodBuilder method)
		{
			if (AreAllTargetsLocalAccess())
			{
				// Optimise parallel assignments to all-locals.
				// The store is guaranteed not to have any side-effects of any kind
				// (other than updating the locals, of course), so we can make this
				// quite a lot simpler!

				// Evaluate the values in order
				foreach (var value in Values)
					value.Compile(compiler, method);

				// And store them in reverse!
				for (var i = Targets.Count - 1; i >= 0; i--)
				{
					var variable = ((LocalVariableAccess)Targets[i]).GetLocal(method);
					method.Append(new StoreLocal(variable));
				}
			}
			else
			{
				var count = Targets.Count;
				var targetLocals = new LocalVariable[count][];

				// First, evaluate the instance expression (if any) of each target
				for (var i = 0; i < count; i++)
					targetLocals[i] = ((AssignableExpression)Targets[i]).CompileParallelFirstEvaluation(compiler, method);

				// Then, evaluate each value, storing the result in a local
				// unless the value can be safely inlined.

				var valueLocals = new LocalVariable[Values.Count];
				for (var i = 0; i < count; i++)
					if (!Values[i].CanSafelyInline)
					{
						Values[i].Compile(compiler, method);
						valueLocals[i] = method.GetAnonymousLocal();
						method.Append(new StoreLocal(valueLocals[i]));
					}

				// Finally, put each instance (+ args, if any) back on the stack,
				// along with each value, and perform the store.
				for (var i = 0; i < count; i++)
				{
					var target = (AssignableExpression)Targets[i];
					var currentTargetLocals = targetLocals[i];

					// Load the target instance (if any)

					target.CompileParallelLoadInstance(compiler, method, currentTargetLocals);

					// Load the value
					if (Values[i].CanSafelyInline)
						Values[i].Compile(compiler, method);
					else
					{
						method.Append(new LoadLocal(valueLocals[i]));
						valueLocals[i].Done(); // No longer needed
					}

					// Perform the store
					target.CompileParallelAssignment(compiler, method, currentTargetLocals);

					if (currentTargetLocals != null)
						foreach (var local in currentTargetLocals)
							if (local != null && local.IsAnonymous)
								local.Done(); // No longer needed
				}
			}
		}

		private bool AreAllTargetsLocalAccess()
		{
			HashSet<Variable> variables = new HashSet<Variable>();
			foreach (var e in Targets)
				if (!(e is LocalVariableAccess) ||
					!variables.Add(((LocalVariableAccess)e).Variable))
					return false;
			return true;
		}
	}

	public sealed class BaseInitializer : Statement
	{
		public BaseInitializer(List<Expression> arguments)
		{
			Arguments = arguments;
		}

		/// <summary>The arguments passed into the base constructor.</summary>
		public List<Expression> Arguments;

		private MethodGroup constructor;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + "new base(" + Arguments.JoinString(", ", indent + 1) + ");";
		}

		public override void FoldConstant()
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			bool _;
			Class @class = context.GetContainingClass(out _);
			if (@class.BaseType == null)
				throw new CompileTimeException(this, "Base initializers may not be used inside aves.Object.");

			var baseCtor = @class.BaseType.FindConstructor(this, Arguments.Count, @class);
			if (baseCtor == null)
				throw new CompileTimeException(this, string.Format("Could not find a constructor for '{0}' that takes {1} arguments.",
					@class.BaseType.FullName, Arguments.Count));

			constructor = baseCtor.Group;

			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].ResolveNames(context, document, false, false);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.Append(new LoadLocal(method.GetParameter(0))); // Load 'this'

			foreach (var arg in Arguments) // Evaluate each argument
				arg.Compile(compiler, method);

			method.Append(new StaticCall(method.Module.GetMethodId(constructor), Arguments.Count)); // Call the base constructor
			method.Append(new SimpleInstruction(Opcode.Pop));
		}
	}
}