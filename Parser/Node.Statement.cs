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

		/// <summary>
		/// Performs constant folding on the statement.
		/// </summary>
		public virtual void FoldConstant() { }

		/// <summary>
		/// Performs name resolution on the statement.
		/// </summary>
		/// <param name="context">The context in which to resolve names.</param>
		/// <param name="document">The file namespace in which to resolve global variables.</param>
		/// <param name="reachable">Indicates whether the statement is reachable.</param>
		public virtual void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable) { }

		public virtual void DeclareNames(BlockSpace parent) { }

		public virtual void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator) { }

		public virtual void Compile(Compiler compiler, MethodBuilder method) { }
	}

	public sealed class EmptyStatement : Statement
	{
		public EmptyStatement() { }
		public EmptyStatement(int startIndex, int endIndex)
		{
			StartIndex = startIndex;
			EndIndex = endIndex;
		}

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
			canReturn = canYield = isEndReachable = null; // reset!

			foreach (var stmt in Statements)
				stmt.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			canReturn = canYield = isEndReachable = null; // reset!

			foreach (var stmt in Statements)
			{
				stmt.ResolveNames(this.DeclSpace, document, reachable);
				if (reachable && !stmt.IsEndReachable)
					reachable = false;
			}

			if (DeclSpace.members.Count > 0)
				foreach (var member in DeclSpace.members.Values)
					switch (member.Kind)
					{
						case MemberKind.Variable:
							{
								var variable = (Variable)member;
								if ((variable.ReadCount == 0 || variable.AssignmentCount == 0) &&
									!variable.IsParameter && variable.VariableKind != VariableKind.IterationVariable)
								{
									string warning;
									if (variable.ReadCount == 0 && variable.AssignmentCount == 0)
										warning = "The variable '{0}' is never used.";
									else if (variable.ReadCount == 0)
										warning = "The variable '{0}' is assigned but its value is never used.";
									else // variable.AssignmentCount == 0
										warning = "The variable '{0}' is read but never assigned. It will always have the default value null.";
									
									document.Compiler.Warning(string.Format(warning, variable.Name),
										CompilerVerbosity.NotVerbose,
										MessageLocation.FromNode(variable.Node));
								}
							}
							break;
						case MemberKind.LocalConstant:
						case MemberKind.LocalFunction:
							if (member.ReadCount == 0)
							{
								document.Compiler.Warning(string.Format("The local {0} '{1}' is declared but never used.",
									member.Kind == MemberKind.LocalFunction ? "function" : "constant",
									member.Name),
									CompilerVerbosity.NotVerbose,
									MessageLocation.FromNode(member.Node));
							}
							break;
					}
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
			if (!forGenerator)
			{
				foreach (var member in DeclSpace.members.Values)
				{
					var variable = member as Variable;
					if (variable != null && variable.IsCaptured &&
						variable.VariableKind == VariableKind.IterationVariable)
						Initializer.Add(new AssignmentExpression(
							new InstanceMemberAccess(
								new LocalVariableAccess(DeclSpace.ClosureVariable, LocalAccessKind.ClosureLocal),
								DeclSpace.ClosureClass,
								variable.CaptureField
							) { IsAssignment = true },
							new LocalVariableAccess(variable, LocalAccessKind.NonCapturing)
						) { IgnoreValue = true });
				}
			}
			else if (CanYield)
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

				// If the initializer assigns 'this' to anything, we now need to transform it to use
				// this.'<>this' instead. hasLocalVarAccess is used if DeclSpace.ClosureVariable is
				// not null, only as a way of avoiding a potentially unnecessary loop.
				bool hasLocalVarAccess = false;
				if (Initializer != null)
					for (var i = 0; i < Initializer.Count; i++)
					{
						var expr = Initializer[i];
						if (expr.Value is ThisAccess)
							expr.Value = new InstanceMemberAccess(expr.Value, genClass, genClass.ThisField);
						else if (!hasLocalVarAccess)
							hasLocalVarAccess = expr.Value is LocalVariableAccess;
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
					// To elaborate, if you have the following function:
					//    function i(x) {
					//        yield @= x;
					//    }
					// the parameter x is copied like this:
					//    i(x): x -> I.'var:x'
					//    I.moveNext(): I.'var:x' -> C.'var:x'
					// where I is the iterator class and C is the closure class.
					if (hasLocalVarAccess)
						for (var i = 0; i < Initializer.Count - 1; i++)
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
				{
					if (expr.Document != null)
						method.AppendLocation(expr.Document, expr.StartIndex, expr.EndIndex);
					expr.Compile(compiler, method);
				}

			var iMax = Statements.Count - 1;
			for (var i = 0; i <= iMax; i++)
			{
				var stmt = Statements[i];
				stmt.Compile(compiler, method);

				if (!stmt.IsEndReachable)
				{
					if (i < iMax)
					{
						var next = Statements[i + 1];
						compiler.Warning("Unreachable code detected.",
							CompilerVerbosity.NotVerbose,
							new MessageLocation(next.Document.FileName,
								next.Document.FileSource,
								next.StartIndex, next.EndIndex));
					}
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
				if (decl.Initializer != null)
					decl.FoldConstant(IsConst);
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			foreach (var decl in Declarators)
				decl.ResolveNames(context, document);
		}

		public override void DeclareNames(BlockSpace parent)
		{
			foreach (var decl in Declarators)
			{
				if (IsConst)
					parent.DeclareConstant(new LocalConstant(decl.Name, decl));
				else
				{
					if (!IsGlobal)
						decl.Variable = new Variable(decl.Name, decl);
					parent.DeclareVariable(decl.Variable);
					if (decl.Initializer != null)
						decl.Variable.AssignmentCount++;
				}
			}
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			foreach (var decl in Declarators)
				if (decl.Initializer != null)
					decl.Initializer = decl.Initializer.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (IsConst)
				return; // Don't do anything!

			foreach (var decl in Declarators)
				if (decl.Initializer != null)
				{
					method.AppendLocation(decl.Initializer);
					if (IsGlobal && decl.Variable.IsCaptured)
					{
						decl.Initializer.Compile(compiler, method); // Evaluate expression
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
						decl.Initializer.Compile(compiler, method); // Evaluate expression
						method.Append(StoreField.Create(method.Module, decl.Variable.CaptureField)); // Store value in field
					}
					else
					{
						var variable = method.GetLocal(decl.Name);

						var value = decl.Initializer;
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
			Initializer = value;
		}

		/// <summary>The name of the variable being declared.</summary>
		public string Name;

		/// <summary>The initial value of the variable. May be null, except in constant declarations.</summary>
		public Expression Initializer;

		internal Variable Variable;

		public override string ToString(int indent)
		{
			return Initializer == null ? Name : Name + " = " + Initializer.ToString(indent);
		}

		public void FoldConstant(bool isConst)
		{
			if (Initializer == null)
				return;
			Initializer = Initializer.FoldConstant();
			if (isConst && !(Initializer is ConstantExpression))
				throw new CompileTimeException(this, "The expression could not be reduced to a constant value.");
		}

		public void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			if (Initializer != null)
			{
				// If you have something like
				//    var myLambda = @(a, b) { a.method(b); };
				// then the lambda expression will be compiled to a method named
				//    λ:myLambda${1}
				// where {1} is replaced with an internal counter.
				// It is thought that this might aid in debugging.
				if (Initializer is LambdaExpression)
					((LambdaExpression)Initializer).NameHint = this.Name;
				else if (Initializer is LambdaMemberExpression)
					((LambdaMemberExpression)Initializer).NameHint = this.Name;
				Initializer = Initializer.ResolveNames(context, document, false, false);
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			Value = Value.ResolveNames(context, document, false, false);
			if (Value.IsTypeKnown(document.Compiler) &&
				!Value.GetKnownType(document.Compiler).InheritsFrom(document.Compiler.ListType))
				throw new CompileTimeException(Value,
					"The value in a parallel local variable declaration must be of type aves.List.");
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
						Document = Document,
					});
					Variables[i].AssignmentCount++; // Always initialized
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

			method.AppendLocation(Value);
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			foreach (var param in Parameters)
				param.ResolveNames(context, document);
			Body.ResolveNames(context, document, reachable);

			if (DeclSpace != null)
			{
				DeclSpace.ReadCount = 0; // References inside the function body don't count
				if (DeclSpace.Method.IsGenerator)
					document.Compiler.AddGeneratorMethod(DeclSpace.Method);
			}
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			Expression = Expression.ResolveNames(context, document, false, false);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Expression = Expression.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.AppendLocation(Expression);
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			Body.ResolveNames(context, document, reachable);
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			Condition = Condition.ResolveNames(context, document, false, false);
			base.ResolveNames(context, document, reachable);
			if (Else != null)
				Else.ResolveNames(context, document, reachable);
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
				// The condition will always be compiled next
				method.AppendLocation(Condition);

				var elseLabel = new Label("else");
				var bodyBlock = (Block)Body;
				var firstStmt = bodyBlock.Statements.Count == 0 ? null : bodyBlock.Statements[0];
				if (firstStmt is BreakStatement ? !((BreakStatement)firstStmt).IsLeave :
					firstStmt is NextStatement && !((NextStatement)firstStmt).IsLeave)
				{
					// Let's find the correct loop, yay!
					var isBreak = firstStmt is BreakStatement;
					IterationStatement.LoopState loopState;
					IterationStatement.FindLoopState(method,
						isBreak ? ((BreakStatement)firstStmt).Label : ((NextStatement)firstStmt).Label,
						out loopState);

					Condition.CompileBoolean(compiler, isBreak ? loopState.BreakLabel : loopState.NextLabel, true, method);
				}
				else
				{
					Condition.CompileBoolean(compiler, elseLabel, false, method); // Evaluate the condition

					Body.Compile(compiler, method); // Evaluate the if body
				}

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
					NextCount > 0 || // A 'next' makes the end of the body reachable
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			List = List.ResolveNames(context, document, false, false);
			base.ResolveNames(context, document, reachable); // Body
			if (Else != null)
				Else.ResolveNames(context, document, reachable);
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
						StartIndex = this.StartIndex,
						EndIndex = this.EndIndex,
						Document = Document,
					}, VariableKind.IterationVariable);
				Variables[i].AssignmentCount++;
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
				method.AppendLocation(this);
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
				method.AppendLocation(this);
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
				method.AppendLocation(List); // May change this to something else
				method.Append(loopCond); // 'next' target
				iterState.LoadIterator(compiler, method); // Load iterator
				method.Append(new CallMember(method.Module.GetStringId("moveNext"), 0)); // Call iterator.moveNext()
				method.Append(Branch.IfFalse(loopEnd)); // If iterator.moveNext() is false, end the loop
				// Otherwise, evaluate the loop body
			}
			{ // Loop body
				method.AppendLocation(this);
				iterState.UpdateCurrent(compiler, method); // Update the current iteration variables, based on iterator.current

				Body.Compile(compiler, method);

				method.AppendLocation(this);
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

			method.AppendLocation(this);
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
					method.AppendLocation(this);
					method.Append(loopNext);

					rangeState.Increment(compiler, method);
				}
				{ // Loop condition
					rangeState.LoadCounter(compiler, method); // Load counter
					rangeState.LoadHighValue(compiler, method);// Load high

					method.Append(new SimpleInstruction(Opcode.Lte)); // counter <= high

					method.Append(Branch.IfTrue(loopBody)); // Run another iteration if counter <= high
				}
				method.PopState(expected: loopState); // Exit the loop

				if (this.IsEndReachable)
				{
					method.AppendLocation(this);
					method.Append(Branch.Always(loopEnd)); // Jump past the else
				}

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

			method.AppendLocation(List);
			iterState.LoadIterator(compiler, method); // Load iterator
			method.Append(new CallMember(method.Module.GetStringId("moveNext"), 0)); // Call iterator.moveNext()
			method.Append(Branch.IfFalse(elseLabel)); // Branch to else if false

			{ // Main body
				var loopState = new LoopState(Label, loopNext, loopEnd);
				method.PushState(loopState);

				{ // Body
					method.Append(loopBody);

					method.AppendLocation(this);
					iterState.UpdateCurrent(compiler, method); // Update iteration variables

					Body.Compile(compiler, method);
				}
				{ // Loop increment
					method.AppendLocation(List);
					method.Append(loopNext);

					iterState.LoadIterator(compiler, method); // Load iterator
					method.Append(new CallMember(method.Module.GetStringId("moveNext"), 0)); // Call iterLoc.moveNext()
					method.Append(Branch.IfTrue(loopBody)); // Run another iteration if iterLoc.moveNext()
				}

				method.PopState(expected: loopState);

				if (this.IsEndReachable)
				{
					method.AppendLocation(this);
					method.Append(Branch.Always(loopEnd)); // Jump past else
				}
			}

			{ // Else
				method.Append(elseLabel);
				Else.Compile(compiler, method);
			}

			if (this.IsEndReachable)
				method.Append(loopEnd);

			iterState.Done();
		}

		private struct RangeState
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
				method.AppendLocation(Range.Low);
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
					method.AppendLocation(Range.High);
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
					method.AppendLocation(Range.Step);
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

		private struct IterState
		{
			public Variable[] Variables;
			public Compiler.UnpackAssigner[] VariableAssigners;

			public LocalVariable IterLoc;
			public Field IterField;

			public void Init(Compiler compiler, MethodBuilder method, Expression iterExpr)
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

				method.AppendLocation(iterExpr);
				if (IterField != null)
				{
					method.Append(new LoadLocal(method.GetParameter(0)));
					iterExpr.Compile(compiler, method);
					method.Append(new SimpleInstruction(Opcode.Lditer));
					method.Append(StoreField.Create(method.Module, IterField));
				}
				else
				{
					IterLoc = method.GetAnonymousLocal();
					iterExpr.Compile(compiler, method);
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			Condition = Condition.ResolveNames(context, document, false, false);
			base.ResolveNames(context, document, reachable); // Body
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
			{
				method.AppendLocation(this);
				method.Append(Branch.Always(loopCond)); // Branch to condition if non-constant
			}
			{ // Body
				method.Append(loopBody);
				Body.Compile(compiler, method); // Evaluate the body
			}
			{ // Condition
				method.AppendLocation(Condition);
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			base.ResolveNames(context, document, reachable); // Body
			if (Condition != null)
				Condition = Condition.ResolveNames(context, document, false, false);
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
				method.AppendLocation(Condition);
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
				// The end of the try statement is reachable if both of the following are true:
				//   1. The end of the try clause is reachable or the end of any
				//      catch clause is reachable.
				//   2. There is no finally clause, or the end of it is reachable.
				if (!isEndReachable.HasValue)
					isEndReachable = (Body.IsEndReachable ||
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			base.ResolveNames(context, document, reachable); // Body
			foreach (var @catch in Catches)
				@catch.ResolveNames(context, document, reachable);

			// It is illegal for a catch clause to specify a type
			// derived from the type of any preceding catch clause,
			// or the same type as a preceding catch clause.
			// For example, this is invalid:
			//    try { }
			//    catch Error { }
			//    catch ArgumentError { } // Nope: ArgumentError derives from Error
			// but this is fine:
			//    try { }
			//    catch ArgumentError { }
			//    catch Error { }
			// This is because catch clauses are examined in lexical
			// order. Without this restriction, the ArgumentError catch
			// clause in the first example would be unreachable.
			if (Catches.Count > 1)
			{
				int max = Catches[Catches.Count - 1] is SpecificCatchClause ?
					Catches.Count : // The last catch clause has a type, process it
					Catches.Count - 1; // The last catch clause is generic, ignore it
				for (var i = 1; i < max; i++)
				{
					var catchType = ((SpecificCatchClause)Catches[i]).Type.Type;
					for (var k = 0; k < i; k++)
						if (catchType.InheritsFrom(((SpecificCatchClause)Catches[k]).Type.Type))
							throw new CompileTimeException(Catches[i],
								"The type in a catch clause cannot be derived from the type in a preceding catch clause.");
				}
			}

			if (Finally != null)
				Finally.ResolveNames(context, document, reachable);
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			context.GetContainingNamespace().ResolveTypeName(Type, document);
			if (!Type.Type.InheritsFrom(document.Compiler.ErrorType))
				throw new CompileTimeException(Type, "The type in a catch clause must inherit from aves.Error.");
			base.ResolveNames(context, document, reachable); // Body
		}

		public override void DeclareNames(BlockSpace parent)
		{
			base.DeclareNames(parent); // Body

			if (Variable != null)
			{
				var variable = new Variable(Variable,
					new VariableDeclarator(Variable, null)
					{
						StartIndex = this.StartIndex,
						EndIndex = this.EndIndex,
						Document = Document,
					}, VariableKind.CatchVariable);
				variable.AssignmentCount++; // Catch variables start out assigned
				BodyBlock.DeclareVariable(variable);
			}
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
			for (var i = 0; i < ReturnValues.Count; i++)
				ReturnValues[i] = ReturnValues[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			var block = context as BlockSpace;
			if (block != null)
			{
				if (ReturnValues.Count > 0 && block.Method is ClassMemberMethod)
				{
					var member = ((ClassMemberMethod)block.Method).Owner;
					string error = null;

					if (member.Kind == MemberKind.Constructor)
						error = "Static and instance constructors cannot return any values.";
					else if (member.Kind == MemberKind.PropertySetter ||
						member.Kind == MemberKind.IndexerSetter)
						error = "Property and indexer setters cannot return any values.";

					if (error != null)
						throw new CompileTimeException(this, error);
				}
				else if (block.Method == document.Compiler.MainMethod)
					throw new CompileTimeException(this, "Cannot return from the top level of a script.");

				if (block.IsInsideFinally())
					throw new CompileTimeException(this, "Cannot return inside a finally clause.");

				block.Method.AddReturn(this);
			}

			for (var i = 0; i < ReturnValues.Count; i++)
				ReturnValues[i] = ReturnValues[i].ResolveNames(context, document, false, false);
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
			method.AppendLocation(this);

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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			var block = context as BlockSpace;
			if (block == null)
				throw new CompileTimeException(this, "'yield' encountered outside block.");

			if (block.Method is ClassMemberMethod)
			{
				var member = ((ClassMemberMethod)block.Method).Owner;
				if (member.Kind == MemberKind.Constructor ||
					member.Kind == MemberKind.PropertySetter ||
					member.Kind == MemberKind.IndexerSetter ||
					member.Kind == MemberKind.Operator)
					throw new CompileTimeException(this,
						"Constructors, property setters, indexer setters and operators cannot be generator methods.");
			}
			else if (block.Method == document.Compiler.MainMethod)
				throw new CompileTimeException(this, "Cannot yield from the top level of a script.");

			if (block.IsInsideProtectedBlock())
				throw new CompileTimeException(this, "Cannot yield inside a try, catch or finally.");

			block.Method.AddYield(this);

			for (var i = 0; i < ReturnValues.Count; i++)
				ReturnValues[i] = ReturnValues[i].ResolveNames(context, document, false, false);
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

			method.AppendLocation(this);

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
		internal bool IsLeave = false;

		public override bool IsEndReachable { get { return false; } }

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (Label == null ? "next;" : "next " + Label + ";");
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			// Verify that there is a loop to refer to.
			if (context is BlockSpace)
			{
				parent = (BlockSpace)context;
				var loop = parent.FindLoop(this, Label, out IsLeave);

				if (loop is DoWhileStatement && ((DoWhileStatement)loop).Condition == null)
					throw new CompileTimeException(this, "A 'next' statement may not refer to a 'do {...};' statement. Use 'break' instead.");

				if (reachable)
					loop.NextCount++;
			}
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.AppendLocation(this);
			IterationStatement.LoopState loop;
			IterationStatement.FindLoopState(method, Label, out loop);
			if (IsLeave)
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
		internal bool IsLeave = false;

		public override bool IsEndReachable { get { return false; } }

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (Label == null ? "break;" : "break " + Label + ";");
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			if (context is BlockSpace)
			{
				parent = (BlockSpace)context;
				var loop = parent.FindLoop(this, Label, out IsLeave);

				if (reachable)
					loop.BreakCount++;
			}
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.AppendLocation(this);
			IterationStatement.LoopState loop;
			IterationStatement.FindLoopState(method, Label, out loop);
			if (IsLeave)
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
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
			{
				Value = Value.ResolveNames(context, document, false, false);
				if (Value.IsTypeKnown(document.Compiler) &&
					!Value.GetKnownType(document.Compiler).InheritsFrom(document.Compiler.ErrorType))
					throw new CompileTimeException(this,
						"If the expression in a throw statement has a known type, that type must be or derive from aves.Error.");
			}
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			if (Value != null)
				Value = Value.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.AppendLocation(this);
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			Target = Target.ResolveNames(context, document, false, false);
			AssignmentExpression.EnsureAssignable(Target);
			Value = Value.ResolveNames(context, document, false, false);
		}

		public override void TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			Target = Target.TransformClosureLocals(currentBlock, forGenerator);
			Value = Value.TransformClosureLocals(currentBlock, forGenerator);
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.AppendLocation(this);
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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			for (var i = 0; i < Targets.Count; i++)
			{
				var result = Targets[i].ResolveNames(context, document, false, false);
				AssignmentExpression.EnsureAssignable(result);
				Targets[i] = result;
			}

			for (var i = 0; i < Values.Count; i++)
				Values[i] = Values[i].ResolveNames(context, document, false, false);

			if (Values.Count == 1 && Values[0].IsTypeKnown(document.Compiler) &&
				!Values[0].GetKnownType(document.Compiler).InheritsFrom(document.Compiler.ListType))
				throw new CompileTimeException(Values[0],
					"The value in a parallel assignment must be of type aves.List.");
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
			method.AppendLocation(this);
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
					LocalVariable variable;
					if (Targets[i] is GlobalVariableAccess)
						variable = method.GetLocal(((GlobalVariableAccess)Targets[i]).Variable.Name);
					else
						variable = ((LocalVariableAccess)Targets[i]).GetLocal(method);

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
			{
				var local = e as LocalVariableAccess;
				var global = e as GlobalVariableAccess;
				if (local == null && (global == null || global.Variable.IsCaptured))
					return false; // Neither local nor non-captured global

				var variable = local != null ? local.Variable : global.Variable;
				if (!variables.Add(variable))
					return false; // Already a target, can't compile as right-to-left unpacking
			}
			return true;
		}
	}

	public sealed class ConstructorCall : Statement
	{
		public ConstructorCall(List<Expression> arguments, bool isBaseCtor)
		{
			Arguments = arguments;
			IsBaseConstructor = isBaseCtor;
		}

		/// <summary>The arguments passed into the constructor.</summary>
		public List<Expression> Arguments;

		public bool IsBaseConstructor;

		private MethodGroup constructor;

		public override string ToString(int indent)
		{
			return string.Format("{0}new {1}({2});",
				new string('\t', indent),
				IsBaseConstructor ? "base" : "this",
				Arguments.JoinString(", "));
		}

		public override void FoldConstant()
		{
			for (var i = 0; i < Arguments.Count; i++)
				Arguments[i] = Arguments[i].FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			bool _;
			var thisClass = context.GetContainingClass(out _);
			var ctorClass = thisClass;
			if (IsBaseConstructor)
			{
				if (thisClass.BaseType == null)
					throw new CompileTimeException(this, "Base initializers may not be used inside aves.Object.");
				ctorClass = (Class)thisClass.BaseType;
			}

			var ctor = ctorClass.FindConstructor(this, Arguments.Count, instClass: thisClass, fromClass: thisClass);
			if (ctor == null)
				throw new CompileTimeException(this, string.Format("Could not find a constructor for '{0}' that takes {1} arguments.",
					ctorClass.FullName, Arguments.Count));

			// Constructor calls are only allowed at the very beginning of a constructor,
			// hence 'context' really should be a constructor Method's BlockSpace here.
			if (((BlockSpace)context).Method == ctor)
				throw new CompileTimeException(this, "Constructor call cannot directly recurse into itself.");

			constructor = ctor.Group;

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
			method.AppendLocation(this);
			method.Append(new LoadLocal(method.GetParameter(0))); // Load 'this'

			foreach (var arg in Arguments) // Evaluate each argument
				arg.Compile(compiler, method);

			method.Append(new StaticCall(method.Module.GetMethodId(constructor), Arguments.Count)); // Call the base constructor
			method.Append(new SimpleInstruction(Opcode.Pop));
		}
	}
}