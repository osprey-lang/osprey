using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Osprey.Nodes;
using Osprey.Instructions;
using CI = System.Globalization.CultureInfo;
using Switch = Osprey.Instructions.Switch;

namespace Osprey.Members
{
	[DebuggerDisplay("Method {FullName} (overloads: {Count})")]
	public class MethodGroup : NamedMember, IEnumerable<Method>
	{
		public MethodGroup(string name, NamedMember parent, AccessLevel access)
			: base(name, MemberKind.MethodGroup, null, access)
		{
			Parent = parent;
		}

		public override string FullName
		{
			get
			{
				string parentName = null;
				if (Parent is Namespace)
					parentName = ((Namespace)Parent).FullName;
				else if (Parent is Class)
					parentName = ((Class)Parent).FullName;
				if (parentName != null)
					return parentName + "." + this.Name;
				return this.Name;
			}
		}

		/// <summary>Gets the module that the method group belongs to.</summary>
		public Module Module { get; internal set; }

		/// <summary>
		/// Gets the member that contains the method group. This is either a class or, for global functions, a namespace.
		/// </summary>
		public NamedMember Parent { get; internal set; }

		public Class ParentAsClass { get { return Parent as Class; } }
		public Namespace ParentAsNamespace { get { return Parent as Namespace; } }

		public bool IsStatic { get; private set; }

		/// <summary>Contains the overloads in the method group.</summary>
		private List<Method> overloads = new List<Method>();

		internal MethodGroup BaseGroup;

		/// <summary>
		/// Gets the overload at the specified index.
		/// </summary>
		/// <param name="index">The index of the overload to get.</param>
		/// <returns>The overload at the specified index.</returns>
		public Method this[int index] { get { return overloads[index]; } }

		/// <summary>
		/// Gets the total number of overloads contained in the method group.
		/// </summary>
		public int Count { get { return overloads.Count; } }

		internal uint Id;

		/// <summary>
		/// Adds an overload to the method group.
		/// </summary>
		/// <param name="method">The overload to add to the method group.</param>
		/// <exception cref="ArgumentNullException"><paramref name="method"/> is null.</exception>
		/// <exception cref="OverloadException"><paramref name="method"/> is ambiguous with another overload in the method group.</exception>
		public void AddOverload(Method method)
		{
			if (method == null)
				throw new ArgumentNullException("method");

			if (overloads.Count > 0)
			{
				if (method.IsStatic != this.IsStatic)
					throw new OverloadException(method.Node, overloads.ToArray(),
						"If one overload is marked static, then all overloads must be.");

				foreach (var overload in overloads)
					if (overload == method)
						return; // already in the method group
					else if (overload.Signature.IsAmbiguous(method.Signature))
						throw new OverloadException(method.Node, overload, method);
			}

			this.IsStatic = method.IsStatic;
			overloads.Add(method);
			method.Group = this;
		}

		/// <summary>
		/// Finds the overload that matches the specified number of arguments, or null if none could be found.
		/// </summary>
		/// <param name="argCount">The number of arguments that the method group is being invoked with.</param>
		/// <returns>The <see cref="Method"/> instances that can be invoked with the specified number of arguments, or null if none could be found.</returns>
		/// <exception cref="ArgumentOutOfRangeException"><paramref name="argCount"/> is less than zero.</exception>
		public Method FindOverload(int argCount, bool lookInBase = false)
		{
			if (argCount < 0)
				throw new ArgumentOutOfRangeException("argCount");

			foreach (var method in overloads)
				if (method.Accepts(argCount))
					return method;

			if (lookInBase && BaseGroup != null)
				return BaseGroup.FindOverload(argCount, true);

			return null;
		}

		internal int IndexOfOverload(Method method)
		{
			return overloads.IndexOf(method);
		}

		/// <summary>
		/// Finds the overload that matches the specified signature exactly.
		/// </summary>
		/// <param name="signature">The signature to match against.</param>
		/// <returns>The overload that matches the signature, or null if none could be found.</returns>
		public Method FindOverload(Signature signature, bool lookInBase = false)
		{
			foreach (var method in overloads)
				if (method.Signature == signature)
					return method;

			if (lookInBase && BaseGroup != null)
				return BaseGroup.FindOverload(signature, true);

			return null;
		}

		public bool CanDeclare(Signature signature)
		{
			foreach (var overload in overloads)
				if (overload.Signature.IsAmbiguous(signature))
					return false;
			return true;
		}

		public override Namespace GetContainingNamespace()
		{
			if (Parent.Kind == MemberKind.Namespace)
				return (Namespace)Parent;
			else
				return ((Class)Parent).GetContainingNamespace();
		}

		public List<Method>.Enumerator GetEnumerator()
		{
			return overloads.GetEnumerator();
		}

		IEnumerator<Method> IEnumerable<Method>.GetEnumerator()
		{
			return overloads.GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return overloads.GetEnumerator();
		}
	}

	public class Method : NamedMember, IDeclarationSpace
	{
		/// <summary>
		/// Initializes a new <see cref="Method"/> instance.
		/// </summary>
		/// <param name="name">The name of the method.</param>
		/// <param name="node">The node that declares the method.</param>
		public Method(MethodDeclaration node)
			: base(node.DeclaredName, MemberKind.Method, node, node.Access)
		{
			// Note: node.Body can be a Block or EmptyStatement.
			// If it's the latter, then node.Body as Block is null,
			// which is exactly what I want.
			this.body = BlockSpace.FromStatement(node.Body, this);

			Signature = new Signature(node.Parameters, node.Splat);

			foreach (var param in node.Parameters)
				body.DeclareVariable(new Variable(param.DeclaredName, param));

			Parameters = node.Parameters.ToArray();

			IsStatic = node.IsStatic;
			IsAbstract = node.IsAbstract;
			IsOverride = node.IsOverride;
			IsOverridable = node.IsOverridable;

			node.DeclSpace = this;
		}
		public Method(GlobalFunctionDeclaration node)
			: base(node.Function.Name, MemberKind.Method, node, node.IsPublic ? AccessLevel.Public : AccessLevel.None)
		{
			var func = node.Function;
			this.body = BlockSpace.FromStatement(func.Body, this);

			Signature = new Signature(func.Parameters, func.Splat);

			foreach (var param in func.Parameters)
				body.DeclareVariable(new Variable(param.DeclaredName, param));

			Parameters = func.Parameters.ToArray();

			IsStatic = true;
			IsAbstract = false;
			IsOverride = false;
			IsOverridable = false;

			node.DeclSpace = this;
		}
		public Method(ParseNode node, string name, AccessLevel access, Statement body, Splat splat, params Parameter[] parameters)
			: base(name, MemberKind.Method, node, access)
		{
			this.body = BlockSpace.FromStatement(body, this);

			if (parameters != null)
			{
				Signature = new Signature(parameters, splat);

				foreach (var param in parameters)
				{
					this.body.DeclareVariable(new Variable(param.DeclaredName, param));
					if (param is ConstructorParam && ((ConstructorParam)param).HasThisPrefix)
						this.body.ReserveName(param.Name, ReserveReason.UsedAsThisParameter);
				}

				Parameters = parameters;
			}
			else
			{
				Signature = new Signature(0, 0, splat);
				Parameters = new Parameter[0];
			}
		}
		public Method(ParseNode node, string name, AccessLevel access, Statement body, Signature signature)
			: base(name, MemberKind.Method, node, access)
		{
			this.body = BlockSpace.FromStatement(body, this);
			Signature = signature;
		}

		/// <summary>Gets the group to which the method belongs.</summary>
		public MethodGroup Group { get; internal set; }

		public override string FullName { get { return Group.FullName; } }

		/// <summary>Gets the module that declares the method.</summary>
		public Module Module { get; internal set; }

		private BlockSpace body;
		/// <summary>Gets the body of the method.</summary>
		public BlockSpace Body { get { return body; } }

		public Signature Signature { get; private set; }

		private MemberFlags flags;

		public MemberFlags Flags { get { return flags; } internal set { flags = value; } }

		internal Parameter[] Parameters;

		internal bool HasRefParams { get { return Parameters != null && Parameters.Any(p => p.IsByRef); } }

		internal Method OverriddenBaseMethod;

		internal CompiledMethodData CompiledMethod;

		public bool IsStatic
		{
			get { return (flags & MemberFlags.Instance) != MemberFlags.Instance; }
			internal set { ToggleFlag(MemberFlags.Instance, !value); }
		}
		public bool IsAbstract
		{
			get { return (flags & MemberFlags.Abstract) == MemberFlags.Abstract; }
			internal set { ToggleFlag(MemberFlags.Abstract, value); }
		}
		public bool IsOverride
		{
			get { return (flags & MemberFlags.Override) == MemberFlags.Override; }
			internal set { ToggleFlag(MemberFlags.Override, value); }
		}
		public bool IsOverridable
		{
			get { return (flags & MemberFlags.Overridable) == MemberFlags.Overridable; }
			internal set { ToggleFlag(MemberFlags.Overridable, value); }
		}
		public bool IsImplDetail
		{
			get { return (flags & MemberFlags.ImplDetail) == MemberFlags.ImplDetail; }
			internal set { ToggleFlag(MemberFlags.ImplDetail, value); }
		}
		internal bool IsAutoOverride
		{
			get { return (flags & MemberFlags.AutoOverride) == MemberFlags.AutoOverride; }
			set { ToggleFlag(MemberFlags.AutoOverride, value); }
		}

		public bool IsVirtualCall
		{
			get { return IsAbstract || IsOverridable || IsOverride; }
		}

		internal List<ReturnStatement> Returns;
		internal List<YieldStatement> Yields;

		public bool IsGenerator { get { return Yields != null && Yields.Count > 0; } }

		internal int LambdaNameCounter = 0;
		internal int LambdaParamCounter = 0;
		internal int ClosureCounter = 0;

		internal List<LocalFunction> LocalFunctions;
		
		internal bool HasLocalFunctions { get { return LocalFunctions != null && LocalFunctions.Count > 0; } }

		private GeneratorClass genClass;
		public GeneratorClass GeneratorClass { get { return genClass; } }

		/// <summary>
		/// Determines whether the method accepts the specified number of arguments.
		/// </summary>
		/// <param name="argCount">The number of arguments to test against.</param>
		/// <returns>True if the method can be invoked with <paramref name="argCount"/> arguments; otherwise, false.</returns>
		public bool Accepts(int argCount)
		{
			return Signature.Accepts(argCount);
		}

		private void ToggleFlag(MemberFlags flag, bool on)
		{
			if (on)
				flags |= flag;
			else
				flags &= ~flag;
		}

		internal void InitBody(Compiler compiler)
		{
			if (body is ExternBlockSpace)
			{
				var entryPoint = ((ExternBlockSpace)body).EntryPoint;

				compiler.EnsureNativeMethodExists(this.body.Node, entryPoint);

				// Note: we don't actually store the procedure address anywhere.
				// Extern methods are linked by name, not by address.
			}
			else if (body.Node != null)
				body.Node.DeclareNames(null);
		}

		internal void AddLocalFunction(LocalFunction function)
		{
			if (LocalFunctions == null)
				LocalFunctions = new List<LocalFunction>();
			LocalFunctions.Add(function);
		}

		internal void AddReturn(ReturnStatement value)
		{
			if (value.ReturnValues.Length > 0 && Yields != null)
				throw new CompileTimeException(value,
					"Generator methods may only contain empty return statements.");

			if (Returns == null)
				Returns = new List<ReturnStatement>();
			Returns.Add(value);
		}

		internal void AddYield(YieldStatement value)
		{
			Func<ReturnStatement, bool> hasReturnValue = stmt => stmt.ReturnValues.Length > 0;

			if (Returns != null && Returns.Any(hasReturnValue))
				throw new CompileTimeException(Returns.First(hasReturnValue),
					"Generator methods may only contain empty return statements.");

			if (Yields == null)
				Yields = new List<YieldStatement>();

			Yields.Add(value);
		}

		internal void VerifyArgumentRefness(Expression[] args)
		{
			if (Signature.Splat == Splat.Beginning)
			{
				// Parameters that get packed into a list must be by value
				var ia = 0; // Argument index
				while (ia < args.Length - Signature.ParameterCount + 1)
				{
					if (args[ia] is RefExpression)
						ErrorWrongRefness(ia, args[ia], false);
					ia++;
				}

				// But required parameters may be by reference
				var ip = 1; // Parameter index (param 0 is the variadic param)
				while (ia < args.Length)
				{
					if ((args[ia] is RefExpression) != Parameters[ip].IsByRef)
						ErrorWrongRefness(ia, args[ia], Parameters[ip].IsByRef);
					ia++;
					ip++;
				}
			}
			else if (Signature.Splat == Splat.End)
			{
				var i = 0;
				// Required parameters may be by reference
				while (i < Signature.ParameterCount - 1)
				{
					if ((args[i] is RefExpression) != Parameters[i].IsByRef)
						ErrorWrongRefness(i, args[i], Parameters[i].IsByRef);
					i++;
				}
				// But not the parameters that get packed into a list
				while (i < args.Length)
				{
					if (args[i] is RefExpression)
						ErrorWrongRefness(i, args[i], false);
					i++;
				}
			}
			else
			{
				// Skip missing arguments; if we've found this overload
				// through normal overload resolution, then those arguments
				// must be optional (and optional params are never by ref).
				var max = Math.Min(args.Length, Signature.ParameterCount);
				for (var i = 0; i < max; i++)
					if ((args[i] is RefExpression) != Parameters[i].IsByRef)
						ErrorWrongRefness(i, args[i], Parameters[i].IsByRef);
			}
		}

		private void ErrorWrongRefness(int argIndex, Expression arg, bool shouldBeRef)
		{
			throw new CompileTimeException(arg, string.Format("Argument {0} to '{1}' must be passed by {2}.",
				argIndex + 1, this.FullName, shouldBeRef ? "reference" : "value"));
		}

		internal virtual void Compile(Compiler compiler)
		{
			if (IsAbstract)
				return; // Nothing to do!
			if (Body is ExternBlockSpace)
			{
				var externBody = (ExternBody)(Body.Node);
				CompiledMethod = new CompiledMethodData
				{
					LocalCount = (int)((ConstantExpression)externBody.Locals).Value.IntValue,
					MaxStack = (int)((ConstantExpression)externBody.MaxStack).Value.IntValue,
				};
				return; // No body to initialize
			}

			var mb = new MethodBuilder(!IsStatic, Parameters, compiler.OutputModule, compiler.UseDebugSymbols);
			if (Signature.Splat == Splat.None && Signature.OptionalParameterCount > 0)
				CompileOptionalParams(compiler, mb);

			if (IsGenerator)
				CompileGenerator(compiler, mb);
			else
			{
				var bodyNode = Body.Node;
				bodyNode.Compile(compiler, mb);
				if (bodyNode.IsEndReachable)
				{
					// If the end of the method is reachable, then it must mean that the method
					// uses curly braces for its body; otherwise, if it's a lambda like @= E or
					// @.blah, or a getter declared as "get x = ...;", there is always an implicit
					// return at the end of the body.
					// Hence, we can safely make the following retnull instruction belong to that
					// curly brace. Note that the main method has a node with a null document.
					if (bodyNode.Document != null)
						mb.PushLocation(bodyNode.Document.SourceFile, bodyNode.EndIndex, bodyNode.EndIndex + 1);
					mb.Append(new SimpleInstruction(Opcode.Retnull));
					if (bodyNode.Document != null)
						mb.PopLocation();
				}
			}

			var body = mb.GetBodyBytes();
			CompiledMethod = new CompiledMethodData(
				bodyOffset: compiler.OutputModule.AppendMethodBody(body),
				bodyLength: body.Length,
				localCount: mb.LocalCount,
				maxStack: mb.GetMaxStack(),
				tryBlocks: mb.GetTryBlocks(),
				debugSymbols: mb.GetDebugSymbols()
			);
		}

		private void CompileOptionalParams(Compiler compiler, MethodBuilder builder)
		{
			// We compile optional parameters by outputting a switch that
			// initializes parameters based on the argument count.

			// All missing parameters are auto-initialized to null when the method is called,
			// hence we only need to output a switch for those optional parameters whose
			// default value is not null.
			var firstOptionalNonNull = -1; // The index of the first optional parameter with a non-null default value
			var lastOptionalNonNull = -1; // Same as above, but the last such parameter
			for (var i = 0; i < Parameters.Length; i++)
				if (Parameters[i].DefaultValue != null &&
					!Parameters[i].DefaultValue.IsNull)
				{
					if (firstOptionalNonNull == -1)
						firstOptionalNonNull = i;
					lastOptionalNonNull = i;
				}
			if (firstOptionalNonNull == -1)
				return;

			// Calculate the total number of parameters that do not require initialization,
			// including the instance if there is one.
			var offset = IsStatic ? 0 : 1; // 'this' is a required, unnamed parameter
			var uninitializedParams = firstOptionalNonNull + offset;

			builder.PushLocation(this.Node);
			builder.Append(new SimpleInstruction(Opcode.Ldargc)); // Load argument count
			if (uninitializedParams != 0)
			{
				// Make sure top of stack is 0 when all optional parameters are missing
				builder.Append(new LoadConstantInt(uninitializedParams));
				builder.Append(new SimpleInstruction(Opcode.Sub));
			}

			var endLabel = new Label();

			var optionalCount = lastOptionalNonNull - firstOptionalNonNull + 1;
			var jumpTargets = new List<Label>(optionalCount);
			for (var i = 0; i < optionalCount; i++)
				jumpTargets.Add(new Label());

			builder.Append(new Switch(jumpTargets)); // Jump to the first unassigned parameter
			builder.Append(Branch.Always(endLabel)); // No missing parameters

			for (var i = 0; i < optionalCount; i++)
			{
				builder.Append(jumpTargets[i]); // The label for this parameter

				var index = firstOptionalNonNull + i;
				var param = Parameters[index];
				if (param.DefaultValue.IsNull)
					continue;

				builder.PushLocation(param.DefaultValue);
				param.DefaultValue.Compile(compiler, builder); // Evaluate the value
				builder.Append(new StoreLocal(builder.GetParameter(index + offset))); // Store!
				builder.PopLocation(); // param.DefaultValue
				// Fall through to the next parameter, which is also missing
			}
			builder.PopLocation(); // this.Node

			builder.Append(endLabel);
		}

		private void CompileGenerator(Compiler compiler, MethodBuilder builder)
		{
			var stateField = ((GeneratorClass)this.Group.ParentAsClass).StateField;

			var stateSwitch = new Switch(); // The target list will be updated later

			var bodyNode = Body.Node;
			// During generator setup, the cursor is at the "{" of the body
			// Note: it is impossible to have a generator block without curlies.
			builder.PushLocation(bodyNode.Document.SourceFile, bodyNode.StartIndex, bodyNode.StartIndex + 1);

			builder.Append(new LoadLocal(builder.GetParameter(0))); // Load this
			builder.Append(LoadField.Create(builder.Module, stateField)); // Load this.'<>state'
			builder.Append(stateSwitch); // Switch on this.'<>state'

			builder.PopLocation(); // '{'

			// This list will be repopulated when traversing all the statements,
			// to make sure it only contains reachable yields.
			Yields.Clear();
			var canEnd = bodyNode.IsEndReachable || bodyNode.CanReturn;
			if (canEnd)
				Yields.Add(null); // End label placeholder thing

			bodyNode.Compile(compiler, builder);

			var endLabel = canEnd ? new Label("generator-end") : null;
			stateSwitch.SetTargets(Yields.Select(y => y == null ? endLabel : y.StateLabel));

			if (canEnd)
			{
				// The last step appears to be at the closing "}" of the body
				builder.PushLocation(bodyNode.Document.SourceFile, bodyNode.EndIndex, bodyNode.EndIndex + 1);

				if (bodyNode.IsEndReachable)
				{
					builder.Append(new LoadLocal(builder.GetParameter(0))); // Load this
					builder.Append(new LoadConstantInt(0)); // Load the end state
					builder.Append(StoreField.Create(builder.Module, stateField)); // Store value in this.'<>state'
				}

				builder.Append(endLabel);
				builder.Append(LoadConstant.False());
				builder.Append(new SimpleInstruction(Opcode.Ret));

				builder.PopLocation(); // '}'
			}
		}

		IDeclarationSpace IDeclarationSpace.Parent { get { return Group == null ? null : (IDeclarationSpace)Group.Parent; } }

		bool IDeclarationSpace.ContainsMember(string name)
		{
			return Group != null && ((IDeclarationSpace)Group.Parent).ContainsMember(name);
		}

		NamedMember IDeclarationSpace.ResolveName(string name, Class fromClass)
		{
			if (Group != null)
				return ((IDeclarationSpace)Group.Parent).ResolveName(name, fromClass);
			return null;
		}

		public override Namespace GetContainingNamespace()
		{
			return Group.GetContainingNamespace();
		}

		public virtual Class GetContainingClass(out bool hasInstance)
		{
			if (Group == null)
			{
				hasInstance = false;
				return null;
			}
			hasInstance = !Group.IsStatic;
			return Group.ParentAsClass;
		}

		internal GeneratorClass GenerateGeneratorClass(Compiler compiler)
		{
			if (genClass != null)
				return genClass;

			bool _;
			var @class = GetContainingClass(out _);
			var ns = GetContainingNamespace();
			string namePrefix = @class != null ? @class.Name + "/" : "";

			genClass = new GeneratorClass(GetGeneratorClassName(namePrefix), this, ns, compiler);
			genClass.SharedType = @class;

			var moveNext = new Method(this.Node, "moveNext", AccessLevel.Public, null, Signature.Empty)
			{
				IsStatic = false,
				IsOverride = true,
			};
			moveNext.body = this.body; // Copy the entire body across, yep
			genClass.DeclareMethod(moveNext);

			var needThisField = this is LocalMethod && ((LocalMethod)this).Function.CapturesThis || !this.IsStatic;
			if (needThisField)
				genClass.DeclareThisField();

			body.Node.TransformClosureLocals(null, forGenerator: true);

			// Note: this replaces this.body entirely!
			TransformToIteratorGenerator(genClass, needThisField);

			moveNext.Returns = this.Returns;
			moveNext.Yields = this.Yields;
			this.Returns = null;
			this.Yields = null;

			return genClass;
		}

		private void TransformToIteratorGenerator(GeneratorClass genClass, bool hasThisField)
		{
			var ctor = genClass.FindConstructor(null, 0, genClass, Group.ParentAsClass);
			Block block;
			if (hasThisField || Parameters.Length > 0)
			{
				var generatorLocal = new Variable("<generator>", (VariableDeclarator)null);

				var body = new TempList<Statement>();
				body.Add(new ExpressionStatement(new AssignmentExpression(
					new LocalVariableAccess(generatorLocal, LocalAccessKind.NonCapturing) { IsAssignment = true },
					new ObjectCreationExpression(null, EmptyArrays.Expressions, false) { Constructor = ctor }
				) { IgnoreValue = true }));

				if (hasThisField)
					body.Add(new ExpressionStatement(new AssignmentExpression(
						new InstanceMemberAccess(new LocalVariableAccess(generatorLocal, LocalAccessKind.NonCapturing),
							genClass, genClass.ThisField) { IsAssignment = true },
						new ThisAccess()
					) { IgnoreValue = true }));

				for (var i = 0; i < Parameters.Length; i++)
				{
					var paramVar = (Variable)this.body.members[Parameters[i].DeclaredName];
					body.Add(new ExpressionStatement(new AssignmentExpression(
						new InstanceMemberAccess(new LocalVariableAccess(generatorLocal, LocalAccessKind.NonCapturing),
							genClass, paramVar.CaptureField) { IsAssignment = true },
						new LocalVariableAccess(paramVar, LocalAccessKind.NonCapturing)
					) { IgnoreValue = true }));
				}

				body.Add(new ReturnStatement(new LocalVariableAccess(generatorLocal, LocalAccessKind.NonCapturing)));

				block = new Block(body.ToArray());
			}
			else
			{
				block = new Block(new Statement[]
				{
					new ReturnStatement(new ObjectCreationExpression(null, EmptyArrays.Expressions, false) { Constructor = ctor })
				});
			}

			this.body = new BlockSpace(block, this);
		}

		internal virtual string GetLambdaName(string nameHint)
		{
			return string.Format("λ${0}!{1}",
				nameHint ?? "__",
				LambdaNameCounter++.ToStringInvariant());
		}

		internal virtual string GetLambdaParam()
		{
			return string.Format("λ$arg${0}",
				LambdaParamCounter++.ToStringInvariant());
		}

		internal virtual string GetClosureClassName(string prefix, out int blockNumber)
		{
			var overloadIndex = Group.Count == 1 ?
				"" :
				"!" + Group.IndexOfOverload(this).ToStringInvariant();
			blockNumber = ClosureCounter++;
			return string.Format("C${0}{1}{2}__{3}",
				prefix,
				Name.Replace('.', '#'),
				overloadIndex,
				blockNumber.ToStringInvariant());
		}

		internal virtual string GetGeneratorClassName(string prefix)
		{
			var overloadIndex = Group.Count == 1 ?
				"" :
				"!" + Group.IndexOfOverload(this).ToStringInvariant();
			var classNumber = ClosureCounter++;
			return string.Format("I${0}{1}{2}__{3}",
				prefix,
				Name.Replace('.', '#'),
				overloadIndex,
				classNumber.ToStringInvariant());
		}

		internal class CompiledMethodData
		{
			internal CompiledMethodData() { }
			internal CompiledMethodData(uint bodyOffset, int bodyLength,
				int localCount, int maxStack, TryBlock[] tryBlocks,
				SourceLocation[] debugSymbols)
			{
				BodyOffset = bodyOffset;
				BodyLength = bodyLength;
				LocalCount = localCount;
				MaxStack = maxStack;
				TryBlocks = tryBlocks;
				DebugSymbols = debugSymbols;
			}

			/// <summary>
			/// The offset of the first instruction of the method body,
			/// within the module's method block.
			/// </summary>
			internal uint BodyOffset;
			/// <summary>
			/// The total number of bytes taken up by the compiled body.
			/// </summary>
			internal int BodyLength;
			/// <summary>
			/// The total number of managed locals required by the method.
			/// </summary>
			internal int LocalCount;
			/// <summary>
			/// The maximum number of stack slots used by the method.
			/// </summary>
			internal int MaxStack;
			/// <summary>
			/// The try blocks associated with the method.
			/// </summary>
			internal TryBlock[] TryBlocks;
			/// <summary>
			/// The debug symbols associated with the method.
			/// </summary>
			internal SourceLocation[] DebugSymbols;
		}
	}

	internal class BytecodeMethod : Method
	{
		public BytecodeMethod(string name, AccessLevel access, Splat splat, params Parameter[] parameters)
			: base(null, name, access, null, new Signature(parameters, splat))
		{
			if (parameters != null)
				this.Parameters = (Parameter[])parameters.Clone();
			else
				this.Parameters = EmptyArrays.Parameters;
		}

		// Should only contain labels and instructions
		private List<object> contents = new List<object>();

		public void Append(Instruction instr)
		{
			if (instr == null)
				throw new ArgumentNullException("instr");

			contents.Add(instr);
		}
		public void Append(Label label)
		{
			if (label == null)
				throw new ArgumentNullException("label");

			contents.Add(label);
		}

		internal override void Compile(Compiler compiler)
		{
			var mb = new MethodBuilder(!IsStatic, Parameters, compiler.OutputModule, false);

			foreach (var obj in contents)
				if (obj is Label)
					mb.Append((Label)obj);
				else
					mb.Append((Instruction)obj);

			var body = mb.GetBodyBytes();
			CompiledMethod = new CompiledMethodData(
				bodyOffset: compiler.OutputModule.AppendMethodBody(body),
				bodyLength: body.Length,
				localCount: mb.LocalCount,
				maxStack: mb.GetMaxStack(),
				tryBlocks: mb.GetTryBlocks(),
				debugSymbols: null
			);
		}
	}

	/// <summary>
	/// Flags applied to a member. Not all flags are applicable to all kinds of members.
	/// </summary>
	[Flags]
	public enum MemberFlags
	{
		/// <summary>
		/// Specifies no flags.
		/// </summary>
		None = 0,
		/// <summary>
		/// The member is an instance method.
		/// </summary>
		Instance = 0x0001,
		/// <summary>
		/// The member is abstract.
		/// </summary>
		Abstract = 0x0002,
		/// <summary>
		/// The member is an override.
		/// </summary>
		Override = 0x0004,
		/// <summary>
		/// The member is overridable.
		/// </summary>
		Overridable = 0x0008,
		/// <summary>
		/// The member is a constructor.
		/// </summary>
		Constructor = 0x0010,
		/// <summary>
		/// The member is an instance constructor. This flag sets <see cref="Constructor"/> and <see cref="Instance"/>.
		/// </summary>
		InstanceConstructor = Constructor | Instance,
		/// <summary>
		/// The member is an implementation detail, such as an auto-generated property accessor method, or an auto-generated field.
		/// </summary>
		ImplDetail = 0x0020,
		/// <summary>
		/// (Internal use only) The member automatically overrides the base member, even if the override modifier is not used.
		/// </summary>
		AutoOverride = 0x0040,
		/// <summary>
		/// The member is an operator overload.
		/// </summary>
		Operator = 0x0080,
	}
}