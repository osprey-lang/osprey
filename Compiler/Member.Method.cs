using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Osprey.Nodes;
using Osprey.Instructions;
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
		public Method(string name, AccessLevel access, Statement body, Splat splat, params Parameter[] parameters)
			: base(name, MemberKind.Method, null, access)
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
		public Method(string name, AccessLevel access, Statement body, Signature signature)
			: base(name, MemberKind.Method, null, access)
		{
			this.body = BlockSpace.FromStatement(body, this);
			Signature = signature;
		}

		/// <summary>Gets the group to which the method belongs.</summary>
		public MethodGroup Group { get; internal set; }

		/// <summary>Gets the module that declares the method.</summary>
		public Module Module { get; internal set; }

		private BlockSpace body;
		/// <summary>Gets the body of the method.</summary>
		public BlockSpace Body { get { return body; } }

		public Signature Signature { get; private set; }

		private MemberFlags flags;

		public MemberFlags Flags { get { return flags; } internal set { flags = value; } }

		internal Parameter[] Parameters;

		internal Method OverriddenBaseMethod;

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
			if (value.ReturnValues.Count > 0 && Yields != null)
				throw new CompileTimeException(value,
					"Generator methods may only contain empty return statements.");

			if (Returns == null)
				Returns = new List<ReturnStatement>();
			Returns.Add(value);
		}

		internal void AddYield(YieldStatement value)
		{
			Func<ReturnStatement, bool> hasReturnValue = stmt => stmt.ReturnValues.Count > 0;

			if (Returns != null && Returns.Any(hasReturnValue))
				throw new CompileTimeException(Returns.First(hasReturnValue),
					"Generator methods may only contain empty return statements.");

			if (Yields == null)
				Yields = new List<YieldStatement>();

			Yields.Add(value);
		}

		internal virtual void Compile(Compiler compiler)
		{
			if (IsAbstract)
				return; // Nothing to do!
			if (Body is ExternBlockSpace)
			{
				var externBody = (ExternBody)(Body.Node);
				LocalCount = (int)((ConstantExpression)externBody.Locals).Value.IntValue;
				MaxStack = (int)((ConstantExpression)externBody.MaxStack).Value.IntValue;
				return; // No body to initialize
			}

			var mb = new MethodBuilder(!IsStatic, Parameters, compiler.OutputModule);
			if (Signature.Splat == Splat.None && Signature.OptionalParameterCount > 0)
				CompileOptionalParams(compiler, mb);

			if (IsGenerator)
				CompileGenerator(compiler, mb);
			else
			{
				Body.Node.Compile(compiler, mb);
				if (Body.Node.IsEndReachable)
					mb.Append(new SimpleInstruction(Opcode.Retnull));
			}

			var body = mb.GetBodyBytes();
			BodyOffset = compiler.OutputModule.AppendMethodBody(body);
			BodyLength = body.Length;
			LocalCount = mb.LocalCount;
			MaxStack = mb.GetMaxStack();
			TryBlocks = mb.GetTryBlocks();
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
				jumpTargets.Add(Parameters[i + firstOptionalNonNull].DefaultValue.IsNull ? endLabel : new Label());

			builder.Append(new Switch(jumpTargets)); // Jump to the first unassigned parameter
			builder.Append(Branch.Always(endLabel)); // No missing parameters

			for (var i = 0; i < optionalCount; i++)
			{
				var index = firstOptionalNonNull + i;
				if (Parameters[index].DefaultValue.IsNull)
					continue;

				builder.Append(jumpTargets[i]); // The label for this parameter
				Parameters[index].DefaultValue.Compile(compiler, builder); // Evaluate the value
				builder.Append(new StoreLocal(builder.GetParameter(index + offset))); // Store!
				// Fall through to the next parameter, which is also missing
			}

			builder.Append(endLabel);
		}

		private void CompileGenerator(Compiler compiler, MethodBuilder builder)
		{
			var stateField = ((GeneratorClass)this.Group.ParentAsClass).StateField;

			var stateSwitch = new Switch(); // The target list will be updated later

			builder.Append(new LoadLocal(builder.GetParameter(0))); // Load this
			builder.Append(LoadField.Create(builder.Module, stateField)); // Load this.'<>state'
			builder.Append(stateSwitch); // Switch on this.'<>state'

			// This list will be repopulated when traversing all the statements,
			// to make sure it only contains reachable yields.
			Yields.Clear();
			var canEnd = Body.Node.IsEndReachable || Body.Node.CanReturn;
			if (canEnd)
				Yields.Add(null); // End label placeholder thing

			Body.Node.Compile(compiler, builder);

			var endLabel = canEnd ? new Label("generator-end") : null;
			stateSwitch.SetTargets(Yields.Select(y => y == null ? endLabel : y.StateLabel));

			if (canEnd)
			{
				if (Body.Node.IsEndReachable)
				{
					builder.Append(new LoadLocal(builder.GetParameter(0))); // Load this
					builder.Append(new LoadConstantInt(0)); // Load the end state
					builder.Append(StoreField.Create(builder.Module, stateField)); // Store value in this.'<>state'
				}

				builder.Append(endLabel);
				builder.Append(LoadConstant.False());
				builder.Append(new SimpleInstruction(Opcode.Ret));
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

			var method = this;
			while (method is LocalMethod)
				method = ((LocalMethod)method).Function.Parent.Method;

			var group = method.Group;

			Namespace ns;
			string namePrefix;
			if (group.Parent.Kind == MemberKind.Namespace)
			{
				namePrefix = "";
				ns = group.ParentAsNamespace;
			}
			else // class
			{
				namePrefix = group.ParentAsClass.Name + "/";
				ns = group.ParentAsClass.Parent;
			}

			genClass = new GeneratorClass(GetGeneratorClassName(namePrefix, method), this, ns, compiler);
			genClass.SharedType = group.ParentAsClass;

			//var moveNextBody = new Block(body.Node.Statements); // Reuse the statements list
			var moveNext = new Method("moveNext", AccessLevel.Public, null, Signature.Empty)
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

				var body = new List<Statement>();
				body.Add(new ExpressionStatement(new AssignmentExpression(
					new LocalVariableAccess(generatorLocal, LocalAccessKind.NonCapturing) { IsAssignment = true },
					new ObjectCreationExpression(null, new List<Expression>()) { Constructor = ctor }
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

				block = new Block(body);
			}
			else
			{
				block = new Block(new List<Statement>
				{
					new ReturnStatement(new ObjectCreationExpression(null, new List<Expression>()) { Constructor = ctor })
				});
			}

			this.body = new BlockSpace(block, this);
		}

		private static string GetGeneratorClassName(string prefix, Method method)
		{
			var groupIndex = method.Group.IndexOfOverload(method);
			return string.Format("I:{0}{1}@{2}__{3}", prefix, method.Name.Replace('.', '#'),
				groupIndex, method.ClosureCounter++);
		}
	}

	internal class BytecodeMethod : Method
	{
		public BytecodeMethod(string name, AccessLevel access, Splat splat, params Parameter[] parameters)
			: base(name, access, null, new Signature(parameters, splat))
		{
			if (parameters != null)
				this.Parameters = (Parameter[])parameters.Clone();
			else
				this.Parameters = new Parameter[0];
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
			var mb = new MethodBuilder(!IsStatic, Parameters, compiler.OutputModule);

			foreach (var obj in contents)
				if (obj is Label)
					mb.Append((Label)obj);
				else
					mb.Append((Instruction)obj);

			var body = mb.GetBodyBytes();
			BodyOffset = compiler.OutputModule.AppendMethodBody(body);
			BodyLength = body.Length;
			LocalCount = mb.LocalCount;
			MaxStack = mb.GetMaxStack();
			TryBlocks = mb.GetTryBlocks();
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