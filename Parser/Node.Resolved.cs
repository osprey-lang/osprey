using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.Instructions;
using Type = Osprey.Members.Type;

// Contains nodes that represent resolved names.

namespace Osprey.Nodes
{
	public sealed class LocalVariableAccess : AssignableExpression
	{
		public LocalVariableAccess(Variable variable, LocalAccessKind kind)
		{
			Variable = variable;
			AccessKind = kind;

			// Assume the variable is being read, until IsAssignment is set to true.
			variable.ReadCount++;
		}

		/// <summary>The variable that this expression accesses.</summary>
		public readonly Variable Variable;

		/// <summary>The kind of access this node represents.</summary>
		public readonly LocalAccessKind AccessKind;

		public override bool IsAssignment
		{
			get { return base.IsAssignment; }
			set
			{
				if (Variable != null && value != IsAssignment)
				{
					// The value has changed! We must update the Variable's status.
					if (value)
					{
						Variable.AssignmentCount++;
						Variable.ReadCount--;
					}
					else
					{
						Variable.AssignmentCount--;
						Variable.ReadCount++;
					}
				}

				base.IsAssignment = value;
			}
		}
		
		public override string ToString(int indent)
		{
			switch (AccessKind)
			{
				case LocalAccessKind.CapturingSameScope:
					return "«cap " + Variable.Name + "»";
				case LocalAccessKind.CapturingOtherScope:
					return "«^cap " + Variable.Name + "»";
				case LocalAccessKind.ClosureLocal:
					return "«closure " + Variable.Parent.BlockNumber + "»";
				default:
					return "«var " + Variable.Name + "»";
			}
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			if (Variable.IsCaptured && (!forGenerator || Variable.CaptureField.Parent is GeneratorClass))
			{
				/* If the variable is captured, then we need to transform the parse tree!
				 * (Capture field = the field that captures the variable)
				 * If this is a non-capturing access:
				 *   * Load the anonymous local that contains the block's closure class
				 *   * Access the capture field
				 * If this is a capturing-same-scope access:
				 *   * The capture field is in the current instance
				 *   * Load 'this'
				 *   * Access the capture field
				 * If this is a capturing-other-scope access:
				 *   * The capture field is in another castle
				 *   * Load 'this'
				 *   * Load the field that contains the closure class of the declaring block
				 *   * Access the capture field
				 */
				switch (AccessKind)
				{
					case LocalAccessKind.NonCapturing:
						{
							// Iteration variables are readonly; we don't need to access them
							// through the closure class, except in a generator.
							if (!forGenerator &&
								Variable.VariableKind == VariableKind.IterationVariable)
								break;

							var varBlock = Variable.Parent;
							var inner = forGenerator ?
								(Expression)new ThisAccess() :
								(Expression)new LocalVariableAccess(varBlock.ClosureVariable, LocalAccessKind.ClosureLocal);

							var closureClass = forGenerator ?
								(Class)currentBlock.Method.GeneratorClass :
								(Class)varBlock.ClosureClass;

							return new InstanceMemberAccess(inner, closureClass, Variable.CaptureField).At(this);
						}
					case LocalAccessKind.CapturingSameScope:
						{
							// Load 'this.<closure field>'
							var closureClass = Variable.Parent.ClosureClass;
							return new InstanceMemberAccess(new ThisAccess(), closureClass, Variable.CaptureField).At(this);
						}
					case LocalAccessKind.CapturingOtherScope:
						{
							var varBlock = Variable.Parent;

							// Load block field in current closure class
							var currentClosure = ((LocalMethod)currentBlock.Method).Function.Parent.ClosureClass;
							var inner = new InstanceMemberAccess(new ThisAccess(), currentClosure,
								currentClosure.DeclareBlockField(varBlock));

							// Load variable field
							var varClosure = varBlock.ClosureClass;
							return new InstanceMemberAccess(inner, varClosure, Variable.CaptureField).At(this);
						}
					case LocalAccessKind.ClosureLocal:
						if (forGenerator)
							return new InstanceMemberAccess(new ThisAccess(),
								Variable.CaptureField.Parent, Variable.CaptureField).At(this);
						break;
				}
			}

			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (IsAssignment)
				throw new InvalidOperationException("LocalVariableAccess assignment not compiled specially by AssignmentExpression.");

			LocalVariable local;
			if (AccessKind == LocalAccessKind.ClosureLocal)
				local = Variable.Parent.ClosureLocal;
			else if (Variable.IsParameter)
				local = method.GetParameter(Variable.Name);
			else
				local = method.GetLocal(Variable.Name);

			method.Append(new LoadLocal(local));
		}

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value, bool useValue)
		{
			value.Compile(compiler, method);
			if (useValue)
				method.Append(new SimpleInstruction(Opcode.Dup));

			method.Append(new StoreLocal(GetLocal(method)));
		}

		public override void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op)
		{
			var local = GetLocal(method);
			method.Append(new LoadLocal(local));

			value.Compile(compiler, method);
			method.Append(SimpleInstruction.FromOperator(op));

			method.Append(new StoreLocal(local));
		}

		public override LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method)
		{
			return null;
		}

		public override void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			// Do nothing!
		}

		public override void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			method.Append(new StoreLocal(GetLocal(method)));
		}

		internal LocalVariable GetLocal(MethodBuilder method)
		{
			if (AccessKind == LocalAccessKind.ClosureLocal)
				return Variable.Parent.ClosureLocal;
			else if (Variable.IsParameter)
				return method.GetParameter(Variable.Name);
			else
				return method.GetLocal(Variable.Name);
		}
	}

	public enum LocalAccessKind
	{
		/// <summary>
		/// The local variable access does not capture the variable.
		/// </summary>
		NonCapturing = 0,
		/// <summary>
		/// The local variable is captured from a function or lambda expression in the same block as the variable.
		/// </summary>
		CapturingSameScope = 1,
		/// <summary>
		/// The local variable is captured from a function or lambda expression in a different block than the variable.
		/// </summary>
		CapturingOtherScope = 2,
		/// <summary>
		/// The local variable contains the closure class of a block.
		/// (Anonymous locals cannot be captured, so this is a non-capturing access.)
		/// </summary>
		ClosureLocal = 3,
	}

	public sealed class LocalConstantAccess : ConstantExpression
	{
		public LocalConstantAccess(LocalConstant constant)
			: base() // Value will be updated later
		{
			Constant = constant;
			constant.ReadCount++;
		}

		public LocalConstant Constant;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return Constant.Value.GetTypeObject(compiler);
		}

		public override ConstantValue Value
		{
			get
			{
				return Constant.Value;
			}
			internal set
			{
				throw new InvalidOperationException("Local constants are assigned during constant folding.");
			}
		}

		public override string ToString(int indent)
		{
			return "«const " + Constant.Name + "»";
		}
	}

	public class LocalFunctionAccess : Expression
	{
		public LocalFunctionAccess(LocalFunction function)
		{
			Function = function;
			function.ReadCount++;
		}

		public LocalFunction Function;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.MethodType;
		}

		public override string ToString(int indent)
		{
			return "«function " + Function.Name + "»";
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			switch (Function.CompilationStrategy)
			{
				case LocalFunctionCompilationStrategy.ClosureMethod:
					{
						/* A local function can be accessed in one of three ways:
						 *   1. From an expression in the body that declares it:
						 *       function a() {
						 *           function b() { }
						 *           b(); // <-- a == b.declaringMethod
						 *           for x in something:
						 *               b(); // <-- also a == b.declaringMethod
						 *       }
						 *       
						 *   2. From inside a local function declared in the same scope
						 *     as the function being accessed:
						 *       function a() {
						 *           function b() {
						 *               b(); // <-- b.block == b.block (duh)
						 *           }
						 *           function c() {
						 *               b(); // <-- c.block == b.block
						 *           }
						 *       }
						 *       
						 *   3. From a local function inside the function being accessed
						 *     (with arbitrarily deep nesting):
						 *       function a() {
						 *           function b() {
						 *               function c() {
						 *                   b(); // <--
						 *                   function d() {
						 *                       b(); // <--
						 *                   }
						 *               }
						 *           }
						 *       }
						 *       
						 * Note that the opposite of case 3 - referring to a function nested
						 * several levels within the current method - is not possible.
						 */
						var group = Function.Method.Group;
						if (Function.Parent.Method == currentBlock.Method)
							// The function is declared inside this method.
							// Load it from the appropriate closure local.
							return new InstanceMemberAccess(
								new LocalVariableAccess(Function.Parent.ClosureVariable, LocalAccessKind.ClosureLocal),
								Function.Parent.ClosureClass, group
							);

						var currentMethod = currentBlock.Method as LocalMethod;

						var functionClosure = Function.Parent.ClosureClass;
						if (currentMethod.Function.Parent == Function.Parent)
							// The function is declared in the same scope as the current method;
							// they therefore belong to the same closure class. Load from 'this'.
							return new InstanceMemberAccess(new ThisAccess(), functionClosure, group);

						// The function is not declared in the same scope as the current method,
						// nor is it a child of the current method. We must therefore load it from
						// the captured scope of the declaring block!
						var closure = currentMethod.Function.Parent.ClosureClass;
						var inner = new InstanceMemberAccess(new ThisAccess(),
							closure, closure.GetBlockField(Function.Parent));
						return new InstanceMemberAccess(inner, functionClosure, group);
					}
				case LocalFunctionCompilationStrategy.InstanceMethod:
					{
						// Access the (maybe captured) current instance, then load the method group from that.
						// ThisAccess knows how to access captured instances, so we let it do that.
						var inner = new ThisAccess().TransformClosureLocals(currentBlock, forGenerator);
						var group = Function.Method.Group;
						return new InstanceMemberAccess(inner, group.ParentAsClass, group);
					}
				case LocalFunctionCompilationStrategy.StaticMethod:
					// Just load the method!
					return new StaticMethodAccess(Function.Method.Group);
			}

			throw new InvalidOperationException("Invalid compilation strategy for local function.");
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException("Local function access is supposed to be transformed away!");
		}
	}

	public class LambdaFunctionAccess : LocalFunctionAccess
	{
		public LambdaFunctionAccess(LocalFunction function)
			: base(function)
		{ }

		public override Expression FoldConstant()
		{
			// This is the only reference to the local function in the entire parse tree,
			// so we need to perform constant reduction in here.
			((LocalFunctionDeclaration)Function.Node).FoldConstant();
			return base.FoldConstant();
		}

		public override Expression TransformClosureLocals(BlockSpace currentBlock, bool forGenerator)
		{
			// This is the only reference to the local function in the entire parse tree,
			// so we need to perform the transformation here.
			((LocalFunctionDeclaration)Function.Node).TransformClosureLocals(currentBlock, forGenerator);
			return base.TransformClosureLocals(currentBlock, forGenerator);
		}
	}

	// Note: only classes have instance members.
	// (The members of an enum are all statically accessible.)
	public sealed class InstanceMemberAccess : AssignableExpression
	{
		public InstanceMemberAccess(Expression inner, Class declType, NamedMember member)
		{
			if (inner == null)
				throw new ArgumentNullException("inner");
			if (declType == null)
				throw new ArgumentNullException("declType");
			if (member == null)
				throw new ArgumentNullException("member");
			if (member.Kind != MemberKind.Field &&
				member.Kind != MemberKind.Property &&
				member.Kind != MemberKind.MethodGroup)
				throw new ArgumentException("The instance member must be a field, propery or method group.", "member");

			Inner = inner;
			DeclType = declType;
			Member = member;
			IsBaseAccess = inner is BaseAccess;
		}

		/// <summary>The expression whose instance member is being accessed.</summary>
		public Expression Inner;
		/// <summary>The type that declares the member.</summary>
		public readonly Class DeclType;
		/// <summary>The member that is being accessed. This is a method, property or field.</summary>
		public readonly NamedMember Member;
		/// <summary>Whether or not the member is being accessed through 'base',
		/// in which case overridable members are treated as non-overridable.</summary>
		public bool IsBaseAccess;

		public override bool IsTypeKnown(Compiler compiler) { return Member.Kind == MemberKind.MethodGroup; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.MethodType;
		}

		public override string ToString(int indent)
		{
			return string.Format("{0}.({1}){2}", Inner.ToString(indent), DeclType.FullName, Member.Name);
		}

		public override Expression FoldConstant()
		{
			Inner = Inner.FoldConstant();
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
				throw new InvalidOperationException("InstanceMemberAccess assignment not compiled specially by AssignmentExpression.");

			Inner.Compile(compiler, method); // Evaluate the instance expression

			if (Member.Kind == MemberKind.Field)
			{
				method.Append(LoadField.Create(method.Module, (Field)Member)); // Load the field
			}
			else if (Member.Kind == MemberKind.Property)
			{
				var property = (Property)Member;
				if (!IsBaseAccess && (property.IsAbstract || property.IsOverridable))
					method.Append(new LoadMember(method.Module.GetStringId(property.Name)));
				else
					method.Append(new StaticCall(method.Module.GetMethodId(property.Getter.Method.Group), 0));
			}
			else // MethodGroup
			{
				// If this method access were being compiled as part of an invocation,
				// then InvocationExpression would compile directly to a call or scall.
				// We're passing an instance method as a value into something.
				method.Append(new LoadMember(method.Module.GetStringId(Member.Name)));
			}
		}

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value, bool useValue)
		{
			// Note: the member is either a field or a property. Method groups cannot be assigned to,
			// which is caught by AssignmentExpression.EnsureAssignable.

			Inner.Compile(compiler, method); // Always evaluate the instance

			LocalVariable valueLocal = null;
			value.Compile(compiler, method); // And the value
			if (useValue && !value.CanSafelyInline)
			{
				valueLocal = method.GetAnonymousLocal();
				method.Append(new SimpleInstruction(Opcode.Dup));
				method.Append(new StoreLocal(valueLocal));
			}

			if (Member.Kind == MemberKind.Field)
				method.Append(StoreField.Create(method.Module, (Field)Member)); // Store the field
			else // Property
			{
				var prop = (Property)Member;
				var setter = prop.Setter.Method;
				if (setter.IsVirtualCall && !IsBaseAccess)
					method.Append(new StoreMember(method.Module.GetStringId(prop.Name)));
				else
				{
					method.Append(new StaticCall(method.Module.GetMethodId(setter.Group), 1)); // Call the setter directly
					method.Append(new SimpleInstruction(Opcode.Pop)); // Pop the result of the call
				}
			}

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
			method.Append(new SimpleInstruction(Opcode.Dup));

			if (Member.Kind == MemberKind.Field)
			{
				method.Append(LoadField.Create(method.Module, (Field)Member)); // Load the field
				value.Compile(compiler, method); // Evaluate the value
				method.Append(SimpleInstruction.FromOperator(op)); // instance.field op value -> finalValue

				// Stack: instance, finalValue
				method.Append(StoreField.Create(method.Module, (Field)Member)); // Store the field
			}
			else // Property
			{
				var prop = (Property)Member;
				var getter = prop.Getter.Method;
				var setter = prop.Setter.Method;

				if (getter.IsVirtualCall && !IsBaseAccess)
					method.Append(new LoadMember(method.Module.GetStringId(prop.Name)));
				else
					method.Append(new StaticCall(method.Module.GetMethodId(getter.Group), 0)); // Call the getter directly

				value.Compile(compiler, method); // Evaluate the value
				method.Append(SimpleInstruction.FromOperator(op)); // instance.prop op value -> finalValue

				// Stack: instance, finalValue
				if (setter.IsVirtualCall && !IsBaseAccess)
					method.Append(new StoreMember(method.Module.GetStringId(prop.Name)));
				else
				{
					method.Append(new StaticCall(method.Module.GetMethodId(setter.Group), 1)); // Call the setter directly
					method.Append(new SimpleInstruction(Opcode.Pop)); // Pop the result of the call
				}
			}
		}

		public override LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method)
		{
			if (Inner.CanSafelyInline)
				return null;
			else
			{
				var innerLocal = method.GetAnonymousLocal();

				Inner.Compile(compiler, method);
				method.Append(new StoreLocal(innerLocal));

				return new LocalVariable[] { innerLocal };
			}
		}

		public override void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			if (Inner.CanSafelyInline)
				Inner.Compile(compiler, method);
			else
				method.Append(new LoadLocal(locals[0]));
		}

		public override void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			if (Member.Kind == MemberKind.Field)
				method.Append(StoreField.Create(method.Module, (Field)Member));
			else // property
			{
				var prop = (Property)Member;
				var setter = prop.Setter.Method;
				if (setter.IsVirtualCall && !IsBaseAccess)
					method.Append(new StoreMember(method.Module.GetStringId(prop.Name)));
				else
				{
					method.Append(new StaticCall(method.Module.GetMethodId(setter.Group), 1));
					method.Append(new SimpleInstruction(Opcode.Dup));
				}
			}
		}
	}

	public sealed class StaticFieldAccess : AssignableExpression
	{
		public StaticFieldAccess(Field field)
		{
			Field = field;
		}

		public readonly Field Field;

		public override string ToString(int indent)
		{
			return "‹field " + Field.Parent.FullName + "." + Field.Name + "›";
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (IsAssignment)
				throw new InvalidOperationException("StaticFieldAccess assignment not compiled specially by AssignmentExpression.");

			method.Append(LoadField.Create(method.Module, Field));
		}

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value, bool useValue)
		{
			value.Compile(compiler, method);
			if (useValue)
				method.Append(new SimpleInstruction(Opcode.Dup));
			method.Append(StoreField.Create(method.Module, Field));
		}

		public override void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op)
		{
			method.Append(LoadField.Create(method.Module, Field)); // Load the field
			value.Compile(compiler, method); // Evaluate the value
			method.Append(SimpleInstruction.FromOperator(op)); // field op value

			method.Append(StoreField.Create(method.Module, Field));
		}

		public override LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method)
		{
			return null;
		}

		public override void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			// Do nothing!
		}

		public override void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			method.Append(StoreField.Create(method.Module, Field));
		}
	}

	public sealed class StaticMethodAccess : Expression
	{
		public StaticMethodAccess(MethodGroup method)
		{
			Method = method;
		}

		public MethodGroup Method;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return compiler.MethodType;
		}

		public override string ToString(int indent)
		{
			if (Method.Parent is Class)
				return "‹method " + ((Class)Method.Parent).FullName + "." + Method.Name + "›";
			else if (Method.Parent is Namespace)
				return "‹method " + ((Namespace)Method.Parent).FullName + "." + Method.Name + "›";

			return Method.Name; // probably a global
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			// Note: if this were part of an invocation, InvocationExpression would
			// have compiled it to a static call instruction. If we reach this point,
			// the method is being passed as a value.
			method.Append(new LoadStaticMethod(method.Module.GetMethodId(Method)));
		}
	}

	public sealed class StaticPropertyAccess : AssignableExpression
	{
		public StaticPropertyAccess(Property property)
		{
			Property = property;
		}

		public Property Property;

		public override string ToString(int indent)
		{
			return "‹prop " + Property.Parent.FullName + "." + Property.Name + "›";
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			if (IsAssignment)
				throw new InvalidOperationException();

			method.Append(new StaticCall(method.Module.GetMethodId(Property.Getter.Method.Group), 0));
		}

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value, bool useValue)
		{
			value.Compile(compiler, method); // Evaluate the value
			if (useValue)
				method.Append(new SimpleInstruction(Opcode.Dup));

			method.Append(new StaticCall(method.Module.GetMethodId(Property.Setter.Method.Group), 1)); // Set the property
			method.Append(new SimpleInstruction(Opcode.Pop)); // Pop the result of the call
		}

		public override void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op)
		{
			method.Append(new StaticCall(method.Module.GetMethodId(Property.Getter.Method.Group), 0)); // Get the property
			value.Compile(compiler, method); // Evaluate the value

			method.Append(SimpleInstruction.FromOperator(op)); // prop op value

			method.Append(new StaticCall(method.Module.GetMethodId(Property.Setter.Method.Group), 1)); // Set the property
			method.Append(new SimpleInstruction(Opcode.Pop)); // Pop the result of the call
		}

		public override LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method)
		{
			return null;
		}

		public override void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			// Do nothing!
		}

		public override void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			method.Append(new StaticCall(method.Module.GetMethodId(Property.Setter.Method.Group), 1));
			method.Append(new SimpleInstruction(Opcode.Pop));
		}
	}

	public sealed class ClassConstantAccess : ConstantExpression
	{
		public ClassConstantAccess(ClassConstant constant)
		{
			Constant = constant;
		}

		public ClassConstant Constant;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return Constant.Value.GetTypeObject(compiler);
		}

		public override ConstantValue Value
		{
			get
			{
				return Constant.Value;
			}
			internal set
			{
				throw new InvalidOperationException("Class constants are assigned during constant folding.");
			}
		}

		public override string ToString(int indent)
		{
			return "‹const " + Constant.Parent.FullName + "." + Constant.Name + "›";
		}
	}

	public sealed class EnumFieldAccess : ConstantExpression
	{
		public EnumFieldAccess(EnumField field, bool useUnderlyingValue)
		{
			Field = field;
			UseUnderlyingValue = useUnderlyingValue;
		}

		public EnumField Field;
		public bool UseUnderlyingValue;

		public override ConstantValue Value
		{
			get
			{
				var fieldValue = Field.Value;
				if (fieldValue.Type == ConstantValueType.Enum)
				{
					if (UseUnderlyingValue)
						return ConstantValue.CreateInt(fieldValue.EnumValue.Value);

					return fieldValue;
				}
				else
				{
					if (UseUnderlyingValue)
						return fieldValue;

					var value = fieldValue.IntValue;
					return ConstantValue.CreateEnumValue(value, Field.Parent);
				}
			}
			internal set
			{
				throw new InvalidOperationException("Enum fields are assigned during constant reduction.");
			}
		}

		public override bool IsTypeKnown(Compiler compiler)
		{
			return true;
		}

		public override Type GetKnownType(Compiler compiler)
		{
			// Enum values contain the members of their containing type's base type.
			// For enum sets, this means aves.EnumSet; for normal enums, aves.Enum.
			return Field.Parent.BaseType;
		}

		public override string ToString(int indent)
		{
			return "‹enum " + Field.Parent.FullName + "." + Field.Name + "›";
		}
	}

	public sealed class GlobalConstantAccess : ConstantExpression
	{
		public GlobalConstantAccess(GlobalConstant constant)
		{
			Constant = constant;
		}

		public GlobalConstant Constant;

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Type GetKnownType(Compiler compiler)
		{
			return Constant.Value.GetTypeObject(compiler);
		}

		public override ConstantValue Value
		{
			get
			{
				return Constant.Value;
			}
			internal set
			{
				throw new InvalidOperationException("Global constants are assigned during constant folding.");
			}
		}

		public override string ToString(int indent)
		{
			return "‹const global." + Constant.Parent.FullName + "." + Constant.Name + "›";
		}
	}

	public sealed class NamespaceAccess : Expression
	{
		public NamespaceAccess(Namespace ns)
		{
			Namespace = ns;
		}

		public Namespace Namespace;

		public override string ToString(int indent)
		{
			return "‹namespace " + Namespace.FullName + "›";
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException();
		}
	}

	public sealed class TypeAccess : Expression
	{
		public TypeAccess(Type type)
		{
			Type = type;
		}

		public Type Type;

		public override string ToString(int indent)
		{
			return "‹type " + Type.FullName + "›";
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException();
		}
	}

	public sealed class GlobalVariableAccess : AssignableExpression
	{
		public GlobalVariableAccess(GlobalVariable variable)
		{
			Variable = variable;
		}

		public GlobalVariable Variable;

		public override string ToString(int indent)
		{
			return "‹var global." + Variable.Name + "›";
		}

		private void Load(Compiler compiler, MethodBuilder method)
		{
			if (!Variable.IsCaptured)
				method.Append(new LoadLocal(method.GetLocal(Variable.Name)));
			else
				method.Append(LoadField.Create(method.Module, Variable.CaptureField));
		}

		private void Store(Compiler compiler, MethodBuilder method)
		{
			if (!Variable.IsCaptured)
				method.Append(new StoreLocal(method.GetLocal(Variable.Name)));
			else
				method.Append(StoreField.Create(method.Module, Variable.CaptureField));
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			Load(compiler, method);
		}

		public override void CompileSimpleAssignment(Compiler compiler, MethodBuilder method, Expression value, bool useValue)
		{
			value.Compile(compiler, method);
			if (useValue)
				method.Append(new SimpleInstruction(Opcode.Dup));
			Store(compiler, method);
		}

		public override void CompileCompoundAssignment(Compiler compiler, MethodBuilder method, Expression value, BinaryOperator op)
		{
			Load(compiler, method);
			value.Compile(compiler, method);
			method.Append(SimpleInstruction.FromOperator(op));

			Store(compiler, method);
		}

		public override LocalVariable[] CompileParallelFirstEvaluation(Compiler compiler, MethodBuilder method)
		{
			return null;
		}

		public override void CompileParallelLoadInstance(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			// Do nothing!
		}

		public override void CompileParallelAssignment(Compiler compiler, MethodBuilder method, LocalVariable[] locals)
		{
			Store(compiler, method);
		}
	}
}