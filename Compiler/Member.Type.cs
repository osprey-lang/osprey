using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Osprey.Nodes;
using CI = System.Globalization.CultureInfo;

namespace Osprey.Members
{
	[DebuggerDisplay("Type {FullName}")]
	public abstract class Type : NamedMember, IDeclarationSpace
	{
		protected Type(TypeDeclaration node, MemberKind kind, Namespace parent)
			: base(node == null ? null : node.Name, kind, node, node.Access)
		{
			if (parent == null)
				throw new ArgumentNullException("parent");
			this.parent = parent;
		}
		protected Type(string name, MemberKind kind, AccessLevel access, Namespace parent)
			: base(name, kind, null, access)
		{
			this.parent = parent;
		}

		public bool IsPrimitive { get; internal set; }

		/// <summary>Gets the module that the type is defined in.</summary>
		public Module Module { get; internal set; }

		private Namespace parent;
		/// <summary>Gets the namespace that contains the type.</summary>
		public Namespace Parent { get { return parent; } internal set { parent = value; } }

		/// <summary>Gets the base of the type.</summary>
		public Type BaseType { get; internal set; }
		/// <summary>Gets the shared type, which this class can access private and protected members of.</summary>
		public Type SharedType { get; internal set; }

		internal uint Id;

		/// <summary>Gets the fully qualified name of the type.</summary>
		public string FullName
		{
			get
			{
				return parent == null || parent.Name == null ? this.Name :
					parent.FullName + "." + this.Name;
			}
		}

		IDeclarationSpace IDeclarationSpace.Parent { get { return parent; } }

		public abstract bool ContainsMember(string name);

		public NamedMember GetMember(string name, Type instType, Type fromType)
		{
			var type = this;
			while (type != null)
			{
				var mem = type.GetMember(name);
				if (mem != null)
				{
					Type declType;
					if (mem is ClassMember)
						declType = ((ClassMember)mem).Parent;
					else if (mem is EnumField)
						declType = ((EnumField)mem).Parent;
					else if (mem is MethodGroup)
						declType = ((MethodGroup)mem).ParentAsClass;
					else
						throw new Exception("Type contains an invalid member type.");

					if (IsAccessible(mem.Access, instType: instType, declType: declType, fromType: fromType))
						return mem;
				}

				// Try the base type, if there is one
				type = type.BaseType;
			}

			return null;
		}

		public abstract NamedMember GetMember(string name);

		public abstract NamedMember ResolveName(string name, Class fromClass);

		public override Namespace GetContainingNamespace()
		{
			return Parent;
		}

		public abstract Method FindConstructor(ParseNode errorNode, int argCount, Class fromClass);

		Class IDeclarationSpace.GetContainingClass(out bool hasInstance)
		{
			// Note: this point can only ever be reached from the initializer of a field
			// (including constants), and they do NOT have access to the instance, even
			// instance fields.
			hasInstance = false;
			return this as Class;
		}

		/// <summary>
		/// Determines whether a member is accessible.
		/// </summary>
		/// <param name="access">The accessibility of the member.</param>
		/// <param name="instType">The type of the instance that the member belongs to.</param>
		/// <param name="declType">The type that declares the member.</param>
		/// <param name="fromType">The type that contains the expression that looks up the member, or null if it is not contained within a type.</param>
		/// <returns>True if the member is accessible; otherwise, false.</returns>
		public static bool IsAccessible(AccessLevel access, Type instType, Type declType, Type fromType)
		{
			// Note: this algorithm is basically lifted straight from Ovum,
			// though it does not take shared types into account (because all
			// accessibility checking is done before the compiler extracts
			// closure classes and the like, i.e. the things that make use
			// of type sharing).
			if (access == AccessLevel.Private)
				return fromType != null && (declType == fromType || declType == fromType.SharedType);

			if (access == AccessLevel.Protected)
			{
				if (fromType == null)
					return false;

				while (instType != null && instType != fromType)
					instType = instType.BaseType;

				if (instType == null)
					return false;

				while (fromType != null && fromType != declType)
					fromType = fromType.BaseType;
			}

			return true; // AccessLevel.Public or accessible
		}
	}

	public class Enum : Type
	{
		public Enum(EnumDeclaration node, Namespace parent)
			: base(node, MemberKind.Enum, parent)
		{
			IsPrimitive = true;
			IsSet = node.IsSet;
			foreach (var member in node.Members)
				DeclareField(new EnumField(member, this));
		}
		public Enum(string name, AccessLevel access, bool isSet, Namespace parent)
			: base(name, MemberKind.Enum, access, parent)
		{
			IsPrimitive = true;
			IsSet = isSet;
		}

		internal Dictionary<string, EnumField> members = new Dictionary<string, EnumField>();

		public bool IsSet { get; private set; }

		public void DeclareField(EnumField field)
		{
			if (field == null)
				throw new ArgumentNullException("field");

			if (members.ContainsKey(field.Name))
				throw new DuplicateNameException(field.Node, field.Name);

			members.Add(field.Name, field);
			field.Parent = this;
		}

		public override bool ContainsMember(string name)
		{
			if (name == null)
				throw new ArgumentNullException("name");

			return members.ContainsKey(name);
		}

		public override NamedMember ResolveName(string name, Class fromClass)
		{
			if (name == null)
				throw new ArgumentNullException("name");

			if (members.ContainsKey(name))
				return members[name];
			if (Parent != null)
				return Parent.ResolveName(name, fromClass);

			return null;
		}

		public override NamedMember GetMember(string name)
		{
			EnumField result;
			members.TryGetValue(name, out result);
			return result;
		}

		public override Method FindConstructor(ParseNode errorNode, int argCount, Class fromClass)
		{
			throw new CompileTimeException(errorNode, "Enums do not have any constructors.");
		}

		public IEnumerable<EnumField> GetFieldsSorted()
		{
			return members.Values.OrderBy(m => m.Name, StringComparer.InvariantCultureIgnoreCase);
		}
	}

	public class EnumField : NamedMember, IConstantMember
	{
		public EnumField(EnumMember node, Enum parent)
			: base(node.Name, MemberKind.EnumField, node, AccessLevel.Public)
		{
			Parent = parent;
		}
		public EnumField(string name, long value, Enum parent)
			: base(name, MemberKind.EnumField, null, AccessLevel.Public)
		{
			this.state = ConstantState.HasValue;
			this.value = ConstantValue.CreateEnumValue(value, parent);
		}

		public string FullName
		{
			get { return Parent.FullName + "." + this.Name; }
		}

		private ConstantState state;
		public ConstantState State { get { return state; } }

		private ConstantValue value;
		public ConstantValue Value
		{
			get
			{
				if (Node == null) return value; // Imported enum field

				var enumField = (EnumMember)Node;

				if (state == ConstantState.Folding)
					throw new CompileTimeException(enumField, "Circular enum field definition detected.");
				if (state == ConstantState.NotFolded)
				{
					state = ConstantState.Folding;
					enumField.FoldConstant();
					state = ConstantState.HasValue;
				}

				return ((ConstantExpression)enumField.Value).Value;
			}
		}

		public Enum Parent { get; internal set; }

		internal uint Id;

		public override Namespace GetContainingNamespace()
		{
			return Parent.GetContainingNamespace();
		}
	}

	public class Class : Type
	{
		public Class(ClassDeclaration node, Namespace parent)
			: base(node, MemberKind.Class, parent)
		{
			this.IsPrimitive = node.IsPrimitive;
			this.IsAbstract = node.IsAbstract;
			this.IsInheritable = node.IsInheritable;
			this.IsStatic = node.IsStatic;
			this.initializer = node.Initializer;
		}
		public Class(string name, AccessLevel access, Namespace parent)
			: base(name, MemberKind.Class, access, parent)
		{
			state = ClassState.Inited;
		}

		private ClassState state = ClassState.NotInited;

		public bool IsInheritable { get; internal set; }
		public bool IsAbstract { get; internal set; }
		public bool IsStatic { get; internal set; }

		private MethodGroup constructors;
		/// <summary>Gets the method group that corresponds to the constructors of the class.</summary>
		public MethodGroup Constructors { get { return constructors; } internal set { constructors = value; } }

		private Indexer indexer = null;
		/// <summary>Gets the indexer declared in the class.</summary>
		public Indexer Indexer { get { return indexer; } }

		private Iterator iterator;
		/// <summary>Gets the iterator declared in the class, or null if no iterator was declared.</summary>
		public Iterator Iterator { get { return iterator; } internal set { iterator = value; } }

		private MethodGroup staticCtor;
		/// <summary>Gets the static constructor of the class, or null if there is none.</summary>
		public MethodGroup StaticConstructor { get { return staticCtor; } internal set { staticCtor = value; } }

		private string initializer;
		/// <summary>Gets the name of the type initializer, or null if none was specified.</summary>
		public string Initializer { get { return initializer; } }

		internal Dictionary<string, NamedMember> members = new Dictionary<string, NamedMember>();

		/// <summary>
		/// The total number of overloadable operators. If the number changes and this member
		/// is not updated, you have no one to blame but yourself!
		/// </summary>
		internal const int OperatorCount = 18;
		internal OperatorOverload[] operators = new OperatorOverload[OperatorCount];

		private int overloadedOperatorCount = 0;
		/// <summary>
		/// Gets the total number of operator overloads that this class declares.
		/// </summary>
		public int OverloadedOperatorCount { get { return overloadedOperatorCount; } }

		private int lambdaNameCounter = 0;

		/// <summary>
		/// List of inherited abstract methods which have not been overridden in this class.
		/// </summary>
		/// <remarks>
		/// This is only initialized to an instance if the base class is abstract and this
		/// class is not. Only abstract classes can have abstract members, hence:
		///   1. If this class is abstract, then it is not an error for it not to implement
		///      abstract methods; they're simply left up to a derived class to implement.
		///   2. If the base class it NOT abstract, then it cannot contain any abstract methods,
		///      so we don't actually need to keep track of anything in this one.
		/// It is an error to compile a non-abstract class that has unimplemented inherited
		/// abstract methods. In other words, if this member is not null and not empty, then
		/// the class is invalid.
		/// </remarks>
		private HashSet<Method> inheritedAbstractMethods = null;

		public bool HasUnimplementedAbstractMethods
		{
			get { return inheritedAbstractMethods != null && inheritedAbstractMethods.Count > 0; }
		}

		public void Init(Compiler compiler)
		{
			if (state == ClassState.Inited)
				return; // Nothing to do here!
			if (state == ClassState.Initing)
				throw new CompileTimeException(Node, "Cyclic class declaration (class inherits from itself).");

			state = ClassState.Initing;

			if (BaseType is Class) // may be null (but only for aves.Object)
			{
				var baseClass = (Class)BaseType;
				if (baseClass.state != ClassState.Inited)
					baseClass.Init(compiler);
				if (baseClass.IsAbstract && !this.IsAbstract)
					InitInheritedAbstractMethods();
			}

			var node = Node as ClassDeclaration;
			if (node == null)
				return;

			if (this.initializer != null)
				compiler.EnsureNativeMethodExists(this.Node, this.initializer);

			if (node.Constructors.Count > 0)
				foreach (var ctor in node.Constructors)
				{
					var ctorObject = new Constructor(ctor, this);
					DeclareConstructor(ctorObject);
					ctorObject.InitBody(compiler);
				}
			else if (!this.IsStatic)
				throw new Exception("Internal error: non-static class without constructors. Parser is supposed to add default constructor.");

			foreach (var field in node.Fields)
				foreach (var decl in field.Declarators)
					DeclareField(new Field(decl, field.Access, this)
					{
						IsStatic = field.IsStatic,
					});

			foreach (var constant in node.Constants)
				foreach (var decl in constant.Declarators)
					DeclareConstant(new ClassConstant(decl, constant.Access, this));

			foreach (var method in node.Methods)
			{
				var methodObject = new Method(method);
				DeclareMethod(methodObject);
				methodObject.InitBody(compiler);
			}

			var properties = new Dictionary<string, Property>();
			foreach (var prop in node.Properties)
			{
				if (prop is IndexerAccessorDeclaration)
				{
					var indexer = new IndexerAccessor((IndexerAccessorDeclaration)prop);
					DeclareIndexerAccessor(indexer);
					indexer.InitBody(compiler);
				}
				else
				{
					var name = prop.Name;

					Property property;
					if (properties.ContainsKey(name))
						property = properties[name];
					else
						properties[name] = property = new Property(name, this);

					if (prop.IsSetter)
						property.Setter = new PropertyAccessor(prop);
					else
						property.Getter = new PropertyAccessor(prop);
				}
			}

			foreach (var prop in properties.Values)
			{
				DeclareProperty(prop);
				if (prop.Getter != null)
					prop.Getter.InitBody(compiler);
				if (prop.Setter != null)
					prop.Setter.InitBody(compiler);
			}

			foreach (var op in node.Operators)
			{
				OperatorOverload overload;
				if (op is UnaryOperatorOverload)
					overload = new OperatorOverload((UnaryOperatorOverload)op, this);
				else
					overload = new OperatorOverload((BinaryOperatorOverload)op, this);
				DeclareOperatorOverload(overload);
				overload.Method.InitBody(compiler);
				if (overload.Method.IsGenerator)
					throw new CompileTimeException(overload.Method.Returns[0],
						"An operator overloads is not allowed to be a generator.");
			}

			if (node.Iterator != null)
			{
				this.iterator = new Iterator(node.Iterator, this);
				DeclareMethod(this.iterator.Method);
				this.iterator.Method.InitBody(compiler);
			}

			if (node.StaticConstructor != null)
			{
				var staticCtor = new Constructor(node.StaticConstructor, this);
				DeclareStaticConstructor(staticCtor);
				staticCtor.InitBody(compiler);
			}

			state = ClassState.Inited;
		}

		protected void InitInheritedAbstractMethods()
		{
			inheritedAbstractMethods = new HashSet<Method>();
			var overriddenMethods = new HashSet<Method>();

			var baseClass = BaseType as Class;
			do
			{
				foreach (var member in baseClass.members.Values)
					if (member.Kind == MemberKind.MethodGroup)
						foreach (var method in (MethodGroup)member)
						{
							if (overriddenMethods.Contains(method))
								// This method has already been overridden in a derived class, so we ignore it.
								// But since we'll never run into this metohd again, might as well remove it.
								overriddenMethods.Remove(method);
							else if (method.IsAbstract)
								inheritedAbstractMethods.Add(method);

							if (method.IsOverride && method.OverriddenBaseMethod.IsAbstract)
								overriddenMethods.Add(method.OverriddenBaseMethod);
						}

				baseClass = baseClass.BaseType as Class;
			} while (baseClass != null && baseClass.IsAbstract);
		}

		public void DeclareConstructor(Constructor ctor)
		{
			if (ctor == null)
				throw new ArgumentNullException("ctor");
			if (ctor.IsStatic)
				throw new ArgumentException("Cannot declare a static constructor in DeclareConstructor.");

			if (this.IsStatic)
				throw new DeclarationException(ctor.Node, string.Format(
					"The class '{0}' is static, and therefore cannot declare instance constructors.",
					FullName));

			if (ctor.Access == AccessLevel.Private && (this.IsInheritable || this.IsAbstract))
				throw new DeclarationException(ctor.Node, string.Format(
					"The class '{0}' is inheritable or abstract, and therefore cannot declare private constructors.",
					FullName));

			constructors = DeclareMethod(ctor.Method);
		}

		public void DeclareField(Field field)
		{
			if (field == null)
				throw new ArgumentNullException("name");

			var name = field.Name;
			Class baseMemberClass;
			var baseMember = GetBaseMember(name, this.BaseType as Class, out baseMemberClass);
			if (baseMember != null)
				throw new DeclarationException(field.Node, string.Format("Cannot override inherited member '{0}.{1}'.",
					baseMemberClass.FullName, name));

			if (members.ContainsKey(field.Name))
				throw new DuplicateNameException(field.Node, field.Name);

			members.Add(field.Name, field);
			field.Parent = this;
		}

		public void DeclareConstant(ClassConstant constant)
		{
			if (constant == null)
				throw new ArgumentNullException("constant");

			Class baseMemberClass;
			var baseMember = GetBaseMember(constant.Name, this.BaseType as Class, out baseMemberClass);
			if (baseMember != null)
				throw new DeclarationException(constant.Node, string.Format("Cannot hide inherited member '{0}.{1}'.",
					baseMemberClass.FullName, constant.Name));

			if (members.ContainsKey(constant.Name))
				throw new DuplicateNameException(constant.Node, constant.Name);

			members.Add(constant.Name, constant);
			constant.Parent = this;
		}

		public void DeclareProperty(Property property)
		{
			if (property == null)
				throw new ArgumentNullException("property");

			EnsureDeclarable(property.Node, property);

			if (members.ContainsKey(property.Name))
				throw new DuplicateNameException(property.Node, property.Name, "There is already a member with the specified name in this class.");

			if (property.Getter != null)
				DeclareMethod(property.Getter.Method);
			if (property.Setter != null)
				DeclareMethod(property.Setter.Method);

			if (property.Getter != null && property.Setter != null &&
				property.Getter.Method.Access != property.Setter.Method.Access)
				throw new DeclarationException(property.Getter.Node, "Both accessors must have the same declared accessibility.");

			members.Add(property.Name, property);
		}

		public void DeclareIndexerAccessor(IndexerAccessor accessor)
		{
			if (accessor == null)
				throw new ArgumentNullException("accessor");

			if (indexer == null)
			{
				if (members.ContainsKey(".item"))
					throw new DuplicateNameException(accessor.Node, ".item", "There is already a member with the name '.item' in this class.");

				indexer = new Indexer(this);
				members.Add(".item", indexer);
			}

			indexer.AddAccessor(accessor);
		}

		public MethodGroup DeclareMethod(Method method)
		{
			if (method == null)
				throw new ArgumentNullException("method");

			if (this.IsStatic && !method.IsStatic)
				throw new DeclarationException(method.Node, "Static classes can only contain static members.");

			string name = method.Name;

			EnsureDeclarable(method.Node, method);

			MethodGroup group;
			if (members.ContainsKey(name))
			{
				var mem = members[name];
				if (mem.Kind != MemberKind.MethodGroup)
					throw new DuplicateNameException(method.Node, name,
						string.Format("There is already a member with the name '{0}' in this class.", name));
				group = (MethodGroup)mem;
				if (group.Access != method.Access)
					throw new InconsistentAccessibilityException(method.Node);
			}
			else
			{
				members[name] = group = new MethodGroup(name, this, method.Access);

				Class _;
				var baseMember = GetBaseMember(name, this.BaseType as Class, out _);
				if (baseMember != null && baseMember.Kind == MemberKind.MethodGroup)
					group.BaseGroup = (MethodGroup)baseMember;
			}

			group.AddOverload(method);
			return group;
		}

		public void DeclareOperatorOverload(OperatorOverload op)
		{
			if (op == null)
				throw new ArgumentNullException("op");

			if (operators[op.Index] != null)
				throw new DeclarationException(op.Node, string.Format("The operator {0} has already been overloaded.",
					op.Method.Name));

			DeclareMethod(op.Method);
			operators[op.Index] = op;
			overloadedOperatorCount++;
		}

		public void DeclareStaticConstructor(Constructor ctor)
		{
			if (ctor == null)
				throw new ArgumentNullException("ctor");
			if (!ctor.IsStatic)
				throw new ArgumentException("Cannot declare an instance constructor in DeclareStaticConstructor.");

			if (this.staticCtor != null)
				throw new DeclarationException(ctor.Node, string.Format("The type '{0}' already has a static constructor.",
					this.FullName));

			this.staticCtor = DeclareMethod(ctor.Method);
		}

		internal void ImportProperty(Property property)
		{
			if (property == null)
				throw new ArgumentNullException("property");

			if (members.ContainsKey(property.Name))
				throw new DuplicateNameException(null, property.Name, "There is already a member with the specified name in this class.");

			members.Add(property.Name, property);
		}

		internal void ImportMethodGroup(MethodGroup group)
		{
			if (group == null)
				throw new ArgumentNullException("group");

			if (this.IsStatic && !group.IsStatic)
				throw new DeclarationException(null, "Static classes can only contain static members.");

			var name = group.Name;
			if (members.ContainsKey(name))
				throw new DuplicateNameException(null, name,
					string.Format("There is already a member with the name '{0}' in this class.", name));

			foreach (var overload in group)
				OverrideIfPossible(overload);

			members[name] = group;
			Class _;
			var baseMember = GetBaseMember(name, this.BaseType as Class, out _);
			if (baseMember != null && baseMember.Kind == MemberKind.MethodGroup)
				group.BaseGroup = (MethodGroup)baseMember;
		}

		internal void ImportIndexer(Indexer indexer)
		{
			if (this.indexer != null)
				throw new DuplicateNameException(null, ".item", string.Format("The class '{0}' already has an indexer.", this.FullName));

			this.indexer = indexer;
			members.Add(".item", indexer);
		}

		internal void ImportOperatorOverload(OperatorOverload op)
		{
			if (op == null)
				throw new ArgumentNullException("op");

			if (operators[op.Index] != null)
				throw new InvalidOperationException("This was supposed to be checked earlier!");

			operators[op.Index] = op;
			overloadedOperatorCount++;
		}

		public override bool ContainsMember(string name)
		{
			if (name == null)
				return false;

			return members.ContainsKey(name) ||
				BaseType != null && BaseType.ContainsMember(name);
		}

		public override NamedMember ResolveName(string name, Class fromClass)
		{
			// The only way we could reach Class.ResolveName during compilation is
			// from within the body of the class. So it's okay to look for private
			// members here, but we can't do that in the base class.
			if (members.ContainsKey(name))
				return members[name];

			// Try the base classes! But don't touch their privates.
			var baseClass = this.BaseType;
			while (baseClass != null)
			{
				NamedMember mem;
				if (((Class)baseClass).members.TryGetValue(name, out mem) && mem.Access != AccessLevel.Private)
					return mem;
				baseClass = baseClass.BaseType;
			}

			// And NOW we can try the parent namespace!
			if (Parent != null)
				return Parent.ResolveName(name, fromClass);

			return null; // not found
		}

		public override NamedMember GetMember(string name)
		{
			NamedMember result;
			members.TryGetValue(name, out result);
			return result;
		}

		public override Method FindConstructor(ParseNode errorNode, int argCount, Class fromClass)
		{
			if (!IsAccessible(constructors.Access, instType: fromClass, declType: this, fromType: fromClass))
				throw new CompileTimeException(errorNode,
					string.Format("The constructor of '{0}' is not accessible from this location.",
						this.FullName));

			var ctor = constructors.FindOverload(argCount);
			if (ctor == null)
				throw new CompileTimeException(errorNode,
					string.Format("The type '{0}' does not declare a constructor that takes {1} arguments.",
						this.FullName, argCount.ToString(CI.InvariantCulture)));

			return ctor;
		}

		public IEnumerable<NamedMember> GetMembersSorted()
		{
			return members.Values.OrderBy(m => m.Name, StringComparer.InvariantCultureIgnoreCase);
		}

		/// <summary>
		/// Gets the nearest accessible base member with the specified name.
		/// The member is accessible if it is public or protected.
		/// </summary>
		/// <param name="name">The name of the member to find.</param>
		/// <returns>The nearest accessible member with the specified name declared in a base type of the current class,
		/// or null if no matching member could be found.</returns>
		private NamedMember GetBaseMember(string name, Class startType, out Class declType)
		{
			var type = startType;
			while (type != null)
			{
				var @class = type as Class;
				if (@class != null)
				{
					if (@class.members.ContainsKey(name) &&
						@class.members[name].Access != AccessLevel.Private)
					{
						declType = @class;
						return @class.members[name];
					}
				}
				type = (Class)type.BaseType;
			}

			declType = null;
			return null;
		}

		private void EnsureDeclarable(ParseNode errorNode, Method method)
		{
			// A method is declarable if:
			//   a) there is no public or protected method group with the same name
			//      in any base type; or,
			//
			//   b) there IS such a method group, but it has an overridable overload
			//      with the same signature as the method we're adding, OR the method
			//      can be added as an overload without ambiguity.
			//      An overload is overridable if it has the 'overridable' modifier,
			//      or if it's abstract. In addition, if the method we're declaring
			//      CAN override a base method, then it MUST be marked 'override'.
			//
			//   c) the method is a constructor method. They are not marked overridable,
			//      because they actually hide their base constructor siblings.
			//
			//   d) the method is an operator overload method. These cannot override
			//      their base method siblings, because they are static; however, they
			//      always succeed in hiding said siblings.
			//
			// Since static methods cannot have the overridable or abstract modifier,
			// it follows that static methods are not overridable.
			//
			// Unfortunately, we cannot stop looking once we've found a method in one
			// of the base types. Consider the following scenario:
			//     class A {
			//         public overridable foo() { }
			//     }
			//     class B is A {
			//         public foo(a)    { } // valid; overloads A.foo
			//     }
			//     class C is B {
			//         public foo()     { } // invalid: need override modifier to override A.foo
			//         public foo(a...) { } // invalid: overload is ambiguous with B.foo
			//         public foo(a, b) { } // valid; overloads B.foo and A.foo
			//     }
			// If we stopped searching as soon as we found a suitable base method, then
			// the first C.foo would be considered valid, as it would overload B.foo.

			if ((method.Flags & MemberFlags.Constructor) == MemberFlags.Constructor ||
				(method.Flags & MemberFlags.Operator) == MemberFlags.Operator)
				return; // always declarable!

			var hasOverridden = false;

			var name = method.Name;
			var baseStartType = this.BaseType as Class;
			NamedMember baseMember;
			do
			{
				Class baseMemberClass;
				baseMember = GetBaseMember(name, baseStartType, out baseMemberClass);

				if (baseMember != null)
				{
					// If the base member is not a method group, we can't declare this method.
					if (baseMember.Kind != MemberKind.MethodGroup)
						throw new DeclarationException(errorNode, string.Format("Cannot hide inherited member '{0}.{1}'.",
							baseMemberClass.FullName, name));

					// If the member is a method group, we MAY be able to override it!
					var baseGroup = (MethodGroup)baseMember;
					if (baseGroup.IsStatic != method.IsStatic)
						throw new DeclarationException(errorNode, string.Format("Static modifier mismatch with inherited member '{0}.{1}'.",
							baseMemberClass.FullName, name));

					var overrideCandidate = baseGroup.FindOverload(method.Signature);
					if (overrideCandidate != null)
					{
						if (!overrideCandidate.IsAbstract && !overrideCandidate.IsOverridable)
						{
							if (method.IsOverride)
								throw new DeclarationException(errorNode,
									string.Format("Cannot override non-overridable inherited method '{0}.{1}'.",
										baseMemberClass.FullName, name));
							throw new DeclarationException(errorNode,
								string.Format("Cannot hide inherited member '{0}.{1}'.",
									baseMemberClass.FullName, name));
						}
						else if (!method.IsOverride && !method.IsAutoOverride)
							throw new DeclarationException(errorNode,
								string.Format("Cannot hide inherited member '{0}.{1}'. Specify 'override' if the intent is to override it.",
									baseMemberClass.FullName, name));

						if (method.IsAutoOverride)
							method.IsOverride = true;

						if (method.Access == AccessLevel.None) // no explicitly declared accessibility
							method.Access = overrideCandidate.Access; // inherit from base method
						else if (method.Access != overrideCandidate.Access)
							throw new InconsistentAccessibilityException(errorNode,
								string.Format("Accessibility mismatch when overriding '{0}.{1}'.",
									baseMemberClass.FullName, name));

						// At this point:
						//   1. The override candidate is overridable or abstract; and
						//   2. The method is an override.
						//   3. The method has the correct accessibility.
						// So, we've successfully overridden an inherited method. Good job!
						hasOverridden = true;
						method.OverriddenBaseMethod = overrideCandidate;
						if (overrideCandidate.IsAbstract)
							inheritedAbstractMethods.Remove(overrideCandidate);
						// Now it's basically safe to assume the override candidate is a validly
						// declared method, in which case we don't need to check any base types
						// or check for ambiguous overloads or anything like that.
						// So we just break. Plain and simple.
						break;
					}
					if (!baseGroup.CanDeclare(method.Signature))
						throw new OverloadException(errorNode, method);

					baseStartType = baseStartType.BaseType as Class;
				}
			} while (baseMember != null);

			// At this point, we've either found a method to override (in which case we've
			// already verified method.IsOverride), or we haven't found one, in which case
			// method.IsOverride has not been checked. So we need to check that here.

			if (!hasOverridden)
			{
				if (method.IsAutoOverride)
					method.IsOverride = false;
				else if (method.IsOverride)
					throw new DeclarationException(errorNode, "Found no suitable method to override.");
			}
		}

		private void EnsureDeclarable(ParseNode errorNode, Property property)
		{
			// A propery is declarable if:
			//   a) There is no public or protected member with the same name in any base type.
			//
			//   b) If there is such a member, it must be a property, and it must be abstract or
			//      overridable, and have the same readability/writability as this property (i.e.
			//      readonly matched with readonly, writeonly with writeonly and read-write with
			//      read-write).
			//
			//   c) If this property overrides an inherited property, it must be marked with the
			//      'override' modifier.

			var name = property.Name;

			var hasOverridden = false;

			Class baseMemberClass;
			var baseMember = GetBaseMember(name, this.BaseType as Class, out baseMemberClass);
			if (baseMember != null)
			{
				if (baseMember.Kind != MemberKind.Property)
					throw new DeclarationException(errorNode,
						string.Format("Cannot hide inherited member '{0}.{1}'.",
							baseMemberClass.FullName, name));

				// At this point, baseMember is a property
				var baseProp = ((Property)baseMember);
				if (baseProp.IsStatic != property.IsStatic)
					throw new DeclarationException(errorNode,
						string.Format("Static modifier mismatch with inherited member '{0}.{1}'.",
							baseMemberClass.FullName, name));

				if (!baseProp.IsOverridable && !baseProp.IsAbstract)
					throw new DeclarationException(errorNode,
						string.Format("Cannot override inherited member '{0}.{1}'.",
							baseMemberClass.FullName, name));

				// property is overridable or abstract
				if (!property.IsOverride)
					throw new DeclarationException(errorNode,
						string.Format("Cannot hide inherited member '{0}.{1}'. Specify 'override' if the intent is to override it.",
							baseMemberClass.FullName, name));

				// Read-writeness must match base property.
				if (property.PropertyKind != baseProp.PropertyKind)
					throw new DeclarationException(errorNode,
						string.Format("Cannot override {0} property '{1}.{2}' with {3} property.",
							PropertyKindToString(baseProp.PropertyKind),
							baseMemberClass.FullName, name,
							PropertyKindToString(property.PropertyKind)));

				if (property.Access == AccessLevel.None)
				{
					property.Access = baseProp.Access;
					if (property.Getter != null)
						if (property.Getter.Access == AccessLevel.None)
							property.Getter.Access = baseProp.Access;
						else if (property.Getter.Access != baseProp.Access)
							throw new InconsistentAccessibilityException(errorNode,
								string.Format("Accessibility mismatch when overriding '{0}.get {1}'.",
									baseMemberClass.FullName, name));
					if (property.Setter != null)
						if (property.Setter.Access == AccessLevel.None)
							property.Setter.Access = baseProp.Access;
						else if (property.Setter.Access != baseProp.Access)
							throw new InconsistentAccessibilityException(errorNode,
								string.Format("Accessibility mismatch when overriding '{0}.set {1}'.",
									baseMemberClass.FullName, name));
				}
				else if (property.Access != baseProp.Access)
					throw new InconsistentAccessibilityException(errorNode,
						string.Format("Accessibility mismatch when overriding '{0}.{1}'.",
							baseMemberClass.FullName, name));

				hasOverridden = true;
			}

			if (!hasOverridden && property.IsOverride)
				throw new DeclarationException(errorNode, "Found no suitable property to override.");
		}

		private void OverrideIfPossible(Method method)
		{
			// This is different from EnsureDeclarable: here, we only override if we can,
			// and we don't throw if the method declaration is invalid.
			if ((method.Flags & MemberFlags.Constructor) == MemberFlags.Constructor ||
				method.IsStatic)
				return; // Constructors do not override, and static methods cannot be overrides

			Class _;
			var baseMember = GetBaseMember(method.Name, this.BaseType as Class, out _);
			if (baseMember != null && baseMember.Kind == MemberKind.MethodGroup)
			{
				var overrideCandidate = ((MethodGroup)baseMember).FindOverload(method.Signature);
				method.IsOverride = overrideCandidate != null &&
					(overrideCandidate.IsAbstract || overrideCandidate.IsOverridable);
			}
		}

		private string PropertyKindToString(PropertyKind kind)
		{
			switch (kind)
			{
				case PropertyKind.ReadOnly: return "read-only";
				case PropertyKind.WriteOnly: return "write-only";
				case PropertyKind.ReadWrite: return "read-write";
				default: return "[invalid]";
			}
		}

		internal string GetLambdaParam()
		{
			return "<λc>arg$";
		}

		internal string GetLambdaName(string nameHint)
		{
			return string.Format("<λc>{0}${1}", nameHint ?? "<anon>", lambdaNameCounter++);
		}

		private enum ClassState
		{
			NotInited,
			Initing,
			Inited,
		}
	}

	public abstract class ClassMember : NamedMember
	{
		public ClassMember(string name, MemberKind kind, ParseNode node, AccessLevel access, Class parent)
			: base(name, kind, node, access)
		{
			Parent = parent;
		}

		/// <summary>
		/// Gets the class that contains the member.
		/// </summary>
		public Class Parent { get; internal set; }

		public string FullName { get { return Parent.FullName + "." + this.Name; } }

		private MemberFlags flags;

		public bool IsStatic
		{
			get { return (flags & MemberFlags.Instance) != MemberFlags.Instance; }
			internal set { ToggleFlag(MemberFlags.Instance, !value); }
		}
		public bool IsImplDetail
		{
			get { return (flags & MemberFlags.ImplDetail) == MemberFlags.ImplDetail; }
			internal set { ToggleFlag(MemberFlags.ImplDetail, !value); }
		}

		protected bool HasFlag(MemberFlags flag)
		{
			return (flags & flag) == flag;
		}
		protected void ToggleFlag(MemberFlags flag, bool on)
		{
			if (on)
				flags |= flag;
			else
				flags &= ~flag;
		}

		//protected static AccessLevel GetDeclaredAccessibility(Member mem)
		//{
		//	if (mem.Access == AccessLevel.None)
		//	{
		//		var t = mem.Kind;
		//		if (t == MemberKind.Iterator || t == MemberKind.Operator)
		//			return AccessLevel.Public;

		//		// All other class members (methods, fields, property accessors, constructors, etc.) default to private.
		//		return AccessLevel.Private;
		//	}

		//	return mem.Access;
		//}

		public override Namespace GetContainingNamespace()
		{
			return Parent.GetContainingNamespace();
		}
	}

	public class ClassMemberMethod : Method
	{
		public ClassMemberMethod(string name, NamedMember owner, AccessLevel access, Statement body, Splat splat, params Parameter[] parameters)
			: base(name, access, body, splat, parameters)
		{
			this.owner = owner;
		}
		public ClassMemberMethod(string name, NamedMember owner, AccessLevel access, Statement body, Signature signature)
			: base(name, access, body, signature)
		{
			this.owner = owner;
		}

		private NamedMember owner;
		public NamedMember Owner { get { return owner; } internal set { owner = value; } }
	}

	public class Field : ClassMember
	{
		public Field(VariableDeclarator node, AccessLevel access, Class parent)
			: base(node.Name, MemberKind.Field, node, access, parent)
		{ }
		public Field(string name, AccessLevel access, Class parent)
			: base(name, MemberKind.Field, null, access, parent)
		{ }

		internal uint Id;
	}

	public class ClassConstant : ClassMember, IConstantMember
	{
		public ClassConstant(VariableDeclarator node, AccessLevel access, Class parent) :
			base(node.Name, MemberKind.Constant, node, access, parent)
		{ }
		protected ClassConstant(string name, AccessLevel access, Class parent)
			: base(name, MemberKind.Constant, null, access, parent)
		{
			state = ConstantState.HasValue;
		}

		private ConstantState state;
		public ConstantState State { get { return state; } }

		/// <summary>Gets the value associated with this constant.</summary>
		public virtual ConstantValue Value
		{
			get
			{
				var varDecl = (VariableDeclarator)this.Node;
				if (state == ConstantState.Folding)
					throw new CompileTimeException(varDecl, "Circular constant definition detected.");
				if (state == ConstantState.NotFolded)
				{
					state = ConstantState.Folding;
					varDecl.FoldConstant(true);
					state = ConstantState.HasValue;
				}

				return ((ConstantExpression)varDecl.Value).Value;
			}
		}

		internal uint Id;
	}

	public class ImportedClassConstant : ClassConstant
	{
		public ImportedClassConstant(string name, AccessLevel access, Class parent, ConstantValue value)
			: base(name, access, parent)
		{
			this.value = value;
		}

		private ConstantValue value;
		public override ConstantValue Value { get { return value; } }

		internal void UpdateValue(ConstantValue value)
		{
			this.value = value;
		}
	}

	public class Property : ClassMember
	{
		public Property(string name, Class parent)
			: base(name, MemberKind.Property, null, AccessLevel.None, parent)
		{ }

		private PropertyAccessor getter, setter;

		public bool IsOverridable
		{
			get { return HasFlag(MemberFlags.Overridable); }
			internal set { ToggleFlag(MemberFlags.Overridable, value); }
		}
		public bool IsAbstract
		{
			get { return HasFlag(MemberFlags.Abstract); }
			internal set { ToggleFlag(MemberFlags.Abstract, value); }
		}
		public bool IsOverride
		{
			get { return HasFlag(MemberFlags.Override); }
			internal set { ToggleFlag(MemberFlags.Override, value); }
		}

		/// <summary>Gets the getter associated with the property.</summary>
		public PropertyAccessor Getter
		{
			get { return getter; }
			internal set
			{
				if (getter != null)
					throw new DeclarationException(getter.Node, "The property already has a getter.");
				if (setter != null)
					ValidateAccessors(value, setter);
				SetField(value, out getter);
			}
		}
		/// <summary>Gets the getter associated with the property.</summary>
		public PropertyAccessor Setter
		{
			get { return setter; }
			internal set
			{
				if (setter != null)
					throw new DeclarationException(setter.Node, "The property already has a setter.");
				if (getter != null)
					ValidateAccessors(getter, value);
				SetField(value, out setter);
			}
		}

		internal uint GetterId
		{
			get { return getter == null ? 0 : getter.Method.Group.Id; }
		}

		internal uint SetterId
		{
			get { return setter == null ? 0 : setter.Method.Group.Id; }
		}

		public PropertyKind PropertyKind
		{
			get
			{
				return (getter == null ? 0 : PropertyKind.Read)
					|
					(setter == null ? 0 : PropertyKind.Write);
			}
		}

		private void SetField(PropertyAccessor value, out PropertyAccessor field)
		{
			var method = value.Method;

			this.Access = method.Access;
			this.IsAbstract = method.IsAbstract;
			this.IsOverridable = method.IsOverridable;
			this.IsOverride = method.IsOverride;
			this.IsStatic = method.IsStatic;

			value.Parent = this;

			field = value;
		}

		internal static void ValidateAccessors(PropertyAccessor getter, PropertyAccessor setter)
		{
			// Since the getter and setter MAY have no declared accessibility while being overrides,
			// we wait with checking the accessibility until we've found the correct methods to override.
			//if (GetDeclaredAccessibility(getter) != GetDeclaredAccessibility(setter))
			//	throw new InconsistentAccessibilityException(
			//		"The getter and setter of a property accessor must both have the same declared accessibility.");

			var getterMethod = getter.Method;
			var setterMethod = setter.Method;

			if (getterMethod.IsAbstract != setterMethod.IsAbstract)
				throw new DeclarationException(getter.Node, "If either property accessor is marked abstract, then both must be.");

			if (getterMethod.IsOverridable != setterMethod.IsOverridable)
				throw new DeclarationException(getter.Node, "If either property accessor is marked overridable, then both must be.");

			if (getterMethod.IsOverride != setterMethod.IsOverride)
				throw new DeclarationException(getter.Node, "If either property accessor is marked override, then both must be.");

			if (getterMethod.IsStatic != setterMethod.IsStatic)
				throw new DeclarationException(getter.Node, "If either property accessor is marked static, then both must be.");
		}
	}

	[Flags]
	public enum PropertyKind
	{
		Read = 1,
		Write = 2,
		ReadOnly = Read,
		WriteOnly = Write,
		ReadWrite = Read | Write,
	}

	public class PropertyAccessor : NamedMember
	{
		public PropertyAccessor(PropertyAccessorDeclaration node)
			: base(node.Name, node.IsSetter ? MemberKind.PropertySetter : MemberKind.PropertyGetter, node, node.Access)
		{
			Parameter[] parameters = null;
			if (node.IsSetter)
				parameters = new Parameter[]
				{
					new Parameter("value", null)
					{
						StartIndex = node.StartIndex,
						EndIndex = node.EndIndex,
					}
				};

			isSetter = node.IsSetter;
			method = new ClassMemberMethod(GetAccessorMethodName(node.IsSetter, node.Name),
				this, node.Access, node.Body, Splat.None, parameters)
				{
					IsStatic = node.IsStatic,
					IsAbstract = node.IsAbstract,
					IsOverridable = node.IsOverridable,
					IsOverride = node.IsOverride,
					IsImplDetail = true,
				};
		}
		public PropertyAccessor(ClassMemberMethod method, bool isSetter)
			: base(method.Name, isSetter ? MemberKind.PropertySetter : MemberKind.IndexerGetter, null, method.Access)
		{
			this.method = method;
			this.isSetter = isSetter;
			method.Owner = this;
		}

		public Property Parent { get; internal set; }

		public Class Owner { get { return Parent == null ? null : Parent.Parent; } }

		private ClassMemberMethod method;
		/// <summary>Gets the method that implements the property accessor.</summary>
		public ClassMemberMethod Method { get { return method; } }

		private bool isSetter;
		/// <summary>Gets a value indicating whether the accessor is a getter.</summary>
		public bool IsGetter { get { return !isSetter; } }
		/// <summary>Gets a value indicating whether the accessor is a setter.</summary>
		public bool IsSetter { get { return isSetter; } }

		public override Namespace GetContainingNamespace()
		{
			return Owner.GetContainingNamespace();
		}

		public void InitBody(Compiler compiler)
		{
			method.InitBody(compiler);
			if (isSetter)
			{
				if (method.IsGenerator)
					throw new CompileTimeException(method.Yields[0], "A property setter may not be a generator.");

				Func<ReturnStatement, bool> hasReturnValue = ret => ret.ReturnValues.Count > 0;
				if (method.Returns != null && method.Returns.Any(hasReturnValue))
					throw new CompileTimeException(method.Returns.First(hasReturnValue),
						"A property setter can only contain empty return statements.");
			}
		}

		internal static string GetAccessorMethodName(bool isSetter, string propName)
		{
			return string.Format(isSetter ? SetterNameFormat : GetterNameFormat, propName);
		}

		internal const string GetterNameFormat = "get<{0}>";
		internal const string SetterNameFormat = "set<{0}>";
	}

	public class Indexer : ClassMember
	{
		public Indexer(Class parent)
			: base(".item", MemberKind.Property, null, AccessLevel.None, parent)
		{ }

		private MethodGroup getterGroup, setterGroup;

		public MethodGroup Getter { get { return getterGroup; } }
		public MethodGroup Setter { get { return setterGroup; } }

		internal uint GetterId
		{
			get { return getterGroup == null ? 0 : getterGroup.Id; }
		}
		internal uint SetterId
		{
			get { return setterGroup == null ? 0 : setterGroup.Id; }
		}

		public void AddAccessor(IndexerAccessor accessor)
		{
			if (accessor.IsGetter)
			{
				if (getterGroup != null && getterGroup.Name != accessor.Method.Name)
					throw new ArgumentException("The indexer getter must have the same name as the other getters.", "accessor");
				getterGroup = Parent.DeclareMethod(accessor.Method);
			}
			else
			{
				if (setterGroup != null && setterGroup.Name != accessor.Method.Name)
					throw new ArgumentException("The indexer setter must have the same name as the other setters.", "accessor");
				setterGroup = Parent.DeclareMethod(accessor.Method);
			}
		}

		internal void SetAccessors(MethodGroup getter, MethodGroup setter)
		{
			this.getterGroup = getter;
			this.setterGroup = setter;
		}
	}

	public class IndexerAccessor : NamedMember
	{
		public IndexerAccessor(IndexerAccessorDeclaration node)
			: base(".item", node.IsSetter ? MemberKind.IndexerSetter : MemberKind.IndexerGetter, node, node.Access)
		{
			var parameters = new Parameter[node.Parameters.Count + (node.IsSetter ? 1 : 0)];
			for (var i = 0; i < node.Parameters.Count; i++)
				parameters[i] = node.Parameters[0];
			if (node.IsSetter)
				parameters[node.Parameters.Count] = new Parameter("value", null)
				{
					StartIndex = node.StartIndex,
					EndIndex = node.EndIndex,
				};

			this.isSetter = node.IsSetter;
			method = new ClassMemberMethod(PropertyAccessor.GetAccessorMethodName(node.IsSetter, ".item"),
				this, node.Access, node.Body, Splat.None, parameters)
				{
					IsAbstract = node.IsAbstract,
					IsOverridable = node.IsOverridable,
					IsOverride = node.IsOverride,
					IsStatic = false,
					IsImplDetail = true,
				};
		}

		public Indexer Parent { get; internal set; }
		public Class Owner { get { return Parent == null ? null : Parent.Parent; } }

		private ClassMemberMethod method;
		/// <summary>Gets the method that implements the property accessor.</summary>
		public ClassMemberMethod Method { get { return method; } }

		private bool isSetter;
		/// <summary>Gets a value indicating whether the accessor is a getter.</summary>
		public bool IsGetter { get { return !isSetter; } }
		/// <summary>Gets a value indicating whether the accessor is a setter.</summary>
		public bool IsSetter { get { return isSetter; } }

		public override Namespace GetContainingNamespace()
		{
			return Owner.GetContainingNamespace();
		}

		public void InitBody(Compiler compiler)
		{
			method.InitBody(compiler);
			if (isSetter)
			{
				if (method.IsGenerator)
					throw new CompileTimeException(method.Yields[0], "An indexer setter may not be a generator.");

				Func<ReturnStatement, bool> hasReturnValue = ret => ret.ReturnValues.Count > 0;
				if (method.Returns != null && method.Returns.Any(hasReturnValue))
					throw new CompileTimeException(method.Returns.First(hasReturnValue),
						"An indexer setter can only contain empty return statements.");
			}
		}
	}

	public class Constructor : ClassMember
	{
		public Constructor(ConstructorDeclaration node, Class parent)
			: base(node.IsStatic ? ".init" : ".new", MemberKind.Constructor, node, node.Access, parent)
		{
			method = new ClassMemberMethod(node.IsStatic ? ".init" : ".new", this, node.Access,
				node.Body, new Signature(node.Parameters, node.Splat))
				{
					Flags = node.IsStatic ? MemberFlags.Constructor : MemberFlags.InstanceConstructor,
				};
			this.IsStatic = node.IsStatic;

			var parameters = new Parameter[node.Parameters.Count];
			for (var i = 0; i < node.Parameters.Count; i++)
			{
				var param = node.Parameters[i];
				if (!param.HasThisPrefix)
				{
					method.Body.DeclareVariable(new Variable(param.Name, param));
					parameters[i] = param;
				}
				else
					parameters[i] = param;
			}
			method.Parameters = parameters;
		}
		public Constructor(Block body, AccessLevel access, Class parent, Splat splat, params Parameter[] parameters)
			: base(".new", MemberKind.Constructor, null, access, parent)
		{
			method = new ClassMemberMethod(".new", this, access, body, splat, parameters)
			{
				Flags = MemberFlags.InstanceConstructor,
			};
			this.IsStatic = false;
		}
		public Constructor(Block body, AccessLevel access, Class parent, Signature signature)
			: base(".new", MemberKind.Constructor, null, access, parent)
		{
			method = new ClassMemberMethod(".new", this, access, body, signature)
			{
				Flags = MemberFlags.InstanceConstructor,
			};
			this.IsStatic = false;
		}
		public Constructor(Block body, Class parent)
			: base(".init", MemberKind.Constructor, null, AccessLevel.Private, parent)
		{
			method = new ClassMemberMethod(".init", this, AccessLevel.Private, body, Signature.Empty)
			{
				Flags = MemberFlags.Constructor,
			};
			this.IsStatic = true;
		}

		private ClassMemberMethod method;
		/// <summary>
		/// Gets the method that contains the constructor implementation.
		/// </summary>
		public ClassMemberMethod Method { get { return method; } }

		public void InitBody(Compiler compiler)
		{
			method.InitBody(compiler);
			if (method.IsGenerator)
				throw new CompileTimeException(method.Yields[0], "Constructors are not allowed to be generators.");

			Func<ReturnStatement, bool> hasReturnValue = ret => ret.ReturnValues.Count > 0;
			if (method.Returns != null && method.Returns.Any(hasReturnValue))
				throw new CompileTimeException(method.Returns.First(hasReturnValue),
					"Constructors can only contain empty return statements.");
		}
	}

	public class Iterator : ClassMember
	{
		public Iterator(IteratorDeclaration node, Class parent)
			: base(".iter", MemberKind.Iterator, node, AccessLevel.None, parent)
		{
			method = new ClassMemberMethod(".iter", this, AccessLevel.Public, node.Body, Splat.None, new Parameter[0])
			{
				Flags = MemberFlags.Instance | MemberFlags.AutoOverride | MemberFlags.Overridable | MemberFlags.ImplDetail,
			};
		}

		private ClassMemberMethod method;
		/// <summary>Gets the method that contains the iterator implementation.</summary>
		public ClassMemberMethod Method { get { return method; } }
	}

	public class OperatorOverload : ClassMember
	{
		public OperatorOverload(BinaryOperatorOverload node, Class parent)
			: base(null, MemberKind.Operator, node, AccessLevel.Public, parent)
		{
			index = node.GetIndex();

			Parameter[] parameters = {
				new Parameter(node.Left, null),
				new Parameter(node.Right, null),
			};
			method = new ClassMemberMethod(node.GetMethodName(), this, AccessLevel.Public, node.Body, Splat.None, parameters)
			{
				Flags = MemberFlags.Operator,
			};
		}

		public OperatorOverload(UnaryOperatorOverload node, Class parent)
			: base(null, MemberKind.Operator, node, AccessLevel.Public, parent)
		{
			index = node.GetIndex();

			Parameter[] parameters = { new Parameter(node.Operand, null) };
			method = new ClassMemberMethod(node.GetMethodName(), this, AccessLevel.Public, node.Body, Splat.None, parameters)
			{
				Flags = MemberFlags.Operator,
			};
		}

		internal OperatorOverload(int index, Class parent, ClassMemberMethod method)
			: base(null, MemberKind.Operator, null, AccessLevel.Public, parent)
		{
			this.index = index;
			this.method = method;
			method.Owner = this;
		}

		private ClassMemberMethod method;
		/// <summary>Gets the method that contains the operator implementation.</summary>
		public ClassMemberMethod Method { get { return method; } }

		private int index;
		/// <summary>Gets the operator index of this overload.</summary>
		public int Index { get { return index; } }
	}

	public class ClosureClass : Class
	{
		public ClosureClass(string name, BlockSpace parentBlock, Namespace parent, Compiler compiler)
			: base(name, AccessLevel.None, parent)
		{
			BaseType = compiler.ObjectType;

			this.parentBlock = parentBlock;
		}

		private BlockSpace parentBlock;
		public BlockSpace ParentBlock { get { return parentBlock; } }

		// Blocks may be captured by more than one block, so we don't store a capture field in the block.
		private Dictionary<BlockSpace, Field> blockToField;
		private Field thisField;
		public Field ThisField { get { return thisField; } }

		public Field GetBlockField(BlockSpace block)
		{
			if (blockToField != null && blockToField.ContainsKey(block))
					return blockToField[block];

			return null;
		}

		public Field DeclareVariableField(Variable variable)
		{
			if (variable.CaptureField == null)
			{
				variable.CaptureField = new Field(GetFieldName(variable), AccessLevel.Public, this)
				{
					IsStatic = false,
				};
				DeclareField(variable.CaptureField);
			}

			return variable.CaptureField;
		}

		public Method DeclareFunctionMethod(LocalFunction function)
		{
			if (function.Method.Group == null)
			{
				function.Method.Name = GetMethodName(function);
				DeclareMethod(function.Method);
			}

			return function.Method;
		}

		public Field DeclareBlockField(BlockSpace block)
		{
			if (blockToField == null)
				blockToField = new Dictionary<BlockSpace, Field>();
			else if (blockToField.ContainsKey(block))
				return blockToField[block];

			var field = new Field(GetFieldName(block), AccessLevel.Public, this);
			field.IsStatic = false;

			DeclareField(field);
			blockToField[block] = field;

			return field;
		}

		public Field DeclareThisField()
		{
			if (thisField == null)
			{
				thisField = new Field(ThisFieldName, AccessLevel.Public, this)
				{
					IsStatic = false,
				};
				DeclareField(thisField);
			}

			return thisField;
		}

		internal static string GetFieldName(Variable variable)
		{
			return string.Format("<var>{0}", variable.Name);
		}

		internal static string GetFieldName(BlockSpace block)
		{
			return string.Format("<scope>block_{0}", block.BlockNumber);
		}

		private static string GetMethodName(LocalFunction function)
		{
			if (function.Name[0] == '<') // captured lambda
				return function.Name;
			else
				return string.Format("<ƒ>{0}", function.Name);
		}

		internal const string ThisFieldName = "<>this";
	}

	public class GeneratorClass : Class
	{
		public GeneratorClass(string name, Method owner, Namespace parent, Compiler compiler)
			: base(name, AccessLevel.Private, parent)
		{
			BaseType = compiler.IteratorType;

			InitInheritedAbstractMethods();

			AddDefaultMembers();
		}

		private Field stateField;
		public Field StateField { get { return stateField; } }

		private Field currentValueField;
		public Field CurrentValueField { get { return currentValueField; } }

		private Field thisField;
		public Field ThisField { get { return thisField; } }

		private List<AnonField> anonFields;

		private void AddDefaultMembers()
		{
			stateField = new Field(StateFieldName, AccessLevel.Private, this)
			{
				IsStatic = false,
				IsImplDetail = true,
			};
			DeclareField(stateField);

			currentValueField = new Field(CurrentValueFieldName, AccessLevel.Private, this)
			{
				IsStatic = false,
				IsImplDetail = true,
			};
			DeclareField(currentValueField);

			var currentValueGetterMethod = new ClassMemberMethod(PropertyAccessor.GetAccessorMethodName(false, "current"),
				null, AccessLevel.Public,
				new Block(
					new ReturnStatement(
						new InstanceMemberAccess(new ThisAccess(), this, currentValueField)
					)
				), Signature.Empty)
			{
				IsStatic = false,
				IsImplDetail = true,
				IsOverride = true,
			};

			var currentValueGetter = new PropertyAccessor(currentValueGetterMethod, false);

			DeclareProperty(new Property("current", this) { Getter = currentValueGetter });

			AddConstructor();
		}

		private void AddConstructor()
		{
			// Find the base constructor
			var baseCtor = BaseType.FindConstructor(null, 0, this);

			// Add a statement to this constructor body that calls the base constructor
			var ctorBody = new List<Statement>
			{
				new ExpressionStatement(
					new InvocationExpression( // new base();
						new InstanceMemberAccess(new ThisAccess(), (Class)BaseType, baseCtor.Group), // base.'.new'
						new List<Expression>() // ()
					)
				),
				new ExpressionStatement(
					new AssignmentExpression( // this.'<>state' = -1
						new InstanceMemberAccess(new ThisAccess(), (Class)BaseType, stateField) { IsAssignment = true },
						new ConstantExpression(ConstantValue.CreateInt(-1))
					) { IgnoreValue = true }
				),
			};
			var ctor = new Constructor(new Block(ctorBody), AccessLevel.Public, this, Signature.Empty);
			DeclareConstructor(ctor);
		}

		// Note: Different Variable instances may end up in the same field
		// if they have the same name. This is OK. If that happens, they're
		// from different scopes, and are not accessible from each other's
		// locations. Note that if they're captured, they don't end up here
		// in the first place.

		public Field DeclareVariableField(Variable variable)
		{
			if (variable.CaptureField == null || variable.CaptureField.Parent != this)
			{
				var name = ClosureClass.GetFieldName(variable);
				if (!members.ContainsKey(name))
				{
					var field = new Field(name, AccessLevel.Public, this)
					{
						IsStatic = false,
					};
					DeclareField(field);
				}
				variable.CaptureField = (Field)members[name];
			}

			return variable.CaptureField;
		}

		public Field DeclareAnonField(BlockSpace scope)
		{
			if (anonFields == null)
				anonFields = new List<AnonField>();

			for (var i = 0; i < anonFields.Count; i++)
				if (!anonFields[i].ScopeContains(scope))
				{
					// Reuse the same field, because there is no scope overlap
					var field = anonFields[i];
					field.Scopes.Add(scope);
					return field.Field;
				}

			{
				var field = new Field(GetAnonFieldName(anonFields.Count), AccessLevel.Public, this);
				field.IsStatic = false;
				field.IsImplDetail = true;
				DeclareField(field);
				anonFields.Add(new AnonField(field, scope));
				return field;
			}
		}

		public Field DeclareThisField()
		{
			if (thisField == null)
			{
				thisField = new Field(ClosureClass.ThisFieldName, AccessLevel.Public, this)
				{
					IsStatic = false,
				};
				DeclareField(thisField);
			}

			return thisField;
		}

		private static string GetAnonFieldName(int counter)
		{
			return string.Format("<anon>__{0}", counter);
		}

		private const string StateFieldName = "<>state";
		private const string CurrentValueFieldName = "<>current";

		private struct AnonField
		{
			public AnonField(Field field, BlockSpace firstScope)
			{
				Field = field;
				Scopes = new List<BlockSpace> { firstScope };
			}

			public Field Field;
			public List<BlockSpace> Scopes;

			/// <summary>
			/// Determines whether any of the scopes in this anonymous field
			/// contains the specified scope.
			/// </summary>
			/// <param name="otherScope">The scope to test against.</param>
			/// <returns>True if <paramref name="otherScope"/> is contained within
			/// any of the scopes in this AnonField; otherwise, false.</returns>
			public bool ScopeContains(BlockSpace otherScope)
			{
				for (var i = 0; i < Scopes.Count; i++)
				{
					var current = Scopes[i];
					var temp = otherScope;

					do
					{
						if (temp == current)
							return true;
						temp = temp.Parent;
					} while (temp != null);
				}

				return false;
			}
		}
	}
}