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
		protected Type(string name, MemberKind kind, Accessibility access, Namespace parent)
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
		public override string FullName
		{
			get
			{
				return parent == null || parent.Name == null ? this.Name :
					parent.FullName + "." + this.Name;
			}
		}

		IDeclarationSpace IDeclarationSpace.Parent { get { return parent; } }

		public abstract bool ContainsMember(string name);

		/// <summary>
		/// Determines whether this type is or inherits from another type.
		/// </summary>
		/// <param name="other">The type to test against.</param>
		/// <returns>True if this type is or inherits from <paramref name="other"/>; otherwise, false.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="other"/> is null.</exception>
		public bool InheritsFrom(Type other)
		{
			if (other == null)
				throw new ArgumentNullException("other");

			var t = this;
			do
			{
				if (t == other)
					return true;
				t = t.BaseType;
			} while (t != null);

			return false;
		}

		public NamedMember GetMember(string name, Type instType, Type fromType)
		{
			NamedMember _;
			return GetMember(name, instType, fromType, out _);
		}

		public NamedMember GetMember(string name, Type instType, Type fromType, out NamedMember inaccessibleMember)
		{
			inaccessibleMember = null;

			var type = this;
			while (type != null)
			{
				var mem = type.GetMember(name);
				if (mem != null)
				{
					Type declType;
					if (mem is ClassMember)
					{
						if (mem.Access == Accessibility.Protected &&
							mem.Kind == MemberKind.Property)
						{
							var prop = (Property)mem;
							var method = (prop.Getter != null ? prop.Getter.Method :
								prop.Setter.Method).Group;
							while (method.BaseGroup != null)
								method = method.BaseGroup;
							declType = method.ParentAsClass;
						}
						else
							declType = ((ClassMember)mem).Parent;
					}
					else if (mem is EnumField)
						declType = ((EnumField)mem).Parent;
					else if (mem is MethodGroup)
					{
						var method = (MethodGroup)mem;
						if (mem.Access == Accessibility.Protected)
						{
							while (method.BaseGroup != null)
								method = method.BaseGroup;
						}
						declType = method.ParentAsClass;
					}
					else
						throw new Exception("Type contains an invalid member type.");

					if (IsAccessible(mem.Access, instType: instType, declType: declType, fromType: fromType))
					{
						inaccessibleMember = null;
						return mem;
					}
					else
						inaccessibleMember = mem;
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

		public abstract Method FindConstructor(ParseNode errorNode, int argCount, Type instClass, Type fromClass);

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
		public static bool IsAccessible(Accessibility access, Type instType, Type declType, Type fromType)
		{
			// Note: this algorithm is basically lifted straight from Ovum,
			// though it does not take shared types into account (because all
			// accessibility checking is done before the compiler extracts
			// closure classes and the like, i.e. the things that make use
			// of type sharing).
			if (access == Accessibility.Private)
				return fromType != null && (declType == fromType || declType == fromType.SharedType);
			else if (access == Accessibility.Protected)
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
			else if (access == Accessibility.Internal)
			{
				return declType.Module == null || !declType.Module.Imported;
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
		public Enum(string name, Accessibility access, bool isSet, Namespace parent)
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

		public override Method FindConstructor(ParseNode errorNode, int argCount, Type instType, Type fromType)
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
			: base(node.Name, MemberKind.EnumField, node, Accessibility.Public)
		{
			Parent = parent;
			node.Field = this;
		}
		public EnumField(string name, long value, Enum parent)
			: base(name, MemberKind.EnumField, null, Accessibility.Public)
		{
			this.state = ConstantState.HasValue;
			this.value = ConstantValue.CreateEnumValue(value, parent);
		}

		public override string FullName
		{
			get { return Parent.FullName + "." + this.Name; }
		}

		private ConstantState state;
		public ConstantState State { get { return state; } internal set { state = value; } }

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
		public Class(string name, Accessibility access, Namespace parent)
			: base(name, MemberKind.Class, access, parent)
		{
			state = ClassState.Inited;
		}

		private ClassState state = ClassState.NotInited;

		public bool IsInheritable { get; internal set; }
		public bool IsAbstract { get; internal set; }
		public bool IsStatic { get; internal set; }

		public bool CanInherit { get { return IsInheritable || IsAbstract; } }

		private MethodGroup constructors;
		/// <summary>Gets the method group that corresponds to the constructors of the class.</summary>
		public MethodGroup Constructors { get { return constructors; } internal set { constructors = value; } }

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
		internal const int OperatorCount = 16;
		internal OperatorOverload[] operators = new OperatorOverload[OperatorCount];

		private int overloadedOperatorCount = 0;
		/// <summary>
		/// Gets the total number of operator overloads that this class declares.
		/// </summary>
		public int OverloadedOperatorCount { get { return overloadedOperatorCount; } }

		private int lambdaNameCounter = 0;
		private int closureClassCounter = 0;

		/// <summary>
		/// Set of inherited abstract methods which have not been overridden in this class.
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
				throw new CompileTimeException(Node, "Cyclic class declaration (class indirectly inherits from itself).");

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
				string name;
				PropertyAccessor accessor;
				if (prop is IndexerAccessorDeclaration)
				{
					var idxDecl = (IndexerAccessorDeclaration)prop;
					name = Indexer.MemberName + "`" + idxDecl.Parameters.Length.ToStringInvariant();
					accessor = new IndexerAccessor(idxDecl);
				}
				else
				{
					name = prop.Name;
					accessor = new PropertyAccessor(prop);
				}

				Property property;
				if (!properties.TryGetValue(name, out property))
				{
					property = prop is IndexerAccessorDeclaration ?
						new Indexer(this, ((IndexerAccessorDeclaration)prop).Parameters.Length) :
						new Property(name, this);
					properties.Add(name, property);
				}

				if (prop.IsSetter)
					property.Setter = accessor;
				else
					property.Getter = accessor;
			}

			foreach (var prop in properties.Values)
			{
				if (prop.Kind == MemberKind.Indexer)
					DeclareIndexer((Indexer)prop);
				else
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
								// But since we'll never run into this method again, might as well remove it.
								overriddenMethods.Remove(method);
							else if (method.IsAbstract)
								inheritedAbstractMethods.Add(method);

							if (method.IsOverride &&
								method.OverriddenBaseMethod.IsAbstract)
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

			if (ctor.Access == Accessibility.Private && this.CanInherit)
				throw new DeclarationException(ctor.Node, string.Format(
					"The class '{0}' is inheritable or abstract, and therefore cannot declare private constructors.",
					FullName));

			constructors = DeclareMethod(ctor.Method);
		}

		public void DeclareField(Field field)
		{
			if (field == null)
				throw new ArgumentNullException("name");

			if (this.IsStatic && !field.IsStatic)
				throw new DeclarationException(field.Node, "Static classes can only declare static members.");
			if (this.IsPrimitive && !field.IsStatic)
				throw new DeclarationException(field.Node, "Primitive types cannot declare instance fields.");

			var name = field.Name;
			Class baseMemberClass;
			var baseMember = GetBaseMember(name, this.BaseType as Class, out baseMemberClass);
			if (baseMember != null)
				throw new DeclarationException(field.Node, string.Format("Cannot hide inherited member '{0}.{1}'.",
					baseMemberClass.FullName, name));

			if (members.ContainsKey(field.Name))
				throw new DuplicateNameException(field.Node, field.Name);

			if (field.Access == Accessibility.None)
				field.Access = Accessibility.Private;

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

			if (constant.Access == Accessibility.None)
				constant.Access = Accessibility.Private;

			members.Add(constant.Name, constant);
			constant.Parent = this;
		}

		public void DeclareProperty(Property property)
		{
			if (property == null)
				throw new ArgumentNullException("property");

			if (this.IsStatic && !property.IsStatic)
				throw new DeclarationException(property.GetAccessorNode(),
					"Static classes can only contain static members.");
			if (!this.IsAbstract && property.IsAbstract)
				throw new DeclarationException(property.GetAccessorNode(),
					"Only abstract classes may declare abstract members.");
			if (!this.CanInherit && property.IsOverridable)
				throw new DeclarationException(property.GetAccessorNode(),
					"Only inheritable and abstract classes may declare overridable members.");

			EnsureDeclarable(property.GetAccessorNode(), property);

			if (members.ContainsKey(property.Name))
				throw new DuplicateNameException(property.GetAccessorNode(), property.Name,
					"There is already a member with the specified name in this class.");

			// These steps also update the accessibility of the accessors,
			// if they are overrides without a declared accessibility. We
			// must therefore declare the accessors before making sure they
			// have the same declared accessibility.
			if (property.Getter != null)
				DeclareMethod(property.Getter.Method);
			if (property.Setter != null)
				DeclareMethod(property.Setter.Method);

			if (property.Getter != null && property.Setter != null &&
				property.Getter.Method.Access != property.Setter.Method.Access)
				throw new DeclarationException(property.Getter.Node, "Both accessors must have the same declared accessibility.");

			property.Access = property.Getter != null ?
				property.Getter.Method.Access :
				property.Setter.Method.Access;

			members.Add(property.Name, property);
		}

		public void DeclareIndexer(Indexer indexer)
		{
			if (indexer == null)
				throw new ArgumentNullException("indexer");

			if (!this.IsAbstract && indexer.IsAbstract)
				throw new DeclarationException(indexer.GetAccessorNode(),
					"Only abstract classes may declare abstract members.");
			if (!this.CanInherit && indexer.IsOverridable)
				throw new DeclarationException(indexer.GetAccessorNode(),
					"Only inheritable and abstract classes may declare overridable members.");

			NamedMember member;
			IndexerMember indexerMember;
			if (!members.TryGetValue(Indexer.MemberName, out member))
				members.Add(Indexer.MemberName, indexerMember = new IndexerMember(this));
			else
				indexerMember = (IndexerMember)member;

			EnsureDeclarable(indexer.GetAccessorNode(), indexer);

			indexerMember.AddIndexer(indexer);

			// These steps also update the accessibility of the accessors,
			// if they are overrides without a declared accessibility. We
			// must therefore declare the accessors before making sure they
			// have the same declared accessibility.
			if (indexer.Getter != null)
				DeclareMethod(indexer.Getter.Method);
			if (indexer.Setter != null)
				DeclareMethod(indexer.Setter.Method);

			if (indexer.Getter != null && indexer.Setter != null &&
				indexer.Getter.Method.Access != indexer.Setter.Method.Access)
				throw new DeclarationException(indexer.Getter.Node, "Both accessors must have the same declared accessibility.");

			indexer.Access = indexer.Getter != null ?
				indexer.Getter.Method.Access :
				indexer.Setter.Method.Access;
		}

		public MethodGroup DeclareMethod(Method method)
		{
			if (method == null)
				throw new ArgumentNullException("method");
			if (method.Group != null)
				throw new ArgumentException("This method already belongs to another class or namespace.", "method");

			if (this.IsStatic && !method.IsStatic)
				throw new DeclarationException(method.Node, "Static classes can only declare static members.");
			if (!this.IsAbstract && method.IsAbstract)
				throw new DeclarationException(method.Node, "Only abstract classes may declare abstract members.");
			if (!this.CanInherit && method.IsOverridable && !method.IsImplDetail)
				throw new DeclarationException(method.Node, "Only inheritable and abstract classes may declare overridable members.");

			string name = method.Name;

			EnsureDeclarable(method.Node, method);
			if (method.Access == Accessibility.None)
				method.Access = Accessibility.Private;

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

			if ((group[0].Flags & MemberFlags.Constructor) == MemberFlags.None)
			{
				members[name] = group;
				Class _;
				var baseMember = GetBaseMember(name, this.BaseType as Class, out _);
				if (baseMember != null && baseMember.Kind == MemberKind.MethodGroup)
					group.BaseGroup = (MethodGroup)baseMember;
			}
		}

		internal void ImportIndexer(IndexerMember indexer)
		{
			if (this.members.ContainsKey(Indexer.MemberName))
				throw new DuplicateNameException(null, ".item",
					string.Format("The class '{0}' already has an indexer member.", this.FullName));

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
				if (((Class)baseClass).members.TryGetValue(name, out mem) && mem.Access != Accessibility.Private)
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

		public override Method FindConstructor(ParseNode errorNode, int argCount, Type instType, Type fromType)
		{
			if (!IsAccessible(constructors.Access, instType: instType, declType: this, fromType: fromType))
				throw new CompileTimeException(errorNode,
					string.Format("The constructor of '{0}' is not accessible from this location.",
						this.FullName));

			var ctor = constructors.FindOverload(argCount);
			if (ctor == null)
				throw new CompileTimeException(errorNode,
					string.Format("The type '{0}' does not declare a constructor that takes {1} arguments.",
						this.FullName, argCount.ToStringInvariant()));

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
		private static NamedMember GetBaseMember(string name, Class startType, out Class declType)
		{
			var type = startType;
			while (type != null)
			{
				var @class = type as Class;
				if (@class != null)
				{
					if (@class.members.ContainsKey(name) &&
						@class.members[name].Access != Accessibility.Private)
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
			//      (It is not, however, possible to overload protected method groups
			//      declared in a base class[1].)
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
			//
			// [1] Example:
			//     class A {
			//         protected overridable f() { }
			//     }
			//     class B is A {
			//         // The method group f originated in A, and this attempts
			//         // to overload it:
			//         protected f(x) { } // error
			//         // We can still override protected methods, though:
			//         overridable override f() { } // valid
			//     }
			//     class C is A {
			//         protected f(κ, λ) { } // same as before; can't overload f outside A
			//         override f() { } // still valid; overrides B.f
			//     }

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

						if (method.Access == Accessibility.None) // no explicitly declared accessibility
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
						if (overrideCandidate.IsAbstract && !this.IsAbstract)
							inheritedAbstractMethods.Remove(overrideCandidate);
						// Now it's basically safe to assume the override candidate is a validly
						// declared method, in which case we don't need to check any base types
						// or check for ambiguous overloads or anything like that.
						// So we just break. Plain and simple.
						break;
					}
					// We cannot add overloads to protected method groups outside
					// the originating class, that is, the class that introduced
					// the method group.
					if (baseGroup.Access == Accessibility.Protected)
					{
						// ... but we don't throw that error until we actually reach the base group.
						if (baseGroup.BaseGroup == null)
							throw new DeclarationException(errorNode,
								string.Format("Cannot add overload to protected method group '{0}.{1}' outside of the class '{0}'.",
									baseGroup.ParentAsClass.FullName, name));
					}
					else if (!baseGroup.CanDeclare(method.Signature))
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
			//      'override' modifier. Otherwise, if there is nothing to override, it must not
			//      have that modifier.

			var name = property.Name;

			var hasOverridden = false;

			Class baseMemberClass;
			var baseMember = GetBaseMember(name, this.BaseType as Class, out baseMemberClass);
			if (baseMember != null)
			{
				if (baseMember.Kind != MemberKind.Property)
					throw new DeclarationException(errorNode,
						string.Format("Cannot hide inherited member '{0}'.",
							baseMember.FullName));

				// At this point, baseMember is a property
				var baseProp = ((Property)baseMember);
				if (baseProp.IsStatic != property.IsStatic)
					throw new DeclarationException(errorNode,
						string.Format("Static modifier mismatch with inherited member '{0}'.",
							baseProp.FullName));

				if (!baseProp.IsOverridable && !baseProp.IsAbstract)
					throw new DeclarationException(errorNode,
						string.Format("Cannot override inherited member '{0}'.",
							baseProp.FullName));

				// baseProp is overridable or abstract
				if (!property.IsOverride)
					throw new DeclarationException(errorNode,
						string.Format("Cannot hide inherited member '{0}'. Specify 'override' if the intent is to override it.",
							baseProp.FullName));

				// Read-writeness must match base property.
				if (property.PropertyKind != baseProp.PropertyKind)
					throw new DeclarationException(errorNode,
						string.Format("Cannot override {0} property '{1}' with {2} property.",
							PropertyKindToString(baseProp.PropertyKind),
							baseProp.FullName,
							PropertyKindToString(property.PropertyKind)));

				if (property.Access == Accessibility.None)
				{
					property.Access = baseProp.Access;

					var getter = property.Getter;
					if (getter != null)
						if (getter.Access == Accessibility.None)
							getter.Access = baseProp.Access;
						else if (getter.Access != baseProp.Access)
							throw new InconsistentAccessibilityException(errorNode,
								string.Format("Accessibility mismatch when overriding '{0}.get {1}'.",
									baseMemberClass.FullName, name));

					var setter = property.Setter;
					if (setter != null)
						if (setter.Access == Accessibility.None)
							setter.Access = baseProp.Access;
						else if (setter.Access != baseProp.Access)
							throw new InconsistentAccessibilityException(errorNode,
								string.Format("Accessibility mismatch when overriding '{0}.set {1}'.",
									baseMemberClass.FullName, name));
				}
				else if (property.Access != baseProp.Access)
					throw new InconsistentAccessibilityException(errorNode,
						string.Format("Accessibility mismatch when overriding '{0}'.",
							baseMember.FullName));

				hasOverridden = true;
			}

			if (!hasOverridden && property.IsOverride)
				throw new DeclarationException(errorNode, "Found no suitable property to override.");
		}

		private void EnsureDeclarable(ParseNode errorNode, Indexer indexer)
		{
			// An indexer, a getter and setter for a specific argument count, is declarable if:
			//   a) No base type does contains a public or protected indexer, or that indexer
			//      does not take the argument count of this indexer[1].
			//
			//   b) If there is such an indexer in any base type, it must be abstract or
			//      overridable, and have the same readability/writability as the new indexer
			//      (i.e. readonly matched with readonly, writeonly with writeonly and read-write
			//      with read-write).
			//
			//   c) If this indexer overrides another indexer, it must be marked with the
			//      'override' modifier. Otherwise, if there is nothing to override, it must
			//      not have that modifier.
			//
			// Indexers do not take optional or variadic parameters, so we never have to worry
			// about overlapping signatures. It is sufficient to check for an exact arg count.
			//
			// [1] Note that it is not possible to add overloads to protected indexers outside
			// of the declaring class. However, this check is performed when the accessor methods
			// are declared, by EnsureDeclarable(ParseNode, Method). See the comments there for
			// more details.

			var hasOverridden = false;

			var baseStartType = this.BaseType as Class;
			NamedMember baseMember;
			do
			{
				Class baseMemberClass;
				baseMember = GetBaseMember(Indexer.MemberName, baseStartType, out baseMemberClass);

				if (baseMember != null)
				{
					if (baseMember.Kind != MemberKind.IndexerMember)
						throw new DeclarationException(errorNode,
							string.Format("Cannot hide inherited member '{0}'.",
								baseMember.FullName));

					var baseIndexer = ((IndexerMember)baseMember).GetIndexer(indexer.ArgCount);
					if (baseIndexer != null)
					{
						if (!baseIndexer.IsOverridable && !baseIndexer.IsAbstract)
							throw new DeclarationException(errorNode,
								string.Format("Cannot override inherited member '{0}'.",
									baseIndexer.GetDisplayName()));

						// baseProp is overridable or abstract
						if (!indexer.IsOverride)
							throw new DeclarationException(errorNode,
								string.Format("Cannot hide inherited member '{0}'. Specify 'override' if the intent is to override it.",
									baseIndexer.GetDisplayName()));

						// Read-writeness must match base property.
						if (indexer.PropertyKind != baseIndexer.PropertyKind)
							throw new DeclarationException(errorNode,
								string.Format("Cannot override {0} indexer '{1}' with {2} indexer.",
									PropertyKindToString(baseIndexer.PropertyKind),
									baseIndexer.GetDisplayName(),
									PropertyKindToString(indexer.PropertyKind)));

						if (indexer.Access == Accessibility.None)
						{
							indexer.Access = baseIndexer.Access;

							var getter = indexer.IndexerGetter;
							if (getter != null)
								if (getter.Access == Accessibility.None)
									getter.Access = baseIndexer.Access;
								else if (getter.Access != baseIndexer.Access)
									throw new InconsistentAccessibilityException(errorNode,
										string.Format("Accessibility mismatch when overriding '{0}'.",
											baseIndexer.IndexerGetter.GetDisplayName()));

							var setter = indexer.IndexerSetter;
							if (setter != null)
								if (setter.Access == Accessibility.None)
									setter.Access = baseIndexer.Access;
								else if (setter.Access != baseIndexer.Access)
									throw new InconsistentAccessibilityException(errorNode,
										string.Format("Accessibility mismatch when overriding '{0}'.",
											baseIndexer.IndexerSetter.GetDisplayName()));
						}
						else if (indexer.Access != baseIndexer.Access)
							throw new InconsistentAccessibilityException(errorNode,
								string.Format("Accessibility mismatch when overriding '{0}'.",
									baseIndexer.GetDisplayName()));

						// If we get here, we've successfully overridden a base indexer!
						// We can stop looking.
						hasOverridden = true;
						break;
					}

					baseStartType = baseStartType.BaseType as Class;
				}
			} while (baseMember != null);

			if (!hasOverridden && indexer.IsOverride)
				throw new DeclarationException(errorNode, "Found no suitable indexer to override.");
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
				method.OverriddenBaseMethod = overrideCandidate;
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

		internal string GetUnimplementedAbstractMethodNames()
		{
			if (inheritedAbstractMethods == null ||
				inheritedAbstractMethods.Count == 0)
				return "";

			var sb = new StringBuilder();

			var needSep = false;
			foreach (var m in inheritedAbstractMethods)
			{
				if (needSep)
					sb.Append(", ");
				else
					needSep = true;
				sb.Append(m.Group.FullName);
				sb.Append('(');
				sb.Append(string.Join(", ", m.Parameters.Select(p => p.Name)));
				if (m.Signature.Splat == Splat.End)
					sb.Append("...");
				sb.Append(')');
			}

			return sb.ToString();
		}

		internal string GetLambdaParam()
		{
			return "λc$arg$";
		}

		internal string GetLambdaName(string nameHint)
		{
			return string.Format("λc${0}!{1}", nameHint ?? "__", lambdaNameCounter++);
		}

		internal string GetClosureClassName(string prefix, out int blockNumber)
		{
			blockNumber = closureClassCounter++;
			return string.Format("C${0}{1}!{2}", prefix, "__", blockNumber.ToStringInvariant());
		}

		internal string GetGeneratorClassName(string prefix)
		{
			var classNumber = closureClassCounter++;
			return string.Format("I${0}{1}!{2}", prefix, "__", classNumber.ToStringInvariant());
		}

		internal const string InvocatorName = ".call";

		private enum ClassState
		{
			NotInited,
			Initing,
			Inited,
		}
	}

	public abstract class ClassMember : NamedMember
	{
		public ClassMember(string name, MemberKind kind, ParseNode node, Accessibility access, Class parent)
			: base(name, kind, node, access)
		{
			Parent = parent;
		}

		/// <summary>
		/// Gets the class that contains the member.
		/// </summary>
		public Class Parent { get; internal set; }

		public override string FullName { get { return Parent.FullName + "." + this.Name; } }

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
		public ClassMemberMethod(ParseNode node, string name, NamedMember owner,
			Accessibility access, Statement body, Splat splat, params Parameter[] parameters)
			: base(node, name, access, body, splat, parameters)
		{
			this.owner = owner;
		}
		public ClassMemberMethod(ParseNode node, string name, NamedMember owner,
			Accessibility access, Statement body, Signature signature)
			: base(node, name, access, body, signature)
		{
			this.owner = owner;
		}

		private NamedMember owner;
		public NamedMember Owner { get { return owner; } internal set { owner = value; } }
	}

	public class Field : ClassMember
	{
		public Field(VariableDeclarator node, Accessibility access, Class parent)
			: base(node.Name, MemberKind.Field, node, access, parent)
		{ }
		public Field(string name, Accessibility access, Class parent)
			: base(name, MemberKind.Field, null, access, parent)
		{ }

		internal uint Id;
	}

	public class ClassConstant : ClassMember, IConstantMember
	{
		public ClassConstant(VariableDeclarator node, Accessibility access, Class parent) :
			base(node.Name, MemberKind.Constant, node, access, parent)
		{ }
		protected ClassConstant(string name, Accessibility access, Class parent)
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

				return ((ConstantExpression)varDecl.Initializer).Value;
			}
		}

		internal uint Id;
	}

	public class ImportedClassConstant : ClassConstant
	{
		public ImportedClassConstant(string name, Accessibility access, Class parent, ConstantValue value)
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
			: base(name, MemberKind.Property, null, Accessibility.None, parent)
		{ }
		protected internal Property(string name, Class parent, bool isIndexer)
			: base(name, isIndexer ? MemberKind.Indexer : MemberKind.Property, null, Accessibility.None, parent)
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
					ValidateAccessors(value.Method, setter.Method, value.Node);
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
					ValidateAccessors(getter.Method, value.Method, value.Node);
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

		public virtual string GetDisplayName()
		{
			return FullName;
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

		internal static void ValidateAccessors(Method getter, Method setter, ParseNode errorNode)
		{
			// Note: The accessors must also have the same accessibility, but
			// we check that in Class.EnsureDeclarable(ParseNode, Property),
			// since property accessors may have the 'override' modifier and
			// no explicitly declared accessibility.

			if (getter.IsAbstract != setter.IsAbstract)
				throw new DeclarationException(errorNode, "If either property accessor is marked abstract, then both must be.");

			if (getter.IsOverridable != setter.IsOverridable)
				throw new DeclarationException(errorNode, "If either property accessor is marked overridable, then both must be.");

			if (getter.IsOverride != setter.IsOverride)
				throw new DeclarationException(errorNode, "If either property accessor is marked override, then both must be.");

			if (getter.IsStatic != setter.IsStatic)
				throw new DeclarationException(errorNode, "If either property accessor is marked static, then both must be.");
		}

		internal ParseNode GetAccessorNode()
		{
			return getter != null ? getter.Node : setter.Node;
		}

		internal void EnsureReadable(ParseNode errorNode)
		{
			if (getter == null)
				throw new CompileTimeException(errorNode,
					string.Format("The property '{0}' cannot be read from; it is write-only.", GetDisplayName()));
		}
		internal void EnsureWritable(ParseNode errorNode)
		{
			if (setter == null)
				throw new CompileTimeException(errorNode,
					string.Format("The property '{0}' cannot be assigned to; it is read-only.", GetDisplayName()));
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
			method = new ClassMemberMethod(node,
				GetAccessorMethodName(node.IsSetter, node.Name), this,
				node.Access, node.Body, Splat.None, parameters)
				{
					IsStatic = node.IsStatic,
					IsAbstract = node.IsAbstract,
					IsOverridable = node.IsOverridable,
					IsOverride = node.IsOverride,
					IsImplDetail = true,
				};
			node.DeclSpace = method;
		}
		protected PropertyAccessor(IndexerAccessorDeclaration node)
			: base(Indexer.MemberName, node.IsSetter ? MemberKind.IndexerSetter : MemberKind.IndexerGetter, node, node.Access)
		{
			var parameters = new Parameter[node.Parameters.Length + (node.IsSetter ? 1 : 0)];
			for (var i = 0; i < node.Parameters.Length; i++)
				parameters[i] = node.Parameters[i];
			if (node.IsSetter)
				parameters[node.Parameters.Length] = new Parameter("value", null)
				{
					StartIndex = node.StartIndex,
					EndIndex = node.EndIndex,
				};

			isSetter = node.IsSetter;
			method = new ClassMemberMethod(node,
				PropertyAccessor.GetAccessorMethodName(node.IsSetter, Indexer.MemberName), this,
				node.Access, node.Body, Splat.None, parameters)
				{
					IsAbstract = node.IsAbstract,
					IsOverridable = node.IsOverridable,
					IsOverride = node.IsOverride,
					IsStatic = false,
					IsImplDetail = true,
				};
			node.DeclSpace = method;
		}

		internal PropertyAccessor(ClassMemberMethod method, bool isSetter, bool isIndexer)
			: base(method.Name,
				isIndexer ?
					(isSetter ? MemberKind.IndexerSetter : MemberKind.IndexerGetter) :
					(isSetter ? MemberKind.PropertySetter : MemberKind.PropertyGetter),
				null, method.Access)
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

				Func<ReturnStatement, bool> hasReturnValue = ret => ret.ReturnValue != null;
				if (method.Returns != null && method.Returns.Any(hasReturnValue))
					throw new CompileTimeException(method.Returns.First(hasReturnValue),
						"A property setter can only contain empty return statements.");
			}
		}

		internal static string GetAccessorMethodName(bool isSetter, string propName)
		{
			return string.Format(isSetter ? SetterNameFormat : GetterNameFormat, propName);
		}

		internal const string GetterNameFormat = "get:{0}";
		internal const string SetterNameFormat = "set:{0}";
	}

	public class IndexerMember : ClassMember
	{
		public IndexerMember(Class parent)
			: base(Indexer.MemberName, MemberKind.IndexerMember, null, Accessibility.None, parent)
		{
			Indexers = new List<Indexer>(1);
		}

		internal List<Indexer> Indexers;

		internal uint GetterGroupId
		{
			get
			{
				foreach (var idx in Indexers)
					if (idx.Getter != null)
						return idx.GetterId;
				return 0;
			}
		}

		internal uint SetterGroupId
		{
			get
			{
				foreach (var idx in Indexers)
					if (idx.Setter != null)
						return idx.SetterId;
				return 0;
			}
		}

		public Indexer GetIndexer(int argCount)
		{
			for (var i = 0; i < Indexers.Count; i++)
			{
				var idx = Indexers[i];
				if (idx.ArgCount == argCount)
					return idx;
			}
			return null;
		}

		internal void AddIndexer(Indexer indexer)
		{
			if (indexer == null)
				throw new ArgumentNullException("indexer");

			for (var i = 0; i < Indexers.Count; i++)
				if (Indexers[i].ArgCount == indexer.ArgCount)
					throw new DeclarationException(indexer.GetAccessorNode(),
						string.Format("There is already an indexer that takes {0} arguments.", indexer.ArgCount));
			Indexers.Add(indexer);
		}
	}

	public class Indexer : Property
	{
		public Indexer(Class parent, int argCount)
			: base(MemberName, parent, isIndexer: true)
		{
			this.argCount = argCount;
		}

		private int argCount;
		/// <summary>
		/// Gets the number of arguments that each indexer accessor takes,
		/// not including the value for the setter.
		/// </summary>
		public int ArgCount { get { return argCount; } }

		internal IndexerAccessor IndexerGetter { get { return (IndexerAccessor)Getter; } }

		internal IndexerAccessor IndexerSetter { get { return (IndexerAccessor)Setter; } }

		public override string GetDisplayName()
		{
			var sb = new StringBuilder(128);

			sb.Append(Parent.FullName);
			sb.Append(".this[");
			var node = GetAccessorNode() as IndexerAccessorDeclaration;
			if (node != null)
				sb.Append(node.Parameters.Select(p => p.Name).JoinString(","));
			else
				sb.Append(argCount.ToStringInvariant());
			sb.Append("]");

			return sb.ToString();
		}

		internal const string MemberName = ".item";
	}

	public class IndexerAccessor : PropertyAccessor
	{
		public IndexerAccessor(IndexerAccessorDeclaration node)
			: base(node)
		{
			argCount = node.Parameters.Length;
		}

		internal IndexerAccessor(int argCount, ClassMemberMethod method, bool isSetter)
			: base(method, isSetter, isIndexer: true)
		{
			this.argCount = argCount;
		}

		private int argCount;
		/// <summary>
		/// Gets the number of arguments that the indexer accessor takes,
		/// not including the value for setters.
		/// </summary>
		public int ArgCount { get { return argCount; } }

		public Indexer ParentIndexer { get { return (Indexer)Parent; } }

		public string GetDisplayName()
		{
			var sb = new StringBuilder(128);
			sb.Append(Owner.FullName);
			sb.Append(".");
			sb.Append(IsGetter ? "get this[" : "set this[");
			var node = Node as IndexerAccessorDeclaration;
			if (node != null)
				sb.Append(node.Parameters.Select(p => p.Name).JoinString(","));
			else
				sb.Append(argCount.ToStringInvariant());
			sb.Append("]");

			return sb.ToString();
		}
	}

	public class Constructor : ClassMember
	{
		public Constructor(ConstructorDeclaration node, Class parent)
			: base(node.IsStatic ? StaticCtorName : InstanceCtorName, MemberKind.Constructor, node, node.Access, parent)
		{
			method = new ClassMemberMethod(node,
				node.IsStatic ? StaticCtorName : InstanceCtorName,
				this, node.Access, node.Body,
				new Signature(node.Parameters, node.Splat))
			{
				Flags = node.IsStatic ? MemberFlags.Constructor : MemberFlags.InstanceConstructor,
			};
			this.IsStatic = node.IsStatic;

			var parameters = new Parameter[node.Parameters.Length];
			for (var i = 0; i < node.Parameters.Length; i++)
			{
				var param = node.Parameters[i];
				method.Body.DeclareVariable(new Variable(param.DeclaredName, param));
				if (param.HasThisPrefix)
					method.Body.ReserveName(param.Name, ReserveReason.UsedAsThisParameter);
				parameters[i] = param;
			}
			method.Parameters = parameters;
		}
		public Constructor(Block body, Accessibility access, Class parent, Splat splat, params Parameter[] parameters)
			: base(InstanceCtorName, MemberKind.Constructor, null, access, parent)
		{
			method = new ClassMemberMethod(null, InstanceCtorName, this,
				access, body, splat, parameters)
			{
				Flags = MemberFlags.InstanceConstructor,
			};
			this.IsStatic = false;
		}
		public Constructor(Block body, Accessibility access, Class parent, Signature signature)
			: base(InstanceCtorName, MemberKind.Constructor, null, access, parent)
		{
			method = new ClassMemberMethod(null, InstanceCtorName, this,
				access, body, signature)
			{
				Flags = MemberFlags.InstanceConstructor,
			};
			this.IsStatic = false;
		}
		public Constructor(Block body, Class parent)
			: base(StaticCtorName, MemberKind.Constructor, null, Accessibility.Private, parent)
		{
			method = new ClassMemberMethod(null, StaticCtorName, this,
				Accessibility.Private, body, Signature.Empty)
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

			Func<ReturnStatement, bool> hasReturnValue = ret => ret.ReturnValue != null;
			if (method.Returns != null && method.Returns.Any(hasReturnValue))
				throw new CompileTimeException(method.Returns.First(hasReturnValue),
					"Constructors can only contain empty return statements.");
		}

		internal const string InstanceCtorName = ".new";
		internal const string StaticCtorName = ".init";
	}

	public class Iterator : ClassMember
	{
		public Iterator(IteratorDeclaration node, Class parent)
			: base(MemberName, MemberKind.Iterator, node, Accessibility.None, parent)
		{
			method = new ClassMemberMethod(node, MemberName, this,
				Accessibility.Public, node.Body, Splat.None, new Parameter[0])
			{
				Flags = MemberFlags.Instance | MemberFlags.AutoOverride | MemberFlags.Overridable | MemberFlags.ImplDetail,
			};
		}

		private ClassMemberMethod method;
		/// <summary>Gets the method that contains the iterator implementation.</summary>
		public ClassMemberMethod Method { get { return method; } }

		internal const string MemberName = ".iter";
	}

	public class OperatorOverload : ClassMember
	{
		public OperatorOverload(BinaryOperatorOverload node, Class parent)
			: base(null, MemberKind.Operator, node, Accessibility.Public, parent)
		{
			index = node.GetIndex();

			Parameter[] parameters = {
				new Parameter(node.Left, null),
				new Parameter(node.Right, null),
			};
			method = new ClassMemberMethod(node, node.GetMethodName(), this,
				Accessibility.Public, node.Body, Splat.None, parameters)
			{
				Flags = MemberFlags.Operator,
			};
		}
		public OperatorOverload(UnaryOperatorOverload node, Class parent)
			: base(null, MemberKind.Operator, node, Accessibility.Public, parent)
		{
			index = node.GetIndex();

			Parameter[] parameters = { new Parameter(node.Operand, null) };
			method = new ClassMemberMethod(node, node.GetMethodName(), this,
				Accessibility.Public, node.Body, Splat.None, parameters)
			{
				Flags = MemberFlags.Operator,
			};
		}
		internal OperatorOverload(int index, Class parent, ClassMemberMethod method)
			: base(null, MemberKind.Operator, null, Accessibility.Public, parent)
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
			: base(name, Accessibility.None, parent)
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
			Field field;
			if (blockToField != null &&
				blockToField.TryGetValue(block, out field))
				return field;

			return null;
		}

		public Field DeclareVariableField(Variable variable)
		{
			if (variable.CaptureField == null)
			{
				variable.CaptureField = new Field(GetFieldName(variable), Accessibility.Internal, this)
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

			var field = new Field(GetFieldName(block), Accessibility.Internal, this);
			field.IsStatic = false;

			DeclareField(field);
			blockToField[block] = field;

			return field;
		}

		public Field DeclareThisField()
		{
			if (thisField == null)
			{
				thisField = new Field(ThisFieldName, Accessibility.Internal, this)
				{
					IsStatic = false,
				};
				DeclareField(thisField);
				parentBlock.AddClosureThisFieldInitializer(thisField);
			}

			return thisField;
		}

		internal static string GetFieldName(Variable variable)
		{
			return "$" + variable.Name;
		}

		internal static string GetFieldName(BlockSpace block)
		{
			return string.Format("<>block_{0}", block.BlockNumber);
		}

		private static string GetMethodName(LocalFunction function)
		{
			if (function.Name.StartsWith("λ$") || function.Name.StartsWith("λc$")) // captured lambda
				return function.Name;
			else
				return string.Format("ƒ${0}", function.Name);
		}

		internal const string ThisFieldName = "<>this";
	}

	public class GeneratorClass : Class
	{
		public GeneratorClass(string name, Method owner, Namespace parent, Compiler compiler)
			: base(name, Accessibility.Private, parent)
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

		public Method MoveNextMethod
		{
			get
			{
				var m = GetMember("moveNext");
				if (m.Kind == MemberKind.MethodGroup)
					return ((MethodGroup)m)[0];

				return null;
			}
		}

		private List<AnonField> anonFields;

		private void AddDefaultMembers()
		{
			stateField = new Field(StateFieldName, Accessibility.Private, this)
			{
				IsStatic = false,
				IsImplDetail = true,
			};
			DeclareField(stateField);

			currentValueField = new Field(CurrentValueFieldName, Accessibility.Private, this)
			{
				IsStatic = false,
				IsImplDetail = true,
			};
			DeclareField(currentValueField);

			var currentValueGetterMethod = new ClassMemberMethod(null,
				PropertyAccessor.GetAccessorMethodName(false, "current"),
				null, Accessibility.Public,
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

			var currentValueGetter = new PropertyAccessor(currentValueGetterMethod, isSetter: false, isIndexer: false);

			DeclareProperty(new Property("current", this) { Getter = currentValueGetter });

			AddConstructor();
		}

		private void AddConstructor()
		{
			// Find the base constructor
			var baseCtor = BaseType.FindConstructor(null, 0, this, this);

			// Add a statement to this constructor body that calls the base constructor
			var ctorBody = new Statement[]
			{
				new ExpressionStatement(
					new InvocationExpression( // new base();
						new InstanceMemberAccess(new ThisAccess(), (Class)BaseType, baseCtor.Group), // base.'.new'
						EmptyArrays.Expressions // ()
					)
				),
				new SimpleAssignment( // this.'<>state' = -1
					new InstanceMemberAccess(new ThisAccess(), (Class)BaseType, stateField) { IsAssignment = true },
					new ConstantExpression(ConstantValue.CreateInt(-1))
				),
			};
			var ctor = new Constructor(new Block(ctorBody), Accessibility.Internal, this, Signature.Empty);
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
					var field = new Field(name, Accessibility.Internal, this)
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
				var field = new Field(GetAnonFieldName(anonFields.Count), Accessibility.Internal, this);
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
				thisField = new Field(ClosureClass.ThisFieldName, Accessibility.Internal, this)
				{
					IsStatic = false,
				};
				DeclareField(thisField);
			}

			return thisField;
		}

		private static string GetAnonFieldName(int counter)
		{
			return string.Format("anon:{0}", counter);
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