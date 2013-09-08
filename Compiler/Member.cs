using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Osprey;
using Osprey.Nodes;

namespace Osprey.Members
{
	/// <summary>
	/// Represents a single member in a declaration space. The <see cref="Member.Kind"/> contains information about what type of member it is.
	/// </summary>
	public abstract class Member
	{
		/// <summary>
		/// Initializes a new <see cref="Member"/> with a specific type and node.
		/// </summary>
		/// <param name="kind">The type of the member.</param>
		/// <param name="node">The <see cref="Node"/> that declares the member.</param>
		/// <param name="access">The declared accessibility of the member.</param>
		protected Member(MemberKind kind, ParseNode node, AccessLevel access)
		{
			this.kind = kind;
			this.node = node;
			this.access = access;
		}

		private MemberKind kind;
		/// <summary>Gets the kind of the member.</summary>
		public MemberKind Kind { get { return kind; } }

		private ParseNode node;
		/// <summary>Gets the node that declares the member.</summary>
		public ParseNode Node { get { return node; } }

		private AccessLevel access;
		/// <summary>Gets the declared accessibility of the member.</summary>
		public AccessLevel Access { get { return access; } internal set { access = value; } }

		public abstract Namespace GetContainingNamespace();
	}

	/// <summary>
	/// Represents a named member in a declaration space.
	/// </summary>
	public abstract class NamedMember : Member
	{
		public NamedMember(string name, MemberKind kind, ParseNode node, AccessLevel access)
			: base(kind, node, access)
		{
			this.name = name;
		}

		private string name;
		/// <summary>Gets the name of the member.</summary>
		public string Name { get { return name; } internal set { name = value; } }

		/// <summary>
		/// Gets the fully qualified name of the member.
		/// This may be the same as <see cref="Name"/>.
		/// </summary>
		public virtual string FullName { get { return name; } }

		// The name is writable because local methods may need to change the name.

		public override string ToString()
		{
			return string.Format("{0}, Name={1}", base.ToString(), name);
		}
	}

	/// <summary>
	/// Represents an ambiguous member. This can occur e.g. when two or more modules export global members (types, functions or
	/// constants) with the same fully qualified name. This is not an error condition unless you try to refer to the member.
	/// </summary>
	public class AmbiguousMember : NamedMember
	{
		public AmbiguousMember(string name, NamedMember firstMember)
			: base(name, MemberKind.Ambiguous, null, AccessLevel.None)
		{
			Members.Add(firstMember);
		}
		public AmbiguousMember(string name, params NamedMember[] members)
			: base(name, MemberKind.Ambiguous, null, AccessLevel.None)
		{
			Members.AddRange(members);
		}

		/// <summary>A list of the members that have the same fully qualified name.</summary>
		public List<NamedMember> Members = new List<NamedMember>();

		internal Namespace GetNamespaceMember()
		{
			foreach (var mem in Members)
				if (mem.Kind == MemberKind.Namespace)
					return (Namespace)mem;

			return null;
		}

		public override Namespace GetContainingNamespace()
		{
			throw new InvalidOperationException();
		}
	}

	/// <summary>
	/// Represents an ambiguous type name. These are only constructed when a type name is ambiguous;
	/// an <see cref="AmbiguousType"/> does not occur as a member inside a <see cref="Namespace"/>.
	/// </summary>
	public class AmbiguousTypeName : Type
	{
		public AmbiguousTypeName(TypeName name, params Type[] types)
			: base(null, MemberKind.Ambiguous, AccessLevel.None, null)
		{
			typeName = name;
			Types.AddRange(types);
		}

		private TypeName typeName;
		/// <summary>
		/// Gets the type name that was ambiguous.
		/// </summary>
		public TypeName TypeName { get { return typeName; } }

		/// <summary>The types that match the type name.</summary>
		public List<Type> Types = new List<Type>();

		public override bool ContainsMember(string name)
		{
			throw new NotSupportedException();
		}

		public override NamedMember GetMember(string name)
		{
			throw new NotSupportedException();
		}

		public override NamedMember ResolveName(string name, Class fromClass)
		{
			throw new NotSupportedException();
		}

		public override Namespace GetContainingNamespace()
		{
			return null;
		}

		public override Method FindConstructor(ParseNode node, int argCount, Class fromClass)
		{
			throw new NotSupportedException();
		}
	}

	/// <summary>
	/// Describes the kind of a <see cref="Member"/>.
	/// </summary>
	public enum MemberKind
	{
		/// <summary>The member is a namespace.</summary>
		Namespace,

		/// <summary>The member is a global constant.</summary>
		GlobalConstant,
		/// <summary>The member is a global variable.</summary>
		GlobalVariable,

		/// <summary>The member is a class.</summary>
		Class,
		/// <summary>The member is an enum.</summary>
		Enum,

		/// <summary>The member is a constructor.</summary>
		Constructor,

		/// <summary>The member is a class field.</summary>
		Field,
		/// <summary>The member is a class constant.</summary>
		Constant,
		/// <summary>The member is an enum field.</summary>
		EnumField,

		/// <summary>The member is a method group.</summary>
		MethodGroup,
		/// <summary>The member is a single method.</summary>
		Method,

		/// <summary>The member is a property.</summary>
		Property,
		/// <summary>The member is a property getter.</summary>
		PropertyGetter,
		/// <summary>The member is a property setter.</summary>
		PropertySetter,
		/// <summary>The member is an indexer getter.</summary>
		IndexerGetter,
		/// <summary>The member is an indexer setter.</summary>
		IndexerSetter,

		/// <summary>The member is an operator.</summary>
		Operator,
		/// <summary>The member is an iterator.</summary>
		Iterator,

		/// <summary>The member is a local variable or parameter.</summary>
		Variable,
		/// <summary>The member is a local constant.</summary>
		LocalConstant,
		/// <summary>The member is a local function.</summary>
		LocalFunction,

		/// <summary>The member is ambiguous between several imported names.</summary>
		/// <remarks>This is not an error condition until you try to access the ambiguous member.</remarks>
		Ambiguous,
	}

	/// <summary>
	/// Represents a namespace, which is a member and a declaration space.
	/// Namespaces can contain global functions, global constants, types, global variables and other namespaces.
	/// </summary>
	[DebuggerDisplay("Namespace {FullName}")]
	public class Namespace : NamedMember, IDeclarationSpace
	{
		/// <summary>
		/// Initializes a new, top-level namespace; a global declaration space, in other words.
		/// </summary>
		internal Namespace()
			: base(null, MemberKind.Namespace, null, AccessLevel.Public)
		{ }
		/// <summary>
		/// Initializes a new <see cref="Namespace"/> member with the specified name.
		/// </summary>
		/// <param name="name">The name of the new namespace.</param>
		/// <remarks>Only the global declaration space should have a name of null.</remarks>
		/// <remarks>This constructor should only ever be called from outside <see cref="Namespace"/> by classes that inherit from it. Use <see cref="GetNamespace"/> to create and access namespaces within any given namespace.</remarks>
		protected Namespace(string name)
			: base(name, MemberKind.Namespace, null, AccessLevel.Public)
		{ }

		private Namespace parent;
		/// <summary>
		/// The namespace that contains the current namespace, or null if the namespace is the global declaration space.
		/// </summary>
		public Namespace Parent { get { return parent; } }

		/// <summary>
		/// Gets the fully qualified name of the namespace.
		/// </summary>
		public override string FullName
		{
			get
			{
				return parent == null || parent.Name == null ?
					this.Name :
					parent.FullName + "." + this.Name;
			}
		}

		private Dictionary<string, NamedMember> members = new Dictionary<string, NamedMember>();
		/// <summary>Gets a dictionary of all the members in the namespace.</summary>
		protected internal Dictionary<string, NamedMember> Members { get { return members; } }

		private int lambdaNameCounter = 0;

		/// <summary>
		/// Declares a method within this namespace. This will create a group for the method if one does not already exist; otherwise, the method will be added to that group as an overload.
		/// </summary>
		/// <param name="method">The method to declare.</param>
		/// <exception cref="ArgumentNullException"><paramref name="method"/> is null.</exception>
		/// <exception cref="DuplicateNameException">The namespace already contains a non-method member with the same name.
		/// -or-
		/// The method group that is already in the namespace comes from a different module.</exception>
		public MethodGroup DeclareMethod(Method method)
		{
			return DeclareMethod(method, false);
		}
		/// <summary>
		/// Declares a method within this namespace. This will create a group for the method if one does not already exist; otherwise, the method will be added to that group as an overload. If <paramref name="imported"/> is true and a member already exists, that member is turned into an <see cref="AmbiguousMember"/>.
		/// </summary>
		/// <param name="method">The method to declare.</param>
		/// <param name="imported">Whether the method is imported from a module other than the one that is being compiled.</param>
		/// <exception cref="ArgumentNullException"><paramref name="method"/> is null.</exception>
		/// <exception cref="DuplicateNameException"><paramref name="imported"/> is false and the namespace already contains a non-method member with the same name.
		/// -or-
		/// <paramref name="imported"/> is false and he method group that is already in the namespace comes from a different module.</exception>
		internal MethodGroup DeclareMethod(Method method, bool imported)
		{
			if (method == null)
				throw new ArgumentNullException("method");

			var name = method.Name;
			MethodGroup group;
			if (members.ContainsKey(name))
			{
				var mem = members[name];
				if (mem.Kind != MemberKind.MethodGroup || ((MethodGroup)mem).Module != method.Module)
				{
					if (!imported)
						throw new DuplicateNameException(method.Node, name,
							"There is already a member with the name '" + name + "' in this namespace.");
					group = new MethodGroup(name, this, method.Access);

					if (mem.Kind == MemberKind.Ambiguous)
						((AmbiguousMember)mem).Members.Add(group);
					else
						members[name] = new AmbiguousMember(name, mem, group);
				}
				else
					group = (MethodGroup)mem;
				if (group.Access != method.Access)
					throw new InconsistentAccessibilityException(method.Node);
			}
			else
				members[name] = group = new MethodGroup(name, this, method.Access);

			group.AddOverload(method);
			return group;
		}

		internal void ImportMethodGroup(MethodGroup group)
		{
			if (group == null)
				throw new ArgumentNullException("group");

			var name = group.Name;

			if (members.ContainsKey(name))
			{
				var member = members[name];
				if (member.Kind == MemberKind.Ambiguous)
					((AmbiguousMember)member).Members.Add(group);
				else
					members[name] = new AmbiguousMember(name, member, group);
			}
			else
				members.Add(name, group);
		}

		/// <summary>
		/// Declares a global constant within the namespace.
		/// </summary>
		/// <param name="constant">The constant to declare.</param>
		/// <exception cref="ArgumentNullException"><paramref name="constant"/> is null.</exception>
		/// <exception cref="DuplicateNameException">The namespace already contains a member with the same name.</exception>
		public void DeclareConstant(GlobalConstant constant)
		{
			DeclareConstant(constant, false);
		}
		/// <summary>
		/// Declares a global constant within the namespace. If <paramref name="imported"/> is true and the namespace already contains a member with the specified name, that member is turned into an <see cref="AmbiguousMember"/>.
		/// </summary>
		/// <param name="constant">The constant to declare.</param>
		/// <param name="imported">Whether the member is imported from a module other than the one that is being compiled.</param>
		/// <exception cref="ArgumentNullException"><paramref name="constant"/> is null.</exception>
		/// <exception cref="DuplicateNameException"><paramref name="imported"/> is false and the namespace already contains a member with the same name.</exception>
		internal void DeclareConstant(GlobalConstant constant, bool imported)
		{
			if (constant == null)
				throw new ArgumentNullException("constant");
			if (constant.Parent != null)
				throw new ArgumentException("This constant already belongs to another namespace.", "constant");

			constant.Parent = this;

			var name = constant.Name;
			if (members.ContainsKey(name))
			{
				if (!imported)
					throw new DuplicateNameException(constant.Node, name,
						"There is already a member with the name '" + name + "' in this namespace.");

				var mem = members[name];
				if (mem.Kind == MemberKind.Ambiguous)
					((AmbiguousMember)mem).Members.Add(constant);
				else
					members[name] = new AmbiguousMember(name, mem, constant);
			}
			else
				members.Add(name, constant);
		}

		/// <summary>
		/// Declares a type within the namespace.
		/// </summary>
		/// <param name="type">The type to declare.</param>
		/// <exception cref="ArgumentNullException"><paramref name="type"/> is null.</exception>
		public void DeclareType(Type type)
		{
			if (type == null)
				throw new ArgumentNullException("type");

			members.Add(type.Name, type);
		}
		internal void DeclareType(Type type, bool imported)
		{
			if (type == null)
				throw new ArgumentNullException("type");

			var name = type.Name;
			if (members.ContainsKey(name))
			{
				if (!imported)
					throw new DuplicateNameException(type.Node, name,
						"There is already a member with the name '" + name + "' in this namespace.");

				var mem = members[name];
				if (mem.Kind == MemberKind.Ambiguous)
					((AmbiguousMember)mem).Members.Add(type);
				else
					members[name] = new AmbiguousMember(name, mem, type);
			}
			else
				members.Add(name, type);
		}
		
		/// <summary>
		/// Returns a namespace with a specified qualified name, relative to the current namespace. If it does not exist, it is created. If the path leading up to the namespace does not exist, those namespaces are created as well.
		/// </summary>
		/// <param name="name">The name of the namespace to look up.</param>
		/// <returns>The specified namespace.</returns>
		/// <remarks>Whitespace in <paramref name="name"/> is NOT ignored. The string is split on '.' characters only.</remarks>
		public Namespace GetNamespace(string name)
		{
			return GetNamespace(name, false);
		}
		internal Namespace GetNamespace(string name, bool imported)
		{
			var parts = name.Split('.');

			return GetNamespace(parts, 0, imported);
		}

		/// <summary>
		/// Returns a namespace with a specified qualified name, relative to the current namespace. If it does not exist, it is created. If the path leading up to the namespace does not exist, those namespaces are created as well.
		/// </summary>
		/// <param name="path">The path leading up to the namespace.</param>
		/// <returns>The specified namespace.</returns>
		public Namespace GetNamespace(string[] path)
		{
			return GetNamespace(path, 0, false);
		}
		internal Namespace GetNamespace(string[] path, bool imported)
		{
			return GetNamespace(path, 0, imported);
		}

		private Namespace GetNamespace(string[] path, int offset, bool imported)
		{
			var name = path[offset];
			Namespace ns;
			if (members.ContainsKey(name))
			{
				var mem = members[name];
				if (mem.Kind != MemberKind.Namespace)
				{
					if (!imported)
						throw new ArgumentException("There is already a member with the name '" + name + "' in this namespace.");

					if (mem.Kind != MemberKind.Ambiguous)
						mem = new AmbiguousMember(name, mem);

					ns = ((AmbiguousMember)mem).GetNamespaceMember();
					if (ns == null)
						((AmbiguousMember)mem).Members.Add(ns = new Namespace(name));
				}
				else
					ns = (Namespace)mem;
			}
			else
				members[name] = ns = new Namespace(name);

			ns.parent = this;

			if (offset == path.Length - 1)
				return ns;
			else
				return ns.GetNamespace(path, offset + 1, imported);
		}

		internal Namespace FindNamespace(QualifiedName name)
		{
			return FindNamespace(name, name.Parts.ToArray(), 0);
		}
		private Namespace FindNamespace(ParseNode node, string[] path, int offset)
		{
			var name = path[offset];
			Namespace ns = null;
			if (members.ContainsKey(name))
			{
				var mem = members[name];
				if (mem.Kind == MemberKind.Namespace)
					ns = (Namespace)mem;
			}
			if (ns == null)
				throw new CompileTimeException(node,
					string.Format("The namespace '{0}' could not be found. (Did you forget to import a module?)",
						path.JoinString(".")));

			if (offset == path.Length - 1)
				return ns;
			else
				return ns.FindNamespace(node, path, offset + 1);
		}

		public bool ContainsMember(string name)
		{
			if (name == null)
				return false;
			return members.ContainsKey(name);
		}

		public virtual NamedMember ResolveName(string name, Class fromClass)
		{
			if (name == null)
				throw new ArgumentNullException("name");

			if (members.ContainsKey(name))
				return members[name];
			if (parent != null)
				return parent.ResolveName(name, fromClass);

			return null;
		}

		/// <summary>
		/// Resolves a <see cref="TypeName"/> relative to this namespace.
		/// </summary>
		/// <param name="name">The type name to resolve.</param>
		/// <param name="document">The <see cref="FileNamespace"/> of the document in which the <see cref="TypeName"/> occurs.</param>
		/// <returns>The type that the type name resolves to.</returns>
		/// <exception cref="UndefinedNameException">The type name could not be resolved.</exception>
		/// <exception cref="AmbiguousTypeNameException">The type name is ambiguous.</exception>
		/// <remarks>This method modifies <paramref name="name"/>'s <see cref="TypeName.Type"/> field.</remarks>
		public virtual Type ResolveTypeName(TypeName name, FileNamespace document)
		{
			// If the type name is global or this namespace is a global declaration space (this.Name == null),
			// we need to resolve the type name relative to the document, because it may be a simple identifier,
			// and then we need to check imported namespaces.
			// Note, however, that document is null in some internal calls. In that case, we ignore it.
			if ((name.IsGlobal || this.Name == null) && document != null)
				return document.ResolveTypeName(name, document);
			
			var first = name.Parts[0];

			if (members.ContainsKey(first))
			{
				// If this namespace contains a member matching the first component of name.Parts,
				// then we /have to/ find the member in this namespace, or a namespace inside it.

				Type result = null;

				NamedMember member = members[first];
				for (var i = 1; i < name.Parts.Count; i++)
				{
					// Note: this loop is only entered if name.Parts.Count > 1
					if (member.Kind != MemberKind.Namespace)
						break;

					if (!((Namespace)member).members.TryGetValue(name.Parts[i], out member))
						break;
				}

				if (member != null &&
					(member.Kind == MemberKind.Class ||
					member.Kind == MemberKind.Enum))
					result = (Type)member;

				if (result != null)
					return name.Type = result; // found! hurrah!
			}
			else if (parent != null)
				return parent.ResolveTypeName(name, document); // Try the parent namespace!

			throw new UndefinedNameException(name, name.ToString(),
				string.Format("The type name '{0}' could not be resolved.", name.ToString()));
		}

		public NamedMember GetMember(string name)
		{
			return members[name];
		}

		public override Namespace GetContainingNamespace()
		{
			return parent;
		}

		internal string GetLambdaParam()
		{
			return "<λns>arg$";
		}

		internal string GetLambdaName(string nameHint)
		{
			return string.Format("<λns>{0}${1}", nameHint ?? "<anon>", lambdaNameCounter++);
		}

		IDeclarationSpace IDeclarationSpace.Parent { get { return parent; } }

		Class IDeclarationSpace.GetContainingClass(out bool hasInstance)
		{
			hasInstance = false;
			return null;
		}
	}

	public class FileNamespace : Namespace
	{
		public FileNamespace(Namespace projectNamespace, Compiler compiler)
			: base(null)
		{
			this.projectNamespace = projectNamespace;
			this.compiler = compiler;
		}

		/// <summary>
		/// The "root" namespace of the members declared by the project;
		/// the "global" project namespace, if you will.
		/// </summary>
		private Namespace projectNamespace;
		private List<Namespace> importedNamespaces = new List<Namespace>();

		/// <summary>
		/// Gets the namespace of the project that the file belongs to.
		/// </summary>
		public Namespace ProjectNamespace { get { return projectNamespace; } }

		private Compiler compiler;
		internal Compiler Compiler { get { return compiler; } }

		/// <summary>
		/// Declares a global variable in the global declaration space.
		/// </summary>
		/// <param name="variable">The variable to declare.</param>
		/// <exception cref="ArgumentNullException"><paramref name="variable"/> is null.</exception>
		public void DeclareGlobalVariable(GlobalVariable variable)
		{
			if (variable == null)
				throw new ArgumentNullException("variable");

			if (projectNamespace.ContainsMember(variable.Name))
				throw new DuplicateNameException(variable.Node, variable.Name);

			Members.Add(variable.Name, variable);
		}

		public void ImportNamespace(Namespace ns)
		{
			importedNamespaces.Add(ns);
		}

		public override NamedMember ResolveName(string name, Class fromClass)
		{
			return ResolveName(name, fromClass, false);
		}

		public override Type ResolveTypeName(TypeName name, FileNamespace document)
		{
			var output = new List<Type>();

			// Try to find a fully qualified member in the project namespace
			NamedMember member = projectNamespace;
			for (var i = 0; i < name.Parts.Count; i++)
			{
				if (member.Kind != MemberKind.Namespace)
					break; // Not found!

				if (!((Namespace)member).Members.TryGetValue(name.Parts[i], out member))
					break; // Also not found!

				if (i == name.Parts.Count - 1 && member is Type)
					output.Add((Type)member); // Found!
			}

			if (name.Parts.Count == 1 && !name.IsGlobal)
			{
				var first = name.Parts[0];
				foreach (var ns in importedNamespaces)
					if (ns.Members.TryGetValue(first, out member) && member is Type)
						output.Add((Type)member);
			}

			if (output.Count == 0)
				throw new UndefinedNameException(name, name.ToString(),
					string.Format("The type name '{0}' could not be resolved.", name));
			if (output.Count == 1)
				return name.Type = output[0];

			name.Type = new AmbiguousTypeName(name, output.ToArray());
			throw new AmbiguousTypeNameException(name, (AmbiguousTypeName)name.Type);
		}

		public NamedMember ResolveName(string name, Class fromClass, bool hasGlobalPrefix)
		{
			if (hasGlobalPrefix)
			{
				if (Members.ContainsKey(name))
					return Members[name];
				if (projectNamespace.ContainsMember(name))
					return projectNamespace.ResolveName(name, fromClass);
				return null;
			}

			List<NamedMember> matches = new List<NamedMember>();
			if (Members.ContainsKey(name))
				matches.Add(Members[name]);

			if (projectNamespace.ContainsMember(name))
				matches.Add(projectNamespace.Members[name]);

			foreach (var ns in importedNamespaces)
				if (ns.ContainsMember(name))
				{
					var mem = ns.GetMember(name);
					// Only non-namespace members are imported from namespaces
					if (mem.Kind != MemberKind.Namespace)
						matches.Add(mem);
				}

			if (matches.Count == 0)
				return null;
			if (matches.Count == 1)
				return matches[0]; // the first match is the deepest
			return new AmbiguousMember(name, matches.ToArray());
		}
	}

	public class GlobalConstant : NamedMember, IConstantMember
	{
		public GlobalConstant(string name, VariableDeclarator node, bool isPublic)
			: base(name, MemberKind.GlobalConstant, node, isPublic ? AccessLevel.Public : AccessLevel.Private)
		{ }

		internal GlobalConstant(string name, ConstantValue value, AccessLevel access)
			: base(name, MemberKind.GlobalConstant, null, access)
		{
			this.state = ConstantState.HasValue;
			this.value = value;
		}

		public override string FullName
		{
			get
			{
				if (Parent == null || Parent.Name == null)
					return this.Name;
				return Parent.FullName + "." + this.Name;
			}
		}

		private ConstantState state;
		public ConstantState State { get { return state; } }

		private ConstantValue value;
		/// <summary>Gets the value associated with this constant.</summary>
		public ConstantValue Value
		{
			get
			{
				if (Node == null) return value; // imported constant

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

		/// <summary>Gets the namespace that contains the global constant.</summary>
		public Namespace Parent { get; internal set; }

		/// <summary>Gets the module that the constant is defined in.</summary>
		public Module Module { get; internal set; }

		public override Namespace GetContainingNamespace()
		{
			return Parent;
		}
	}

	/// <summary>
	/// Represents the declaration space of a block. Blocks (even those of lambda functions) are only ever
	/// contained within <see cref="Method"/>s and other blocks.
	/// </summary>
	public class BlockSpace : IDeclarationSpace
	{
		public BlockSpace(Block node, Method method)
		{
			Method = method;
			Node = node;
			if (node != null)
				node.DeclSpace = this;
		}
		public BlockSpace(Block node, BlockSpace parent)
		{
			Method = parent.Method;
			Parent = parent;
			Node = node;
			if (node != null)
				node.DeclSpace = this;
		}

		/// <summary>Gets the <see cref="Node"/> that introduced the block.</summary>
		public Block Node { get; private set; }

		/// <summary>Gets the method in which the block is contained.</summary>
		public Method Method { get; internal set; }

		/// <summary>Gets the parent of the block. This is always another block.</summary>
		public BlockSpace Parent { get; internal set; }

		/// <summary>Gets the statement that this block is the body of.</summary>
		public Statement Owner { get; internal set; }

		internal Dictionary<string, LocalMember> members = new Dictionary<string, LocalMember>();

		private string label;
		/// <summary>Gets the label of the block, or null if there is none.</summary>
		public string Label { get { return label; } }

		private ClosureClass closure;
		public ClosureClass ClosureClass { get { return closure; } }

		internal int BlockNumber;
		internal Variable ClosureVariable;
		/// <summary>
		/// Used by the method builder; initialized by the block.
		/// </summary>
		internal LocalVariable ClosureLocal;

		private HashSet<BlockSpace> capturedBlocks;

		public void DeclareVariable(Variable variable)
		{
			if (variable == null)
				throw new ArgumentNullException("variable");

			if (ContainsMember(variable.Name))
				throw new DuplicateNameException(variable.Node, variable.Name);

			members.Add(variable.Name, variable);
			variable.Parent = this;
		}

		public void DeclareConstant(LocalConstant constant)
		{
			if (constant == null)
				throw new ArgumentNullException("constant");

			if (ContainsMember(constant.Name))
				throw new DuplicateNameException(constant.Node, constant.Name);

			members.Add(constant.Name, constant);
			constant.Parent = this;
		}

		public void DeclareLocalFunction(LocalFunction function)
		{
			if (function == null)
				throw new ArgumentNullException("function");

			if (ContainsMember(function.Name))
				throw new DuplicateNameException(function.Node, function.Name);

			members.Add(function.Name, function);
			function.Parent = this;

			// Remember to mark the containing Method, too!
			// If this is a LocalMethod, then that method will mark its parent
			// once declared inside it.
			Method.AddLocalFunction(function);
		}

		public virtual bool ContainsMember(string name)
		{
			if (members.ContainsKey(name))
				return true;
			if (Parent != null)
				return Parent.ContainsMember(name);
			if (Method is LocalMethod)
				return ((IDeclarationSpace)Method).ContainsMember(name);
			return false;
		}

		public virtual NamedMember ResolveName(string name, Class fromClass)
		{
			if (members.ContainsKey(name))
				return members[name];
			if (Parent != null)
				return Parent.ResolveName(name, fromClass);

			if (Method != null)
				return ((IDeclarationSpace)Method).ResolveName(name, fromClass);

			return null;
		}

		IDeclarationSpace IDeclarationSpace.Parent { get { return Parent ?? (IDeclarationSpace)Method; } }

		public Namespace GetContainingNamespace()
		{
			if (Parent == null)
				return Method.GetContainingNamespace();
			return Parent.GetContainingNamespace();
		}

		public Class GetContainingClass(out bool hasInstance)
		{
			if (Method == null) // This is not supposed to happen!
				throw new InvalidOperationException("BlockSpace is not supposed to have a null Method.");

			// Note: this.Method may be a LocalMethod, but they do know
			// how to return their containing class correctly.
			return Method.GetContainingClass(out hasInstance);
		}

		public void SetLabel(string label)
		{
			if (LabelExists(label))
				throw new DeclarationException(this.Owner, string.Format(
					"The loop label '{0}' cannot be used in this scope, because it is already used in a parent scope.",
					label));

			this.label = label; // whee
		}

		public bool LabelExists(string label)
		{
			var block = this;

			do
			{
				if (block.label == label)
					return true;

				if (block.Parent != null)
					block = block.Parent;
				else if (block.Method is LocalMethod)
					block = ((LocalMethod)block.Method).Function.Parent;
				else
					block = null; // reached the end!
			} while (block != null);

			return false; // Not found. :)
		}

		/// <summary>
		/// Finds the nearest loop matching the specified label.
		/// </summary>
		/// <param name="errorNode">The node to report an error at if the algorithm passes a finally or fails to find an appropriate.</param>
		/// <param name="label">The loop label to look for, or null to find the nearest loop regardless of label.</param>
		/// <param name="clause">A <see cref="TryStatement"/> or <see cref="CatchClause"/> to test whether the loop is outside it, or null.</param>
		/// <param name="isLeave">If <see cref="clause"/> is null: true if the loop is outside a protected block, otherwise false. If <see cref="clause"/> is not null: true if the loop lies outside that clause, otherwise false.</param>
		/// <returns>The <see cref="IterationStatement"/> corresponding to the loop.</returns>
		public IterationStatement FindLoop(ParseNode errorNode, string label, out bool isLeave)
		{
			// Never cross the streams.
			var crossesFinally = false;
			IterationStatement result = null;
			isLeave = false;

			var block = this;
			do
			{
				var owner = block.Owner;
				if (owner is IterationStatement && 
					(label == null || ((IterationStatement)owner).Label == label))
				{
					// Found it!
					result = (IterationStatement)owner;
					break;
				}
				if (owner is FinallyClause)
					crossesFinally = true; // Oh dear
				if (owner is TryStatement || owner is CatchClause)
					isLeave = true;

				block = block.Parent; // This will stop at the method boundary. That's okay.
			} while (block != null);

			if (result == null)
				throw new CompileTimeException(errorNode, label == null ?
					"'break' or 'next' encountered outside loop." :
					"Unresolved 'break' or 'next' label.");
			if (crossesFinally)
				throw new CompileTimeException(errorNode,
					"A 'break' or 'next' inside a finally clause may not refer to a loop outside the finally clause.");
			return result;
		}

		public bool IsInsideFinally()
		{
			var block = this;
			do
			{
				if (block.Owner is FinallyClause)
					return true;

				block = block.Parent; // This stops at the method boundary, which we want
			} while (block != null);

			return false;
		}

		public bool IsInsideCatch(out bool crossesFinally)
		{
			crossesFinally = false;

			var block = this;
			do
			{
				if (block.Owner is FinallyClause)
					crossesFinally = true; // cannot 'throw;' in a finally nested inside a catch
				if (block.Owner is CatchClause)
					return true;

				block = block.Parent; // Stops at the method boundary, as desired
			} while (block != null);

			return false;
		}

		public bool IsInsideProtectedBlock()
		{
			var block = this;
			do
			{
				var owner = block.Owner;
				if (owner is TryStatement ||
					owner is CatchClause ||
					owner is FinallyClause)
					return true;

				block = block.Parent;
			} while (block != null);

			return false;
		}

		/// <summary>
		/// Informs the block that it needs to capture another block, which must be in the same method.
		/// </summary>
		/// <param name="block">The block to capture.</param>
		internal void Capture(BlockSpace block)
		{
			if (block == null)
				throw new ArgumentNullException("block");
			if (block == this)
				throw new ArgumentException("I can't bloody well capture myself!");

			// If the block comes from another method, then every method up to (but not including) that method
			// needs to capture the block as well.
			if (block.Method != this.Method && this.Method is LocalMethod &&
				((LocalMethod)this.Method).Function.Parent != block)
				((LocalMethod)this.Method).Function.Parent.Capture(block);

			if (capturedBlocks == null)
				capturedBlocks = new HashSet<BlockSpace>();
			capturedBlocks.Add(block);
		}

		/// <summary>
		/// Gets a lambda expression name that is guaranteed not to clash with
		/// the name of any local variable in scope.
		/// </summary>
		/// <param name="nameHint">A name to incorporate into the return value, or null.</param>
		/// <returns>A lambda expression name.</returns>
		internal string GetLambdaName(string nameHint)
		{
			var method = this.Method;
			while (method is LocalMethod)
				method = ((LocalMethod)method).Function.Parent.Method;

			return string.Format("<λ>{0}${1}", nameHint ?? "<anon>", method.LambdaNameCounter++);
		}
		/// <summary>
		/// Gets a lambda parameter name that is guaranteed not to clash with
		/// the name of any local variable in scope.
		/// </summary>
		/// <returns>A lambda parameter name.</returns>
		internal string GetLambdaParam()
		{
			var method = this.Method;
			while (method is LocalMethod)
				method = ((LocalMethod)method).Function.Parent.Method;

			return string.Format("<λ>arg${0}", method.LambdaParamCounter++);
		}

		internal ClosureClass GenerateClosureClass(Compiler compiler)
		{
			if (closure != null)
				return closure;

			// Generate outer blocks' closure classes first.
			// There's no real reason to do this other than to have some kind
			// of basically consistent block numbering.
			if (capturedBlocks != null)
				foreach (var block in capturedBlocks)
					block.GenerateClosureClass(compiler);

			var method = this.Method;
			while (method is LocalMethod)
				method = ((LocalMethod)method).Function.Parent.Method;

			var group = method.Group;

			Namespace ns;
			string namePrefix;
			if (group.Parent.Kind == MemberKind.Namespace)
			{
				namePrefix = "global";
				ns = group.ParentAsNamespace;
			}
			else // class
			{
				namePrefix = group.ParentAsClass.Name;
				ns = group.ParentAsClass.Parent;
			}

			closure = new ClosureClass(GetClosureClassName(namePrefix, method, out BlockNumber), this, ns, compiler);
			closure.SharedType = method.Group.ParentAsClass;
			ClosureVariable = new Variable(null, (VariableDeclarator)null) { Parent = this };

			ns.DeclareType(closure);
			compiler.AddType(closure);

			// And then we declare fields for all the variables that have been captured
			foreach (var member in members.Values)
				if (member.Kind == MemberKind.Variable && ((Variable)member).IsCaptured)
					closure.DeclareVariableField((Variable)member);

			if (capturedBlocks != null)
				foreach (var block in capturedBlocks)
					closure.DeclareBlockField(block);

			AddInitializers(closure);

			return closure;
		}

		private void AddInitializers(ClosureClass closure)
		{
			var init = new List<AssignmentExpression>(2 + (capturedBlocks == null ? 0 : capturedBlocks.Count));

			{ // Call the constructor for the closure class and assign it to the closure local
				var ctorMethod = AddClosureConstructor(closure);

				var closureLocalAccess = new LocalVariableAccess(ClosureVariable, LocalAccessKind.ClosureLocal);
				closureLocalAccess.IsAssignment = true;

				var newExpr = new ObjectCreationExpression(null, new List<Expression>()) { Constructor = ctorMethod };
				init.Add(new AssignmentExpression(closureLocalAccess, newExpr)
					{
						IgnoreValue = true,
					});
			}

			if (closure.ThisField != null)
			{
				var target = new InstanceMemberAccess(new LocalVariableAccess(ClosureVariable, LocalAccessKind.ClosureLocal),
					closure, closure.ThisField);
				target.IsAssignment = true;

				Expression value;
				if (this.Method is LocalMethod)
				{
					// If this is inside a local function, then the block declaring the local function
					// will have a closure class with a ThisField. That's where we get our instance from.
					var methodClosure = ((LocalMethod)Method).Function.Parent.closure;
					value = new InstanceMemberAccess(new ThisAccess(), methodClosure, methodClosure.ThisField);
				}
				else
					value = new ThisAccess();

				init.Add(new AssignmentExpression(target, value) { IgnoreValue = true });
			}

			if (capturedBlocks != null)
			{
				// Initialize all the fields of the blocks that have been captured

				// Reuse this node! Naughty.
				var closureLocalAccess = new LocalVariableAccess(ClosureVariable, LocalAccessKind.ClosureLocal);

				foreach (var block in capturedBlocks)
				{
					var target = new InstanceMemberAccess(closureLocalAccess, closure, closure.GetBlockField(block));
					target.IsAssignment = true;

					Expression value;
					if (block.Method == this.Method)
						// If the block is from the same method, then we just load its closure local
						// and store it in the appropriate field.
						value = new LocalVariableAccess(block.ClosureVariable, LocalAccessKind.ClosureLocal);
					else
					{
						// If the block comes from another method, then we know that the closure class
						// containing this method captures the block. So we load THAT field instead!
						// The closure class containing the current method is always in 'this'.
						// (Note: the closure class of the the current method is initialized in the parent
						// method, as the current method's declaring block belongs to it.)
						var methodBlock = ((LocalMethod)Method).Function.Parent;
						if (methodBlock == block)
							value = new ThisAccess();
						else
						{
							var methodClosure = methodBlock.closure;
							var field = methodClosure.GetBlockField(block);
							value = new InstanceMemberAccess(new ThisAccess(), methodClosure, field);
						}
					}

					init.Add(new AssignmentExpression(target, value) { IgnoreValue = true });
				}
			}

			// Initialize captured parameters, but only if this is actually the top block of a method.
			if (this.Parent == null)
				foreach (var member in members.Values)
					if (member.Kind == MemberKind.Variable)
					{
						var variable = (Variable)member;
						if (!(variable.IsCaptured &&
							(variable.VariableKind == VariableKind.Parameter ||
							variable.VariableKind == VariableKind.IterationVariable ||
							variable.VariableKind == VariableKind.CatchVariable)))
							continue;
						var closureLocalAccess = new LocalVariableAccess(ClosureVariable, LocalAccessKind.ClosureLocal);

						var target = new InstanceMemberAccess(closureLocalAccess, closure, variable.CaptureField);
						target.IsAssignment = true;

						init.Add(new AssignmentExpression(target, new LocalVariableAccess(variable, LocalAccessKind.NonCapturing))
							{
								IgnoreValue = true
							});
					}

			this.Node.Initializer = init;
		}

		private Method AddClosureConstructor(ClosureClass closure)
		{
			// Find the base constructor
			var baseCtor = closure.BaseType.FindConstructor(null, 0, closure);

			// Add a statement to this constructor body that calls the base constructor
			var ctorBody = new List<Statement>
			{
				new ExpressionStatement(
					new InvocationExpression( // new base();
						new InstanceMemberAccess(new ThisAccess(), (Class)closure.BaseType, baseCtor.Group), // base.'.new'
						new List<Expression>() // ()
					)
				)
			};
			var ctor = new Constructor(new Block(ctorBody), AccessLevel.Public, closure, Signature.Empty);
			closure.DeclareConstructor(ctor);

			// And return the method! Simple, innit?
			return ctor.Method;
		}

		private static string GetClosureClassName(string prefix, Method method, out int blockNumber)
		{
			var groupIndex = method.Group.IndexOfOverload(method);
			blockNumber = method.ClosureCounter++;
			return string.Format("Closure<{0}/{1}@{2}>__{3}", prefix, method.Name.Replace('.', '#'), groupIndex, blockNumber);
		}

		public static BlockSpace FromStatement(Statement stmt, BlockSpace parent)
		{
			if (stmt is ExternBody)
				return new ExternBlockSpace((ExternBody)stmt, parent);

			return new BlockSpace(stmt as Block, parent);
		}
		public static BlockSpace FromStatement(Statement stmt, Method method)
		{
			if (stmt is ExternBody)
				return new ExternBlockSpace((ExternBody)stmt, method);

			return new BlockSpace(stmt as Block, method);
		}
	}

	public class ExternBlockSpace : BlockSpace
	{
		public ExternBlockSpace(ExternBody entryPoint, BlockSpace parent)
			: base(entryPoint, parent)
		{
			this.entryPoint = entryPoint.EntryPoint.StringValue;
		}

		public ExternBlockSpace(ExternBody entryPoint, Method method)
			: base(entryPoint, method)
		{
			this.entryPoint = entryPoint.EntryPoint.StringValue;
		}

		private string entryPoint;
		/// <summary>Gets the name of the method to bind to in the native library.</summary>
		public string EntryPoint { get { return entryPoint; } }

		// We let ContainsMember and DeclareVariable work normally, because
		// we do need to verify that the parameter names are correct.

		public override NamedMember ResolveName(string name, Class fromClass)
		{
			throw new InvalidOperationException("An extern body does not contain any members.");
		}
	}

	public abstract class LocalMember : NamedMember
	{
		public LocalMember(string name, MemberKind kind, ParseNode node)
			: base(name, kind, node, AccessLevel.None)
		{ }

		public BlockSpace Parent { get; internal set; }

		public override Namespace GetContainingNamespace()
		{
			return Parent.GetContainingNamespace();
		}
	}

	/// <summary>
	/// Represents a local variable, that is, an assignable member declared within a block.
	/// This class is also used for parameters.
	/// </summary>
	public class Variable : LocalMember
	{
		public Variable(string name, VariableDeclarator node)
			: base(name, MemberKind.Variable, node)
		{
			varKind = Members.VariableKind.Regular;
		}
		public Variable(string name, Parameter node)
			: base(name, MemberKind.Variable, node)
		{
			varKind = Members.VariableKind.Parameter;
		}
		public Variable(string name, ParseNode node, VariableKind varKind)
			: this(name, node, MemberKind.Variable, varKind)
		{ }
		protected Variable(string name, ParseNode node, MemberKind kind, VariableKind varKind)
			: base(name, kind, node)
		{
			this.varKind = varKind;
		}

		private VariableKind varKind;
		/// <summary>Gets a value indicating whether the variable refers to a parameter.</summary>
		public bool IsParameter { get { return varKind == VariableKind.Parameter; } }

		public VariableKind VariableKind { get { return varKind; } }

		private bool isCaptured = false;

		public bool IsCaptured { get { return isCaptured; } }

		/// <summary>
		/// The field that the variable is captured in, if it is captured.
		/// For global variables, this is the field that contains the variable value.
		/// </summary>
		internal Field CaptureField;

		public VariableDeclarator VarNode
		{
			get { return (VariableDeclarator)Node; }
		}
		public Parameter ParamNode
		{
			get { return (Parameter)Node; }
		}

		internal virtual void Capture()
		{
			if (isCaptured)
				return;

			if (Parent == null)
				throw new InvalidOperationException("The local variable does not belong to a block and cannot be captured.");

			isCaptured = true;
		}
	}

	public enum VariableKind
	{
		/// <summary>The variable is a normal local variable.</summary>
		Regular,
		/// <summary>The variable is a parameter.</summary>
		Parameter,
		/// <summary>The variable is an iteration variable; that is, it is declared in a for-in loop.</summary>
		/// <remarks>
		/// This value is also used for variables declared in the for-in of a list comprehension.
		/// Variables with this kind MAY be referenced before their <see cref="ParseNode.StartIndex"/>,
		/// or after their <see cref="ParseNode.EndIndex"/>, but not inbetween (where the list expression goes).
		/// </remarks>
		IterationVariable,
		/// <summary>The variable is a catch variable, into which an error is caught.</summary>
		CatchVariable,
		/// <summary>The variable is a global variable.</summary>
		Global,
	}

	/// <summary>
	/// Represents a local constant, that is, a constant declared within a block.
	/// </summary>
	public class LocalConstant : LocalMember, IConstantMember
	{
		public LocalConstant(string name, VariableDeclarator node)
			: base(name, MemberKind.LocalConstant, node)
		{ }

		private ConstantState state;
		public ConstantState State { get { return state; } }

		/// <summary>Gets the value associated with this constant.</summary>
		public ConstantValue Value
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
	}

	public class LocalFunction : LocalMember, IDeclarationSpace
	{
		public LocalFunction(string name, LocalFunctionDeclaration node, BlockSpace parent)
			: base(name, MemberKind.LocalFunction, node)
		{
			Parent = parent;

			method = new LocalMethod(name, this, node.Body, node.Splat, node.Parameters != null ? node.Parameters.ToArray() : null);
			// Method flags are set at a later point, as appropriate!
			node.DeclSpace = this;
		}
		internal LocalFunction(string name, LocalFunctionDeclaration node, IDeclarationSpace globalParent)
			: base(name, MemberKind.LocalFunction, node)
		{
			Parent = globalParent as BlockSpace;
			if (!(globalParent is BlockSpace))
				this.globalParent = globalParent;

			method = new LocalMethod(name, this, node.Body, node.Splat, node.Parameters != null ? node.Parameters.ToArray() : null);
			node.DeclSpace = this;
		}

		private LocalMethod method;
		/// <summary>Gets the method that contains the implementation of the function.</summary>
		public LocalMethod Method { get { return method; } }

		private bool hasCaptures;
		/// <summary>Gets a value indicating whether the function captures variables from an outer method.</summary>
		public bool HasCaptures
		{
			get { return hasCaptures; }
			internal set { hasCaptures = value; }
		}

		private bool capturesThis;
		/// <summary>Gets a value indicating whether the function captures the 'this' instance of the class containing the function.</summary>
		public bool CapturesThis
		{
			get { return capturesThis; }
			internal set
			{
				capturesThis = value;
				if (value && Parent != null)
					// If this function is nested within another local function, then
					// we need to mark that as CapturesThis as well, even if it does
					// not capture anything else. This will allow it to be compiled to
					// an instance method, ensuring it, too, has access to the 'this'.
					if (Parent.Method is LocalMethod)
						((LocalMethod)Parent.Method).Function.CapturesThis = true;
			}
		}

		public LocalFunctionCompilationStrategy CompilationStrategy { get; internal set; }

		private IDeclarationSpace globalParent;
		internal IDeclarationSpace GlobalParent { get { return globalParent; } }

		/// <summary>
		/// Captures a variable.
		/// </summary>
		/// <param name="variable">The variable to capture</param>
		internal void Capture(Variable variable)
		{
			hasCaptures = true;
			variable.Capture();

			/* If the variable is in the same method as this function, then any outer
			 * local functions do NOT need to capture it. If it is not, then it must
			 * have been declared in a parent method, and then we must capture that
			 * method's block, wherever it may be. So we just tell the containing
			 * local function to capture the variable as well. Simple!
			 *
			 * An example, to make things clearer:
			 * public function foo() { // global
			 *     var x;
			 *     function bar() {
			 *         var y;
			 *         function baz() {
			 *             return x + y;
			 *         }
			 *         function qux() {
			 *             return y;
			 *         }
			 *     }
			 * }
			 * In this example, both baz and bar need to capture x; if bar did not
			 * capture it, it would not be available in baz. baz also needs to capture y,
			 * which bar does NOT need to capture, because it's declared in bar.
			 * qux, on the other hand, if baz did not exist, would only cause bar to
			 * capture y; or rather, to put it in a closure class. There would be no
			 * need for bar to be marked HasCaptures.
			 */
			if (this.Parent.Method != variable.Parent.Method)
			{
				if (this.Parent.Method is LocalMethod)
				{
					((LocalMethod)Parent.Method).Function.Capture(variable);
					// Also capture the block that declares the variable:
					Parent.Capture(variable.Parent);
				}
			}
			else
			{
				// The parent method is the same method that declares the variable.
				// If this local function is declared in a different scope than the variable,
				// then the scope of this closure class must capture the scope of the variable.

				if (this.Parent != variable.Parent)
					this.Parent.Capture(variable.Parent);
			}
		}
		/// <summary>
		/// Captures a local function.
		/// </summary>
		/// <param name="function">The local function to capture.</param>
		internal void Capture(LocalFunction function)
		{
			if (!function.hasCaptures)
				throw new ArgumentException("Cannot capture a function without captured variables.");

			hasCaptures = true;

			if (this.Parent.Method != function.Parent.Method)
			{
				if (this.Parent.Method is LocalMethod &&
					function.Parent != this.Method.Body) // cannot "capture" own child
				{
					((LocalMethod)Parent.Method).Function.Capture(function);
					// Also capture the block that declares the function:
					Parent.Capture(function.Parent);
				}
			}
			else
			{
				// The parent method is the same method that declares the function.
				// If this local function is declared in a different scope than the function,
				// then the scope of this closure class must capture the scope of the function.
				if (this.Parent != function.Parent)
					this.Parent.Capture(function.Parent);
			}
		}

		IDeclarationSpace IDeclarationSpace.Parent { get { return Parent != null ? Parent.Parent : globalParent; } }

		bool IDeclarationSpace.ContainsMember(string name)
		{
			if (Parent == null)
				return globalParent.ContainsMember(name);
			return Parent.ContainsMember(name);
		}

		NamedMember IDeclarationSpace.ResolveName(string name, Class fromClass)
		{
			if (Parent == null)
				return globalParent.ResolveName(name, fromClass);
			return Parent.ResolveName(name, fromClass);
		}

		public Class GetContainingClass(out bool hasInstance)
		{
			if (Parent == null)
				return globalParent.GetContainingClass(out hasInstance);
			return Parent.GetContainingClass(out hasInstance);
		}

		public override Namespace GetContainingNamespace()
		{
			if (Parent == null)
				return globalParent.GetContainingNamespace();
			return Parent.GetContainingNamespace();
		}
	}

	public class LocalMethod : Method, IDeclarationSpace
	{
		public LocalMethod(string name, LocalFunction function, Statement body, Splat splat, params Parameter[] parameters)
			: base(name, AccessLevel.Public, body, new Signature(parameters, splat))
		{
			this.function = function;
			IsImplDetail = true;

			if (parameters != null)
				foreach (var param in parameters)
					Body.DeclareVariable(new Variable(param.Name, param));
			Parameters = parameters;
		}

		private LocalFunction function;
		/// <summary>
		/// Gets the local function that declares this local method.
		/// </summary>
		public LocalFunction Function { get { return function; } }

		IDeclarationSpace IDeclarationSpace.Parent { get { return function; } }

		bool IDeclarationSpace.ContainsMember(string name)
		{
			return ((IDeclarationSpace)function).ContainsMember(name);
		}

		NamedMember IDeclarationSpace.ResolveName(string name, Class fromClass)
		{
			return ((IDeclarationSpace)function).ResolveName(name, fromClass);
		}

		public override Class GetContainingClass(out bool hasInstance)
		{
			return function.GetContainingClass(out hasInstance);
		}

		internal static string GetExtractedName(Method parentMethod, LocalMethod localMethod)
		{
			if (parentMethod == localMethod)
				// This is only the case for lambdas outside method bodies,
				// e.g. in field initializers and the like.
				return localMethod.Name;

			var groupIndex = parentMethod.Group != null ? parentMethod.Group.IndexOfOverload(parentMethod) : 0;
			var result = string.Format("{0}@{1}{2}{3}",
				parentMethod.Name, groupIndex,
				localMethod.Name[0] == '<' ? "" : "<ƒ>",
				localMethod.Name);
			if (localMethod.Name[0] != '<')
				result += "$" + parentMethod.LambdaNameCounter++;
			return result;
		}
	}

	public enum LocalFunctionCompilationStrategy
	{
		Invalid = 0,
		ClosureMethod = 1,
		InstanceMethod = 2,
		StaticMethod = 3,
	}

	public class GlobalVariable : Variable
	{
		public GlobalVariable(string name, VariableDeclarator node, Document document)
			: base(name, node, MemberKind.GlobalVariable, VariableKind.Global)
		{
			this.document = document;
		}

		private Document document;

		internal override void Capture()
		{
			base.Capture();
			document.Compiler.AddGlobalVariable(document, this);
		}
	}

	public enum ConstantState
	{
		/// <summary>
		/// The constant has not been reduced to a constant value yet.
		/// </summary>
		NotFolded = 0,
		/// <summary>
		/// The constant is in the process of being reduced.
		/// It is an error to refer to a constant with this state.
		/// </summary>
		Folding = 1,
		/// <summary>
		/// The constant has been successfully reduced to a constant value.
		/// It is safe to refer to it.
		/// </summary>
		HasValue = 2,
	}
}