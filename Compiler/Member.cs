using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Osprey;
using Osprey.Nodes;
using CI = System.Globalization.CultureInfo;

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

		public string GetMemberNamesJoined()
		{
			return GetMemberNamesJoined(Members);
		}

		public static string GetMemberNamesJoined(IEnumerable<NamedMember> members)
		{
			var fullNames = members.Select(m => m.FullName).ToArray();
			var nameToUsageCount = new Dictionary<string, int>();

			foreach (var name in fullNames)
			{
				int prev;
				nameToUsageCount.TryGetValue(name, out prev);
				nameToUsageCount[name] = prev + 1;
			}

			var sb = new StringBuilder();

			var i = 0;
			foreach (var member in members)
			{
				var name = fullNames[i];

				if (i > 0)
					sb.Append(", ");
				sb.Append(name);

				if (nameToUsageCount[name] > 1)
				{
					// If the same fully qualified name has been used more than once,
					// it must be a global member, so we try to add information about
					// the module of origin. Note that it is not possible to declare
					// a name in user code that conflicts with an imported name, so we
					// don't /actually/ have to check whether the module is null.
					if (member.Kind == MemberKind.Namespace)
						sb.Append(" (namespace)");
					else if (member is Type && ((Type)member).Module != null)
					{
						sb.Append(" (imported from '");
						sb.Append(((Type)member).Module.Name);
						sb.Append("')");
					}
					else if (member is Method && ((Method)member).Module != null)
					{
						sb.Append(" (imported from '");
						sb.Append(((Method)member).Module.Name);
						sb.Append("')");
					}
				}

				i++;
			}

			return sb.ToString();
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

		public override Method FindConstructor(ParseNode node, int argCount, Type instType, Type fromType)
		{
			throw new NotSupportedException();
		}
	}

	/// <summary>
	/// Describes the kind of a <see cref="Member"/>.
	/// </summary>
	public enum MemberKind
	{
		/// <summary>Namespace.</summary>
		Namespace,

		/// <summary>Global constant.</summary>
		GlobalConstant,
		/// <summary>Global variable.</summary>
		GlobalVariable,

		/// <summary>Class.</summary>
		Class,
		/// <summary>Enum.</summary>
		Enum,

		/// <summary>Constructor.</summary>
		Constructor,

		/// <summary>Class field.</summary>
		Field,
		/// <summary>Class constant.</summary>
		Constant,
		/// <summary>Enum field.</summary>
		EnumField,

		/// <summary>Method group.</summary>
		MethodGroup,
		/// <summary>Single method overload.</summary>
		Method,

		/// <summary>Property.</summary>
		Property,
		/// <summary>Property getter.</summary>
		PropertyGetter,
		/// <summary>Property setter.</summary>
		PropertySetter,

		/// <summary>An indexer member, which is a wrapper class for all the indexers in a class.</summary>
		IndexerMember,
		/// <summary>Indexer.</summary>
		Indexer,
		/// <summary>Indexer getter.</summary>
		IndexerGetter,
		/// <summary>Indexer setter.</summary>
		IndexerSetter,

		/// <summary>Operator.</summary>
		Operator,
		/// <summary>Iterator.</summary>
		Iterator,

		/// <summary>Local variable or parameter.</summary>
		Variable,
		/// <summary>Local constant.</summary>
		LocalConstant,
		/// <summary>Local function.</summary>
		LocalFunction,

		/// <summary>The member is ambiguous between several imported names.</summary>
		/// <remarks>This is not an error condition until you try to access the ambiguous member.</remarks>
		Ambiguous,

		/// <summary>The member is a reserved name.</summary>
		Reserved,
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
			DeclareType(type, false);
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
			var parts = name.Split(Compiler.Dot);

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
			if (offset == path.Length)
				return this;

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
			return ns.GetNamespace(path, offset + 1, imported);
		}

		internal Namespace FindNamespace(QualifiedName name)
		{
			return FindNamespace(name, name.Parts, 0);
		}
		private Namespace FindNamespace(ParseNode errorNode, string[] path, int offset)
		{
			var name = path[offset];
			Namespace ns = null;
			NamedMember mem;
			if (members.TryGetValue(name, out mem) &&
				mem.Kind == MemberKind.Namespace)
				ns = (Namespace)mem;
			if (ns == null)
				throw new CompileTimeException(errorNode,
					string.Format("The namespace '{0}' could not be found. (Did you forget to import a module?)",
						path.JoinString(".")));

			if (offset == path.Length - 1)
				return ns;
			else
				return ns.FindNamespace(errorNode, path, offset + 1);
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

			NamedMember member;
			if (members.TryGetValue(name, out member))
				return member;
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
				var i = 1;
				for (; i < name.Parts.Length; i++)
				{
					// Note: this loop is only entered if name.Parts.Length > 1
					if (member.Kind != MemberKind.Namespace)
						break;

					if (!((Namespace)member).members.TryGetValue(name.Parts[i], out member))
						break;
				}

				if (member != null && i == name.Parts.Length)
				{
					if (member.Kind == MemberKind.Class ||
						member.Kind == MemberKind.Enum)
						result = (Type)member;
					else if (member.Kind == MemberKind.Ambiguous)
						throw new AmbiguousNameException(name, (AmbiguousMember)member,
							string.Format("The type name '{0}' is ambiguous between the following members: {1}",
								name.ToString(), ((AmbiguousMember)member).GetMemberNamesJoined()));
				}

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
			return "λns$arg$";
		}

		internal string GetLambdaName(string nameHint)
		{
			return string.Format("λns${0}!{1}", nameHint ?? "__", lambdaNameCounter++);
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
		private List<Namespace> importedNamespaces;
		private Dictionary<string, NamedMember> aliases;

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

			var name = variable.Name;

			if (projectNamespace.ContainsMember(name) ||
				Members.ContainsKey(name))
				throw new DuplicateNameException(variable.Node, name);

			Members.Add(variable.Name, variable);
		}

		public void DeclareAlias(ParseNode errorNode, string name, NamedMember member)
		{
			if (aliases == null)
				aliases = new Dictionary<string, NamedMember>();

			if (aliases.ContainsKey(name))
				throw new DuplicateNameException(errorNode,
					string.Format("There is already an alias with the name '{0}'.", name));
			aliases.Add(name, member);
		}

		public void ImportNamespace(Namespace ns)
		{
			if (importedNamespaces == null)
				importedNamespaces = new List<Namespace>();
			importedNamespaces.Add(ns);
		}

		public override NamedMember ResolveName(string name, Class fromClass)
		{
			return ResolveName(name, fromClass, false);
		}

		public override Type ResolveTypeName(TypeName name, FileNamespace document)
		{
			var output = new HashSet<Type>();
			Type lastType = null;

			NamedMember member;
			// If the name is not global and has one part, see if there is an
			// alias with that name.
			if (aliases != null && !name.IsGlobal && name.Parts.Length == 1)
			{
				if (aliases.TryGetValue(name.Parts[0], out member))
				{
					lastType = member as Type;
					if (lastType != null)
						return name.Type = lastType;
				}
			}

			// Try to find a fully qualified member in the project namespace
			member = projectNamespace;
			for (var i = 0; i < name.Parts.Length; i++)
			{
				if (member.Kind != MemberKind.Namespace)
					break; // Not found!

				if (!((Namespace)member).Members.TryGetValue(name.Parts[i], out member))
					break; // Also not found!

				if (i == name.Parts.Length - 1)
				{
					if (member is Type)
						output.Add(lastType = (Type)member); // Found!
					else if (member.Kind == MemberKind.Ambiguous)
						foreach (var type in ((AmbiguousMember)member).Members.OfType<Type>())
							output.Add(type);
				}
			}

			// If the name is not global and has one part, try also to find
			// type members from imported namespaces.
			if (!name.IsGlobal && name.Parts.Length == 1)
			{
				var first = name.Parts[0];
				foreach (var ns in importedNamespaces)
					if (ns.Members.TryGetValue(first, out member))
					{
						if (member is Type)
							output.Add(lastType = (Type)member); // Found!
						else if (member.Kind == MemberKind.Ambiguous)
							foreach (var type in ((AmbiguousMember)member).Members.OfType<Type>())
								output.Add(type);
					}
			}

			if (output.Count == 0)
				throw new UndefinedNameException(name, name.ToString(),
					string.Format("The type name '{0}' could not be resolved.", name));
			if (output.Count == 1)
				// If there is only one type, then lastType can only have been assigned once.
				return name.Type = lastType;

			throw new AmbiguousTypeNameException(name, new AmbiguousTypeName(name, output.ToArray()),
				string.Format("The type name '{0}' is ambiguous between the following members: {1}",
					name.ToString(), AmbiguousMember.GetMemberNamesJoined(output)));
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

			NamedMember member, lastMember = null;
			if (aliases != null && aliases.TryGetValue(name, out member))
				return member;

			var matches = new HashSet<NamedMember>();
			if (Members.TryGetValue(name, out member))
				matches.Add(lastMember = member);

			if (projectNamespace.Members.TryGetValue(name, out member))
				matches.Add(lastMember = member);

			if (importedNamespaces != null)
				foreach (var ns in importedNamespaces)
					if (ns.Members.TryGetValue(name, out member))
					{
						// Only non-namespace members are imported from namespaces
						if (member.Kind != MemberKind.Namespace)
							matches.Add(lastMember = member);
					}

			if (matches.Count == 0)
				return null;
			if (matches.Count == 1)
				// If there is only one member, then 'lastMember' could only
				// have been assigned to once, so use that.
				return lastMember;
			return new AmbiguousMember(name, matches.ToArray());
		}
	}

	public class GlobalConstant : NamedMember, IConstantMember
	{
		public GlobalConstant(string name, VariableDeclarator node, bool isPublic)
			: base(name, MemberKind.GlobalConstant, node, isPublic ? AccessLevel.Public : AccessLevel.Internal)
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

				return ((ConstantExpression)varDecl.Initializer).Value;
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
			containingMember = method;
			Node = node;
			if (node != null)
				node.DeclSpace = this;
		}
		public BlockSpace(Block node, BlockSpace parent)
		{
			containingMember = parent.containingMember;
			this.parent = parent;
			Node = node;
			if (node != null)
				node.DeclSpace = this;
		}
		public BlockSpace(Block node, Class @class)
		{
			containingMember = @class;
			Node = node;
			if (node != null)
				node.DeclSpace = this;
		}

		/// <summary>Gets the <see cref="ParseNode"/> that introduced the block.</summary>
		public Block Node { get; private set; }

		// The Method (in almost all cases) or Class (in case of a use-in expression
		// in a field initializer) that ultimately contains the block.
		private IDeclarationSpace containingMember;
		/// <summary>Gets the member (Method or Class) that contains the block.</summary>
		public IDeclarationSpace ContainingMember { get { return containingMember; } }

		/// <summary>Gets the method in which the block is contained.</summary>
		public Method Method { get { return (Method)containingMember; } }

		private BlockSpace parent;
		/// <summary>Gets the parent of the block. This is always another block.</summary>
		public BlockSpace Parent { get { return parent; } }

		IDeclarationSpace IDeclarationSpace.Parent { get { return Parent ?? containingMember; } }

		/// <summary>Gets the statement that this block is the body of.</summary>
		public Statement Owner { get; internal set; }

		/// <summary>
		/// Gets the immediate parent block of this block. This differs from <see cref="Parent"/>
		/// in that it happily crosses <see cref="LocalMethod"/> boundaries.
		/// </summary>
		internal BlockSpace Up
		{
			get
			{
				if (Parent != null)
					return Parent;
				if (containingMember is LocalMethod)
					return ((LocalMethod)containingMember).Function.Parent;
				return null;
			}
		}

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

			EnsureDeclarable(variable.Name, variable.Node);

			members.Add(variable.Name, variable);
			variable.Parent = this;

			if (Up != null)
				Up.ReserveName(variable.Name, ReserveReason.UsedInChildBlock);
		}

		public void DeclareConstant(LocalConstant constant)
		{
			if (constant == null)
				throw new ArgumentNullException("constant");

			EnsureDeclarable(constant.Name, constant.Node);

			members.Add(constant.Name, constant);
			constant.Parent = this;

			if (Up != null)
				Up.ReserveName(constant.Name, ReserveReason.UsedInChildBlock);
		}

		public void DeclareLocalFunction(LocalFunction function)
		{
			if (function == null)
				throw new ArgumentNullException("function");

			EnsureDeclarable(function.Name, function.Node);

			members.Add(function.Name, function);
			function.Parent = this;

			if (Up != null)
				Up.ReserveName(function.Name, ReserveReason.UsedInChildBlock);

			// Remember to mark the containing Method, too!
			// If this is a LocalMethod, then that method will mark its parent
			// once declared inside it.
			Method.AddLocalFunction(function);
		}

		private void EnsureDeclarable(string name, ParseNode errorNode)
		{
			if (ContainsMember(name))
				throw new DuplicateNameException(errorNode, name);
			ReservedName reserved = GetReservedName(name);
			if (reserved != null)
				throw new DeclarationException(errorNode, reserved.GetReasonMessage());
		}

		public void ReserveName(string name, ReserveReason reason)
		{
			var member = new ReservedName(name, reason);

			if (reason == ReserveReason.UsedInChildBlock)
			{
				// Reserve the name in this block and every parent.
				var block = this;
				do
				{
					if (!block.members.ContainsKey(name))
						block.members.Add(name, member);
					block = block.Up;
				} while (block != null);
			}
			else if (reason == ReserveReason.UsedAsThisParameter)
			{
				// Make sure we are at the top
				if (Method is LocalMethod || Parent != null)
					throw new InvalidOperationException("Cannot reserve a 'this' parameter in a nested block.");

				if (members.ContainsKey(name))
					throw new DeclarationException(members[name].Node,
						member.GetReasonMessage());

				members.Add(name, member);
			}
			else
				members.Add(name, member);
		}

		public virtual bool ContainsMember(string name)
		{
			if (members.ContainsKey(name) &&
				members[name].Kind != MemberKind.Reserved)
				return true;
			if (Parent != null)
				return Parent.ContainsMember(name);
			if (containingMember is LocalMethod)
				return containingMember.ContainsMember(name);
			return false;
		}

		public virtual NamedMember ResolveName(string name, Class fromClass)
		{
			LocalMember member;
			if (members.TryGetValue(name, out member) &&
				member.Kind != MemberKind.Reserved)
				return member;

			if (Parent != null)
				return Parent.ResolveName(name, fromClass);

			if (containingMember is Method)
				return containingMember.ResolveName(name, fromClass);

			return null;
		}

		public ReservedName GetReservedName(string name)
		{
			LocalMember mem;
			if (members.TryGetValue(name, out mem) &&
				mem.Kind == MemberKind.Reserved)
				return (ReservedName)mem;

			var block = this.Up;
			while (block != null)
			{
				if (block.members.TryGetValue(name, out mem) &&
					mem.Kind == MemberKind.Reserved &&
					((ReservedName)mem).Reason != ReserveReason.UsedInChildBlock)
					return (ReservedName)mem;
				block = block.Up;
			}

			return null;
		}

		public Namespace GetContainingNamespace()
		{
			if (Parent == null)
				return containingMember.GetContainingNamespace();
			return Parent.GetContainingNamespace();
		}

		public Class GetContainingClass(out bool hasInstance)
		{
			if (containingMember == null) // This is not supposed to happen!
				throw new InvalidOperationException("BlockSpace.containingMember is not supposed to be null.");

			// Note: this.Method may be a LocalMethod, but they do know
			// how to return their containing class correctly.
			return containingMember.GetContainingClass(out hasInstance);
		}

		/// <summary>
		/// Determines whether the specified block is nested within this block.
		/// </summary>
		/// <param name="other">The block to test against.</param>
		/// <returns>True if <paramref name="other"/> is nested within this block; otherwise, false.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="other"/> is null.</exception>
		public bool Contains(BlockSpace other)
		{
			if (other == null)
				throw new ArgumentNullException("other");

			do
			{
				if (other == this)
					return true;
				other = other.Up;
			} while (other != null);

			return false;
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

				block = block.Up;
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
				throw new ArgumentException("I can't bloody well capture myself!", "block");

			// If the block comes from another method, then every method up to (but not including) that method
			// needs to capture the block as well.
			var localMethod = containingMember as LocalMethod;
			if (localMethod != null && block.containingMember != localMethod &&
				localMethod.Function.Parent != block)
				localMethod.Function.Parent.Capture(block);

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
			if (containingMember is Class)
				return ((Class)containingMember).GetLambdaName(nameHint);
			return ((Method)containingMember).GetLambdaName(nameHint);
		}
		/// <summary>
		/// Gets a lambda parameter name that is guaranteed not to clash with
		/// the name of any local variable in scope.
		/// </summary>
		/// <returns>A lambda parameter name.</returns>
		internal string GetLambdaParam()
		{
			if (containingMember is Class)
				return ((Class)containingMember).GetLambdaParam();
			return ((Method)containingMember).GetLambdaParam();
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

			bool _;
			var @class = containingMember.GetContainingClass(out _);
			var ns = containingMember.GetContainingNamespace();

			string namePrefix = @class != null ? @class.Name + "/" : "";

			closure = new ClosureClass(GetClosureClassName(namePrefix, out BlockNumber), this, ns, compiler);
			closure.SharedType = @class;
			ClosureVariable = new Variable(null, (VariableDeclarator)null) { Parent = this };

			ns.DeclareType(closure);
			compiler.AddType(closure);

			// And then we declare fields for all the variables that have been captured
			foreach (var member in members.Values)
				if (member.Kind == MemberKind.Variable && ((Variable)member).IsCaptured)
					closure.DeclareVariableField((Variable)member);

			// As well as captured blocks
			if (capturedBlocks != null)
				foreach (var block in capturedBlocks)
					closure.DeclareBlockField(block);

			AddInitializers(closure);

			return closure;
		}

		// Note: This does not add an initializer for the "<>this" field.
		// We don't know yet if we'll need one. It's added on demand when
		// DeclareThisField is called on the ClosureClass.
		private void AddInitializers(ClosureClass closure)
		{
			var init = new List<AssignmentExpression>(2 + (capturedBlocks == null ? 0 : capturedBlocks.Count));

			{ // Call the constructor for the closure class and assign it to the closure local
				var ctorMethod = AddClosureConstructor(closure);

				var closureLocalAccess = new LocalVariableAccess(ClosureVariable, LocalAccessKind.ClosureLocal);
				closureLocalAccess.IsAssignment = true;

				var newExpr = new ObjectCreationExpression(null, EmptyArrays.Expressions, false) { Constructor = ctorMethod };
				init.Add(new AssignmentExpression(closureLocalAccess, newExpr)
					{
						IgnoreValue = true,
					});
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
					if (block.containingMember == this.containingMember)
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

		/// <summary>
		/// This method is called from ClosureClass.DeclareThisField, to ensure that the
		/// closure class actually has an initializer for the “&lt;&gt;this” field.
		/// </summary>
		/// <param name="thisField">The field that contains the “this” value.</param>
		/// <remarks>
		/// This method is needed because the block does not know at the time it constructs
		/// the closure class whether it will contain a “this” field. Previously, the
		/// <see cref="AddInitializers"/> method added this initializer if closure.ThisField
		/// was not null, but it was always null.
		/// </remarks>
		internal void AddClosureThisFieldInitializer(Field thisField)
		{
			if (closure == null)
				throw new InvalidOperationException("Cannot call AddClosureThisFieldInitializer with no closure class.");

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

			this.Node.Initializer.Add(new AssignmentExpression(target, value) { IgnoreValue = true });
		}

		private Method AddClosureConstructor(ClosureClass closure)
		{
			// Find the base constructor
			var baseCtor = closure.BaseType.FindConstructor(null, 0, closure, closure);

			// Add a statement to this constructor body that calls the base constructor
			var ctorBody = new Statement[]
			{
				new ExpressionStatement(
					new InvocationExpression( // new base();
						new InstanceMemberAccess(new ThisAccess(), (Class)closure.BaseType, baseCtor.Group), // base.'.new'
						EmptyArrays.Expressions // ()
					)
				)
			};
			var ctor = new Constructor(new Block(ctorBody), AccessLevel.Internal, closure, Signature.Empty);
			closure.DeclareConstructor(ctor);

			// And return the method! Simple, innit?
			return ctor.Method;
		}

		internal string GetClosureClassName(string namePrefix, out int blockNumber)
		{
			if (containingMember is Class)
				return ((Class)containingMember).GetClosureClassName(namePrefix, out blockNumber);
			return ((Method)containingMember).GetClosureClassName(namePrefix, out blockNumber);
		}

		internal string GetGeneratorClassName(string namePrefix)
		{
			if (containingMember is Class)
				return ((Class)containingMember).GetGeneratorClassName(namePrefix);
			return ((Method)containingMember).GetGeneratorClassName(namePrefix);
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

		/// <summary>The number of times the local member is read.</summary>
		internal int ReadCount = 0;

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

		internal int AdditionalAccessStart = -1, AdditionalAccessEnd = -1;

		/// <summary>The number of times the variable is assigned to.</summary>
		internal int AssignmentCount = 0;

		public VariableDeclarator VarNode
		{
			get { return (VariableDeclarator)Node; }
		}
		public Parameter ParamNode
		{
			get { return (Parameter)Node; }
		}

		public bool CanSafelyInline
		{
			get
			{
				var kind = varKind;
				if (kind == VariableKind.IterationVariable ||
					kind == VariableKind.WithVariable)
					// These variables cannot be reassigned
					return true;
				if (kind == VariableKind.CatchVariable)
					// Catch variables get assigned once by the catch block;
					// they have AssignmentCount = 1 if never reassigned
					return AssignmentCount == 1;
				if (kind == VariableKind.Parameter)
					// Parameters have AssignmentCount = 0 if never reassigned
					return AssignmentCount == 0;

				var decl = Node as VariableDeclarator;
				if (decl != null)
					// If the variable is only ever assigned to by
					// its initializer, we can inline it
					return decl.Initializer != null && AssignmentCount == 1;

				// Other, more unusual scenarios are not worth
				// bothering with, so we just return false here.
				return false;
			}
		}

		internal virtual void Capture(ParseNode errorNode)
		{
			if (isCaptured)
				return;

			if (Parent == null)
				throw new InvalidOperationException("The local variable does not belong to a block and cannot be captured.");
			if (VariableKind == VariableKind.Parameter && ParamNode.IsByRef)
				throw new CompileTimeException(errorNode, "Cannot capture a pass-by-reference parameter.");

			isCaptured = true;
		}

		internal void SetAdditionalAccessRange(int start, int end)
		{
			AdditionalAccessStart = start;
			AdditionalAccessEnd = end;
		}

		public void EnsureAccessibleFrom(ParseNode node)
		{
			if (node.EndIndex < this.Node.StartIndex && (AdditionalAccessStart == -1 ||
				node.EndIndex < this.AdditionalAccessStart || node.StartIndex > AdditionalAccessEnd))
				throw new CompileTimeException(node,
					string.Format("The variable '{0}' cannot be accessed before its declaration.", Name));

			var varNode = Node as VariableDeclarator;
			if (varNode != null && varNode.Initializer != null &&
				node.EndIndex >= varNode.Initializer.StartIndex &&
				node.StartIndex < varNode.Initializer.EndIndex)
				throw new CompileTimeException(node,
					string.Format("The variable '{0}' cannot be accessed in its initializer.", Name));
		}

		public void EnsureAssignable(ParseNode errorNode)
		{
			if (VariableKind == VariableKind.IterationVariable ||
				VariableKind == VariableKind.WithVariable)
				throw new CompileTimeException(errorNode,
					string.Format("The variable '{0}' cannot be reassigned to because it is {1}.",
						Name,
						VariableKind == VariableKind.IterationVariable ? "an iteration variable" : "a 'with' variable"));
		}
	}

	public enum VariableKind
	{
		/// <summary>The variable is a normal local variable.</summary>
		Regular,
		/// <summary>The variable is a parameter.</summary>
		Parameter,
		/// <summary>The variable is an iteration variable; that is, it is declared in a for-in loop.</summary>
		IterationVariable,
		/// <summary>The variable is a catch variable, into which an error is caught.</summary>
		CatchVariable,
		/// <summary>The variable is a global variable.</summary>
		Global,
		/// <summary>The variable is declared in a 'with' statement.</summary>
		WithVariable,
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

				return ((ConstantExpression)varDecl.Initializer).Value;
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
					if (Parent.ContainingMember is LocalMethod)
						((LocalMethod)Parent.ContainingMember).Function.CapturesThis = true;
			}
		}

		/// <summary>
		/// The most deeply nested block that the function captures variables from.
		/// The extracted function ends up in the closure class of that block, instead
		/// of the actual parent block of the function.
		/// </summary>
		internal BlockSpace DeepestCapturedBlock;

		public LocalFunctionCompilationStrategy CompilationStrategy { get; internal set; }

		private IDeclarationSpace globalParent;
		internal IDeclarationSpace GlobalParent { get { return globalParent; } }

		/// <summary>
		/// Captures a variable.
		/// </summary>
		/// <param name="variable">The variable to capture</param>
		internal void Capture(Variable variable, ParseNode errorNode)
		{
			hasCaptures = true;
			variable.Capture(errorNode);
			var varBlock = variable.Parent;
			if (DeepestCapturedBlock == null || DeepestCapturedBlock.Contains(varBlock))
				DeepestCapturedBlock = varBlock;

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
			 * if baz did not exist, on the other hand, qux would only cause bar to
			 * capture y; or rather, to put it in a closure class. There would be no
			 * need for bar to be marked HasCaptures.
			 */
			if (Parent.ContainingMember != varBlock.ContainingMember)
			{
				if (Parent.ContainingMember is LocalMethod)
				{
					((LocalMethod)Parent.ContainingMember).Function.Capture(variable, errorNode);
					// Also capture the block that declares the variable:
					Parent.Capture(varBlock);
				}
			}
			else
			{
				// The parent method is the same method that declares the variable.
				// If this local function is declared in a different scope than the variable,
				// then the scope of this closure class must capture the scope of the variable.

				if (Parent != varBlock)
					Parent.Capture(varBlock);
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

			if (this.Parent.ContainingMember != function.Parent.ContainingMember)
			{
				if (this.Parent.ContainingMember is LocalMethod &&
					function.Parent != this.Method.Body) // cannot "capture" own child
				{
					((LocalMethod)Parent.ContainingMember).Function.Capture(function);
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
			: base(function.Node, name, AccessLevel.Internal, body, new Signature(parameters, splat))
		{
			this.function = function;
			IsImplDetail = true;

			if (parameters != null)
				foreach (var param in parameters)
					Body.DeclareVariable(new Variable(param.DeclaredName, param));
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

		public override Namespace GetContainingNamespace()
		{
			return function.GetContainingNamespace();
		}

		internal override string GetLambdaName(string nameHint)
		{
			if (function.Parent == null)
				// Lambda expression in field initializer
				return ((Class)function.GlobalParent).GetLambdaName(nameHint);
			return function.Parent.GetLambdaName(nameHint);
		}

		internal override string GetLambdaParam()
		{
			if (function.Parent == null)
				// Lambda expression in field initializer
				return ((Class)function.GlobalParent).GetLambdaParam();
			return function.Parent.GetLambdaParam();
		}

		internal override string GetClosureClassName(string prefix, out int blockNumber)
		{
			if (function.Parent == null)
				// Lambda expression in field initializer
				return ((Class)function.GlobalParent).GetClosureClassName(prefix, out blockNumber);
			return function.Parent.GetClosureClassName(prefix, out blockNumber);
		}

		internal override string GetGeneratorClassName(string prefix)
		{
			if (function.Parent == null)
				// Lambda expression in field initializer
				return ((Class)function.GlobalParent).GetGeneratorClassName(prefix);
			return function.Parent.GetGeneratorClassName(prefix);
		}

		internal static string GetExtractedName(Method parentMethod, LocalMethod localMethod)
		{
			var localName = localMethod.Name;
			if (parentMethod == localMethod)
				// This is only the case for lambdas outside method bodies,
				// e.g. in field initializers and the like.
				return localName;

			var groupIndex = parentMethod.Group != null && parentMethod.Group.Count > 1 ?
				"!" + parentMethod.Group.IndexOfOverload(parentMethod).ToStringInvariant() :
				"";
			var result = string.Concat(parentMethod.Name,
				groupIndex,
				localName.StartsWith("λ$") || localName.StartsWith("λc$") ? "" : "ƒ$",
				localName);
			if (!localName.StartsWith("λ$") && !localName.StartsWith("λc$"))
				result += "__" + parentMethod.LambdaNameCounter++;
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

		internal override void Capture(ParseNode errorNode)
		{
			base.Capture(errorNode);
			document.Compiler.AddGlobalVariable(document, this);
		}
	}

	public sealed class ReservedName : LocalMember
	{
		public ReservedName(string name, ReserveReason reason)
			: base(name, MemberKind.Reserved, null)
		{
			this.reason = reason;
		}

		private ReserveReason reason;
		/// <summary>
		/// Gets the reason why the name is reserved.
		/// </summary>
		public ReserveReason Reason { get { return reason; } }

		/// <summary>
		/// Gets a user-friendly message for the reason why the name is reserved, in the
		/// general format "The name 'blah' cannot be declared in this context, because ...".
		/// </summary>
		/// <returns>
		/// A string containing a user-friendly error message
		/// explaining why the name is reserved.
		/// </returns>
		public string GetReasonMessage()
		{
			return GetReasonMessage(Name, reason);
		}

		public static string GetReasonMessage(string name, ReserveReason reason)
		{
			const string before = "The name '{0}' cannot be declared in this context, because ";
			string explanation;
			switch (reason)
			{
				case ReserveReason.UsedInChildBlock:
					explanation = before + "it is used in a nested block";
					break;
				case ReserveReason.UsedAsThisParameter:
					explanation = before + "it is used in the 'this.{0}' parameter";
					break;
				default:
					return string.Format("The name '{0}' cannot be declared in this context.", name);
			}

			return string.Format(explanation, name);
		}
	}

	public enum ReserveReason
	{
		Invalid = 0,

		/// <summary>
		/// The name is used in a child block. It may not be used in a parent
		/// of that block, although it may be used in sibling and cousins of
		/// that block. See the example.
		/// </summary>
		/// <example>
		/// <code>
		/// function f() {
		///		// x is reserved with UsedInChildBlock in this block
		///		if one {
		///			// and in this
		///			if two {
		///				var x;
		///			}
		///			var x; // hence, this declaration is invalid
		///			if three {
		///				var x; // but not this
		///			}
		///		} else {
		///			var x; // nor this
		///		}
		///		var x; // and this is invalid again
		/// }
		/// </code>
		/// </example>
		UsedInChildBlock,

		/// <summary>
		/// The name is reserved because it is used as the identifier in
		/// a 'this.name' parameter. The name must unambiguously refer to
		/// that member in the body of the constructor.
		/// </summary>
		/// <example>
		/// <code>
		/// class C {
		///		public value;
		///		public new(this.value, /*invalid:*/ value) {
		///			var value; // invalid
		///		}
		///	}
		/// </code>
		/// </example>
		UsedAsThisParameter,
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