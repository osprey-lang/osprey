using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using Osprey.Members;
using CI = System.Globalization.CultureInfo;
using Enum = Osprey.Members.Enum;
using Type = Osprey.Members.Type;

namespace Osprey.Nodes
{
	/// <summary>
	/// Represents a node in a parse tree.
	/// </summary>
	public abstract class ParseNode
	{
		public override string ToString()
		{
			return this.ToString(0);
		}

		public int StartIndex;
		public int EndIndex;
		public Document Document;

		public abstract string ToString(int indent);
	}

	#region Namespaces

	public class Document : ParseNode
	{
		public Document(SourceFile sourceFile)
		{
			if (sourceFile == null)
				throw new ArgumentNullException("sourceFile");

			this.SourceFile = sourceFile;
		}

		public List<UseDirective> Uses = new List<UseDirective>();

		/// <summary>The global declaration space.</summary>
		/// <remarks>If the document does not contain a file-namespace-declaration, the name of this namespace declaration is null.</remarks>
		public NamespaceDeclaration GlobalDeclarationSpace = new NamespaceDeclaration(null);

		/// <summary>Global statements.</summary>
		public List<Statement> Statements = new List<Statement>();

		/// <summary>The version of the file, which determines the version of the project.</summary>
		public Version Version;

		/// <summary>The <see cref="FileNamespace"/> associated with this document.</summary>
		internal FileNamespace Namespace;
		
		/// <summary>The source file from which the document was created.</summary>
		public SourceFile SourceFile;

		/// <summary>The compiler instance that opened the file.</summary>
		internal Compiler Compiler;

		/// <summary>
		/// True if any use directive requires a second pass.
		/// See the remarks for <see cref="UseDirective.ResolveNames(Namespace)"/>.
		/// </summary>
		internal bool UseDirectivesRequireSecondPass;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();

			if (Uses.Count > 0)
				sb.AppendLine(Uses.JoinString("\r\n", indent));

			sb.AppendLine(Statements.JoinString("\r\n", indent));

			sb.AppendLine(GlobalDeclarationSpace.ToString(indent));

			return sb.ToString();
		}
	}

	public abstract class UseDirective : ParseNode
	{
		/// <summary>
		/// Attempts to resolve names within the use directive.
		/// </summary>
		/// <param name="globalNamespace">The global namespace for the project.</param>
		/// <param name="firstPass">
		/// True if this is the first pass (before type bodies are initialized),
		/// or false if it's the second (after type bodies are initialized).
		/// </param>
		/// <returns>True if the use directive requires a second pass; otherwise, false.</returns>
		/// <remarks>
		/// <para>The <paramref name="firstPass"/> parameter is required for two reasons:</para>
		/// <list type="bullet">
		///		<item><para>
		///			Class declarations may use a base type that is from an imported namespace or
		///			an aliased name, so we need to be able to resolve those use directives before
		///			we initialize base types and type bodies.
		///		</para></item>
		///		<item><para>
		///			Aliases may refer to members inside types (such as a static field or property),
		///			and we can't find those until we've initialized base types and type bodies.
		///		</para></item>
		/// </list>
		/// <para>The returning of a bool is strictly for performance reasons. Second passes are only
		/// used with aliases, and only aliases into type members, which are likely to be uncommon.
		/// By returning a bool, we save ourselves unnecessary work in the common case.</para>
		/// </remarks>
		public virtual bool ResolveNames(Namespace globalNamespace, bool firstPass) { return false; }
	}

	public sealed class UseModuleDirective : UseDirective
	{
		public UseModuleDirective(QualifiedName name)
		{
			Name = name;
		}

		/// <summary>The name of the module to import.</summary>
		public QualifiedName Name;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + "use " + Name + ";";
		}
	}

	public sealed class UseFileDirective : UseDirective
	{
		public UseFileDirective(StringLiteral name)
		{
			if (name.Value.Type != ConstantValueType.String)
				throw new ArgumentException("The script name must be a string literal.");

			Name = name;
		}

		/// <summary>The name of the script to import, which is a literal string.</summary>
		public StringLiteral Name;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + "use " + Name + ";";
		}
	}

	public sealed class UseNamespaceDirective : UseDirective
	{
		public UseNamespaceDirective(QualifiedName name)
		{
			Name = name;
		}

		/// <summary>The name of the namespace to import.</summary>
		public QualifiedName Name;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + "use namespace " + Name + ";";
		}

		public override bool ResolveNames(Namespace globalNamespace, bool firstPass)
		{
			var ns = globalNamespace.FindNamespace(Name);
			Document.Namespace.ImportNamespace(ns);
			return false;
		}
	}

	public sealed class UseAliasDirective : UseDirective
	{
		public UseAliasDirective(string aliasName, QualifiedName fullName)
		{
			AliasName = aliasName;
			FullName = fullName;
		}

		/// <summary>The name of the alias.</summary>
		public string AliasName;

		/// <summary>The name of the member the alias refers to.</summary>
		public QualifiedName FullName;

		// The type that was found during the first pass, if the alias
		// refers to a member inside that type.
		private Type foundType;
		// The index of the type within the qualified name's parts.
		private int foundIndex;

		public override string ToString(int indent)
		{
			return string.Format("{0}use {1} = {2};",
				new string('\t', indent), AliasName,
				FullName.ToString(indent + 1));
		}

		public override bool ResolveNames(Namespace globalNamespace, bool firstPass)
		{
			if (firstPass)
				return ResolveNamesFirstPass(globalNamespace);
			else if (foundType != null)
				ResolveNamesSecondPass();
			return false;
		}

		private bool ResolveNamesFirstPass(Namespace globalNamespace)
		{
			NamedMember member = null;

			var path = FullName.Parts;
			var i = 0;

			// First, let's look for namespaces as far along the path as we can
			Namespace ns = globalNamespace;
			while (ns != null && i < path.Length)
			{
				if (!ns.ContainsMember(path[i]))
					throw new CompileTimeException(this,
						string.Format("The member '{0}' could not be found. (Did you forget to import a module?)",
							string.Join(".", path, 0, i + 1)));
				member = ns.GetMember(path[i++]);
				ns = member as Namespace;
			}

			// If the member is not a namespace, then we may have stopped
			// before the last component in the path. The member can only
			// be a type, global function, global constant or ambiguous name.
			switch (member.Kind)
			{
				case MemberKind.GlobalConstant:
				case MemberKind.MethodGroup:
					if (i < path.Length)
						// This member is not the last; throw
						ErrorAccessThroughInstance(path, i);
					break;
				case MemberKind.Class:
				case MemberKind.Enum:
					if (i < path.Length)
					{
						// Look for the type member in the next pass
						foundIndex = i;
						foundType = member as Type;
						return true; // need second pass
					}
					break;
				case MemberKind.Ambiguous:
					throw new AmbiguousNameException(this, (AmbiguousMember)member);
			}

			Document.Namespace.DeclareAlias(this, AliasName, member);
			return false;
		}

		private void ResolveNamesSecondPass()
		{
			NamedMember member;

			var i = foundIndex;
			var path = FullName.Parts;
			if (foundType is Class)
			{
				var type = (Class)foundType;

				NamedMember inaccessibleMember;
				member = type.GetMember(path[i++], null, null, out inaccessibleMember);
				if (inaccessibleMember != null)
					throw new CompileTimeException(FullName,
						string.Format("The member '{0}' is not accessible from this context.",
							inaccessibleMember.FullName));
				if (member == null)
					throw new CompileTimeException(FullName,
						string.Format("The type '{0}' does not contain a definition for '{1}'.",
							type.FullName, path[i - 1]));

				var classMember = member as ClassMember;
				if (!(classMember != null ? classMember.IsStatic : ((MethodGroup)member).IsStatic))
					throw new InstanceMemberAccessException(this, member);

				if (i < path.Length)
					// The member is not the last; throw
					ErrorAccessThroughInstance(path, i);
			}
			else
			{
				var type = (Enum)foundType;

				member = type.GetMember(path[i++], null, null);
				if (member == null)
					throw new CompileTimeException(FullName,
						string.Format("The type '{0}' does not contain a definition for '{1}'.",
							type.FullName, path[i - 1]));

				if (i < path.Length)
					// The member is not the last; throw
					ErrorAccessThroughInstance(path, i);
			}

			Document.Namespace.DeclareAlias(this, AliasName, member);
		}

		private void ErrorAccessThroughInstance(string[] path, int i)
		{
			throw new CompileTimeException(this,
				string.Format("The member '{0}' cannot be aliased because it is accessed through an instance.",
					string.Join(".", path, 0, i + 1)));
		}
	}

	public sealed class NamespaceDeclaration : ParseNode
	{
		public NamespaceDeclaration(QualifiedName name)
		{
			Name = name;
		}

		/// <summary>The name of the namespace. If this is the file namespace and its name has not been set, this member is null.</summary>
		public QualifiedName Name;

		public List<TypeDeclaration> Types = new List<TypeDeclaration>();
		public List<GlobalConstantDeclaration> Constants = new List<GlobalConstantDeclaration>();
		public List<GlobalFunctionDeclaration> Functions = new List<GlobalFunctionDeclaration>();
		public List<NamespaceDeclaration> Namespaces = new List<NamespaceDeclaration>();

		internal Namespace Namespace;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			// Note: the global declaration space has a name of null.
			if (Name != null)
			{
				sb.Append('\t', indent);
				sb.Append("namespace ");
				sb.Append(Name.ToString());
				sb.AppendLine(" {");
				indent++;
			}

			if (Functions.Count > 0)
				sb.AppendLine(Functions.JoinString("\r\n", indent));
			if (Constants.Count > 0)
				sb.AppendLine(Constants.JoinString("\r\n", indent));
			if (Types.Count > 0)
				sb.AppendLine(Types.JoinString("\r\n", indent));
			if (Namespaces.Count > 0)
				sb.AppendLine(Namespaces.JoinString("\r\n", indent));

			if (Name != null)
			{
				indent--;
				sb.Append('\t', indent);
				sb.Append("}");
			}
			return sb.ToString();
		}
	}

	public sealed class GlobalFunctionDeclaration : ParseNode
	{
		public GlobalFunctionDeclaration(bool isPublic, LocalFunctionDeclaration func)
		{
			IsPublic = isPublic;
			Function = func;
			StartIndex = func.StartIndex;
			EndIndex = func.EndIndex;
			Document = func.Document;
		}

		/// <summary>Indicates whether the function has the modifier "public".</summary>
		public bool IsPublic;
		/// <summary>The function that was declared.</summary>
		public LocalFunctionDeclaration Function;

		public Token DocString;

		internal Method DeclSpace;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (IsPublic ? "public " : "") + Function.ToString(indent).TrimStart(null);
		}
	}

	public sealed class GlobalConstantDeclaration : ParseNode
	{
		public GlobalConstantDeclaration(bool isPublic, SimpleLocalVariableDeclaration declaration)
		{
			if (!declaration.IsConst)
				throw new ArgumentException("The simple local variable declaration must be constant.");

			IsPublic = isPublic;
			Declaration = declaration;
		}

		/// <summary>
		/// Indicates whether the declaration has the modifier "public".
		/// </summary>
		public bool IsPublic;

		/// <summary>
		/// The constant that were declared.
		/// </summary>
		public SimpleLocalVariableDeclaration Declaration;

		public Token DocString;

		/// <summary>
		/// The constants that were declared in this declaration.
		/// </summary>
		internal GlobalConstant[] Constants;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (IsPublic ? "public " : "") + Declaration.ToString(indent).TrimStart(null);
		}

		public void FoldConstant()
		{
			foreach (var decl in Declaration.Declarators)
			{
				decl.FoldConstant(true);

				var constValue = ((ConstantExpression)decl.Initializer).Value;

				if (IsPublic && constValue.Type == ConstantValueType.Enum &&
					constValue.EnumValue.Type.Access != AccessLevel.Public)
					throw new CompileTimeException(decl, "A public constant cannot contain a value of a non-public type.");
			}
		}
	}

	#endregion

	public abstract class MemberDeclaration : ParseNode
	{
		public MemberDeclaration(string name, AccessLevel access)
		{
			Name = name;
			Access = access;
		}

		/// <summary>The name of the member.</summary>
		public string Name;

		/// <summary>
		/// The access level of the member.
		/// This field is not relevant for all members, and should then be set to <see cref="AccessLevel.None"/>.
		/// </summary>
		public AccessLevel Access;

		public Token DocString = null;

		protected string AccessLevelToString()
		{
			switch (this.Access)
			{
				case AccessLevel.Public: return "public ";
				case AccessLevel.Protected: return "protected ";
				case AccessLevel.Private: return "private ";
				default: return "";
			}
		}

		public abstract void FoldConstant();

		public abstract void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler);
	}

	public abstract class TypeDeclaration : MemberDeclaration
	{
		public TypeDeclaration(string name, AccessLevel access)
			: base(name, access)
		{ }

		internal Type Type;

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler)
		{
			ResolveNames(context, document, compiler, false);
		}

		public abstract void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler, bool firstPass);
	}

	public sealed class EnumDeclaration : TypeDeclaration
	{
		public EnumDeclaration(string name, bool isSet, AccessLevel access)
			: base(name, access)
		{
			IsSet = isSet;
		}

		/// <summary>If true, the enum declaration represents a set enum.</summary>
		public bool IsSet;

		public List<EnumMember> Members = new List<EnumMember>();

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			sb.Append(AccessLevelToString());
			sb.Append("enum ");

			if (IsSet)
				sb.Append("set ");

			sb.Append(Name);
			sb.AppendLine(" {");

			sb.AppendLine(Members.JoinString(",\r\n", indent + 1));

			sb.Append('\t', indent);
			sb.Append("}");

			return sb.ToString();
		}

		public override void FoldConstant()
		{
			foreach (var member in Members)
				member.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler, bool firstPass)
		{
			if (firstPass)
			{
				foreach (var member in Members)
					member.ResolveNames(this.Type, document, compiler);
			}
		}
	}

	public sealed class EnumMember : MemberDeclaration
	{
		public EnumMember(string name, Expression value)
			: base(name, AccessLevel.None)
		{
			Value = value;
		}

		public Expression Value;
		internal EnumField Field;

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (Value == null ? Name : string.Format("{0} = {1}", Name, Value.ToString(indent)));
		}

		public override void FoldConstant()
		{
			if (Value == null)
				return;

			var value = Value.FoldConstant();
			var constExpr = value as ConstantExpression;
			if (constExpr == null || constExpr.Value.Type != ConstantValueType.Int)
				throw new CompileTimeException(value, "The value of an enum member must be a constant expression of type Int.");

			// And now convert the integral expression
			// to an enum value of the containing type
			Value = new ConstantExpression(ConstantValue.CreateEnumValue(constExpr.Value.IntValue, Field.Parent))
				.At(constExpr);

			// Prevent multiple folding
			Field.State = ConstantState.HasValue;
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler)
		{
			if (Value != null)
				Value = Value.ResolveNames(context, document, false, false);
		}
	}

	public sealed class ClassDeclaration : TypeDeclaration
	{
		public ClassDeclaration(string name, AccessLevel access)
			: base(name, access)
		{ }

		public TypeName BaseClass = null;

		public List<ConstructorDeclaration> Constructors = new List<ConstructorDeclaration>();
		public List<FieldDeclaration> Fields = new List<FieldDeclaration>();
		public List<FieldDeclaration> Constants = new List<FieldDeclaration>();
		public List<MethodDeclaration> Methods = new List<MethodDeclaration>();
		public List<PropertyAccessorDeclaration> Properties = new List<PropertyAccessorDeclaration>();
		public List<OperatorOverloadDeclaration> Operators = new List<OperatorOverloadDeclaration>();
		public IteratorDeclaration Iterator = null;
		public ConstructorDeclaration StaticConstructor = null;

		public string Initializer = null;
		
		public bool IsStatic, IsAbstract, IsInheritable, IsPrimitive;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			sb.Append(AccessLevelToString());
			if (IsStatic) sb.Append("static ");
			if (IsAbstract) sb.Append("abstract ");
			if (IsInheritable) sb.Append("inheritable ");
			sb.Append("class ");
			sb.Append(Name);
			if (BaseClass != null)
			{
				sb.Append(" is ");
				sb.Append(BaseClass.ToString(indent));
			}
			else if (Type != null && Type.BaseType != null)
			{
				sb.Append(" is ");
				sb.Append("‹type " + Type.BaseType.FullName + "›");
			}
			sb.AppendLine(" {");

			// class members
			if (Constructors.Count > 0)
				sb.AppendLine(Constructors.JoinString("\r\n", indent + 1));

			if (Fields.Count > 0)
				sb.AppendLine(Fields.JoinString("\r\n", indent + 1));

			if (Properties.Count > 0)
				sb.AppendLine(Properties.JoinString("\r\n", indent + 1));

			if (Methods.Count > 0)
				sb.AppendLine(Methods.JoinString("\r\n", indent + 1));

			if (Operators.Count > 0)
				sb.AppendLine(Operators.JoinString("\r\n", indent + 1));

			if (Constants.Count > 0)
				sb.AppendLine(Constants.JoinString("\r\n", indent + 1));

			if (Iterator != null)
				sb.AppendLine(Iterator.ToString(indent + 1));
			// end class members

			sb.Append('\t', indent);
			sb.Append("}");

			return sb.ToString();
		}

		public override void FoldConstant()
		{
			foreach (var constant in Constants)
				constant.FoldConstant();

			var instanceFieldsWithIniter = new List<VariableDeclarator>();
			var staticFieldsWithIniter = new List<VariableDeclarator>();
			foreach (var field in Fields)
			{
				field.FoldConstant();
				(field.IsStatic ? staticFieldsWithIniter : instanceFieldsWithIniter).AddRange(field.Declarators
					.Where(decl => decl.Initializer != null && !decl.Initializer.IsNull));
			}

			foreach (var prop in Properties)
				prop.FoldConstant();

			foreach (var ctor in Constructors)
			{
				ctor.FoldConstant();
				if (instanceFieldsWithIniter.Count > 0)
					ctor.AddFieldInitializers(instanceFieldsWithIniter, (Class)this.Type);
			}

			foreach (var method in Methods)
				method.FoldConstant();

			if (Iterator != null)
				Iterator.FoldConstant();

			if (StaticConstructor != null)
				StaticConstructor.FoldConstant();
			if (staticFieldsWithIniter.Count > 0)
			{
				var @class = (Class)this.Type;
				if (StaticConstructor == null)
				{
					StaticConstructor = new ConstructorDeclaration(new List<ConstructorParam>(), new Block());
					var staticCtorObject = new Constructor(StaticConstructor, @class);
					@class.DeclareStaticConstructor(staticCtorObject);
				}
				StaticConstructor.AddFieldInitializers(staticFieldsWithIniter, @class);
			}

			foreach (var op in Operators)
				op.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler, bool firstPass)
		{
			var typeContext = this.Type;

			if (firstPass)
			{
				foreach (var constant in Constants)
					constant.ResolveNames(typeContext, document, compiler);
			}
			else
			{
				Action<Method, Compiler> processMethod = (method, compilerInner) =>
				{
					if (!method.IsAbstract)
					{
						if (method.HasLocalFunctions)
							compilerInner.AddMethodWithLocalFunctions(method);
						if (method.IsGenerator)
							compilerInner.AddGeneratorMethod(method);
					}
				};

				foreach (var method in Methods)
				{
					method.ResolveNames(typeContext, document, compiler);
					processMethod(method.DeclSpace, compiler);
				}

				foreach (var ctor in Constructors)
				{
					ctor.ResolveNames(typeContext, document, compiler);
					processMethod(ctor.DeclSpace, compiler);
				}

				foreach (var field in Fields)
					field.ResolveNames(typeContext, document, compiler);

				foreach (var prop in Properties)
				{
					prop.ResolveNames(typeContext, document, compiler);

					if (!prop.IsAbstract)
						processMethod(prop.DeclSpace, compiler);
				}

				if (Iterator != null)
				{
					Iterator.ResolveNames(typeContext, document, compiler);
					processMethod(Iterator.DeclSpace, compiler);
				}

				if (StaticConstructor != null)
				{
					StaticConstructor.ResolveNames(typeContext, document, compiler);
					processMethod(StaticConstructor.DeclSpace, compiler);
				}

				foreach (var op in Operators)
				{
					op.ResolveNames(typeContext, document, compiler);
					processMethod(op.DeclSpace, compiler);
				}
			}
		}
	}

	public sealed class FieldDeclaration : MemberDeclaration
	{
		public FieldDeclaration(AccessLevel access, bool isConst, List<VariableDeclarator> declarators)
			: base(null, access)
		{
			IsConstant = isConst;
			Declarators = declarators;
		}

		public bool IsConstant, IsStatic;
		/// <summary>The fields that are being declared.</summary>
		public List<VariableDeclarator> Declarators;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			sb.Append(AccessLevelToString());
			if (IsStatic)
				sb.Append("static ");
			if (IsConstant)
				sb.Append("const ");
			else if (Access == AccessLevel.None)
				sb.Append("var ");
			sb.Append(Declarators.JoinString(", ", indent + 1));
			sb.Append(";");
			return sb.ToString();
		}

		public override void FoldConstant()
		{
			foreach (var decl in Declarators)
			{
				decl.FoldConstant(isConst: IsConstant);
				if (IsConstant)
				{
					var constValue = ((ConstantExpression)decl.Initializer).Value;
					if (Access == AccessLevel.Public &&
						constValue.Type == ConstantValueType.Enum &&
						constValue.EnumValue.Type.Access != AccessLevel.Public)
						throw new CompileTimeException(decl, "A public constant cannot contain a value of a non-public type.");
				}
			}
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler)
		{
			foreach (var decl in Declarators)
			{
				var localsBefore = compiler.MethodsWithLocalFunctionsCount;
				decl.ResolveNames(context, document);

				// If MethodsWithLocalFunctionsCount is different, then the initializer contains a lambda expression.
				if (compiler.MethodsWithLocalFunctionsCount != localsBefore)
				{
					var _decl = decl;
					compiler.AddLocalExtractor(() =>
					{
						_decl.Initializer = _decl.Initializer.TransformClosureLocals(null, false);
					});
				}
			}
		}
	}

	public sealed class ConstructorDeclaration : MemberDeclaration
	{
		public ConstructorDeclaration(AccessLevel access, List<ConstructorParam> parameters, Splat splat, Block body)
			: base("new", access)
		{
			Parameters = parameters;
			Splat = splat;
			Body = body;
		}
		public ConstructorDeclaration(List<ConstructorParam> parameters, Block body)
			: base("init", AccessLevel.Private)
		{
			IsStatic = true;
			Parameters = parameters;
			Splat = Splat.None;
			Body = body;
		}

		/// <summary>Indicates whether the constructor is a static constructor.</summary>
		public bool IsStatic;
		/// <summary>The parameters of the constructor.</summary>
		public List<ConstructorParam> Parameters;
		/// <summary>The location of the splat, if any.</summary>
		public Splat Splat;
		/// <summary>The body of the constructor.</summary>
		public Block Body;
		/// <summary>The constructor call. If there is one, this value is also the first statement in <see cref="Body"/>.</summary>
		public ConstructorCall ConstructorCall;

		internal Method DeclSpace { get { return Body.DeclSpace.Method; } }

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			if (IsStatic)
				sb.Append("static ");
			else
				sb.Append(AccessLevelToString());

			sb.Append("new(");

			if (Splat == Splat.Beginning)
				sb.Append("...");
			sb.Append(Parameters.JoinString(", "));
			if (Splat == Splat.End)
				sb.Append("...");

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

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler)
		{
			if (!IsStatic)
			{
				bool _;
				var @class = context.GetContainingClass(out _);

				if (!(Body is ExternBody) && ConstructorCall == null &&
					@class.BaseType != null)
				{
					ConstructorCall = new ConstructorCall(new List<Expression>(), false, true)
						{
							StartIndex = this.StartIndex,
							EndIndex = this.EndIndex,
							Document = this.Document,
						};
					Body.Statements.Insert(0, ConstructorCall);
				}

				foreach (var param in Parameters)
				{
					param.ResolveNames(context, document);
					if (param.HasThisPrefix)
					{
						if (Body.Initializer == null)
							Body.Initializer = new List<AssignmentExpression>();

						var expr = new AssignmentExpression(
							new InstanceMemberAccess(new ThisAccess(), @class, param.Member)
							{
								IsAssignment = true
							},
							new LocalVariableAccess(
								(Variable)Body.DeclSpace.members[param.DeclaredName],
								LocalAccessKind.NonCapturing
							)
						) { IgnoreValue = true };
						expr.At(param.StartIndex, param.EndIndex, param.Document);
						Body.Initializer.Add(expr);
					}
				}
			}

			Body.ResolveNames(context, document, true);
		}

		internal void AddFieldInitializers(IEnumerable<VariableDeclarator> fields, Class @class)
		{
			if (Body is ExternBody || ConstructorCall != null && !ConstructorCall.IsBaseConstructor)
				return; // No field initializers for extern bodies or ctors with 'new this(...);'

			if (Body.Initializer == null)
				Body.Initializer = new List<AssignmentExpression>();

			// Note: for field initializers, we use the special class FieldInitializer,
			// which updates its Value immediately before compilation, because it might
			// change if the field initializer expression is or contains a lambda.
			// FieldInitializer inherits from AssignmentExpression and only overrides
			// the Compile method.

			var index = 0;
			if (!IsStatic)
			{
				// Do not generate initializer code for fields assigned to
				// in 'this' parameters.
				var fieldParams = new HashSet<Field>(Parameters
					.Where(p => p.HasThisPrefix && p.Member.Kind == MemberKind.Field)
					.Select(p => (Field)p.Member));

				foreach (var field in fields)
				{
					var f = (Field)@class.GetMember(field.Name);
					if (fieldParams.Contains(f))
						continue;

					var expr = new FieldInitializer(
						new InstanceMemberAccess(new ThisAccess(), @class, f) { IsAssignment = true },
						field
					);
					expr.IgnoreValue = true;
					expr.At(field.Initializer);
					Body.Initializer.Insert(index++, expr);
				}
			}
			else
			{
				foreach (var field in fields)
				{
					var expr = new FieldInitializer(
						new StaticFieldAccess((Field)@class.GetMember(field.Name)) { IsAssignment = true },
						field
					);
					expr.IgnoreValue = true;
					expr.At(field.Initializer);
					Body.Initializer.Insert(index++, expr);
				}
			}
		}
	}

	public sealed class ConstructorParam : Parameter
	{
		public ConstructorParam(string name, bool hasThisPrefix, bool isByRef)
			: base(name, isByRef)
		{
			HasThisPrefix = hasThisPrefix;
		}
		public ConstructorParam(string name, bool hasThisPrefix, Expression defaultValue)
			: base(name, defaultValue)
		{
			HasThisPrefix = hasThisPrefix;
		}

		/// <summary>Indicates whether the parameter has the prefix 'this.', for auto-assign parameters.</summary>
		public bool HasThisPrefix;

		/// <summary>If <see cref="HasThisPrefix"/> is true, this member contains the member that the parameter assigns to.</summary>
		internal ClassMember Member;

		public override string DeclaredName { get { return HasThisPrefix ? "this:" + Name : Name; } }

		public override string ToString(int indent)
		{
			string result = Name;
			if (IsByRef)
				result = "ref " + result;
			else
			{
				if (HasThisPrefix)
					result = "this." + result;
				if (DefaultValue != null)
					result += " = " + DefaultValue.ToString(indent + 1);
			}
			return result;
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			base.ResolveNames(context, document); // Default value

			if (HasThisPrefix)
			{
				bool _;
				var @class = context.GetContainingClass(out _);

				if (@class == null)
					throw new InvalidOperationException("ConstructorParam found outside class.");

				NamedMember inaccessibleMember;
				var member = @class.GetMember(this.Name, instType: @class, fromType: @class,
					inaccessibleMember: out inaccessibleMember);
				if (inaccessibleMember != null)
					throw new CompileTimeException(this, string.Format("The member '{0}' is not accessible in this context.",
						inaccessibleMember.FullName));
				if (member == null)
					throw new UndefinedNameException(this, this.Name,
						string.Format("The type '{0}' does not contain a definition for '{1}'.",
							@class.FullName, this.Name));

				if (member.Kind != MemberKind.Field &&
					(member.Kind != MemberKind.Property || ((Property)member).PropertyKind == PropertyKind.ReadOnly))
					throw new CompileTimeException(this, string.Format("The member '{0}.{1}' cannot be assigned to.",
						@class.FullName, this.Name));

				// At this point, we know it's a field or a property, and those are both derived from ClassMember.
				var classMem = (ClassMember)member;
				if (classMem.IsStatic)
					throw new CompileTimeException(this, string.Format("The member '{0}.{1}' is static and cannot be used in a 'this' parameter.",
						@class.FullName, this.Name));
				if (classMem.Parent != @class)
					throw new CompileTimeException(this, string.Format("The member '{0}.{1}' cannot be used in a 'this' parameter, because it is not declared in the class '{2}'.",
						classMem.Parent.FullName, this.Name, @class.FullName));

				this.Member = classMem;
			}
		}
	}

	public sealed class MethodDeclaration : MemberDeclaration
	{
		public MethodDeclaration(string name, AccessLevel access, List<Parameter> parameters, Splat splat, Statement body)
			: base(name, access)
		{
			Parameters = parameters;
			Splat = splat;
			Body = body;
		}

		/// <summary>The parameters of the method.</summary>
		public List<Parameter> Parameters;
		/// <summary>The location of the splat, if any.</summary>
		public Splat Splat;
		/// <summary>The method body. This can be a <see cref="Block"/> or <see cref="EmptyStatement"/>.</summary>
		public Statement Body;

		public bool IsStatic, IsAbstract, IsOverride, IsOverridable;

		internal Method DeclSpace;

		internal string DeclaredName { get { return Name == "this" ? Class.InvocatorName : Name; } }

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			sb.Append(AccessLevelToString());

			if (IsStatic) sb.Append("static ");
			if (IsAbstract) sb.Append("abstract ");
			if (IsOverridable) sb.Append("overridable ");
			if (IsOverride) sb.Append("override ");

			if (Access == AccessLevel.None)
				sb.Append("function ");

			sb.Append(Name);
			if (Splat == Splat.Beginning)
				sb.Append("(...");
			else
				sb.Append("(");

			sb.Append(Parameters.JoinString(", "));

			if (Splat == Splat.End)
				sb.Append("...)");
			else
				sb.Append(")");

			if (Body == null)
				sb.Append(";"); // empty method
			else
			{
				sb.Append(" ");
				sb.Append(Body.ToString(indent));
			}

			return sb.ToString();
		}

		public override void FoldConstant()
		{
			foreach (var param in Parameters)
				param.FoldConstant();

			Body.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler)
		{
			foreach (var param in Parameters)
				param.ResolveNames(context, document);

			Body.ResolveNames(context, document, true);
		}
	}

	public class Parameter : ParseNode
	{
		public Parameter(string name, bool isByRef)
		{
			Name = name;
			IsByRef = isByRef;
			DefaultValue = null;
		}
		public Parameter(string name, Expression defaultValue)
		{
			Name = name;
			IsByRef = false;
			DefaultValue = defaultValue;
		}
		/// <summary>The name of the parameter.</summary>
		public string Name;
		/// <summary>Whether the parameter is passed by reference ("ref ...").</summary>
		public bool IsByRef;
		/// <summary>The default value of the parameter, or null if the parameter is required.</summary>
		public Expression DefaultValue = null;

		/// <summary>Gets the name that the parameter is declared with.</summary>
		public virtual string DeclaredName { get { return Name; } }

		public override string ToString(int indent)
		{
			if (DefaultValue != null)
				return Name + " = " + DefaultValue.ToString(indent + 1);
			else if (IsByRef)
				return "ref " + Name;
			else
				return Name;
		}

		public void FoldConstant()
		{
			if (DefaultValue != null)
			{
				DefaultValue = DefaultValue.FoldConstant();
				var value = DefaultValue;
				if (!(value is ConstantExpression ||
					value is ListLiteralExpression && ((ListLiteralExpression)value).Values.Count == 0 ||
					value is HashLiteralExpression && ((HashLiteralExpression)value).Members.Count == 0))
					throw new CompileTimeException(value, "The default value of an optional parameter must be a constant expression, [] or {}.");
			}
		}

		public virtual void ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			if (DefaultValue != null)
				DefaultValue = DefaultValue.ResolveNames(context, document, false, false);
		}
	}

	public class PropertyAccessorDeclaration : MemberDeclaration
	{
		public PropertyAccessorDeclaration(string name, AccessLevel access, bool isSetter, Statement body)
			: base(name, access)
		{
			IsSetter = isSetter;
			Body = body;
		}

		/// <summary>Indicates whether the property accessor is a setter.
		/// If false, the property accessor is a getter.</summary>
		public bool IsSetter;
		/// <summary>
		/// The body of the property accessor.
		/// On getters, this can be an ExpressionStatement, a Block or an EmptyStatement.
		/// On setters, only the latter two are possible.
		/// </summary>
		public Statement Body;

		public bool IsStatic, IsAbstract, IsOverride, IsOverridable;

		internal Method DeclSpace;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			sb.Append(AccessLevelToString());
			if (IsStatic) sb.Append("static ");
			if (IsAbstract) sb.Append("abstract ");
			if (IsOverridable) sb.Append("overridable ");
			if (IsOverride) sb.Append("override ");

			if (IsSetter)
				sb.Append("set ");
			else
				sb.Append("get ");

			sb.Append(GetName());
			if (Body is ExpressionStatement)
			{
				sb.Append(" = ");
				sb.Append(((ExpressionStatement)Body).Expression.ToString(indent + 1));
				sb.Append(";");
			}
			else if (Body != null) // Block
			{
				sb.Append(" ");
				sb.Append(Body.ToString(indent));
			}
			else
				sb.Append(";");

			return sb.ToString();
		}

		protected virtual string GetName()
		{
			return Name;
		}

		public override void FoldConstant()
		{
			Body.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler)
		{
			Body.ResolveNames(context, document, true);
		}
	}

	public sealed class IndexerAccessorDeclaration : PropertyAccessorDeclaration
	{
		public IndexerAccessorDeclaration(AccessLevel access, bool isSetter, List<Parameter> parameters, Statement body)
			: base(".item", access, isSetter, body)
		{
			this.Parameters = parameters;
		}

		/// <summary>The parameters of the indexer.</summary>
		public List<Parameter> Parameters;

		protected override string GetName()
		{
			return "this[" + Parameters.JoinString(", ") + "]";
		}

		public override void FoldConstant()
		{
			foreach (var param in Parameters)
				param.FoldConstant();

			base.FoldConstant(); // Body
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler)
		{
			foreach (var param in Parameters)
				param.ResolveNames(context, document);

			base.ResolveNames(context, document, compiler); // Body
		}
	}

	/* Ovum's source has the following to say about operator indices:
	 * 
	 * enum class Operator : uint8_t
	 * {
	 *     ADD,    // The binary + operator.  ( 0)
	 *     SUB,    // The binary - operator.  ( 1)
	 *     OR,     // The | operator.	     ( 2)
	 *     XOR,    // The ^ operator.	     ( 3)
	 *     MUL,    // The * operator.	     ( 4)
	 *     DIV,    // The / operator.	     ( 5)
	 *     MOD,    // The % operator.	     ( 6)
	 *     AND,    // The & operator.	     ( 7)
	 *     POW,    // The ** operator.	     ( 8)
	 *     SHL,    // The << operator.	     ( 9)
	 *     SHR,    // The >> operator.	     (10)
	 *     HASHOP, // The # operator.	     (11)
	 *     DOLLAR, // The $ operator.	     (12)
	 *     PLUS,   // The unary + operator.   (13)
	 *     NEG,    // The unary - operator.   (14)
	 *     NOT,    // The ~ operator.	     (15)
	 *     EQ,     // The == operator.	     (16)
	 *     CMP,    // The <=> operator.	     (17)
	 * };
	 * 
	 * These are the indices that OperatorOverloadDeclaration.GetIndex()
	 * refers to. If the Ovum order or numbering changes, these absolutely
	 * positively must must must must be updated! If you don't, you'll
	 * break spacetime or something.
	 */

	public abstract class OperatorOverloadDeclaration : MemberDeclaration
	{
		protected OperatorOverloadDeclaration(Block body) : base("operator", AccessLevel.None)
		{
			Body = body;
		}

		/// <summary>The body of the operator overload.</summary>
		public Block Body;

		internal Method DeclSpace { get { return Body.DeclSpace.Method; } }

		public abstract string GetMethodName();

		/// <summary>
		/// Gets the numeric index of this operator within the operators list.
		/// </summary>
		/// <returns>The index of this operator within the operators list.</returns>
		public abstract int GetIndex();

		public override void FoldConstant()
		{
			Body.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler)
		{
			Body.ResolveNames(context, document, true);
		}
	}

	public sealed class UnaryOperatorOverload : OperatorOverloadDeclaration
	{
		public UnaryOperatorOverload(UnaryOperator op, string operand, Block body)
			: base(body)
		{
			Operator = op;
			Operand = operand;
		}

		/// <summary>The operator that is being overloaded.</summary>
		public UnaryOperator Operator;
		/// <summary>The name of the operand.</summary>
		public string Operand;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			sb.Append("operator ");
			sb.Append(Operator == UnaryOperator.Plus ? "+" : Operator == UnaryOperator.Minus ? "-" : "~");
			sb.Append("(");
			sb.Append(Operand);
			sb.Append(") ");
			sb.Append(Body.ToString(indent));
			return sb.ToString();
		}

		public override string GetMethodName()
		{
			switch (Operator)
			{
				case UnaryOperator.Plus:       return "op:Plus";
				case UnaryOperator.Minus:      return "op:Negate";
				case UnaryOperator.BitwiseNot: return "op:Not";
				default:
					throw new InvalidOperationException("UnaryOperatorOverload with invalid operator.");
			}
		}

		public override int GetIndex()
		{
			switch (Operator)
			{
				case UnaryOperator.Plus: return 13;
				case UnaryOperator.Minus: return 14;
				case UnaryOperator.BitwiseNot: return 15;
				// Note: 'not' is not overridable.
				default:
					throw new InvalidOperationException("UnaryOperatorOverload with invalid operator.");
			}
		}
	}

	public sealed class BinaryOperatorOverload : OperatorOverloadDeclaration
	{
		public BinaryOperatorOverload(BinaryOperator op, string left, string right, Block body)
			: base(body)
		{
			this.Operator = op;
			this.Left = left;
			this.Right = right;
		}

		/// <summary>The operator that is being overloaded.</summary>
		public BinaryOperator Operator;
		/// <summary>The name of the left operand.</summary>
		public string Left;
		/// <summary>The name of the right operand.</summary>
		public string Right;

		public override string ToString(int indent)
		{
			var sb = new StringBuilder();
			sb.Append('\t', indent);
			sb.Append("operator ");
			sb.Append(BinaryOperatorExpression.GetOperatorToken(Operator));
			sb.Append("(");
			sb.Append(Left);
			sb.Append(", ");
			sb.Append(Right);
			sb.Append(") ");
			sb.Append(Body.ToString(indent));
			return sb.ToString();
		}

		public override string GetMethodName()
		{
			switch (Operator)
			{
				case BinaryOperator.Equality:       return "op:Equals";
				case BinaryOperator.Hash:           return "op:Hash";
				case BinaryOperator.Dollar:         return "op:Dollar";
				case BinaryOperator.ShiftLeft:      return "op:ShiftLeft";
				case BinaryOperator.ShiftRight:     return "op:ShiftRight";
				case BinaryOperator.Addition:       return "op:Add";
				case BinaryOperator.Subtraction:    return "op:Subtract";
				case BinaryOperator.BitwiseOr:      return "op:Or";
				case BinaryOperator.BitwiseXor:     return "op:Xor";
				case BinaryOperator.Multiplication: return "op:Multiply";
				case BinaryOperator.Division:       return "op:Divide";
				case BinaryOperator.Modulo:         return "op:Modulo";
				case BinaryOperator.BitwiseAnd:     return "op:And";
				case BinaryOperator.Exponentiation: return "op:Power";
				case BinaryOperator.Comparison:     return "op:Compare";
				default:
					throw new InvalidOperationException("BinaryOperatorOverload with invalid operator.");
			}
		}

		public override int GetIndex()
		{
			switch (Operator)
			{
				case BinaryOperator.Addition:       return 0;
				case BinaryOperator.Subtraction:    return 1;
				case BinaryOperator.BitwiseOr:      return 2;
				case BinaryOperator.BitwiseXor:     return 3;
				case BinaryOperator.Multiplication: return 4;
				case BinaryOperator.Division:       return 5;
				case BinaryOperator.Modulo:         return 6;
				case BinaryOperator.BitwiseAnd:     return 7;
				case BinaryOperator.Exponentiation: return 8;
				case BinaryOperator.ShiftLeft:      return 9;
				case BinaryOperator.ShiftRight:     return 10;
				case BinaryOperator.Hash:           return 11;
				case BinaryOperator.Dollar:         return 12;
				case BinaryOperator.Equality:       return 16;
				case BinaryOperator.Comparison:     return 17;
				default:
					throw new InvalidOperationException("BinaryOperatorOverload with invalid operator.");
			}
		}
	}

	public sealed class IteratorDeclaration : MemberDeclaration
	{
		public IteratorDeclaration(Block body) : base("iter", AccessLevel.None)
		{
			Body = body;
		}

		/// <summary>The body of the iterator.</summary>
		public Block Body;

		internal Method DeclSpace { get { return Body.DeclSpace.Method; } }

		public override string ToString(int indent)
		{
			return new string('\t', indent) + "iter " + Body.ToString(indent);
		}

		public override void FoldConstant()
		{
			Body.FoldConstant();
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, Compiler compiler)
		{
			Body.ResolveNames(context, document, true);
		}
	}
}