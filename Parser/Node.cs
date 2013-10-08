using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using Osprey.Members;
using CI = System.Globalization.CultureInfo;
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

		public int StartIndex = 0;
		public int EndIndex = 0;

		public abstract string ToString(int indent);
	}

	#region Namespaces

	public class Document : ParseNode
	{
		public Document() { }

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

		/// <summary>The name of the file from which the document was created.</summary>
		public string FileName;

		/// <summary>The source code of the file from which the document was created.</summary>
		internal string FileSource;

		/// <summary>The compiler instance that opened the file.</summary>
		internal Compiler Compiler;

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
	{ }

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

	public sealed class UseScriptDirective : UseDirective
	{
		public UseScriptDirective(StringLiteral name)
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
		}

		/// <summary>Indicates whether the function has the modifier "public".</summary>
		public bool IsPublic;
		/// <summary>The function that was declared.</summary>
		public LocalFunctionDeclaration Function;

		public string DocString;

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

		public string DocString;

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

		public string DocString = null;

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

		public override string ToString(int indent)
		{
			return new string('\t', indent) + (Value == null ? Name : string.Format("{0} = {1}", Name, Value.ToString(indent)));
		}

		public override void FoldConstant()
		{
			if (Value == null)
				return;

			Value = Value.FoldConstant();
			if (!(Value is ConstantExpression) ||
				((ConstantExpression)Value).Value.Type != ConstantValueType.Int)
				throw new CompileTimeException(Value, "The value of an enum member must be a constant expression of type Int.");
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
					compiler.AddLocalExtractor(() => _decl.Initializer = _decl.Initializer.TransformClosureLocals(null, false));
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
		/// <summary>The base initializer. If there is one, this value is also the first statement in <see cref="Body"/>.</summary>
		public BaseInitializer BaseInitializer;

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

				if (!(Body is ExternBody) && BaseInitializer == null &&
					@class.BaseType != null)
				{
					BaseInitializer = new BaseInitializer(new List<Expression>());
					Body.Statements.Insert(0, BaseInitializer);
				}

				foreach (var param in Parameters)
				{
					param.ResolveNames(context, document);
					if (param.HasThisPrefix)
					{
						if (Body.Initializer == null)
							Body.Initializer = new List<AssignmentExpression>();

						Body.Initializer.Add(new AssignmentExpression(
							new InstanceMemberAccess(new ThisAccess(), @class, param.Member)
							{
								IsAssignment = true
							},
							new LocalVariableAccess(
								(Variable)Body.DeclSpace.members[param.DeclaredName],
								LocalAccessKind.NonCapturing
							)
						) { IgnoreValue = true });
					}
				}
			}

			Body.ResolveNames(context, document);
		}

		internal void AddFieldInitializers(IEnumerable<VariableDeclarator> fields, Class @class)
		{
			if (Body is ExternBody)
				return; // Do nothing

			if (Body.Initializer == null)
				Body.Initializer = new List<AssignmentExpression>();

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

					var inner = new InstanceMemberAccess(new ThisAccess(), @class, f);
					inner.IsAssignment = true;
					Body.Initializer.Insert(index++, new AssignmentExpression(inner, field.Initializer)
					{
						IgnoreValue = true
					});
				}
			}
			else
			{
				foreach (var field in fields)
				{
					var inner = new StaticFieldAccess((Field)@class.GetMember(field.Name));
					inner.IsAssignment = true;
					Body.Initializer.Insert(index++, new AssignmentExpression(inner, field.Initializer)
					{
						IgnoreValue = true
					});
				}
			}
		}
	}

	public sealed class ConstructorParam : Parameter
	{
		public ConstructorParam(string name, bool hasThisPrefix, Expression defaultValue)
			: base(name, defaultValue)
		{
			HasThisPrefix = hasThisPrefix;
		}

		/// <summary>Indicates whether the parameter has the prefix 'this.', for auto-assign parameters.</summary>
		public bool HasThisPrefix;

		/// <summary>If <see cref="HasThisPrefix"/> is true, this member contains the member that the parameter assigns to.</summary>
		internal ClassMember Member;

		public override string DeclaredName { get { return HasThisPrefix ? "<this>" + Name : Name; } }

		public override string ToString(int indent)
		{
			return (HasThisPrefix ? "this." : "") + base.ToString(indent);
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

				var member = @class.GetMember(this.Name, instType: @class, fromType: @class);
				if (member == null)
					throw new CompileTimeException(this, string.Format("The type '{0}' does not contain a definition for '{1}'.",
						@class.FullName, this.Name));

				if (member.Kind != MemberKind.Field &&
					(member.Kind != MemberKind.Property || ((Property)member).PropertyKind == PropertyKind.ReadOnly))
					throw new CompileTimeException(this, string.Format("The member '{0}.{1}' cannot be assigned to.",
						@class.FullName, this.Name));

				// At this point, we know it's a field or a property, and those are both derived from ClassMember.
				if (((ClassMember)member).IsStatic)
					throw new CompileTimeException(this, string.Format("The member '{0}.{1}' is static and cannot be referred to in a 'this' parameter.",
						@class.FullName, this.Name));

				this.Member = (ClassMember)member;
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

			Body.ResolveNames(context, document);
		}
	}

	public class Parameter : ParseNode
	{
		public Parameter(string name, Expression defaultValue)
		{
			Name = name;
			DefaultValue = defaultValue;
		}
		/// <summary>The name of the parameter.</summary>
		public string Name;
		/// <summary>The default value of the parameter, or null if the parameter is required.</summary>
		public Expression DefaultValue = null;

		/// <summary>Gets the name that the parameter is declared with.</summary>
		public virtual string DeclaredName { get { return Name; } }

		public override string ToString(int indent)
		{
			if (DefaultValue != null)
				return Name + " = " + DefaultValue.ToString(indent + 1);
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

		internal Method DeclSpace { get { return ((Block)Body).DeclSpace.Method; } }

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
			Body.ResolveNames(context, document);
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
	 * TYPED_ENUM(Operator, uint8_t)
	 * {
	 *     OP_ADD,    // The binary + operator.  ( 0)
	 *     OP_SUB,    // The binary - operator.  ( 1)
	 *     OP_OR,     // The | operator.	     ( 2)
	 *     OP_XOR,    // The ^ operator.	     ( 3)
	 *     OP_MUL,    // The * operator.	     ( 4)
	 *     OP_DIV,    // The / operator.	     ( 5)
	 *     OP_MOD,    // The % operator.	     ( 6)
	 *     OP_AND,    // The & operator.	     ( 7)
	 *     OP_POW,    // The ** operator.	     ( 8)
	 *     OP_SHL,    // The << operator.	     ( 9)
	 *     OP_SHR,    // The >> operator.	     (10)
	 *     OP_HASHOP, // The # operator.	     (11)
	 *     OP_DOLLAR, // The $ operator.	     (12)
	 *     OP_PLUS,   // The unary + operator.   (13)
	 *     OP_NEG,    // The unary - operator.   (14)
	 *     OP_NOT,    // The ~ operator.	     (15)
	 *     OP_EQ,     // The == operator.	     (16)
	 *     OP_CMP,    // The <=> operator.	     (17)
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
			Body.ResolveNames(context, document);
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

		public Method DeclSpace { get { return Body.DeclSpace.Method; } }

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
			Body.ResolveNames(context, document);
		}
	}
}