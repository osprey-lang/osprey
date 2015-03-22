using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Nodes;

namespace Osprey
{
	public sealed class Parser
	{
		private Parser(SourceFile file, ParseFlags flags)
		{
			var tokFlags = (flags & ParseFlags.NormalizeIdentifiers) == ParseFlags.NormalizeIdentifiers ?
				TokenizerFlags.NormalizeIdentifiers : TokenizerFlags.None;
			this.tok = new Tokenizer(file, tokFlags);
			this.flags = flags;
		}
		private Parser(string source, ParseFlags flags)
		{
			var tokFlags = (flags & ParseFlags.NormalizeIdentifiers) == ParseFlags.NormalizeIdentifiers ?
				TokenizerFlags.NormalizeIdentifiers : TokenizerFlags.None;
			this.tok = new Tokenizer(source, tokFlags);
			this.flags = flags;
		}

		private Tokenizer tok;
		private Document document;
		private ParseFlags flags;

		public Tokenizer Tok { get { return tok; } }
		public Document Document { get { return document; } }

		/// <summary>
		/// If true, the parser uses compiler-specific extensions.
		/// Otherwise, they're completely ignored.
		/// </summary>
		public bool UseExtensions
		{
			get { return (flags & ParseFlags.UseExtensions) != ParseFlags.None; }
		}

		public bool SimplifiedTree
		{
			get { return (flags & ParseFlags.SimplifiedTree) != ParseFlags.None; }
		}

		#region "Accept" helpers

		/// <summary>
		/// Determines whether the token at the specified index is of a certain type.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="type">The token type to test against.</param>
		/// <returns>true if the token at the specified index matches <paramref name="type"/>; otherwise, false.</returns>
		public bool Accept(int index, TokenType type)
		{
			return tok[index].Match(type);
		}
		/// <summary>
		/// Determines whether the token at the specified index is of a certain type.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="t1">The first type to test against.</param>
		/// <param name="t2">The second type to test against</param>
		/// <returns>true if the token at the specified index matches either <see cref="TokenType"/> argument; otherwise, false.</returns>
		public bool Accept(int index, TokenType t1, TokenType t2)
		{
			return tok[index].Match(t1) || tok[index].Match(t2);
		}
		/// <summary>
		/// Determines whether the token at the specified index is of a certain type.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="t1">The first type to test against.</param>
		/// <param name="t2">The second type to test against</param>
		/// <param name="t3">The third type to test against.</param>
		/// <returns>true if the token at the specified index matches any of the <see cref="TokenType"/> arguments; otherwise, false.</returns>
		public bool Accept(int index, TokenType t1, TokenType t2, TokenType t3)
		{
			return tok[index].Match(t1) || tok[index].Match(t2) || tok[index].Match(t3);
		}
		/// <summary>
		/// Determines whether the token at the specified index is of a certain type.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="t1">The first type to test against.</param>
		/// <param name="t2">The second type to test against</param>
		/// <param name="t3">The third type to test against.</param>
		/// <param name="t4">The fourth type to test against.</param>
		/// <returns>true if the token at the specified index matches any of the <see cref="TokenType"/> arguments; otherwise, false.</returns>
		public bool Accept(int index, TokenType t1, TokenType t2, TokenType t3, TokenType t4)
		{
			return tok[index].Match(t1) || tok[index].Match(t2) || tok[index].Match(t3) || tok[index].Match(t4);
		}
		/// <summary>
		/// Determines whether the token at the specified index is of a certain type.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="types">An array of types to test against.</param>
		/// <returns>true if the token at the specified index matches one of the types in <paramref name="types"/>; otherwise, false.</returns>
		public bool Accept(int index, params TokenType[] types)
		{
			for (var i = 0; i < types.Length; i++)
				if (tok[index].Match(types[i]))
					return true;
			return false;
		}

		/// <summary>
		/// Determines whether the token at the specified index is of a certain type, and increments <paramref name="index"/> if it is.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="type">The token type to test against.</param>
		/// <returns>true if the token at the specified index matches <paramref name="type"/>; otherwise, false.</returns>
		public bool Accept(ref int index, TokenType type)
		{
			if (Accept(index, type))
			{
				index++;
				return true;
			}
			return false;
		}
		/// <summary>
		/// Determines whether the token at the specified index is of a certain type, and increments <paramref name="index"/> if it is.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="t1">The first type to test against.</param>
		/// <param name="t2">The second type to test against</param>
		/// <returns>true if the token at the specified index matches any of the <see cref="TokenType"/> arguments; otherwise, false.</returns>
		public bool Accept(ref int index, TokenType t1, TokenType t2)
		{
			if (Accept(index, t1, t2))
			{
				index++;
				return true;
			}
			return false;
		}
		/// <summary>
		/// Determines whether the token at the specified index is of a certain type, and increments <paramref name="index"/> if it is.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="t1">The first type to test against.</param>
		/// <param name="t2">The second type to test against</param>
		/// <param name="t3">The third type to test against.</param>
		/// <returns>true if the token at the specified index matches any of the <see cref="TokenType"/> arguments; otherwise, false.</returns>
		public bool Accept(ref int index, TokenType t1, TokenType t2, TokenType t3)
		{
			if (Accept(index, t1, t2, t3))
			{
				index++;
				return true;
			}
			return false;
		}
		/// <summary>
		/// Determines whether the token at the specified index is of a certain type, and increments <paramref name="index"/> if it is.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="t1">The first type to test against.</param>
		/// <param name="t2">The second type to test against</param>
		/// <param name="t3">The third type to test against.</param>
		/// <param name="t4">The fourth type to test against.</param>
		/// <returns>true if the token at the specified index matches any of the <see cref="TokenType"/> arguments; otherwise, false.</returns>
		public bool Accept(ref int index, TokenType t1, TokenType t2, TokenType t3, TokenType t4)
		{
			if (Accept(index, t1, t2, t3, t4))
			{
				index++;
				return true;
			}
			return false;
		}
		/// <summary>
		/// Determines whether the token at the specified index is of a certain type, and increments <paramref name="index"/> if it is.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="types">An array of types to test against.</param>
		/// <returns>true if the token at the specified index matches one of the types in <paramref name="types"/>; otherwise, false.</returns>
		public bool Accept(ref int index, params TokenType[] types)
		{
			for (var i = 0; i < types.Length; i++)
				if (tok[index].Match(types[i]))
				{
					index++;
					return true;
				}
			return false;
		}

		public bool AcceptContextual(int index, string keyword)
		{
			var token = tok[index] as Identifier;
			if (token != null && !token.Escaped && token.Value == keyword)
				return true;
			return false;
		}

		public bool AcceptContextual(ref int index, string keyword)
		{
			if (AcceptContextual(index, keyword))
			{
				index++;
				return true;
			}
			return false;
		}

		public bool AcceptExtension(int index, string keyword)
		{
			return UseExtensions && AcceptContextual(index, keyword);
		}

		public bool AcceptExtension(ref int index, string keyword)
		{
			return UseExtensions && AcceptContextual(ref index, keyword);
		}

		#endregion

		#region "Expect" helpers

		/// <summary>
		/// Tests whether the token at the specified index is of the specified type. Otherwise, throws a <see cref="ParseException"/>.
		/// </summary>
		/// <param name="index">The index of the token to test.</param>
		/// <param name="type">The type to test against.</param>
		/// <returns>The token at the specified index.</returns>
		public Token Expect(int index, TokenType type)
		{
			if (!Accept(index, type))
				throw new ParseException(tok[index], "Expected " + FormatTokenType(type) + "; got " + tok[index].ToString() + ".");
			return tok[index];
		}
		/// <summary>
		/// Tests whether the token at the specified index is of the specified type. Otherwise, throws a <see cref="ParseException"/> with a custom message.
		/// </summary>
		/// <param name="index">The index of the token to test.</param>
		/// <param name="type">The type to test against.</param>
		/// <param name="message">The exception message to display in case of an exception.</param>
		/// <returns>The token at the specified index.</returns>
		public Token Expect(int index, TokenType type, string message)
		{
			if (!Accept(index, type))
				throw new ParseException(tok[index], message);
			return tok[index];
		}

		/// <summary>
		/// Tests whether the token at the specified index is of the specified type and, if so, increases <paramref name="index"/> by 1. Otherwise, throws a <see cref="ParseException"/>.
		/// </summary>
		/// <param name="index">The index of the token to test.</param>
		/// <param name="type">The type to test against.</param>
		/// <returns>The token at the specified index (prior to incrementing).</returns>
		public Token Expect(ref int index, TokenType type)
		{
			if (!Accept(index, type))
				throw new ParseException(tok[index], "Expected " + FormatTokenType(type) + "; got " + tok[index].ToString() + ".");
			return tok[index++];
		}
		/// <summary>
		/// Tests whether the token at the specified index is of the specified type and, if so, increases <paramref name="index"/> by 1. Otherwise, throws a <see cref="ParseException"/> with a custom message.
		/// </summary>
		/// <param name="index">The index of the token to test.</param>
		/// <param name="type">The type to test against.</param>
		/// <param name="message">The exception message to display in case of an exception.</param>
		/// <returns>The token at the specified index (prior to incrementing).</returns>
		public Token Expect(ref int index, TokenType type, string message)
		{
			if (!Accept(index, type))
				throw new ParseException(tok[index], message);
			return tok[index++];
		}

		internal string FormatTokenType(TokenType type)
		{
			if ((type & TokenType.Punctuation) == TokenType.Punctuation)
				return string.Format("'{0}' ({1})", Tokenizer.PunctToString[type],
					System.Enum.GetName(typeof(TokenType), type));
			else if (type == TokenType.Identifier)
				return "identifier";
			else if ((type & TokenType.Keyword) == TokenType.Keyword)
				return string.Format("keyword '{0}'", Tokenizer.KeywordToString[type]);
			else if (type == TokenType.EOF)
				return "end of file";
			else
				return System.Enum.GetName(typeof(TokenType), type) ?? "unknown token";
		}

		#endregion

		#region Parse error helpers

		internal void ParseError(int tokenIndex, string message)
		{
			ParseError(tok[tokenIndex], message);
		}
		internal void ParseError(int tokenIndex, string format, object arg0)
		{
			ParseError(tok[tokenIndex], format, arg0);
		}
		internal void ParseError(int tokenIndex, string format, object arg0, object arg1)
		{
			ParseError(tok[tokenIndex], format, arg0, arg1);
		}
		internal void ParseError(int tokenIndex, string format, object arg0, object arg1, object arg2)
		{
			ParseError(tok[tokenIndex], format, arg0, arg1, arg2);
		}
		internal void ParseError(int tokenIndex, string format, params object[] args)
		{
			ParseError(tok[tokenIndex], format, args);
		}

		internal void ParseError(Token token, string message)
		{
			throw new ParseException(token, message);
		}
		internal void ParseError(Token token, string format, object arg0)
		{
			throw new ParseException(token, string.Format(format, arg0));
		}
		internal void ParseError(Token token, string format, object arg0, object arg1)
		{
			throw new ParseException(token, string.Format(format, arg0, arg1));
		}
		internal void ParseError(Token token, string format, object arg0, object arg1, object arg2)
		{
			throw new ParseException(token, string.Format(format, arg0, arg1, arg2));
		}
		internal void ParseError(Token token, string format, params object[] args)
		{
			throw new ParseException(token, string.Format(format, args));
		}

		internal void ParseError(ParseNode node, string message)
		{
			throw new ParseException(node, message);
		}
		internal void ParseError(ParseNode node, string format, object arg0)
		{
			throw new ParseException(node, string.Format(format, arg0));
		}
		internal void ParseError(ParseNode node, string format, object arg0, object arg1)
		{
			throw new ParseException(node, string.Format(format, arg0, arg1));
		}
		internal void ParseError(ParseNode node, string format, object arg0, object arg1, object arg2)
		{
			throw new ParseException(node, string.Format(format, arg0, arg1, arg2));
		}
		internal void ParseError(ParseNode node, string format, params object[] args)
		{
			throw new ParseException(node, string.Format(format, args));
		}

		#endregion

		private AssignableExpression EnsureAssignable(Expression expr)
		{
			if (expr is ThisAccess)
				ParseError(expr, "Cannot assign to 'this'.");
			if (expr is BaseAccess)
				ParseError(expr, "Cannot assign to 'base'.");

			var assignableExpr = expr as AssignableExpression;
			if (assignableExpr == null)
				ParseError(expr, "Can only assign to variables, members and indexers.");

			return assignableExpr;
		}

		/// <summary>
		/// Gets a binary operator expression, or a <see cref="ConstantExpression"/> if <see cref="SimplifiedTree"/> is true
		/// and the operands are both constant expressions, and they support the operator.
		/// </summary>
		/// <param name="left">The left operand.</param>
		/// <param name="right">The right operand.</param>
		/// <param name="op">The operator that combines the operands.</param>
		/// <returns>If <see cref="SimplifiedTree"/> is true and <paramref name="left"/> and <paramref name="right"/> are both
		/// <see cref="ConstantExpression"/>s and the operator <paramref name="op"/> is supported on the operands, then this
		/// method returns a new <see cref="ConstantExpression"/> that represents the result of evaluating the operation.
		/// Otherwise, it returns a new <see cref="BinaryOperatorExpression"/>.</returns>
		private Expression GetBinaryOperatorExpression(Expression left, Expression right, BinaryOperator op)
		{
			if (SimplifiedTree)
			{
				var leftInner = left;
				var rightInner = right;
				while (leftInner is ParenthesizedExpression)
					leftInner = ((ParenthesizedExpression)leftInner).Inner;

				while (rightInner is ParenthesizedExpression)
					rightInner = ((ParenthesizedExpression)rightInner).Inner;

				if (leftInner is ConstantExpression && rightInner is ConstantExpression)
				{
					ConstantValue leftVal = ((ConstantExpression)leftInner).Value;
					ConstantValue rightVal = ((ConstantExpression)rightInner).Value;

					if (leftVal.SupportsOperator(op, rightVal))
					{
						ConstantValue result;
						try
						{
							result = leftVal.ExecuteOperator(op, rightVal);
						}
						catch (DivideByZeroException e)
						{
							throw new ParseException(tok.GetErrorToken(left.StartIndex, right.EndIndex - left.StartIndex),
								"The expression cannot be evaluated because it involves dividing by zero.",
								e);
						}
						catch (OverflowException e)
						{
							throw new ParseException(tok.GetErrorToken(left.StartIndex, right.EndIndex - left.StartIndex),
								"The expression cannot be evaluated because it results in an arithmetic overflow.",
								e);
						}

						return new ConstantExpression(result)
							.At(left.StartIndex, right.EndIndex, document);
					}
				}
				// [a, b] :: [c, d] => [a, b, c, d]
				if (op == BinaryOperator.Concatenation)
				{
					ListLiteralExpression leftList = leftInner as ListLiteralExpression,
						rightList = rightInner as ListLiteralExpression;
					if (leftList != null && rightList != null)
					{
						var resultValues = new Expression[leftList.Values.Length + rightList.Values.Length];
						leftList.Values.CopyTo(resultValues, 0);
						rightList.Values.CopyTo(resultValues, leftList.Values.Length);
						var result = (ListLiteralExpression)
							new ListLiteralExpression(resultValues)
							.At(left.StartIndex, right.EndIndex, document);
						return result;
					}

					return new ConcatenationExpression(left, right)
						.At(left.StartIndex, right.EndIndex, document);
				}

				// a != b   becomes   not (a == b)
				if (op == BinaryOperator.Inequality)
				{
					var inner = new BinaryOperatorExpression(left, right, BinaryOperator.Equality)
						.At(left.StartIndex, right.EndIndex, document);
					return new UnaryExpression(inner, UnaryOperator.Not)
						.At(inner.StartIndex, inner.EndIndex, document);
				}
			}

			return new BinaryOperatorExpression(left, right, op)
				.At(left.StartIndex, right.EndIndex, document);
		}

		/// <summary>
		/// Gets a unary operator expression, or a <see cref="ConstantExpression"/> if <see cref="SimplifiedTree"/> is true
		/// and the operand is a constant expression that supports the operator.
		/// </summary>
		/// <param name="operand">The operand to the operator.</param>
		/// <param name="op">The operator that is applied to the operand.</param>
		/// <returns>If <see cref="SimplifiedTree"/> is true and <paramref name="operand"/> is a <see cref="ConstantExpression"/>
		/// and it supports the specified operator, then this method returns a new <see cref="ConstantExpression"/> that represents
		/// the result of evaluating the operation. Otherwise, a new <see cref="UnaryOperatorExpression"/> is returned.</returns>
		private Expression GetUnaryOperatorExpression(Expression operand, UnaryOperator op, int start)
		{
			if (SimplifiedTree)
			{
				var operandInner = operand;
				while (operandInner is ParenthesizedExpression)
					operandInner = ((ParenthesizedExpression)operandInner).Inner;

				if (operandInner is ConstantExpression)
				{
					ConstantValue operandValue = ((ConstantExpression)operandInner).Value;

					if (operandValue.SupportsOperator(op))
					{
						ConstantValue result;
						try
						{
							result = operandValue.ExecuteOperator(op);
						}
						catch (OverflowException e)
						{
							throw new ParseException(tok.GetErrorToken(start, operand.EndIndex - start),
								"The expression cannot be evaluated because it results in an arithmetic overflow.",
								e);
						}

						return new ConstantExpression(result).At(start, operand.EndIndex, document);
					}
				}
			}

			return new UnaryExpression(operand, op).At(start, operand.EndIndex, document);
		}

		#region Declarations

		private Document ParseDocument()
		{
			document = new Document(tok.File);

			var i = 0;

			if (AcceptContextual(i, "version") &&
				Accept(i + 1, TokenType.Integer))
				document.Version = ParseVersion(ref i);

			// parse all use directives first
			while (Accept(ref i, TokenType.Use))
				document.Uses.Add(ParseUseDirective(ref i));

			var fileNs = document.GlobalDeclarationSpace;

			if (Accept(ref i, TokenType.Namespace))
			{
				var name = ParseQualifiedName(ref i);
				if (Accept(ref i, TokenType.Semicolon)) // file namespace
				{
					fileNs.Name = name;
				}
				else if (Accept(ref i, TokenType.CurlyOpen))
				{
					var ns = new NamespaceDeclaration(name) { Document = document };
					ParseNamespaceMembers(ref i, ns, null);
					Expect(ref i, TokenType.CurlyClose);

					fileNs.Namespaces.Add(ns);
				}
				else
					ParseError(i, "Expected ';' or namespace body.");
			}

			var statements = new List<Statement>();
			while (!Accept(i, TokenType.EOF, TokenType.CurlyClose))
				ParseNamespaceMembers(ref i, fileNs, statements);

			document.Statements = statements.ToArray();

			Expect(i, TokenType.EOF);

			return document;
		}

		private Version ParseVersion(ref int i)
		{
			Expect(ref i, TokenType.Identifier); // "version"

			int major = 0, minor = 0, build = 0, revision = 0;

			// Keep in mind: integer literals cannot be negative!

			Func<Parser, Token, int> tokenToField = (_this, tok) =>
			{
				var value = IntegerLiteral.ParseToken(tok);
				if (value.Type != ConstantValueType.Int)
					_this.ParseError(tok, "Version number field must be of type Int.");
				if (value.IntValue > int.MaxValue)
					_this.ParseError(tok, "Version number field out of range.");

				return unchecked((int)value.IntValue);
			};

			major = tokenToField(this, Expect(ref i, TokenType.Integer));

			Expect(ref i, TokenType.Colon);

			minor = tokenToField(this, Expect(ref i, TokenType.Integer));

			if (Accept(ref i, TokenType.Colon))
				build = tokenToField(this, Expect(ref i, TokenType.Integer));

			if (Accept(ref i, TokenType.Colon))
				revision = tokenToField(this, Expect(ref i, TokenType.Integer));

			Expect(ref i, TokenType.Semicolon);

			return new Version(major, minor, build, revision);
		}

		private UseDirective ParseUseDirective(ref int i)
		{
			int start = tok[i - 1].Index, end = tok[i - 1].EndIndex;

			UseDirective result = null;
			if (Accept(i, TokenType.String)) // use "<file name>";
			{
				var token = tok[i];
				i++;
				Expect(ref i, TokenType.Semicolon);
				result = new UseFileDirective(new StringLiteral((StringToken)token));
			}
			else if (Accept(ref i, TokenType.Namespace)) // use namespace foo.bar.baz;
			{
				var name = ParseQualifiedName(ref i);
				Expect(ref i, TokenType.Semicolon);
				result = new UseNamespaceDirective(name);
			}
			else if (Accept(i, TokenType.Identifier)) // use foo.bar.baz;  or  use alias = foo.bar.baz;
			{
				string aliasName = null;
				if (Accept(i + 1, TokenType.Assign))
				{
					// use alias = foo.bar.baz;
					aliasName = tok[i++].Value;
					i++; // skip =
				}
				var name = ParseQualifiedName(ref i);
				Expect(ref i, TokenType.Semicolon);
				if (aliasName != null)
					result = new UseAliasDirective(aliasName, name);
				else
					result = new UseModuleDirective(name);
			}
			else
				ParseError(i, "Expected identifier, string or 'namespace'.");

			result.StartIndex = start;
			result.EndIndex = end;
			result.Document = document;
			return result;
		}

		private MemberModifiers ParseModifiers(ref int i)
		{
			MemberModifiers output = new MemberModifiers();

			while (Accept(i, TokenType.MemberModifier))
			{
				if (Accept(i, TokenType.Public, TokenType.Protected, TokenType.Private))
				{
					if (output.Access != AccessLevel.None)
						ParseError(i, "More than one access level modifier.");
					output.Access = tok[i].Type == TokenType.Public ? AccessLevel.Public :
						tok[i].Type == TokenType.Protected ? AccessLevel.Protected :
						AccessLevel.Private;
				}
				if (Accept(i, TokenType.Static))
				{
					if (output.IsStatic)
						ParseError(i, "Duplicate static modifier.");
					output.IsStatic = true;
				}
				if (Accept(i, TokenType.Abstract))
				{
					if (output.IsAbstract)
						ParseError(i, "Duplicate abstract modifier.");
					output.IsAbstract = true;
				}
				if (Accept(i, TokenType.Inheritable))
				{
					if (output.IsInheritable)
						ParseError(i, "Duplicate inheritable modifier.");
					output.IsInheritable = true;
				}
				if (Accept(i, TokenType.Overridable))
				{
					if (output.IsOverridable)
						ParseError(i, "Duplicate overridable modifier.");
					output.IsOverridable = true;
				}
				if (Accept(i, TokenType.Override))
				{
					if (output.IsOverride)
						ParseError(i, "Duplicate override modifier.");
					output.IsOverride = true;
				}
				i++;
			}

			return output;
		}

		private void ParseNamespaceMembers(ref int i, NamespaceDeclaration target, List<Statement> stmtList)
		{
			while (!Accept(i, TokenType.CurlyClose, TokenType.EOF))
			{
				var startTok = tok[i];
				var modifiers = ParseModifiers(ref i);
				if (Accept(i, TokenType.Class) || i > 0 && tok[i - 1].Match(TokenType.Inheritable)) // class declaration
				{
					modifiers.ValidateForClass(tok[i]);
					var @class = ParseClass(ref i, ref modifiers);
					@class.DocString = startTok.Documentation;
					target.Types.Add(@class);
				}
				else if (Accept(i, TokenType.Enum)) // enum declaration
				{
					modifiers.ValidateForEnum(tok[i]);
					var @enum = ParseEnum(ref i, ref modifiers);
					@enum.DocString = startTok.Documentation;
					target.Types.Add(@enum);
				}
				else if (Accept(i, TokenType.Function)) // global function
				{
					modifiers.ValidateForGlobal(tok[i]);
					var func = ParseLocalFunctionDeclaration(ref i, isLocal: false);
					var globalFunc = new GlobalFunctionDeclaration(modifiers.Access == AccessLevel.Public, func);
					globalFunc.DocString = startTok.Documentation;
					target.Functions.Add(globalFunc);
				}
				else if (Accept(i, TokenType.Const)) // global constant
				{
					modifiers.ValidateForGlobal(tok[i]);

					// This makes sure all the names have values.
					// It also makes sure the declaration is not "parallel", e.g.
					//     public const (a, b) = someValue;
					// which is not constant.
					var decl = ParseLocalVariableDeclaration(ref i);

					var simpleDecl = decl as SimpleLocalVariableDeclaration;
#if DEBUG
					if (simpleDecl == null || !simpleDecl.IsConst)
						throw new ParseException(decl,
							"Internal error: global constant declaration should be a simple declaration and marked IsConst.");
#endif

					var constDecl = new GlobalConstantDeclaration(modifiers.Access == AccessLevel.Public, simpleDecl)
					{
						Document = document
					};
					if (simpleDecl.Declarators.Length == 1)
						constDecl.DocString = startTok.Documentation;

					target.Constants.Add(constDecl);
				}
				else if (Accept(i, TokenType.Namespace)) // nomspace
				{
					if (!modifiers.IsEmpty)
						ParseError(i, "Namespaces do not accept any modifiers.");

					i++;
					var name = ParseQualifiedName(ref i);

					var ns = new NamespaceDeclaration(name) { Document = document };
					if (Accept(i, TokenType.Semicolon))
						ParseError(i,
							"The file namespace declaration is only allowed at the beginning of the file, after any use declarations.");
					else if (!Accept(i, TokenType.CurlyOpen))
						ParseError(i, "Expected namespace body.");

					Expect(ref i, TokenType.CurlyOpen);

					ParseNamespaceMembers(ref i, ns, null);

					Expect(ref i, TokenType.CurlyClose);

					target.Namespaces.Add(ns);
				}
				else if (!modifiers.IsEmpty)
					ParseError(i, "Expected class, function or constant declaration.");
				else if (stmtList == null)
					ParseError(i, "Expected class, function, constant or namespace declaration.");
				else
				{
					var statement = ParseStatement(ref i);
					var localVarDecl = statement as LocalVariableDeclaration;
					if (localVarDecl != null)
						localVarDecl.IsGlobal = true;
					stmtList.Add(statement);
				}
			}
		}

		private ClassDeclaration ParseClass(ref int i, ref MemberModifiers modifiers)
		{
			Accept(ref i, TokenType.Class); // if modifiers.IsInheritable, class is optional

			var name = Expect(ref i, TokenType.Identifier); // nom
			var output = new ClassDeclaration(name.Value, modifiers.Access)
			{
				StartIndex = name.Index,
				EndIndex = name.EndIndex,
				Document = document,
				IsAbstract = modifiers.IsAbstract,
				IsInheritable = modifiers.IsInheritable,
				IsStatic = modifiers.IsStatic,
			};

			if (Accept(ref i, TokenType.Is)) // base class thing
				output.BaseClass = ParseTypeName(ref i);
			else if (AcceptExtension(ref i, "__primitive"))
			{
				output.IsPrimitive = true;
				if (output.IsInheritable || output.IsAbstract || output.IsStatic)
					ParseError(name, "Primitive classes cannot be marked inheritable, abstract or static.");
			}

			Expect(ref i, TokenType.CurlyOpen);

			if (AcceptExtension(ref i, "__init_type"))
			{
				// Type initializer! Must be the very first thing in the class.
				Expect(ref i, TokenType.ParenOpen);

				var initerToken = Expect(ref i, TokenType.String);
				output.Initializer = ((StringToken)initerToken).LiteralValue;

				Expect(ref i, TokenType.ParenClose);
				Expect(ref i, TokenType.Semicolon);
			}

			ParseClassMembers(ref i, output);

			Expect(ref i, TokenType.CurlyClose);

			return output;
		}

		private void ParseClassMembers(ref int i, ClassDeclaration target)
		{
			while (!Accept(i, TokenType.CurlyClose, TokenType.EOF))
			{
				var startTok = tok[i];
				var modifiers = ParseModifiers(ref i);
				if (Accept(i, TokenType.New))
				{
					ParseConstructorDeclaration(ref i, target, startTok, ref modifiers);
				}
				else if (Accept(i, TokenType.Const, TokenType.Var)) // field (maybe even a constant one)
				{
					var token = tok[i++];
					modifiers.ValidateForField(token);

					var isConst = token.Type == TokenType.Const;
					if (isConst && modifiers.IsStatic)
						ParseError(token, "A constant cannot be marked static.");
					// Note: ParseFieldDeclaration ignores the var/const keyword.
					var field = ParseFieldDeclaration(ref i, isConst, modifiers.Access, modifiers.IsStatic);
					field.StartIndex = token.Index;
					field.EndIndex = token.EndIndex;
					field.DocString = startTok.Documentation;

					if (isConst)
						target.Constants.Add(field);
					else
						target.Fields.Add(field);
				}
				else if (Accept(i, TokenType.Function)) // method
				{
					i++;
					var method = ParseMethod(ref i, ref modifiers);
					method.DocString = startTok.Documentation;
					target.Methods.Add(method);
				}
				else if (Accept(i, TokenType.Get, TokenType.Set)) // property accessor
				{
					var isSetter = tok[i++].Type == TokenType.Set;

					PropertyAccessorDeclaration decl;
					if (Accept(i, TokenType.This))
						decl = ParseIndexer(ref i, isSetter, ref modifiers);
					else
						decl = ParseProperty(ref i, isSetter, ref modifiers);

					decl.DocString = startTok.Documentation;
					target.Properties.Add(decl);
				}
				else if (Accept(i, TokenType.Iter)) // iter declaration
				{
					if (!modifiers.IsEmpty)
						ParseError(i, "Iterator declarations cannot have any modifiers.");
					if (target.Iterator != null)
						ParseError(i, "A class can only declare one iterator.");
					if (Accept(i + 1, TokenType.ParenOpen))
						ParseError(i + 1, "Iterators do not take any parameters.");

					int start = tok[i].Index, end = tok[i].EndIndex;
					i++;
					target.Iterator = new IteratorDeclaration(ParseBlockOrExtern(ref i))
					{
						StartIndex = start,
						EndIndex = end,
						Document = document,
						DocString = startTok.Documentation,
					};
				}
				else if (Accept(i, TokenType.Operator))
				{
					if (!modifiers.IsEmpty)
						throw new ParseException(tok[i], "Operator declarations cannot have any modifiers.");
					var op = ParseOperatorOverload(ref i);
					op.DocString = startTok.Documentation;
					target.Operators.Add(op);
				}
				else if (!modifiers.IsEmpty) // need at least an access level modifier
				{
					// The only possibility remaining here is that of a field or method
					// declared without the var/function keyword, such as
					//     public foo(x, y, z) { }
					//     public x, y, z;
					// which means that no matter what, we have to have at least one identifier.
					// However! If we encounter 'this', it must be parsed as a method, because
					// it could only ever be part of '[public|protected|private] this(...) { ... }'.
					if (Accept(i, TokenType.This))
					{
						if (modifiers.IsStatic)
							ParseError(i, "This member cannot be declared static.");
						var method = ParseMethod(ref i, ref modifiers);
						method.DocString = startTok.Documentation;
						target.Methods.Add(method);
					}
					else
					{
						Expect(i, TokenType.Identifier);
						// If the next token is a ParenOpen, it's a method.
						if (Accept(i + 1, TokenType.ParenOpen))
						{
							var method = ParseMethod(ref i, ref modifiers);
							method.DocString = startTok.Documentation;
							target.Methods.Add(method);
						}
						else
						{
							// Otherwise, it's a field.
							modifiers.ValidateForField(tok[i]);
							var field = ParseFieldDeclaration(ref i, false, modifiers.Access, modifiers.IsStatic);
							field.DocString = startTok.Documentation;
							target.Fields.Add(field);
						}
					}
				}
				else if (Accept(i, TokenType.Identifier, TokenType.This))
				{
					// At this point, we could only ever be dealing with a method declared
					// without an access level modifier and function keyword, e.g.:
					// class Test {
					//     foo(a, b) { }
					// }
					// Note that the name of the method could still be 'this', because it
					// might be an invocator.
					var method = ParseMethod(ref i, ref modifiers);
					method.DocString = startTok.Documentation;
					target.Methods.Add(method);
				}
				else
					ParseError(i, "Invalid token in class body.");
			} // while

			if (SimplifiedTree && !target.IsStatic && target.Constructors.Count == 0)
			{
				// Add the default constructor, which is public and parameterless
				target.Constructors.Add(new ConstructorDeclaration(AccessLevel.Public,
					EmptyArrays.CtorParameters, Splat.None, new Block()) { Document = document });
			}
		}

		private void ParseConstructorDeclaration(ref int i, ClassDeclaration target, Token startTok, ref MemberModifiers modifiers)
		{
			var newTok = tok[i++];
			modifiers.ValidateForConstructor(newTok);
			Expect(ref i, TokenType.ParenOpen);

			var parameters = ParseConstructorParameters(ref i);

			Expect(ref i, TokenType.ParenClose);

			if (modifiers.IsStatic)
			{
				if (parameters.Parameters.Length > 0)
					ParseError(parameters.Parameters[0], "Static constructors cannot declare any parameters.");

				// Note: ParseConstructorBody permits an optional 'new base(...);' as
				// the first statement, so we can't use that here.
				var body = ParseBlockOrExtern(ref i);
				target.StaticConstructor = new ConstructorDeclaration(parameters.Parameters, body)
				{
					StartIndex = newTok.Index,
					EndIndex = newTok.EndIndex,
					Document = document,
					DocString = startTok.Documentation,
				};
			}
			else
			{
				Block body;
				ConstructorCall ctorCall = null;
				if (Accept(i, TokenType.Semicolon))
					body = new Block()
					{
						StartIndex = tok[i].Index,
						EndIndex = tok[i++].EndIndex,
						Document = document,
					};
				else
					// Permits 'new base(...);' as first statement
					body = ParseConstructorBody(ref i, out ctorCall);

				target.Constructors.Add(new ConstructorDeclaration(modifiers.Access, parameters.Parameters, parameters.Splat, body)
				{
					ConstructorCall = ctorCall,
					StartIndex = newTok.Index,
					EndIndex = newTok.EndIndex,
					Document = document,
					DocString = startTok.Documentation,
				});
			}
		}

		private ParameterInfo<ConstructorParam> ParseConstructorParameters(ref int i)
		{
			var parameters = new TempList<ConstructorParam>();
			var output = new ParameterInfo<ConstructorParam>(EmptyArrays.CtorParameters);

			if (Accept(i, TokenType.ParenClose))
				return output;

			if (Accept(ref i, TokenType.Splat))
				output.Splat = Splat.Beginning;

			var optionalSeen = false;
			bool isByRef;
			do
			{
				isByRef = Accept(ref i, TokenType.Ref);
				output.HasRefParams |= isByRef;
				var thisPrefix = Accept(ref i, TokenType.This);

				Token nameTok = null;
				if (thisPrefix) // this.<name> parameter
				{
					if (isByRef)
						ParseError(i, "A 'this' parameter cannot be passed by reference.");

					Expect(ref i, TokenType.Dot);
					nameTok = Expect(ref i, TokenType.Identifier);
				}
				else if (Accept(i, TokenType.Identifier))
					nameTok = tok[i++];
				else
					ParseError(i, "Expected identifier or 'this'.");

				if (isByRef && output.Splat == Splat.Beginning && parameters.Count == 0)
					ParseError(nameTok, CannotPassVariadicParamByRef);

				if (Accept(i, TokenType.Assign)) // optional parameter
				{
					if (output.Splat != Splat.None)
						ParseError(i, CannotMixOptionalAndVariadicParams);
					if (isByRef)
						ParseError(i, CannotPassOptionalParamByRef);
					i++;

					var value = ParseExpression(ref i);
					// As in ParseParameterList, the default value expression is validated later.
					parameters.Add(new ConstructorParam(nameTok.Value, thisPrefix, value)
					{
						StartIndex = nameTok.Index,
						EndIndex = nameTok.EndIndex,
						Document = document,
					});
					output.HasOptionalParams = optionalSeen = true;
				}
				else if (optionalSeen)
					ParseError(nameTok, RequiredParamAfterOptional);
				else
					parameters.Add(new ConstructorParam(nameTok.Value, thisPrefix, isByRef)
					{
						StartIndex = nameTok.Index,
						EndIndex = nameTok.EndIndex,
						Document = document,
					});
			} while (Accept(ref i, TokenType.Comma));

			if (Accept(i, TokenType.Splat))
			{
				if (optionalSeen)
					ParseError(i, CannotMixOptionalAndVariadicParams);
				if (isByRef)
					ParseError(i, CannotPassVariadicParamByRef);
				if (output.Splat != Splat.None)
					ParseError(i, MoreThanOneVariadicParam);
				i++;
				output.Splat = Splat.End;
			}

			output.Parameters = parameters.ToArray();
			return output;
		}

		private Block ParseConstructorBody(ref int i, out ConstructorCall ctorCall)
		{
			ctorCall = null;

			if (AcceptExtension(i, "__extern"))
				return ParseExternBody(ref i);

			// Block
			var start = Expect(ref i, TokenType.CurlyOpen).Index;

			var body = new TempList<Statement>();
			if (!Accept(i, TokenType.CurlyClose))
			{
				var first = ParseStatement(ref i, allowCtorCall: true);
				if (first is ConstructorCall)
					ctorCall = (ConstructorCall)first;
				body.Add(first);
			}

			while (!Accept(i, TokenType.CurlyClose))
				body.Add(ParseStatement(ref i));

			Expect(i, TokenType.CurlyClose);

			return new Block(body.ToArray())
			{
				StartIndex = start,
				EndIndex = tok[i++].EndIndex,
				Document = document,
			};
		}

		private FieldDeclaration ParseFieldDeclaration(ref int i, bool isConst, AccessLevel access, bool isStatic)
		{
			if (Accept(i, TokenType.ParenOpen))
				ParseError(i, "Parallel declaration is not allowed for fields or constants.");

			var vars = new TempList<VariableDeclarator>();
			do
			{
				Expect(i, TokenType.Identifier);
				var nameTok = tok[i++];

				Expression value = null;

				if (Accept(ref i, TokenType.Assign))
					value = ParseExpression(ref i);
				else if (isConst)
					ParseError(i, "Constant declaration without value.");

				vars.Add(new VariableDeclarator(nameTok.Value, value)
				{
					StartIndex = nameTok.Index,
					EndIndex = nameTok.EndIndex,
					Document = document,
				});
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.Semicolon);

			return new FieldDeclaration(access, isConst, vars.ToArray())
			{
				IsStatic = isStatic,
				Document = document,
			};
		}

		private MethodDeclaration ParseMethod(ref int i, ref MemberModifiers modifiers)
		{
			if (!Accept(i, TokenType.Identifier, TokenType.This))
				ParseError(i, "Expected identifier or 'this'.");

			var nameTok = tok[i++]; // this name may also be 'this', but that's okay
			modifiers.ValidateForMethodOrProperty(nameTok);

			Expect(ref i, TokenType.ParenOpen);
			var parameters = ParseParameterList(ref i);
			Expect(ref i, TokenType.ParenClose);

			Statement body = null;
			if (Accept(i, TokenType.Semicolon))
				body = new EmptyStatement(tok[i].Index, tok[i++].EndIndex) { Document = document };
			else if (Accept(i, TokenType.CurlyOpen))
				body = ParseBlock(ref i);
			else if (AcceptExtension(i, "__extern"))
				body = ParseExternBody(ref i);
			else
				ParseError(i, "Expected block or ';'.");

			if (modifiers.IsAbstract != (body is EmptyStatement))
			{
				ParseError(nameTok, modifiers.IsAbstract ?
					"Abstract methods cannot have a body." :
					"Non-abstract methods cannot have empty bodies.");
			}

			return new MethodDeclaration(nameTok.Value, modifiers.Access, parameters.Parameters, parameters.Splat, body)
				{
					StartIndex = nameTok.Index,
					EndIndex = nameTok.EndIndex,
					Document = document,
					IsOverride = modifiers.IsOverride,
					IsAbstract = modifiers.IsAbstract,
					IsOverridable = modifiers.IsOverridable,
					IsStatic = modifiers.IsStatic,
				};
		}

		private PropertyAccessorDeclaration ParseProperty(ref int i, bool isSetter, ref MemberModifiers modifiers)
		{
			var nameTok = Expect(ref i, TokenType.Identifier);
			modifiers.ValidateForMethodOrProperty(nameTok);

			Statement body;
			if (Accept(i, TokenType.Semicolon))
				body = new EmptyStatement(tok[i].Index, tok[i++].EndIndex) { Document = document };
			else if (!isSetter && Accept(i, TokenType.Assign)) // get x = expr;
			{
				i++;
				body = ExpressionToStatement(ParseExpression(ref i));
				Expect(ref i, TokenType.Semicolon);
			}
			else
				body = ParseBlockOrExtern(ref i);

			if (modifiers.IsAbstract && !(body is EmptyStatement))
				ParseError(nameTok, "Abstract property accessors cannot have a body.");

			if (!modifiers.IsAbstract && body is EmptyStatement)
				ParseError(nameTok, "Non-abstract property accessors cannot have empty bodies.");

			return new PropertyAccessorDeclaration(nameTok.Value, modifiers.Access, isSetter, body)
			{
				StartIndex = nameTok.Index,
				EndIndex = nameTok.EndIndex,
				Document = document,
				IsOverride = modifiers.IsOverride,
				IsAbstract = modifiers.IsAbstract,
				IsOverridable = modifiers.IsOverridable,
				IsStatic = modifiers.IsStatic,
			};
		}

		private IndexerAccessorDeclaration ParseIndexer(ref int i, bool isSetter, ref MemberModifiers modifiers)
		{
			var startTok = Expect(ref i, TokenType.This);
			modifiers.ValidateForIndexer(startTok);
			Expect(ref i, TokenType.SquareOpen);

			var parameters = ParseParameterList(ref i, false, TokenType.SquareClose);
			if (parameters.HasOptionalParams ||
				parameters.HasRefParams ||
				parameters.Splat != Splat.None)
				ParseError(startTok, "Indexers cannot have any optional, variadic or pass-by-reference parameters.");

			Expect(ref i, TokenType.SquareClose);

			Statement body;
			if (Accept(i, TokenType.Semicolon))
				body = new EmptyStatement(tok[i].Index, tok[i++].EndIndex) { Document = document };
			else if (!isSetter && Accept(i, TokenType.Assign)) // get this[x] = expr;
			{
				i++;
				body = ExpressionToStatement(ParseExpression(ref i));
				Expect(ref i, TokenType.Semicolon);
			}
			else
				body = ParseBlockOrExtern(ref i);

			if (modifiers.IsAbstract != (body is EmptyStatement))
			{
				if (modifiers.IsAbstract)
					ParseError(startTok, modifiers.IsAbstract ?
						"Abstract indexer accessors cannot have a body." :
						"Non-abstract indexer accessors cannot have empty bodies.");
			}

			return new IndexerAccessorDeclaration(modifiers.Access, isSetter, parameters.Parameters, body)
			{
				StartIndex = startTok.Index,
				EndIndex = startTok.EndIndex,
				Document = document,
				IsOverride = modifiers.IsOverride,
				IsAbstract = modifiers.IsAbstract,
				IsOverridable = modifiers.IsOverridable,
				IsStatic = modifiers.IsStatic,
			};
		}

		private OperatorOverloadDeclaration ParseOperatorOverload(ref int i)
		{
			Expect(i, TokenType.Operator);
			var start = tok[i++].Index;

			int expectedCount = 0;
			// unambiguously binary operators
			if (Accept(i, TokenType.OverloadableBinaryOperator))
				expectedCount = 2;
			// unambiguously unary operator
			else if (Accept(i, TokenType.Tilde))
				expectedCount = 1;
			// may be either
			else if (Accept(i, TokenType.Plus, TokenType.Minus))
				expectedCount = -1; // don't know, look at context
			else
				ParseError(i, "Expected overloadable operator.");

			var opTokenIndex = i;
			var op = tok[i].Type;
			var end = tok[i++].EndIndex;

			Expect(ref i, TokenType.ParenOpen); // opening parenthesis no matter what

			var parameters = ParseParameterList(ref i, false);
			if (parameters.HasOptionalParams ||
				parameters.HasRefParams ||
				parameters.Splat != Splat.None)
				ParseError(opTokenIndex, "Operator overloads cannot have any optional, variadic or pass-by-reference parameters.");

			Expect(ref i, TokenType.ParenClose); // ahem

			var body = ParseBlockOrExtern(ref i);

			OperatorOverloadDeclaration result = null;
			if (parameters.Parameters.Length == 1)
			{
				if (expectedCount == 2)
					ParseError(opTokenIndex, "This is a binary operator; two parameters are required.");

				UnaryOperator uop = op == TokenType.Plus ? UnaryOperator.Plus :
					op == TokenType.Minus ? UnaryOperator.Minus :
					UnaryOperator.BitwiseNot;
				result = new UnaryOperatorOverload(uop, parameters.Parameters[0].Name, body);
			}
			else if (parameters.Parameters.Length == 2)
			{
				if (expectedCount == 1)
					ParseError(opTokenIndex, "This is a unary operator; one parameter is required.");

				result = new BinaryOperatorOverload(TokenTypeToOperator(op),
					parameters.Parameters[0].Name,
					parameters.Parameters[1].Name, body);
			}
			else
				ParseError(opTokenIndex, "Wrong number of parameters for operator overload (expected {0}).",
					expectedCount == 1 ? "1" :
					expectedCount == 2 ? "2" :
					"1 or 2");

			result.StartIndex = start;
			result.EndIndex = end;
			result.Document = document;
			return result;
		}

		private static BinaryOperator TokenTypeToOperator(TokenType type)
		{
			switch (type)
			{
				case TokenType.DoubleEqual: return BinaryOperator.Equality;
				case TokenType.Plus: return BinaryOperator.Addition;
				case TokenType.Minus: return BinaryOperator.Subtraction;
				case TokenType.Pipe: return BinaryOperator.BitwiseOr;
				case TokenType.Multiply: return BinaryOperator.Multiplication;
				case TokenType.Divide: return BinaryOperator.Division;
				case TokenType.Mod: return BinaryOperator.Modulo;
				case TokenType.Ampersand: return BinaryOperator.BitwiseAnd;
				case TokenType.Caret: return BinaryOperator.BitwiseXor;
				case TokenType.ShiftLeft: return BinaryOperator.ShiftLeft;
				case TokenType.ShiftRight: return BinaryOperator.ShiftRight;
				case TokenType.Power: return BinaryOperator.Exponentiation;
				case TokenType.Hash: return BinaryOperator.Hash;
				case TokenType.Dollar: return BinaryOperator.Dollar;
				case TokenType.Compare: return BinaryOperator.Comparison;
				default: throw new ArgumentException("Internal error: invalid overloadable binary operator.", "type");
			}
		}

		private EnumDeclaration ParseEnum(ref int i, ref MemberModifiers modifiers)
		{
			Expect(ref i, TokenType.Enum);

			bool isSet = Accept(ref i, TokenType.Set);

			var enumName = Expect(ref i, TokenType.Identifier);

			var @enum = new EnumDeclaration(enumName.Value, isSet, modifiers.Access)
			{
				StartIndex = enumName.Index,
				EndIndex = enumName.EndIndex,
				Document = document,
			};
			Expect(ref i, TokenType.CurlyOpen);

			if (Accept(ref i, TokenType.CurlyClose))
				return @enum;

			long counter = 1; // Enumeration begins at 1 in Osprey
			do
			{
				var memberName = Expect(ref i, TokenType.Identifier);

				Expression memberValue = null;
				if (Accept(ref i, TokenType.Assign))
					memberValue = ParseExpression(ref i);
				else if (SimplifiedTree)
				{
					if (counter < 0)
						ParseError(i, "Too many auto-enumerated values in enum; the counter has overflowed.");
					memberValue = new ConstantExpression(ConstantValue.CreateInt(counter))
					{
						StartIndex = memberName.Index,
						EndIndex = memberName.EndIndex,
						Document = document,
					};

					// Both of these may overflow into the sign bit, but that's
					// not a problem until you try to use the value.
					if (isSet)
						counter <<= 1;
					else
						counter = unchecked(counter + 1);
				}

				@enum.Members.Add(new EnumMember(memberName.Value, memberValue)
				{
					StartIndex = memberName.Index,
					EndIndex = memberValue != null ? memberValue.EndIndex : memberName.EndIndex,
					Document = document,
					DocString = memberName.Documentation,
				});
			} while (Accept(ref i, TokenType.Comma) && !Accept(i, TokenType.CurlyClose));

			Expect(ref i, TokenType.CurlyClose);

			return @enum;
		}

		private Statement ExpressionToStatement(Expression expr)
		{
			// In some situations, a body can be replaced by a single expression,
			// which becomes the return value of the construct. In effect,
			//    = expr;
			// is equivalent to
			//    { return expr; }
			// so we transform it to that here, but only if SimplifiedTree.

			if (!SimplifiedTree)
				return new ExpressionStatement(expr)
				{
					StartIndex = expr.StartIndex,
					EndIndex = expr.EndIndex,
					Document = expr.Document,
				};

			var retStmt = new ReturnStatement(expr)
			{
				StartIndex = expr.StartIndex,
				EndIndex = expr.EndIndex,
				Document = expr.Document,
			};
			var result = new Block(retStmt)
			{
				StartIndex = expr.StartIndex,
				EndIndex = expr.EndIndex,
				Document = expr.Document,
			};
			return result;
		}

		#endregion

		internal TypeName ParseTypeName(ref int i)
		{
			var start = tok[i].Index;
			var global = Accept(ref i, TokenType.Global);
			if (global)
				Expect(ref i, TokenType.Dot); // always 'global' '.' identifier

			Expect(i, TokenType.Identifier);

			var idents = new TempList<string>(1);
			idents.Add(tok[i++].Value);
			while (Accept(ref i, TokenType.Dot))
				idents.Add(Expect(ref i, TokenType.Identifier).Value);

			return new TypeName(idents.ToArray(), global)
			{
				StartIndex = start,
				EndIndex = tok[i - 1].EndIndex,
				Document = document,
			};
		}

		internal QualifiedName ParseQualifiedName(ref int i)
		{
			Expect(i, TokenType.Identifier);

			var start = tok[i].Index;

			var idents = new TempList<string>(1);
			idents.Add(tok[i++].Value);
			while (Accept(ref i, TokenType.Dot))
				idents.Add(Expect(ref i, TokenType.Identifier).Value);
			return new QualifiedName(idents.ToArray())
			{
				StartIndex = start,
				EndIndex = tok[i - 1].EndIndex,
				Document = document,
			};
		}

		private ParameterInfo<Parameter> ParseParameterList(ref int i,
			bool allowEmpty = true, TokenType end = TokenType.ParenClose)
		{
			var parameters = new TempList<Parameter>();
			var output = new ParameterInfo<Parameter>(EmptyArrays.Parameters);

			if (Accept(i, end))
			{
				if (!allowEmpty)
					ParseError(i, "At least one parameter is needed.");
				// Don't skip closing bracket
				return output;
			}

			if (Accept(ref i, TokenType.Splat))
				output.Splat = Splat.Beginning;

			var optionalSeen = false;
			bool isByRef;
			do
			{
				isByRef = Accept(ref i, TokenType.Ref);
				output.HasRefParams |= isByRef;
				var nameTok = Expect(ref i, TokenType.Identifier);

				if (isByRef && output.Splat == Splat.Beginning && parameters.Count == 0)
					ParseError(nameTok, CannotPassVariadicParamByRef); 

				if (Accept(i, TokenType.Assign)) // optional parameter/argument
				{
					if (output.Splat != Splat.None)
						ParseError(i, CannotMixOptionalAndVariadicParams);
					if (isByRef)
						ParseError(i, CannotPassOptionalParamByRef);
					i++;

					var value = ParseExpression(ref i);
					// The expression is validated at a later stage of parsing,
					// since it needs to be a constant expression, or an empty
					// list/hash expression.
					parameters.Add(new Parameter(nameTok.Value, value)
					{
						StartIndex = nameTok.Index,
						EndIndex = value.EndIndex,
						Document = document
					});
					output.HasOptionalParams = optionalSeen = true;
				}
				else if (optionalSeen)
					ParseError(i - 1, RequiredParamAfterOptional);
				else
					parameters.Add(new Parameter(nameTok.Value, isByRef)
					{
						StartIndex = nameTok.Index,
						EndIndex = nameTok.EndIndex,
						Document = document,
					});
			} while (Accept(ref i, TokenType.Comma));

			if (Accept(i, TokenType.Splat))
			{
				if (optionalSeen)
					ParseError(i, CannotMixOptionalAndVariadicParams);
				if (isByRef)
					ParseError(i, CannotPassVariadicParamByRef);
				if (output.Splat != Splat.None)
					ParseError(i, MoreThanOneVariadicParam);
				output.Splat = Splat.End;
				i++;
			}

			output.Parameters = parameters.ToArray();
			return output;
		}

		#region Statements

		private Statement ParseStatement(ref int i, bool allowCtorCall = false)
		{
			var stmt = ParseStatementInner(ref i);
			if (!allowCtorCall && stmt is ConstructorCall)
				ParseError(stmt, "A constructor call is only allowed as the first statment in a constructor.");
			return stmt;
		}

		private Statement ParseStatementInner(ref int i)
		{
			if (Accept(i, TokenType.Var, TokenType.Const))
				return ParseLocalVariableDeclaration(ref i);
			if (Accept(i, TokenType.Function))
				return ParseLocalFunctionDeclaration(ref i, isLocal: true);
			if (Accept(i, TokenType.If))
				return ParseIfStatement(ref i);
			if (Accept(i, TokenType.Try))
				return ParseTryStatement(ref i);
			if (Accept(i, TokenType.Return, TokenType.Yield))
				return ParseTransferStatement(ref i);
			if (Accept(i, TokenType.Break, TokenType.Next))
				return ParseLoopFlowStatement(ref i);
			if (Accept(i, TokenType.Throw))
				return ParseThrowStatement(ref i);
			if (Accept(i, TokenType.With))
				return ParseWithStatement(ref i);
			if (Accept(i, TokenType.New) && Accept(i + 1, TokenType.Base, TokenType.This))
				return ParseConstructorCall(ref i);
			if (Accept(i, TokenType.Semicolon))
				return new EmptyStatement(tok[i].Index, tok[i++].EndIndex) { Document = document };

			var loopLabelFound = Accept(i, TokenType.Identifier) && Accept(i + 1, TokenType.Colon);
			string loopLabel = loopLabelFound ? tok[i].Value : null;
			if (loopLabelFound) i += 2;

			if (Accept(i, TokenType.For))
				return ParseForStatement(ref i, loopLabel);
			if (Accept(i, TokenType.While))
				return ParseWhileStatement(ref i, loopLabel);
			if (Accept(i, TokenType.Do))
				return ParseDoWhileStatement(ref i, loopLabel);

			if (loopLabelFound)
				ParseError(i, "Expected for, while, or do–while loop after label.");

			// Note: although not technically conforming to the grammar,
			// ParseExpressionStatement also parses parallel assignments.
			// Parallel assignments are not expression statements, but
			// they begin with a series of expressions, and this way
			// there's no need whatsoever for backtracking.
			return ParseExpressionStatement(ref i);
		}

		private Statement ParseLocalVariableDeclaration(ref int i)
		{
			var isConst = Accept(i, TokenType.Const);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			// parallel declaration, e.g. var (x, y, z) = someList;
			if (Accept(i, TokenType.ParenOpen))
			{
				if (isConst)
					ParseError(i, "Parallel declaration is not allowed for constants.");

				i++;

				var names = new TempList<string>();
				do
				{
					names.Add(Expect(ref i, TokenType.Identifier).Value);
				} while (Accept(ref i, TokenType.Comma));

				Expect(ref i, TokenType.ParenClose);
				Expect(ref i, TokenType.Assign, "Parallel declaration without initializer.");

				var value = ParseExpression(ref i);

				Expect(ref i, TokenType.Semicolon);

				return new ParallelLocalVariableDeclaration(names.ToArray(), value)
				{
					StartIndex = start,
					EndIndex = end,
					Document = document,
				};
			}

			var vars = new TempList<VariableDeclarator>();
			do
			{
				var name = Expect(ref i, TokenType.Identifier);

				Expression value = null;

				if (Accept(ref i, TokenType.Assign))
					value = ParseExpression(ref i);
				else if (isConst)
					ParseError(i, "Constant declaration without value.");

				vars.Add(new VariableDeclarator(name.Value, value)
				{
					StartIndex = name.Index,
					EndIndex = name.EndIndex,
					Document = document,
				});
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.Semicolon);

			return new SimpleLocalVariableDeclaration(isConst, vars.ToArray())
			{
				StartIndex = start,
				EndIndex = end,
				Document = document
			};
		}

		private LocalFunctionDeclaration ParseLocalFunctionDeclaration(ref int i, bool isLocal)
		{
			Expect(ref i, TokenType.Function);

			var nameTok = Expect(ref i, TokenType.Identifier);

			Expect(ref i, TokenType.ParenOpen);

			var pi = ParseParameterList(ref i);

			var end = Expect(ref i, TokenType.ParenClose).EndIndex;

			Block body;
			if (isLocal)
				body = ParseBlock(ref i);
			else
				body = ParseBlockOrExtern(ref i);

			return new LocalFunctionDeclaration(nameTok.Value, pi.Parameters, pi.Splat, body)
			{
				StartIndex = nameTok.Index,
				EndIndex = end,
				Document = document,
			};
		}

		private Block ParseBlock(ref int i)
		{
			int start = Expect(ref i, TokenType.CurlyOpen).Index;

			var statements = new TempList<Statement>();

			while (!Accept(i, TokenType.CurlyClose))
			{
				statements.Add(ParseStatement(ref i));
			}

			Expect(ref i, TokenType.CurlyClose);

			return new Block(statements.ToArray())
			{
				StartIndex = start,
				EndIndex = tok[i - 1].EndIndex,
				Document = document,
			};
		}

		private Statement ParseIfStatement(ref int i)
		{
			Expect(i, TokenType.If);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			Expression cond;
			try { cond = ParseExpression(ref i); }
			catch (ParseException e) { throw new ParseException(e.Token ?? tok[i], "Expected expression.", e); }

			var body = ParseControlBody(ref i);

			ElseClause @else = null;
			if (Accept(i, TokenType.Else))
				@else = ParseElseClause(ref i);

			return new IfStatement(cond, body, @else) { StartIndex = start, EndIndex = end, Document = document };
		}

		private Statement ParseControlBody(ref int i)
		{
			if (Accept(ref i, TokenType.Colon))
			{
				var stmt = ParseStatement(ref i);
				if (stmt is LocalDeclaration)
					ParseError(stmt, "Embedded statement cannot be a declaration.");

				Statement result;
				if (!SimplifiedTree)
					result = new EmbeddedStatement(stmt);
				else
					result = new Block(new[] { stmt });

				result.StartIndex = stmt.StartIndex;
				result.EndIndex = stmt.EndIndex;
				result.Document = document;
				return result;
			}
			if (!Accept(i, TokenType.CurlyOpen))
				ParseError(i, "Expected ': <statement>' or block.");

			return ParseBlock(ref i);
		}

		private ElseClause ParseElseClause(ref int i)
		{
			Expect(i, TokenType.Else);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			Statement body;
			if (Accept(i, TokenType.CurlyOpen))
				body = ParseBlock(ref i);
			else
			{
				Accept(ref i, TokenType.Colon); // Optional ':'

				body = ParseStatement(ref i);
				if (body is LocalDeclaration)
					ParseError(body, "Embedded statement cannot be a declaration.");

				if (SimplifiedTree)
					// Force the embedded statement into a block
					body = new Block(new[] { body })
					{
						StartIndex = body.StartIndex,
						EndIndex = body.EndIndex,
						Document = document,
					};
			}

			return new ElseClause(body) { Document = document };
		}

		private Statement ParseTryStatement(ref int i)
		{
			var startTok = Expect(ref i, TokenType.Try);

			var body = ParseBlock(ref i);

			var catches = new TempList<CatchClause>();
			var genericCatchSeen = false;

			while (Accept(i, TokenType.Catch))
			{
				int catchStart = tok[i].Index, catchEnd = tok[i++].EndIndex;
				if (Accept(i, TokenType.Identifier, TokenType.Global)) // specific catch clause
				{
					if (genericCatchSeen)
						throw new ParseException(tok[i], "The generic catch clause must be the last catch clause.");

					var type = ParseTypeName(ref i);
					string varName = null;

					if (Accept(ref i, TokenType.In))
						varName = Expect(ref i, TokenType.Identifier).Value;

					catches.Add(new SpecificCatchClause(type, varName, ParseBlock(ref i))
					{
						StartIndex = catchStart,
						EndIndex = catchEnd,
						Document = document,
					});
				}
				else if (Accept(i, TokenType.CurlyOpen)) // generic catch clause
				{
					if (genericCatchSeen)
						throw new ParseException(tok[i], "There can only be one generic catch clause per try-catch.");

					catches.Add(new CatchClause(ParseBlock(ref i))
					{
						StartIndex = catchStart,
						EndIndex = catchEnd,
						Document = document,
					});
					genericCatchSeen = true;
				}
				else
					throw new ParseException(tok[i], "Expected type name or block.");
			}

			FinallyClause fin = null;
			if (Accept(i, TokenType.Finally))
			{
				int finStart = tok[i].Index, finEnd = tok[i++].EndIndex;
				fin = new FinallyClause(ParseBlock(ref i))
				{
					StartIndex = finStart,
					EndIndex = finEnd,
					Document = document,
				};
			}

			if (catches.Count == 0 && fin == null)
				throw new ParseException(startTok, "A try statement must have at least one catch or finally clause.");

			return new TryStatement(body, catches.ToArray(), fin)
			{
				StartIndex = startTok.Index,
				EndIndex = startTok.EndIndex,
				Document = document
			};
		}

		private Statement ParseTransferStatement(ref int i)
		{
			var isYield = tok[i].Type == TokenType.Yield;
			if (!Accept(i, TokenType.Return, TokenType.Yield))
				ParseError(i, "Internal error: expected 'return' or 'yield'");

			int start = tok[i].Index, end = tok[i++].EndIndex;

			if (Accept(i, TokenType.Semicolon))
				if (isYield)
					ParseError(i, "Empty yield statement is not allowed. Use 'return;' to stop a generator.");
				else
				{
					i++;
					return new ReturnStatement() { StartIndex = start, EndIndex = end, Document = document };
				}

			var values = new TempList<Expression>(1);
			do
			{
				try { values.Add(ParseExpression(ref i)); }
				catch (ParseException e) { throw e.Extend("Expected expression."); }
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.Semicolon);

			if (values.Count > 1 && SimplifiedTree)
			{
				var list = new ListLiteralExpression(values.ToNewArray())
				{
					StartIndex = values[0].StartIndex,
					EndIndex = values[values.Count - 1].EndIndex,
					Document = document,
				};
				values.Clear();
				values.Add(list);
			}

			Statement result;
			if (isYield)
				result = new YieldStatement(values.ToArray());
			else
				result = new ReturnStatement(values.ToArray());
			result.StartIndex = start;
			result.EndIndex = end;
			result.Document = document;
			return result;
		}

		private Statement ParseLoopFlowStatement(ref int i)
		{
			var isBreak = Accept(i, TokenType.Break);

			if (!Accept(i, TokenType.Break, TokenType.Next))
				ParseError(i, "Internal error: expected 'break' or 'next'.");

			int start = tok[i].Index, end = tok[i++].EndIndex;

			string label = null;
			if (Accept(i, TokenType.Identifier))
			{
				label = tok[i++].Value;
			}

			Expect(ref i, TokenType.Semicolon);

			Statement result;
			if (isBreak)
				result = new BreakStatement(label);
			else
				result = new NextStatement(label);
			result.StartIndex = start;
			result.EndIndex = end;
			result.Document = document;
			return result;
		}

		private Statement ParseThrowStatement(ref int i)
		{
			Expect(i, TokenType.Throw);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			Expression value = null;
			if (!Accept(ref i, TokenType.Semicolon))
			{
				value = ParseExpression(ref i);

				Expect(ref i, TokenType.Semicolon);
			}

			return new ThrowStatement(value) { StartIndex = start, EndIndex = end, Document = document };
		}

		private Statement ParseForStatement(ref int i, string label = null)
		{
			Expect(i, TokenType.For);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			var vars = new TempList<string>(1);
			do
			{
				vars.Add(Expect(ref i, TokenType.Identifier).Value);
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.In);

			var expr = ParseExpression(ref i);
			var body = ParseControlBody(ref i);
			ElseClause @else = null;

			if (Accept(i, TokenType.Else))
				@else = ParseElseClause(ref i);

			return new ForStatement(label, vars.ToArray(), expr, body, @else)
			{
				StartIndex = start,
				EndIndex = end,
				Document = document,
			};
		}

		private Statement ParseWhileStatement(ref int i, string label = null)
		{
			Expect(i, TokenType.While);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			Expression cond;
			try { cond = ParseExpression(ref i); }
			catch (ParseException e) { throw e.Extend("Expected expression."); }

			var body = ParseControlBody(ref i);

			return new WhileStatement(label, cond, body) { StartIndex = start, EndIndex = end, Document = document };
		}

		private Statement ParseDoWhileStatement(ref int i, string label = null)
		{
			Expect(i, TokenType.Do);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			var body = ParseBlock(ref i); // not ParseControlBody! do-while only takes a block.

			Expression cond = null;
			if (!Accept(i, TokenType.Semicolon))
			{
				Expect(ref i, TokenType.While);
				cond = ParseExpression(ref i);
			}

			Expect(ref i, TokenType.Semicolon);

			return new DoWhileStatement(label, body, cond) { StartIndex = start, EndIndex = end, Document = document };
		}

		private Statement ParseWithStatement(ref int i)
		{
			Expect(i, TokenType.With);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			string varName = Expect(ref i, TokenType.Identifier).Value;

			Expect(ref i, TokenType.Assign);

			var initializer = ParseExpression(ref i);
			var body = ParseControlBody(ref i);

			if (SimplifiedTree)
			{
				// Surround the body with a try-finally. This will allow us to detect
				// the presence of a try statement (which affects certain things, e.g.
				// it's not possible to yield inside a try), while also letting us
				// declare and initialize the 'with' variable in the surrounding block.
				// Code will be injected later into the finally clause.
				var @try = new TryStatement((Block)body,
					catches: EmptyArrays.CatchClauses,
					fin: new FinallyClause(new Block()) { StartIndex = start, EndIndex = end, Document = document })
				{
					StartIndex = start,
					EndIndex = end,
					Document = document,
				};
				body = new Block(@try);
			}

			return new WithStatement(varName, initializer, body)
				{
					StartIndex = start,
					EndIndex = end,
					Document = document,
				};
		}

		private Statement ParseConstructorCall(ref int i)
		{
			Expect(i, TokenType.New);
			var isBaseCall = Accept(i + 1, TokenType.Base);

			int start = tok[i].Index, end = tok[i + 1].EndIndex;
			i += 2;
			Expect(ref i, TokenType.ParenOpen);

			bool hasRefArgs;
			var args = ParseArgumentList(ref i, out hasRefArgs);

			Expect(ref i, TokenType.ParenClose);
			Expect(ref i, TokenType.Semicolon);

			return new ConstructorCall(args, hasRefArgs, isBaseCall) { StartIndex = start, EndIndex = end, Document = document };
		}

		private Statement ParseExpressionStatement(ref int i)
		{
			var start = tok[i].Index;
			var expr = ParseExpression(ref i);

			if (expr is AssignmentExpression)
			{
				Expect(ref i, TokenType.Semicolon);
				((AssignmentExpression)expr).IgnoreValue = true;
				return new ExpressionStatement(expr)
				{
					StartIndex = start,
					EndIndex = expr.EndIndex,
					Document = document,
				};
			}

			if (Accept(i, TokenType.CompoundAssign))
			{
				var assignableExpr = EnsureAssignable(expr);

				var op = GetCompoundAssignmentOp(tok[i].Type);
				i++;

				Expression value;
				try { value = ParseExpression(ref i); }
				catch (ParseException e) { throw e.Extend("Expected expression."); }

				Expect(ref i, TokenType.Semicolon);

				return new CompoundAssignment(assignableExpr, value, op)
				{
					StartIndex = start,
					EndIndex = value.EndIndex,
					Document = document,
				};
			}

			if (Accept(i, TokenType.Comma)) // parallel assignment
			{
				return ParseParallelAssignment(ref i, expr, start);
			}

			// plain expression
			Expect(ref i, TokenType.Semicolon);
			var end = tok[i - 2].EndIndex;

			if (!(expr is BinaryOperatorExpression && ((BinaryOperatorExpression)expr).Operator == BinaryOperator.FunctionApplication ||
				expr is InvocationExpression || expr is ObjectCreationExpression ||
				expr is SafeAccess && ((SafeAccess)expr).Chain[((SafeAccess)expr).Chain.Count - 1] is SafeInvocation))
				throw new ParseException(expr,
					"Only invocation, function application, assignment and object creation can be used as a statement.");

			return new ExpressionStatement(expr)
			{
				StartIndex = start,
				EndIndex = end,
				Document = document,
			};
		}

		private static BinaryOperator GetCompoundAssignmentOp(TokenType type)
		{
			switch (type)
			{
				case TokenType.PlusAssign: return BinaryOperator.Addition;
				case TokenType.MinusAssign: return BinaryOperator.Subtraction;
				case TokenType.PipeAssign: return BinaryOperator.BitwiseOr;
				case TokenType.MulAssign: return BinaryOperator.Multiplication;
				case TokenType.DivAssign: return BinaryOperator.Division;
				case TokenType.ModAssign: return BinaryOperator.Modulo;
				case TokenType.AmpAssign: return BinaryOperator.BitwiseAnd;
				case TokenType.CaretAssign: return BinaryOperator.BitwiseXor;
				case TokenType.ConcatAssign: return BinaryOperator.Concatenation;
				case TokenType.ShiftLeftAssign: return BinaryOperator.ShiftLeft;
				case TokenType.ShiftRightAssign: return BinaryOperator.ShiftRight;
				case TokenType.PowerAssign: return BinaryOperator.Exponentiation;
				case TokenType.HashAssign: return BinaryOperator.Hash;
				case TokenType.DollarAssign: return BinaryOperator.Dollar;
				default:
					throw new ArgumentException("Internal error: invalid compound assignment operator.", "type");
			}
		}

		private ParallelAssignment ParseParallelAssignment(ref int i, Expression expr, int startIndex)
		{
			var targets = new TempList<Expression>(4) { expr };
			while (Accept(ref i, TokenType.Comma))
			{
				expr = ParseNullCoalescingExpr(ref i); // ParseExpression(ref i) would gobble up assignments
				targets.Add(expr);

				// Assignability is tested for later; it's better to say "Cannot assign to ..."
				// *after* determining there's an equals sign somewhere, so we can emit the
				// error message "Expected '='" first.
			}

			if (Accept(i, TokenType.CompoundAssign))
				ParseError(i, "Compound assignment operators are not allowed in parallel assignment.");

			Expect(ref i, TokenType.Assign,
				"Expected '='. If parallel assignment was not intended, use ';' rather than ',' to separate statements.");

			foreach (var ex in targets)
				EnsureAssignable(ex);

			var values = new TempList<Expression>();
			do
			{
				values.Add(ParseExpression(ref i));
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.Semicolon);
			var end = tok[i - 2].EndIndex;

			if (values.Count != 1 && values.Count != targets.Count)
				ParseError(i - 1, "Wrong number of values in parallel assignment (expected 1 or {0}, got {1}).",
						targets.Count.ToStringInvariant(), values.Count.ToStringInvariant());

			return new ParallelAssignment(targets.Cast<AssignableExpression>().ToArray(), values.ToArray())
			{
				StartIndex = startIndex,
				EndIndex = end,
				Document = document,
			};
		}

		private Block ParseBlockOrExtern(ref int i)
		{
			if (AcceptExtension(i, "__extern"))
				return ParseExternBody(ref i);

			return ParseBlock(ref i);
		}

		private ExternBody ParseExternBody(ref int i)
		{
			if (Expect(i, TokenType.Identifier).Value != "__extern")
				throw new ParseException(tok[i], "Expected identifier '__extern', got " + tok[i].ToString());

			var start = tok[i++];

			StringLiteral entryPoint = null;
			Expect(ref i, TokenType.ParenOpen);

			var entryPointStr = Expect(ref i, TokenType.String);
			entryPoint = new StringLiteral((StringToken)entryPointStr);

			Expression locals = null, stack = null;
			while (Accept(ref i, TokenType.Comma))
			{
				var name = Expect(ref i, TokenType.Identifier);
				if (name.Value == "locals")
				{
					if (locals != null)
						throw new ParseException(name, "Duplicate 'locals' parameter");
					Expect(ref i, TokenType.Assign);
					locals = ParseExpression(ref i);
				}
				else if (name.Value == "stack")
				{
					if (stack != null)
						throw new ParseException(name, "Duplicate 'stack' parameter");
					Expect(ref i, TokenType.Assign);
					stack = ParseExpression(ref i);
				}
				else
					ParseError(name, "Unknown __extern parameter '{0}'; expected 'locals' or 'stack'.", name);
			}

			if (SimplifiedTree)
			{
				if (locals == null)
					locals = new ConstantExpression(ConstantValue.CreateInt(0));
				if (stack == null)
					stack = new ConstantExpression(ConstantValue.CreateInt(8));
			}

			Expect(ref i, TokenType.ParenClose);
			Expect(ref i, TokenType.Semicolon);

			return new ExternBody(entryPoint, locals, stack)
			{
				StartIndex = start.Index,
				EndIndex = start.EndIndex,
				Document = document,
			};
		}

		#endregion

		#region Expressions

		private Expression ParseExpression(ref int i)
		{
			// use-in expressions are NOT primary expressions!
			if (Accept(i, TokenType.Use))
				return ParseUseInExpression(ref i);

			var left = ParseConditionalExpr(ref i);
			if (Accept(i, TokenType.Assign))
			{
				var leftAssignable = EnsureAssignable(left);
				i++;

				var value = ParseExpression(ref i);
				return new AssignmentExpression(leftAssignable, value)
					.At(left.StartIndex, value.EndIndex, document);
			}
			return left;
		}

		private Expression ParseConditionalExpr(ref int i)
		{
			var left = ParseNullCoalescingExpr(ref i);
			if (Accept(i, TokenType.Question))
			{
				i++;
				var truePart = ParseExpression(ref i);
				Expect(ref i, TokenType.Colon);
				var falsePart = ParseExpression(ref i);
				return new ConditionalExpression(left, truePart, falsePart)
					.At(left.StartIndex, falsePart.EndIndex, document);
			}
			return left;
		}

		private Expression ParseNullCoalescingExpr(ref int i)
		{
			var left = ParseNullOrExpr(ref i);
			if (Accept(i, TokenType.NullCoalescing))
			{
				i++;
				var right = ParseNullCoalescingExpr(ref i);
				return new NullCoalescingExpression(left, right)
					.At(left.StartIndex, right.EndIndex, document);
			}
			return left;
		}

		private Expression ParseNullOrExpr(ref int i)
		{
			var left = ParseConditionalOrExpr(ref i);
			if (Accept(i, TokenType.NullOr))
			{
				i++;
				var right = ParseNullOrExpr(ref i);
				return new NullOrExpression(left, right)
					.At(left.StartIndex, right.EndIndex, document);
			}
			return left;
		}

		private Expression ParseConditionalOrExpr(ref int i)
		{
			var left = ParseConditionalXorExpr(ref i);
			while (Accept(ref i, TokenType.Or))
			{
				var right = ParseConditionalXorExpr(ref i);
				left = new ConditionalOrExpression(left, right)
					.At(left.StartIndex, right.EndIndex, document);
			}
			return left;
		}

		private Expression ParseConditionalXorExpr(ref int i)
		{
			var left = ParseConditionalAndExpr(ref i);
			while (Accept(ref i, TokenType.Xor))
			{
				var right = ParseConditionalAndExpr(ref i);
				left = new ConditionalXorExpression(left, right)
					.At(left.StartIndex, right.EndIndex, document);
			}
			return left;
		}

		private Expression ParseConditionalAndExpr(ref int i)
		{
			var left = ParseEqualityExpr(ref i);
			while (Accept(ref i, TokenType.And))
			{
				var right = ParseEqualityExpr(ref i);
				left = new ConditionalAndExpression(left, right)
					.At(left.StartIndex, right.EndIndex, document);
			}
			return left;
		}

		private Expression ParseEqualityExpr(ref int i)
		{
			var left = ParseRelationalExpr(ref i);
			while (Accept(i, TokenType.DoubleEqual, TokenType.NotEqual, TokenType.Refeq, TokenType.Is) ||
				Accept(i, TokenType.Not) && Accept(i + 1, TokenType.Refeq))
			{
				if (Accept(ref i, TokenType.Is))
				{
					bool negated = Accept(i, TokenType.Not);
					if (negated) i++;

					if (Accept(i, TokenType.Null)) // 'is [not] null'; special syntax and shiz
						return new TypeTestExpression(left, negated, null)
							.At(left.StartIndex, tok[i++].EndIndex, document);

					try
					{
						var type = ParseTypeName(ref i);
						return new TypeTestExpression(left, negated, type)
							.At(left.StartIndex, type.EndIndex, document);
					}
					catch (ParseException e)
					{
						throw e.Extend("'is'/'is not' must be followed by a type name or 'null'.");
					}
				}
				else if (Accept(i, TokenType.DoubleEqual, TokenType.NotEqual))
				{
					var op = tok[i].Type == TokenType.DoubleEqual ? BinaryOperator.Equality : BinaryOperator.Inequality;
					i++;
					var right = ParseRelationalExpr(ref i);
					left = GetBinaryOperatorExpression(left, right, op);
				}
				else // [not] refeq
				{
					bool negated = Accept(ref i, TokenType.Not); // skips 'not' if present
					i++; // skip 'refeq'
					var right = ParseRelationalExpr(ref i);
					left = new ReferenceTestExpression(left, negated, right)
						.At(left.StartIndex, right.EndIndex, document);
				}
			}
			return left;
		}

		private Expression ParseRelationalExpr(ref int i)
		{
			var left = ParseUnreservedExpr(ref i);
			while (Accept(i, relationalOps))
			{
				var op = tok[i].Type == TokenType.Less ? BinaryOperator.LessThan :
					tok[i].Type == TokenType.LessEqual ? BinaryOperator.LessEqual :
					tok[i].Type == TokenType.Greater ? BinaryOperator.GreaterThan :
					tok[i].Type == TokenType.GreaterEqual ? BinaryOperator.GreaterEqual :
					BinaryOperator.Comparison;
				i++;
				var right = ParseUnreservedExpr(ref i);
				left = GetBinaryOperatorExpression(left, right, op);
			}
			return left;
		}

		private Expression ParseUnreservedExpr(ref int i)
		{
			var left = ParseShiftExpr(ref i);
			while (Accept(i, TokenType.Hash, TokenType.Dollar))
			{
				var op = tok[i].Type == TokenType.Hash ? BinaryOperator.Hash : BinaryOperator.Dollar;
				i++;
				var right = ParseShiftExpr(ref i);
				left = GetBinaryOperatorExpression(left, right, op);
			}
			return left;
		}

		private Expression ParseShiftExpr(ref int i)
		{
			var left = ParseAdditiveExpr(ref i);
			while (Accept(i, TokenType.ShiftLeft, TokenType.ShiftRight))
			{
				var op = tok[i].Type == TokenType.ShiftLeft ? BinaryOperator.ShiftLeft : BinaryOperator.ShiftRight;
				i++;
				var right = ParseAdditiveExpr(ref i);
				left = GetBinaryOperatorExpression(left, right, op);
			}
			return left;
		}

		private Expression ParseAdditiveExpr(ref int i)
		{
			var left = ParseBitwiseXorExpr(ref i);
			while (Accept(i, TokenType.Plus, TokenType.Minus, TokenType.Pipe))
			{
				var op = tok[i].Type == TokenType.Plus ? BinaryOperator.Addition :
					tok[i].Type == TokenType.Minus ? BinaryOperator.Subtraction :
					BinaryOperator.BitwiseOr;
				i++;
				var right = ParseBitwiseXorExpr(ref i);
				left = GetBinaryOperatorExpression(left, right, op);
			}
			return left;
		}

		private Expression ParseBitwiseXorExpr(ref int i)
		{
			var left = ParseMultiplicativeExpr(ref i);
			while (Accept(ref i, TokenType.Caret))
			{
				var right = ParseMultiplicativeExpr(ref i);
				left = GetBinaryOperatorExpression(left, right, BinaryOperator.BitwiseXor);
			}
			return left;
		}

		private Expression ParseMultiplicativeExpr(ref int i)
		{
			var left = ParseConcatenationExpr(ref i);
			while (Accept(i, TokenType.Multiply, TokenType.Divide, TokenType.Mod, TokenType.Ampersand))
			{
				var op = tok[i].Type == TokenType.Multiply ? BinaryOperator.Multiplication :
					tok[i].Type == TokenType.Divide ? BinaryOperator.Division :
					tok[i].Type == TokenType.Mod ? BinaryOperator.Modulo :
					BinaryOperator.BitwiseAnd;
				i++;
				var right = ParseConcatenationExpr(ref i);
				left = GetBinaryOperatorExpression(left, right, op);
			}
			return left;
		}

		private Expression ParseConcatenationExpr(ref int i)
		{
			var left = ParseUnaryExpr(ref i);
			while (Accept(ref i, TokenType.Concatenation))
			{
				var right = ParseUnaryExpr(ref i);
				left = GetBinaryOperatorExpression(left, right, BinaryOperator.Concatenation);
			}
			return left;
		}

		private Expression ParseUnaryExpr(ref int i)
		{
			UnaryOperator op;
			var start = tok[i].Index;
			switch (tok[i].Type)
			{
				case TokenType.Plus: op = UnaryOperator.Plus; break;
				case TokenType.Minus: op = UnaryOperator.Minus; break;
				case TokenType.Tilde: op = UnaryOperator.BitwiseNot; break;
				case TokenType.Not: op = UnaryOperator.Not; break;
				default:
					return ParseExponentialExpr(ref i);
			}
			i++;
			var inner = ParseUnaryExpr(ref i);
			return GetUnaryOperatorExpression(inner, op, start);
		}

		private Expression ParseExponentialExpr(ref int i)
		{
			var left = ParseFunctionApplicationExpr(ref i);
			if (Accept(ref i, TokenType.Power))
			{
				var right = ParseExponentialExpr(ref i);
				left = GetBinaryOperatorExpression(left, right, BinaryOperator.Exponentiation);
			}
			return left;
		}

		private Expression ParseFunctionApplicationExpr(ref int i)
		{
			var left = ParsePrimaryExpr(ref i);
			if (Accept(ref i, TokenType.FuncApplication))
			{
				var right = ParseConcatenationExpr(ref i);
				left = GetBinaryOperatorExpression(left, right, BinaryOperator.FunctionApplication);
			}
			return left;
		}

		private Expression ParsePrimaryExpr(ref int i)
		{
			var left = ParseStandalonePrimaryExpr(ref i);
			while (Accept(i, primaryExprTokens))
			{
				var type = tok[i].Type;
				if (type == TokenType.Dot) // member access or expr.iter
				{
					i++;
					if (Accept(i, TokenType.Iter))
					{
						if (left is BaseAccess)
							ParseError(i, "'base' cannot be used in an iterator lookup expression.");
						left = new IteratorLookup(left).At(left.StartIndex, tok[i++].EndIndex, document);
					}
					else if (Accept(i, TokenType.Identifier))
						left = new MemberAccess(left, tok[i].Value).At(left.StartIndex, tok[i++].EndIndex, document);
					else
						ParseError(i, "Expected identifier or 'iter'.");
				}
				else if (type == TokenType.SafeAccess ||
					type == TokenType.ParenOpenSafe ||
					type == TokenType.SquareOpenSafe) // safe access
				{
					if (left is BaseAccess)
						ParseError(i, "'base' cannot be used in a safe access.");

					var access = new SafeAccess(left);
					ParseSafeAccessChain(ref i, access.Chain);
					access.At(left.StartIndex, tok[i - 1].EndIndex, document);
					left = access;
				}
				else if (type == TokenType.ParenOpen)
				{
					i++;
					bool hasRefArgs;
					var args = ParseArgumentList(ref i, out hasRefArgs);
					Expect(i, TokenType.ParenClose);
					if (left is MemberAccess)
						((MemberAccess)left).IsInvocation = true;
					left = new InvocationExpression(left, args, hasRefArgs)
						.At(left.StartIndex, tok[i++].EndIndex, document);
				}
				else // tok[i].Type == TokenType.SquareOpen
				{
					i++;

					bool _;
					var args = ParseArgumentList(ref i, out _, allowEmpty: false, allowRefs: false, closing: TokenType.SquareClose);

					Expect(i, TokenType.SquareClose);

					left = new IndexerAccess(left, args)
						.At(left.StartIndex, tok[i++].EndIndex, document);
				}
			}
			// If 'base' is followed by an assignment operator, let this error be caught
			// with the slightly better "Can't assign to 'base'" message.
			if (left is BaseAccess && !Accept(i, TokenType.Assign, TokenType.CompoundAssign))
				ParseError(i - 1, "'base' cannot be an expression on its own");
			return left;
		}

		private void ParseSafeAccessChain(ref int i, List<SafeNode> target)
		{
			while (Accept(i, TokenType.Dot, TokenType.ParenOpen, TokenType.SquareOpen,
				TokenType.SafeAccess, TokenType.ParenOpenSafe, TokenType.SquareOpenSafe))
			{
				var start = tok[i].Index;
				var type = tok[i++].Type;
				SafeNode safeNode;
				if (type == TokenType.Dot || type == TokenType.SafeAccess)
				{
					if (Accept(i, TokenType.Iter))
						safeNode = new SafeIteratorLookup(type == TokenType.SafeAccess);
					else
					{
						Expect(i, TokenType.Identifier);
						safeNode = new SafeMemberAccess(tok[i].Value, type == TokenType.SafeAccess);
					}
					i++; // Skip 'iter'/identifier
				}
				else if (type == TokenType.ParenOpen || type == TokenType.ParenOpenSafe)
				{
					bool _;
					var args = ParseArgumentList(ref i, out _);
					Expect(ref i, TokenType.ParenClose);
					safeNode = new SafeInvocation(args, type == TokenType.ParenOpenSafe);
				}
				else // type == TokenType.SquareOpen || type == TokenType.SquareOpenSafe
				{
					bool _;
					var args = ParseArgumentList(ref i, out _, allowEmpty: false, allowRefs: false, closing: TokenType.SquareClose);
					Expect(ref i, TokenType.SquareClose);
					safeNode = new SafeIndexerAccess(args, type == TokenType.SquareOpenSafe);
				}
				safeNode.StartIndex = start;
				safeNode.EndIndex = tok[i - 1].EndIndex;
				safeNode.Document = document;
				target.Add(safeNode);
			}
		}

		private Expression ParseStandalonePrimaryExpr(ref int i)
		{
			// A "standalone" primary is one of:
			//  - a parenthesised expression;
			//  - an identifier or literal;
			//  - __named_const(scope: name) if useExtensions;
			//  - 'this' or 'base';
			//  - global.identifier;
			//  - an object creation expression;
			//  - a list or hash creation expression;
			//  - a lambda expression;
			//  - typeof(expr)
			var start = tok[i].Index;
			if (Accept(i, TokenType.ParenOpen)) // Parenthesised expression
			{
				i++;
				var inner = ParseExpression(ref i);
				Expect(ref i, TokenType.ParenClose);

				return new ParenthesizedExpression(inner)
					.At(start, tok[i - 1].EndIndex, document);
			}
			if (Accept(i, TokenType.Literal))
			{
				var t = tok[i++];
				Expression result = null;
				switch (t.Type)
				{
					case TokenType.Null: result = new NullLiteral(); break;
					case TokenType.True:
					case TokenType.False: result = new BooleanLiteral(t.Value); break;
					case TokenType.Integer: result = new IntegerLiteral(t); break;
					case TokenType.Real: result = new RealLiteral(t); break;
					case TokenType.String: result = new StringLiteral((StringToken)t); break;
					case TokenType.Character: result = new CharacterLiteral((CharToken)t); break;
					default:
						ParseError(t, "Invalid/unknown literal type");
						break;
				}
				result.At(start, t.EndIndex, document);
				return result;
			}
			if (Accept(i, TokenType.Identifier))
			{
				var ident = (Identifier)tok[i];
				if (UseExtensions && !ident.Escaped)
				{
					if (ident.Value == "__named_const")
						return ParseNamedConstExpr(ref i);
					if (ident.Value == "__get_argc")
					{
						i++;
						return new GetArgumentCount() { StartIndex = start, EndIndex = ident.EndIndex, Document = document };
					}
				}

				i++;
				return new SimpleNameExpression(ident.Value)
					.At(start, ident.EndIndex, document);
			}

			if (Accept(i, TokenType.This))
				return new ThisAccess().At(start, tok[i++].EndIndex, document);
			if (Accept(i, TokenType.Base))
				return new BaseAccess().At(start, tok[i++].EndIndex, document);

			if (Accept(ref i, TokenType.Global))
			{
				Expect(ref i, TokenType.Dot);
				Expect(i, TokenType.Identifier);
				return new GlobalAccess(tok[i].Value).At(start, tok[i++].EndIndex, document);
			}
			if (Accept(i, TokenType.New))
				return ParseObjectCreationExpr(ref i);

			if (Accept(i, TokenType.SquareOpen))
				return ParseListCreationExpr(ref i);

			if (Accept(i, TokenType.CurlyOpen))
				return ParseHashCreationExpr(ref i);

			if (Accept(i, TokenType.At))
				return ParseLambaExpr(ref i);

			if (Accept(i, TokenType.Iter, TokenType.Typeof))
			{
				var isTypeof = tok[i].Type == TokenType.Typeof;
				i++;
				Expect(ref i, TokenType.ParenOpen);

				if (Accept(i, TokenType.ParenClose))
					throw new ParseException(tok[i], "Expected expression");

				var inner = ParseExpression(ref i);

				Expect(i, TokenType.ParenClose);

				var expr = isTypeof ? (Expression)new TypeofExpression(inner) : (Expression)new IteratorLookup(inner);
				expr.At(start, tok[i++].EndIndex, document);

				return expr;
			}

			if (Accept(i, TokenType.Async))
				ParseError(i, "The keyword 'async' is reserved for future use.");

			throw new ParseException(tok[i]);
		}

		private Expression ParseNamedConstExpr(ref int i)
		{
			var startIndex = tok[i].Index;

			if (Expect(i, TokenType.Identifier).Value != "__named_const")
				ParseError(i, "Expected identifier '__named_const'; got {0}.", tok[i]);
			i++;

			Expect(ref i, TokenType.ParenOpen);

			var scope = Expect(ref i, TokenType.Identifier).Value;
			Expect(ref i, TokenType.Colon);

			var name = Expect(ref i, TokenType.Identifier).Value;

			Expect(i, TokenType.ParenClose);

			return new NamedConstant(scope, name)
				.At(startIndex, tok[i++].EndIndex, document);
		}

		private Expression ParseObjectCreationExpr(ref int i)
		{
			var start = Expect(ref i, TokenType.New).Index;

			TypeName type;
			try { type = ParseTypeName(ref i); }
			catch (ParseException e) { throw e.Extend("Expected type name."); }

			bool hasRefArgs = false;
			Expression[] args;
			bool requireInitializer = false;
			if (Accept(ref i, TokenType.ParenOpen))
			{
				args = ParseArgumentList(ref i, out hasRefArgs);

				Expect(ref i, TokenType.ParenClose);
			}
			else
			{
				args = EmptyArrays.Expressions;
				requireInitializer = true;
			}

			ObjectInitializer initializer = null;
			if (Accept(i, TokenType.With))
				initializer = ParseObjectInitializer(ref i);
			else if (requireInitializer)
				ParseError(i, "An object initializer is required if there are no arguments to the constructor.");

			return new ObjectCreationExpression(type, args, hasRefArgs)
				{ Initializer = initializer }
				.At(start, tok[i - 1].EndIndex, document);
		}

		private ObjectInitializer ParseObjectInitializer(ref int i)
		{
			var start = Expect(ref i, TokenType.With).Index;
			Expect(ref i, TokenType.CurlyOpen);

			if (Accept(ref i, TokenType.CurlyClose))
				return new ObjectInitializer(EmptyArrays.MemberInitializers)
					{
						StartIndex = start,
						EndIndex = tok[i - 1].EndIndex,
						Document = document,
					};

			var members = new TempList<MemberInitializer>();
			var memberNames = new HashSet<string>();
			do
			{
				var nameTok = Expect(ref i, TokenType.Identifier);
				if (!memberNames.Add(nameTok.Value))
					throw new ParseException(nameTok,
						string.Format("There is already an initializer for the member '{0}'.", nameTok.Value));

				Expect(ref i, TokenType.Colon);

				var value = ParseExpression(ref i);

				members.Add(new MemberInitializer(nameTok.Value, value)
					{
						StartIndex = nameTok.Index,
						EndIndex = value.EndIndex,
						Document = document,
					});
			} while (Accept(ref i, TokenType.Comma) && !Accept(i, TokenType.CurlyClose));

			Expect(ref i, TokenType.CurlyClose);

			return new ObjectInitializer(members.ToArray())
				{
					StartIndex = start,
					EndIndex = tok[i - 1].EndIndex,
					Document = document,
				};
		}

		private Expression[] ParseArgumentList(ref int i, out bool hasRefs,
			bool allowEmpty = true, bool allowRefs = true,
			TokenType closing = TokenType.ParenClose)
		{
			hasRefs = false;

			if (Accept(i, closing)) // no expressions :(
			{
				if (!allowEmpty)
					ParseError(i, "At least one argument is needed.");
				return EmptyArrays.Expressions;
			}

			var args = new TempList<Expression>();
			do
			{
				if (allowRefs && Accept(i, TokenType.Ref))
				{
					var start = tok[i++].Index;
					var inner = ParsePrimaryExpr(ref i);
					args.Add(new RefExpression(inner)
					{
						StartIndex = start,
						EndIndex = inner.EndIndex,
						Document = document
					});
					hasRefs = true;
				}
				else
					args.Add(ParseExpression(ref i));
			} while (Accept(ref i, TokenType.Comma) && !Accept(i, closing));

			return args.ToArray();
		}

		private Expression ParseListCreationExpr(ref int i)
		{
			var start = Expect(ref i, TokenType.SquareOpen).Index;

			if (Accept(i, TokenType.SquareClose)) // empty list
				return new ListLiteralExpression(EmptyArrays.Expressions)
					.At(start, tok[i++].EndIndex, document);

			var items = new TempList<Expression>();
			// [,] is an illegal list in Osprey: trailing commas are allowed only if
			// there is at least one item.
			// At least one expression is needed now, even in a list comprehension;
			// [for i in [1 to 10]] is not allowed, and neither is [where true].
			items.Add(ParseExpression(ref i));

			if (AcceptContextual(ref i, "to")) // range expression, e.g. [1 to 10] or [something.length - x() to 12 + hi/mom];
			{
				items.Add(ParseExpression(ref i));

				Expression step = null;
				if (Accept(ref i, TokenType.Comma)) // step
					step = ParseExpression(ref i);

				Expect(i, TokenType.SquareClose);

				return new RangeExpression(items[0], items[1], step)
					.At(start, tok[i++].EndIndex, document);
			}

			// If there are commas not followed by ], there are more items in the list
			while (Accept(i, TokenType.Comma) && !Accept(i + 1, TokenType.SquareClose))
			{
				i++;
				items.Add(ParseExpression(ref i));
			}

			if (Accept(i, TokenType.For) || tok[i].Value == "where") // List comprehension
				return ParseListComprehension(ref i, start, ref items);

			Accept(ref i, TokenType.Comma); // trailing comma; if it were followed by anything but ], it would have been caught above

			Expect(i, TokenType.SquareClose);

			return new ListLiteralExpression(items.ToArray())
				.At(start, tok[i++].EndIndex, document);
		}

		private Expression ParseListComprehension(ref int i, int start, ref TempList<Expression> items)
		{
			var parts = new TempList<ListCompPart>(1);
			while (Accept(i, TokenType.For) || AcceptContextual(i, "where"))
			{
				if (Accept(ref i, TokenType.For))
				{
					if (!Accept(i, TokenType.Identifier))
						ParseError(i, "For clause without variables in list comprehension.");

					var vars = new TempList<string>(1);
					vars.Add(tok[i].Value);
					i++;

					while (Accept(ref i, TokenType.Comma))
					{
						Expect(i, TokenType.Identifier);
						vars.Add(tok[i].Value);
						i++;
					}

					Expect(ref i, TokenType.In);

					parts.Add(new ListCompIterator(vars.ToArray(), ParseExpression(ref i)));
				}
				else // where
				{
					i++;
					parts.Add(new ListCompCondition(ParseExpression(ref i)));
				}
			}

			if (Accept(i, TokenType.Comma))
				ParseError(i, "Trailing commas are not allowed in list comprehensions.");
			Expect(i, TokenType.SquareClose);

			return new ListComprehension(items.ToArray(), parts.ToArray())
				.At(start, tok[i++].EndIndex, document);
		}

		private Expression ParseHashCreationExpr(ref int i)
		{
			Expect(i, TokenType.CurlyOpen);
			var start = tok[i++].Index;

			if (Accept(i, TokenType.CurlyClose)) // empty hash
				return new HashLiteralExpression(EmptyArrays.HashMembers)
					.At(start, tok[i++].EndIndex, document);

			var members = new TempList<HashMember>();

			// {,} is not a valid hash: trailing commas are only allowed if there's at least one member
			do
			{
				Expression key = ParseExpression(ref i);
				Expect(ref i, TokenType.Colon);
				Expression value = ParseExpression(ref i);

				members.Add(new HashMember(key, value)
				{
					StartIndex = key.StartIndex,
					EndIndex = value.EndIndex,
					Document = this.document
				});
			} while (Accept(ref i, TokenType.Comma) && !Accept(i, TokenType.CurlyClose));

			Expect(i, TokenType.CurlyClose);

			return new HashLiteralExpression(members.ToArray())
				.At(start, tok[i++].EndIndex, document);
		}

		private Expression ParseLambaExpr(ref int i)
		{
			var start = Expect(ref i, TokenType.At).Index;

			if (Accept(i, TokenType.LambdaOperator))
			{
				// @op
				var lambdaOp = TokenTypeToLambdaOperator(tok[i].Type);
				return new LambdaOperatorExpression(lambdaOp)
					.At(start, tok[i++].EndIndex, document);
			}
			else if (Accept(i, TokenType.Dot, TokenType.SafeAccess))
			{
				// @.blah... or @?.blah...
				var lambda = new LambdaMemberExpression();
				ParseSafeAccessChain(ref i, lambda.SafeAccessChain);
				lambda.At(start, tok[i - 1].EndIndex, document);
				return lambda;
			}

			Parameter[] parameters = EmptyArrays.Parameters;
			var splat = Splat.None;
			if (Accept(i, TokenType.Identifier))
			{
				// @identifier
				parameters = new[] {
					new Parameter(tok[i].Value, null)
					{
						StartIndex = start,
						EndIndex = tok[i++].EndIndex,
						Document = document,
					}
				};
			}
			else if (Accept(i, TokenType.ParenOpen))
			{
				// @(parameter-list)
				i++;
				var paramInfo = ParseParameterList(ref i);

				Expect(ref i, TokenType.ParenClose);

				splat = paramInfo.Splat;
				parameters = paramInfo.Parameters;
			}

			Statement body = null;
			if (Accept(ref i, TokenType.Assign)) // single expression as body
			{
				var inner = ParseExpression(ref i);
				body = ExpressionToStatement(inner);
			}
			else if (Accept(i, TokenType.CurlyOpen)) // block
				body = ParseBlock(ref i);
			else
				ParseError(i, "Unexpected token {0}. Expected lambda expression body.", tok[i]);

			return new LambdaExpression(parameters, splat, body)
				.At(start, tok[i - 1].EndIndex, document);
		}

		private Expression ParseUseInExpression(ref int i)
		{
			var useTok = Expect(ref i, TokenType.Use);

			var variables = new TempList<VariableDeclarator>(1);
			do
			{
				var name = Expect(ref i, TokenType.Identifier);
				Expect(ref i, TokenType.Assign);
				var value = ParseExpression(ref i);

				variables.Add(new VariableDeclarator(name.Value, value)
				{
					StartIndex = name.Index,
					EndIndex = value.EndIndex,
					Document = document,
				});
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.In);

			var inner = ParseExpression(ref i);
			return new UseInExpression(variables.ToArray(), inner)
			{
				StartIndex = useTok.Index,
				EndIndex = useTok.EndIndex,
				Document = document,
			};
		}

		private LambdaOperator TokenTypeToLambdaOperator(TokenType type)
		{
			switch (type)
			{
				case TokenType.Plus: return LambdaOperator.Plus;
				case TokenType.Minus: return LambdaOperator.Minus;
				case TokenType.Pipe: return LambdaOperator.BitwiseOr;
				case TokenType.Caret: return LambdaOperator.BitwiseXor;
				case TokenType.Multiply: return LambdaOperator.Multiplication;
				case TokenType.Divide: return LambdaOperator.Division;
				case TokenType.Mod: return LambdaOperator.Modulo;
				case TokenType.Ampersand: return LambdaOperator.BitwiseAnd;
				case TokenType.Power: return LambdaOperator.Exponentiation;
				case TokenType.Hash: return LambdaOperator.Hash;
				case TokenType.Dollar: return LambdaOperator.Dollar;
				case TokenType.ShiftLeft: return LambdaOperator.ShiftLeft;
				case TokenType.ShiftRight: return LambdaOperator.ShiftRight;
				case TokenType.DoubleEqual: return LambdaOperator.Equality;
				case TokenType.NotEqual: return LambdaOperator.Inequality;
				case TokenType.Compare: return LambdaOperator.Comparison;
				case TokenType.Less: return LambdaOperator.Less;
				case TokenType.Greater: return LambdaOperator.Greater;
				case TokenType.LessEqual: return LambdaOperator.LessEquals;
				case TokenType.GreaterEqual: return LambdaOperator.GreaterEquals;
				case TokenType.Tilde: return LambdaOperator.BitwiseNot;
				case TokenType.FuncApplication: return LambdaOperator.FuncApplication;
				case TokenType.Concatenation: return LambdaOperator.Concatenation;
				case TokenType.Not: return LambdaOperator.Not;
				case TokenType.Or: return LambdaOperator.Or;
				case TokenType.Xor: return LambdaOperator.Xor;
				case TokenType.And: return LambdaOperator.And;
				default: throw new ArgumentOutOfRangeException("type");
			}
		}

		#endregion

		private const string CannotMixOptionalAndVariadicParams =
			"Cannot use optional parameters and a variadic parameter in the same parameter list.";
		private const string RequiredParamAfterOptional =
			"All required parameters must come before the optional parameters.";
		private const string CannotPassOptionalParamByRef =
			"An optional parameter cannot be passed by reference.";
		private const string CannotPassVariadicParamByRef =
			"A variadic parameter cannot be passed by reference.";
		private const string MoreThanOneVariadicParam =
			"There can only be one variadic parameter, and it must be at the very first or the very last parameter.";

		public static Document Parse(string input, ParseFlags flags)
		{
			if (input == null)
				throw new ArgumentNullException("input");

			var p = new Parser(input, flags);
			return p.ParseDocument();
		}
		public static Document Parse(SourceFile file, ParseFlags flags)
		{
			if (file == null)
				throw new ArgumentNullException("file");

			var p = new Parser(file, flags);
			return p.ParseDocument();
		}

		public static Expression ParseExpression(string input, ParseFlags flags)
		{
			if (input == null)
				throw new ArgumentNullException("input");

			var p = new Parser(input, flags);
			var i = 0;
			var e = p.ParseExpression(ref i);
			p.Expect(i, TokenType.EOF);
			return e;
		}

		internal static Dictionary<string, string> ParseMetadata(string input)
		{
			var output = new Dictionary<string, string>();

			var p = new Parser(input, ParseFlags.None);
			var i = 0;

			while (!p.Accept(i, TokenType.EOF))
			{
				string name;
				if (p.Accept(i, TokenType.Identifier, TokenType.Keyword))
					name = p.tok[i].Value;
				else if (p.Accept(i, TokenType.String))
					name = ((StringToken)p.tok[i]).LiteralValue;
				else
					throw new ParseException(p.tok[i], "Expected a field name, which must be an identifier or a string.");

				i++;
				p.Expect(ref i, TokenType.Assign);

				var value = ((StringToken)p.Expect(ref i, TokenType.String)).LiteralValue;

				p.Expect(ref i, TokenType.Semicolon);

				output.Add(name, value);
			}

			return output;
		}

		private static readonly TokenType[] relationalOps =
		{
			TokenType.Less, TokenType.LessEqual, TokenType.Greater, TokenType.GreaterEqual, TokenType.Compare
		};

		private static readonly TokenType[] primaryExprTokens =
		{
			TokenType.Dot, TokenType.ParenOpen, TokenType.SquareOpen, 
			TokenType.SafeAccess, TokenType.ParenOpenSafe, TokenType.SquareOpenSafe
		};

		/// <summary>
		/// Contains information about a parameter list.
		/// </summary>
		private struct ParameterInfo<T> where T : Parameter
		{
			public ParameterInfo(T[] parameters)
			{
				Parameters = parameters;
				Splat = Splat.None;
				HasOptionalParams = false;
				HasRefParams = false;
			}

			public T[] Parameters;
			public Splat Splat;
			public bool HasOptionalParams;
			public bool HasRefParams;
		}

		private struct MemberModifiers
		{
			public bool IsStatic, IsOverridable, IsInheritable, IsAbstract, IsOverride;
			public AccessLevel Access;

			/// <summary>
			/// Gets a value indicating whether the instance is empty; that is, no modifiers have been declared.
			/// </summary>
			public bool IsEmpty
			{
				get { return !(IsStatic || IsOverridable || IsInheritable || IsAbstract || IsOverride) && Access == AccessLevel.None; }
			}

			public void ValidateForClass(Token errorToken)
			{
				if (IsOverride || IsOverridable)
					throw new ParseException(errorToken, "A class cannot be marked as override or overridable.");
				if (IsStatic && (IsInheritable || IsAbstract))
					throw new ParseException(errorToken, "A static class cannot be inheritable or abstract.");
				if (IsAbstract && IsInheritable)
					throw new ParseException(errorToken, "A class cannot be marked both abstract and inheritable.");
				if (Access == AccessLevel.Protected)
					throw new ParseException(errorToken, "A class cannot be marked as protected.");

				if (Access == AccessLevel.None)
					Access = AccessLevel.Public;
			}
			public void ValidateForEnum(Token errorToken)
			{
				if (IsOverride || IsStatic || IsOverridable || IsInheritable || IsAbstract || Access == AccessLevel.Protected)
					throw new ParseException(errorToken,
						"An enum cannot be marked as override, static, overridable, inheritable, abstract or protected.");

				if (Access == AccessLevel.None)
					Access = AccessLevel.Public;
			}
			public void ValidateForMethodOrProperty(Token errorToken)
			{
				if (IsInheritable)
					throw new ParseException(errorToken, "A method, property or indexer cannot be marked as inheritable.");
				if (IsStatic && (IsAbstract || IsOverridable || IsOverride))
					throw new ParseException(errorToken,
						"A static method, property or indexer cannot be marked as abstract, overridable or override.");
				if (IsAbstract && IsOverridable)
					throw new ParseException(errorToken, "A method, property or indexer cannot be marked as both abstract and overridable.");

				if (Access == AccessLevel.Private && (IsAbstract || IsOverridable || IsOverride))
					throw new ParseException(errorToken,
						"A private method, property or indexer cannot be marked abstract, overridable or override.");
				// No declared accessibility => default to private, unless override, in which case inherit the accessibility.
				// We can't check what's being overridden here, but if there is no override specifier, we can still throw.
				// (Code at a later stage makes sure that there is a member to override, and inherits the accessibility correctly.)
				if (Access == AccessLevel.None && !IsOverride && (IsAbstract || IsOverridable))
					throw new ParseException(errorToken,
						"A method, property or indexer marked abstract or overridable cannot be private.");
			}
			public void ValidateForIndexer(Token errorToken)
			{
				if (IsStatic)
					throw new ParseException(errorToken, "An indexer cannot be static.");
				ValidateForMethodOrProperty(errorToken);
			}
			public void ValidateForField(Token errorToken)
			{
				if (IsAbstract || IsInheritable || IsOverridable || IsOverride)
					throw new ParseException(errorToken, "A field cannot be marked as abstract, inheritable, overridable or override.");
			}
			public void ValidateForConstructor(Token errorToken)
			{
				if (IsStatic)
				{
					if (IsOverridable || IsInheritable || IsAbstract || IsOverride ||
						Access != AccessLevel.None)
						throw new ParseException(errorToken,
							"A static constructor cannot have any modifiers except 'static'.");
				}
				else
				{
					if (IsAbstract || IsInheritable || IsOverridable || IsOverride)
						throw new ParseException(errorToken,
							"An instance constructor cannot be marked as abstract, inheritable, overridable or override.");
					if (Access == AccessLevel.None)
						Access = AccessLevel.Private;
				}
			}
			public void ValidateForGlobal(Token errorToken)
			{
				if (IsStatic || IsInheritable || IsOverridable || IsAbstract || IsOverride)
					throw new ParseException(errorToken,
						"Global functions and constants cannot be declared static, inheritable, overridable, abstract or override.");
				if (Access == AccessLevel.Protected)
					throw new ParseException(errorToken, "Global functions and constants cannot be declared protected.");
			}
		}
	}

	/// <summary>
	/// Configurations options that are passed to <see cref="Parser"/> when parsing documents.
	/// </summary>
	[Flags]
	public enum ParseFlags
	{
		/// <summary>
		/// Specifies no parse flags.
		/// </summary>
		None = 0,
		/// <summary>
		/// Specifies that the parser should use nonstandard extension keywords.
		/// </summary>
		UseExtensions = 1,
		/// <summary>
		/// Specifies that the parser should output a simplified parse tree.
		/// For a full description of the transformations performed when this
		/// flag is in effect, see parser.readme.txt.
		/// </summary>
		SimplifiedTree = 2,
		/// <summary>
		/// Specifies that the identifier tokens processed by the parser are to be normalized.
		/// Suitable for compilation.
		/// See <see cref="TokenFlags.NormalizeIdentifiers"/> for more details.
		/// </summary>
		NormalizeIdentifiers = 4,
		/// <summary>
		/// Specifies flags suitable for compilation.
		/// </summary>
		Compilation = SimplifiedTree | NormalizeIdentifiers,
	}
}