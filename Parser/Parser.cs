using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Nodes;

namespace Osprey
{
	public sealed class Parser
	{
		private Parser(string input, ParseFlags flags)
		{
			var tokFlags = (flags & ParseFlags.NormalizeIdentifiers) == ParseFlags.NormalizeIdentifiers ?
				TokenizerFlags.NormalizeIdentifiers : TokenizerFlags.None;
			this.tok = new Tokenizer(input, tokFlags);
			this.flags = flags;
		}

		private Tokenizer tok;
		private Document document;
		private ParseFlags flags;
		/// <summary>
		/// If true, the parser uses compiler-specific extensions.
		/// Otherwise, they're completely ignored.
		/// </summary>
		private bool UseExtensions
		{
			get { return (flags & ParseFlags.UseExtensions) != ParseFlags.None; }
		}

		private bool SimplifiedTree
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
		private bool Accept(int index, TokenType type)
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
		private bool Accept(int index, TokenType t1, TokenType t2)
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
		private bool Accept(int index, TokenType t1, TokenType t2, TokenType t3)
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
		private bool Accept(int index, TokenType t1, TokenType t2, TokenType t3, TokenType t4)
		{
			return tok[index].Match(t1) || tok[index].Match(t2) || tok[index].Match(t3) || tok[index].Match(t4);
		}
		/// <summary>
		/// Determines whether the token at the specified index is of a certain type.
		/// </summary>
		/// <param name="index">The index to test.</param>
		/// <param name="types">An array of types to test against.</param>
		/// <returns>true if the token at the specified index matches one of the types in <paramref name="types"/>; otherwise, false.</returns>
		private bool Accept(int index, params TokenType[] types)
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
		private bool Accept(ref int index, TokenType type)
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
		private bool Accept(ref int index, TokenType t1, TokenType t2)
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
		private bool Accept(ref int index, TokenType t1, TokenType t2, TokenType t3)
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
		private bool Accept(ref int index, TokenType t1, TokenType t2, TokenType t3, TokenType t4)
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
		private bool Accept(ref int index, params TokenType[] types)
		{
			for (var i = 0; i < types.Length; i++)
				if (tok[index].Match(types[i]))
				{
					index++;
					return true;
				}
			return false;
		}

		private bool AcceptExtension(int index, string keyword)
		{
			if (UseExtensions &&
				tok[index].Match(TokenType.Identifier) &&
				tok[index].Value == keyword)
				return true;
			return false;
		}

		private bool AcceptExtension(ref int index, string keyword)
		{
			if (AcceptExtension(index, keyword))
			{
				index++;
				return true;
			}
			return false;
		}

		#endregion

		#region "Expect" helpers

		/// <summary>
		/// Tests whether the token at the specified index is of the specified type. Otherwise, throws a <see cref="ParseException"/>.
		/// </summary>
		/// <param name="index">The index of the token to test.</param>
		/// <param name="type">The type to test against.</param>
		/// <returns>The token at the specified index.</returns>
		private Token Expect(int index, TokenType type)
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
		private Token Expect(int index, TokenType type, string message)
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
		private Token Expect(ref int index, TokenType type)
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
		private Token Expect(ref int index, TokenType type, string message)
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

		private void EnsureAssignable(Expression expr, Token t)
		{
			if (expr is ThisAccess)
				throw new ParseException(t, "Cannot assign to 'this'.");
			else if (expr is BaseAccess)
				throw new ParseException(t, "Cannot assign to 'base'.");
			else if (!(expr is SimpleNameExpression || expr is MemberAccess ||
				expr is IndexerAccess || expr is GlobalAccess))
				throw new ParseException(t, "Can only assign to variables, members and indexers.");
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
							var errorToken = new Token(tok.Source,
								tok.Source.Substring(left.StartIndex, right.EndIndex - left.StartIndex),
								TokenType.Invalid, left.StartIndex);
							throw new ParseException(errorToken,
								"The expression cannot be evaluated because it results in a division by zero.",
								e);
						}
						catch (OverflowException e)
						{
							var errorToken = new Token(tok.Source,
								tok.Source.Substring(left.StartIndex, right.EndIndex - left.StartIndex),
								TokenType.Invalid, left.StartIndex);
							throw new ParseException(errorToken,
								"The expression cannot be evaluated because it results in an arithmetic overflow.",
								e);
						}

						return new ConstantExpression(result)
						{
							StartIndex = left.StartIndex,
							EndIndex = right.EndIndex,
							Document = document,
						};
					}
				}
				// [a, b] :: [c, d] => [a, b, c, d]
				if (op == BinaryOperator.Concatenation &&
					leftInner is ListLiteralExpression && rightInner is ListLiteralExpression)
				{
					var result = new ListLiteralExpression(new List<Expression>())
					{
						StartIndex = left.StartIndex,
						EndIndex = right.EndIndex,
						Document = document,
					};
					result.Values.AddRange(((ListLiteralExpression)leftInner).Values);
					result.Values.AddRange(((ListLiteralExpression)rightInner).Values);
					return result;
				}

				// a != b   becomes   not (a == b)
				if (op == BinaryOperator.Inequality)
				{
					var inner = new BinaryOperatorExpression(left, right, BinaryOperator.Equality)
					{
						StartIndex = left.StartIndex,
						EndIndex = right.EndIndex,
						Document = document,
					};
					return new UnaryExpression(inner, UnaryOperator.Not)
					{
						StartIndex = inner.StartIndex,
						EndIndex = inner.EndIndex,
						Document = document,
					};
				}
			}

			return new BinaryOperatorExpression(left, right, op)
			{
				StartIndex = left.StartIndex,
				EndIndex = right.EndIndex,
				Document = document,
			};
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
							var errorToken = new Token(tok.Source,
								tok.Source.Substring(start, operand.EndIndex - start),
								TokenType.Invalid, start);
							throw new ParseException(errorToken,
								"The expression cannot be evaluated because it results in an arithmetic overflow.",
								e);
						}

						return new ConstantExpression(result)
						{
							StartIndex = start,
							EndIndex = operand.EndIndex,
							Document = document,
						};
					}
				}
			}

			return new UnaryExpression(operand, op)
			{
				StartIndex = start,
				EndIndex = operand.EndIndex,
				Document = document,
			};
		}

		#region Declarations

		private Document ParseDocument()
		{
			document = new Document();

			var i = 0;

			if (Accept(i, TokenType.Identifier) && tok[i].Value == "version")
				document.Version = ParseVersion(ref i);

			// parse all use directives first
			while (Accept(ref i, TokenType.Use))
				document.Uses.Add(ParseUse(ref i));

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
					throw new ParseException(tok[i], "Expected ';' or namespace body.");
			}

			while (!Accept(i, TokenType.EOF, TokenType.CurlyClose))
				ParseNamespaceMembers(ref i, fileNs, document.Statements);

			Expect(i, TokenType.EOF);

			return document;
		}

		private Version ParseVersion(ref int i)
		{
			Expect(ref i, TokenType.Identifier); // "version"

			int major = 0, minor = 0, build = 0, revision = 0;

			// Keep in mind: integer literals cannot be negative!

			Func<Token, int> tokenToField = (tok) =>
			{
				var value = IntegerLiteral.ParseToken(tok);
				if (value.Type != ConstantValueType.Int)
					throw new ParseException(tok, "Version number field must be of type Int.");
				if (value.IntValue > int.MaxValue)
					throw new ParseException(tok, "Version number field out of range.");

				return unchecked((int)value.IntValue);
			};

			major = tokenToField(Expect(ref i, TokenType.Integer));

			Expect(ref i, TokenType.Colon);

			minor = tokenToField(Expect(ref i, TokenType.Integer));

			if (Accept(ref i, TokenType.Colon))
				build = tokenToField(Expect(ref i, TokenType.Integer));

			if (Accept(ref i, TokenType.Colon))
				revision = tokenToField(Expect(ref i, TokenType.Integer));

			Expect(ref i, TokenType.Semicolon);

			return new Version(major, minor, build, revision);
		}

		private UseDirective ParseUse(ref int i)
		{
			int start = tok[i - 1].Index, end = tok[i - 1].EndIndex;

			if (Accept(i, TokenType.String)) // use "<script name>";
			{
				var token = tok[i];
				i++;
				Expect(ref i, TokenType.Semicolon);
				return new UseScriptDirective(new StringLiteral((StringToken)token))
				{
					StartIndex = start,
					EndIndex = end,
					Document = document,
				};
			}
			else if (Accept(ref i, TokenType.Namespace)) // use namespace foo.bar.baz;
			{
				var name = ParseQualifiedName(ref i);
				Expect(ref i, TokenType.Semicolon);
				return new UseNamespaceDirective(name)
				{
					StartIndex = start,
					EndIndex = end,
					Document = document,
				};
			}
			else if (Accept(i, TokenType.Identifier)) // use foo.bar.baz;
			{
				var name = ParseQualifiedName(ref i);
				Expect(ref i, TokenType.Semicolon);
				return new UseModuleDirective(name)
				{
					StartIndex = start,
					EndIndex = end,
					Document = document,
				};
			}
			else
				throw new ParseException(tok[i], "Expected identifier, string or 'namespace'.");
		}

		private MemberModifiers ParseModifiers(ref int i)
		{
			MemberModifiers output = new MemberModifiers();

			while (Accept(i, TokenType.MemberModifier))
			{
				if (Accept(i, TokenType.Public, TokenType.Protected, TokenType.Private))
				{
					if (output.Access != AccessLevel.None)
						throw new ParseException(tok[i], "More than one access level modifier.");
					output.Access = tok[i].Type == TokenType.Public ? AccessLevel.Public :
						tok[i].Type == TokenType.Protected ? AccessLevel.Protected :
						AccessLevel.Private;
				}
				if (Accept(i, TokenType.Static))
				{
					if (output.IsStatic)
						throw new ParseException(tok[i], "Duplicate static modifier.");
					output.IsStatic = true;
				}
				if (Accept(i, TokenType.Abstract))
				{
					if (output.IsAbstract)
						throw new ParseException(tok[i], "Duplicate abstract modifier.");
					output.IsAbstract = true;
				}
				if (Accept(i, TokenType.Inheritable))
				{
					if (output.IsInheritable)
						throw new ParseException(tok[i], "Duplicate inheritable modifier.");
					output.IsInheritable = true;
				}
				if (Accept(i, TokenType.Overridable))
				{
					if (output.IsOverridable)
						throw new ParseException(tok[i], "Duplicate overridable modifier.");
					output.IsOverridable = true;
				}
				if (Accept(i, TokenType.Override))
				{
					if (output.IsOverride)
						throw new ParseException(tok[i], "Duplicate override modifier.");
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
					var @class = ParseClass(ref i, modifiers);
					@class.DocString = startTok.Documentation;
					target.Types.Add(@class);
				}
				else if (Accept(i, TokenType.Enum)) // enum declaration
				{
					modifiers.ValidateForEnum(tok[i]);
					var @enum = ParseEnum(ref i, modifiers);
					@enum.DocString = startTok.Documentation;
					target.Types.Add(@enum);
				}
				else if (Accept(i, TokenType.Function)) // global function
				{
					modifiers.ValidateForGlobal(tok[i]);
					var func = ParseLocalFunctionDeclaration(ref i);
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

					if (!(decl is SimpleLocalVariableDeclaration) || !((SimpleLocalVariableDeclaration)decl).IsConst)
						throw new ParseException(decl, tok.Source,
							"Internal error: global constant declaration should be a simple declaration and marked IsConst.");

					var simpleDecl = (SimpleLocalVariableDeclaration)decl;

					var constDecl = new GlobalConstantDeclaration(modifiers.Access == AccessLevel.Public, simpleDecl)
						{
							Document = document
						};
					if (simpleDecl.Declarators.Count == 1)
						constDecl.DocString = startTok.Documentation;

					target.Constants.Add(constDecl);
				}
				else if (Accept(i, TokenType.Namespace)) // nomspace
				{
					if (!modifiers.IsEmpty)
						throw new ParseException(tok[i], "Namespaces do not accept any modifiers.");

					i++;
					var name = ParseQualifiedName(ref i);

					var ns = new NamespaceDeclaration(name) { Document = document };
					if (Accept(i, TokenType.Semicolon))
						throw new ParseException(tok[i], "The file namespace declaration is only allowed at the top of the script, after any use declarations.");
					else if (!Accept(i, TokenType.CurlyOpen))
						throw new ParseException(tok[i], "Expected namespace body.");

					Expect(ref i, TokenType.CurlyOpen);

					ParseNamespaceMembers(ref i, ns, null);

					Expect(ref i, TokenType.CurlyClose);

					target.Namespaces.Add(ns);
				}
				else if (!modifiers.IsEmpty)
					throw new ParseException(tok[i], "Expected class, function or constant declaration.");
				else if (stmtList == null)
					throw new ParseException(tok[i], "Expected class, function, constant or namespace declaration.");
				else
				{
					var statement = ParseStatement(ref i);
					if (statement is SimpleLocalVariableDeclaration)
						((SimpleLocalVariableDeclaration)statement).IsGlobal = true;
					else if (statement is ParallelLocalVariableDeclaration)
						((ParallelLocalVariableDeclaration)statement).IsGlobal = true;
					stmtList.Add(statement);
				}
			}
		}

		private ClassDeclaration ParseClass(ref int i, MemberModifiers modifiers)
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
					throw new ParseException(name, "Primitive classes cannot be marked inheritable, abstract or static.");
			}

			Expect(ref i, TokenType.CurlyOpen);

			if (AcceptExtension(ref i, "__init_type"))
			{
				// Type initializer! Must be the very first thing in the class.
				Expect(ref i, TokenType.ParenOpen);

				var initerToken = Expect(ref i, TokenType.String);
				output.Initializer = ((StringToken)initerToken).RealValue;

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
					var newTok = tok[i++];
					modifiers.ValidateForConstructor(newTok);
					Expect(ref i, TokenType.ParenOpen);

					Splat splat;
					var parameters = ParseConstructorParameters(ref i, out splat);

					Expect(ref i, TokenType.ParenClose);

					if (modifiers.IsStatic)
					{
						if (parameters.Count > 0)
							throw new ParseException(parameters[0], tok.Source, "Static constructors cannot declare any parameters.");

						// Note: ParseConstructorBody permits an optional 'new base(...);' as
						// the first statement, so we can't use that here.
						var body = ParseBlockOrExtern(ref i);
						target.StaticConstructor = new ConstructorDeclaration(parameters, body)
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
						BaseInitializer baseInit = null;
						if (Accept(i, TokenType.Semicolon))
							body = new Block()
							{
								StartIndex = tok[i].Index,
								EndIndex = tok[i++].EndIndex,
								Document = document,
							};
						else
							// Permits 'new base(...);' as first statement
							body = ParseConstructorBody(ref i, out baseInit);

						target.Constructors.Add(new ConstructorDeclaration(modifiers.Access, parameters, splat, body)
						{
							BaseInitializer = baseInit,
							StartIndex = newTok.Index,
							EndIndex = newTok.EndIndex,
							Document = document,
							DocString = startTok.Documentation,
						});
					}
				}
				else if (Accept(i, TokenType.Const, TokenType.Var)) // field (maybe even a constant one)
				{
					var token = tok[i++];
					modifiers.ValidateForField(token);

					var isConst = token.Type == TokenType.Const;
					if (isConst && modifiers.IsStatic)
						throw new ParseException(token, "A constant cannot be marked static.");
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
					var method = ParseMethod(ref i, modifiers);
					method.DocString = startTok.Documentation;
					target.Methods.Add(method);
				}
				else if (Accept(i, TokenType.Get, TokenType.Set)) // property accessor
				{
					var isSetter = tok[i++].Type == TokenType.Set;

					PropertyAccessorDeclaration decl;
					if (Accept(i, TokenType.This))
						decl = ParseIndexer(ref i, isSetter, modifiers);
					else
						decl = ParseProperty(ref i, isSetter, modifiers);

					decl.DocString = startTok.Documentation;
					target.Properties.Add(decl);
				}
				else if (Accept(i, TokenType.Iter)) // iter declaration
				{
					if (!modifiers.IsEmpty)
						throw new ParseException(tok[i], "Iterator declarations cannot have any modifiers.");
					else if (target.Iterator != null)
						throw new ParseException(tok[i], "A class can only declare one iterator.");
					else if (Accept(i + 1, TokenType.ParenOpen))
						throw new ParseException(tok[i + 1], "Iterators do not take any parameters.");
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
							throw new ParseException(tok[i], "This member cannot be declared static.");
						var method = ParseMethod(ref i, modifiers);
						method.DocString = startTok.Documentation;
						target.Methods.Add(method);
					}
					else
					{
						Expect(i, TokenType.Identifier);
						// If the next token is a ParenOpen, it's a method.
						if (Accept(i + 1, TokenType.ParenOpen))
						{
							var method = ParseMethod(ref i, modifiers);
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
				else if (Accept(i, TokenType.Identifier))
				{
					// At this point, we could only ever be dealing with a method declared
					// without an access level modifier and function keyword, e.g.:
					// class Test {
					//     foo(a, b) { }
					// }
					var method = ParseMethod(ref i, modifiers);
					method.DocString = startTok.Documentation;
					target.Methods.Add(method);
				}
				else
					throw new ParseException(tok[i], "Invalid token in class body.");
			} // while

			if (SimplifiedTree && !target.IsStatic && target.Constructors.Count == 0)
			{
				// Add the default constructor, which is public and parameterless
				target.Constructors.Add(new ConstructorDeclaration(AccessLevel.Public,
					new List<ConstructorParam>(), Splat.None, new Block()) { Document = document });
			}
		}

		private List<ConstructorParam> ParseConstructorParameters(ref int i, out Splat splat)
		{
			var output = new List<ConstructorParam>();
			splat = Splat.None;

			if (Accept(i, TokenType.ParenClose))
				return output;

			if (Accept(ref i, TokenType.Splat))
				splat = Splat.Beginning;

			var optionalSeen = false;
			do
			{
				var thisPrefix = Accept(ref i, TokenType.This);

				Token nameTok;
				if (thisPrefix) // this.<name> parameter
				{
					Expect(ref i, TokenType.Dot);
					nameTok = Expect(ref i, TokenType.Identifier);
				}
				else if (Accept(i, TokenType.Identifier))
					nameTok = tok[i++];
				else
					throw new ParseException(tok[i], "Expected identifier or 'this'.");

				if (Accept(i, TokenType.Assign)) // optional parameter
				{
					if (splat != Splat.None)
						throw new ParseException(tok[i], "Optional parameters cannot be combined with a splat.");
					i++;
					var value = ParseExpression(ref i);
					// As in ParseParameterList, the default value expression is validated later.
					output.Add(new ConstructorParam(nameTok.Value, thisPrefix, value)
					{
						StartIndex = nameTok.Index,
						EndIndex = nameTok.EndIndex,
						Document = document,
					});
					optionalSeen = true;
				}
				else if (optionalSeen)
					throw new ParseException(tok[i - 1], "Required parameters must come before optional parameters.");
				else
					output.Add(new ConstructorParam(nameTok.Value, thisPrefix, null)
					{
						StartIndex = nameTok.Index,
						EndIndex = nameTok.EndIndex,
						Document = document,
					});
			} while (Accept(ref i, TokenType.Comma));

			if (Accept(i, TokenType.Splat))
			{
				if (optionalSeen)
					throw new ParseException(tok[i], "Splats cannot be combined with optional parameters.");
				else if (splat != Splat.None)
					throw new ParseException(tok[i], "There can only be one splat per parameter list, and it must be at the very beginning or the very end of the list.");
				i++;
				splat = Splat.End;
			}

			return output;
		}

		private Block ParseConstructorBody(ref int i, out BaseInitializer baseInit)
		{
			baseInit = null;

			if (AcceptExtension(i, "__extern"))
				return ParseExternBody(ref i);

			// Block
			var start = Expect(ref i, TokenType.CurlyOpen).Index;

			var body = new List<Statement>();
			if (!Accept(i, TokenType.CurlyClose))
			{
				var first = ParseStatement(ref i, allowBaseInitializer: true);
				if (first is BaseInitializer)
					baseInit = (BaseInitializer)first;
				body.Add(first);
			}

			while (!Accept(i, TokenType.CurlyClose))
				body.Add(ParseStatement(ref i));

			Expect(i, TokenType.CurlyClose);

			return new Block(body)
			{
				StartIndex = start,
				EndIndex = tok[i++].EndIndex,
				Document = document,
			};
		}

		private FieldDeclaration ParseFieldDeclaration(ref int i, bool isConst, AccessLevel access, bool isStatic)
		{
			if (Accept(i, TokenType.ParenOpen))
				throw new ParseException(tok[i], "Parallel declaration is not allowed for fields or constants.");

			var vars = new List<VariableDeclarator>();
			do
			{
				Expect(i, TokenType.Identifier);
				var nameTok = tok[i++];

				Expression value = null;

				if (Accept(ref i, TokenType.Assign))
					value = ParseExpression(ref i);
				else if (isConst)
					throw new ParseException(tok[i], "Constant declaration without value.");

				vars.Add(new VariableDeclarator(nameTok.Value, value)
				{
					StartIndex = nameTok.Index,
					EndIndex = nameTok.EndIndex,
					Document = document,
				});
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.Semicolon);

			return new FieldDeclaration(access, isConst, vars)
			{
				IsStatic = isStatic,
				Document = document,
			};
		}

		private MethodDeclaration ParseMethod(ref int i, MemberModifiers modifiers)
		{
			if (!Accept(i, TokenType.Identifier, TokenType.This))
				throw new ParseException(tok[i], "Expected identifier or 'this'.");

			var nameTok = tok[i++]; // this name may also be 'this', but that's okay
			modifiers.ValidateForMethodOrProperty(nameTok);

			Expect(ref i, TokenType.ParenOpen);
			var parameters = ParseParameterList(ref i);
			Expect(ref i, TokenType.ParenClose);

			Statement body;
			if (Accept(i, TokenType.Semicolon))
				body = new EmptyStatement(tok[i].Index, tok[i++].EndIndex) { Document = document };
			else if (Accept(i, TokenType.CurlyOpen))
				body = ParseBlock(ref i);
			else if (AcceptExtension(i, "__extern"))
				body = ParseExternBody(ref i);
			else
				throw new ParseException(tok[i], "Expected block or ';'.");

			if (modifiers.IsAbstract && !(body is EmptyStatement))
				throw new ParseException(nameTok, "Abstract methods cannot have a body.");
			
			if (!modifiers.IsAbstract && body is EmptyStatement)
				throw new ParseException(nameTok, "Non-abstract methods cannot have empty bodies.");

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

		private PropertyAccessorDeclaration ParseProperty(ref int i, bool isSetter, MemberModifiers modifiers)
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
				throw new ParseException(nameTok, "Abstract property accessors cannot have a body.");

			if (!modifiers.IsAbstract && body is EmptyStatement)
				throw new ParseException(nameTok, "Non-abstract property accessors cannot have empty bodies.");

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

		private IndexerAccessorDeclaration ParseIndexer(ref int i, bool isSetter, MemberModifiers modifiers)
		{
			var startTok = Expect(ref i, TokenType.This);
			modifiers.ValidateForIndexer(startTok);
			Expect(ref i, TokenType.SquareOpen);

			var parameters = ParseParameterList(ref i, null, false, TokenType.SquareClose);
			if (parameters.HasOptionalParams || parameters.Splat != Splat.None)
				throw new ParseException(startTok, "Indexers may not have optional parameters or splats ('...').");

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

			if (modifiers.IsAbstract && !(body is EmptyStatement))
				throw new ParseException(startTok, "Abstract indexer accessors cannot have a body.");

			if (!modifiers.IsAbstract && body is EmptyStatement)
				throw new ParseException(startTok, "Non-abstract indexer accessors cannot have empty bodies.");

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

			int expectedCount;
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
				throw new ParseException(tok[i], "Expected overloadable operator.");

			var opTokenIndex = i;
			var op = tok[i].Type;
			var end = tok[i++].EndIndex;

			Expect(ref i, TokenType.ParenOpen); // opening parenthesis no matter what

			var parameters = ParseParameterList(ref i, null, false);
			if (parameters.Splat != Splat.None)
				throw new ParseException(tok[opTokenIndex], "Operator overloads cannot have any '...' terms in the parameter list.");
			if (parameters.HasOptionalParams)
				throw new ParseException(tok[opTokenIndex], "Operator overloads cannot have any optional parameters.");

			Expect(ref i, TokenType.ParenClose); // ahem

			var body = ParseBlockOrExtern(ref i);

			if (parameters.Parameters.Count == 1)
			{
				if (expectedCount == 2)
					throw new ParseException(tok[opTokenIndex], "This is a binary operator; two parameters are required.");
				UnaryOperator uop = op == TokenType.Plus ? UnaryOperator.Plus :
					op == TokenType.Minus ? UnaryOperator.Minus :
					UnaryOperator.BitwiseNot;
				return new UnaryOperatorOverload(uop, parameters.Parameters[0].Name, body)
					{
						StartIndex = start,
						EndIndex = end,
						Document = document,
					};
			}
			else if (parameters.Parameters.Count == 2)
			{
				if (expectedCount == 1)
					throw new ParseException(tok[opTokenIndex], "This is a unary operator; one parameter is required.");
				return new BinaryOperatorOverload(TokenTypeToOperator(op), parameters.Parameters[0].Name,
					parameters.Parameters[1].Name, body)
					{
						StartIndex = start,
						EndIndex = end,
						Document = document,
					};
			}
			else
				throw new ParseException(tok[opTokenIndex], "Wrong number of parameters for operator overload (expected " +
					(expectedCount == 1 ? "1" : expectedCount == 2 ? "2" : "1 or 2") + ").");
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

		private EnumDeclaration ParseEnum(ref int i, MemberModifiers modifiers)
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
						throw new ParseException(tok[i], "Too many auto-enumerated values in enum; the counter has overflowed.");
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

			var retStmt = new ReturnStatement(new List<Expression>(1))
			{
				StartIndex = expr.StartIndex,
				EndIndex = expr.EndIndex,
				Document = expr.Document,
			};
			retStmt.ReturnValues.Add(expr);
			var result = new Block(new List<Statement>(1))
			{
				StartIndex = expr.StartIndex,
				EndIndex = expr.EndIndex,
				Document = expr.Document,
			};
			result.Statements.Add(retStmt);
			return result;
		}

		#endregion

		private TypeName ParseTypeName(ref int i)
		{
			var start = tok[i].Index;
			var global = false;
			if (Accept(ref i, TokenType.Global))
			{
				global = true;
				Expect(ref i, TokenType.Dot); // always 'global' '.' identifier
			}

			Expect(i, TokenType.Identifier);

			var idents = new List<string> { tok[i++].Value };
			while (Accept(ref i, TokenType.Dot))
			{
				Expect(i, TokenType.Identifier);
				idents.Add(tok[i++].Value);
			}
			return new TypeName(idents, global) { StartIndex = start, EndIndex = tok[i - 1].EndIndex, Document = document };
		}

		private QualifiedName ParseQualifiedName(ref int i)
		{
			Expect(i, TokenType.Identifier);

			var start = tok[i].Index;

			var idents = new List<string> { tok[i++].Value };
			while (Accept(i, TokenType.Dot))
			{
				i++;
				Expect(i, TokenType.Identifier);
				idents.Add(tok[i].Value);
				i++;
			}
			return new QualifiedName(idents) { StartIndex = start, EndIndex = tok[i - 1].EndIndex, Document = document };
		}

		private ParameterInfo ParseParameterList(ref int i, List<Parameter> parameters = null,
			bool allowEmpty = true, TokenType end = TokenType.ParenClose)
		{
			if (parameters == null)
				parameters = new List<Parameter>();

			var output = new ParameterInfo(parameters);

			if (Accept(i, end))
			{
				if (!allowEmpty)
					throw new ParseException(tok[i], "At least one parameter is needed");
				// Don't skip closing bracket
				return output;
			}

			if (Accept(ref i, TokenType.Splat))
				output.Splat = Splat.Beginning;

			var optionalSeen = false;
			do
			{
				Expect(i, TokenType.Identifier);
				var name = tok[i].Value;
				var startParam = tok[i++].Index;

				if (Accept(i, TokenType.Assign)) // optional parameter/argument
				{
					if (output.Splat != Splat.None)
						throw new ParseException(tok[i], "Optional parameters cannot be combined with a splat.");
					i++;
					// Technically, literals are expressions. And besides, using
					// ParseExpression() lets me generate better error messages.
					var value = ParseExpression(ref i);
					// The expression is validated at a later stage of parsing,
					// since it needs to be a constant expression.
					parameters.Add(new Parameter(name, value)
					{
						StartIndex = startParam,
						EndIndex = value.EndIndex,
						Document = document
					});
					output.HasOptionalParams = optionalSeen = true;
				}
				else if (optionalSeen)
					throw new ParseException(tok[i - 1], "All required parameters must come before optional parameters.");
				else
					parameters.Add(new Parameter(name, null)
					{
						StartIndex = startParam,
						EndIndex = tok[i - 1].EndIndex,
						Document = document,
					});
			} while (Accept(ref i, TokenType.Comma));

			if (Accept(i, TokenType.Splat))
			{
				if (optionalSeen)
					throw new ParseException(tok[i], "Splats cannot be combined with optional parameters.");
				else if (output.Splat != Splat.None)
					throw new ParseException(tok[i], "There can only be one splat per parameter list, and it must be at the very beginning or the very end of the list.");
				output.Splat = Splat.End;
				i++;
			}

			return output;
		}

		#region Statements

		private Statement ParseStatement(ref int i, bool allowBaseInitializer = false)
		{
			var stmt = ParseStatementInner(ref i);
			if (!allowBaseInitializer && stmt is BaseInitializer)
				throw new ParseException(stmt, tok.Source, "A base initializer is only allowed as the first statment in a constructor.");
			return stmt;
		}

		private Statement ParseStatementInner(ref int i)
		{
			if (Accept(i, TokenType.Var, TokenType.Const))
				return ParseLocalVariableDeclaration(ref i);
			if (Accept(i, TokenType.Function))
				return ParseLocalFunctionDeclaration(ref i);
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
			if (Accept(i, TokenType.New) && Accept(i + 1, TokenType.Base))
				return ParseBaseInitializer(ref i);
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
				throw new ParseException(tok[i], "Expected for, while, or do–while loop after label.");

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
					throw new ParseException(tok[i], "Parallel declaration is not allowed for constants.");

				i++;

				var names = new List<string>();
				do
				{
					names.Add(Expect(ref i, TokenType.Identifier).Value);
				} while (Accept(ref i, TokenType.Comma));

				Expect(ref i, TokenType.ParenClose);
				Expect(ref i, TokenType.Assign, "Parallel declaration without initializer.");

				var value = ParseExpression(ref i);

				Expect(ref i, TokenType.Semicolon);

				return new ParallelLocalVariableDeclaration(names, value)
					{
						StartIndex = start,
						EndIndex = end,
						Document = document,
					};
			}

			var vars = new List<VariableDeclarator>();
			do
			{
				var name = Expect(ref i, TokenType.Identifier);

				Expression value = null;

				if (Accept(ref i, TokenType.Assign))
					value = ParseExpression(ref i);
				else if (isConst)
					throw new ParseException(tok[i], "Constant declaration without value.");

				vars.Add(new VariableDeclarator(name.Value, value)
				{
					StartIndex = name.Index,
					EndIndex = name.EndIndex,
					Document = document,
				});
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.Semicolon);

			return new SimpleLocalVariableDeclaration(isConst, vars) { StartIndex = start, EndIndex = end, Document = document };
		}

		private LocalFunctionDeclaration ParseLocalFunctionDeclaration(ref int i)
		{
			Expect(ref i, TokenType.Function);

			var nameTok = Expect(ref i, TokenType.Identifier);

			Expect(ref i, TokenType.ParenOpen);

			var pi = ParseParameterList(ref i);

			var end = Expect(ref i, TokenType.ParenClose).EndIndex;

			var body = ParseBlockOrExtern(ref i);

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

			var statements = new List<Statement>();

			while (!Accept(i, TokenType.CurlyClose))
			{
				statements.Add(ParseStatement(ref i));
			}

			Expect(ref i, TokenType.CurlyClose);

			return new Block(statements)
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
					throw new ParseException(stmt, tok.Source, "Embedded statement cannot be a declaration.");
				if (!SimplifiedTree)
					return new EmbeddedStatement(stmt) { Document = document };

				var statements = new List<Statement>();
				statements.Add(stmt);
				return new Block(statements)
				{
					StartIndex = stmt.StartIndex,
					EndIndex = stmt.EndIndex,
					Document = document,
				};
			}
			if (!Accept(i, TokenType.CurlyOpen))
				throw new ParseException(tok[i], "Expected ': <statement>' or block.");

			return ParseBlock(ref i);
		}

		private ElseClause ParseElseClause(ref int i)
		{
			Expect(i, TokenType.Else);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			if (Accept(i, TokenType.CurlyOpen))
				return new ElseClause(ParseBlock(ref i)) { Document = document };

			Accept(ref i, TokenType.Colon); // Optional ':'

			var body = ParseStatement(ref i);
			if (body is LocalDeclaration)
				throw new ParseException(body, tok.Source, "Embedded statement cannot be a declaration.");
			// Force the embedded statement into a block
			if (SimplifiedTree)
			{
				var statements = new List<Statement>(1);
				statements.Add(body);
				body = new Block(statements)
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

			var catches = new List<CatchClause>();
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

					catches.Add(new CatchClause(ParseBlock(ref i)) { StartIndex = catchStart, EndIndex = catchEnd, Document = document });
					genericCatchSeen = true;
				}
				else
					throw new ParseException(tok[i], "Expected type name or block.");
			}

			FinallyClause fin = null;
			if (Accept(i, TokenType.Finally))
			{
				int finStart = tok[i].Index, finEnd = tok[i++].EndIndex;
				fin = new FinallyClause(ParseBlock(ref i)) { StartIndex = finStart, EndIndex = finEnd, Document = document };
			}

			if (catches.Count == 0 && fin == null)
				throw new ParseException(startTok, "A try statement must have at least one catch or finally clause.");

			return new TryStatement(body, catches, fin)
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
				throw new ParseException(tok[i], "Internal error: expected 'return' or 'yield'");

			int start = tok[i].Index, end = tok[i++].EndIndex;

			if (Accept(i, TokenType.Semicolon))
				if (isYield)
					throw new ParseException(tok[i], "Empty yield statement is not allowed. Use 'return;' to stop a generator.");
				else
				{
					i++;
					return new ReturnStatement() { StartIndex = start, EndIndex = end, Document = document };
				}

			var values = new List<Expression>();
			do
			{
				try { values.Add(ParseExpression(ref i)); }
				catch (ParseException e) { throw e.Extend("Expected expression."); }
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.Semicolon);

			if (values.Count > 1 && SimplifiedTree)
			{
				var list = new ListLiteralExpression(values)
				{
					StartIndex = values[0].StartIndex,
					EndIndex = values[values.Count - 1].EndIndex,
					Document = document,
				};
				values = new List<Expression> { list };
			}

			if (isYield)
				return new YieldStatement(values) { StartIndex = start, EndIndex = end, Document = document };
			else
				return new ReturnStatement(values) { StartIndex = start, EndIndex = end, Document = document };
		}

		private Statement ParseLoopFlowStatement(ref int i)
		{
			var isBreak = Accept(i, TokenType.Break);

			if (!Accept(i, TokenType.Break, TokenType.Next))
				throw new ParseException(tok[i], "Internal error: expected 'break' or 'next'.");

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

			if (Accept(ref i, TokenType.Semicolon))
				return new ThrowStatement(null) { StartIndex = start, EndIndex = end, Document = document };

			var value = ParseExpression(ref i);

			Expect(ref i, TokenType.Semicolon);

			return new ThrowStatement(value) { StartIndex = start, EndIndex = end, Document = document };
		}

		private Statement ParseForStatement(ref int i, string label = null)
		{
			Expect(i, TokenType.For);
			int start = tok[i].Index, end = tok[i++].EndIndex;

			var vars = new List<string>();
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

			return new ForStatement(label, vars, expr, body, @else)
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
			catch (ParseException e) { throw new ParseException(e.Token, "Expected expression.", e); }

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

		private Statement ParseBaseInitializer(ref int i)
		{
			Expect(i, TokenType.New);
			Expect(i + 1, TokenType.Base);

			int start = tok[i].Index, end = tok[i + 1].EndIndex;
			i += 2;
			Expect(ref i, TokenType.ParenOpen);

			var args = ParseArgumentList(ref i);

			Expect(ref i, TokenType.ParenClose);
			Expect(ref i, TokenType.Semicolon);

			return new BaseInitializer(args) { StartIndex = start, EndIndex = end, Document = document };
		}

		private Statement ParseExpressionStatement(ref int i)
		{
			var start = tok[i].Index;
			var expr = ParseExpression(ref i);

			if (expr is AssignmentExpression)
			{
				Expect(ref i, TokenType.Semicolon);
				((AssignmentExpression)expr).IgnoreValue = true;
				return new ExpressionStatement(expr) { StartIndex = start, EndIndex = expr.EndIndex, Document = document };
			}

			if (Accept(i, TokenType.CompoundAssign))
			{
				EnsureAssignable(expr, tok[i]);

				var op = GetCompoundAssignmentOp(tok[i].Type);
				i++;

				Expression value;
				try { value = ParseExpression(ref i); }
				catch (ParseException e) { throw e.Extend("Expected expression."); }

				Expect(ref i, TokenType.Semicolon);

				return new CompoundAssignment(expr, value, op) { StartIndex = start, EndIndex = value.EndIndex, Document = document };
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
				throw new ParseException(expr, tok.Source,
					"Only invocation, function application, assignment and object creation can be used as a statement.");

			return new ExpressionStatement(expr) { StartIndex = start, EndIndex = end, Document = document };
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
			var targets = new List<Expression> { expr };
			while (Accept(ref i, TokenType.Comma))
			{
				expr = ParseNullCoalescingExpr(ref i); // ParseExpression(ref i) would gobble up assignments
				targets.Add(expr);

				// EnsureAssignable(expr, null);
				// tested later; it's better to say "Cannot assign to ..." /after/
				// determining there's an equals sign somewhere, so we can emit the
				// error message "Expected '='" first.
			}

			if (Accept(i, TokenType.CompoundAssign))
				throw new ParseException(tok[i], "Compound assignment operators are not allowed in parallel assignment.");

			Expect(ref i, TokenType.Assign, "Expected '='. If parallel assignment was not intended, use ';' rather than ',' to separate statements.");

			foreach (var ex in targets)
				EnsureAssignable(ex, null);

			List<Expression> values = new List<Expression>();
			do
			{
				values.Add(ParseExpression(ref i));
			} while (Accept(ref i, TokenType.Comma));

			Expect(ref i, TokenType.Semicolon);
			var end = tok[i - 2].EndIndex;

			if (values.Count != 1 && values.Count != targets.Count)
				throw new ParseException(tok[i - 1],
					string.Format("Wrong number of values in parallel assignment (expected {0}, got {1})",
					targets.Count, values.Count));

			return new ParallelAssignment(targets, values) { StartIndex = startIndex, EndIndex = end, Document = document };
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
					throw new ParseException(name, string.Format("Unknown __extern parameter '{0}'; expected 'locals' or 'stack'.", name));
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
			var left = ParseConditionalExpr(ref i);
			if (Accept(i, TokenType.Assign))
			{
				EnsureAssignable(left, tok[i]);
				i++;

				var value = ParseExpression(ref i);
				return new AssignmentExpression(left, value)
					{
						StartIndex = left.StartIndex,
						EndIndex = value.EndIndex,
						Document = document,
					};
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
					{
						StartIndex = left.StartIndex,
						EndIndex = falsePart.EndIndex,
						Document = document,
					};
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
					{
						StartIndex = left.StartIndex,
						EndIndex = right.EndIndex,
						Document = document,
					};
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
					{
						StartIndex = left.StartIndex,
						EndIndex = right.EndIndex,
						Document = document,
					};
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
				{
					StartIndex = left.StartIndex, // The start index never changes
					EndIndex = right.EndIndex,
					Document = document,
				};
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
				{
					StartIndex = left.StartIndex,
					EndIndex = right.EndIndex,
					Document = document,
				};
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
				{
					StartIndex = left.StartIndex,
					EndIndex = right.EndIndex,
					Document = document,
				};
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
					{
						return new TypeTestExpression(left, negated, null)
							{
								StartIndex = left.StartIndex,
								EndIndex = tok[i++].EndIndex,
								Document = document,
							};
					}

					try
					{
						var type = ParseTypeName(ref i);
						return new TypeTestExpression(left, negated, type)
							{
								StartIndex = left.StartIndex,
								EndIndex = type.EndIndex,
								Document = document,
							};
					}
					catch (ParseException e)
					{
						throw new ParseException(tok[i], "'is'/'is not' must be followed by a type name or 'null'.", e);
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
					{
						StartIndex = left.StartIndex,
						EndIndex = right.EndIndex,
						Document = document,
					};
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
				var right = ParseUnaryExpr(ref i);
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
			while (Accept(i, TokenType.Dot, TokenType.ParenOpen, TokenType.SquareOpen, 
				TokenType.SafeAccess, TokenType.ParenOpenSafe, TokenType.SquareOpenSafe))
			{
				var type = tok[i].Type;
				if (type == TokenType.Dot) // member access or expr.iter
				{
					i++;
					if (Accept(i, TokenType.Iter))
						left = new IteratorLookup(left)
						{
							StartIndex = left.StartIndex,
							EndIndex = tok[i++].EndIndex,
							Document = document,
						};
					else if (Accept(i, TokenType.Identifier))
						left = new MemberAccess(left, tok[i].Value)
						{
							StartIndex = left.StartIndex,
							EndIndex = tok[i++].EndIndex,
							Document = document,
						};
					else
						throw new ParseException(tok[i], "Expected identifier or 'iter'.");
				}
				else if (type == TokenType.SafeAccess ||
					type == TokenType.ParenOpenSafe ||
					type == TokenType.SquareOpenSafe) // safe access
				{
					var access = new SafeAccess(left) { StartIndex = left.StartIndex };
					ParseSafeAccessChain(ref i, access.Chain);
					access.EndIndex = tok[i - 1].EndIndex;
					access.Document = document;
					left = access;
				}
				else if (type == TokenType.ParenOpen)
				{
					i++;
					var args = ParseArgumentList(ref i);
					Expect(i, TokenType.ParenClose);
					if (left is MemberAccess)
						((MemberAccess)left).IsInvocation = true;
					left = new InvocationExpression(left, args)
					{
						StartIndex = left.StartIndex,
						EndIndex = tok[i++].EndIndex,
						Document = document,
					};
				}
				else // tok[i].Type == TokenType.SquareOpen
				{
					if (left is BaseAccess)
						throw new ParseException(tok[i], "'base' cannot be indexed into.");
					i++;

					var args = ParseArgumentList(ref i, false, TokenType.SquareClose);

					Expect(i, TokenType.SquareClose);

					left = new IndexerAccess(left, args)
					{
						StartIndex = left.StartIndex,
						EndIndex = tok[i++].EndIndex,
						Document = document,
					};
				}
			}
			// If 'base' is followed by an assignment operator, let this error be caught
			// with the slightly better "Can't assign to 'base'" message.
			if (left is BaseAccess && !Accept(i, TokenType.Assign, TokenType.CompoundAssign))
				throw new ParseException(tok[i - 1], "'base' cannot be an expression on its own");
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
					var args = ParseArgumentList(ref i);
					Expect(ref i, TokenType.ParenClose);
					safeNode = new SafeInvocation(args, type == TokenType.ParenOpenSafe);
				}
				else // type == TokenType.SquareOpen || type == TokenType.SquareOpenSafe
				{
					var args = ParseArgumentList(ref i, false, TokenType.SquareClose);
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
					{
						StartIndex = start,
						EndIndex = tok[i - 1].EndIndex,
						Document = document,
					};
			}
			if (Accept(i, TokenType.Literal))
			{
				var t = tok[i++];
				Expression result;
				switch (t.Type)
				{
					case TokenType.Null: result = new NullLiteral(); break;
					case TokenType.True:
					case TokenType.False: result = new BooleanLiteral(t.Value); break;
					case TokenType.Integer: result = new IntegerLiteral(t); break;
					case TokenType.Real: result = new RealLiteral(t); break;
					case TokenType.String: result = new StringLiteral((StringToken)t); break;
					case TokenType.Regex: result = new RegexLiteral(t.Value); break;
					default:
						throw new ParseException(t, "Invalid/unknown literal type");
				}
				result.StartIndex = start;
				result.EndIndex = t.EndIndex;
				result.Document = document;
				return result;
			}
			if (Accept(i, TokenType.Identifier))
			{
				if (UseExtensions)
				{
					if (tok[i].Value == "__named_const")
						return ParseNamedConstExpr(ref i);
					if (tok[i].Value == "__get_argc")
						return new GetArgumentCount() { StartIndex = start, EndIndex = tok[i++].EndIndex, Document = document };
				}

				return new SimpleNameExpression(tok[i].Value)
				{
					StartIndex = start,
					EndIndex = tok[i++].EndIndex,
					Document = document,
				};
			}

			if (Accept(i, TokenType.This))
				return new ThisAccess() { StartIndex = start, EndIndex = tok[i++].EndIndex, Document = document };
			if (Accept(i, TokenType.Base))
				return new BaseAccess() { StartIndex = start, EndIndex = tok[i++].EndIndex, Document = document };

			if (Accept(ref i, TokenType.Global))
			{
				Expect(ref i, TokenType.Dot);
				Expect(i, TokenType.Identifier);
				return new GlobalAccess(tok[i].Value) { StartIndex = start, EndIndex = tok[i++].EndIndex, Document = document };
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
				expr.StartIndex = start;
				expr.EndIndex = tok[i++].EndIndex;
				expr.Document = document;

				return expr;
			}
			if (Accept(i, TokenType.Async))
				throw new ParseException(tok[i], "The keyword 'async' is reserved for future use.");
			if (Accept(i, TokenType.Ref))
				throw new ParseException(tok[i], "The keyword 'ref' is reserved for future use.");

			throw new ParseException(tok[i]);
		}

		private Expression ParseNamedConstExpr(ref int i)
		{
			var startIndex = tok[i].Index;

			if (Expect(i, TokenType.Identifier).Value != "__named_const")
				throw new ParseException(tok[i], "Expected identifier '__named_const'; got " + tok[i].ToString());
			i++;

			Expect(ref i, TokenType.ParenOpen);

			var scope = Expect(ref i, TokenType.Identifier).Value;
			Expect(ref i, TokenType.Colon);

			var name = Expect(ref i, TokenType.Identifier).Value;

			Expect(i, TokenType.ParenClose);

			return new NamedConstant(scope, name)
			{
				StartIndex = startIndex,
				EndIndex = tok[i++].EndIndex,
				Document = document,
			};
		}

		private Expression ParseObjectCreationExpr(ref int i)
		{
			var start = Expect(ref i, TokenType.New).Index;

			TypeName type;
			try { type = ParseTypeName(ref i); }
			catch (ParseException e) { throw new ParseException(e.Token, "Expected type name", e); }

			Expect(ref i, TokenType.ParenOpen);

			var args = ParseArgumentList(ref i);

			Expect(i, TokenType.ParenClose);

			return new ObjectCreationExpression(type, args)
				{
					StartIndex = start,
					EndIndex = tok[i++].EndIndex,
					Document = document,
				};
		}

		private List<Expression> ParseArgumentList(ref int i, bool allowEmpty = true, TokenType closing = TokenType.ParenClose)
		{
			var args = new List<Expression>();

			if (Accept(i, closing)) // no expressions :(
			{
				if (allowEmpty)
					return args;
				else
					throw new ParseException(tok[i], "Expected at least one expression.");
			}

			do
			{
				args.Add(ParseExpression(ref i));
			} while (Accept(ref i, TokenType.Comma) && !Accept(i, closing));

			return args;
		}

		private Expression ParseListCreationExpr(ref int i)
		{
			Expect(i, TokenType.SquareOpen);
			var start = tok[i++].Index;

			var items = new List<Expression>();

			if (Accept(i, TokenType.SquareClose)) // empty list
				return new ListLiteralExpression(items)
					{
						StartIndex = start,
						EndIndex = tok[i++].EndIndex,
						Document = document,
					};
			
			// [,] is an illegal list in Osprey: trailing commas are allowed only if
			// there is at least one item.
			// At least one expression is needed now, even in a list comprehension;
			// [for i in [1 to 10]] is not allowed, and neither is [yield for x in [1 to 10]].
			items.Add(ParseExpression(ref i));

			if (tok[i].Value == "to") // range expression, e.g. [1 to 10] or [something.length - x() to 12 + hi/mom];
			{
				i++;
				items.Add(ParseExpression(ref i));

				Expression step = null;

				if (Accept(ref i, TokenType.Comma)) // step
					step = ParseExpression(ref i);

				Expect(i, TokenType.SquareClose);

				return new RangeExpression(items[0], items[1], step)
					{
						StartIndex = start,
						EndIndex = tok[i++].EndIndex,
						Document = document,
					};
			}

			// If there are commas not followed by ], there are more items in the list
			while (Accept(i, TokenType.Comma) && !Accept(i + 1, TokenType.SquareClose))
			{
				i++;
				items.Add(ParseExpression(ref i));
			}

			if (Accept(i, TokenType.For)) // List comprehension
			{
				var parts = new List<ListCompPart>();
				while (Accept(i, TokenType.For) || tok[i].Value == "where")
				{
					if (Accept(ref i, TokenType.For))
					{
						if (!Accept(i, TokenType.Identifier))
							throw new ParseException(tok[i], "For clause without variables in list comprehension.");

						List<string> vars = new List<string>();
						vars.Add(tok[i].Value);
						i++;

						while (Accept(ref i, TokenType.Comma))
						{
							Expect(i, TokenType.Identifier);
							vars.Add(tok[i].Value);
							i++;
						}

						Expect(ref i, TokenType.In);

						parts.Add(new ListCompIterator(vars, ParseExpression(ref i)));
					}
					else // where
					{
						i++;
						parts.Add(new ListCompCondition(ParseExpression(ref i)));
					}
				}

				if (Accept(i, TokenType.Comma))
					throw new ParseException(tok[i], "Trailing commas are not allowed in list comprehensions.");
				Expect(i, TokenType.SquareClose);

				return new ListComprehension(items, parts)
					{
						StartIndex = start,
						EndIndex = tok[i++].EndIndex,
						Document = document,
					};
			}

			Accept(ref i, TokenType.Comma); // trailing comma; if it were followed by anything but ], it would have been caught above

			Expect(i, TokenType.SquareClose);

			return new ListLiteralExpression(items)
				{
					StartIndex = start,
					EndIndex = tok[i++].EndIndex,
					Document = document,
				};
		}

		private Expression ParseHashCreationExpr(ref int i)
		{
			Expect(i, TokenType.CurlyOpen);
			var start = tok[i++].Index;

			List<HashMember> members = new List<HashMember>();

			if (Accept(i, TokenType.CurlyClose)) // empty hash
				return new HashLiteralExpression(members) { StartIndex = start, EndIndex = tok[i++].EndIndex, Document = document };

			// {,} is not a valid hash: trailing commas are only allowed if there's at least one member
			do
			{
				Expression key;
				if (Accept(i, TokenType.Identifier))
				{
					if (SimplifiedTree)
						key = new ConstantExpression(ConstantValue.CreateString(tok[i].Value));
					else
						key = new SimpleNameExpression(tok[i].Value);

					key.StartIndex = tok[i].Index;
				}
				else if (Accept(i, TokenType.String))
					key = new StringLiteral((StringToken)tok[i])
					{
						StartIndex = tok[i].Index,
					};
				else if (Accept(i, TokenType.Integer))
					key = new IntegerLiteral(tok[i])
					{
						StartIndex = tok[i].Index,
					};
				else if (Accept(i, TokenType.ParenOpen)) // parenthesised expression as key
				{
					var startParen = tok[i++].Index;
					key = new ParenthesizedExpression(ParseExpression(ref i));

					Expect(i, TokenType.ParenClose);

					key.StartIndex = startParen;
				}
				else
					throw new ParseException(tok[i], "The hash key must be an identifier, string, integer or parenthesized expression.");

				key.EndIndex = tok[i++].EndIndex;
				key.Document = document;

				Expect(ref i, TokenType.Colon);

				members.Add(new HashMember(key, ParseExpression(ref i)));
			} while (Accept(i++, TokenType.Comma) && !Accept(i /*+ 1*/, TokenType.CurlyClose));
			i--; // the condition above skips a token otherwise

			Accept(ref i, TokenType.Comma); // trailing comma

			Expect(i, TokenType.CurlyClose);

			return new HashLiteralExpression(members)
				{
					StartIndex = start,
					EndIndex = tok[i++].EndIndex,
					Document = document,
				};
		}

		private Expression ParseLambaExpr(ref int i)
		{
			var start = Expect(ref i, TokenType.At).Index;

			if (Accept(i, TokenType.LambdaOperator))
			{
				var lambdaOp = TokenTypeToLambdaOperator(tok[i].Type);
				return new LambdaOperatorExpression(lambdaOp)
					{
						StartIndex = start,
						EndIndex = tok[i++].EndIndex,
						Document = document,
					};
			}
			else if (Accept(i, TokenType.Dot, TokenType.SafeAccess))
			{
				var lambda = new LambdaMemberExpression() { StartIndex = start };
				ParseSafeAccessChain(ref i, lambda.SafeAccessChain);
				lambda.EndIndex = tok[i - 1].EndIndex;
				lambda.Document = document;
				return lambda;
			}
			var parameters = new List<Parameter>();
			var splat = Splat.None;
			if (Accept(i, TokenType.Identifier)) // single parameter
			{
				parameters.Add(new Parameter(tok[i].Value, null)
				{
					StartIndex = start,
					EndIndex = tok[i++].EndIndex,
					Document = document,
				});
			}
			else if (Accept(i, TokenType.ParenOpen))
			{
				i++;
				ParameterInfo paramInfo = ParseParameterList(ref i, parameters);

				Expect(ref i, TokenType.ParenClose);

				splat = paramInfo.Splat;
			}

			Statement body;
			if (Accept(ref i, TokenType.Assign)) // single expression as body
			{
				var inner = ParseExpression(ref i);
				body = ExpressionToStatement(inner);
			}
			else if (Accept(i, TokenType.CurlyOpen)) // block
				body = ParseBlock(ref i);
			else
				throw new ParseException(tok[i], "Unexpected token " + tok[i] + ". Expected lambda expression body.");

			return new LambdaExpression(parameters, splat, body)
				{
					StartIndex = start,
					EndIndex = tok[i - 1].EndIndex,
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

		public static Document Parse(string input, ParseFlags flags)
		{
			if (input == null)
				throw new ArgumentNullException("input");

			var p = new Parser(input, flags);
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
					name = ((StringToken)p.tok[i]).RealValue;
				else
					throw new ParseException(p.tok[i], "Expected a field name, which must be an identifier or a string.");

				i++;
				p.Expect(ref i, TokenType.Assign);

				var value = ((StringToken)p.Expect(ref i, TokenType.String)).RealValue;

				p.Expect(ref i, TokenType.Semicolon);

				output.Add(name, value);
			}

			return output;
		}

		private static readonly TokenType[] relationalOps =
		{
			TokenType.Less, TokenType.LessEqual, TokenType.Greater, TokenType.GreaterEqual, TokenType.Compare
		};

		/// <summary>
		/// Contains information about a parameter list.
		/// </summary>
		private struct ParameterInfo
		{
			public ParameterInfo(List<Parameter> parameters)
			{
				Parameters = parameters;
				Splat = Splat.None;
				HasOptionalParams = false;
			}
			public List<Parameter> Parameters;
			public Splat Splat;
			public bool HasOptionalParams;
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