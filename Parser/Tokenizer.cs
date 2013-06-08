using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using CI = System.Globalization.CultureInfo;

namespace Osprey
{
	/// <summary>
	/// Tokenizes an Osprey source file.
	/// </summary>
	public class Tokenizer : IEnumerable<Token>
	{
		/// <summary>
		/// Initializes a new instance of <see cref="Tokenizer"/>.
		/// </summary>
		/// <param name="input">The string to tokenize.</param>
		/// <param name="includeComments">Determines whether to include comments; set to false when parsing.</param>
		/// <exception cref="ArgumentNullException"><paramref name="input"/> is null.</exception>
		public Tokenizer(string input, TokenizerFlags flags)
		{
			if (input == null)
				throw new ArgumentNullException("input");

			this.flags = flags;
			this.source = input;
		}

		private List<Token> tokens = new List<Token>();

		private TokenizerFlags flags;

		private string source;
		/// <summary>
		/// Gets the source string; that is, the string that's being tokenized.
		/// </summary>
		public string Source { get { return source; } }

		/// <summary>
		/// Gets a value indicating whether the tokenizer includes comments.
		/// </summary>
		public bool IncludeComments { get { return (flags & TokenizerFlags.IncludeComments) != TokenizerFlags.None; } }

		/// <summary>
		/// Gets a value indicating whether the tokenizer normalizes identifiers.
		/// </summary>
		public bool NormalizeIdentifiers { get { return (flags & TokenizerFlags.NormalizeIdentifiers) != TokenizerFlags.None; } }

		private int sourceIndex = 0;
		/// <summary>
		/// If the end of the file has been reached, this field is set to a Token with type <see cref="TokenType.EOF"/>.
		/// </summary>
		private Token eof = null;

		private string lastDocComment = null;

		/// <summary>
		/// Gets the token at a specified index.
		/// </summary>
		/// <param name="index">The index of the token to get.</param>
		/// <returns>The token at the specified index.</returns>
		/// <exception cref="ArgumentOutOfRangeException"><paramref name="index"/> is less than 0.
		/// -or-
		/// <paramref name="index"/> is greater than or equal to the number of tokens in the source string.</exception>
		public Token this[int index]
		{
			get
			{
				// Note: EnsureMinIndex updates 'tokens' and, if it returns false, 'eof'. Mmm, side-effective.
				if (index < 0)
					throw new ArgumentOutOfRangeException("index");
				else if (EnsureMinIndex(index))
					return tokens[index];
				else
					return eof;
			}
		}

		/// <summary>
		/// Ensures that the tokenizer contains at least <c><paramref name="index"/> + 1</c> elements.
		/// </summary>
		/// <param name="index">The minimum index to reach.</param>
		/// <returns>true if the input string has enough tokens to reach <paramref name="index"/>; otherwise, false.</returns>
		private bool EnsureMinIndex(int index)
		{
			if (eof != null && index >= tokens.Count)
				return false; // we've already determined that there are no more tokens to read

			while (tokens.Count < index + 1)
				if (!MoveNext())
					return false;

			return true;
		}

		#region Token scanner

		private bool MoveNext()
		{
			Token tok;
			do
			{
				tok = ScanToken(ref sourceIndex);
			} while (!IncludeComments && tok.Type == TokenType.Comment);
			if (lastDocComment != null)
			{
				tok.documentation = lastDocComment;
				lastDocComment = null;
			}
			tokens.Add(tok);
			return tok.Type != TokenType.EOF;
		}

		private Token ScanToken(ref int i)
		{
			SkipWhitespace(ref i);

			if (IsEOF(i))
				return eof = new Token(source, null, TokenType.EOF, i);

			var ch = source[i];
			if (ch == '/' && !IsEOF(i + 1) && (source[i + 1] == '/' || source[i + 1] == '*'))
				return ScanComment(ref i);

			if (ch >= '0' && ch <= '9' || // 123
				(ch == '.' && !IsEOF(i + 1) && source[i + 1] >= '0' && source[i + 1] <= '9')) // .123
				// Numeric literals, even hexadecimal ones, always begin with a digit.
				// However, a real literal can also begin with a dot, e.g. ".125".
				return ScanNumber(ref i);

			if (ch == '"')
				return ScanRegularString(ref i);

			if (ch == 'r' && IsChar(i + 1, '"'))
				return ScanVerbatimString(ref i);

			//if (ch == '\'')
			//	return ScanRegularExpression(ref i);

			if (ch == '_' || char.IsLetter(source, i) ||
				char.GetUnicodeCategory(source, i) == UnicodeCategory.LetterNumber)
				return ScanIdentifier(ref i);

			return ScanPunctuation(ref i);
		}

		private Token ScanComment(ref int i)
		{
			var startIndex = i;
			var isMultiline = source[i + 1] == '*';
			i += 2; // comments always start with // or /*

			var isDocComment = isMultiline ? IsChar(i, '*') : IsChar(i, '/');

			if (!isMultiline)
			{
				do
				{
					while (!IsEOF(i) && !IsNewline(source[i]))
						i += GetCharSize(i);
					// if we're at EOF or newline, we've found the end of the comment!
					// ... however, if this is a documentation comment, we need to look
					// for the next line.
					if (isDocComment)
					{
						var k = i;
						SkipWhitespace(ref k);
						if (IsEOF(k + 2) || source[k] != '/' || source[k + 1] != '/' || source[k + 2] != '/')
							break;
						// If we fall through here, then k + 2 is not past the end, and there is
						// a substring "///" starting at k.
						i = k;
					}
				} while (isDocComment);
			}
			else
			{
				var foundEnd = false;
				while (!IsEOF(i))
				{
					var ch = source[i];
					if (ch == '*' && IsChar(i + 1, '/'))
					{
						i += 2; // skip * and /
						foundEnd = true;
						break;
					}
					else
						i += GetCharSize(i);
				}

				if (!foundEnd)
					throw new ParseException(GetErrorToken(startIndex, 2),
						"Unable to locate end of block comment.");
			}

			var value = source.Substring(startIndex, i - startIndex);
			if (isDocComment)
				lastDocComment = value;
			// if we reach this point, i is one index past the last character in the comment
			return new Token(source, value, TokenType.Comment, startIndex);
		}

		private Token ScanNumber(ref int i)
		{
			var startIndex = i;
			var isReal = false;
			// 0x or 0X, hexadecimal literal
			if (source[i] == '0' && !IsEOF(i + 1) &&
				(source[i + 1] == 'x' || source[i + 1] == 'X'))
			{
				i += 2;
				ScanHexNumber(ref i);
			}
			else
			{
				var emptySection = true; // false if one or more digits encountered after '_'

				var hasExp = false;
				// decimal, either (u)int or real
				while (!IsEOF(i))
				{
					var ch = source[i];
					if (ch == '.' && !IsEOF(i + 1) &&
						source[i + 1] >= '0' && source[i + 1] <= '9')
					{
						if (emptySection && i != startIndex)
							throw new ParseException(GetErrorToken(i, 1),
								"Underscore ('_') not followed by one or more digits.");
						if (isReal)
							throw new ParseException(GetErrorToken(i, 1),
								"A floating-point literal may only have one decimal point.");
						isReal = true;
					}
					else if (ch == 'e' || ch == 'E')
					{
						if (!isReal)
							throw new ParseException(GetErrorToken(i, 1),
								"Exponential part not allowed without preceding decimal point. " +
								"In other words, a number like 10e9 is not allowed; use 10.0e9 instead.");
						if (hasExp)
							throw new ParseException(GetErrorToken(i, 1),
								"More than one exponential part in number literal.");
						i++; // move to the sign or first digit of exponential part

						// The exponential part may be followed by an optional sign
						// and one or more digits.
						if (!IsEOF(i) && (source[i] == '+' || source[i] == '-'))
							i++; // skip sign :O

						if (IsEOF(i) || source[i] < '0' || source[i] > '9')
							throw new ParseException(GetErrorToken(i, 1),
								"Invalid exponential part. An e or E in a number literal must " +
								"be followed by an optional sign (+ or -) and one or more digits");

						hasExp = true;
						// keep scanning decimal numbers (i++ below skips the first one)
					}
					else if (ch == '_')
					{
						if (isReal)
							throw new ParseException(GetErrorToken(i, 1),
								"Underscore ('_') not allowed after decimal point.");
						if (emptySection)
							throw new ParseException(GetErrorToken(i, 1),
								"Underscore ('_') not preceded by one or more digits.");
						emptySection = true;
					}
					else if (ch < '0' || ch > '9')
						break;
					else
						emptySection = false;

					i++; // all of these are single UTF-16 codons
				}

				if (emptySection)
					throw new ParseException(GetErrorToken(i, 1),
						"Underscore ('_') not followed by one or more digits.");

				// I can has multiplier?
				if (!IsEOF(i))
				{
					var ch = source[i];
					if (ch == 'k' || ch == 'K' ||
						ch == 'm' || ch == 'M' ||
						ch == 'g' || ch == 'G' ||
						ch == 't' || ch == 'T')
					{
						if (isReal)
							throw new ParseException(GetErrorToken(i, 1),
								"Floating-point literals cannot have a multiplier (k, M, G, T).");
						i++; // skip multiplier
					}
				}

				// EOF or non-digit here
				if (!IsEOF(i) && (source[i] == 'u' || source[i] == 'U'))
				{
					if (isReal)
						throw new ParseException(GetErrorToken(i, 1),
							"Floating-point literals cannot have an unsigned suffix (u or U).");
					i++; // skip u/U
				}
			}

			return new Token(source, source.Substring(startIndex, i - startIndex),
				isReal ? TokenType.Real : TokenType.Integer, startIndex);
		}

		private void ScanHexNumber(ref int i)
		{
			var startIndex = i - 2; // starts with 0x or 0X

			if (!IsHex(source[i]))
				throw new ParseException(GetErrorToken(startIndex, 2),
					"Hex specifier (0x or 0X) without hexadecimal digits.");

			// Hex literals may contain _ as separators; however, you may not have
			// more than one consecutive _, and an underscore must be preceded by
			// at least one digit. In other words,
			//    0x0000		valid
			//    0x00_00		valid
			//    0x0000_		invalid
			//    0x_0000		invalid
			//    0x00__00		invalid

			var emptySection = true; // false if one or more hex digits encountered in current section

			while (!IsEOF(i))
			{
				var ch = source[i];
				if (ch == '_')
				{
					if (emptySection)
						throw new ParseException(GetErrorToken(i, 1),
							"Underscore ('_') not preceded by one or more hexadecimal digits.");
					emptySection = true; // new section
				}
				else if (!IsHex(ch))
					break; // reached the end of the literal
				else
					emptySection = false;

				i++; // all digits are single UTF-16 codons
			}

			if (emptySection)
				throw new ParseException(GetErrorToken(i, 1),
					"Underscore ('_') not followed by at least one hexadecimal digit.");

			var isUnsigned = !IsEOF(i) && (source[i] == 'u' || source[i] == 'U');
			if (isUnsigned)
				i++; // skip the unsigned suffix

			// all done!
		}

		private Token ScanRegularString(ref int i)
		{
			var startIndex = i;
			i++; // skip "

			var sb = new StringBuilder();

			while (!IsEOF(i) && source[i] != '"')
			{
				var ch = source[i];
				if (IsNewline(ch))
					throw new ParseException(GetErrorToken(i, 1), "Newline in string literal.");

				if (ch == '\\')
				{
					i++; // always skip the backslash!
					if (IsEOF(i))
						throw new ParseException(GetErrorToken(i - 1, 1), "Unterminated string literal.");

					var len = 1; // the length of the escape sequence, in number of characters following the \
					ch = source[i];
					switch (ch)
					{
						case '"': sb.Append('"'); break;
						case '\\': sb.Append('\\'); break;
						case '0': sb.Append('\0'); break;
						case 'a': sb.Append('\a'); break;
						case 'b': sb.Append('\b'); break;
						case 'n': sb.Append('\n'); break;
						case 'r': sb.Append('\r'); break;
						case 't': sb.Append('\t'); break;
						case '_': sb.Append('\xA0'); break; // non-breaking space
						case '-': sb.Append('\xAD'); break; // soft hyphen
						case 'u':
						case 'U':
							len = ch == 'U' ? 9 : 5; // \uxxxx or \Uxxxxxxxx
							for (var k = 1; k < len; k++)
								if (IsEOF(i + k) || !IsHex(source[i + k]))
									throw new ParseException(GetErrorToken(i - 1, 2),
										ch == 'U'
											? "'\\U' must be followed by 8 hexadecimal digits."
											: "'\\u' must be followed by 4 hexadecimal digits.");

							var charCode = uint.Parse(source.Substring(i + 1, len - 1),
								NumberStyles.AllowHexSpecifier, CI.InvariantCulture);

							if (charCode > 0x10FFFF)
								throw new ParseException(GetErrorToken(i + 1, len - 1),
									"Code point in a Unicode escape sequence cannot be greater than U+10FFFF");

							sb.Append(char.ConvertFromUtf32(unchecked((int)charCode)));
							break;
						default:
							throw new ParseException(GetErrorToken(i - 1, 2), "Invalid escape sequence.");
					}
					i += len; // skip escape sequence :D
				}
				else
				{
					var size = GetCharSize(i);
					sb.Append(source, i, size);
					i += size;
				}
			}

			if (IsEOF(i))
				throw new ParseException(GetErrorToken(startIndex, 1), "Unterminated string literal.");
			i++; // skip closing "

			return new StringToken(source, source.Substring(startIndex, i - startIndex),
				sb.ToString(), startIndex);
		}

		private Token ScanVerbatimString(ref int i)
		{
			var startIndex = i;
			i += 2; // skip r"

			var sb = new StringBuilder();

			var foundEnd = false;
			while (!IsEOF(i))
			{
				if (source[i] == '"')
				{
					i++;
					if (IsEOF(i) || source[i] != '"')
					{
						foundEnd = true;
						break; // single ", terminates the string
					}
					// otherwise, double "" => "
					// (we skip the first one and output the next)
				}

				var size = GetCharSize(i);
				sb.Append(source, i, size);
				i += size;
			}

			if (!foundEnd)
				throw new ParseException(GetErrorToken(startIndex, 2), "Unterminated string literal.");

			return new StringToken(source, source.Substring(startIndex, i - startIndex),
				sb.ToString(), startIndex);
		}

		private Token ScanRegularExpression(ref int i)
		{
			throw new NotImplementedException();
		}

		private Token ScanIdentifier(ref int i)
		{
			var startIndex = i;

			if (IsEOF(i) || !char.IsLetter(source, i) && source[i] != '_' &&
				char.GetUnicodeCategory(source, i) != UnicodeCategory.LetterNumber)
				throw new Exception("Internal error: ScanIdentifier called without valid identifier-start-character at i.");

			while (!IsEOF(i))
			{
				var cat = char.GetUnicodeCategory(source, i);
				if (char.IsLetter(source, i) || // L
					cat == UnicodeCategory.LetterNumber || // Nl
					cat == UnicodeCategory.DecimalDigitNumber || // Nd
					cat == UnicodeCategory.NonSpacingMark || // Mn
					cat == UnicodeCategory.SpacingCombiningMark || // Mc
					cat == UnicodeCategory.ConnectorPunctuation || // Pc
					cat == UnicodeCategory.Format) // Cf
					i += GetCharSize(i);
				else
					break; // all done!
			}

			var ident = source.Substring(startIndex, i - startIndex);
			TokenType type;
			if (!IdentToKeyword.TryGetValue(ident, out type))
			{
				type = TokenType.Identifier;
				if (NormalizeIdentifiers)
					ident = NormalizeIdentifier(ident);
			}

			return new Token(source, ident, type, startIndex);
		}

		private Token ScanPunctuation(ref int i)
		{
			var startIndex = i;

			char ch = source[i++];
			switch (ch)
			{
				// always single: { } [ ] ( ) , ; ~ @
				case '{': case '}':
				case '[': case ']':
				case '(': case ')':
				case ',': case ';':
				case '~': case '@':
					break; // So simple!
				// other combinations:
				// .  ...  <=>  <<?=?  >>?=?  **?=?
				// ::=?  :  ->?  ==?  !=  \?[?!.]?
				case '.':
					if (!IsEOF(i + 1) && source[i] == '.' && source[i + 1] == '.')
						i += 2;
					break;
				case '<':
					if (!IsEOF(i + 1) && source[i] == '=' && source[i + 1] == '>')
						i += 2;
					else
					{
						if (!IsEOF(i) && source[i] == '<')
							i++;
						goto case '='; // may be followed by =
					}
					break;
				case '>':
					if (!IsEOF(i) && source[i] == '>')
						i++;
					goto case '='; // may be followed by =
				case '*':
					if (!IsEOF(i) && source[i] == '*')
						i++;
					goto case '='; // may be followed by =
				case ':':
					if (!IsEOF(i) && source[i] == ':')
					{
						i++;
						goto case '='; // may be followed by =
					}
					break;
				case '-':
					if (!IsEOF(i) && source[i] == '>') // ->
						i++;
					else
						goto case '='; // may be followed by =
					break;
				// optionally followed by an '=': + | / % & ^ # $ =
				case '+': case '|':
				case '/': case '%': case '&':
				case '^':
				case '#': case '$':
				case '=':
					if (!IsEOF(i) && source[i] == '=')
						i++;
					break;
				case '!':
					if (IsEOF(i) || source[i] != '=')
						goto default; // must be followed by =
					i++; // skip =
					break;
				case '?':
					char next = IsEOF(i) ? '\0' : source[i];
					if (next == '.' || next == '(' || next == '[' ||
						next == '?' || next == '!')
						i++;
					break;
				default:
					throw new ParseException(GetErrorToken(startIndex, 1),
						string.Format("Invalid character: {0} (U+{1:X4}).", ch, (int)ch));
			}

			var punct = source.Substring(startIndex, i - startIndex);
			return new Token(source, punct, PunctToType[punct], startIndex);
		}

		private void SkipWhitespace(ref int i)
		{
			while (!IsEOF(i) && char.IsWhiteSpace(source, i))
				i += GetCharSize(i);
		}

		private int GetCharSize(int i) { return char.IsSurrogatePair(source, i) ? 2 : 1; }

		private Token GetErrorToken(int i, int length)
		{
			return new Token(source, IsEOF(i) ? "" : source.Substring(i, length), TokenType.Invalid, i);
		}

		private bool IsEOF(int i)
		{
			return i >= source.Length;
		}

		private bool IsChar(int i, char ch)
		{
			return !IsEOF(i) && source[i] == ch;
		}

		private static bool IsNewline(char ch)
		{
			return ch == '\u000D' || ch == '\u000A' || ch == '\u0085' ||
				ch == '\u2028' || ch == '\u2029';
		}

		private static bool IsHex(char ch)
		{
			return
				ch >= '0' && ch <= '9' ||
				ch >= 'A' && ch <= 'F' ||
				ch >= 'a' && ch <= 'f';
		}

		#endregion

		#region Interface members

		public TokenEnumerator GetEnumerator()
		{
			return new TokenEnumerator(this);
		}

		IEnumerator<Token> IEnumerable<Token>.GetEnumerator()
		{
			return this.GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return this.GetEnumerator();
		}

		public class TokenEnumerator : IEnumerator<Token>
		{
			public TokenEnumerator(Tokenizer owner)
			{
				this.owner = owner;
			}

			private Tokenizer owner;
			private Token current = null;
			private int index = -1;
			private bool done = false;

			public Token Current
			{
				get
				{
					if (index == -1 || done)
						throw new InvalidOperationException();
					return current;
				}
			}

			void IDisposable.Dispose()
			{
				owner = null;
			}

			object System.Collections.IEnumerator.Current
			{
				get { return ((IEnumerator<Token>)this).Current; }
			}

			public bool MoveNext()
			{
				if (owner.EnsureMinIndex(++index))
				{
					current = owner[index];
					return true;
				}
				else
				{
					current = null;
					done = true;
					return false;
				}
			}

			public void Reset()
			{
				current = null;
				index = -1;
				done = false;
			}
		}

		#endregion

		#region Static members

		private static string NormalizeIdentifier(string ident)
		{
			ident = ident.Normalize(NormalizationForm.FormC);

			// Formatting characters are very, very rare. This algorithm only initialises sb once
			// it encounters a formatting character, and only after that point are other characters
			// appended to sb. This makes the loop very fast if there aren't any formatting characters
			// (very likely), and marginally slower if there are (highly unlikely).

			StringBuilder sb = null;
			for (var i = 0; i < ident.Length; i++)
			{
				if (char.GetUnicodeCategory(ident, i) == UnicodeCategory.Format)
				{
					if (sb == null)
					{
						sb = new StringBuilder(ident.Length);
						sb.Append(ident, 0, i); // up to but NOT including the formatting character
					}

					if (char.IsSurrogate(ident, i))
						i++;
				}
				else if (sb != null)
					sb.Append(ident[i]);
			}

			return sb == null ? ident : sb.ToString();
		}

		static Tokenizer()
		{
			KeywordToString = new Dictionary<TokenType, string>(IdentToKeyword.Count);
			foreach (var kvp in IdentToKeyword)
				KeywordToString.Add(kvp.Value, kvp.Key);

			PunctToString = new Dictionary<TokenType, string>(PunctToType.Count);
			foreach (var kvp in PunctToType)
				PunctToString.Add(kvp.Value, kvp.Key);
		}

		internal static readonly Dictionary<string, TokenType> IdentToKeyword = new Dictionary<string, TokenType>
		{
			{"abstract", TokenType.Abstract}, {"and", TokenType.And}, {"async", TokenType.Async},
			{"base", TokenType.Base}, {"break", TokenType.Break},
			{"catch", TokenType.Catch}, {"class", TokenType.Class}, {"const", TokenType.Const},
			{"do", TokenType.Do},
			{"else", TokenType.Else}, {"enum", TokenType.Enum},
			{"false", TokenType.False}, {"finally", TokenType.Finally}, {"for", TokenType.For}, {"function", TokenType.Function},
			{"get", TokenType.Get}, {"global", TokenType.Global},
			{"if", TokenType.If}, {"in", TokenType.In}, {"inheritable", TokenType.Inheritable}, {"is", TokenType.Is}, {"iter", TokenType.Iter},
			{"namespace", TokenType.Namespace}, {"new", TokenType.New}, {"next", TokenType.Next}, {"not", TokenType.Not},
			{"null", TokenType.Null},
			{"operator", TokenType.Operator}, {"or", TokenType.Or}, {"overridable", TokenType.Overridable}, {"override", TokenType.Override},
			{"private", TokenType.Private}, {"protected", TokenType.Protected}, {"public", TokenType.Public},
			{"refeq", TokenType.Refeq}, {"return", TokenType.Return},
			{"set", TokenType.Set}, {"static", TokenType.Static},
			{"this", TokenType.This}, {"throw", TokenType.Throw}, {"true", TokenType.True},
			{"try", TokenType.Try}, {"typeof", TokenType.Typeof},
			{"use", TokenType.Use}, {"var", TokenType.Var},
			{"while", TokenType.While}, {"xor", TokenType.Xor}, {"yield", TokenType.Yield},
		};
		internal static readonly Dictionary<string, TokenType> PunctToType = new Dictionary<string, TokenType>
		{
			{"{", TokenType.CurlyOpen}, {"}", TokenType.CurlyClose},
			{"[", TokenType.SquareOpen}, {"]", TokenType.SquareClose},
			{"(", TokenType.ParenOpen}, {")", TokenType.ParenClose},
			{".", TokenType.Dot}, {",", TokenType.Comma},
			{":", TokenType.Colon}, {";", TokenType.Semicolon}, {"~", TokenType.Tilde},
			{"<", TokenType.Less}, {"<=", TokenType.LessEqual}, {">", TokenType.Greater}, {">=", TokenType.GreaterEqual},
			{"==", TokenType.DoubleEqual}, {"!=", TokenType.NotEqual}, {"?", TokenType.Question},
			{"??", TokenType.NullCoalescing}, {"?!", TokenType.NullOr}, {"->", TokenType.FuncApplication},
			{"+", TokenType.Plus}, {"-", TokenType.Minus}, {"|", TokenType.Pipe},
			{"*", TokenType.Multiply}, {"/", TokenType.Divide}, {"%", TokenType.Mod}, {"&", TokenType.Ampersand},
			{"^", TokenType.Caret}, {"::", TokenType.Concatenation},
			{"<<", TokenType.ShiftLeft}, {">>", TokenType.ShiftRight},
			{"**", TokenType.Power}, {"#", TokenType.Hash}, {"$", TokenType.Dollar},
			{"=", TokenType.Assign},
			{"+=", TokenType.PlusAssign}, {"-=", TokenType.MinusAssign}, {"|=", TokenType.PipeAssign},
			{"*=", TokenType.MulAssign}, {"/=", TokenType.DivAssign}, {"%=", TokenType.ModAssign}, {"&=", TokenType.AmpAssign},
			{"^=", TokenType.CaretAssign}, {"::=", TokenType.ConcatAssign},
			{"<<=", TokenType.ShiftLeftAssign}, {">>=", TokenType.ShiftRightAssign},
			{"**=", TokenType.PowerAssign}, {"#=", TokenType.HashAssign}, {"$=", TokenType.DollarAssign},
			{"@", TokenType.At}, {"...", TokenType.Splat}, {"?.", TokenType.SafeAccess}, {"<=>", TokenType.Compare},
			{"?(", TokenType.ParenOpenSafe}, {"?[", TokenType.SquareOpenSafe}
		};

		internal static readonly Dictionary<TokenType, string> KeywordToString;
		internal static readonly Dictionary<TokenType, string> PunctToString;

		#endregion
	}

	/// <summary>
	/// Represents option that are passed to the tokenizer.
	/// </summary>
	[Flags]
	public enum TokenizerFlags
	{
		/// <summary>
		/// Specifies no tokenizer flags.
		/// </summary>
		None = 0,
		/// <summary>
		/// If specified, the tokenizer includes comments.
		/// By default, comments are ignored; only tokens are returned.
		/// </summary>
		IncludeComments = 1,
		/// <summary>
		/// If specified, the tokenizer normalizes identifiers, which consists of
		/// normalizing the string to Normalization Form C, then stripping out all
		/// formatting characters (Unicode character class Cf).
		/// </summary>
		NormalizeIdentifiers = 2,
	}
}