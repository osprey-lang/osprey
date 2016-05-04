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
		/// Initializes a new instance of <see cref="Tokenizer"/> with the specified source file and flags.
		/// </summary>
		/// <param name="file">The file to tokenize.</param>
		/// <param name="includeComments">Determines whether to include comments; set to false when parsing.</param>
		/// <exception cref="ArgumentNullException"><paramref name="input"/> is null.</exception>
		public Tokenizer(SourceFile file, TokenizerFlags flags)
		{
			if (file == null)
				throw new ArgumentNullException("file");

			this.flags = flags;
			this.file = file;
			// Store this in our own field to avoid double indirection
			this.source = file.Source;
		}

		/// <summary>
		/// Initializes a new instance of <see cref="Tokenizer"/> with the specified source text and flags.
		/// The tokenizer's <see cref="File"/> becomes an anonymous file with the given contents.
		/// </summary>
		/// <param name="fileSource"></param>
		/// <param name="flags"></param>
		public Tokenizer(string fileSource, TokenizerFlags flags)
		{
			if (fileSource == null)
				throw new ArgumentNullException("fileSource");

			this.flags = flags;
			this.file = SourceFile.CreateAnonymous(fileSource);
			this.source = fileSource;
		}

		private int tokenCount = 0;
		private Token[] tokens = new Token[128];

		private TokenizerFlags flags;

		private SourceFile file;
		/// <summary>
		/// Gets the source file that is being tokenized.
		/// </summary>
		public SourceFile File { get { return file; } }

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

		private Token lastDocComment = null;

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
				if (index < 0)
					throw new ArgumentOutOfRangeException("index");

				// Note: EnsureMinIndex updates 'tokens' and, if it returns false, 'eof'. Mmm, side-effective.
				if (EnsureMinIndex(index))
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
			if (index < tokenCount)
				return true;
			if (eof != null && index >= tokenCount)
				return false; // we've already determined that there are no more tokens to read

			while (tokenCount <= index + ExtraTokens)
				if (!MoveNext())
					break;

			return index < tokenCount;
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
			if (tokenCount == tokens.Length)
				// Grow the array by 50%
				Array.Resize(ref tokens, tokens.Length * 3 / 2);
			tokens[tokenCount++] = tok;
			return tok.Type != TokenType.EOF;
		}

		private Token ScanToken(ref int i)
		{
			SkipWhitespace(ref i);

			if (IsEOF(i))
				return eof = new Token(file, TokenType.EOF, i, i);

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

			if ((ch == 'r' || ch == 'R') && IsChar(i + 1, '"'))
				return ScanVerbatimString(ref i);

			if (ch == '\'')
				return ScanCharLiteral(ref i);

			if (ch == '\\' || // escaped identifier
				ch == '_' || char.IsLetter(source, i) ||
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

			var tok = new Token(file, TokenType.Comment, startIndex, i);
			if (isDocComment)
				lastDocComment = tok;
			// if we reach this point, i is one index past the last character in the comment
			return tok;
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

					i++; // all of these are single UTF-16 code units
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

			return new Token(file, source.Substring(startIndex, i - startIndex),
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

			var substrStart = i;
			while (!IsEOF(i) && source[i] != '"')
			{
				var ch = source[i];
				if (IsNewline(ch))
					throw new ParseException(GetErrorToken(i, 1), "Newline in string literal.");

				if (ch == '\\')
				{
					// Append any characters we may have eaten so far
					if (i != substrStart)
						sb.Append(source, substrStart, i - substrStart);

					int codepoint = ScanEscapeSequence(ref i);
					if (codepoint > 0xFFFF)
						sb.Append(char.ConvertFromUtf32(codepoint));
					else
						sb.Append(unchecked((char)codepoint));

					substrStart = i;
				}
				else
					i++;
			}

			if (IsEOF(i))
				throw new ParseException(GetErrorToken(startIndex, 1), "Unterminated string literal.");

			// Append remaining characters
			if (i != substrStart)
				sb.Append(source, substrStart, i - substrStart);

			i++; // skip closing "

			return new StringToken(file, sb.ToString(), startIndex, i);
		}

		private Token ScanVerbatimString(ref int i)
		{
			var startIndex = i;
			i += 2; // skip r"/R"

			var hasEscapes = false;
			var foundEnd = false;
			while (!IsEOF(i))
			{
				if (source[i++] == '"')
				{
					if (!IsEOF(i) && source[i] == '"')
					{
						// Found "" sequence
						hasEscapes = true;
						i++; // Skip second "
						continue;
					}
					// Single ", terminates the string
					foundEnd = true;
					break;
				}
			}

			if (!foundEnd)
				throw new ParseException(GetErrorToken(startIndex, 2), "Unterminated string literal.");

			// -3 because of starting r" and final "
			var value = source.Substring(startIndex + 2, i - startIndex - 3);
			if (hasEscapes)
				// Replace all "" with " in one go
				value = value.Replace("\"\"", "\"");

			return new StringToken(file, value, startIndex, i);
		}

		private Token ScanCharLiteral(ref int i)
		{
			var startIndex = i;
			i++; // skip '

			int codepoint = -1;
			if (!IsEOF(i))
			{
				var ch = source[i];
				if (ch == '\'')
					throw new ParseException(GetErrorToken(i, 1), "Empty character literal is not allowed.");
				if (IsNewline(ch))
					throw new ParseException(GetErrorToken(i, 1), "Newline in character literal.");

				if (ch == '\\')
					codepoint = ScanEscapeSequence(ref i);
				else
				{
					codepoint = (int)ch;
					i++; // skip character
				}
			}
			if (IsEOF(i))
				throw new ParseException(GetErrorToken(startIndex, 1), "Unterminated character literal.");
			if (source[i] != '\'')
				throw new ParseException(GetErrorToken(i, 1),
					"Expected \"'\" (single quote); character literals can only contain one character.");
			i++; // skip closing '

			return new CharToken(file, codepoint, startIndex, i);
		}

		// Parses an escape sequence (of any kind) and returns the Unicode code point.
		private int ScanEscapeSequence(ref int i)
		{
			i++; // always skip the backslash!
			if (IsEOF(i))
				throw new ParseException(GetErrorToken(i - 1, 1), "Invalid escape sequence.");

			int codepoint;

			var len = 1; // the length of the escape sequence, in number of characters following the \
			var ch = source[i];
			switch (ch)
			{
				// ", ' and \ are preserved as is; all others represent other characters
				case '"':
				case '\'':
				case '\\': codepoint = (int)ch; break;
				case '0': codepoint = (int)'\0'; break;
				case 'a': codepoint = (int)'\a'; break;
				case 'b': codepoint = (int)'\b'; break;
				case 'n': codepoint = (int)'\n'; break;
				case 'r': codepoint = (int)'\r'; break;
				case 't': codepoint = (int)'\t'; break;
				case '_': codepoint = (int)'\xA0'; break; // non-breaking space
				case '-': codepoint = (int)'\xAD'; break; // soft hyphen
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

					codepoint = unchecked((int)charCode);
					break;
				default:
					throw new ParseException(GetErrorToken(i - 1, 2), "Invalid escape sequence.");
			}
			i += len; // skip escape sequence :D

			return codepoint;
		}

		private Token ScanIdentifier(ref int i)
		{
			var escaped = source[i] == '\\';
			if (escaped)
				i++;

			// Note: does not include the \ prefix, if any
			var startIndex = i;

			if (IsEOF(i) || !char.IsLetter(source, i) && source[i] != '_' &&
				char.GetUnicodeCategory(source, i) != UnicodeCategory.LetterNumber)
				// This can only happen if a backslash is not followed by an identifier-start-character
				throw new ParseException(GetErrorToken(i - 1, 1),
					"Expected an identifier or keyword after backslash ('\\').");

			var hasFormatChars = false;
			while (!IsEOF(i))
			{
				var cat = char.GetUnicodeCategory(source, i);
				if (char.IsLetter(source, i) || // L
					cat == UnicodeCategory.LetterNumber || // Nl
					cat == UnicodeCategory.DecimalDigitNumber || // Nd
					cat == UnicodeCategory.NonSpacingMark || // Mn
					cat == UnicodeCategory.SpacingCombiningMark || // Mc
					cat == UnicodeCategory.ConnectorPunctuation) // Pc
					i += GetCharSize(i);
				else if (cat == UnicodeCategory.Format)
				{
					i += GetCharSize(i);
					hasFormatChars = true;
				}
				else
					break; // all done!
			}

			var ident = source.Substring(startIndex, i - startIndex);
			var type = TokenType.Identifier;
			var isKeyword = !escaped &&
				ident.Length <= TokenFacts.LongestKeywordLength &&
				TokenFacts.IdentToKeyword.TryGetValue(ident, out type);
			if (isKeyword)
				return new Token(file, ident, type, startIndex);

			var contextualType = ContextualType.None;
			if (!escaped &&
				ident.Length <= TokenFacts.LongestContextualLength)
				TokenFacts.IdentToContextual.TryGetValue(ident, out contextualType);

			if (NormalizeIdentifiers)
				ident = NormalizeIdentifier(ident, hasFormatChars);

			return new Identifier(file, ident, escaped, startIndex, contextualType);
		}

		private Token ScanPunctuation(ref int i)
		{
			var startIndex = i;

			TokenType type;
			char ch = source[i++];
			switch (ch)
			{
				// always single: { } [ ] ( ) , ; ~ @
				case '{':
					type = TokenType.CurlyOpen;
					break;
				case '}':
					type = TokenType.CurlyClose;
					break;
				case '[':
					type = TokenType.SquareOpen;
					break;
				case ']':
					type = TokenType.SquareClose;
					break;
				case '(':
					type = TokenType.ParenOpen;
					break;
				case ')':
					type = TokenType.ParenClose;
					break;
				case ',':
					type = TokenType.Comma;
					break;
				case ';':
					type = TokenType.Semicolon;
					break;
				case '~':
					type = TokenType.Tilde;
					break;
				case '@':
					type = TokenType.At;
					break; // So simple!
				// other combinations:
				// .  ...  <=>  <<?=?  >>?=?  **?=?
				// ::=?  :  ->?  ==?  !=  \?[?!.]?
				case '.':
					type = TokenType.Dot;
					if (!IsEOF(i + 1) && source[i] == '.' && source[i + 1] == '.')
					{
						type = TokenType.Splat;
						i += 2;
					}
					break;
				case '<':
					type = TokenType.Less;
					if (!IsEOF(i + 1) && source[i] == '=' && source[i + 1] == '>')
					{
						type = TokenType.Compare;
						i += 2;
					}
					else
					{
						if (!IsEOF(i) && source[i] == '<')
						{
							type = TokenType.ShiftLeft;
							i++;
							AcceptEquals(ref i, ref type, TokenType.ShiftLeftAssign);
						}
						else
							AcceptEquals(ref i, ref type, TokenType.LessEqual);
					}
					break;
				case '>':
					type = TokenType.Greater;
					if (!IsEOF(i) && source[i] == '>')
					{
						type = TokenType.ShiftRight;
						i++;
						AcceptEquals(ref i, ref type, TokenType.ShiftRightAssign);
					}
					else
						AcceptEquals(ref i, ref type, TokenType.GreaterEqual);
					break;
				case '*':
					type = TokenType.Multiply;
					if (!IsEOF(i) && source[i] == '*')
					{
						type = TokenType.Power;
						i++;
						AcceptEquals(ref i, ref type, TokenType.PowerAssign);
					}
					else
						AcceptEquals(ref i, ref type, TokenType.MulAssign);
					break;
				case ':':
					type = TokenType.Colon;
					if (!IsEOF(i) && source[i] == ':')
					{
						type = TokenType.Concatenation;
						i++;
						AcceptEquals(ref i, ref type, TokenType.ConcatAssign);
					}
					break;
				case '-':
					type = TokenType.Minus;
					if (!IsEOF(i) && source[i] == '>') // ->
					{
						type = TokenType.FuncApplication;
						i++;
					}
					else
						AcceptEquals(ref i, ref type, TokenType.MinusAssign);
					break;
				// optionally followed by an '=': + | / % & ^ # $ =
				case '+':
					type = TokenType.Plus;
					AcceptEquals(ref i, ref type, TokenType.PlusAssign);
					break;
				case '|':
					type = TokenType.Pipe;
					AcceptEquals(ref i, ref type, TokenType.PipeAssign);
					break;
				case '/':
					type = TokenType.Divide;
					AcceptEquals(ref i, ref type, TokenType.DivAssign);
					break;
				case '%':
					type = TokenType.Mod;
					AcceptEquals(ref i, ref type, TokenType.ModAssign);
					break;
				case '&':
					type = TokenType.Ampersand;
					AcceptEquals(ref i, ref type, TokenType.AmpAssign);
					break;
				case '^':
					type = TokenType.Caret;
					AcceptEquals(ref i, ref type, TokenType.CaretAssign);
					break;
				case '=':
					type = TokenType.Assign;
					if (!IsEOF(i) && source[i] == '>') // =>
					{
						type = TokenType.FatArrow;
						i++;
					}
					else
						AcceptEquals(ref i, ref type, TokenType.DoubleEqual);
					break;
				case '!':
					if (IsEOF(i) || source[i] != '=')
						goto default; // must be followed by =
					type = TokenType.NotEqual;
					i++; // skip =
					break;
				case '?':
					type = TokenType.Question;
					if (!IsEOF(i))
					{
						switch (source[i++])
						{
							case '.': type = TokenType.SafeAccess; break;
							case '(': type = TokenType.ParenOpenSafe; break;
							case '[': type = TokenType.SquareOpenSafe; break;
							case '?': type = TokenType.NullCoalescing; break;
							case '!': type = TokenType.NullOr; break;
							default: i--; break; // Nope, back up
						}
					}
					break;
				default:
					throw new ParseException(GetErrorToken(startIndex, 1),
						string.Format("Invalid character: {0} (U+{1:X4}).", ch, (int)ch));
			}

			return new Token(file, type, startIndex, i);
		}

		private void AcceptEquals(ref int i, ref TokenType tokenType, TokenType equalsType)
		{
			if (!IsEOF(i) && source[i] == '=')
			{
				tokenType = equalsType;
				i++;
			}
		}

		private void SkipWhitespace(ref int i)
		{
			while (!IsEOF(i) && char.IsWhiteSpace(source, i))
				i += GetCharSize(i);
		}

		private int GetCharSize(int i) { return char.IsSurrogatePair(source, i) ? 2 : 1; }

		internal Token GetErrorToken(int i, int length)
		{
			if (IsEOF(i))
				return new Token(file, "", TokenType.Invalid, i);
			return new Token(file, TokenType.Invalid, i, i + length);
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
				current = null;
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

		// When trying to read a token, scan up to this number of tokens
		// past the requested index. This somewhat improves performance.
		private const int ExtraTokens = 25;

		private static string NormalizeIdentifier(string ident, bool hasFormatChars)
		{
			ident = ident.Normalize(NormalizationForm.FormC);

			// Only run the slow path in the very unlikely case that there are
			// any format characters (Cf) in the identifier.

			if (hasFormatChars)
			{
				var sb = new StringBuilder();
				for (var i = 0; i < ident.Length; i++)
				{
					if (char.GetUnicodeCategory(ident, i) == UnicodeCategory.Format)
					{
						if (char.IsSurrogate(ident, i))
							i++;
					}
					else if (sb != null)
						sb.Append(ident[i]);
				}

				return sb.ToString();
			}

			return ident;
		}

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