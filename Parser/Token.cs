using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey
{
	/// <summary>
	/// Represents a single token (or comment) in a source file.
	/// </summary>
	public class Token
	{
		/// <summary>
		/// Initializes a new <see cref="Token"/> with the specified source, value, type and index.
		/// </summary>
		/// <param name="source">The source file that the token comes from.</param>
		/// <param name="value">The token's string value.</param>
		/// <param name="type">The type of the token.</param>
		/// <param name="index">The index in the source script at which the token appears.</param>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="source"/> is null.
		/// -or-
		/// <paramref name="value"/> is null.</exception>
		public Token(SourceFile source, string value, TokenType type, int index)
		{
			if (source == null)
				throw new ArgumentNullException("source");
			if (value == null)
				throw new ArgumentNullException("value");
			this.source = source;
			this.value = value;
			this.type = type;
			this.index = index;
			this.endIndex = index + value.Length;
		}
		/// <summary>
		/// Initializes a new <see cref="Token"/> with the specified source, type, index and end index.
		/// The string value of the token is initialized only on demand.
		/// </summary>
		/// <param name="source">The source file that the token comes from.</param>
		/// <param name="type">The type of the token.</param>
		/// <param name="index">The index in the source script at which the token appears.</param>
		/// <param name="endIndex">The last index (exclusive) of the token in the source file.</param>
		/// <exception cref="ArgumentNullException"><paramref name="source"/> is null.</exception>
		public Token(SourceFile source, TokenType type, int index, int endIndex)
		{
			if (source == null)
				throw new ArgumentNullException("source");
			this.source = source;
			this.value = null;
			this.type = type;
			this.index = index;
			this.endIndex = endIndex;
		}

		private SourceFile source;
		/// <summary>
		/// Gets the text of the source file that the token comes from.
		/// </summary>
		public SourceFile Source { get { return source; } }

		private string value;
		/// <summary>
		/// Gets the value of the token.
		/// </summary>
		public string Value
		{
			get
			{
				if (value == null)
					value = source.Source.Substring(index, endIndex - index);
				return value;
			}
		}

		private TokenType type;
		/// <summary>
		/// Gets the type of the token.
		/// </summary>
		public TokenType Type { get { return type; } }

		/// <summary>
		/// Gets the contextual type of the token, if the token can be used as a contextual keyword.
		/// </summary>
		public virtual ContextualType ContextualType { get { return ContextualType.None; } }

		private int index;
		/// <summary>
		/// Gets the index in the source script at which the token appears.
		/// </summary>
		public int Index { get { return index; } }

		private int endIndex;
		/// <summary>
		/// Gets the index in the source code script immediately following the token.
		/// </summary>
		public int EndIndex { get { return endIndex; } }

		internal Token documentation;
		/// <summary>
		/// Gets the contents of the documentation comment immediately preceding this token,
		/// or null if there is none.
		/// </summary>
		public Token Documentation { get { return documentation; } }

		/// <summary>
		/// Returns the line number (1-based) and column (1-based) the token appears at.
		/// </summary>
		/// <param name="tabSize">The visual size of a tab character. Pass 1 to treat tabs as single characters.</param>
		/// <param name="column">The column at which the token appears.</param>
		/// <returns>The line number at which the token appears.</returns>
		public int GetLineNumber(int tabSize, out int column)
		{
			return source.GetLineNumber(index, tabSize, out column);
		}

		/// <summary>
		/// Determines whether the token's type matches a specified <see cref="TokenType"/>.
		/// </summary>
		/// <param name="type">The <see cref="TokenType"/> to test against.</param>
		/// <returns>true if the token matches the specified <see cref="TokenType"/>; otherwise, false.</returns>
		/// <remarks>If <paramref name="type"/> is a <see cref="TokenType.CategoryMask"/>
		/// (that is, its <see cref="TokenType.ValueMask"/> bits are 0), this method tests category;
		/// otherwise, both value and category are tested.</remarks>
		public bool Match(TokenType type)
		{
			if (type == TokenType.Invalid)
				return this.type == TokenType.Invalid;
			else if ((type & TokenType.ValueMask) == TokenType.None)
				return (this.type & type) == type;
			else
				return this.type == type;
		}

		public override string ToString()
		{
			if (type == TokenType.Identifier)
				return string.Format("identifier '{0}'", Value);
			if ((type & TokenType.Literal) == TokenType.Literal)
				return string.Format("literal ({0})", Value);
			if ((type & TokenType.Keyword) == TokenType.Keyword)
				return string.Format("keyword '{0}'", Value);
			if (type == TokenType.EOF)
				return "end of file";
			return string.Format("'{0}' ({1})", Value, System.Enum.GetName(typeof(TokenType), type) ?? "unknown");
		}
	}

	public class Identifier : Token
	{
		public Identifier(SourceFile source, string value, bool escaped, int index, ContextualType contextualType)
			: base(source, value, TokenType.Identifier, index)
		{
			this.escaped = escaped;
			this.contextualType = contextualType;
		}

		private bool escaped;
		/// <summary>
		/// Gets a value that indicates whether the identifier is escaped
		/// (that is, it has the '\' prefix).
		/// </summary>
		public bool Escaped { get { return escaped; } }

		private ContextualType contextualType;
		public override ContextualType ContextualType { get { return contextualType; } }
	}

	/// <summary>
	/// Represents a string literal token. The tokenizer "parses" string literals,
	/// turning escape sequences into the characters they actually refer to, and
	/// this class contains that "real" value.
	/// </summary>
	public class StringToken : Token
	{
		public StringToken(SourceFile source, string literalValue, int index, int endIndex)
			: base(source, TokenType.String, index, endIndex)
		{
			this.literalValue = literalValue;
		}

		private string literalValue;
		/// <summary>
		/// Gets the string value represented by the literal.
		/// </summary>
		public string LiteralValue { get { return literalValue; } }
	}

	public class CharToken : Token
	{
		public CharToken(SourceFile source, int codepoint, int index, int endIndex)
			: base(source, TokenType.Character, index, endIndex)
		{
			this.codepoint = codepoint;
		}

		private int codepoint;
		/// <summary>
		/// Gets the Unicode code point represented by the token.
		/// </summary>
		public int Codepoint { get { return codepoint; } }
	}
}