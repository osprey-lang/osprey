using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ParseNode = Osprey.Nodes.ParseNode;

namespace Osprey
{
	public class ParseException : Exception
	{
		// These two constructors have been removed, because we always always always
		// want to make sure there's an (accurate!) error location reported.
		// public ParseException(string message)
		// public ParseException(string message, Exception innerException)

		public ParseException(Token offender)
			: this(offender, offender.Type == TokenType.EOF ? "Unexpected end of file" : "Unexpected token " + offender.ToString(), null)
		{ }
		public ParseException(Token offender, string message)
			: this(offender, message, null)
		{
			this.token = offender;
		}
		public ParseException(Token offender, string message, Exception innerException)
			: base(message, innerException)
		{
			if (offender == null)
				throw new ArgumentNullException("offender");
			this.token = offender;
		}

		public ParseException(ParseNode offender, string source, string message)
			: this(offender, source, message, null)
		{ }
		public ParseException(ParseNode offender, string source, string message, Exception innerException)
			: base(message, innerException)
		{
			if (offender == null)
				throw new ArgumentNullException("offender");
			if (source == null)
				throw new ArgumentNullException("source");
			this.node = offender;
			this.source = source;
		}

		private Token token = null;
		/// <summary>
		/// Gets the offending token, or null if there was none.
		/// </summary>
		public Token Token { get { return token; } }

		private ParseNode node = null;
		/// <summary>
		/// Gets the offending node, or null if there was none.
		/// </summary>
		public ParseNode Node { get { return node; } }

		private string source; // Very private.

		/// <summary>
		/// Gets the index at which the exception occurred in the source file, or -1 if it cannot be determined.
		/// </summary>
		public int Index
		{
			get
			{
				return token != null ? token.Index :
					node != null ? node.StartIndex :
					-1;
			}
		}

		/// <summary>
		/// Gets the name of the file in which the parse exception occurred.
		/// </summary>
		public string FileName { get; internal set; }

		public override string ToString()
		{
			if (token != null)
			{
				int line, column;
				line = token.GetLineNumber(4, out column);

				return "ParseException at line " + line + ", column " + column + ": " + Message;
			}
			else
				return "ParseException: " + Message;
		}

		/// <summary>
		/// Returns the line number (1-based) and column (1-based) the error appears at.
		/// </summary>
		/// <param name="tabSize">The visual size of a tab character. Pass 1 to treat tabs as single characters.</param>
		/// <param name="column">The column at which the token appears.</param>
		/// <returns>The line number at which the token appears.</returns>
		public int GetLineNumber(int tabSize, out int column)
		{
			if (token != null)
				return token.GetLineNumber(tabSize, out column);
			if (node != null)
				return Token.GetLineNumber(source, node.StartIndex, tabSize, out column);

			column = -1;
			return -1;
		}

		public string GetFileSource()
		{
			if (token != null)
				return token.Source;
			return source;
		}

		internal ParseException Extend(string newMessage)
		{
			if (token != null)
				return new ParseException(token, newMessage, this);
			// node must be non-null
			return new ParseException(node, source, newMessage, this);
		}
	}
}