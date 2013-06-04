using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Nodes;

namespace Osprey
{
	/// <summary>
	/// Represents an exception that is thrown at compile-time.
	/// </summary>
	public class CompileTimeException : Exception
	{
		private CompileTimeException()
			: this("An error occurred during compilation.")
		{ }

		public CompileTimeException(ParseNode node)
			: this(node, "An error occurred during compilation.", null)
		{ }

		private CompileTimeException(string message)
			: this(null, message, null)
		{ }

		public CompileTimeException(ParseNode node, string message)
			: this(node, message, null)
		{ }

		private CompileTimeException(string message, Exception innerException)
			: this(null, message, innerException)
		{ }

		public CompileTimeException(ParseNode node, string message, Exception innerException)
			: base(message, innerException)
		{
			this.node = node;
		}

		private ParseNode node;
		/// <summary>
		/// Gets the <see cref="Node"/> that caused the exception, if available.
		/// </summary>
		public ParseNode Node { get { return node; } }

		/// <summary>
		/// Gets the document in which the exception occurred.
		/// </summary>
		public Document Document { get; internal set; }
	}
}