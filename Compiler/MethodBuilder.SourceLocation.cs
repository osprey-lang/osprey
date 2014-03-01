using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Instructions;
using Osprey.Nodes;

namespace Osprey
{
	public sealed class SourceLocation
	{
		public SourceLocation(ParseNode node)
		{
			if (node == null)
				throw new ArgumentNullException("node");
			if (node.Document == null)
				throw new ArgumentException("The node must belong to a document.", "node");

			this.document = node.Document;
			this.startIndex = node.StartIndex;
			this.endIndex = node.EndIndex;
		}
		public SourceLocation(Document document, int startIndex, int endIndex)
		{
			if (document == null)
				throw new ArgumentNullException("document");

			this.document = document;
			this.startIndex = startIndex;
			this.endIndex = endIndex;
		}

		private Document document;
		/// <summary>
		/// Gets the document containing the source location.
		/// </summary>
		public Document Document { get { return document; } }

		private int startIndex, endIndex;
		/// <summary>
		/// Gets the character index of the source location within the document.
		/// </summary>
		public int SourceStartIndex { get { return startIndex; } }
		/// <summary>
		/// Gets the last character index (exclusive) of the source location within the document.
		/// </summary>
		public int SourceEndIndex { get { return endIndex; } }

		internal int startOffset = -1, endOffset = -1;
		/// <summary>
		/// Gets the first byte offset within the bytecode of the method to which
		/// this <see cref="SourceLocation"/> applies.
		/// </summary>
		public int BytecodeStartOffset { get { return startOffset; } }
		/// <summary>
		/// Gets the last byte offset (exclusive) within the bytecode of the method
		/// to which this <see cref="SourceLocation"/> applies.
		/// </summary>
		public int BytecodeEndOffset { get { return endOffset; } }

		public int GetLineNumber(int tabSize, out int column)
		{
			return Token.GetLineNumber(document.FileSource, startIndex, tabSize, out column);
		}

		public override string ToString()
		{
			int column;
			int lineNumber = GetLineNumber(1, out column);

			var length = endIndex - startIndex;

			return string.Format("\"{0}\":{1}:{2}+{3}",
				document.FileName, lineNumber, column, length);
		}
	}
}