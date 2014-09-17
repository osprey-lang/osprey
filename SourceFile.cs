using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;

namespace Osprey
{
	public class SourceFile
	{
		private SourceFile(string fileName, string source, byte[] hash)
		{
			if (fileName == null)
				throw new ArgumentNullException("fileName");
			if (source == null)
				throw new ArgumentNullException("source");

			this.fileName = fileName;
			this.source = source;
			this.FileHash = hash;
		}

		private string fileName;
		/// <summary>Gets the name of the file.</summary>
		public string FileName { get { return fileName; } }

		private string source;
		/// <summary>Gets the text contents of the file.</summary>
		public string Source { get { return source; } }

		private int lineCount = 0;
		private LineInfo[] lineInfo = null;

		/// <summary>SHA-1 hash code of the file contents, or null if the program is being compiled without debug symbols.</summary>
		internal byte[] FileHash;

		private LineInfo GetNearestLine(int sourceIndex)
		{
			if (lineInfo != null)
			{
				int iMin = 0, iMax = lineCount - 1;

				while (iMax >= iMin)
				{
					var iMid = (iMin + iMax) / 2;
					var line = lineInfo[iMid];

					if (sourceIndex < line.StartIndex)
						// Search the lower half
						iMax = iMid - 1;
					else if (sourceIndex >= line.EndIndex)
					{
						if (iMid == lineCount)
							// The nearest line is the last; return it.
							// When GetLineNumber walks the source from
							// this location, it will add lines to the
							// cache as it encounters their ends.
							return line;

						// Otherwise, the nearest line may still be in
						// the cache somewhere, so search the upper half
						iMin = iMid + 1;
					}
					else
						return line;
				}
			}

			// Lines start at 1!
			return new LineInfo { LineNumber = 1 };
		}

		private void AppendLineInfo(int lineNumber, int startIndex, int endIndex)
		{
			if (lineCount > 0)
			{
				var lastLine = lineInfo[lineCount - 1];
				if (lineNumber <= lastLine.LineNumber)
					return; // The line is already in the cache

				if (lineNumber != lastLine.LineNumber + 1)
					throw new ArgumentOutOfRangeException("lineNumber",
						string.Format("Lines must be added consecutively (got {0}, expected {1}).",
							lineNumber, lastLine.LineNumber + 1));

				// The last line might be a partial; update its range
				// to run up to the startIndex of the next line, i.e.
				// the line we're adding
				lineInfo[lineCount - 1].EndIndex = startIndex;
			}
			else
			{
				if (lineNumber != 1 || startIndex != 0)
					throw new ArgumentOutOfRangeException(null,
						"The first line must be numbered 1 and start at index 0.");
			}

			if (lineInfo == null)
				lineInfo = new LineInfo[20];
			else if (lineCount == lineInfo.Length)
				Array.Resize(ref lineInfo, lineInfo.Length * 3 / 2);

			lineInfo[lineCount++] = new LineInfo
			{
				LineNumber = lineNumber,
				StartIndex = startIndex,
				EndIndex = endIndex,
			};
		}

		/// <summary>
		/// Calculates the 1-based line number and 1-based column of an index within the source file.
		/// </summary>
		/// <param name="index">The index of the character.</param>
		/// <param name="tabSize">The visual size of a tab character. Pass 1 to treat tabs as single characters.</param>
		/// <param name="column">(Out) The column of the character.</param>
		/// <returns>The line number of the character at the specified index.</returns>
		public int GetLineNumber(int index, int tabSize, out int column)
		{
			var nearestLine = GetNearestLine(index);

			int line = nearestLine.LineNumber;
			int col = 0;

			int curLineStart = nearestLine.StartIndex;
			for (var i = curLineStart; i < index; )
			{
				char ch = source[i++];
				if (ch == '\r' || ch == '\n' || ch == '\u0085' ||
					ch == '\u2028' || ch == '\u2029')
				{
					// \r\n counts as a single unit
					if (i < source.Length && ch == '\r' && source[i] == '\n')
						i++;

					// Add the last line to the cache
					AppendLineInfo(line, curLineStart, i);

					col = 0;
					line++;
					curLineStart = i;
				}
				else if (ch == '\t')
					col += tabSize - col % tabSize;
				else
					col++;
			}

			// Append the current line, even if it's partial.
			// We'll expand it later if we ever look past it.
			AppendLineInfo(line, curLineStart, index);

			column = col + 1;
			return line;
		}

		public static SourceFile Open(string fileName)
		{
			return Open(fileName, computeHash: false);
		}

		public static SourceFile Open(string fileName, bool computeHash)
		{
			byte[] hash = null;

			string fileText;
			if (computeHash)
				using (var fs = File.OpenRead(fileName))
				{
					if (!fs.CanSeek)
						throw new IOException(string.Format("The source file '{0}' must be seekable.", fileName));

					// First, hash the entire file
					using (var sha1 = SHA1.Create())
						hash = sha1.ComputeHash(fs);

					// Then, seek back to the beginning and use StreamReader
					// to read the text contents of the file
					fs.Seek(0, SeekOrigin.Begin);
					using (var sr = new StreamReader(fs, detectEncodingFromByteOrderMarks: true))
						fileText = sr.ReadToEnd();
				}
			else
				fileText = File.ReadAllText(fileName);

			return new SourceFile(fileName, fileText, hash);
		}

		[DebuggerDisplay("Line #{LineNumber} from {StartIndex} to {EndIndex}")]
		private struct LineInfo
		{
			public int LineNumber; // 1-based
			public int StartIndex; // Inclusive
			public int EndIndex; // Exclusive
		}
	}
}