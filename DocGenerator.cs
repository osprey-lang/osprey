using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.Nodes;

namespace Osprey
{
	/// <summary>
	/// Represents a documentation generator, which turns a bunch of documents into a JSON documentation file.
	/// </summary>
	internal static class DocGenerator
	{
		public static void Generate(Namespace projectNamespace, IEnumerable<Document> documents, string outputPath)
		{
			if (projectNamespace == null)
				throw new ArgumentNullException("projectNamespace");
			if (documents == null)
				throw new ArgumentNullException("documents");
			if (outputPath == null)
				throw new ArgumentNullException("outputPath");

			using (var file = File.Open(outputPath, FileMode.Create, FileAccess.Write, FileShare.None))
				Generate(projectNamespace, documents, file);
		}

		public static void Generate(Namespace projectNamespace, IEnumerable<Document> documents, Stream output)
		{
			if (projectNamespace == null)
				throw new ArgumentNullException("projectNamespace");
			if (documents == null)
				throw new ArgumentNullException("documents");
			if (output == null)
				throw new ArgumentNullException("output");
			if (!output.CanWrite)
				throw new ArgumentException("The output stream must be writable.", "output");

			var writer = new StreamWriter(output, Encoding.UTF8);
		}
	}
}