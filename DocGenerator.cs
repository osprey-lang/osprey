using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Osprey.Json;
using Osprey.Members;
using Osprey.Nodes;
using Type = Osprey.Members.Type;

namespace Osprey
{
	/// <summary>
	/// Represents a documentation generator, which turns a bunch of documents into a JSON documentation file.
	/// </summary>
	internal static class DocGenerator
	{
		public static void Generate(Namespace projectNamespace, Compiler compiler,
			IEnumerable<Document> documents, string outputPath, bool prettyPrint)
		{
			if (projectNamespace == null)
				throw new ArgumentNullException("projectNamespace");
			if (documents == null)
				throw new ArgumentNullException("documents");
			if (outputPath == null)
				throw new ArgumentNullException("outputPath");

			using (var file = File.Open(outputPath, FileMode.Create, FileAccess.Write, FileShare.None))
				Generate(projectNamespace, compiler, documents, file, prettyPrint);
		}

		public static void Generate(Namespace projectNamespace, Compiler compiler,
			IEnumerable<Document> documents, Stream output, bool prettyPrint)
		{
			compiler.Notice("[doc] Generating documentation file...");

			if (projectNamespace == null)
				throw new ArgumentNullException("projectNamespace");
			if (documents == null)
				throw new ArgumentNullException("documents");
			if (output == null)
				throw new ArgumentNullException("output");
			if (!output.CanWrite)
				throw new ArgumentException("The output stream must be writable.", "output");

			var state = new DocGenState();
			foreach (var doc in documents)
				BuildNamespaceMembers(state, projectNamespace, doc.GlobalDeclarationSpace, doc.Namespace);

			var result = state.ToJson();
			using (var writer = new StreamWriter(output))
				writer.Write(result.ToString(prettyPrint));

			compiler.Notice("[doc] Finished generating documentation file.");
			// Done!
		}

		private static void BuildNamespaceMembers(DocGenState target, Namespace projectNamespace,
			NamespaceDeclaration nsDecl, FileNamespace document)
		{
			foreach (var type in nsDecl.Types)
			{
				var result = BuildTypeMembers(type, projectNamespace, document);
				if (result != null)
					target.AddTypeDoc(type.Type, result);
			}

			foreach (var function in nsDecl.Functions)
			{
				var result = BuildFunctionDoc(function, projectNamespace, document);
				if (result != null)
					target.AddFunctionDoc(function.DeclSpace, result);
			}

			foreach (var constant in nsDecl.Constants)
			{
				if (constant.Constants.Length > 1)
					// Cannot document multiple constants in a single declaration!
					continue;

				var result = BuildConstantDoc(constant, projectNamespace, document);
				if (result != null)
					target.AddConstDoc(constant.Constants[0], result);
			}

			foreach (var subNs in nsDecl.Namespaces)
				BuildNamespaceMembers(target, projectNamespace, subNs, document);
		}

		private static JsonObject BuildTypeMembers(TypeDeclaration type,
			Namespace projectNamespace, FileNamespace document)
		{
			JsonObject result = new JsonObject();
			if (type.DocString != null)
			{
				var doc = ParseDocString(type.DocString, type.Type, document, type.Type);
				if (doc.IsValidForType)
					doc.ToJson(result);
				else
					document.Compiler.Warning(string.Format(
						"[doc] Ignoring documentation for type '{0}' " +
						"(types cannot have documentation for parameters, return value or thrown errors).",
						type.Type.FullName));
			}

			if (type is EnumDeclaration)
				ProcessEnumMembers((EnumDeclaration)type, result, projectNamespace, document);
			else // ClassDeclaration
				ProcessClassMembers((ClassDeclaration)type, result, projectNamespace, document);

			return result.Count == 0 ? null : result;
		}

		private static void ProcessEnumMembers(EnumDeclaration @enum, JsonObject target,
			Namespace projectNamespace, FileNamespace document)
		{
			if (@enum.Members.Count > 0)
			{
				JsonObject fieldDocs = null;
				for (var i = 0; i < @enum.Members.Count; i++)
				{
					var field = @enum.Members[i];
					if (field.DocString != null)
					{
						var member = @enum.Type.GetMember(field.Name);
						var doc = ParseDocString(field.DocString, @enum.Type, document, member);
						if (doc.IsValidForField)
						{
							if (fieldDocs == null)
								fieldDocs = new JsonObject();
							fieldDocs[field.Name] = doc.ToJson();
						}
						else
							document.Compiler.Warning(string.Format(InvalidFieldDoc,
								member.FullName, "enum field"));
					}
				}

				if (fieldDocs != null)
					target.Add("fields", fieldDocs);
			}
		}

		private static void ProcessClassMembers(ClassDeclaration @class, JsonObject target,
			Namespace projectNamespace, FileNamespace document)
		{
			var context = @class.Type;

			{ // fields: instance, static and constant
				JsonObject fieldDocs = null;

				foreach (var field in @class.Fields)
					if (field.DocString != null && field.Declarators.Count == 1)
					{
						var firstDecl = field.Declarators[0];
						var member = context.GetMember(firstDecl.Name);
						var doc = ParseDocString(field.DocString, context, document, member);

						if (doc.IsValidForField)
						{
							if (fieldDocs == null)
								fieldDocs = new JsonObject();
							fieldDocs[firstDecl.Name] = doc.ToJson();
						}
						else
							document.Compiler.Warning(string.Format(InvalidFieldDoc,
								member.FullName, "field"));
					}

				foreach (var field in @class.Constants)
					if (field.DocString != null && field.Declarators.Count == 1)
					{
						var firstDecl = field.Declarators[0];
						var constant = context.GetMember(firstDecl.Name);
						var doc = ParseDocString(field.DocString, context, document, constant);
						if (doc.IsValidForField)
						{
							if (fieldDocs == null)
								fieldDocs = new JsonObject();
							fieldDocs[firstDecl.Name] = doc.ToJson();
						}
						else
							document.Compiler.Warning(string.Format(InvalidFieldDoc,
								constant.FullName, "class constant"));
					}

				if (fieldDocs != null)
					target["fields"] = fieldDocs;
			}

			{ // methods: instance, static, constructors, operators
				JsonObject methodDocs = null;
				Action<Method, MemberDoc, IEnumerable<Parameter>, Compiler> addMethodDoc =
					(method, doc, parameters, compiler) =>
					{
						if (!VerifyParameters(method.Group.FullName, doc, parameters, compiler))
							return;

						if (methodDocs == null)
							methodDocs = new JsonObject();

						var name = method.Name;

						JsonArray overloads;
						if (methodDocs.ContainsKey(name))
							overloads = (JsonArray)methodDocs[name];
						else
							methodDocs[name] = overloads = new JsonArray();

						var jsonDoc = new JsonObject();
						jsonDoc.Add("signature", method.Signature.ToJson());
						doc.ToJson(jsonDoc);
						overloads.Add(jsonDoc);
					};

				foreach (var method in @class.Methods)
					if (method.DocString != null)
						addMethodDoc(method.DeclSpace,
							ParseDocString(method.DocString, context, document, method.DeclSpace.Group),
							method.Parameters, document.Compiler);

				foreach (var ctor in @class.Constructors)
					if (ctor.DocString != null)
						addMethodDoc(ctor.DeclSpace,
							ParseDocString(ctor.DocString, context, document, ctor.DeclSpace.Group),
							ctor.Parameters, document.Compiler);

				foreach (var op in @class.Operators)
					if (op.DocString != null)
						addMethodDoc(op.DeclSpace,
							ParseDocString(op.DocString, context, document, op.DeclSpace.Group),
							op.DeclSpace.Parameters, document.Compiler);

				if (methodDocs != null)
					target["methods"] = methodDocs;
			}

			{ // properties: instance and static
				JsonObject propDocs = null;

				foreach (var prop in @class.Properties)
					if (prop.DocString != null && !(prop is IndexerAccessorDeclaration))
					{
						if (propDocs == null)
							propDocs = new JsonObject();

						JsonObject propDocObj;
						if (propDocs.ContainsKey(prop.Name))
							propDocObj = (JsonObject)propDocs[prop.Name];
						else
							propDocs[prop.Name] = propDocObj = new JsonObject();

						propDocObj[prop.IsSetter ? "set" : "get"] = ParseDocString(prop.DocString,
							context, document, prop.DeclSpace.Group).ToJson();
					}
					// TODO: indexers

				if (propDocs != null)
					target["properties"] = propDocs;
			}
		}

		private static JsonObject BuildFunctionDoc(GlobalFunctionDeclaration function,
			Namespace projectNamespace, FileNamespace document)
		{
			if (function.DocString == null)
				return null;

			var doc = ParseDocString(function.DocString, function.DeclSpace, document, function.DeclSpace);
			if (!VerifyParameters(function.DeclSpace.Group.FullName,
				doc, function.Function.Parameters, document.Compiler))
				return null;

			var jsonDoc = new JsonObject();
			jsonDoc.Add("signature", function.DeclSpace.Signature.ToJson());
			doc.ToJson(jsonDoc);

			return jsonDoc;
		}

		private static JsonObject BuildConstantDoc(GlobalConstantDeclaration constant,
			Namespace projectNamespace, FileNamespace document)
		{
			if (constant.DocString == null)
				return null;

			var firstDecl = constant.Declaration.Declarators[0];
			var globalConst = constant.Constants[0];
			var doc = ParseDocString(constant.DocString, globalConst.Parent, document, globalConst);

			if (!doc.IsValidForField)
			{
				document.Compiler.Warning(string.Format(InvalidFieldDoc, constant.Constants[0].FullName, "global constant"));
				return null;
			}

			return doc.ToJson();
		}

		private static MemberDoc ParseDocString(string input,
			IDeclarationSpace context, FileNamespace document, NamedMember targetMember)
		{
			var isMultiline = input.StartsWith("/**");
			if (isMultiline)
				// Strip the leading '/' and trailing '*/' from multiline comments
				input = input.Substring(1, input.Length - 3);

			var lines = input.Split(LineSeparators, StringSplitOptions.None);

			var output = new MemberDoc();

			string currentKeyword = null;
			string currentParam = null;
			StringBuilder currentValue = null;
			var nextSep = ' ';

			var lastIndent = -1;
			foreach (var line in lines)
			{
				var realLine = (isMultiline ? MultilineStartRegex : SingleLineStartRegex).Replace(line, "");

				var indent = 0;
				var i = 0;
				while (i < realLine.Length && char.IsWhiteSpace(realLine, i))
				{
					indent++;
					i += char.IsSurrogatePair(realLine, i) ? 2 : 1;
				}

				if (i == realLine.Length)
				{
					// If this is the case, then the entire line is whitespace.
					if (currentKeyword != null)
						nextSep = ParagraphSeparator;
				}
				else if (currentKeyword == null || indent <= lastIndent)
				{
					var match = FormatKeywordRegex.Match(realLine);
					if (match.Success)
					{
						if (currentKeyword != null)
							output.AddDocumentation(currentKeyword, currentParam,
								currentValue.ToString(), context, document, targetMember);

						currentKeyword = match.Groups[1].Value;
						currentParam = match.Groups[2].Success ? match.Groups[2].Value.Trim() : null;

						currentValue = AppendDocLine(null, match.Groups[3].Value, ref nextSep);
						lastIndent = indent;
					}
				}
				else
				{
					AppendDocLine(currentValue, realLine, ref nextSep);
				}
			}

			if (currentKeyword != null)
				output.AddDocumentation(currentKeyword, currentParam,
					currentValue.ToString(), context, document, targetMember);

			return output;
		}

		private static StringBuilder AppendDocLine(StringBuilder currentValue, string line, ref char nextSep)
		{
			var forcedLineBreak = line.EndsWith("\\");
			if (forcedLineBreak)
				line = line.Substring(0, line.Length - 1);

			line = line.Trim();

			if (currentValue == null)
				currentValue = new StringBuilder(line);
			else
			{
				if (currentValue.Length > 0)
					currentValue.Append(nextSep);
				currentValue.Append(line);
			}

			nextSep = forcedLineBreak ? '\n' : ' ';

			return currentValue;
		}

		private static bool VerifyParameters(string memberName, MemberDoc documentation,
			IEnumerable<Parameter> parameters, Compiler compiler)
		{
			if (parameters == null)
				throw new ArgumentNullException("parameters");

			if (documentation.Parameters == null)
				return true;

			var paramDict = parameters.ToDictionary(p => p.Name);
			var firstInvalidParam = documentation.Parameters
				.Select(kvp => kvp.Key)
				.FirstOrDefault(p => !paramDict.ContainsKey(p));

			if (firstInvalidParam != null)
				compiler.Warning(string.Format("[doc] Ignoring documentation for '{0}' (parameter '{1}' does not exist).",
					memberName, firstInvalidParam));

			return firstInvalidParam == null;
		}

		private static string GetDocName(Type type)
		{
			return string.Format("{0} {1}", type.Module.Name, type.FullName);
		}

		private static string GetDocName(Field field)
		{
			return string.Format("{0} {1} {2}", field.Parent.Module.Name,
				field.Parent.FullName, field.Name);
		}

		private static string GetDocName(MethodGroup method)
		{
			if (method.Parent is Namespace)
				return string.Format("{0} {1}", method.Module.Name, method.FullName);
			else
				return string.Format("{0} {1} {2}", method.Module.Name, method.ParentAsClass.FullName, method.Name);
		}

		private static string GetDocName(Property prop)
		{
			return string.Format("{0} {1} {2}", prop.Parent.Module.Name, prop.Parent.FullName, prop.Name);
		}

		private static string[] LineSeparators = { "\r\n", "\n", "\r", "\u2028", "\u2029" };

		private static Regex SingleLineStartRegex = new Regex(@"^\s*///");
		private static Regex MultilineStartRegex = new Regex(@"^\s*\**");
		private static Regex FormatKeywordRegex = new Regex(@"^\s*([a-zA-Z]+)\s*([^:]+)?\:(?:\s(.*))?$");

		private const char ParagraphSeparator = '\u2029';

		private const string InvalidFieldDoc =
			"[doc] Ignoring documentation for {1} '{0}' " +
			"({1}s cannot have documentation for parameters, return value or thrown errors).";

		private class DocGenState
		{
			public JsonObject FunctionDocs;
			public JsonObject TypeDocs;
			public JsonObject ConstDocs;

			public void AddFunctionDoc(Method method, JsonObject doc)
			{
				if (FunctionDocs == null)
					FunctionDocs = new JsonObject();

				var name = method.Group.FullName;
				JsonArray overloads;

				if (FunctionDocs.ContainsKey(name))
					overloads = (JsonArray)FunctionDocs[name];
				else
					FunctionDocs[name] = overloads = new JsonArray();

				overloads.Add(doc);
			}

			public void AddTypeDoc(Type type, JsonObject doc)
			{
				if (TypeDocs == null)
					TypeDocs = new JsonObject();

				TypeDocs[type.FullName] = doc;
			}

			public void AddConstDoc(GlobalConstant constant, JsonObject doc)
			{
				if (ConstDocs == null)
					ConstDocs = new JsonObject();

				ConstDocs[constant.FullName] = doc;
			}

			public JsonObject ToJson()
			{
				var output = new JsonObject(3);
				if (FunctionDocs != null)
					output["functions"] = FunctionDocs;
				if (TypeDocs != null)
					output["types"] = TypeDocs;
				if (ConstDocs != null)
					output["constants"] = ConstDocs;
				return output;
			}
		}

		private class MemberDoc
		{
			/// <summary>
			/// The summary of the member. (Always applicable)
			/// </summary>
			public string Summary;
			/// <summary>
			/// A description of the return value of the member.
			/// (Applicable to property getters, methods and operators)
			/// </summary>
			public string Returns;
			/// <summary>
			/// Additional remarks. (Always applicable)
			/// </summary>
			public string Remarks;

			/// <summary>
			/// Parameter documentation. This member is null if there are no parameters.
			/// (Applicable to parametrised members, e.g. methods, constructors, operators, etc.)
			/// </summary>
			public Dictionary<string, string> Parameters;
			/// <summary>
			/// Throws declarations. This member is null if there is no throws documentation.
			/// (Applicable to everything except fields and constants)
			/// </summary>
			public List<ThrowsDoc> Throws;

			public bool IsValidForField
			{
				get
				{
					return Returns == null &&
						Parameters == null &&
						Throws == null;
				}
			}

			public bool IsValidForType
			{
				get
				{
					return Returns == null && Parameters == null && Throws == null;
				}
			}

			/// <summary>
			/// Adds documentation for the specified keyword.
			/// </summary>
			/// <param name="keyword">The keyword to add documentation for.</param>
			/// <param name="param">A parameter attached to the documentation keyword; if missing, null.</param>
			/// <param name="value">The documentation for the keyword.</param>
			/// <param name="context">The context in which to resolve type names.</param>
			/// <param name="document">The document containing the doc string.</param>
			/// <exception cref="ArgumentNullException"><paramref name="keyword"/> is null.
			/// -or-
			/// <paramref name="value"/> is null.
			/// -or-
			/// <paramref name="context"/> is null.
			/// -or-
			/// <paramref name="document"/> is null.</exception>
			public void AddDocumentation(string keyword, string param, string value,
				IDeclarationSpace context, FileNamespace document, NamedMember targetMember)
			{
				if (keyword == null)
					throw new ArgumentNullException("keyword");
				if (value == null)
					throw new ArgumentNullException("value");
				if (context == null)
					throw new ArgumentNullException("context");
				if (document == null)
					throw new ArgumentNullException("document");

				keyword = keyword.ToLowerInvariant();
				switch (keyword)
				{
					case "summary":
						Summary = value;
						break;
					case "returns":
						Returns = value;
						break;
					case "remarks":
						Remarks = value;
						break;
					case "param":
						if (param != null)
						{
							if (Parameters == null)
								Parameters = new Dictionary<string, string>();
							Parameters[param] = value;
						}
						break;
					case "throws":
						if (param != null)
						{
							if (Throws == null)
								Throws = new List<ThrowsDoc>();

							var ns = context is Namespace ? (Namespace)context : context.GetContainingNamespace();
							var isGlobal = param.StartsWith("global.");
							if (isGlobal)
								param = param.Substring(7);

							try
							{
								var type = ns.ResolveTypeName(new TypeName(param, isGlobal), document);
								Throws.Add(new ThrowsDoc(type, value));
							}
							catch (CompileTimeException)
							{
								document.Compiler.Warning(
									string.Format("[doc] Ignoring throws declaration for '{0}' (could not resolve type name '{1}')",
										targetMember == null ? "(unknown member)" : targetMember.FullName,
										param));
							}
						}
						break;
				}
			}

			public JsonObject ToJson()
			{
				var output = new JsonObject();
				this.ToJson(output);
				return output;
			}

			public void ToJson(JsonObject target)
			{
				if (target == null)
					throw new ArgumentNullException("target");

				if (Summary != null)
					target["summary"] = (JsonString)Summary;
				if (Returns != null)
					target["returns"] = (JsonString)Returns;
				if (Remarks != null)
					target["remarks"] = (JsonString)Remarks;

				if (Parameters != null && Parameters.Count > 0)
					target["params"] = new JsonObject(Parameters);
				if (Throws != null && Throws.Count > 0)
					target["throws"] = new JsonArray(Throws.Select(t => t.ToJson()));
			}
		}

		private struct ThrowsDoc
		{
			public ThrowsDoc(Type thrownType, string condition)
			{
				ThrownType = thrownType;
				Condition = condition;
			}

			/// <summary>
			/// The type of the error that is thrown.
			/// </summary>
			public readonly Type ThrownType;
			/// <summary>
			/// A description of the condition that causes the error to be thrown.
			/// </summary>
			public readonly string Condition;

			public JsonObject ToJson()
			{
				return new JsonObject
				{
					{(JsonString)"type", (JsonString)GetDocName(ThrownType)},
					{(JsonString)"condition", (JsonString)Condition},
				};
			}
		}
	}
}