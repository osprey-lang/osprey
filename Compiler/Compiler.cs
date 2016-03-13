using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;
using Osprey.Instructions;
using Osprey.Members;
using Osprey.Nodes;
using CI = System.Globalization.CultureInfo;
using Enum = Osprey.Members.Enum;
using Switch = Osprey.Instructions.Switch;
using Type = Osprey.Members.Type;

namespace Osprey
{
	public partial class Compiler : IDisposable
	{
		private Compiler(ref CompilerOptions options, Dictionary<string, bool> constants, params string[] sourceFiles)
		{
			if (sourceFiles == null)
				throw new ArgumentNullException("sourceFiles");
			if (sourceFiles.Length == 0)
				throw new ArgumentException("There must be at least one source file.", "sourceFiles");

			this.flags = options.Flags;
			this.verbosity = options.Verbosity;
			this.libraryPath = options.LibraryPath;
			this.projectType = options.Type;
			this.metadataFile = options.MetadataFile;

			this.moduleName = options.ModuleName ?? Path.GetFileNameWithoutExtension(sourceFiles[0]);
			this.mainMethodName = options.MainMethod;

			this.sourceFiles = new Dictionary<string, Document>();
			foreach (var file in sourceFiles)
				this.sourceFiles[file] = null;
			this.documents = new List<Document>(this.sourceFiles.Count);

			if (constants != null && constants.Count > 0)
				extraConstants = new Dictionary<string, bool>(constants);

			if (options.NativeLibrary != null)
				this.nativeLibrary = NativeLibrary.Open(options.NativeLibrary);
		}

		~Compiler()
		{
			Dispose(disposing: false);
		}

		private CompilerFlags flags;
		/// <summary>Gets appropriate parser options for the compiler instance.</summary>
		private ParseFlags ParserOptions
		{
			get
			{
				return ((flags & CompilerFlags.UseExtensions) != 0 ? ParseFlags.UseExtensions : 0)
					|
					ParseFlags.Compilation;
			}
		}

		private bool NoStandardModule
		{
			get { return (flags & CompilerFlags.NoStandardModule) == CompilerFlags.NoStandardModule; }
		}
		private bool SilenceWarnings
		{
			get { return (flags & CompilerFlags.SilenceWarnings) == CompilerFlags.SilenceWarnings; }
		}
		private bool SilenceNotices
		{
			get { return (flags & CompilerFlags.SilenceNotices) == CompilerFlags.SilenceNotices; }
		}
		private bool SkipExternChecks
		{
			get { return (flags & CompilerFlags.SkipExternChecks) == CompilerFlags.SkipExternChecks; }
		}

		internal bool UseDebugSymbols
		{
			get { return (flags & CompilerFlags.NoDebugSymbols) == CompilerFlags.None; }
		}

		private CompilerVerbosity verbosity;

		private ProjectType projectType;
		public ProjectType ProjectType { get { return projectType; } }

		private string libraryPath;
		private string metadataFile;

		/// <summary>The name of the module that is being compiled.</summary>
		private string moduleName;

		/// <summary>The name of the main method, if specified in the CompilerOptions.</summary>
		private string mainMethodName;

		private NativeLibrary nativeLibrary;
		internal NativeLibrary NativeLibrary { get { return nativeLibrary; } }

		private Version version;
		/// <summary>Gets the version of the project.</summary>
		public Version Version { get { return version; } }

		private Dictionary<string, string> metadata;
		private Dictionary<string, Document> sourceFiles;
		private List<Document> documents;
		private HashSet<string> importedModules;

		private Dictionary<string, bool> extraConstants;

		private HashSet<Method> methodsWithLocalFunctions;
		private List<Action> additionalLocalExtractors;
		private HashSet<Method> generatorMethods;

		private List<Type> types;
		private List<MethodGroup> globalFunctions;
		private List<GlobalConstant> globalConstants;

		private ModulePool modules;
		private Namespace projectNamespace;
		private Block mainMethodBody;
		private Method mainMethod;
		private Class lambdaOpClass;
		private Class globalsClass;

		public Method MainMethod { get { return mainMethod; } }

		private Module outputModule;
		public Module OutputModule { get { return outputModule; } }

		private Dictionary<string, Type> typeObjects = new Dictionary<string, Type>();

		internal Type ObjectType    { get { return FindType(StandardNames.TypeRootName); } }
		internal Type EnumType      { get { return FindType(StandardNames.EnumName);     } }
		internal Type EnumSetType   { get { return FindType(StandardNames.EnumSetName);  } }
		internal Type BooleanType   { get { return FindType(StandardNames.BooleanName);  } }
		internal Type IntType       { get { return FindType(StandardNames.IntName);      } }
		internal Type UIntType      { get { return FindType(StandardNames.UIntName);     } }
		internal Type RealType      { get { return FindType(StandardNames.RealName);     } }
		internal Type CharType      { get { return FindType(StandardNames.CharName);     } }
		internal Type StringType    { get { return FindType(StandardNames.StringName);   } }
		internal Type ListType      { get { return FindType(StandardNames.ListName);     } }
		internal Type HashType      { get { return FindType(StandardNames.HashName);     } }
		internal Type IteratorType  { get { return FindType(StandardNames.IteratorName); } }
		internal Type MethodType    { get { return FindType(StandardNames.MethodName);   } }
		internal Type TypeType      { get { return FindType(StandardNames.TypeName);     } }
		internal Type ErrorType     { get { return FindType(StandardNames.ErrorName);    } }
		internal Type TypeErrorType { get { return FindType(StandardNames.GetStandardName("TypeError")); } }

		internal int MethodsWithLocalFunctionsCount
		{
			get { return methodsWithLocalFunctions != null ? methodsWithLocalFunctions.Count : 0; }
		}

		internal void AddMethodWithLocalFunctions(Method method)
		{
			if (method == null)
				throw new ArgumentNullException("method");

			if (methodsWithLocalFunctions == null)
				methodsWithLocalFunctions = new HashSet<Method>();
			methodsWithLocalFunctions.Add(method);
		}

		internal void AddLocalExtractor(Action extractor)
		{
			if (additionalLocalExtractors == null)
				additionalLocalExtractors = new List<Action>();
			additionalLocalExtractors.Add(extractor);
		}

		internal void AddGeneratorMethod(Method method)
		{
			if (method == null)
				throw new ArgumentNullException("method");

			if (generatorMethods == null)
				generatorMethods = new HashSet<Method>();
			generatorMethods.Add(method);
		}

		internal bool ImportsModule(string name)
		{
			return importedModules.Contains(name);
		}

		internal void AddType(Type type)
		{
			if (type == null)
				throw new ArgumentNullException("type");
			if (types == null)
				types = new List<Type>();
			types.Add(type);
		}

		internal void AddGlobalFunction(MethodGroup method)
		{
			if (method == null)
				throw new ArgumentNullException("method");
			if (method.ParentAsNamespace == null)
				throw new ArgumentException("The method is not global.", "method");
			if (globalFunctions == null)
				globalFunctions = new List<MethodGroup>();
			globalFunctions.Add(method);
		}

		internal void AddGlobalConstant(GlobalConstant constant)
		{
			if (constant == null)
				throw new ArgumentNullException("constant");
			if (globalConstants == null)
				globalConstants = new List<GlobalConstant>();
			globalConstants.Add(constant);
		}

		internal void AddGlobalVariable(Document parentDocument, GlobalVariable variable)
		{
			if (variable.CaptureField != null)
				return;

			if (globalsClass == null)
			{
				globalsClass = new Class("<global>", AccessLevel.Private, projectNamespace);
				globalsClass.BaseType = ObjectType;
				globalsClass.IsStatic = true;
				AddType(globalsClass);
			}

			var name = string.Format("{0}@{1}", variable.Name, documents.IndexOf(parentDocument));
			variable.CaptureField = new Field(name, AccessLevel.Public, globalsClass);
			globalsClass.DeclareField(variable.CaptureField);
		}

		private Type FindType(string fullName)
		{
			if (typeObjects.ContainsKey(fullName))
				return typeObjects[fullName];

			return typeObjects[fullName] = projectNamespace.ResolveTypeName(new TypeName(fullName, false), null);
		}

		internal MethodGroup GetLambdaOperatorMethod(LambdaOperator op)
		{
			if (lambdaOpClass == null)
			{
				lambdaOpClass = new Class("<lambda>", AccessLevel.Private, projectNamespace);
				lambdaOpClass.BaseType = ObjectType;
				lambdaOpClass.IsStatic = true;
				AddType(lambdaOpClass);
			}

			var name = lambdaOperatorNames[op];
			if (lambdaOpClass.ContainsMember(name))
				return (MethodGroup)lambdaOpClass.GetMember(name);
			else
			{
				// declare it!
				switch (op)
				{
					case LambdaOperator.Plus:
					case LambdaOperator.Minus:
					case LambdaOperator.BitwiseNot:
						{
							var unaryMethod = new BytecodeMethod(name, AccessLevel.Public, Splat.None, new Parameter("a", null));
							unaryMethod.IsStatic = true;
							unaryMethod.IsImplDetail = true;
							unaryMethod.Append(new LoadLocal(new LocalVariable(0, null, false, true)));
							unaryMethod.Append(new SimpleInstruction(GetUnaryOpcode(op)));
							unaryMethod.Append(new SimpleInstruction(Opcode.Ret));
							var group = lambdaOpClass.DeclareMethod(unaryMethod);

							if (op == LambdaOperator.BitwiseNot)
								return group;
							goto default;
						}
					case LambdaOperator.Or:
						{
							var orMethod = new BytecodeMethod(name, AccessLevel.Public, Splat.None,
								new Parameter("a", null), new Parameter("b", null));
							orMethod.IsStatic = true;
							orMethod.IsImplDetail = true;

							var trueLabel = new Label();

							orMethod.Append(new LoadLocal(new LocalVariable(0, null, false, true)));
							orMethod.Append(Branch.IfTrue(trueLabel));

							orMethod.Append(new LoadLocal(new LocalVariable(1, null, false, true)));
							orMethod.Append(Branch.IfTrue(trueLabel));

							orMethod.Append(LoadConstant.False());
							orMethod.Append(new SimpleInstruction(Opcode.Ret));

							orMethod.Append(trueLabel);
							orMethod.Append(LoadConstant.True());
							orMethod.Append(new SimpleInstruction(Opcode.Ret));

							return lambdaOpClass.DeclareMethod(orMethod);
						}
					case LambdaOperator.Xor:
						{
							var xorMethod = new BytecodeMethod(name, AccessLevel.Public, Splat.None,
								new Parameter("a", null), new Parameter("b", null));
							xorMethod.IsStatic = true;
							xorMethod.IsImplDetail = true;

							// "a xor b" is basically the same as "bool(a) != bool(b)".
							// We can represent this as follows:
							//    if a {
							//        if not b:
							//            return true;
							//    } else {
							//        // a is falsy here
							//        if b:
							//            return true;
							//    }
							//    return false;
							// Though it doesn't matter which order we test a and b in;
							// both of them need to be tested, always. In this method,
							// we test b first.

							var firstFalseLabel = new Label();
							var retFalseLabel = new Label();

							xorMethod.Append(new LoadLocal(new LocalVariable(0, null, false, true))); // a
							xorMethod.Append(new LoadLocal(new LocalVariable(1, null, false, true))); // b

							xorMethod.Append(Branch.IfFalse(firstFalseLabel)); // if not b, branch to else
							{ // if b
								xorMethod.Append(Branch.IfTrue(retFalseLabel)); // if a and b, return false
								xorMethod.Append(LoadConstant.True());
								xorMethod.Append(new SimpleInstruction(Opcode.Ret));
							}
							xorMethod.Append(firstFalseLabel);
							{ // else (if not b)
								xorMethod.Append(Branch.IfFalse(retFalseLabel)); // if not a and not b, return false
								xorMethod.Append(LoadConstant.True());
								xorMethod.Append(new SimpleInstruction(Opcode.Ret));
							}

							xorMethod.Append(retFalseLabel);
							xorMethod.Append(LoadConstant.False());
							xorMethod.Append(new SimpleInstruction(Opcode.Ret));

							return lambdaOpClass.DeclareMethod(xorMethod);
						}
					case LambdaOperator.And:
						{
							var andMethod = new BytecodeMethod(name, AccessLevel.Public, Splat.None,
								new Parameter("a", null), new Parameter("b", null));
							andMethod.IsStatic = true;
							andMethod.IsImplDetail = true;

							var falseLabel = new Label();

							andMethod.Append(new LoadLocal(new LocalVariable(0, null, false, true)));
							andMethod.Append(Branch.IfFalse(falseLabel));

							andMethod.Append(new LoadLocal(new LocalVariable(1, null, false, true)));
							andMethod.Append(Branch.IfFalse(falseLabel));

							andMethod.Append(LoadConstant.True());
							andMethod.Append(new SimpleInstruction(Opcode.Ret));

							andMethod.Append(falseLabel);
							andMethod.Append(LoadConstant.False());
							andMethod.Append(new SimpleInstruction(Opcode.Ret));

							return lambdaOpClass.DeclareMethod(andMethod);
						}
					case LambdaOperator.Inequality:
						{
							var neqMethod = new BytecodeMethod(name, AccessLevel.Public, Splat.None,
								new Parameter("a", null), new Parameter("b", null));
							neqMethod.IsStatic = true;
							neqMethod.IsImplDetail = true;

							var falseLabel = new Label();

							neqMethod.Append(new LoadLocal(new LocalVariable(0, null, false, true)));
							neqMethod.Append(new LoadLocal(new LocalVariable(1, null, false, true)));
							neqMethod.Append(new SimpleInstruction(Opcode.Eq));

							neqMethod.Append(Branch.IfFalse(falseLabel));
							neqMethod.Append(LoadConstant.False()); // If true, return false
							neqMethod.Append(new SimpleInstruction(Opcode.Ret));

							neqMethod.Append(falseLabel);
							neqMethod.Append(LoadConstant.True()); // If false, return true!
							neqMethod.Append(new SimpleInstruction(Opcode.Ret));

							return lambdaOpClass.DeclareMethod(neqMethod);
						}
					case LambdaOperator.Not:
						{
							var notMethod = new BytecodeMethod(name, AccessLevel.Public, Splat.None, new Parameter("a", null));
							notMethod.IsStatic = true;
							notMethod.IsImplDetail = true;

							var falseLabel = new Label();

							notMethod.Append(new LoadLocal(new LocalVariable(0, null, false, true)));
							notMethod.Append(Branch.IfFalse(falseLabel));

							notMethod.Append(LoadConstant.False()); // if a is true, return false
							notMethod.Append(new SimpleInstruction(Opcode.Ret));

							notMethod.Append(falseLabel);
							notMethod.Append(LoadConstant.True()); // if a is false, return true
							notMethod.Append(new SimpleInstruction(Opcode.Ret));

							return lambdaOpClass.DeclareMethod(notMethod);
						}
					default:
						{
							// Regular binary operator
							var binaryMethod = new BytecodeMethod(name, AccessLevel.Public, Splat.None,
								new Parameter("a", null), new Parameter("b", null));
							binaryMethod.IsStatic = true;
							binaryMethod.IsImplDetail = true;
							binaryMethod.Append(new LoadLocal(new LocalVariable(0, null, false, true)));
							binaryMethod.Append(new LoadLocal(new LocalVariable(1, null, false, true)));
							binaryMethod.Append(new SimpleInstruction(GetBinaryOpcode(op)));
							binaryMethod.Append(new SimpleInstruction(Opcode.Ret));
							return lambdaOpClass.DeclareMethod(binaryMethod);
						}
				}
			}
		}

		private Opcode GetBinaryOpcode(LambdaOperator op)
		{
			switch (op)
			{
				case LambdaOperator.Plus: return Opcode.Add;
				case LambdaOperator.Minus: return Opcode.Sub;
				case LambdaOperator.BitwiseOr: return Opcode.Or;
				case LambdaOperator.BitwiseXor: return Opcode.Xor;
				case LambdaOperator.Multiplication: return Opcode.Mul;
				case LambdaOperator.Division: return Opcode.Div;
				case LambdaOperator.Modulo: return Opcode.Mod;
				case LambdaOperator.BitwiseAnd: return Opcode.And;
				case LambdaOperator.Exponentiation: return Opcode.Pow;
				case LambdaOperator.ShiftLeft: return Opcode.Shl;
				case LambdaOperator.ShiftRight: return Opcode.Shr;
				case LambdaOperator.Equality: return Opcode.Eq;
				case LambdaOperator.Inequality: return Opcode.Eq;
				case LambdaOperator.Comparison: return Opcode.Cmp;
				case LambdaOperator.Less: return Opcode.Lt;
				case LambdaOperator.Greater: return Opcode.Gt;
				case LambdaOperator.LessEquals: return Opcode.Lte;
				case LambdaOperator.GreaterEquals: return Opcode.Gte;
				case LambdaOperator.FuncApplication: return Opcode.Apply;
				case LambdaOperator.Concatenation: return Opcode.Concat;
				default: throw new ArgumentException("Invalid LambdaOperator for binary Opcode conversion.", "op");
			}
		}

		private Opcode GetUnaryOpcode(LambdaOperator op)
		{
			switch (op)
			{
				case LambdaOperator.Plus: return Opcode.Plus;
				case LambdaOperator.Minus: return Opcode.Neg;
				case LambdaOperator.BitwiseNot: return Opcode.Not;
				default: throw new ArgumentException("Invalid LambdaOperator for unary Opcode conversion.", "op");
			}
		}

		public void Dispose()
		{
			Dispose(disposing: true);
		}

		private void Dispose(bool disposing)
		{
			if (disposing && nativeLibrary != null)
			{
				nativeLibrary.Dispose();
				nativeLibrary = null;
			}
		}

		public void EnsureNativeMethodExists(ParseNode errorNode, string name)
		{
			if (nativeLibrary == null)
				throw new CompileTimeException(errorNode,
					string.Format("Could not resolve __extern method '{0}': there is no native library loaded.",
						name));

			if (!SkipExternChecks && !nativeLibrary.ContainsMethod(name))
				throw new CompileTimeException(errorNode,
					string.Format("The native library '{0}' does not contain an entry point for '{1}'.",
						nativeLibrary.FileName, name));
		}

		/// <summary>
		/// Compiles the Osprey project to a specific output path.
		/// </summary>
		/// <param name="targetPath">The path of the target file of the compilation. Its current contents, if any, will be overwritten.</param>
		/// <exception cref="ArgumentNullException"><paramref name="targetPath"/> is null.</exception>
		/// <exception cref="ParseException">An error occurs while parsing a source file.</exception>
		/// <exception cref="ModuleLoadException">An error occurs while opening a dependent module.</exception>
		/// <exception cref="CompileTimeException">An error occurs during compilation.</exception>
		private void Compile(string targetPath)
		{
			if (targetPath == null)
				throw new ArgumentNullException("target");

			// Note: although I could wrap this entire method in a try-catch and force exceptions
			// into one of the types ParseException, ModuleLoadException or CompileTimeException,
			// I'm not going to. If any exception other than those is thrown, it's either a bug in
			// the parser or compiler, or it's an I/O error from somewhere. We don't want to catch
			// bugs (because they're bugs), nor do we want to catch I/O errors (because they're out
			// of our control; the most we can do is check that files exist before opening them).
			//
			// The only thing we do (in inner methods) is to catch certain exception types, so that
			// we can assign to the FileName or Document property. This enables the program to report
			// an error location, which seems beneficial!

			var parseTimer = new Stopwatch();

			// Step 1: Read all the source files and resolve all use directives.
			// This also loads dependent modules.
			
			parseTimer.Start();
			ProcessFiles();
			parseTimer.Stop();

			Notice("Time taken to parse (ms): " + parseTimer.Elapsed.TotalMilliseconds.ToStringInvariant());

			// And now we start the real work.

			if (!SilenceNotices)
			{
				Notice("Starting compilation at " + DateTime.Now);
				foreach (var file in sourceFiles.Keys)
					Notice("Source file: " + file);
			}

			var compileTimer = Stopwatch.StartNew();

			// Step 2: Using the parse trees we have, build a hierarchical structure of global names
			// and their respective members. During this step, we do NOT initialize class members.
			BuildProjectNamespace();

			if (extraConstants != null)
				DeclareExtraConstants();

			// Step 3: Resolve the type names of base classes. Every type must have a base class,
			// except aves.Object, which is treated specially. We have the following default base
			// classes (if the type does not explicitly declare a base class):
			//    class    => aves.Object
			//    enum     => aves.Enum
			//    enum set => aves.EnumSet
			// If these classes cannot be found when needed, the standard module is broken.
			// During this step, quite sneakily, we also import namespaces for each document.
			// I could break it into another step, but I don't want to.
			ResolveBaseTypeNames();

			// Step 4: Initialize class members. We save this step for later because all class
			// members must be checked against the base class. For example, if the base class defines
			// a public non-overridable property called 'foo', then a derived class cannot define
			// a member with the same name (there is no way to distinguish between them). Only
			// private members are allowed to be "hidden" in derived classes, because they are
			// invisible there anyway.
			InitializeClassMembers();

			// Step 5: Resolve all names, in every expression in every source file. During this step,
			// we also mark variables as captured, if they are indeed captured, but the actual trans-
			// formations take place later.
			ResolveAllNames();

			// Step 6: Constant folding, where we reduce every expression to a ConstantExpression as far
			// as possible. During this phase, we also ensure that every constant has a constant value;
			// it's easy to do while we're at it anyway.
			FoldConstant();

			// Step 7: Local function extraction! By populating methodsWithLocalFunctions, we've managed
			// to find out where all the methods with local functions are (duh), so we can now extract
			// them appropriately. These transformations may involve the creation of closure classes, and
			// the whole process is described elsewhere.
			if (methodsWithLocalFunctions != null)
			{
				ExtractLocalFunctions();
				methodsWithLocalFunctions = null; // We no longer need this, so let it be at peace.
			}

			// Step 8: Extract generator classes, if there are any.
			if (generatorMethods != null)
			{
				ExtractGeneratorClasses();
				generatorMethods = null; // Goodbye, dear hash set
			}

			// Step 9: Prepare the output module, which involves adding a bunch of definitions and stuff,
			// not only for global members, but for everything.
			PrepareOutputModule();

			// Step 10: Compile method bodies! This is when we really do things: each method gets a MethodBuilder,
			// which we populate with appropriate instructions, and then all the byte contents of each MethodBuilder
			// are added to a byte buffer, which becomes the method block of the output module.
			BuildMethodBodies();

			// Step 11: Add ref names to output, as they need to be in the strings table. During this step, we also
			// lock all the ref tables, because they should not change any longer.
			AddRefNamesToOutput();

			// Sanity check: we should not encounter any constant string values that haven't been added to this table.
			// So we lock it to prevent further modification; if something throws, there's a bug in the compiler.
			outputModule.Members.Strings.Lock();

			compileTimer.Stop();

			// Step 12: Save the output module. Once we've done this, we're all done!
			// Saving a module requires a lot of seeking back and forth, because certain
			// structures in the file format need to be prefixed with their size, which
			// usually cannot be calculated without actually emitting data. Seeking appears
			// to be relatively slow, so we write to a memory stream first, then write
			// the resulting buffer to the file in one go.
			// The vast majority of modules are small, and .NET can deal with large byte
			// arrays with no difficulty.
			var emitTimer = new Stopwatch();
			long bytesWritten;
			using (var stream = new MemoryStream(65536))
			{
				emitTimer.Start();

				outputModule.Save(stream);

				bytesWritten = stream.Position;

				// CopyTo reads from the current position, so reset first!
				stream.Position = 0;
				using (var fileStream = File.Open(targetPath, FileMode.Create, FileAccess.Write, FileShare.None))
					stream.CopyTo(fileStream);

				emitTimer.Stop();
			}

			Notice("Compilation finished at " + DateTime.Now);
			Notice("Time taken to compile (ms): " + compileTimer.Elapsed.TotalMilliseconds.ToStringInvariant());
			Notice("Time taken to emit bytes (ms): " + emitTimer.Elapsed.TotalMilliseconds.ToStringInvariant());

			var totalTime = parseTimer.Elapsed + compileTimer.Elapsed + emitTimer.Elapsed;
			Notice("Total time taken (ms): " + totalTime.TotalMilliseconds.ToStringInvariant());
			Notice("Total bytes written: " + bytesWritten.ToStringInvariant());
		}

		private void ProcessFiles()
		{
			Notice("Parsing source files and adding dependent modules...", CompilerVerbosity.Verbose);

			var projectNs = new Namespace();
			var importedModules = new HashSet<string>();
			this.importedModules = importedModules;
			var modules = new ModulePool(libraryPath, projectNs, this);

			Version foundVersion = null;
			string versionFile = null;

			// Always load the standard module first.
			if (!NoStandardModule)
			{
				importedModules.Add(StandardNames.StandardModuleName);
				modules.StandardModule = modules.GetOrLoad(StandardNames.StandardModuleName, null);
			}

			var newFiles = new HashSet<string>(sourceFiles.Select(kvp => Path.GetFullPath(kvp.Key)));

			while (newFiles.Count > 0)
			{
				var newNewFiles = new HashSet<string>();
				foreach (var fileName in newFiles)
				{
					Document doc;
					if (sourceFiles.TryGetValue(fileName, out doc) && doc != null)
						continue;

					var sourceFile = SourceFile.Open(fileName, computeHash: UseDebugSymbols);

					doc = Parser.Parse(sourceFile, ParserOptions);

					if (doc.Version != null)
					{
						if (foundVersion != null && foundVersion != doc.Version)
							throw new ParseException(doc,
								string.Format("Version number mismatch; found version {0} in '{1}'.", foundVersion, versionFile));
						foundVersion = doc.Version;
						versionFile = fileName;
					}

					doc.Compiler = this;

					sourceFiles[fileName] = doc; // Add first, to avoid self-dependency issues
					documents.Add(doc);
					ProcessImports(newNewFiles, importedModules, doc);
				}
				newFiles = newNewFiles;
			}

			foreach (var modName in importedModules)
				if (!modules.HasLoaded(modName, null))
				{
					Notice(CompilerVerbosity.Verbose, "Loading module '{0}'.", modName);
					modules.GetOrLoad(modName, null);
				}

			this.projectNamespace = projectNs;
			this.modules = modules;
			this.version = foundVersion ?? new Version(1, 0, 0, 0);

			Notice("Finished parsing source files and reading dependent modules.", CompilerVerbosity.Verbose);

			if (metadataFile != null)
			{
				Notice(CompilerVerbosity.Verbose, "Reading metadata from '{0}'...", metadataFile);
				metadata = Parser.ParseMetadata(File.ReadAllText(metadataFile));
				Notice(CompilerVerbosity.Verbose, "Finished reading {0} entr{1} of metadata.", metadata.Count, metadata.Count == 1 ? "y" : "ies");
			}
			else
				metadata = new Dictionary<string, string>();

			if (!metadata.ContainsKey("date"))
				metadata["date"] = DateTime.UtcNow.ToString("yyyy-MM-ddTHH\\:mm\\Z", CI.InvariantCulture);

			if (!metadata.ContainsKey("compiler"))
			{
				var assemblyVersion = Assembly.GetExecutingAssembly().GetName().Version;
				metadata["compiler"] = string.Format("Osprey Compiler v{0}", assemblyVersion);
			}
		}

		private void ProcessImports(HashSet<string> newFiles, HashSet<string> modules, Document doc)
		{
			var docFile = doc.SourceFile.FileName;
			foreach (var use in doc.Uses)
				if (use is UseFileDirective)
				{
					var fileName = ((UseFileDirective)use).Name.StringValue;

					// Make the file name absolute, but resolve it relative to the document!
					// If you have the following file structure:
					//   `-- \main.osp
					//   `-- \aux.osp
					//   `-- \inc
					//     `-- main.osp
					//     `-- aux.osp
					// and both of the main.osp files say 'use "aux.osp";', then they refer to the file named
					// aux.osp in their respective containing directories.
					var realFile = Path.GetFullPath(Path.Combine(Path.GetDirectoryName(docFile), fileName));

					// If we haven't already parsed this file, add it to newFiles to be processed
					// This takes care of circular dependencies, e.g.:
					//   a.osp:
					//     use "b.osp";
					//   b.osp:
					//     use "a.osp";
					if (realFile == docFile)
						throw new CompileTimeException(use, "A source file may not include itself.");
					if (!sourceFiles.ContainsKey(realFile))
					{
						Notice(CompilerVerbosity.ExtraVerbose,
							"Adding source file '{0}' to project, referenced in '{1}'",
							realFile, docFile);
						newFiles.Add(realFile);
					}
				}
				else if (use is UseModuleDirective)
				{
					var modName = ((UseModuleDirective)use).Name.Parts.JoinString(".");
					modules.Add(modName);
					Notice(CompilerVerbosity.ExtraVerbose,
						"Adding reference to module '{0}', from '{1}'",
						modName, docFile);
				}
		}

		private void BuildProjectNamespace()
		{
			for (var i = 0; i < documents.Count; i++)
				try
				{
					var doc = documents[i];
					doc.Namespace = new FileNamespace(projectNamespace, this);
					ProcessNamespaceMembers(doc.GlobalDeclarationSpace, projectNamespace);
				}
				catch (CompileTimeException e)
				{
					e.Document = documents[i];
					throw; // rethrow
				}

			// If we're compiling the project as an application, we need to gather up
			// all the global statements into a main method in this phase too.
			var mainMethodContents = new TempList<Statement>();

			for (var i = 0; i < documents.Count; i++)
				try
				{
					InitializeGlobalVariables(documents[i], ref mainMethodContents);
				}
				catch (CompileTimeException e)
				{
					e.Document = documents[i];
					throw;
				}

			if (projectType == ProjectType.Application)
			{
				// Pretend there's a main method that belongs to the project namespace.
				// There isn't, of course, but that won't stop us!
				// NOTE: This method will never actually be /declared/ in the project namespace;
				// we just pretend it's in there. The compiler will still output metadata about
				// the existence of a method with the fully qualified name "<main>", so we can
				// reference it as the main method of the module.

				Notice("Initializing main method...", CompilerVerbosity.Verbose);
				mainMethodBody = new Block(mainMethodContents.ToArray());
				mainMethod = new Method(null, DefaultMainMethodName, AccessLevel.Private, mainMethodBody, Splat.None, null);

				var mainMethodGroup = new MethodGroup(DefaultMainMethodName, projectNamespace, AccessLevel.Private);
				mainMethodGroup.AddOverload(mainMethod);
				AddGlobalFunction(mainMethodGroup); // Always first!
			}

			// If an explicit main method was specified, we now have enough information
			// to locate it, so let's try doing that.
			if (mainMethodName != null)
			{
				var nameParts = mainMethodName.Split(Dot);

				int lastIndex;
				NamedMember member;
				switch (FindNamespace(nameParts, 0, nameParts.Length - 1, out member, out lastIndex))
				{
					case NamespaceLookupResult.Success:
						{
							var ns = (Namespace)member;
							var lastPart = nameParts[nameParts.Length - 1];
							if (!ns.ContainsMember(lastPart))
								throw new CompileTimeException(null, string.Format(
									"Cannot use '{0}.{1}' as the main method because it does not exist.",
									ns.FullName, lastPart));

							// The main method must be a method group
							member = ns.GetMember(lastPart);
							if (member.Kind != MemberKind.MethodGroup)
								throw new CompileTimeException(null, string.Format(
									"Cannot use '{0}' as the main method because it is not a global function.",
									member.FullName));

							// And it must come from the module we're building; can't be imported.
							var mainMethodGroup = (MethodGroup)member;
							// Note: MethodGroup.Module is null until we've actually constructed our module.
							if (mainMethodGroup.Module != null)
								throw new CompileTimeException(null, string.Format(
									"Cannot use an imported method ('{0}') as the main method.",
									mainMethodGroup.FullName));

							// And it must have exactly one overload, which must take 1 or 0 arguments.
							// (If it takes one argument, it gets an aves.List of command-line arguments.)
							mainMethod = mainMethodGroup.FindOverload(1) ?? mainMethodGroup.FindOverload(0);
							if (mainMethodGroup.Count != 1 || mainMethod == null)
								throw new CompileTimeException(null, string.Format(
									"The specified main method ('{0}') must have exactly one overload, which must take one or zero arguments.",
									mainMethodGroup.FullName));
						}
						break;
					case NamespaceLookupResult.IntermediateNamespaceMissing:
						throw new CompileTimeException(null, string.Format(
							"Cannot use '{0}' as the main method because the intermediate namespace '{1}.{2}' does not exist",
							mainMethodName, member.FullName, nameParts[lastIndex]));
					case NamespaceLookupResult.MemberIsNotNamespace:
						throw new CompileTimeException(null, string.Format(
							"Cannot use '{0}' as the main method because '{1}' is not a namespace.",
							mainMethodName, member.FullName));
				}
			}
		}

		private void ProcessNamespaceMembers(NamespaceDeclaration nsDecl, Namespace parent)
		{
			// nsDecl.Name should only be null for the file's global declaration space,
			// when the file doesn't have 'namespace blah;' at the top, and in that case
			// parent will be the project namespace.
			var ns = nsDecl.Name == null ? parent : parent.GetNamespace(nsDecl.Name.Parts);
			nsDecl.Namespace = ns;

			foreach (var typeDecl in nsDecl.Types)
			{
				Type type;
				if (typeDecl is ClassDeclaration)
					type = new Class((ClassDeclaration)typeDecl, ns);
				else if (typeDecl is EnumDeclaration)
					type = new Enum((EnumDeclaration)typeDecl, ns);
				else
					throw new Exception("Internal error: type declaration was neither ClassDeclaration nor EnumDeclaration.");
				typeDecl.Type = type;
				ns.DeclareType(type);
				AddType(type);
				// (Don't initialize the types yet!)
			}

			foreach (var funcDecl in nsDecl.Functions)
			{
				var method = new Method(funcDecl);
				var group = ns.DeclareMethod(method);
				if (group.Count == 1) // If the group has one overload, then the one we just added is the first one!
					AddGlobalFunction(group);
				method.InitBody(this);
			}

			foreach (var constDecl in nsDecl.Constants)
			{
				var declarators = constDecl.Declaration.Declarators;
				var constants = new GlobalConstant[declarators.Length];
				for (var i = 0; i < declarators.Length; i++)
				{
					var varDecl = declarators[i];
					var constant = new GlobalConstant(varDecl.Name, varDecl, constDecl.IsPublic);
					ns.DeclareConstant(constant);
					AddGlobalConstant(constant);
					constants[i] = constant;
				}
				constDecl.Constants = constants;
			}

			foreach (var subNs in nsDecl.Namespaces)
				ProcessNamespaceMembers(subNs, ns);
		}

		private void InitializeGlobalVariables(Document doc, ref TempList<Statement> mainMethodBody)
		{
			if (doc.Statements.Length > 0)
			{
				if (projectType == ProjectType.Module)
					throw new CompileTimeException(doc.Statements[0],
						"A project compiled as a module may not contain any global statements or global variables.");

				foreach (var stmt in doc.Statements)
				{
					if (stmt is SimpleLocalVariableDeclaration) // var a = x;
					{
						foreach (var decl in ((SimpleLocalVariableDeclaration)stmt).Declarators)
						{
							var globalVar = new GlobalVariable(decl.Name, decl, doc);
							decl.Variable = globalVar;
							doc.Namespace.DeclareGlobalVariable(globalVar);
						}
					}
					else if (stmt is ParallelLocalVariableDeclaration) // var (a, b) = list;
					{
						var declaration = (ParallelLocalVariableDeclaration)stmt;
						declaration.Variables = new Variable[declaration.Names.Length];
						for (var i = 0; i < declaration.Names.Length; i++)
						{
							var name = declaration.Names[i];

							var globalVar = new GlobalVariable(name,
								new VariableDeclarator(name, null)
								{
									StartIndex = stmt.StartIndex,
									EndIndex = stmt.EndIndex,
									Document = stmt.Document,
								}, doc);
							declaration.Variables[i] = globalVar;
							doc.Namespace.DeclareGlobalVariable(globalVar);
						}
					}
					mainMethodBody.Add(stmt);
				}
			}
		}

		private void DeclareExtraConstants()
		{
			Notice("Declaring command-line constants...", CompilerVerbosity.Verbose);
			foreach (var kvp in extraConstants)
			{
				var nameParts = kvp.Key.Split(Dot);

				int lastIndex;
				NamedMember member;
				switch (FindNamespace(nameParts, 0, nameParts.Length - 1, out member, out lastIndex))
				{
					case NamespaceLookupResult.Success:
						{
							var ns = (Namespace)member;

							var lastPart = nameParts[nameParts.Length - 1];
							if (!ns.ContainsMember(lastPart))
							{
								ns.DeclareConstant(new GlobalConstant(lastPart,
									ConstantValue.CreateBoolean(kvp.Value), AccessLevel.Private));
								Notice(CompilerVerbosity.Verbose, "Declared constant: {0} = {1}", kvp.Key, kvp.Value);
							}
							else
								Warning(CompilerVerbosity.NotVerbose,
									"Could not declare constant '{0}': a member by that name already exists.",
									kvp.Key);
						}
						break;
					case NamespaceLookupResult.IntermediateNamespaceMissing:
						Warning(CompilerVerbosity.NotVerbose,
							"Could not declare constant '{0}': intermediate namespace '{1}.{2}' does not exist.",
							kvp.Key, member.FullName, nameParts[lastIndex]);
						break;
					case NamespaceLookupResult.MemberIsNotNamespace:
						Warning(CompilerVerbosity.NotVerbose,
							"Could not declare constant '{0}': '{1}' is not a namespace.",
							kvp.Key, member.FullName);
						break;
				}
			}
			Notice("Finished adding command-line constants.", CompilerVerbosity.Verbose);

			extraConstants = null;
		}

		private NamespaceLookupResult FindNamespace(string[] path, int index, int count, out NamedMember member, out int lastIndex)
		{
			member = projectNamespace;
			lastIndex = index;

			var ns = projectNamespace;

			for (var i = 0; i < count; i++)
			{
				var name = path[index + i];
				if (ns.ContainsMember(name))
				{
					member = ns.GetMember(name);
					if (member.Kind == MemberKind.Namespace)
						ns = (Namespace)member;
					else
					{
						lastIndex = index + i;
						return NamespaceLookupResult.MemberIsNotNamespace;
					}
				}
				else
				{
					lastIndex = index + i;
					return NamespaceLookupResult.IntermediateNamespaceMissing;
				}
			}

			return NamespaceLookupResult.Success;
		}

		private void ResolveBaseTypeNames()
		{
			Notice("Resolving base type names...", CompilerVerbosity.Verbose);

			// Prepare some stuff
			try
			{
				FindType(StandardNames.TypeRootName);
				FindType(StandardNames.EnumName);
				FindType(StandardNames.EnumSetName);
			}
			catch (UndefinedNameException e)
			{
				throw new UndefinedNameException(null,
					e.Name, string.Format(NoStandardModule ?
						"The standard type '{0}' is not defined. Since the /nostdlib flag is in use, you must define this type." :
						"The standard type '{0}' could not be found.", e.Name),
					e);
			}

			foreach (var doc in documents)
				try
				{
					foreach (var use in doc.Uses)
					{
						if (use.ResolveNames(projectNamespace, firstPass: true))
							doc.UseDirectivesRequireSecondPass = true;
					}

					// If the document has "namespace blah;" at the top,
					// then we act as if it also had "use namespace blah;",
					// for the benefit of global statements.
					if (doc.GlobalDeclarationSpace.Name != null)
						doc.Namespace.ImportNamespace(doc.GlobalDeclarationSpace.Namespace);

					ResolveBaseTypeNames(doc.GlobalDeclarationSpace, doc.Namespace);
				}
				catch (CompileTimeException e)
				{
					e.Document = doc;
					throw;
				}

			Notice("All base type names were resolved successfully.", CompilerVerbosity.Verbose);
		}

		private void ResolveBaseTypeNames(NamespaceDeclaration nsDecl, FileNamespace doc)
		{
			foreach (var typeDecl in nsDecl.Types)
			{
				var type = typeDecl.Type;
				if (type == ObjectType)
				{
					ValidateObjectType(typeDecl, type);
					continue;
				}

				if (typeDecl is ClassDeclaration)
				{
					var baseTypeName = ((ClassDeclaration)typeDecl).BaseClass;
					if (baseTypeName == null)
						type.BaseType = ObjectType;
					else
						type.BaseType = nsDecl.Namespace.ResolveTypeName(baseTypeName, doc);

					if (type.BaseType is Enum)
						throw new CompileTimeException(baseTypeName,
							"Cannot inherit from an enum type.");
					else if (!((Class)type.BaseType).IsInheritable &&
						!((Class)type.BaseType).IsAbstract)
						throw new CompileTimeException(baseTypeName,
							"Only inheritable classes can be inherited from.");

					if (type.BaseType == type)
						throw new CompileTimeException(baseTypeName,
							"A class cannot inherit from itself.");

					if (type.BaseType == EnumType && type != EnumSetType)
						throw new CompileTimeException(baseTypeName,
							"The type aves.Enum cannot be inherited from explicitly, except by aves.EnumSet. Create an enum instead.");
					if (type.BaseType == EnumSetType)
						throw new CompileTimeException(baseTypeName,
							"The type aves.EnumSet cannot be inherited from explicitly. Create an enum set instead.");
				}
				else // EnumDeclaration
					type.BaseType = ((EnumDeclaration)typeDecl).IsSet ? EnumSetType : EnumType;

				if (type == EnumType)
					ValidateEnumType(typeDecl, type);
				else if (type == EnumSetType)
					ValidateEnumSetType(typeDecl, type);

				if (verbosity > CompilerVerbosity.NotVerbose && !SilenceNotices)
					Notice(CompilerVerbosity.ExtraVerbose, "Initialized base type of '{0}' to '{1}'.",
						type.FullName, type.BaseType.FullName);
			}

			foreach (var subNs in nsDecl.Namespaces)
				ResolveBaseTypeNames(subNs, doc);
		}

		private void ValidateObjectType(TypeDeclaration typeDecl, Type type)
		{
			if (!(typeDecl is ClassDeclaration))
				throw new CompileTimeException(typeDecl, "Invalid aves.Object declaration: must be a class.");

			var classDecl = (ClassDeclaration)typeDecl;
			if (classDecl.IsStatic || classDecl.IsAbstract || classDecl.IsPrimitive ||
				!classDecl.IsInheritable || classDecl.Access == AccessLevel.Private)
				throw new CompileTimeException(classDecl, "Invalid aves.Object declaration: cannot be marked static, abstract, __primitive or private, and must be marked inheritable. Otherwise you'll break the type hierarchy!");
			if (classDecl.BaseClass != null)
				throw new CompileTimeException(classDecl,
					"Invalid aves.Object declaration: cannot have any declared base type. This is the root of the type hierarchy!");

			// The only type that is allowed to have a null base type.
			type.BaseType = null;

			Notice("Initialized base type of aves.Object to null.", CompilerVerbosity.Verbose);
			Notice("Type aves.Object was declared correctly.", CompilerVerbosity.Verbose);
		}

		private void ValidateEnumType(TypeDeclaration typeDecl, Type type)
		{
			if (!(typeDecl is ClassDeclaration))
				throw new CompileTimeException(typeDecl, "Invalid aves.Enum declaration: must be a class.");

			var classDecl = (ClassDeclaration)typeDecl;
			if (classDecl.IsStatic || classDecl.IsPrimitive || classDecl.IsInheritable ||
				!classDecl.IsAbstract || classDecl.Access == AccessLevel.Private)
				throw new CompileTimeException(classDecl, "Invalid aves.Enum declaration: cannot be marked static, __primitive, inheritable or private, and must be marked abstract.");
			if (type.BaseType != ObjectType)
				throw new CompileTimeException(classDecl.BaseClass, "Invalid aves.Enum declaration: must inherit from aves.Object.");

			Notice("Type aves.Enum was declared correctly.", CompilerVerbosity.Verbose);
		}

		private void ValidateEnumSetType(TypeDeclaration typeDecl, Type type)
		{
			if (!(typeDecl is ClassDeclaration))
				throw new CompileTimeException(typeDecl, "Invalid aves.EnumSet declaration: must be a class.");

			var classDecl = (ClassDeclaration)typeDecl;
			if (classDecl.IsStatic || classDecl.IsPrimitive || classDecl.IsInheritable ||
				!classDecl.IsAbstract || classDecl.Access == AccessLevel.Private)
				throw new CompileTimeException(classDecl, "Invalid aves.EnumSet declaration: cannot be marked static, __primitive, inheritable or private, and must be marked abstract.");
			if (type.BaseType != EnumType)
				throw new CompileTimeException(classDecl.BaseClass, "Invalid aves.EnumSet declaration: must inherit from aves.Enum.");

			Notice("Type aves.EnumSet was declared correctly.", CompilerVerbosity.Verbose);
		}

		private void InitializeClassMembers()
		{
			Notice("Initializing all members with bodies...", CompilerVerbosity.Verbose);
			
			foreach (var doc in documents)
				try
				{
					InitializeClassMembers(doc.GlobalDeclarationSpace);

					if (doc.UseDirectivesRequireSecondPass)
						foreach (var use in doc.Uses)
							use.ResolveNames(projectNamespace, firstPass: false);
				}
				catch (CompileTimeException e)
				{
					e.Document = doc;
					throw;
				}

			if (projectType == ProjectType.Application)
				try
				{
					mainMethod.InitBody(this);
				}
				catch (CompileTimeException e)
				{
					// The main method may consist of statments from
					// more than one document, so we must rely on the
					// information in the node.
					e.Document = e.Node.Document;
					throw;
				}

			Notice("All member bodies were initialized successfully.", CompilerVerbosity.Verbose);
		}

		private void InitializeClassMembers(NamespaceDeclaration nsDecl)
		{
			foreach (var type in nsDecl.Types)
				if (type is ClassDeclaration)
				{
					var @class = (Class)type.Type;
					@class.Init(this);
					if (@class.HasUnimplementedAbstractMethods)
						throw new DeclarationException(type,
							string.Format("The class '{0}' does not implement the following inherited abstract methods: {1}",
								@class.FullName,
								@class.GetUnimplementedAbstractMethodNames()));
				}

			foreach (var subNs in nsDecl.Namespaces)
				InitializeClassMembers(subNs);
		}

		private void ResolveAllNames()
		{
			Notice("Resolving all names...", CompilerVerbosity.Verbose);

			Document doc = null;
			try
			{
				// Names are resolved in two passes: first all global and class constants, then
				// everything else. We do this so that said constants can be folded as necessary,
				// without having to worry about whether their names have been resolved.

				var docCount = documents.Count;

				for (var i = 0; i < docCount; i++)
				{
					doc = documents[i];
					ResolveAllNames(doc.GlobalDeclarationSpace, doc.Namespace, firstPass: true);
				}

				for (var i = 0; i < docCount; i++)
				{
					doc = documents[i];
					ResolveAllNames(doc.GlobalDeclarationSpace, doc.Namespace, firstPass: false);

					// Statements must be resolved relative to the document's FileNamespace, because
					// of global variables. We cannot loop through the main method body separately.
					var reachable = true;
					foreach (var stmt in doc.Statements)
					{
						stmt.ResolveNames(mainMethodBody.DeclSpace, doc.Namespace, reachable);
						if (reachable && !stmt.IsEndReachable)
							reachable = false;
					}
				}
			}
			catch (CompileTimeException e)
			{
				e.Document = doc;
				throw;
			}

			if (projectType == ProjectType.Application && mainMethod.HasLocalFunctions)
				AddMethodWithLocalFunctions(mainMethod);

			Notice("All names were resolved successfully.", CompilerVerbosity.Verbose);
		}

		private void ResolveAllNames(NamespaceDeclaration nsDecl, FileNamespace doc, bool firstPass)
		{
			var context = nsDecl.Namespace;

			if (firstPass)
			{
				foreach (var constant in nsDecl.Constants)
					foreach (var decl in constant.Declaration.Declarators)
						decl.Initializer = decl.Initializer.ResolveNames(context, doc);
			}

			foreach (var type in nsDecl.Types)
				type.ResolveNames(context, doc, this, firstPass);

			if (!firstPass)
			{
				foreach (var function in nsDecl.Functions)
				{
					function.Function.ResolveNames(context, doc, true);
					if (function.DeclSpace.HasLocalFunctions)
						AddMethodWithLocalFunctions(function.DeclSpace);
					if (function.DeclSpace.IsGenerator)
						AddGeneratorMethod(function.DeclSpace);
				}
			}

			foreach (var subNs in nsDecl.Namespaces)
				ResolveAllNames(subNs, doc, firstPass);
		}

		private void FoldConstant()
		{
			Notice("Performing constant folding...", CompilerVerbosity.Verbose);

			foreach (var doc in documents)
				try
				{
					FoldConstant(doc.GlobalDeclarationSpace);

					foreach (var stmt in doc.Statements)
						stmt.FoldConstant();
				}
				catch (CompileTimeException e)
				{
					e.Document = doc;
					throw;
				}

			Notice("All expressions were reduced successfully.", CompilerVerbosity.Verbose);
		}

		private void FoldConstant(NamespaceDeclaration nsDecl)
		{
			foreach (var constant in nsDecl.Constants)
				constant.FoldConstant();

			foreach (var type in nsDecl.Types)
				type.FoldConstant();

			foreach (var func in nsDecl.Functions)
				func.Function.FoldConstant();

			foreach (var subNs in nsDecl.Namespaces)
				FoldConstant(subNs);
		}

		private void ExtractLocalFunctions()
		{
			Notice("Extracting local functions and closure classes...", CompilerVerbosity.Verbose);

			foreach (var method in methodsWithLocalFunctions)
			{
				Class @class;
				Namespace ns;
				if (method is LocalMethod)
				{
					bool _;
					var func = ((LocalMethod)method).Function;
					@class = func.GetContainingClass(out _);
					ns = func.GetContainingNamespace();

					ProcessLocalFunction(func, null, @class, ns);

					// This branch is only reached if the LocalMethod is a lambda
					// expression inside a field initializer, in which case there
					// is a local extractor in additionalLocalExtractors that takes
					// care of transforming closure locals. So, we don't actually
					// call TransformClosureLocals on the body, because doing that
					// more than once is bad.
				}
				else
				{
					@class = method.Group.ParentAsClass;
					if (@class == null)
						ns = method.Group.ParentAsNamespace;
					else
						ns = @class.Parent;
					
					// ProcessLocalFunction walks through all the nested local functions,
					// so if we have a LocalMethod, the code above takes care of that.
					if (method.LocalFunctions != null)
						foreach (var function in method.LocalFunctions)
							ProcessLocalFunction(function, method, @class, ns);

					method.Body.Node.TransformClosureLocals(null, false);
				}
			}

			if (additionalLocalExtractors != null)
			{
				foreach (var extractor in additionalLocalExtractors)
					extractor();
				additionalLocalExtractors = null;
			}

			Notice("Finished extracting local functions and closure classes.", CompilerVerbosity.Verbose);
		}

		private void ProcessLocalFunction(LocalFunction function, Method parentMethod, Class parentClass, Namespace parentNs)
		{
			var localMethod = function.Method;

			if (!function.HasCaptures)
			{
				// If the function doesn't capture any variables, then:
				//   * If it captures 'this', then it becomes a private instance method in parentClass.
				//   * Otherwise:
				//     * If it's in a class method, it becomes a private static method in the same class.
				//     * If it's in a global function, it becomes a private global function in the same namespace.

				localMethod.IsStatic = !function.CapturesThis;
				function.CompilationStrategy = function.CapturesThis ?
					LocalFunctionCompilationStrategy.InstanceMethod :
					LocalFunctionCompilationStrategy.StaticMethod;

				// Rename the method to make it unique within the container!
				localMethod.Name = LocalMethod.GetExtractedName(parentMethod ?? localMethod, localMethod);
				localMethod.Access = AccessLevel.Private; // hidd'n from view

				if (parentClass != null)
					parentClass.DeclareMethod(localMethod);
				else
				{
					var group = parentNs.DeclareMethod(localMethod);
					AddGlobalFunction(group);
				}
			}
			else
			{
				localMethod.Access = AccessLevel.Public; // not hidd'n, technically speakin'
				localMethod.IsStatic = false;

				// If the function captures variables, then we need to create a closure class for
				// the deepest nested block from which the function captures variables.
				var closure = function.DeepestCapturedBlock.GenerateClosureClass(this);
				// Although the closure class initializer adds fields for all captured variables, it does NOT
				// declare local functions as methods, so we must do that explicitly.
				closure.DeclareFunctionMethod(function);
				if (function.CapturesThis)
					closure.DeclareThisField();

				function.CompilationStrategy = LocalFunctionCompilationStrategy.ClosureMethod;
			}

			if (localMethod.LocalFunctions != null)
				foreach (var innerFunction in localMethod.LocalFunctions)
					ProcessLocalFunction(innerFunction, parentMethod ?? localMethod, parentClass, parentNs);
		}

		private void ExtractGeneratorClasses()
		{
			Notice("Extracting generator classes...", CompilerVerbosity.Verbose);

			foreach (var method in generatorMethods)
			{
				var @class = method.GenerateGeneratorClass(this);
				AddType(@class);
			}

			Notice("Finished extracting generator classes.", CompilerVerbosity.Verbose);
		}

		private void PrepareOutputModule()
		{
			var outputModule = new Module(modules, this.moduleName, version);
			outputModule.Metadata = this.metadata;
			outputModule.NativeLib = this.nativeLibrary == null ? null : Path.GetFileName(this.nativeLibrary.FileName);

			if (!NoStandardModule)
				// Note: we have to add a reference to the standard module, even if the code doesn't explicitly
				// use any members from it. Otherwise the runtime won't load it.
				// It must also be the first ModuleRef, or the runtime will load things in the wrong order.
				outputModule.AddModuleRef(modules.StandardModule);

			// Note: although the FullName is computed each time you call the property getter,
			// it's internally cached by OrderBy. So there's no performance drawback!

			if (types != null)
				foreach (var type in types.OrderBy(t => t.FullName, StringComparer.InvariantCultureIgnoreCase))
					AddTypeToOutput(type, outputModule);

			if (mainMethod != null)
			{
				var mainMethodGroup = mainMethod.Group;
				outputModule.MainMethod = mainMethodGroup;
				mainMethodGroup.Module = outputModule;
			}

			if (globalFunctions != null)
				foreach (var func in globalFunctions.OrderBy(f => f.FullName, StringComparer.InvariantCultureIgnoreCase))
				{
					outputModule.GetStringId(func.FullName);
					func.Module = outputModule;
					func.Id = outputModule.GetMethodId(func);
					Notice(CompilerVerbosity.ExtraVerbose, "Giving ID {0:X8} to function '{1}'.", func.Id, func.FullName);
					AddParametersToOutput(func, outputModule);
				}

			if (globalConstants != null)
				foreach (var constant in globalConstants.OrderBy(c => c.FullName, StringComparer.InvariantCultureIgnoreCase))
				{
					outputModule.GetStringId(constant.FullName);
					constant.Id = outputModule.GetConstantId(constant);
					if (constant.Value.Type != ConstantValueType.Null)
					{
						outputModule.GetTypeId(constant.Value.GetTypeObject(this)); // If the type comes from another module, we must reference it!
						if (constant.Value.Type == ConstantValueType.String)
							outputModule.GetStringId(constant.Value.StringValue);
					}
					Notice(CompilerVerbosity.ExtraVerbose, "Giving ID {0:X8} to constant '{1}'.", constant.Id, constant.FullName);
				}

			// All definitions should now be in place, so we lock the appropriate tables.
			// The ref tables will not be locked, however, because method bodies may still
			// add references to things. Note, also, that the string table remains open
			// for the very same reason.
			var mems = outputModule.Members;
			mems.TypeDefs.Lock();
			mems.FieldDefs.Lock();
			mems.MethodDefs.Lock();
			mems.GlobalFuncDefs.Lock();
			mems.GlobalConstDefs.Lock();

			this.outputModule = outputModule;
		}

		private void AddTypeToOutput(Type type, Module outputModule)
		{
			if (type.Id != 0) return; // Already added!

			if (type.Module == null)
				type.Module = outputModule;
			if (type.BaseType != null && type.BaseType.Id == 0)
				AddTypeToOutput(type.BaseType, outputModule);
			if (type.SharedType != null && type.SharedType.Id == 0)
				AddTypeToOutput(type.SharedType, outputModule);

			outputModule.GetStringId(type.FullName);
			type.Id = outputModule.GetTypeId(type);
			Notice(CompilerVerbosity.ExtraVerbose, "Giving ID {0:X8} to type '{1}'.", type.Id, type.FullName);

			if (type.Module == outputModule)
				// And then we initialize the type's members (if it's a typedef)
				if (type is Enum)
				{
					foreach (var field in ((Enum)type).GetFieldsSorted())
					{
						outputModule.GetStringId(field.Name);
						field.Id = outputModule.GetFieldId(field);
					}
				}
				else // Class
				{
					foreach (var member in ((Class)type).GetMembersSorted())
					{
						if (member is MethodGroup)
						{
							var method = (MethodGroup)member;
							outputModule.GetStringId(method.Name);
							method.Module = outputModule;
							method.Id = outputModule.GetMethodId(method);
							AddParametersToOutput(method, outputModule);
						}
						else if (member is Field)
						{
							outputModule.GetStringId(member.Name);
							((Field)member).Id = outputModule.GetFieldId((Field)member);
						}
						else if (member is ClassConstant)
						{
							var constant = (ClassConstant)member;
							outputModule.GetStringId(constant.Name);
							constant.Id = outputModule.GetFieldId(constant);

							// If the constant is of an imported type (e.g. String, Int),
							// then we must make sure to import it now. If it is of a type
							// declared in this project, we will either have assigned it
							// an ID now, or it will be done later.
							var constType = constant.Value.GetTypeObject(this);
							if (constType.Module != null &&
								constType.Module != outputModule)
								AddTypeToOutput(constType, outputModule);
							if (constant.Value.Type == ConstantValueType.String)
								outputModule.GetStringId(constant.Value.StringValue);
						}
						else if (member is Property || member is IndexerMember)
							outputModule.GetStringId(member.Name);
						// Properties and operators have no IDs
					}
				}
		}

		private void AddParametersToOutput(MethodGroup group, Module outputModule)
		{
			foreach (var overload in group)
				if (overload.Parameters != null)
					foreach (var param in overload.Parameters)
						outputModule.GetStringId(param.DeclaredName);
		}

		private void BuildMethodBodies()
		{
			// Note: mainMethod is already in GlobalFuncDefs
			//if (mainMethod != null)
			//	mainMethod.Compile(this);

			foreach (var kvp in outputModule.Members.MethodDefs)
				foreach (var method in kvp.Value)
					method.Compile(this);

			foreach (var kvp in outputModule.Members.GlobalFuncDefs)
				foreach (var method in kvp.Value)
					method.Compile(this);
		}

		private void AddRefNamesToOutput()
		{
			var outputModule = this.outputModule;
			var members = outputModule.Members;

			members.ModuleRefs.Lock();
			members.TypeRefs.Lock();
			members.GlobalFuncRefs.Lock();
			members.MethodRefs.Lock();
			members.FieldRefs.Lock();

			foreach (var modRef in members.ModuleRefs)
				outputModule.GetStringId(modRef.Value.Name);

			foreach (var typeRef in members.TypeRefs)
				outputModule.GetStringId(typeRef.Value.FullName);

			foreach (var funcRef in members.GlobalFuncRefs)
				outputModule.GetStringId(funcRef.Value.FullName);

			foreach (var methodRef in members.MethodRefs)
				outputModule.GetStringId(methodRef.Value.Name);

			foreach (var fieldRef in outputModule.Members.FieldRefs)
				outputModule.GetStringId(fieldRef.Value.Name);
		}

		private void SaveDebugSymbols(string targetPath)
		{
			Notice("[debug] Writing debug symbols...", CompilerVerbosity.Verbose);

			var fileToIndex = new Dictionary<SourceFile, int>(documents.Count);
			for (var i = 0; i < documents.Count; i++)
				fileToIndex.Add(documents[i].SourceFile, i);

			using (var outStream = new MemoryStream(65536))
			using (var writer = new ModuleWriter(outStream, Encoding.Unicode))
			{
				writer.Write(DebugSymbolsMagicNumber); // magicNumber

				// Source files!
				// There must always be at least one source file, hence documents.Count
				// can never be zero.
				writer.BeginCollection(documents.Count);
				foreach (var doc in documents)
				{
					writer.Write(doc.SourceFile.FileName); // fileName
					writer.Write(doc.SourceFile.FileHash); // hash
				}
				writer.EndCollection();

				// Debug symbols!
				WriteDebugSymbols(writer, fileToIndex);

				outStream.Position = 0;
				using (var outFileStream = File.Create(targetPath))
					outStream.CopyTo(outFileStream);
			}

			Notice("[debug] Finished writing debug symbols.", CompilerVerbosity.Verbose);
		}

		private void WriteDebugSymbols(ModuleWriter writer, Dictionary<SourceFile, int> fileToIndex)
		{
			// We don't know yet just how many methods there will be with debug symbols.
			long sizePos = writer.BaseStream.Position;
			writer.Write(0); // totalOverloadsWithSymbols (placeholder)
			writer.Write(0); // size (placeholder)

			long lengthPos = writer.BaseStream.Position;
			writer.Write(0); // length (placeholder)

			Func<Method, bool> hasSymbols = m => m.CompiledMethod != null && m.CompiledMethod.DebugSymbols != null;

			int totalOverloadsWithSymbols = 0;
			int length = 0;
			foreach (var kvp in outputModule.Members.GlobalFuncDefs.Concat(outputModule.Members.MethodDefs))
			{
				var group = kvp.Value;
				if (group.Any(hasSymbols))
				{
					length++;
					WriteMethodDebugSymbols(writer, fileToIndex, group, ref totalOverloadsWithSymbols);
				}
			}

			long endPos = writer.BaseStream.Position;
			writer.BaseStream.Seek(sizePos, SeekOrigin.Begin);
			writer.Write(totalOverloadsWithSymbols); // totalOverloadsWithSymbols
			writer.Write(checked((int)(endPos - lengthPos))); // size
			writer.Write(length); // length

			Notice(CompilerVerbosity.Verbose,
				"[debug] Wrote debug symbols for {0} method group{1}",
				length, length == 1 ? "" : "s");
		}

		private void WriteMethodDebugSymbols(ModuleWriter writer,
			Dictionary<SourceFile, int> fileToIndex, MethodGroup group,
			ref int totalOverloadsWithSymbols)
		{
			Notice(CompilerVerbosity.ExtraVerbose,
				"[debug] Emitting debug symbols for method '{0}'",
				group.FullName);

			writer.Write(group.Id); // methodId

			writer.BeginCollection(group.Count);
			foreach (var method in group)
			{
				if (method.CompiledMethod == null || method.CompiledMethod.DebugSymbols == null ||
					method.CompiledMethod.DebugSymbols.Length == 0)
				{
					writer.Write(0); // count
					continue;
				}

				totalOverloadsWithSymbols++;

				var debug = method.CompiledMethod.DebugSymbols;
				writer.Write(debug.Length); // count
				for (var i = 0; i < debug.Length; i++)
				{
					var d = debug[i];
					writer.Write(d.BytecodeStartOffset); // startOffset
					writer.Write(d.BytecodeEndOffset);   // endOffset
					writer.Write(fileToIndex[d.File]); // sourceFile

					int column;
					var lineNumber = d.GetLineNumber(1, out column);
					writer.Write(lineNumber);         // lineNumber
					writer.Write(column);             // column
					writer.Write(d.SourceStartIndex); // sourceStartIndex
					writer.Write(d.SourceEndIndex);   // sourceEndIndex
				}
			}
			writer.EndCollection();
		}

		/// <summary>
		/// Outputs a notice-type compiler message.
		/// </summary>
		/// <param name="message">The message to print.</param>
		public void Notice(string message)
		{
			Notice(message, CompilerVerbosity.NotVerbose);
		}
		/// <summary>
		/// Outputs a notice-type compiler message.
		/// </summary>
		/// <param name="message">The message to print.</param>
		/// <param name="level">The verbosity level of the message.</param>
		public void Notice(string message, CompilerVerbosity level)
		{
			if (SilenceNotices || level > verbosity)
				return;

			Console.Write("[info] ");
			Console.WriteLine(message);
		}
		/// <summary>
		/// Outputs a notice-type compiler message, prefixed with a source file location.
		/// </summary>
		/// <param name="message">The message to print.</param>
		/// <param name="level">The verbosity level of the message.</param>
		/// <param name="location">The source file location of the source of the message.</param>
		public void Notice(string message, CompilerVerbosity level, MessageLocation location)
		{
			if (SilenceNotices || level > verbosity)
				return;

			Console.Write("[info {0}] ", location.ToString(1));
			Console.WriteLine(message);
		}

		internal void Notice(CompilerVerbosity level, string format, object arg0)
		{
			if (SilenceNotices || level > verbosity)
				return;
			Notice(string.Format(format, arg0), level);
		}
		internal void Notice(CompilerVerbosity level, string format, object arg0, object arg1)
		{
			if (SilenceNotices || level > verbosity)
				return;
			Notice(string.Format(format, arg0, arg1), level);
		}
		internal void Notice(CompilerVerbosity level, string format, object arg0, object arg1, object arg2)
		{
			if (SilenceNotices || level > verbosity)
				return;
			Notice(string.Format(format, arg0, arg1, arg2), level);
		}
		internal void Notice(CompilerVerbosity level, string format, params object[] args)
		{
			if (SilenceNotices || level > verbosity)
				return;
			Notice(string.Format(format, args), level);
		}

		/// <summary>
		/// Outputs a warning-type compiler message.
		/// </summary>
		/// <param name="message">The message to print.</param>
		public void Warning(string message)
		{
			Warning(message, CompilerVerbosity.NotVerbose);
		}
		/// <summary>
		/// Outputs a warning-type compiler message.
		/// </summary>
		/// <param name="message">The message to print.</param>
		/// <param name="level">The verbosity level of the message.</param>
		public void Warning(string message, CompilerVerbosity level)
		{
			if (SilenceWarnings || level > verbosity)
				return;

			Console.Write("[warn] ");
			Console.WriteLine(message);
		}
		/// <summary>
		/// Outputs a warning-type compiler message.
		/// </summary>
		/// <param name="message">The message to print.</param>
		/// <param name="level">The verbosity level of the message.</param>
		/// <param name="location">The source file location of the source of the message.</param>
		public void Warning(string message, CompilerVerbosity level, MessageLocation location)
		{
			if (SilenceWarnings || level > verbosity)
				return;

			Console.Write("[warn {0}] ", location.ToString(1));
			Console.WriteLine(message);
		}

		internal void Warning(CompilerVerbosity level, string format, object arg0)
		{
			if (SilenceWarnings || level > verbosity)
				return;
			Warning(string.Format(format, arg0), level);
		}
		internal void Warning(CompilerVerbosity level, string format, object arg0, object arg1)
		{
			if (SilenceWarnings || level > verbosity)
				return;
			Warning(string.Format(format, arg0, arg1), level);
		}
		internal void Warning(CompilerVerbosity level, string format, object arg0, object arg1, object arg2)
		{
			if (SilenceWarnings || level > verbosity)
				return;
			Warning(string.Format(format, arg0, arg1, arg2), level);
		}
		internal void Warning(CompilerVerbosity level, string format, params object[] args)
		{
			if (SilenceWarnings || level > verbosity)
				return;
			Warning(string.Format(format, args), level);
		}

		public static void Compile(ref CompilerOptions options, string targetPath, params string[] sourceFiles)
		{
			Compile(ref options, targetPath, null, sourceFiles);
		}

		public static void Compile(ref CompilerOptions options, string targetPath, Dictionary<string, bool> constants, params string[] sourceFiles)
		{
			using (var c = new Compiler(ref options, constants, sourceFiles))
			{
				c.Compile(targetPath);
				if (c.nativeLibrary != null)
				{
					var libTarget = Path.GetFullPath(Path.Combine(Path.GetDirectoryName(targetPath),
						Path.GetFileName(c.nativeLibrary.FileName)));
					if (libTarget != c.nativeLibrary.FileName)
						File.Copy(c.nativeLibrary.FileName, libTarget, overwrite: true);
				}

				if (!options.NoDebugSymbols)
					c.SaveDebugSymbols(targetPath + ".dbg");

				if (options.DocFile != null)
					DocGenerator.Generate(c.projectNamespace, c, c.documents, options.DocFile, options.PrettyPrintJson);
			}
		}

		private const string DefaultMainMethodName = "<main>";

		private static readonly Dictionary<LambdaOperator, string> lambdaOperatorNames = new Dictionary<LambdaOperator, string>
		{
			{LambdaOperator.Plus, "λ<plus>"},
			{LambdaOperator.Minus, "λ<minus>"},
			{LambdaOperator.BitwiseOr, "λ<or>"},
			{LambdaOperator.BitwiseXor, "λ<xor>"},
			{LambdaOperator.Multiplication, "λ<mul>"},
			{LambdaOperator.Division, "λ<div>"},
			{LambdaOperator.Modulo, "λ<mod>"},
			{LambdaOperator.BitwiseAnd, "λ<and>"},
			{LambdaOperator.Exponentiation, "λ<exp>"},
			{LambdaOperator.ShiftLeft, "λ<shiftLeft>"},
			{LambdaOperator.ShiftRight, "λ<shiftRight>"},
			{LambdaOperator.Equality, "λ<equality>"},
			{LambdaOperator.Inequality, "λ<inequality>"},
			{LambdaOperator.Comparison, "λ<cmp>"},
			{LambdaOperator.Less, "λ<less>"},
			{LambdaOperator.Greater, "λ<greater>"},
			{LambdaOperator.LessEquals, "λ<lessequals>"},
			{LambdaOperator.GreaterEquals, "λ<greaterequals>"},
			{LambdaOperator.BitwiseNot, "λ<not>"},
			{LambdaOperator.FuncApplication, "λ<apply>"},
			{LambdaOperator.Concatenation, "λ<concat>"},
			{LambdaOperator.Not, "λ<boolNot>"},
			{LambdaOperator.Or, "λ<boolOr>"},
			{LambdaOperator.Xor, "λ<boolXor>"},
			{LambdaOperator.And, "λ<boolAnd>"}
		};

		private static readonly byte[] DebugSymbolsMagicNumber = { (byte)'O', (byte)'V', (byte)'D', (byte)'S' };

		internal static readonly char[] Dot = { '.' };

		private enum NamespaceLookupResult
		{
			Success,
			/// <summary>In a path such as 'a.b.c', one of 'a', 'a.b' or 'a.b.c' does not exist.</summary>
			IntermediateNamespaceMissing,
			/// <summary>In a path such as 'a.b.c', one of 'a', 'a.b.' or 'a.b.c' is not a namespace.</summary>
			MemberIsNotNamespace,
		}
	}

	public struct CompilerOptions
	{
		public CompilerOptions(CompilerFlags flags, CompilerVerbosity verbosity, ProjectType type)
		{
			this.flags = flags;
			this.verbosity = verbosity;
			this.type = type;
			this.libraryPath = null;
			this.nativeLibrary = null;
			this.metadataFile = null;
			this.moduleName = null;
			this.mainMethod = null;
			this.docFile = null;
		}
		public CompilerOptions(CompilerFlags flags, CompilerVerbosity verbosity, ProjectType type,
			string moduleName, string libraryPath, string nativeLibrary, string metadataFile, string mainMethod, string docFile)
		{
			this.flags = flags;
			this.verbosity = verbosity;
			this.type = type;
			this.moduleName = moduleName;
			this.libraryPath = libraryPath;
			this.nativeLibrary = nativeLibrary;
			this.metadataFile = metadataFile;
			this.mainMethod = mainMethod;
			this.docFile = docFile;
		}

		private CompilerFlags flags;
		/// <summary>
		/// Gets or sets the compiler flags.
		/// </summary>
		public CompilerFlags Flags { get { return flags; } set { flags = value; } }

		/// <summary>
		/// Gets or sets the <see cref="CompilerFlags.UseExtensions"/> flag.
		/// </summary>
		public bool UseExtensions
		{
			get { return (flags & CompilerFlags.UseExtensions) == CompilerFlags.UseExtensions; }
			set { ToggleFlag(CompilerFlags.UseExtensions, value); }
		}
		/// <summary>
		/// Gets or sets the <see cref="CompilerFlags.NoStandardModule"/> flag.
		/// </summary>
		public bool NoStandardModule
		{
			get { return (flags & CompilerFlags.NoStandardModule) == CompilerFlags.NoStandardModule; }
			set { ToggleFlag(CompilerFlags.NoStandardModule, value); }
		}
		/// <summary>
		/// Gets or sets the <see cref="CompilerFlags.SilenceWarnings"/> flag.
		/// </summary>
		public bool SilenceWarnings
		{
			get { return (flags & CompilerFlags.SilenceWarnings) == CompilerFlags.SilenceWarnings; }
			set { ToggleFlag(CompilerFlags.SilenceWarnings, value); }
		}
		/// <summary>
		/// Gets or sets the <see cref="CompilerFlags.SilenceNotices"/> flag.
		/// </summary>
		public bool SilenceNotices
		{
			get { return (flags & CompilerFlags.SilenceNotices) == CompilerFlags.SilenceNotices; }
			set { ToggleFlag(CompilerFlags.SilenceNotices, value); }
		}
		/// <summary>
		/// Gets or sets the <see cref="CompilerFlags.SkipExternChecks"/> flag.
		/// </summary>
		public bool SkipExternChecks
		{
			get { return (flags & CompilerFlags.SkipExternChecks) == CompilerFlags.SkipExternChecks; }
			set { ToggleFlag(CompilerFlags.SkipExternChecks, value); }
		}
		/// <summary>
		/// Gets or sets the <see cref="CompilerFlags.ErrorToStdout"/> flag.
		/// </summary>
		public bool ErrorToStdout
		{
			get { return (flags & CompilerFlags.ErrorToStdout) == CompilerFlags.ErrorToStdout; }
			set { ToggleFlag(CompilerFlags.ErrorToStdout, value); }
		}
		/// <summary>
		/// Gets or sets the <see cref="CompilerFlags.PrettyPrintJson"/> flag.
		/// </summary>
		public bool PrettyPrintJson
		{
			get { return (flags & CompilerFlags.PrettyPrintJson) == CompilerFlags.PrettyPrintJson; }
			set { ToggleFlag(CompilerFlags.PrettyPrintJson, value); }
		}
		/// <summary>
		/// Gets or sets the <see cref="CompilerFlags.NoDebugSymbols"/> flag.
		/// </summary>
		public bool NoDebugSymbols
		{
			get { return (flags & CompilerFlags.NoDebugSymbols) == CompilerFlags.NoDebugSymbols; }
			set { ToggleFlag(CompilerFlags.NoDebugSymbols, value); }
		}

		private CompilerVerbosity verbosity;
		/// <summary>
		/// Gets or sets the verbosity of the compiler.
		/// </summary>
		public CompilerVerbosity Verbosity { get { return verbosity; } set { verbosity = value; } }

		private ProjectType type;
		/// <summary>
		/// Gets or sets the project type.
		/// </summary>
		public ProjectType Type { get { return type; } set { type = value; } }

		private string libraryPath;
		/// <summary>
		/// Gets or sets the library path, from which modules are loaded if they are not present in the project directory.
		/// </summary>
		public string LibraryPath { get { return libraryPath; } set { libraryPath = value; } }

		private string nativeLibrary;
		/// <summary>
		/// Gets or sets the file path of the native library, or null if none is used.
		/// </summary>
		public string NativeLibrary { get { return nativeLibrary; } set { nativeLibrary = value; } }

		private string metadataFile;
		/// <summary>
		/// Gets or sets the path of the metadata file.
		/// </summary>
		public string MetadataFile { get { return metadataFile; } set { metadataFile = value; } }

		private string moduleName;
		/// <summary>
		/// Gets or sets the name of the output module.
		/// </summary>
		public string ModuleName { get { return moduleName; } set { moduleName = value; } }

		private string mainMethod;
		/// <summary>
		/// Gets or sets the name of the main method.
		/// </summary>
		/// <remarks>
		/// <para>If <see cref="MainMethod"/> is null, then the compiled module gets a main method according to its project type:</para>
		/// <list type="bullet">
		///		<item>
		///			<description>If the <see cref="Type"/> is <see cref="ProjectType.Application"/>,
		///			then the main method is comprised of global statements from all source files,
		///			concatenated in an unspecified order.</description>
		///		</item>
		///		<item>
		///			<description>If the <see cref="Type"/> is <see cref="ProjectType.Module"/>,
		///			then the output module has no main method.</description>
		///		</item>
		/// </list>
		/// <para>The project type <see cref="ProjectType.Application"/> cannot be used if <see cref="MainMethod"/> is not null.
		/// The property value must contain the name of a fully qualified global function; static class methods are not permitted.</para>
		/// </remarks>
		public string MainMethod { get { return mainMethod; } set { mainMethod = value; } }

		private string docFile;
		/// <summary>
		/// Gets or sets the path of the documentation file. If set to null,
		/// the compiler does not generate a documentation file.
		/// </summary>
		public string DocFile { get { return docFile; } set { docFile = value; } }

		private void ToggleFlag(CompilerFlags flag, bool on)
		{
			if (on)
				flags |= flag;
			else
				flags &= ~flag;
		}
	}

	/// <summary>
	/// Represents options that are passed to the compiler.
	/// </summary>
	[Flags]
	public enum CompilerFlags
	{
		/// <summary>
		/// Specifies no compiler options.
		/// </summary>
		None = 0,
		/// <summary>
		/// Specifies that the compiler is allowed to use extension keywords.
		/// </summary>
		UseExtensions = 1 << 0,
		/// <summary>
		/// Specifies that the compiler should not include a reference to the standard module.
		/// This flag should only be specified when compiling the standard module.
		/// </summary>
		NoStandardModule = 1 << 1,
		/// <summary>
		/// Stops the compiler from outputting warnings to the standard output.
		/// </summary>
		/// <remarks>This should not be used in conjunction with <see cref="Verbose"/>.</remarks>
		SilenceWarnings = 1 << 2,
		/// <summary>
		/// Stops the compiler from outputting notices to the standard output.
		/// </summary>
		/// <remarks>This should not be used in conjunction with <see cref="Verbose"/>.</remarks>
		SilenceNotices = 1 << 3,
		/// <summary>
		/// If specified, the compiler does not verify that __extern bodies actually resolve to existing methods.
		/// Use this flag very cautiously! There's a reason the compiler checks these.
		/// </summary>
		SkipExternChecks = 1 << 4,
		/// <summary>
		/// Specifies the <see cref="SilentWarnings"/> and <see cref="SilenceNotices"/> flags.
		/// </summary>
		/// <remarks>This should not be used in conjunction with <see cref="Verbose"/>.</remarks>
		Silent = SilenceWarnings | SilenceNotices,
		/// <summary>
		/// Redirects errors to the standard output stream.
		/// </summary>
		ErrorToStdout = 1 << 5,
		/// <summary>
		/// Specifies that documentation JSON files should be pretty-printed.
		/// </summary>
		PrettyPrintJson = 1 << 6,
		/// <summary>
		/// Suppresses generation of debug symbols.
		/// </summary>
		NoDebugSymbols = 1 << 7,
	}

	/// <summary>
	/// Specifies the verbosity of the compiler.
	/// </summary>
	public enum CompilerVerbosity : byte
	{
		/// <summary>
		/// The compiler outputs no messages beyond normal warnings and notices.
		/// </summary>
		NotVerbose = 0,
		/// <summary>
		/// The compiler outputs verbose messages.
		/// </summary>
		Verbose = 1,
		/// <summary>
		/// The compiler outputs a LOT of extra information.
		/// </summary>
		ExtraVerbose = 2,
	}

	/// <summary>
	/// Specifies the type of a project that is being compiled.
	/// </summary>
	public enum ProjectType : byte
	{
		/// <summary>
		/// The project is an application, which is executable and has a main method.
		/// </summary>
		Application = 0,
		/// <summary>
		/// The project is a module, which only exports members and has no main method.
		/// </summary>
		Module = 1,
	}

	public struct MessageLocation
	{
		public MessageLocation(SourceFile sourceFile, int startIndex, int endIndex)
		{
			this.sourceFile = sourceFile;
			this.startIndex = startIndex;
			this.endIndex = endIndex;
		}

		private SourceFile sourceFile;
		/// <summary>
		/// Gets the file that the message comes from.
		/// </summary>
		public SourceFile SourceFile { get { return SourceFile; } }

		/// <summary>
		/// Gets the name of the file which the message location is inside of.
		/// </summary>
		public string FileName { get { return sourceFile.FileName; } }

		/// <summary>
		/// Gets the text contents of the file that the message location is inside of.
		/// </summary>
		public string SourceText { get { return sourceFile.Source; } }

		private int startIndex, endIndex;
		/// <summary>
		/// Gets the character index within the source text at which
		/// the message begins.
		/// </summary>
		public int StartIndex { get { return startIndex; } }
		/// <summary>
		/// Gets the last character index (exclusive) of the message
		/// within the source text.
		/// </summary>
		public int EndIndex { get { return endIndex; } }

		/// <summary>
		/// Gets the line and column number, both 1-based, of the message
		/// within the source text.
		/// </summary>
		/// <param name="tabSize">The size of the tab character.</param>
		/// <param name="column">The column number of the message location.</param>
		/// <returns>The line number at which the message location begins.</returns>
		public int GetLineNumber(int tabSize, out int column)
		{
			return sourceFile.GetLineNumber(startIndex, tabSize, out column);
		}

		public override string ToString()
		{
			return ToString(1);
		}

		public string ToString(int tabSize)
		{
			int column;
			var line = GetLineNumber(tabSize, out column);

			var length = endIndex - startIndex;

			return string.Format("\"{0}\":{1}:{2}+{3}",
				FileName,
				line.ToStringInvariant(),
				column.ToStringInvariant(),
				length.ToStringInvariant());
		}

		internal static MessageLocation FromNode(ParseNode node)
		{
			if (node == null)
				throw new ArgumentNullException("node");

			return new MessageLocation(node.Document.SourceFile,
				node.StartIndex, node.EndIndex);
		}
	}
}