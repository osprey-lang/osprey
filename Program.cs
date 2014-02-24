using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Reflection;
using System.Text;

namespace Osprey
{
	public class Program
	{
		public static int Main(string[] args)
		{
			try { Console.OutputEncoding = Encoding.UTF8; } catch { }
			try { Console.InputEncoding = Encoding.UTF8; } catch { }

			CompilerOptions compilerOptions;
			ProgramOptions programOptions;
			Dictionary<string, bool> constants;
			List<string> sourceFiles;
			try
			{
				var sourceFileIndex = ParseArguments(args,
					out compilerOptions, out programOptions,
					out constants);

				sourceFiles = new List<string>();
				if (sourceFileIndex != -1)
					for (var i = sourceFileIndex; i < args.Length; i++)
					{
						var path = args[i];
						var dirPart = Path.GetDirectoryName(args[i]);
						var filePart = Path.GetFileName(args[i]);

						if (dirPart.Length == 0)
							dirPart = Environment.CurrentDirectory;

						var matchingFiles = Directory.GetFiles(dirPart, filePart,
							programOptions.RecursiveWildcards ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly);
						if (matchingFiles.Length == 0 &&
							!(filePart.Contains('*') || filePart.Contains('?')))
							throw new ArgumentException(string.Format("Could not find a matching file for non-wildcard path: {0}", args[i]));

						foreach (var file in matchingFiles)
							sourceFiles.Add(Path.Combine(dirPart, file));
					}

				if (sourceFiles.Count == 0)
					throw new ArgumentException("There must be at least one source file.");

				if (programOptions.OutFile == null)
					programOptions.OutFile = Path.Combine(Path.GetDirectoryName(sourceFiles[0]),
						Path.GetFileNameWithoutExtension(sourceFiles[0]) + ".ovm");

				if (compilerOptions.LibraryPath == null)
					compilerOptions.LibraryPath = Environment.CurrentDirectory;
			}
			catch (ArgumentException e)
			{
				Console.Error.WriteLine("[error] Could not parse command-line arguments: {0}", e.Message);
				return 1;
			}

			var exitCode = 0;
#if DEBUG
			Compiler.Compile(compilerOptions, programOptions.OutFile, constants, sourceFiles.ToArray());
			Console.ReadKey(intercept: true);
#else
			var err = compilerOptions.ErrorToStdout ? Console.Out : Console.Error;
			try
			{
				Compiler.Compile(compilerOptions, programOptions.OutFile, constants, sourceFiles.ToArray());
			}
			catch (ParseException e)
			{
				if (!programOptions.SilenceErrors)
				{
					err.Write("[error {0}] Parse error: ",
						new MessageLocation(e.FileName, e.GetFileSource(),
							e.Index, e.EndIndex).ToString(1));

					Console.ForegroundColor = ConsoleColor.Red;
					err.Write(e.Message);
					Console.ForegroundColor = ConsoleColor.Gray;

					err.WriteLine();
					if (!programOptions.SuppressSource)
						PrintErrorLocation(e, err);
				}
				exitCode = 1;
			}
			catch (CompileTimeException e)
			{
				if (!programOptions.SilenceErrors)
				{
					if (e.Node != null && e.Node.Document != null)
					{
						var loc = MessageLocation.FromNode(e.Node);
						err.Write("[error {0}] Compiler error: ", loc.ToString(1));
					}
					else
						err.Write("[error] Compiler error: ");

					Console.ForegroundColor = ConsoleColor.Red;
					err.Write(e.Message);
					Console.ForegroundColor = ConsoleColor.Gray;

					err.WriteLine();
					if (!programOptions.SuppressSource && e.Node != null)
						PrintErrorLocation(e, err);
				}
				exitCode = 1;
			}
			catch (ModuleLoadException e)
			{
				if (!programOptions.SilenceErrors)
				{
					err.Write("[error] Module load error: ");
					Console.ForegroundColor = ConsoleColor.Red;
					err.Write(e.Message);
					Console.ForegroundColor = ConsoleColor.Gray;
					err.WriteLine();
					err.WriteLine("Module file: {0}", e.FileName);
				}
				exitCode = 1;
			}
			catch (Exception e)
			{
				if (!programOptions.SilenceErrors)
				{
					Console.Error.WriteLine("[error] Other exception:");
					Console.ForegroundColor = ConsoleColor.Red;
					Console.Error.WriteLine(e.ToString());
					Console.ForegroundColor = ConsoleColor.Gray;
				}
				exitCode = 1;
			}
#endif
			return exitCode;
		}

		private static void PrintHighlighted(string code, TextWriter target)
		{
			Console.BackgroundColor = ConsoleColor.Black;

			var tokenizer = new Tokenizer(code, TokenizerFlags.IncludeComments);
			var lastEndIndex = 0;
			try
			{
				foreach (var tok in tokenizer)
				{
					if (tok.Index > lastEndIndex)
						target.Write(code.Substring(lastEndIndex, tok.Index - lastEndIndex));

					if (tok.Match(TokenType.Keyword) || tok.Type == TokenType.At)
						Console.ForegroundColor = ConsoleColor.Cyan;
					else if (tok.Match(TokenType.Punctuation))
						Console.ForegroundColor = ConsoleColor.Gray;
					else if (tok.Match(TokenType.Integer) || tok.Match(TokenType.Real))
						Console.ForegroundColor = ConsoleColor.Magenta;
					else if (tok.Match(TokenType.String))
						Console.ForegroundColor = ConsoleColor.Red;
					else if (tok.Type == TokenType.Comment)
						Console.ForegroundColor = ConsoleColor.DarkGreen;
					else
						Console.ForegroundColor = ConsoleColor.White;

					target.Write(tok.Value);
					lastEndIndex = tok.EndIndex;
				}
			}
			catch (ParseException)
			{ }

			if (code.Length > lastEndIndex)
				target.Write(code.Substring(lastEndIndex, code.Length - lastEndIndex));

			Console.ResetColor();
		}

		private static void PrintErrorLocation(ParseException e, TextWriter errorStream)
		{
			var length = e.Token != null ?
				e.Token.Value.Length :
				e.Node.EndIndex - e.Node.StartIndex;
			PrintErrorLocation(e.FileName, e.GetFileSource(), e.Index, length, errorStream);
		}

		private static void PrintErrorLocation(CompileTimeException e, TextWriter errorStream)
		{
			PrintErrorLocation(e.Document.FileName, e.Document.FileSource,
				e.Node.StartIndex, e.Node.EndIndex - e.Node.StartIndex,
				errorStream);
		}

		private static void PrintErrorLocation(string fileName, string fileSource, int charIndex, int length, TextWriter err)
		{
			int column;
			var lineNumber = Token.GetLineNumber(fileSource, charIndex, 1, out column);
			err.WriteLine("At line {0}, character {1}, in {2}:", lineNumber, column, fileName);

			int startIndex;
			var context = FindContext(fileSource, charIndex, length, out startIndex);

			charIndex -= startIndex;
			if (charIndex > 0)
				err.Write(context.Substring(0, charIndex));

			Console.BackgroundColor = ConsoleColor.DarkRed;
			Console.ForegroundColor = ConsoleColor.White;

			var errorCode = context.Substring(charIndex, length);
			if (errorCode.Length == 1 && IsLineSeparator(errorCode[0]))
				err.Write(" ");
			else
				err.Write(errorCode);

			Console.ResetColor();

			if (charIndex + length < context.Length)
				err.Write(context.Substring(charIndex + length));

			err.WriteLine();
		}

		private static string FindContext(string fileSource, int charIndex, int length, out int startIndex)
		{
			const int maxContext = 2; // how many lines to fetch above and below the error location (at most)

			var lines = 0;
			var start = charIndex;
			while (start > 0)
			{
				var ch = fileSource[start - 1];
				if (IsLineSeparator(ch))
				{
					if (lines == maxContext)
						break;

					// \r\n counts as a unit
					if (start > 1 && ch == '\n' && fileSource[start - 2] == '\r')
						start--;
					lines++;
				}

				start--;
			}

			lines = 0;
			var end = charIndex + length;
			while (end < fileSource.Length)
			{
				var ch = fileSource[end];
				if (IsLineSeparator(ch))
				{
					if (lines == maxContext)
						break;

					// \r\n counts as a unit
					if (end < fileSource.Length - 1 && ch == '\r' && fileSource[end + 1] == '\n')
						end++;
					lines++;
				}

				end++;
			}

			startIndex = start;
			return fileSource.Substring(start, end - start);
		}

		private static bool IsLineSeparator(char ch)
		{
			return ch == '\n' || ch == '\r' || ch == '\u2028' || ch == '\u2029';
		}

		/// <summary>
		/// Parses the command-line arguments and returns the index of the first source file within the arguments array.
		/// </summary>
		/// <param name="args">The command-line arguments passed to the compiler.</param>
		/// <param name="compilerOptions">The <see cref="CompilerOptions"/> that receives the parsed arguments.</param>
		/// <param name="outFile">The output file, or null if none was specified.</param>
		/// <returns>The index of the first source file within <paramref name="args"/>.</returns>
		private static int ParseArguments(string[] args, out CompilerOptions compilerOptions,
			out ProgramOptions programOptions, out Dictionary<string, bool> constants)
		{
			compilerOptions = new CompilerOptions();
			compilerOptions.UseExtensions = true; // Use extensions by default

			programOptions = new ProgramOptions();

			var seenSwitches = new HashSet<string>();

			constants = new Dictionary<string, bool>();

			var argc = args.Length;
			for (var i = 0; i < args.Length; i++)
			{
				var arg = args[i];

				if (arg.Length > 0 && (arg[0] == '/' || arg[0] == '-'))
				{
					arg = arg.Substring(1).ToLowerInvariant();
					if (seenSwitches.Contains(arg))
						throw new ArgumentException(string.Format("Parameter '{0}' occurs more than once.", args[i]));

					switch (arg)
					{
						case "out":
							if (i == argc - 1)
								throw new ArgumentException("'/out' must be followed by a file name.");
							programOptions.OutFile = args[++i];
							break;

						case "libpath":
							if (i == argc - 1)
								throw new ArgumentException("'/libpath' must be followed by a directory name.");
							compilerOptions.LibraryPath = args[++i];
							break;

						case "nativelib":
							if (i == argc - 1)
								throw new ArgumentException("'/nativelib' must be followed by a file name.");
							compilerOptions.NativeLibrary = args[++i];
							break;

						case "noexternchecks":
							compilerOptions.SkipExternChecks = true;
							break;

						case "nostdlib":
							compilerOptions.NoStandardModule = true;
							break;

						case "type":
							if (i == argc - 1)
								throw new ArgumentException("'/type' must be followed by a project type (app or module).");
							i++;
							if (args[i] == "app")
								compilerOptions.Type = ProjectType.Application;
							else if (args[i] == "module")
								compilerOptions.Type = ProjectType.Module;
							else
								throw new ArgumentException(
									string.Format("Invalid project type '{0}' (must be \"app\" or \"module\").", args[i]));
							break;

						case "meta":
							if (i == argc - 1)
								throw new ArgumentException("'/meta' must be followed by a file name.");
							compilerOptions.MetadataFile = args[++i];
							break;

						case "name":
							if (i == argc - 1)
								throw new ArgumentException("'/name' must be followed by a project name.");
							compilerOptions.ModuleName = args[++i];
							break;

						case "verbose":
							if (seenSwitches.Contains("extraverbose") || seenSwitches.Contains("silent"))
								throw new ArgumentException("'/verbose' cannot be used together with '/extraverbose' or '/silent'.");
							compilerOptions.Verbosity = CompilerVerbosity.Verbose;
							break;

						case "extraverbose":
							if (seenSwitches.Contains("verbose") || seenSwitches.Contains("silent"))
								throw new ArgumentException("'/extraverbose' cannot be used together with '/verbose' or '/silent'.");
							compilerOptions.Verbosity = CompilerVerbosity.ExtraVerbose;
							break;

						case "silent":
							if (seenSwitches.Contains("verbose") || seenSwitches.Contains("extraverbose"))
								throw new ArgumentException("'/silent' cannot be used together with '/verbose' or '/extraverbose'.");
							if (seenSwitches.Contains("nowarn") || seenSwitches.Contains("noinfo"))
								throw new ArgumentException("'/silent' cannot be used together with '/nowarn' or '/noinfo'.");
							compilerOptions.SilenceNotices = true;
							compilerOptions.SilenceWarnings = true;
							programOptions.SilenceErrors = true;
							break;

						case "nowarn":
							if (seenSwitches.Contains("silent"))
								throw new ArgumentException("'/nowarn' cannot be used together with '/silent'.");
							compilerOptions.SilenceWarnings = true;
							break;

						case "noinfo":
							if (seenSwitches.Contains("silent"))
								throw new ArgumentException("'/noinfo' cannot be used together with '/silent'.");
							compilerOptions.SilenceNotices = true;
							break;

						case "noext":
							compilerOptions.UseExtensions = false;
							break;

						case "doc":
							if (i == argc - 1)
								throw new ArgumentException("'/doc' must be followed by a file name.");
							compilerOptions.DocFile = args[++i];
							break;

						case "errtostdout":
							compilerOptions.ErrorToStdout = true;
							break;

						case "hidesource":
							programOptions.SuppressSource = true;
							break;

						case "formatjson":
							compilerOptions.PrettyPrintJson = true;
							break;

						case "const":
							if (i >= argc - 2 || (args[i + 2] != "true" && args[i + 2] != "false"))
								throw new ArgumentException("'/const' must be followed by a name and either 'true' or 'false'.");

							{
								var name = args[++i].Trim();
								if (constants.ContainsKey(name))
									throw new ArgumentException("Each '/const' declaration must have a unique name.");
								constants.Add(name, args[++i] == "true");
							}
							continue; // Don't add the switch to the set!

						case "r":
							programOptions.RecursiveWildcards = true;
							break;

						case "f":
							return i + 1; // Source file list begins here!

						default:
							throw new ArgumentException(string.Format("Unknown parameter: {0}", args[i]));
					}

					seenSwitches.Add(arg); // If we reach this point, it's correct.
				}
				else
				{
					// The source file list begins here!
					return i;
				}
			}

			return -1; // D:
		}

		private struct ProgramOptions
		{
			public bool SilenceErrors;
			public bool SuppressSource;
			public bool RecursiveWildcards;
			public string OutFile;
		}
	}
}