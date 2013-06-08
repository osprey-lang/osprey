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
		public static void Main(string[] args)
		{
			try { Console.OutputEncoding = Encoding.UTF8; } catch { }
			try { Console.InputEncoding = Encoding.UTF8; } catch { }

			CompilerOptions options;
			string outFile;
			Dictionary<string, bool> constants;
			List<string> sourceFiles;
			try
			{
				var sourceFileIndex = ParseArguments(args, out options, out outFile, out constants);
				if (sourceFileIndex == -1)
					throw new ArgumentException("There must be at least one source file.");

				sourceFiles = new List<string>();
				for (var i = sourceFileIndex; i < args.Length; i++)
				{
					var path = args[i];
					var dirPart = Path.GetDirectoryName(args[i]);
					var filePart = Path.GetFileName(args[i]);

					if (dirPart.Length == 0)
						dirPart = Environment.CurrentDirectory;

					foreach (var file in Directory.EnumerateFiles(dirPart, filePart))
						sourceFiles.Add(Path.Combine(dirPart, file));
				}

				if (outFile == null)
					outFile = Path.Combine(Path.GetDirectoryName(sourceFiles[0]),
						Path.GetFileNameWithoutExtension(sourceFiles[0]) + ".ovm");

				if (options.LibraryPath == null)
					options.LibraryPath = Environment.CurrentDirectory;
			}
			catch (ArgumentException e)
			{
				Console.Error.WriteLine("[error] Could not parse command-line arguments: {0}", e.Message);
				return;
			}

#if DEBUG
			Compiler.Compile(options, outFile, constants, sourceFiles.ToArray());
			Console.ReadKey(intercept: true);
#else
			var err = options.ErrorToStdout ? Console.Out : Console.Error;
			try
			{
				Compiler.Compile(options, outFile, constants, sourceFiles.ToArray());
			}
			catch (ParseException e)
			{
				err.Write("[error] Parse error: ");
				Console.ForegroundColor = ConsoleColor.Red;
				err.Write(e.Message);
				Console.ForegroundColor = ConsoleColor.Gray;
				err.WriteLine();
				PrintErrorLocation(e);
			}
			catch (CompileTimeException e)
			{
				err.Write("[error] Compiler error: ");
				Console.ForegroundColor = ConsoleColor.Red;
				err.Write(e.Message);
				Console.ForegroundColor = ConsoleColor.Gray;
				err.WriteLine();
				if (e.Node != null)
					PrintErrorLocation(e);
			}
			catch (ModuleLoadException e)
			{
				err.Write("[error] Module load error: ");
				Console.ForegroundColor = ConsoleColor.Red;
				err.Write(e.Message);
				Console.ForegroundColor = ConsoleColor.Gray;
				err.WriteLine();
				err.WriteLine("Module file: {0}", e.FileName);
			}
			catch (Exception e)
			{
				Console.Error.WriteLine("[error] Other exception:");
				Console.ForegroundColor = ConsoleColor.Red;
				Console.Error.WriteLine(e.ToString());
				Console.ForegroundColor = ConsoleColor.Gray;
			}
#endif
		}

		private static void PrintHighlighted(string code)
		{
			var tokenizer = new Tokenizer(code, TokenizerFlags.IncludeComments);
			var lastEndIndex = 0;
			foreach (var tok in tokenizer)
			{
				if (tok.Index > lastEndIndex)
					Console.Write(code.Substring(lastEndIndex, tok.Index - lastEndIndex));

				if (tok.Match(TokenType.Keyword) || tok.Type == TokenType.At)
					Console.ForegroundColor = ConsoleColor.Cyan;
				else if (tok.Match(TokenType.Punctuation))
					Console.ForegroundColor = ConsoleColor.Gray;
				else if (tok.Match(TokenType.Integer) || tok.Match(TokenType.Real))
					Console.ForegroundColor = ConsoleColor.Magenta;
				else if (tok.Match(TokenType.String))
					Console.ForegroundColor = ConsoleColor.Red;
				else
					Console.ForegroundColor = ConsoleColor.White;

				Console.Write(tok.Value);
				lastEndIndex = tok.EndIndex;
			}

			if (code.Length > lastEndIndex)
				Console.WriteLine(code.Substring(lastEndIndex, code.Length - lastEndIndex));
			else
				Console.WriteLine();

			Console.ForegroundColor = ConsoleColor.Gray;
		}

		private static void PrintErrorLocation(ParseException e)
		{
			var index = e.Token != null ?
				e.Token.Index :
				e.Node.StartIndex;
			var length = e.Token != null ?
				e.Token.Value.Length :
				e.Node.EndIndex - e.Node.StartIndex;
			PrintErrorLocation(e.FileName, e.GetFileSource(), index, length);
		}

		private static void PrintErrorLocation(CompileTimeException e)
		{
			PrintErrorLocation(e.Document.FileName, e.Document.FileSource,
				e.Node.StartIndex, e.Node.EndIndex - e.Node.StartIndex);
		}

		private static void PrintErrorLocation(string fileName, string fileSource, int charIndex, int length)
		{
			var err = Console.Error;

			int column;
			var lineNumber = Token.GetLineNumber(fileSource, charIndex, 1, out column);
			var lines = fileSource.Split(new string[] { Environment.NewLine, "\n" }, lineNumber + 2, StringSplitOptions.None);

			if (lineNumber > lines.Length)
				err.WriteLine("At line {0}, character {1}, in {2}.", lineNumber, column, fileName);
			else
			{
				err.WriteLine("At line {0}, character {1}, in {2}:", lineNumber, column, fileName);

				column--;

				// Print some context before...
				if (lineNumber > 1)
					err.WriteLine(lines[lineNumber - 2]);

				var line = lines[lineNumber - 1];
				if (column > 0)
					err.Write(line.Substring(0, column));

				Console.BackgroundColor = ConsoleColor.DarkRed;
				Console.ForegroundColor = ConsoleColor.White;
				if (column >= line.Length)
					err.Write(" ");
				else
					err.Write(line.Substring(column, length));
				Console.BackgroundColor = ConsoleColor.Black;
				Console.ForegroundColor = ConsoleColor.Gray;

				if (column + length < line.Length)
					err.Write(line.Substring(column + length));

				err.WriteLine();

				// ... and after!
				if (lineNumber < lines.Length)
					err.WriteLine(lines[lineNumber]);
			}
		}

		/// <summary>
		/// Parses the command-line arguments and returns the index of the first source file within the arguments array.
		/// </summary>
		/// <param name="args">The command-line arguments passed to the compiler.</param>
		/// <param name="options">The <see cref="CompilerOptions"/> that receives the parsed arguments.</param>
		/// <param name="outFile">The output file, or null if none was specified.</param>
		/// <returns>The index of the first source file within <paramref name="args"/>.</returns>
		private static int ParseArguments(string[] args, out CompilerOptions options,
			out string outFile, out Dictionary<string, bool> constants)
		{
			options = new CompilerOptions();
			options.UseExtensions = true; // Use extensions by default

			var seenSwitches = new HashSet<string>();
			outFile = null;

			constants = new Dictionary<string, bool>();

			var argc = args.Length;
			for (var i = 0; i < args.Length; i++)
			{
				var arg = args[i];

				if (arg.Length > 0 && (arg[0] == '/' || arg[0] == '-'))
				{
					arg = arg.Substring(1);
					if (seenSwitches.Contains(arg))
						throw new ArgumentException(string.Format("Parameter '{0}' occurs more than once.", args[i]));

					switch (arg)
					{
						case "out":
							if (i == argc - 1)
								throw new ArgumentException("'/out' must be followed by a file name.");
							outFile = args[++i];
							break;

						case "libpath":
							if (i == argc - 1)
								throw new ArgumentException("'/libpath' must be followed by a directory name.");
							options.LibraryPath = args[++i];
							break;

						case "nativelib":
							if (i == argc - 1)
								throw new ArgumentException("'/nativelib' must be followed by a file name.");
							options.NativeLibrary = args[++i];
							break;

						case "noexternchecks":
							options.SkipExternChecks = true;
							break;

						case "nostdlib":
							options.NoStandardModule = true;
							break;

						case "type":
							if (i == argc - 1)
								throw new ArgumentException("'/type' must be followed by a project type (app or module).");
							i++;
							if (args[i] == "app")
								options.Type = ProjectType.Application;
							else if (args[i] == "module")
								options.Type = ProjectType.Module;
							else
								throw new ArgumentException(
									string.Format("Invalid project type '{0}' (must be \"app\" or \"module\").", args[i]));
							break;

						case "meta":
							if (i == argc - 1)
								throw new ArgumentException("'/meta' must be followed by a file name.");
							options.MetadataFile = args[++i];
							break;

						case "name":
							if (i == argc - 1)
								throw new ArgumentException("'/name' must be followed by a project name.");
							options.ModuleName = args[++i];
							break;

						case "verbose":
							if (seenSwitches.Contains("extraverbose") || seenSwitches.Contains("silent"))
								throw new ArgumentException("'/verbose' cannot be used together with '/extraverbose' or '/silent'.");
							options.Verbosity = CompilerVerbosity.Verbose;
							break;

						case "extraverbose":
							if (seenSwitches.Contains("verbose") || seenSwitches.Contains("silent"))
								throw new ArgumentException("'/extraverbose' cannot be used together with '/verbose' or '/silent'.");
							options.Verbosity = CompilerVerbosity.ExtraVerbose;
							break;

						case "silent":
							if (seenSwitches.Contains("verbose") || seenSwitches.Contains("extraverbose"))
								throw new ArgumentException("'/silent' cannot be used together with '/verbose' or '/extraverbose'.");
							if (seenSwitches.Contains("nowarn") || seenSwitches.Contains("noinfo"))
								throw new ArgumentException("'/silent' cannot be used together with '/nowarn' or '/noinfo'.");
							options.SilenceNotices = true;
							options.SilenceWarnings = true;
							break;

						case "nowarn":
							if (seenSwitches.Contains("silent"))
								throw new ArgumentException("'/nowarn' cannot be used together with '/silent'.");
							options.SilenceWarnings = true;
							break;

						case "noinfo":
							if (seenSwitches.Contains("silent"))
								throw new ArgumentException("'/noinfo' cannot be used together with '/silent'.");
							options.SilenceNotices = true;
							break;

						case "noext":
							options.UseExtensions = false;
							break;

						case "doc":
							if (i == argc - 1)
								throw new ArgumentException("'/doc' must be followed by a file name.");
							options.DocFile = args[++i];
							break;

						case "errtostdout":
							options.ErrorToStdout = true;
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
	}
}