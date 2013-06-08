# Osprey Compiler

The language that this compiler actually compiles, as well as the bytecode format it compiles into, will be described at a later time in a proper specification.

## Command-line options

* `/out <path>`

	Specifies the output file.

	Default value: the first source file with its extension replaced by `.ovm`

* `/libpath <path>`

	The path to the library directory, which contains other modules.

	Default value: current working directory

* `/nativelib <path>`

	A path to a native library (DLL) to compile with the project.

	Default value: none

* `/noexternchecks`

	Forces the compiler not to check `__extern` references.

	Can only be used if `/nativelib` is specified.

	Even if this parameter is specified, the compiler still makes sure the native library file exists, as it must be copied to the output directory.

* `/nostdlib`

	Do not include a reference to the standard library.

	This option should only be used when compiling the standard library.

* `/type app|module`

	Specifies project type.

	Default value: `app`

* `/meta <path>`

	The metadata file associated with the project.

* `/name <name>`

	The name of the project that's being compiled. If omitted, uses the name of the first source file sans file extension.

* `/verbose`

	Makes the compiler more talkative.

* `/extraverbose`

	Makes the compiler a LOT more talkative.

* `/silent`

	Makes the compiler not output any messages whatsoever, even error messages.

* `/nowarn`

	Makes the compiler not output any warnings.

* `/noinfo`

	Makes the compiler not output any informational messages.

* `/noext`

	Disables compiler extensions altogether.

* `/doc <path>`

	Specifies the path for the documentation file. If omitted, no documentation file is generated.

* `/errtostdout`

	Redirects all error messages to STDOUT. By default, they are written to STDERR.

* `/const <name> true|false`

	Declares a boolean constant, which can be referred to in Osprey code. `<name>` is the fully qualified name of the constant, e.g. `company.amazing.product.DEBUG`. The only permitted values are `true` and `false`.

	If the project source files declare or import a name that would make it an error to declare a constant with the specified name, then the command-line constant is ignored.

	The declared constant is always internal to the module, and is always a global constant. It is not possible to define a class constant on the command line.

	**Note:** Osprey does not have a preprocessor, and you cannot conditionally include declarations or source files. These command-line constants should only really be used to exclude or include specific _statements_.

	This switch can be used multiple times, but each constant name must be unique.

* `/f`

	Specifies that every command-line parameter following this one is a source file. By default, the first parameter that does not begin with `/` or `-` marks the beginning of the source file list; use this option if the first source file begins with `/` or `-`, or if the source file list is generated programmatically.

## Command-line output

General information and warnings are written to STDOUT, while errors go to STDERR unless `/errtostdout` is in use.

Informative messages always begin with `[info]`; if they consist of multiple lines, each line will have `[info]` prepended to it. Similarly, warnings have `[warn]`.

Compile-time errors emit messages that begin with `[error]`. These _may_ consist of multiple lines; everything after the line beginning with `[error]` is considered part of the error information. No additional lines are printed after an error.

If you redirect errors to STDOUT with `/errtostdout`, then the compiler may change the text and background colors in the console window to highlight the error location in the offending source file.