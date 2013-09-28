# Osprey Compiler

The language that this compiler actually compiles, as well as the bytecode format it compiles into, will be described at a later time in a proper specification.

## Command-line options

The documentation for these has been moved to the wiki! Please see [Command-Line Parameters](https://bitbucket.org/Heurlinius/osprey/wiki/Command-Line%20Parameters) instead.

## Command-line output

General information and warnings are written to STDOUT, while errors go to STDERR unless `/errtostdout` is in use.

Informative messages always begin with `[info]`; if they consist of multiple lines, each line will have `[info]` prepended to it. Similarly, warnings have `[warn]`.

Compile-time errors emit messages that begin with `[error]`. These _may_ consist of multiple lines; everything after the line beginning with `[error]` is considered part of the error information. No additional lines are printed after an error.

If you redirect errors to STDOUT with `/errtostdout`, then the compiler may change the text and background colors in the console window to highlight the error location in the offending source file.