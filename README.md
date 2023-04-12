# Sigi Frontend

This is the frontend for the Sigi language (see [language reference](/SigiLang.md)).
It implements lexing, parsing, and semantic analysis like type inference.
It also contains an interpreter (and REPL) to easily execute Sigi expressions.

Compilation of Sigi code to an executable is done via [MLIR](mlir.llvm.org/).
This frontend can emit MLIR code that uses the `sigi` and `closure` dialects of `sigi-mlir`.
These dialects, along with the rest of the compilation pipeline, are defined in the [`sigi-mlir` repo](https://github.com/tud-ccc/kp-mlir-sigi-mlir).

Note: the [`sigi` dialect reference](CompilationDetails.md#sigi-dialect-spec) is in this repo.

# Requirements

Please install [just](https://github.com/casey/just) to use common building commands (see the [justfile](./justfile)).

For development you need [Scala 3](https://scala-lang.org/) and SBT. 
For deployment you need GraalVM's [native image](https://www.graalvm.org/22.1/reference-manual/native-image/) utility.

You can install all of these easily with [sdkman](https://sdkman.io/).
If you don't have sdkman, run
```shell
just installSdkman
```
Once you have installed sdkman, run
```shell
just installRequirementsWithSdkman
```

# Summary of development commands

- `just test`: build the project and run tests
- `just build` (or `just b`): build binaries using native-image

After running `just b`, the following commands are available:
- `just repl`: run the interactive Sigi REPL;
- `just sigiToMlir [ - | FILE]`: execute the `sigi-to-mlir` utility with given arguments (output is on stdout):
  - `just sigiToMlir -` takes input from standard input,
  - `just sigiToMlir fileName.sigi` takes input from a file,
  - `just exprToMlir "sigi code"`: convenience wrapper to compile a single Sigi expression;
- `just interpretSigi [ - | FILE ]`: execute the interpreter on a Sigi file or standard input. This can be used to check that compiled code behaves like the interpreter. Arguments are like those of `sigiToMlir`.

Those commands use the binaries built with `just b`. Don't forget to run `just b` to update the binaries when you make a change.

All of those can be run from within an IDE by finding the correct `@main` function. This allows debugging and keeps your class files fresh.

