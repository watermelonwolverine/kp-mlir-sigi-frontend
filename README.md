# Sigi Frontend

This is the frontend for the Sigi language.
It implements lexing, parsing, and semantic analysis like type inference.
It also contains an interpreter (and REPL) to easily execute Sigi expressions.

Compilation of Sigi code to an executable is done via MLIR.
This frontend can emit MLIR code that uses the `sigi` and `closure` dialects of `sigi-mlir`.
The rest of the compilation pipeline is in the `sigi-mlir` repo.

# Requirements

Please install [just](https://github.com/casey/just) to use common building commands (see the [justfile](./justfile)).

For development you need Scala 3 and SBT. 
For deployment you need GraalVM's native image utility.

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
- `just repl`: run the interactive Sigi REPL
- `just sigiToMlir *ARGS`: execute the `sigi-to-mlir` utility with given arguments (output is on stdout)
  - `sigiToMlir -` takes input from standard input
  - `sigiToMlir fileName.sigi` takes input from a file
  - `just exprToMlir "sigi code"`: convenience wrapper to compile a single Sigi expression
