# Sigi compilation

The compilation target for the MLIR emitter of this frontend is a mix of `closure`, `scf`, `func` and `sigi` dialect.
The closure dialect is already implemented in [sigi-mlir](https://github.com/tud-ccc/kp-mlir-sigi-mlir), while `scf` and `func` are [core MLIR dialects](https://mlir.llvm.org/docs/Dialects/). Implementing the `sigi` dialect is left to the student.

## Frontend details

To prepare for compilation, the frontend performs the regular stages of parsing and type checking.
The main difference between the compilation and interpreter pipeline is that the compiler will perform function monomorphisation after type inference, in order to get rid of all generic code in the generated IR. 
This increases code size, but MLIR does not support generics.
The interpreter, by contrast, interprets the AST directly with the assumption that nothing can go wrong if the program is well-typed.

[Builtin Sigi functions](SigiLang.md#builtins) are mostly implemented in the frontend.
The compilation strategy varies: 
- many builtins are handled specially by the MLIR generator, for example `pop` compiles to a call to `sigi.pop`.
- some builtins are defined as regular Sigi functions and are compiled like normal user-defined functions. For example `swap` is defined as `let swap = -> a, b; b a;;`.
- others have a pure MLIR implementation known in the frontend. For example `pp` emits a function declaration that is supposed to be handled by the middle-end, eg to bind it to a C function of the runtime.


## Sigi dialect spec

The `sigi` dialect needs to support the following components.

### Types

- `!sigi.stack` is an opaque type representing a Sigi runtime stack.

### Ops

These operators act on a `!sigi.stack`:

- `%stack2 = sigi.push %stack, %value : type(value)`: pushes a value on a `!sigi.stack`
- `%stack2, %value = sigi.pop %stack : type(value)`: pops a value from a `!sigi.stack`

Notice they take an argument for the stack and return a value for the new stack that is the result of the op.
This explicit data flow helps with processing in the MLIR framework, eg usage resolution and such. It can be eliminated when lowering from `sigi` to an implementation dialect like `llvm` (after all a Sigi program only needs one global stack).

At present the only value types supported are `i1` (boolean), `i32` (int), and `!closure.box<...>`, which represent closures. Sigi closures are always compiled to a closure with function type `(!sigi.stack) -> sigi.stack`.

### Attributes

- `sigi.main` identifies the main function of the compiled Sigi program. This attribute should be present on a `func.func` that has signature `(!sigi.stack) -> sigi.stack`. It can be used to generate wrapper code (stack initialization and cleanup) in the implementation dialect while lowering `sigi`.
- `sigi.builtinfunc` identifies a `func` declaration which was emitted by the frontend to represent a call to a function in the sigi runtime. The dialect can map those functions to however they want to implement them.


