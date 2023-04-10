# Sigi dialect spec

The compilation target for the MLIR emitter is a mix of `closure`, `scf`, `func` and `sigi` dialect.

The `sigi` dialect needs to support the following components.

## Types

- `!sigi.stack` is an opaque type representing a Sigi runtime stack.

## Ops

These operators act on a `!sigi.stack`:

- `%stack2 = sigi.push %stack, %value : type(value)`: pushes a value on a `!sigi.stack`
- `%stack2, %value = sigi.pop %stack : type(value)`: pops a value from a `!sigi.stack`

Notice they take an argument for the stack and return a value for the new stack that is the result of the op.
This explicit data flow helps with processing in the MLIR framework, eg usage resolution and such. It can be eliminated when lowering from `sigi` to an implementation dialect like `llvm` (after all a Sigi program only needs one global stack).

At present the only value types supported are `i1` (boolean), `i32` (int), and `!closure.box<...>`, which represent closures. Sigi closures are always compiled to a closure with function type `(!sigi.stack) -> sigi.stack`.

## Attributes

- `sigi.main` identifies the main function of the compiled Sigi program. This attribute should be present on a `func.func` that has signature `(!sigi.stack) -> sigi.stack`. It can be used to generate wrapper code (stack initialization and cleanup) in the implementation dialect while lowering `sigi`.
- `sigi.builtinfunc` identifies a `func` declaration which was emitted by the frontend to represent a call to a function in the sigi runtime. The dialect can map those functions to however they want to implement them.


