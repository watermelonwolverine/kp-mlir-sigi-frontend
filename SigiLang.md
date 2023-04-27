# Sigi language reference

Sigi is a statically typed concatenative language derived from [Kitten](https://kittenlang.org/).
The language features type inference and first-class function values.

## Syntax

### Lexical analysis

Sigi tokens are the following:
- identifiers: `[a-zA-Z]\w*`, eg `a1`, `a_2`
- type variables: `'[a-z][a-z0-9]*`, eg `'a`, `'var0`
- row variables: `'[A-Z][A-Z0-9]*`, eg `'S`, `'R0`
- operators: 
  - binary:
    - arithmetic: `+`, `-`, `*`, `/`, `%`
    - comparison:  `=`, `<>`, `<=`, `<`, `>`, `>=`
  - unary:
    - arithmetic: `+`, `-`
    - bitwise: `~` (bitwise NOT on integer)
    - boolean: `!` (boolean NOT)
- literals:
  - numbers (decimal only): `0|[1-9][0-9]*`
  - booleans: `true`, `false`
  - strings: double quoted, like `"abc"`, `""`. Supported escapes are `\n`, `\r`, `\"`, `\\`.
- keywords: `if`, `elif`, `else`, `let`
- separators: `;;`, and usual punctuation

Comments are end-of-line and start with `#` or `//`.

### Expressions

Sigi is an expression language. The core language has the following expressions:
```ebnf
expr := literal                 // function that pushes a literal value
      | expr expr               // function composition
      | id | "(" op ")"         // function reference
      | "{" expr "}"            // quotation, ie push a closure
      | "->" "\"? id ";"        // pop the top of the stack and give it a name
```
The language has richer syntax for convenience, all of which reduces to the above expression forms.
Each expression is a function from a stack to another stack (or equivalently it performs side effects on an implicit stack).
Expressions have a static type that describes these effects.
Typing and evaluation rules are described in the semantics section below.

Additional expression forms:
- Writing `\id` or `\op` is shorthand for quotation. Eg `\double` is the same as `{ double }`, `\+` is the same as `{ (+) }`
- It is possible to compact several `-> id;` variable declarations into one pop expression, like `-> x, \f, y;`.
These ids are popped from the stack right-to-left: the top of the stack is always written on the right. This is equivalent to `-> y; -> \f; -> x;`
- Arithmetic expressions are supported with the usual precedence rules. They map to function application, eg
  - `1 + 2` is the same as `1 2 (+)`
  - `1 + 2 * 3` is the same as `1 2 3 (*) (+)`
- if/elif/else are used to build conditionals. For instance `if (a) b else c` is mapped to `a { b } { c } cond apply`, where `cond` and `apply` are [language builtins](#builtins).

Outside of expressions, is also possible to declare a function with the syntax `"let" id (":" stackTy)? "=" expr ";;"`.
For instance this is the definition of a function that squares its argument:
```
# this is a new function named "square"
let square: int -> int = dup (*);;

# this is the same, but the type is inferred
let square = dup (*);;
```


## Semantics

### Type system


#### Data types

Data types are the types of runtime values, i.e., values that can be placed on the stack. 
Data types include:
- simple types: they are simply the name of a type, eg `int`.
- parameterized types: the application of a type constructor to type arguments.
For instance `int list` is the application of the `list` type constructor to the type
argument `int`. Technically, simple types are parameterized types whose type ctors
require zero arguments.
- function types: the type of a function that transforms the top of the stack.
For instance `int -> str` requires an `int` on the top of the stack and replaces it with an `str`.
Function types can be understood as [stack types](#stack-types) when the function is applied.

Additionally, type variables, e.g. `'a` or `'b`, stand for an unknown data type and can occur anywhere a data type is expected.
Type variables are declared implicitly, and their name is only important within the same type.
For instance `'a -> 'a` is the type of a function that requires one argument but does not pop it from the stack.
It is the exact same type as `'b -> 'b`.

#### Stack types

Stack types are the types of *terms* of the language, i.e., of expressions.
They describe how the top of the stack changes when the term is evaluated.

A stack type is written $c_1, \ldots, c_n \to p_1, \ldots, p_m$.  This is the type of a function that pops $n$ arguments from the stack and pushes $m$ results. The $c_i$ and $p_j$ are [data types](#data-types) or type variables.

Arguments are popped right-to-left and results are pushed left-to-right (i.e., the top of the stack is always written on the right).
This convention allows easily seeing how the stack changes when the function is evaluated: here, the $c_1, \ldots, c_n$ (with $c_n$ the top) will be replaced by $p_1, \ldots, p_m$ (with $p_m$ the top).

#### Subtyping

Most types are singular and cannot have subtypes or supertypes.
However, defining a subtyping relation on stack types allows for more flexibility when declaring and using functions.

> Given types $s$ and $t$, $s <: t$ iff $s = t$, or $s = (\mathbf{a} \to \mathbf{b}), t = (\mathbf{c}\to\mathbf{d})$ and there exists a substitution $\theta$ such that $\theta\mathbf{b} <: \mathbf{d}$ and $\theta\mathbf{a} >: \mathbf{c}$.

This is the classic subtyping rule for function types, whereby subtypes of a function type are the function types of more general functions, and supertypes are more specific.
The substitution thing has to do with generic functions.
The type $s = {'}a, {'}b \to {'}a$ is more general than $t = {'}c, {'}c \to {'}c$, because $s$ can accept more input types.
That means it should hold that $s <: t$.
To show this, we define a substitution $\theta$ that maps both `'a` and `'b` to `'c`.
It then holds that $\theta s = t$, so $\theta s <: t$ and so $s <: t$.

#### Type inference

Sigi does not usually require any type annotations.
Type annotations can only be written on explicit function declarations (those that use the `let` keyword).
They are only required if the function is recursive, or if the function is referred to by other functions declared before it (forward references).

```
let a = 1;; // a: -> int
let b = a;; // b: -> int
let c = d;; // illegal forward reference to d, because d has no type annotation
let d = d;; // illegal recursive call, because d has no type annotation
```

Type inference is performed while type-checking expressions as explained in the following section.

### Typing and evaluation

> - `{ e }` creates a function value whose expansion is the term `e` and pushes it on the stack. If $\mathtt{e}: t$, then $\mathtt{\lbrace e \rbrace} : \to t$.
> - `-> x;` pops the value on top of the stack and binds it to a name in the enclosing scope. The type of this expression is ${'}a \to$, where ${'}a$ is a fresh type variable.
> - `-> \x;` pops the value on top of the stack and binds it to a name in the enclosing scope.
> The name binds to a function of maximally general type $({'}A \to {'}B)$ (see [row polymorphism](#row-polymorphic-types)). The type of this expression is therefore $({'}A \to {'}B) \to$.
> - `id` resolves a name in the enclosing scope. Names resolve to a *value*, a value having a data type, not a stack type.
>   - If `id` refers to a function of type $\mathbf{c}\to\mathbf{p}$, which was declared either with `-> \id;` or `let id ...`, or is a builtin, then that function is applied to the stack. The expression has stack type $\mathbf{c}\to\mathbf{p}$.
>   - If `id` otherwise refers to a value of type $t$, then that value is pushed to the stack. The expression has stack type $\to t$.  Note that for this rule to apply the name must have been declared with `-> id;`.
>   - If `id` does not refer to a name in scope, the expression is not well-typed and a compile-time error occurs.
> - `e1 e2` evaluates `e1`, then evaluates `e2`. In more abstract terms, this denotes the *composition* of both stack functions.

#### Typing the composition rule
 
Intuitively, in the term `e1 e2`, the composition rule drives type inference by unifying the outputs of `e1` with the inputs of `e2`.
For instance, let `e1: str -> int` and `e2: 'a -> str`.
For the term to be well-formed, the outputs of `e1` (`int`) must be compatible with the inputs of `e2` (`'a`). It must then hold that `'a = int`, hence we have inferred the type variable `'a`.
Since the intermediate `int` produced by `e1` is immediately consumed by `e2`, it does not appear in the type of the composition `e1 e2: str -> str`.

> Assume $e_1: a_1,\ldots,a_n \to b_1,\ldots,b_m$ and $e_2: c_1,\ldots,c_t \to d_1,\ldots,d_s$. Let $q = \min (m,t)$. 
Assume, to simplify, that all the $a_i, b_i, c_i, d_i$ are either data types or type variables, but not row variables (see [below](#row-polymorphic-types)).
Then $e_1\ e_2$ is well-typed if there exists a substitution $\theta$ such that for all $k$, $0 \leq k < q$, we have 
>
> $$\theta\mathbf{c}_{t-k} <: \theta\mathbf{b}_{m-k},$$
>
> where $<:$ is the [subtyping](#subtyping) relation.
If that is so, then the type of the term is given by 
>
> $$e_1\ e_2 : \theta\mathbf{a}, \theta(\mathbf{c}_i)_{i < {t-q}} \to \theta(\mathbf{b}_j)_{{m-q}\leq j}, \theta\mathbf{d}.$$
>
The rule allows "spilling" of arguments that are not relevant to the composition rule, for instance, if `e1: 'a -> 'a, int` and `e2: -> str`, `e1 e2` is still well-typed, even though `e2` does not consume the results of `e1`. The composed type is `'a -> 'a, int, str`.

#### Scoping and the composition rule

> If `e1 e2` is well-typed as described above, then all names introduced by `e1` are in scope in `e2`, and all names introduced by `e1` and `e2` are introduced by `e1 e2` in the enclosing scope.
> The types of these names are substituted with $\theta$ in the enclosing scope.

This means that names are in scope in every term that follows them.
The use of the substitution gives more specific types to the bindings.
For instance, in a vacuum, `1` has type `-> int` and `-> x;` has type `'a ->`. In the expression `1 -> x;`, the type inference engine unifies `'a` with `int`, so that the binding `x` has type `int` in the following expressions.

#### Row polymorphic types

Note that the typing of the composition rule as described here ignores *row variables* for conciseness. Row polymorphism allows typing higher order functions and provides greater flexibility to the type system. It boils down to the idea that stack types may contain row variables, which can unify with an entire list of data types. For example, the higher-order builtin `apply` has type
```
apply: 'S, ('S -> 'R) -> 'R
```
where `'S` and `'R` are row variables. Given the term `(\+) apply` for instance, we will infer a row variable substitution `'S := int, int`, `'R := int` , so that the ground type of `apply` is `int, int, (int, int -> int) -> int`. The type of `(\+) apply` is therefore `int, int -> int` (in fact the type of `\f apply` is always the type of `f`).

Note that a function type $(c)_n \rightarrow (p)_m$, where no $c_i$ or $p_i$ is a row variable, is canonicalised to ${'}S, (c)_n \rightarrow {'}S, (p)_m$.
The introduced row variable represents "the rest of the stack", which in a trivial function type like this is just preserved.

#### Inference within higher-order functions

The two forms for variable naming `-> x;` and `-> \x;` are here to disambiguate what the term `x` means in the following. 

`-> x;` binds the name `x` to an unknown data type `'a` that will be resolved by type inference.
Writing `x` will push that `'a` value on the stack, ie the expression `x` has stack type `-> 'a`.

`-> \f;` additionally constrains the top of the stack to be a function value of type `'A -> 'B`.
The expression `f` invokes that function, instead of pushing the function on the stack.
It therefore has type `'A -> 'B` and not `-> ('A -> 'B)`.

The second form is useful within higher-order functions, which receive function values as parameters.
Note that with the `-> x;` form, `x` may still bind to a function value, but to invoke it you need to write `x apply` and not just `x`.
Similarly, with the `-> \x;` form, you can push the function on the stack without invoking it by writing `\x` or `{ x }`.

## Builtins

Builtin type constructors:
- `bool`, a boolean type
- `int`, a signed 32-bit integer

Builtin functions, apart from operators:
```
# Discard the top of the stack
pop: 'a ->
# Duplicate the top of the stack
pop: 'a -> 'a, 'a
# Swap the two values at the top of the stack
swap: 'a, 'b -> 'b, 'a
# Select a value using a boolean
cond: bool, 'a, 'a -> 'a
# Invoke a function value that is at the top of the stack
apply: 'S, ('S -> 'R) -> 'R
# Compose the two functions on the top of the stack
compose: ('A -> 'B), ('B -> 'C) -> ('A -> 'C)
# Quote the value on top of the stack
quote: 'a -> (-> 'a)
# Pop the top of the stack and print it with a newline
show: 'a ->
# Print the top of the stack without popping it
pp: 'a -> 'a
# Do nothing
pass: 'S -> 'S
```


## WIP

The following are built-in type constructors that are not fully supported by the language:
- `str`, a string type
- `'a list`, a list type

Both of those have expression forms to create them (eg `"abc"`, `[1, 2]`), and can participate in type inference properly.
However, they are only supported by the REPL and not by the MLIR emitter.
They also lack functions to manipulate them and do something useful with them.

















