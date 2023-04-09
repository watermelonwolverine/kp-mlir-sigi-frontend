package de.cfaed

/** The Sigi compiler frontend.
  *
  * The lexer is in [[tokens]], parser in [[ast]]. These modules use
  * parser combinators, see https://github.com/scala/scala-parser-combinators/blob/main/docs/Getting_Started.md
  * The parser builds a [[ast.KNode]] tree. This tree is turned into
  * a [[types.TypedExpr]] by the type checker ([[types.assignType]]).
  * Typed expressions are interpretable by the [[repl]] module and
  * can be emitted as MLIR code by [[emitmlir]]. Note that the [[repl]]
  * supports some features that the MLIR backend doesn't.
  *
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
package object sigi {

}
