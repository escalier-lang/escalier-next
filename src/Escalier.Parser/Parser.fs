namespace Escalier.Parser

open FParsec

module Parser =
  let lit = run Literals.lit
  let expr = run Expressions.expr
  let pattern = run Patterns.pattern
  let stmt = run Statements.stmt
  let typeAnn = run TypeAnnotations.typeAnn

  let script = run (many Statements.stmt)
