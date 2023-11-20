module Tests

open System.Text
open Escalier.Codegen.TypeScript
open Escalier.Codegen.Printer
open Xunit

[<Fact>]
let CodegenIdent () =
  let ident: Identifier = { Name = "foo"; Loc = None }
  let sb = StringBuilder()
  let code = printExpr sb 0 (Expression.Identifier ident)

  Assert.Equal("foo", code.ToString())

[<Fact>]
let CodegenLiteral () =
  let lit: Literal =
    { Value = LiteralValue.Number 1.23
      Loc = None }

  let sb = StringBuilder()
  let code = printExpr sb 0 (Expression.Literal lit)

  Assert.Equal("1.23", code.ToString())

[<Fact>]
let CodegenAddition () =
  let a: Identifier = { Name = "a"; Loc = None }
  let b: Identifier = { Name = "b"; Loc = None }

  let sum =
    Expression.Binary
      { Operator = BinaryOperator.Plus
        Left = Expression.Identifier a
        Right = Expression.Identifier b
        Loc = None }

  let sb = StringBuilder()
  let code = printExpr sb 0 sum

  Assert.Equal("a + b", code.ToString())

[<Fact>]
let CodegenExpressRequiresParens () =
  let a: Identifier = { Name = "a"; Loc = None }
  let b: Identifier = { Name = "b"; Loc = None }

  let one: Literal =
    { Value = LiteralValue.Number 1.0
      Loc = None }

  let two: Literal =
    { Value = LiteralValue.Number 2.0
      Loc = None }

  let sum =
    Expression.Binary
      { Operator = BinaryOperator.Plus
        Left = Expression.Identifier a
        Right = Expression.Literal one
        Loc = None }

  let diff =
    Expression.Binary
      { Operator = BinaryOperator.Minus
        Left = Expression.Identifier b
        Right = Expression.Literal two
        Loc = None }

  let prod =
    Expression.Binary
      { Operator = BinaryOperator.Multiply
        Left = sum
        Right = diff
        Loc = None }

  let sb = StringBuilder()
  let code = printExpr sb 0 prod

  Assert.Equal("(a + 1) * (b - 2)", code.ToString())

[<Fact>]
let CodegenNoParensExpression () =
  let a: Identifier = { Name = "a"; Loc = None }
  let b: Identifier = { Name = "b"; Loc = None }

  let one: Literal =
    { Value = LiteralValue.Number 1.0
      Loc = None }

  let two: Literal =
    { Value = LiteralValue.Number 2.0
      Loc = None }

  let prod =
    Expression.Binary
      { Operator = BinaryOperator.Multiply
        Left = Expression.Identifier a
        Right = Expression.Literal one
        Loc = None }

  let quot =
    Expression.Binary
      { Operator = BinaryOperator.Divide
        Left = Expression.Identifier b
        Right = Expression.Literal two
        Loc = None }

  let sum =
    Expression.Binary
      { Operator = BinaryOperator.Plus
        Left = prod
        Right = quot
        Loc = None }

  let sb = StringBuilder()
  let code = printExpr sb 0 sum

  Assert.Equal("a * 1 + b / 2", code.ToString())
