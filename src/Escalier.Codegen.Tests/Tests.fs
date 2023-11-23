[<VerifyXunit.UsesVerify>]
module Tests

open Xunit
open VerifyXunit
open VerifyTests
open FsToolkit.ErrorHandling

open Escalier.Codegen.TypeScript
open Escalier.Codegen.Printer
open Escalier.Codegen.Codegen
open Escalier.Parser


let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

let printCtx: PrintCtx = { Indent = 0; Precedence = 0 }

[<Fact>]
let CodegenIdent () =
  let ident: Ident = { Name = "foo"; Loc = None }
  let code = printExpr printCtx (Expr.Identifier ident)

  Assert.Equal("foo", code.ToString())

[<Fact>]
let CodegenLiteral () =
  let lit: Literal =
    { Value = LiteralValue.Number 1.23
      Loc = None }

  let code = printExpr printCtx (Expr.Literal lit)

  Assert.Equal("1.23", code.ToString())

[<Fact>]
let CodegenAddition () =
  let a: Ident = { Name = "a"; Loc = None }
  let b: Ident = { Name = "b"; Loc = None }

  let sum =
    Expr.Binary
      { Operator = BinaryOperator.Plus
        Left = Expr.Identifier a
        Right = Expr.Identifier b
        Loc = None }

  let code = printExpr printCtx sum

  Assert.Equal("a + b", code.ToString())

[<Fact>]
let CodegenExpressRequiresParens () =
  let a: Ident = { Name = "a"; Loc = None }
  let b: Ident = { Name = "b"; Loc = None }

  let one: Literal =
    { Value = LiteralValue.Number 1.0
      Loc = None }

  let two: Literal =
    { Value = LiteralValue.Number 2.0
      Loc = None }

  let sum =
    Expr.Binary
      { Operator = BinaryOperator.Plus
        Left = Expr.Identifier a
        Right = Expr.Literal one
        Loc = None }

  let diff =
    Expr.Binary
      { Operator = BinaryOperator.Minus
        Left = Expr.Identifier b
        Right = Expr.Literal two
        Loc = None }

  let prod =
    Expr.Binary
      { Operator = BinaryOperator.Multiply
        Left = sum
        Right = diff
        Loc = None }

  let code = printExpr printCtx prod

  Assert.Equal("(a + 1) * (b - 2)", code.ToString())

[<Fact>]
let CodegenNoParensExpression () =
  let a: Ident = { Name = "a"; Loc = None }
  let b: Ident = { Name = "b"; Loc = None }

  let one: Literal =
    { Value = LiteralValue.Number 1.0
      Loc = None }

  let two: Literal =
    { Value = LiteralValue.Number 2.0
      Loc = None }

  let prod =
    Expr.Binary
      { Operator = BinaryOperator.Multiply
        Left = Expr.Identifier a
        Right = Expr.Literal one
        Loc = None }

  let quot =
    Expr.Binary
      { Operator = BinaryOperator.Divide
        Left = Expr.Identifier b
        Right = Expr.Literal two
        Loc = None }

  let sum =
    Expr.Binary
      { Operator = BinaryOperator.Plus
        Left = prod
        Right = quot
        Loc = None }

  let code = printExpr printCtx sum

  Assert.Equal("a * 1 + b / 2", code.ToString())

[<Fact>]
let CodegenDoExpression () =
  let res =
    result {
      let src =
        """
        let sum = do {
          let x = 5
          let y = 10
          x + y
        }
        """

      let! escAst = Parser.parseScript src
      let ctx: Ctx = { NextTempId = 0 }
      let block = buildScript ctx escAst
      let js = block.Body |> List.map (printStmt printCtx) |> String.concat "\n"

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenNestedDoExpressions () =
  let res =
    result {
      let src =
        """
        let sum = do {
          let x = do {
            let a = 5
            let b = 10
            a + b
          }
          let y = do {
            let c = 15
            let d = 20
            c - d
          }
          x * y
        }
        """

      let! escAst = Parser.parseScript src
      let ctx: Ctx = { NextTempId = 0 }
      let block = buildScript ctx escAst
      let js = block.Body |> List.map (printStmt printCtx) |> String.concat "\n"

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenFunction () =
  let res =
    result {
      let src =
        """
        let factorial = fn (n) =>
          if (n == 0) { 1 } else { n * factorial(n - 1) } 
        """

      let! escAst = Parser.parseScript src
      let ctx: Ctx = { NextTempId = 0 }
      let block = buildScript ctx escAst

      let js = block.Body |> List.map (printStmt printCtx) |> String.concat "\n"

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenChainedIfElse () =
  let res =
    result {
      let src =
        """
        let result = if (cond1) {
          foo
        } else if (cond2) {
          bar
        } else {
          baz
        }
        """

      let! escAst = Parser.parseScript src
      let ctx: Ctx = { NextTempId = 0 }
      let block = buildScript ctx escAst

      let js = block.Body |> List.map (printStmt printCtx) |> String.concat "\n"

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"
