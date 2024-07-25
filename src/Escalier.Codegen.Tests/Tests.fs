[<VerifyXunit.UsesVerify>]
module Tests

open Xunit
open VerifyXunit
open VerifyTests
open FsToolkit.ErrorHandling

open Escalier.Compiler
open Escalier.Data
open Escalier.Interop.TypeScript
open Escalier.Codegen.Printer
open Escalier.Codegen.Codegen
open Escalier.Parser
open Escalier.TypeChecker

let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

let printCtx: PrintCtx = { Indent = 0; Precedence = 0 }

let parseAndCodegenJS (src: string) =
  result {
    let! escAst = Parser.parseModule src

    let ctx: Ctx =
      { NextTempId = 0
        AutoImports = Set.empty }

    let mod' = buildModule ctx escAst
    let js = printModule printCtx mod'

    return js
  }

[<Fact>]
let CodegenIdent () =
  let ident: Ident = { Name = "foo"; Loc = None }
  let code = printExpr printCtx (Expr.Ident ident)

  Assert.Equal("foo", code.ToString())

[<Fact>]
let CodegenLiteral () =
  let lit: Lit =
    Lit.Num
      { Value = Common.Float 1.23
        Raw = None
        Loc = None }

  let code = printExpr printCtx (Expr.Lit lit)

  Assert.Equal("1.23", code.ToString())

[<Fact>]
let CodegenBinaryExpression () =
  let a: Ident = { Name = "a"; Loc = None }
  let b: Ident = { Name = "b"; Loc = None }

  let sum =
    Expr.Bin
      { Operator = BinOp.Add
        Left = Expr.Ident a
        Right = Expr.Ident b
        Loc = None }

  let code = printExpr printCtx sum

  Assert.Equal("a + b", code.ToString())

[<Fact>]
let CodegenUnaryExpression () =
  let a: Ident = { Name = "a"; Loc = None }

  let sum =
    Expr.Unary
      { Operator = UnaryOperator.Minus
        Argument = Expr.Ident a
        Prefix = false
        Loc = None }

  let code = printExpr printCtx sum

  Assert.Equal("-a", code.ToString())

[<Fact>]
let CodegenExpressRequiresParens () =
  let a: Ident = { Name = "a"; Loc = None }
  let b: Ident = { Name = "b"; Loc = None }

  let one: Lit =
    Lit.Num
      { Value = Common.Float 1.0
        Raw = None
        Loc = None }

  let two: Lit =
    Lit.Num
      { Value = Common.Float 2.0
        Raw = None
        Loc = None }

  let sum =
    Expr.Bin
      { Operator = BinOp.Add
        Left = Expr.Ident a
        Right = Expr.Lit one
        Loc = None }

  let diff =
    Expr.Bin
      { Operator = BinOp.Sub
        Left = Expr.Ident b
        Right = Expr.Lit two
        Loc = None }

  let prod =
    Expr.Bin
      { Operator = BinOp.Mul
        Left = sum
        Right = diff
        Loc = None }

  let code = printExpr printCtx prod

  Assert.Equal("(a + 1) * (b - 2)", code.ToString())

[<Fact>]
let CodegenNoParensExpression () =
  let a: Ident = { Name = "a"; Loc = None }
  let b: Ident = { Name = "b"; Loc = None }

  let one: Lit =
    Lit.Num
      { Value = Common.Float 1.0
        Raw = None
        Loc = None }

  let two: Lit =
    Lit.Num
      { Value = Common.Float 2.0
        Raw = None
        Loc = None }

  let prod =
    Expr.Bin
      { Operator = BinOp.Mul
        Left = Expr.Ident a
        Right = Expr.Lit one
        Loc = None }

  let quot =
    Expr.Bin
      { Operator = BinOp.Div
        Left = Expr.Ident b
        Right = Expr.Lit two
        Loc = None }

  let sum =
    Expr.Bin
      { Operator = BinOp.Add
        Left = prod
        Right = quot
        Loc = None }

  let code = printExpr printCtx sum

  Assert.Equal("a * 1 + b / 2", code.ToString())

[<Fact>]
let CodegenAssignment () =
  let res =
    result {
      let src =
        """
        let mut x: number = 5;
        x = 10;
        """

      let! js = parseAndCodegenJS src

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenIndexing () =
  let res =
    result {
      let src =
        """
        let mut arr: number[] = [1, 2];
        arr[2] = arr[0] + arr[1];
        """

      let! js = parseAndCodegenJS src

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenMemberAccess () =
  let res =
    result {
      let src =
        """
        foo.bar = "baz";
        let c = a?.b?.c;
        """

      let! js = parseAndCodegenJS src

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenDoExpression () =
  let res =
    result {
      let src =
        """
        let sum = do {
          let x = 5;
          let y = 10;
          x + y
        };
        """

      let! js = parseAndCodegenJS src

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenDoExpressionWithReturn () =
  let res =
    result {
      let src =
        """
        let foo = fn (bar: number) {
          let res = do {
            if (bar == 0) {
              "none";
            } else if (bar > 1) {
              "many";
            } else {
              return null;
            }
          };
          return res;
        };
        """

      let! js = parseAndCodegenJS src

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
            let a = 5;
            let b = 10;
            a + b
          };
          let y = do {
            let c = 15;
            let d = 20;
            c - d
          };
          x * y
        };
        """

      let! js = parseAndCodegenJS src

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
          if (n == 0) { 1 } else { n * factorial(n - 1) }; 
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenAsyncFunction () =
  let res =
    result {
      let src =
        """
        let fetchJSON = async fn (url: string) {
          let res = await fetch(url);
          return res.json();
        };
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenCalls () =
  let res =
    result {
      let src =
        """
        let num = parseInt("123");
        let array = new Array(1, 2, 3);
        """

      let! js = parseAndCodegenJS src

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
        };
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenLogicalOperators () =
  let res =
    result {
      let src =
        """
        let foo = a || b || c;
        let bar = x && y && z;
        """

      let! js = parseAndCodegenJS src

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenLogicalOperatorsWithDoExpressions () =
  let res =
    result {
      let src =
        """
        let foo = do {
          let x = Math.random();
          if x > 0.5 { true } else { false }
        } || do {
          let y = Math.random();
          if y > 0.5 { true } else { false }
        };
        """

      let! js = parseAndCodegenJS src

      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenTupleLiteral () =
  let res =
    result {
      let src =
        """
        let tuple = ["hello", 5, true];
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenImmutableTupleLiteral () =
  let res =
    result {
      let src =
        """
        let tuple = #["hello", 5, true];
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenObjectLiteral () =
  let res =
    result {
      let src =
        """
        let object = {a: "hello", b: 5, c: true};
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenImmutableObjectLiteral () =
  let res =
    result {
      let src =
        """
        let object = #{a: "hello", b: 5, c: true};
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenDestructureObjects () =
  let res =
    result {
      let src =
        """
        let object = {point: {x: 5, y: 10}, color: "red"};
        let {point: {x, y}, color} = object;
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenDestructureObjectWithRest () =
  let res =
    result {
      let src =
        """
        let object = {foo: 5, bar: "hello", baz: true};
        let {foo, ...rest} = object;
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenDestructureTuples () =
  let res =
    result {
      let src =
        """
        let tuple = ["hello", [5, true]];
        let [msg, [num, flag]] = tuple;
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenDestructureTupleWithRest () =
  let res =
    result {
      let src =
        """
        let tuple = [5, "hello", true];
        let [foo, ...rest] = tuple;
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenIfLetObject () =
  let res =
    result {
      let src =
        """
        let object = {point: {x: 5, y: 10}, color: "red"};
        let mag = if let {point: {x, y}} = object {
          Math.sqrt(x * x + y * y)
        } else {
          0
        };
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenMatchArray () =
  let res =
    result {
      let src =
        """
        let getCount = fn<T>(array: T[]) {
          return match array {
            [] => "none",
            [x] => "one",
            [x, y] => "a couple",
            _ => "many",
          };
        };
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenMatchUnion () =
  let res =
    result {
      let src =
        """
        type Shape = {kind: "circle", radius: number} | {kind: "rect", width: number, height: number};
        let getCount = fn(shape: Shape) {
          return match shape {
            {kind: "circle", radius: r} => {
              let area = Math.PI * r * r;
              area
            }
            {kind: "rect", width: w, height: h} => {
              let area = w * h;
              area
            }
          };
        };
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenTryCatch () =
  let res =
    result {
      let src =
        """
        let result =
          try {
            JSON.parse(input);
          } catch {
            "SyntaxError" => null,
            "RangeError" => null,
          };
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenTryFinally () =
  let res =
    result {
      let src =
        """
        let result =
          try {
            JSON.parse(input);
          } finally {
            cleanup();
          };
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenTryCatchFinally () =
  let res =
    result {
      let src =
        """
        let result =
          try {
            JSON.parse(input);
          } catch {
            "SyntaxError" => null,
            "RangeError" => null,
          } finally {
            cleanup();
          };
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenJsxElement () =
  let res =
    result {
      let src =
        """
        import "react" {React};
        let foo = <div id="foo" class="bar">
          <p>hello</p>
        </div>;
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenJsxFragment () =
  let res =
    result {
      let src =
        """
        import "react" {React};
        let foo = <>
          <p>hello</p>
          <p>world</p>
        </>;
        """

      let! js = parseAndCodegenJS src
      return $"input: %s{src}\noutput:\n{js}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"


type CompileError = Prelude.CompileError

[<Fact>]
let CodegenDtsBasics () =
  let res =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        let add = fn (a: number, b: number) => a + b;
        """

      let! escAst =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let projectRoot = __SOURCE_DIRECTORY__
      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let env = { env with Filename = "input.esc" }

      let! env =
        Infer.inferModule ctx env escAst
        |> Result.mapError CompileError.TypeError

      let ctx: Ctx =
        { NextTempId = 0
          AutoImports = Set.empty }

      let mod' = buildModuleTypes env ctx escAst
      let dts = printModule printCtx mod'

      return $"input: %s{src}\noutput:\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenDtsGeneric () =
  let res =
    result {
      let src =
        """
        let fst = fn (a, b) => a;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let projectRoot = __SOURCE_DIRECTORY__
      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let env = { env with Filename = "input.esc" }
      // TODO: as part of generalization, we need to update the function's
      // inferred type
      let! env =
        Infer.inferModule ctx env ast |> Result.mapError CompileError.TypeError

      let ctx: Ctx =
        { NextTempId = 0
          AutoImports = Set.empty }

      let mod' = buildModuleTypes env ctx ast
      let dts = printModule printCtx mod'

      return $"input: %s{src}\noutput:\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"
