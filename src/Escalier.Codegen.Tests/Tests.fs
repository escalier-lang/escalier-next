[<VerifyXunit.UsesVerify>]
module Tests

open Xunit
open VerifyXunit
open VerifyTests
open FsToolkit.ErrorHandling

open Escalier.Compiler.Compiler
open Escalier.Data
open Escalier.Interop.TypeScript
open Escalier.Codegen.Printer
open Escalier.Codegen.Codegen
open Escalier.Parser
open Escalier.TypeChecker

let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

let projectRoot = __SOURCE_DIRECTORY__

let parseAndCodegen (src: string) =
  asyncResult {
    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    let! typeCtx, env = TestCompiler.getEnvAndCtx projectRoot

    let! env =
      InferModule.inferModule typeCtx env ast
      |> AsyncResult.mapError CompileError.TypeError

    let ctx: Ctx =
      { NextTempId = 0
        AutoImports = Set.empty }

    let jsMod = buildModule ctx ast
    let js = printModule jsMod

    let dtsMod = buildModuleTypes env ctx typeCtx ast false
    let dts = printModule dtsMod

    return js, dts
  }

let printExpr (expr: Expr) =
  let printCtx: PrintCtx =
    { Indent = 0
      Precedence = 0
      StringBuilder = System.Text.StringBuilder() }

  printExpr printCtx expr

  printCtx.StringBuilder.ToString()

[<Fact>]
let CodegenIdent () =
  let ident: Ident = { Name = "foo"; Loc = None }
  let code = printExpr (Expr.Ident ident)

  Assert.Equal("foo", code)

[<Fact>]
let CodegenLiteral () =
  let lit: Lit =
    Lit.Num
      { Value = Common.Float 1.23
        Raw = None
        Loc = None }

  let code = printExpr (Expr.Lit lit)

  Assert.Equal("1.23", code)

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

  let code = printExpr sum

  Assert.Equal("a + b", code)

[<Fact>]
let CodegenUnaryExpression () =
  let a: Ident = { Name = "a"; Loc = None }

  let sum =
    Expr.Unary
      { Operator = UnaryOperator.Minus
        Argument = Expr.Ident a
        Prefix = false
        Loc = None }

  let code = printExpr sum

  Assert.Equal("-a", code)

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

  let code = printExpr prod

  Assert.Equal("(a + 1) * (b - 2)", code)

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

  let code = printExpr sum

  Assert.Equal("a * 1 + b / 2", code)

[<Fact>]
let CodegenTemplateLiteral () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = 10;
        let p = `(${x}, ${y})`;
        let escapes = `"hello"\n\r\t'world'`;
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenTaggedTemplateLiteral () =
  let res =
    result {
      let src =
        """
        declare fn gql(strings: Array<string>, ...values: Array<unknown>) -> string;
        let id = "foo123";
        let query = gql`query {
          user(id: ${id}) {
            username
            password
          }
        }`;
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenAssignment () =
  let res =
    result {
      let src =
        """
        let mut x: number = 5;
        x = 10;
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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
        let mut arr: Array<number> = [1, 2];
        arr[2] = arr[0] + arr[1];
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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
        declare let mut foo: {bar: string};
        declare let a: {b?: {c: string}} | undefined;
        foo.bar = "baz";
        let c = a?.b?.c;
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) -> failwith $"error = %A{error}"

[<Fact>]
let CodegenCalls () =
  let res =
    result {
      let src =
        """
        declare fn parseInt(input: string) -> number;
        let num = parseInt("123");
        let array = Array(1, 2, 3);
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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
        let cond1 = Math.random() > 0.5;
        let cond2 = Math.random() > 0.5;
        let [foo, bar, baz] = ["foo", "bar", "baz"];
        let result = if (cond1) {
          foo
        } else if (cond2) {
          bar
        } else {
          baz
        };
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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
        let [a, b, c] = [true, false, true];
        let [x, y, z] = [true, true, false];
        let foo = a || b || c;
        let bar = x && y && z;
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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
        let mag = if let {point: {x, y}, color: _} = object {
          Math.sqrt(x * x + y * y)
        } else {
          0
        };
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) -> failwith $"error = %A{error}"

[<Fact>]
let CodegenMatchArray () =
  let res =
    result {
      let src =
        """
        let getCount = fn<T>(array: Array<T>) {
          return match array {
            [] => "none",
            [x] => "one",
            [x, y] => "a couple",
            _ => "many",
          };
        };
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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
        declare fn parseJSON(input: string) -> unknown throws "SyntaxError" | "RangeError";
        let input = "{\"x\": 5, \"y\": 10}";
        let result =
          try {
            parseJSON(input);
          } catch {
            "SyntaxError" => null,
            "RangeError" => null,
          };
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) -> failwith $"error = %A{error}"

[<Fact>]
let CodegenTryFinally () =
  let res =
    result {
      let src =
        """
        declare fn parseJSON(input: string) -> unknown throws "SyntaxError" | "RangeError";
        let input = "{\"x\": 5, \"y\": 10}";
        let result =
          try {
            parseJSON(input);
          } finally {
            console.log("cleaning up");
          };
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) -> failwith $"error = %A{error}"

[<Fact>]
let CodegenTryCatchFinally () =
  let res =
    result {
      let src =
        """
        declare fn parseJSON(input: string) -> unknown throws "SyntaxError" | "RangeError";
        let input = "{\"x\": 5, \"y\": 10}";
        let result =
          try {
            parseJSON(input);
          } catch {
            "SyntaxError" => null,
            "RangeError" => null,
          } finally {
            console.log("cleaning up");
          };
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) -> failwith $"error = %A{error}"

[<Fact>]
let CodegenJsxElement () =
  let res =
    result {
      let src =
        """
        import "react" as React;
        let foo = <div id="foo" class="bar">
          <p>hello</p>
        </div>;
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
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
        import "react" as React;
        let foo = <>
          <p>hello</p>
          <p>world</p>
        </>;
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

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

      let! typeCtx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let env = { env with Filename = "input.esc" }

      let! env =
        InferModule.inferModule typeCtx env escAst
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      let ctx: Ctx =
        { NextTempId = 0
          AutoImports = Set.empty }

      let mod' = buildModuleTypes env ctx typeCtx escAst false
      let dts = printModule mod'

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
        let fst = fn<A, B>(a: A, b: B) -> A => a;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let projectRoot = __SOURCE_DIRECTORY__

      let! typeCtx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let env = { env with Filename = "input.esc" }
      // TODO: as part of generalization, we need to update the function's
      // inferred type
      let! env =
        InferModule.inferModule typeCtx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      let ctx: Ctx =
        { NextTempId = 0
          AutoImports = Set.empty }

      let mod' = buildModuleTypes env ctx typeCtx ast false
      let dts = printModule mod'

      return $"input: %s{src}\noutput:\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) ->
    printfn "error = %A" error
    failwith "ParseError"

[<Fact>]
let CodegenFunctionDeclaration () =
  let res =
    result {
      let src =
        """
        fn add(a: number, b: number) -> number {
          return a + b;
        }
        let sum = add(5, 10);
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) -> failwith $"error = %A{error}"

[<Fact>]
let CodegenFunctionOverloads () =
  let res =
    result {
      let src =
        """
        fn add(a: number, b: number) -> number {
          return a + b;
        }
        fn add(a: string, b: string) -> string {
          return a ++ b;
        }
        let sum = add(5, 10);
        let msg = add("hello, ", "world");
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) -> failwith $"error = %A{error}"

[<Fact>]
let CodegenFunctionOverloadsWithDifferentParams () =
  let res =
    result {
      let src =
        """
        fn add(a: number, b: number, c: number) -> number {
          return a + b + c;
        }
        fn add(x: string, y: string) -> string {
          return x ++ y;
        }
        let sum = add(5, 10);
        let msg = add("hello, ", "world");
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) -> failwith $"error = %A{error}"


[<Fact>]
let CodegenFunctionOverloadsWithShadowing () =
  let res =
    result {
      let src =
        """
        fn add(a: number, b: number) -> number {
          let add = fn (a: number, b: number) => a + b;
          return add(a, b);
        }
        fn add(a: string, b: string) -> string {
          return a ++ b;
        }
        let sum = add(5, 10);
        let msg = add("hello, ", "world");
        """

      let! js, dts = parseAndCodegen src |> Async.RunSynchronously

      return
        $"input: %s{src}\n--- output (js) ---\n{js}\n--- output (dts) ---\n{dts}"
    }

  match res with
  | Ok(res) -> Verifier.Verify(res, settings).ToTask() |> Async.AwaitTask
  | Error(error) -> failwith $"error = %A{error}"
