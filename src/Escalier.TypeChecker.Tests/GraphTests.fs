module GraphTests

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Parser
open Escalier.TypeChecker.Graph
open Escalier.Compiler
open Escalier.TypeChecker.Error
open TestUtils

[<Fact>]
let BuildDeclGraph () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = x;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let BuildDeclGraphIncorrectOrder () =
  let res =
    result {
      let src =
        """
        let y = x;
        let x = 5;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      ()
    }

  printfn "res = %A" res

  Assert.Equal(
    res,
    Error(
      TypeError.SemanticError "x has not been initialized yet"
      |> CompileError.TypeError
    )
  )

[<Fact>]
let BuildDeclGraphWithFunction () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = 10;
        let add = fn () => x + y;
        let sum = add();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithFunctionWithParam () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let add = fn (y) => x + y;
        let sum = add(10);
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "add", "fn <A: number>(y: A) -> 5 + A")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let BuildDeclGraphWithGenericFunction () =
  let res =
    result {
      let src =
        """
        let id = fn (x) => x;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "id", "fn <A>(x: A) -> A")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let BuildDeclGraphWithFunctions () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = 10;
        let obj = {foo: fn () => x, bar: fn () => y};
        let {foo, bar} = obj;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      ()
    }

  Assert.True(Result.isOk res)

[<Fact>]
let BuildDeclGraphWithCapturesDefinedAfterClosure () =
  let res =
    result {
      let src =
        """
        let add = fn () => x + y;
        let x = 5;
        let y = 10;
        let sum = add();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithNonFunctionDeps () =
  let res =
    result {
      let src =
        """
        let add = fn () => x + y;
        let x = 5;
        let y = x + 5;
        let sum = add();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithFunctionCallDeps () =
  let res =
    result {
      let src =
        """
        let add = fn () => x + y;
        let sub = fn () => x - y;
        let poly = fn () => sum * diff;
        let x = 5;
        let y = 10;
        let zero = 0;
        let sum = add() + zero;
        let diff = sub() + zero;
        let result = poly();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "sum", "15")
      Assert.Value(env, "diff", "-5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithFunctionCallDepsWithObjects () =
  let res =
    result {
      let src =
        """
        let math = {add: fn () => x + y, sub: fn () => x - y};
        let poly = fn () => values.sum * values.diff;
        let x = 5;
        let y = 10;
        let zero = 0;
        let values = {sum: math.add() + zero, diff: math.sub() + zero};
        let result = poly();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "math", "{add: fn () -> 15, sub: fn () -> -5}")
      // TODO: simplify these values
      Assert.Value(env, "values", "{sum: 15 + 0, diff: -5 + 0}")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithFunctionsInObject () =
  let res =
    result {
      let src =
        """
        let math = {add: fn () => x + y};
        let x = 5;
        let y = 10;
        let sum = math.add();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)


[<Fact>]
let AcyclicFunctionDeps () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let add = fn () => x + y;
        let sub = fn () => x - y;
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsBuildGraphFirst () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let add = fn () => x + y;
        let sub = fn () => x - y;
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsInObject () =
  let res =
    result {
      let src =
        """
        let poly = fn() => math.add() * math.sub();
        let math = {add: fn () => x + y, sub: fn () => x - y};
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "math", "{add: fn () -> 15, sub: fn () -> -5}")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsInObjectWithDestructuring () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let {add, sub} = {add: fn () => x + y, sub: fn () => x - y};
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsInObjectWithDestructuringInSeparateStatement () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let obj = {add: fn () => x + y, sub: fn () => x - y};
        let {add, sub} = obj;
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact(Skip = "Fix stackoverflow")>]
let AcyclicFunctionDepsInObjectWithDestructuringStress () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let {add, x} = {add: fn () => x + y, x: 5};
        let {sub, y} = {sub: fn () => x - y, y: 10};
        let result = poly();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BuildRecursiveGraph () =
  let res =
    result {
      let src =
        """
        let fact = fn (n) => if n == 0 { 1 } else { n * fact(n - 1) };
        let fib = fn (n) => if n <= 1 { n } else { fib(n - 1) + fib(n - 2) };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      // TODO: merge 1 and number
      // TODO: maintain the name of the function argument
      Assert.Value(env, "fact", "fn (arg0: number) -> 1 | number")
      Assert.Value(env, "fib", "fn (arg0: number) -> number")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveGraph () =
  let res =
    result {
      let src =
        """
        let isEven = fn (n) => if n == 0 { true } else { isOdd(n - 1) };
        let isOdd = fn (n) => if n == 0 { false } else { isEven(n - 1) };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let decls = getDeclsFromModule ast

      let! graph =
        buildGraph env [] [] decls |> Result.mapError CompileError.TypeError

      printfn "graph.Edges = %A" graph.Edges

      ()
    // let! env =
    //   Infer.inferModuleUsingGraph ctx env ast
    //   |> Result.mapError CompileError.TypeError
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveGraphInObjects () =
  let res =
    result {
      let src =
        """
        let foo = {
          isEven: fn (n) => if n == 0 { true } else { bar.isOdd(n - 1) }
        };
        let bar = {
          isOdd: fn (n) => if n == 0 { false } else { foo.isEven(n - 1) } 
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      // TODO: simplify return types to `boolean`
      Assert.Value(
        env,
        "foo",
        "{isEven: fn (n: number) -> true | false | true | false}"
      )

      Assert.Value(
        env,
        "bar",
        "{isOdd: fn (n: number) -> false | true | false}"
      )
    }

  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveGraphInDeppObjects () =
  let res =
    result {
      let src =
        """
        let foo = {
          math: {
            isEven: fn (n) => if n == 0 { true } else { bar.math.isOdd(n - 1) },
          },
        };
        let bar = {
          math: {
            isOdd: fn (n) => if n == 0 { false } else { foo.math.isEven(n - 1) }, 
          },
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      // TODO: simplify return types to `boolean`
      Assert.Value(
        env,
        "foo",
        "{math: {isEven: fn (n: number) -> true | false | true | false}}"
      )

      Assert.Value(
        env,
        "bar",
        "{math: {isOdd: fn (n: number) -> false | true | false}}"
      )
    }

  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveTypeDecl () =
  let res =
    result {
      let src =
        """
        type Foo = {foo: number | Bar["bar"][]};
        type Bar = {bar: string | Foo["foo"][]};
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Foo", "{foo: number | Bar[\"bar\"][]}")
      Assert.Type(env, "Bar", "{bar: string | Foo[\"foo\"][]}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)


[<Fact>]
let MergeInterfaceBetweenFiles () =
  let res =
    result {
      let src =
        """
        interface Keys {foo: "foo"}
        declare let keys: Keys;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Keys", "{foo: \"foo\"}")

      let src =
        """
        interface Keys { bar: "bar"}
        interface Obj {
          [keys.foo]: number,
          [keys.bar]: number,
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Keys", "{foo: \"foo\", bar: \"bar\"}")

      Assert.Type(env, "Obj", "{foo: number, bar: number}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)
