module Graph

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Parser
open Escalier.TypeChecker
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
        Infer.buildDeclGraph ctx env ast
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
        Infer.buildDeclGraph ctx env ast
        |> Result.mapError CompileError.TypeError

      ()
    }

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
        Infer.buildDeclGraph ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sum", "15")
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
        Infer.buildDeclGraph ctx env ast
        |> Result.mapError CompileError.TypeError

      ()
    }

  printfn "res = %A" res
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
        Infer.buildDeclGraph ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sum", "15")
    }

  printfn "res = %A" res
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
        Infer.buildDeclGraph ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "sum", "15")
    }

  printfn "res = %A" res
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
        Infer.buildDeclGraph ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)
