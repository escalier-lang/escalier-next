module Escalier.TypeChecker.Tests.QualifiedGraphTests

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker

open TestUtils

[<Fact>]
let BuildDeclGraph () =
  let res =
    result {
      let src =
        """
        let x = 5;
        namespace Foo {
          let x = "hello";
          namespace Bar {
            let y = x;
          }
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let graph = QualifiedGraph.getIdentsForModule ast
      ()
    }

  Assert.True(Result.isOk res)

[<Fact>]
let NestedNamespaceOnly () =
  let res =
    result {
      let src =
        """
        namespace Foo {
          namespace Bar {
            let x = 5;
          }
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let graph = QualifiedGraph.getIdentsForModule ast
      ()
    }

  Assert.True(Result.isOk res)

[<Fact>]
let BasicGraphInferCompositeValues () =
  let res =
    result {
      let src =
        """
        let [x, y] = [5, "hello"];
        let {a, b} = {a: 10, b: "world"};
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let graph = QualifiedGraph.getIdentsForModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "\"hello\"")
      Assert.Value(env, "a", "10")
      Assert.Value(env, "b", "\"world\"")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BasicGraphInferTypes () =
  let res =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        type Foo<T> = {bar: T | Foo<T>[]};
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let graph = QualifiedGraph.getIdentsForModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Point", "{x: number, y: number}")
      Assert.Type(env, "Foo", "<T>({bar: T | Foo<T>[]})")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BasicGraphInferFunctionDecl () =
  let res =
    result {
      let src =
        """
        fn add(x: number, y: number) -> number {
          return x + y;
        }
        fn fst<T, U>(x: T, y: U) -> T {
          return x;
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let graph = QualifiedGraph.getIdentsForModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "add", "fn (x: number, y: number) -> number throws t2")
      Assert.Value(env, "fst", "fn <T, U>(x: T, y: U) -> T throws t14")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact(Skip = "TODO")>]
let BasicDeps () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = x;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let graph = QualifiedGraph.getIdentsForModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)
