module Escalier.TypeChecker.Tests.QualifiedGraphTests

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker

open TestUtils

[<Fact(Skip = "TODO: namespaces")>]
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

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast
      ()
    }

  Assert.True(Result.isOk res)

[<Fact(Skip = "TODO: namespaces")>]
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

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast
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

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast

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

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast

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

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "add", "fn (x: number, y: number) -> number")
      Assert.Value(env, "fst", "fn <T, U>(x: T, y: U) -> T")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
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

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BasicFunctionCaptures () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = 10;
        fn add() {
          return x + y;
        }
        let sum = add();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "sum", "15")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let OutOfOrderFunctionCaptures () =
  let res =
    result {
      let src =
        """
        fn add() {
          return x + y;
        }
        let x = 5;
        let y = 10;
        let sum = add();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "sum", "15")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let OutOfOrderTypeDepsWithTypeParamConstraint () =
  let res =
    result {
      let src =
        """
        type Bar<T: Baz> = {bar: T};
        type Baz = string;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast
      printfn $"graph = {graph}"

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      ()
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BasicInterface () =
  let res =
    result {
      // We've been setting the `Inferred` field on declarations to the inferred
      // type, but what should we do when there are multiple declarations that
      // result in a shared typed?
      let src =
        """
        interface FooBar { foo: number }
        // interface FooBar { bar: string }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = QualifiedGraph.getIdentsForModule env ast
      printfn $"graph = {graph}"

      let! env =
        QualifiedGraph.inferGraph ctx env graph
        |> Result.mapError CompileError.TypeError

      ()
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact(Skip = "TODO")>]
let MergeInterfaceBetweenFiles () =
  let res =
    result {
      let src =
        """
        interface Keys {foo: "foo"}
        declare let keys: Keys;
        """

      let! ctx, env = inferModule src
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
        Infer.inferModule ctx env ast |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Keys", "{foo: \"foo\", bar: \"bar\"}")

      Assert.Type(env, "Obj", "{foo: number, bar: number}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)
