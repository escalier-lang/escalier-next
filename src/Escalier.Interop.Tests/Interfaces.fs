module Escalier.Interop.Tests.Interfaces

open FParsec
open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler
open Escalier.Interop.Parser
open Escalier.Interop.Migrate
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.Parser

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = env.FindValue name
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = env.FindScheme name
    Assert.Equal(expected, scheme.ToString())

type CompileError = Prelude.CompileError

let projectRoot = __SOURCE_DIRECTORY__

[<Fact>]
let InferInterfaceExtendsInferfaceExtendsInterface () =
  let res =
    result {

      let input =
        """
        interface Foo { foo: number }
        interface Bar extends Foo { bar: string }
        interface Baz extends Bar { baz: boolean }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        InferGraph.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      let input =
        """
        let x: Baz = { foo: 5, bar: "hello", baz: true };
        let foo = x.foo;
        let bar = x.bar;
        let baz = x.baz;
        """

      let! ast =
        Parser.parseModule input |> Result.mapError CompileError.ParseError

      let! env =
        InferGraph.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "Baz")
      Assert.Value(env, "foo", "number")
      Assert.Value(env, "bar", "string")
      Assert.Value(env, "baz", "boolean")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferInterfaceExtendsInferfaceExtendsInterfaceWithDestructuring () =
  let res =
    result {

      let input =
        """
        interface Foo { foo: number }
        interface Bar extends Foo { bar: string }
        interface Baz extends Bar { baz: boolean }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        InferGraph.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      let input =
        """
        let x: Baz = { foo: 5, bar: "hello", baz: true };
        let {foo, bar, baz} = x;
        """

      let! ast =
        Parser.parseModule input |> Result.mapError CompileError.ParseError

      let! env =
        InferGraph.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "Baz")
      Assert.Value(env, "foo", "number")
      Assert.Value(env, "bar", "string")
      Assert.Value(env, "baz", "boolean")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferInterfaceExtendsMultipleInterfaces () =
  let res =
    result {

      let input =
        """
        interface Foo { foo: number }
        interface Bar { bar: string }
        interface Baz extends Foo, Bar { baz: boolean }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        InferGraph.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      let input =
        """
        let x: Baz = { foo: 5, bar: "hello", baz: true };
        let foo = x.foo;
        let bar = x.bar;
        let baz = x.baz;
        """

      let! ast =
        Parser.parseModule input |> Result.mapError CompileError.ParseError

      let! env =
        InferGraph.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "Baz")
      Assert.Value(env, "foo", "number")
      Assert.Value(env, "bar", "string")
      Assert.Value(env, "baz", "boolean")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferInterfaceExtendsMultipleInterfacesWithDestructuring () =
  let res =
    result {

      let input =
        """
        interface Foo { foo: number }
        interface Bar { bar: string }
        interface Baz extends Foo, Bar { baz: boolean }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        InferGraph.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      let input =
        """
        let x: Baz = { foo: 5, bar: "hello", baz: true };
        let {foo, bar, baz} = x;
        """

      let! ast =
        Parser.parseModule input |> Result.mapError CompileError.ParseError

      let! env =
        InferGraph.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "Baz")
      Assert.Value(env, "foo", "number")
      Assert.Value(env, "bar", "string")
      Assert.Value(env, "baz", "boolean")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)
