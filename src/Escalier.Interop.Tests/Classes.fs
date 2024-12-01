module Escalier.Interop.Tests.Classes

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
    let binding = env.FindValue name
    Assert.Equal(expected, binding.Type.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = env.FindScheme name
    Assert.Equal(expected, scheme.ToString())

type CompileError = Prelude.CompileError

let projectRoot = __SOURCE_DIRECTORY__

[<Fact>]
let InferClassExtendsClassExtendsClass () =
  let res =
    result {

      let input =
        """
        declare class Foo { foo: number }
        declare class Bar extends Foo { bar: string }
        declare class Baz extends Bar { baz: boolean }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        InferModule.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      let input =
        """
        declare let x: Baz;
        let foo = x.foo;
        let bar = x.bar;
        let baz = x.baz;
        """

      let! ast =
        Parser.parseModule input |> Result.mapError CompileError.ParseError

      let! env =
        InferModule.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "Baz")
      Assert.Value(env, "foo", "number")
      Assert.Value(env, "bar", "string")
      Assert.Value(env, "baz", "boolean")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferClassExtendsClassExtendsClassWithDestructuring () =
  let res =
    result {

      let input =
        """
        declare class Foo { foo: number }
        declare class Bar extends Foo { bar: string }
        declare class Baz extends Bar { baz: boolean }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        InferModule.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      let input =
        """
        declare let x: Baz;
        let {foo, bar, baz} = x;
        """

      let! ast =
        Parser.parseModule input |> Result.mapError CompileError.ParseError

      let! env =
        InferModule.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "Baz")
      Assert.Value(env, "foo", "number")
      Assert.Value(env, "bar", "string")
      Assert.Value(env, "baz", "boolean")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferClassExtendsClassExtendsClassWithGenerics () =
  let res =
    result {

      let input =
        """
        declare class Foo<A> { foo: A }
        declare class Bar<B> extends Foo<B> { bar: B }
        declare class Baz<C> extends Bar<C> { baz: C }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        InferModule.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      let input =
        """
        declare let x: Baz<number>;
        let foo = x.foo;
        let bar = x.bar;
        let baz = x.baz;
        """

      let! ast =
        Parser.parseModule input |> Result.mapError CompileError.ParseError

      let! env =
        InferModule.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "Baz<number>")
      Assert.Value(env, "foo", "number")
      Assert.Value(env, "bar", "number")
      Assert.Value(env, "baz", "number")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)
