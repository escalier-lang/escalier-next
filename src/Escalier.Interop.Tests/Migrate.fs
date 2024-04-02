[<VerifyXunit.UsesVerify>]
module Migrate

open FParsec
open FsToolkit.ErrorHandling
open FParsec.CharParsers
open System.IO
open VerifyTests
open VerifyXunit
open Xunit

open Escalier.Compiler
// open Escalier.Parser
open Escalier.Interop.Parser
open Escalier.Interop.Migrate
open Escalier.TypeChecker
// open Escalier.TypeChecker.Infer
open Escalier.TypeChecker.Env

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
let ParseAndInferBasicDecls () =
  let res =
    result {

      let input =
        """
        declare var a: number;
        declare var b: string;
        declare var c: boolean;
        declare var d: (x: number[]) => boolean;
        declare var e: { a: number, b: string };
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        Infer.inferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "a", "number")
      Assert.Value(env, "b", "string")
      Assert.Value(env, "c", "boolean")
      Assert.Value(env, "d", "fn (mut x: number[]) -> boolean")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferTypeAliases () =
  let res =
    result {

      let input =
        """
        type Foo = string;
        type Bar<T> = { value: T, next?: Bar<T> };
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        Infer.inferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Foo", "string")
      Assert.Type(env, "Bar", "<T>({value: T, next?: Bar<T>})")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferInterface () =
  let res =
    result {

      let input =
        """
        interface Foo {
          bar(): number;
          baz(x: string): boolean;
          get qux(): string;
          set qux(x: string);
        }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        Infer.inferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(
        env,
        "Foo",
        "{bar fn (self: Self) -> number, baz fn (self: Self, mut x: string) -> boolean, get qux fn () -> string, set qux fn () -> undefined}"
      )
    }

  printfn "%A" res
  Assert.True(Result.isOk res)
