module TestUtils

open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env

type CompileError = Prelude.CompileError

let inferScript src =
  result {
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/"

    let! env =
      Infer.inferScript ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

let inferModule src =
  result {
    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/"

    let! env =
      Infer.inferModule ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

let inferModules (mockFileSystem: MockFileSystem) (src: string) =
  result {
    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    mockFileSystem.AddFile("/prelude.esc", MockFileData(""))
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/"

    let! env =
      Infer.inferModule ctx env "/input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())
