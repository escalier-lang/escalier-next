module Symbol

open FsToolkit.ErrorHandling
open System.IO.Abstractions
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker.Env
open Escalier.TypeChecker

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())

type CompileError = Prelude.CompileError

let inferScript src =
  result {
    let fs = FileSystem()
    let projectRoot = __SOURCE_DIRECTORY__
    let! ctx, env = Prelude.getEnvAndCtxWithES5 fs projectRoot

    let prelude =
      """
        type RangeIterator<Min: number, Max: number> = {
          next: fn () -> { done: boolean, value: Min..Max }
        };
      """

    let! ast =
      Parser.parseScript prelude |> Result.mapError CompileError.ParseError

    let! env =
      Infer.inferScript ctx env "/prelude.esc" ast
      |> Result.mapError CompileError.TypeError

    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let! env =
      Infer.inferScript ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

[<Fact>]
let InfersTypeofWellknownSymbol () =
  let res =
    result {
      let src =
        """
        let iterator: typeof Symbol.iterator = Symbol.iterator;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "iterator", "symbol()")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersSymbolsAreUnique () =
  let res =
    result {
      let src =
        """
        let iterator: Symbol.match = Symbol.iterator;
        """

      let! _, _ = inferScript src

      ()
    }

  Assert.True(Result.isError res)
