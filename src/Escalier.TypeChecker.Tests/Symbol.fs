module Symbol

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler.Compiler
open Escalier.Parser
open Escalier.TypeChecker.Env
open Escalier.TypeChecker

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let binding = env.FindValue name
    Assert.Equal(expected, binding.Type.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = env.FindScheme name
    Assert.Equal(expected, scheme.ToString())

let inferModule src =
  result {
    let projectRoot = __SOURCE_DIRECTORY__

    let! ctx, env =
      TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    let! env =
      InferModule.inferModule ctx env ast
      |> Async.RunSynchronously
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "iterator", "unique symbol")
    }

  printfn $"res = %A{res}"
  Assert.False(Result.isError res)

[<Fact>]
let InfersSymbolsAreUnique () =
  let res =
    result {
      let src =
        """
        let iterator: Symbol.match = Symbol.iterator;
        """

      let! _, _ = inferModule src

      ()
    }

  // NOTE: This test intentionally errors
  Assert.True(Result.isError res)
