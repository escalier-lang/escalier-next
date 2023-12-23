module Exceptions

open FParsec
open FsToolkit.ErrorHandling
open Xunit

open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Infer

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())

type CompileError =
  | ParseError of ParserError
  | TypeError of TypeError


let inferScript src =
  result {
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let env = Prelude.getEnv ()

    let ctx =
      Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

    let! env =
      inferScript ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

[<Fact>]
let InfersExplicitThrow () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) {
          if (x < 0) {
            throw "RangeError"
          }
          return x
        }
        """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn (x: number) -> number throws \"RangeError\"")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)
