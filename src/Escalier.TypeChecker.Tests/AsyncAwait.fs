module AsyncAwait

open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Infer

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
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/" "/input.esc"

    let! env =
      inferScript ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

[<Fact>]
let InfersAsyncFunc () =
  let res =
    result {
      let src =
        """
        let foo = async fn () {
          return 5
        }
        let bar = async fn () {
          let x = await foo()
          return x + await 10
        }
        """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn () -> Promise<5, never>")
      Assert.Value(env, "bar", "fn () -> Promise<15, never>")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersAsyncError () =
  let res =
    result {
      let src =
        """
        let foo = async fn (x) => if x < 0 { throw "RangeError" } else { x }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> Promise<A, \"RangeError\">"
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersPropagateAsyncError () =
  let res =
    result {
      let src =
        """
        let foo = async fn (x) => if x < 0 { throw "RangeError" } else { x }
        let bar = async fn (x) {
          let y = await foo(x)
          return y + await 10
        }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> Promise<A, \"RangeError\">"
      )

      Assert.Value(
        env,
        "bar",
        "fn (x: number) -> Promise<number, \"RangeError\">"
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersTryCatchAsync () =
  let res =
    result {
      let src =
        """
        let foo = async fn (x) => if x < 0 { throw "RangeError" } else { x }
        let bar = async fn (x) =>
          try {
            let y = await foo(x)
            y + await 10
          } catch {
            | "RangeError" => 0
          }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> Promise<A, \"RangeError\">"
      )

      Assert.Value(env, "bar", "fn (x: number) -> Promise<number, never>")
    }

  printfn "res: %A" res
  Assert.False(Result.isError res)
