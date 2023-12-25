module AsyncAwait

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
        "fn (x: number) -> Promise<number, \"RangeError\">"
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
        "fn (x: number) -> Promise<number, \"RangeError\">"
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
          } catch e {
            | "RangeError" => 0
          }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn (x: number) -> Promise<number, \"RangeError\">"
      )

      Assert.Value(env, "bar", "fn (x: number) -> Promise<number, never>")
    }

  printfn "res: %A" res
  Assert.False(Result.isError res)
