module PatternMatching

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
let BasicPatternMatching () =
  let result =
    result {
      let src =
        """
        let foo = fn (x: number) =>
          match x {
            | 0 => "none"
            | 1 => "one"
            | n if n < 0 => "negative"
            | _ => "other"
          }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn (x: number) -> \"none\" | \"one\" | \"negative\" | \"other\""
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let BasicPatternMatchingInferExpr () =
  let result =
    result {
      let src =
        """
        let foo = fn (x) =>
          match x {
            | 0 => "none"
            | 1 => "one"
            | n if n < 0 => "negative"
            | _ => "other"
          }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <A, B: number>(x: A | B | 1 | 0) -> \"none\" | \"one\" | \"negative\" | \"other\""
      )
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let PatternMatchingObjects () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number}
        type Shape = {
          type: "circle",
          radius: number,
          center: Point
        } | {
          type: "line",
          start: Point,
          end: Point
        }
        
        declare let shape: Shape
        
        let centroid =
          match shape {
            | {type: "circle", radius, center} => center
            | {type: "line", start, end} => ({
              x: (start.x + end.x) / 2,
              y: (start.y + end.y) / 2
            })
          }
        """

      let! _, env = inferScript src

      // TODO: Simplify all binary type in a type
      // {x: number / 2, y: number / 2} -> {x: number, y: number}
      // TODO: figure out how to have a type alias subsume a type that's the
      // same as its definition
      Assert.Value(env, "centroid", "Point | {x: number / 2, y: number / 2}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)
