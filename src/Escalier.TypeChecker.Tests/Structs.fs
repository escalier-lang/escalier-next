module Structs

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
let InferBasicStruct () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        """

      let! _, env = inferScript src

      Assert.Value(env, "point", "Point")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferBasicStructIncorrectTypes () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: "hello", y: true}
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let InferGenericStruct () =
  let res =
    result {
      let src =
        """
        struct Point<T> {x: T, y: T}
        let point = Point<number> {x: 5, y: 10}
        """

      let! _, env = inferScript src

      Assert.Value(env, "point", "Point<number>")
    }

  Assert.False(Result.isError res)

[<Fact>]
let StructsAreSubtypesOfObjects () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point: {x: number, y: number} = Point {x: 5, y: 10}
        """

      let! _, env = inferScript src

      Assert.Value(env, "point", "{x: number, y: number}")
    }

  Assert.False(Result.isError res)

[<Fact>]
let PropertyAccessOnStructs () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        let x = point.x
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let PropertyAccessOnPrivateStructs () =
  let res =
    result {
      let src =
        """
        let makePoint = fn (x, y) {
          struct Point {x: number, y: number}
          return Point {x, y}
        }
        let point = makePoint(5, 10)
        let x = point.x
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact(Skip = "update getPropType to return an Result")>]
let PropertyAccessOnPrivateStructsWrongKey () =
  let res =
    result {
      let src =
        """
        let makePoint = fn (x, y) {
          struct Point {x: number, y: number}
          return Point {x, y}
        }
        let point = makePoint(5, 10)
        let z = point.z
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let ObjectDestructuringOfStructs () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        let {x, y} = point
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let StructDestructuring () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        let Point {x, y} = point
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let StructPartialDestructuring () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        let Point {x} = point
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError res)
