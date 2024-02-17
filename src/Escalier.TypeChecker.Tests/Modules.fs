module Modules

open System.Collections.Generic
open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())

type CompileError = Prelude.CompileError


let inferModule src =
  result {
    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    let mockFileSystem = MockFileSystem()
    let prelude = Prelude.Prelude()
    let! ctx, env = prelude.getEnvAndCtx mockFileSystem "/" "/input.esc"

    let! env =
      Infer.inferModule ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

let inferModules (mockFileSystem: MockFileSystem) (src: string) =
  result {
    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    mockFileSystem.AddFile("/prelude.esc", MockFileData(""))
    let prelude = Prelude.Prelude()
    let! ctx, env = prelude.getEnvAndCtx mockFileSystem "/" "/prelude.esc"

    let! env =
      Infer.inferModule ctx env "/input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

[<Fact>]
let InferBasicModule () =
  let res =
    result {
      let src =
        """
        let add = fn(a, b) => a + b
        let sub = fn(a, b) => a - b
        """

      let! _, env = inferModule src

      Assert.Value(env, "add", "fn <A: number, B: number>(a: A, b: B) -> A + B")
      Assert.Value(env, "sub", "fn <A: number, B: number>(a: A, b: B) -> A - B")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferMutuallyRecursiveFunctions () =
  let res =
    result {
      let src =
        """
        let foo = fn() => bar() + 1
        let bar = fn() => foo() - 1
        """

      let! _, env = inferModule src

      Assert.Value(env, "foo", "fn () -> number")
      Assert.Value(env, "bar", "fn () -> number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferMutualRecursion () =
  let res =
    result {
      let src =
        """
        let even = fn (x) => if (x == 0) {
            true
        } else {
            !odd(x - 1)
        }

        let odd = fn (x) => if (x == 1) {
            true
        } else {
            !even(x - 1)
        }
        """

      let! _, env = inferModule src

      Assert.Value(env, "even", "fn (x: number) -> true | !boolean")
      Assert.Value(env, "odd", "fn (x: number) -> true | !(true | !boolean)")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferMutuallyRecursiveTypes () =
  let res =
    result {
      let src =
        """
        type Foo<T> = {bar: Bar<T>}
        type Bar<T> = {foo: Foo<T>}
        """

      let! _, env = inferModule src

      Assert.Type(env, "Foo", "<T>({bar: Bar<T>})")
      Assert.Type(env, "Bar", "<T>({foo: Foo<T>})")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferImports () =
  let res =
    result {
      let files = Dictionary<string, MockFileData>()
      let src = "import \"./foo.esc\" {foo}"
      files.Add("/input.esc", MockFileData(src))
      files.Add("/foo.esc", MockFileData("let foo = 5"))
      let mockFileSystem = MockFileSystem(files, "/")

      let! _, env = inferModules mockFileSystem src

      Assert.Value(env, "foo", "5")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)
