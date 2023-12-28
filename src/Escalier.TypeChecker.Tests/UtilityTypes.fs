module UtilityTypes

open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Infer
open Escalier.TypeChecker.Unify

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
let InferCollaspingNever () =
  let res =
    result {
      let src =
        """
        type Foo = string | never
        type Bar = string | number | never
        type Baz = string | number | never | never
        type Qux = never | never
        """

      let! _, env = inferScript src

      Assert.Type(env, "Foo", "string")
      Assert.Type(env, "Bar", "string | number")
      Assert.Type(env, "Baz", "string | number")
      Assert.Type(env, "Qux", "never")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferSimpleConditionalType () =
  let res =
    result {
      let src =
        """
        type Foo<T> = if T : string {
          "string"
        } else if T : number {
          "number"
        } else {
          "other"
        }
        type A = Foo<string>
        type B = Foo<number>
        type C = Foo<boolean>
        """

      let! ctx, env = inferScript src

      let a = env.ExpandScheme (unify ctx) (Map.find "A" env.Schemes) None
      Assert.Equal("\"string\"", a.ToString())

      let b = env.ExpandScheme (unify ctx) (Map.find "B" env.Schemes) None
      Assert.Equal("\"number\"", b.ToString())

      let c = env.ExpandScheme (unify ctx) (Map.find "C" env.Schemes) None
      Assert.Equal("\"other\"", c.ToString())
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferNestedConditionalTypes () =
  let res =
    result {
      let src =
        """
        type Foo<T> = if T : string | number {
          if T : string {
            "string"
          } else {
            "number"
          }
        } else {
          "other"
        }
        type A = Foo<string>
        type B = Foo<number>
        type C = Foo<boolean>
        """

      let! ctx, env = inferScript src

      let a = env.ExpandScheme (unify ctx) (Map.find "A" env.Schemes) None
      Assert.Equal("\"string\"", a.ToString())

      let b = env.ExpandScheme (unify ctx) (Map.find "B" env.Schemes) None
      Assert.Equal("\"number\"", b.ToString())

      let c = env.ExpandScheme (unify ctx) (Map.find "C" env.Schemes) None
      Assert.Equal("\"other\"", c.ToString())
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferExclude () =
  let res =
    result {
      let src =
        """
        type Exclude<T, U> = if T: U { never } else { T }
        type Result = Exclude<"a" | "b" | "c" | "d" | "e", "a" | "e">
        """

      let! ctx, env = inferScript src

      let result =
        env.ExpandScheme (unify ctx) (Map.find "Result" env.Schemes) None

      Assert.Equal("\"b\" | \"c\" | \"d\"", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferExtract () =
  let res =
    result {
      let src =
        """
        type Point = {x: number, y: number}
        type Extract<T, U> = if T: Point { T } else { never }
        type Result = Extract<{x: 5, y: 10} | number | string, Point>
        """

      let! ctx, env = inferScript src

      let result =
        env.ExpandScheme (unify ctx) (Map.find "Result" env.Schemes) None

      Assert.Equal("{x: 5, y: 10}", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferCartesianProdType () =
  let res =
    result {
      let src =
        """
        type CartesianProduce<X, Y> =
          if X : unknown {
            if Y : unknown {
              [X, Y]
            } else {
              never
            }
          } else {
            never
          }
        type Cells = CartesianProduce<"A" | "B", 1 | 2>
        """

      let! ctx, env = inferScript src

      let result =
        env.ExpandScheme (unify ctx) (Map.find "Cells" env.Schemes) None

      Assert.Equal(
        """["A", 1] | ["A", 2] | ["B", 1] | ["B", 2]""",
        result.ToString()
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersPick () =
  let res =
    result {
      let src =
        """
        type Pick<T, K: keyof T> = {
          [P]: T[P] for P in K
        }

        type Foo = {a: number, b: string, c: boolean}
        type Bar = Pick<Foo, "a" | "c">
        """

      let! ctx, env = inferScript src

      let result =
        env.ExpandScheme (unify ctx) (Map.find "Bar" env.Schemes) None

      Assert.Equal("{a: number, c: boolean}", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersOmit () =
  let res =
    result {
      let src =
        """
        type Pick<T, K: keyof T> = {
          [P]: T[P] for P in K
        }
        type Exclude<T, U> = if T : U { never } else { T }
        type AnyKey = string | number | symbol
        type Omit<T, K: AnyKey> = Pick<T, Exclude<keyof T, K>>
        
        type Foo = {a: number, b: string, c: boolean}
        type Bar = Omit<Foo, "b">
        """

      let! ctx, env = inferScript src

      let result =
        env.ExpandScheme (unify ctx) (Map.find "Bar" env.Schemes) None

      Assert.Equal("{a: number, c: boolean}", result.ToString())
    }

  Assert.False(Result.isError res)
