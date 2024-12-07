module UtilityTypes

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler.Compiler
open Escalier.TypeChecker.Unify

open TestUtils

[<Fact>]
let InferCollaspingNever () =
  let res =
    result {
      let src =
        """
        type Foo = string | never;
        type Bar = string | number | never;
        type Baz = string | number | never | never;
        type Qux = never | never;
        """

      let! _, env = inferModule src

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
        };
        type A = Foo<string>;
        type B = Foo<number>;
        type C = Foo<boolean>;
        """

      let! ctx, env = inferModule src

      let! a =
        expandScheme ctx env None (env.FindScheme "A") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("\"string\"", a.ToString())

      let! b =
        expandScheme ctx env None (env.FindScheme "B") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("\"number\"", b.ToString())

      let! c =
        expandScheme ctx env None (env.FindScheme "C") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("\"other\"", c.ToString())
    }

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
        };
        type A = Foo<string>;
        type B = Foo<number>;
        type C = Foo<boolean>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! a =
        expandScheme ctx env None (env.FindScheme "A") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("\"string\"", a.ToString())

      let! b =
        expandScheme ctx env None (env.FindScheme "B") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("\"number\"", b.ToString())

      let! c =
        expandScheme ctx env None (env.FindScheme "C") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("\"other\"", c.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferExclude () =
  let res =
    result {
      let src =
        """
        type Exclude<T, U> = if T: U { never } else { T };
        type Result = Exclude<"a" | "b" | "c" | "d" | "e", "a" | "e">;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Result") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("\"b\" | \"c\" | \"d\"", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferExtract () =
  let res =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        type Extract<T, U> = if T: Point { T } else { never };
        type Result = Extract<{x: 5, y: 10} | number | string, Point>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Result") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{x: 5, y: 10}", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferCartesianProdType () =
  let res =
    result {
      let src =
        """
        type CartesianProduct<X, Y> =
          if X : unknown {
            if Y : unknown {
              [X, Y]
            } else {
              never
            }
          } else {
            never
          };
        type Cells = CartesianProduct<"A" | "B", 1 | 2>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Cells") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        """["A", 1] | ["A", 2] | ["B", 1] | ["B", 2]""",
        result.ToString()
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersPickUnionOfKeyInScript () =
  let res =
    result {
      let src =
        """
        type Pick<T, K: keyof T> = {
          [P]: T[P] for P in K
        };

        type Foo = {a: number, b: string, c: boolean};
        type Bar = Pick<Foo, "a" | "c">;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Bar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: number, c: boolean}", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersPickUnionOfKeyInModule () =
  let res =
    result {
      let src =
        """
        type MyPick<T, K: keyof T> = {[P]: T[P] for P in K};

        type Foo = {a: number, b: string, c: boolean};
        type Bar = MyPick<Foo, "a" | "c">;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Bar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: number, c: boolean}", result.ToString())
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InfersPickSingleKey () =
  let res =
    result {
      let src =
        """
        type Pick<T, K: keyof T> = {
          [P]: T[P] for P in K
        };

        type Foo = {a: number, b: string, c: boolean};
        type Bar = Pick<Foo, "a" | "c">;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Bar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: number, c: boolean}", result.ToString())
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InfersPickWrongKeyType () =
  let res =
    result {
      let src =
        """
        type Pick<T, K: keyof T> = {
          [P]: T[P] for P in K
        };

        type Foo = {a: number, b: string, c: boolean};
        type Bar = Pick<Foo, 5 | 10>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Bar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: number, c: boolean}", result.ToString())
    }

  // TypeMismatch
  Assert.True(Result.isError res)

[<Fact>]
let InfersOmit () =
  let res =
    result {
      let src =
        """
        type Pick<T, K: keyof T> = {
          [P]: T[P] for P in K
        };
        type Exclude<T, U> = if T : U { never } else { T };
        type AnyKey = string | number | symbol;
        type Omit<T, K: AnyKey> = Pick<T, Exclude<keyof T, K>>;

        type Foo = {a: number, b: string, c: boolean};
        type Bar = Omit<Foo, "b">;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Bar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: number, c: boolean}", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersRecord () =
  let res =
    result {
      let src =
        """
        type AnyKey = string | number | symbol;
        type Record<K: AnyKey, T> = {
          [P]: T for P in K
        };
        type Point = Record<"x" | "y", number>;
        let p: Point = {x: 5, y: 10};
        """

      let! _, env = inferModule src

      Assert.Value(env, "p", "Point")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersNestedConditionals () =
  let res =
    result {
      let src =
        """
        type Extends<X, Y, Z> = if X : Y { X } else { if Y : Z { Y } else { never } };
        type Foo = Extends<5 | 10, 2 | 3 | 5 | 7, 3 | 6 | 9>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Foo") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("5 | 3", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersStdLibReturnType () =
  let res =
    result {
      let src =
        """
        type Foo = ReturnType<fn () -> number>;
        type Bar = ReturnType<fn (a: string, b: boolean) -> number>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let scheme = (env.FindScheme "Foo")

      let! result =
        expandScheme ctx env None scheme Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("number", result.ToString())

      let scheme = (env.FindScheme "Bar")

      let! result =
        expandScheme ctx env None scheme Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("number", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersRedefinedReturnType () =
  let res =
    result {
      let src =
        """
        type MyReturnType<T> = if T: fn (...args: _) -> infer R { R } else { never };
        type Foo = MyReturnType<fn () -> number>;
        type Bar = MyReturnType<fn (a: string, b: boolean) -> number>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let scheme = (env.FindScheme "Foo")

      let! result =
        expandScheme ctx env None scheme Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("number", result.ToString())

      let scheme = (env.FindScheme "Bar")

      let! result =
        expandScheme ctx env None scheme Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("number", result.ToString())
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersParameters () =
  let res =
    result {
      let src =
        """
        type Parameters<T> = if T: fn (...args: infer P) -> _ { P } else { never };
        type Foo = Parameters<fn () -> number>;
        type Bar = Parameters<fn (a: string, b: boolean) -> number>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! result =
        expandScheme ctx env None (env.FindScheme "Foo") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("[]", result.ToString())

      let! result =
        expandScheme ctx env None (env.FindScheme "Bar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("[string, boolean]", result.ToString())
    }

  Assert.False(Result.isError res)
