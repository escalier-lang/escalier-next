module Exceptions

open FsToolkit.ErrorHandling
open Xunit

open TestUtils

[<Fact>]
let InfersExplicitThrow () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) {
          if x < 0 {
            throw "RangeError";
          }
          return x;
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersThrowExpression () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersJustThrowExpression () =
  let res =
    result {
      let src =
        """
        let foo = fn <T: string>(exc: T) => throw exc;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T: string>(exc: T) -> never throws T")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersThrowingMultipleExpressions () =
  let res =
    result {
      let src =
        """
        let foo = fn <A: number>(x: A) =>
          if x < 0 { throw "RangeError" } else { throw "BoundsError" };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> never throws (\"BoundsError\" | \"RangeError\")"
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersThrowsFromCall () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x };
          
        let bar = fn (x) => foo(x);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )

      Assert.Value(
        env,
        "bar",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferCatchesException () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x };
          
        let bar = fn (x) =>
          try {
            foo(x);
          } catch {
            "RangeError" => 0,
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "bar", "fn <A: number>(x: A) -> A | 0")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferCatchesMultipleExceptions () =
  let res =
    result {
      let src =
        """
        let foo = fn <A: number>(x: A) =>
          if x < 0 { throw "RangeError" } else { throw "BoundsError" };
          
        let bar = fn (x) =>
          try {
            foo(x);
          } catch {
            "RangeError" => 0,
            "BoundsError" => 0,
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "bar", "fn <A: number>(x: A) -> 0")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferCatchesOneOfManyExceptions () =
  let res =
    result {
      let src =
        """
        let foo = fn <A: number>(x: A) =>
          if x < 0 { throw "RangeError" } else { throw "BoundsError" };
          
        let bar = fn (x) =>
          try {
            foo(x);
          } catch {
            "RangeError" => 0,
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "bar",
        "fn <A: number>(x: A) -> 0 throws \"BoundsError\""
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferTryFinally () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x };
        let cleanup = fn () => {};

        let bar = fn (x) =>
          try {
            foo(x)
          } finally {
            cleanup()
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "bar",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferTryCatchFinally () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x };
        let cleanup = fn () => {};

        let bar = fn (x) =>
          try {
            foo(x);
          } catch {
            "RangeError" => 0,
          } finally {
            cleanup();
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "bar", "fn <A: number>(x: A) -> A | 0")
    }

  Assert.False(Result.isError res)
