module AsyncAwait

open FsToolkit.ErrorHandling
open Xunit

open TestUtils

[<Fact>]
let InfersAsyncFunc () =
  let res =
    result {
      let src =
        """
        let foo = async fn () {
          return 5;
        };
        let bar = async fn () {
          let x = await foo();
          return x + await 10;
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn () -> Promise<5, never>")
      Assert.Value(env, "bar", "fn () -> Promise<number, never>")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersAsyncError () =
  let res =
    result {
      let src =
        """
        let foo = async fn<A: number>(x: A) => if x < 0 { throw "RangeError" } else { x };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

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
        let foo = async fn(x: number) => if x < 0 { throw "RangeError" } else { x };
        let bar = async fn(x: number) {
          let y = await foo(x);
          return y + await 10;
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

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
        let foo = async fn (x: number) => if x < 0 { throw "RangeError" } else { x };
        let bar = async fn (x: number) =>
          try {
            let y = await foo(x);
            y + await 10;
          } catch {
            "RangeError" => 0,
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn (x: number) -> Promise<number, \"RangeError\">"
      )

      Assert.Value(env, "bar", "fn (x: number) -> Promise<number, never>")
    }

  Assert.False(Result.isError res)
