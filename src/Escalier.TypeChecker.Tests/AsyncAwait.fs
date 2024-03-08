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

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
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
        let foo = async fn (x) => if x < 0 { throw "RangeError" } else { x };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

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
        let foo = async fn (x) => if x < 0 { throw "RangeError" } else { x };
        let bar = async fn (x) {
          let y = await foo(x);
          return y + await 10;
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> Promise<A, \"RangeError\">"
      )

      Assert.Value(
        env,
        "bar",
        "fn <A: number>(x: A) -> Promise<A + 10, \"RangeError\">"
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersTryCatchAsync () =
  let res =
    result {
      let src =
        """
        let foo = async fn (x) => if x < 0 { throw "RangeError" } else { x };
        let bar = async fn (x) =>
          try {
            let y = await foo(x);
            y + await 10;
          } catch {
            | "RangeError" => 0
          };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> Promise<A, \"RangeError\">"
      )

      Assert.Value(
        env,
        "bar",
        "fn <A: number>(x: A) -> Promise<A + 10 | 0, never>"
      )
    }

  Assert.False(Result.isError res)
