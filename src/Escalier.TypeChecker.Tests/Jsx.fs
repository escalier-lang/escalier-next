module Escalier.TypeChecker.Tests.Jsx


open FsToolkit.ErrorHandling
open Xunit

open TestUtils

[<Fact>]
let InferJsx () =
  let res =
    result {
      let src =
        """
        import "react" {React};
        let foo = <div
          id="foo"
          title="bar"
        ></div>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "React.JSX.Element")
    }

  Assert.False(Result.isError res)

[<Fact(Skip = "TODO")>]
let InferJsxWithCallback () =
  let res =
    result {
      let src =
        """
        import "react" {React};
        let foo = <div
          onClick={fn (event) {
            let x = event.clientX;
            let y = event.clientY;
            console.log(`x: ${x}, y: ${y}`);
          }}
        ></div>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "React.JSX.Element")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)


[<Fact>]
let InferJsxDetectsIncorrectProps () =
  let res =
    result {
      let src =
        """
        import "react" {React};
        let foo = <div
          id={5}
          title="bar"
        ></div>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "React.JSX.Element")
    }

  Assert.True(Result.isError res)

[<Fact>]
let InferPropsTypeObject () =
  let res =
    result {
      let src =
        """
        type Props = {
          id: string,
          onClick: fn (event: MouseEvent) -> undefined,
        };
        let props: Props = {
          id: "foo",
          onClick: fn (event) {
            let x = event.clientX;
            let y = event.clientY;
            let slope = y / x;
          },
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)
