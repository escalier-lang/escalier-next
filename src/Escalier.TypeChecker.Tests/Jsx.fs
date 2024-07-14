module Escalier.TypeChecker.Tests.Jsx


open Escalier.TypeChecker.Error
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
      Assert.Value(env, "foo", "React.ReactNode")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferJsxAssignElementToReactNode () =
  let res =
    result {
      let src =
        """
        import "react" {React};
        let foo: React.ReactNode = <div
          id="foo"
          title="bar"
        ></div>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "React.ReactNode")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferJsxWithChildren () =
  let res =
    result {
      let src =
        """
        import "react" {React};
        let foo = <div>
          <p>Hello, world!</p>
        </div>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "React.ReactNode")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferJsxWithInavlidChildren () =
  let res =
    result {
      let src =
        """
        import "react" {React};
        let point = {x: 5, y: 10};
        let foo = <div>{point}</div>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "React.ReactNode")
    }

  Assert.True(Result.isError res)

[<Fact>]
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
            let slope = y / x;
          }}
        ></div>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "React.ReactNode")
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
          title={true}
        ></div>;
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 2)
      Assert.Value(env, "foo", "React.ReactNode")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferJsxWithExtraProps () =
  let res =
    result {
      let src =
        """
        import "react" {React};
        let foo = <div bar={5} baz="hello"></div>;
        """

      let! ctx, env = inferModule src

      Assert.Equal<Diagnostic list>(
        ctx.Report.Diagnostics,
        [ { Description = "No prop named 'bar' exists in div's props"
            Reasons = [] }
          { Description = "No prop named 'baz' exists in div's props"
            Reasons = [] } ]
      )

      Assert.Value(env, "foo", "React.ReactNode")
    }

  Assert.False(Result.isError res)

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

[<Fact>]
let InferHandler () =
  let res =
    result {
      let src =
        """
        import "react" {React};

        let handler: React.MouseEventHandler = fn (e) {
          let x = e.clientX;
          let y = e.clientY;
          let slope = y / x;
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.False(Result.isError res)
