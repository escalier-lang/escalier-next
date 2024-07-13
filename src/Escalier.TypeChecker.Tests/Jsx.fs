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
      Assert.Value(env, "foo", "React.JSX.Element")
    }

  Assert.False(Result.isError res)

// unify(React.JSX.Element, React.ReactNode)
// expands to
// unify({
//   type: _, props: _, key: string | null, type: _, props: _, key: string | null,
//   type: _, props: _, key: string | null, type: _, props: _, key: string | null,
//   type: _, props: _, key: string | null, type: _, props: _, key: string | null,
//   type: _, props: _, key: string | null, type: _, props: _, key: string | null
// },
//    ReactElement | string | number | Iterable<ReactNode> | ReactPortal | boolean |
//    null | undefined | DO_NOT_USE_OR_YOU_WILL_BE_FIRED_EXPERIMENTAL_REACT_NODES[keyof DO_NOT_USE_OR_YOU_WILL_BE_FIRED_EXPERIMENTAL_REACT_NODES]
// )
// TODO: figure out why React.JSX.Element is repeated multiple times when it's expanded
// TODO: come up with a better way to unify type aliases
// - maybe we can create a function that returns a list of type aliases that are
//   expanded from a given type alias and then do fast checks like are the names
//   equal or does one appear in the `implements` clause of the other
[<Fact(Skip = "TODO")>]
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
      Assert.Value(env, "foo", "React.JSX.Element")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact(Skip = "TODO: infer and use defaults in type params correctly")>]
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
          title={true}
        ></div>;
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 2)
      Assert.Value(env, "foo", "React.JSX.Element")
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

      Assert.Value(env, "foo", "React.JSX.Element")
    }

  Assert.False(Result.isError res)

[<Fact(Skip = "TODO")>]
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
