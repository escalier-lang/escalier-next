module Escalier.TypeChecker.Tests.Exact

open Escalier.TypeChecker.Error
open FsToolkit.ErrorHandling
open Xunit

open TestUtils

[<Fact>]
let ExactIsAssignableToInexact () =
  let res =
    result {
      let src =
        """
        let p1: {x: number, y: number} = {x: 5, y: 10};
        let p2: {x: number, y: number, ...} = p1;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.False(Result.isError res)

[<Fact>]
let InexactIsNotAssignableToExact () =
  let res =
    result {
      let src =
        """
        let p1: {x: number, y: number, ...} = {x: 5, y: 10};
        let p2: {x: number, y: number} = p1;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.True(Result.isError res)

[<Fact>]
let ExactIsAssignableExact () =
  let res =
    result {
      let src =
        """
        let p1: {x: number, y: number} = {x: 5, y: 10};
        let p2: {x: number, y: number} = p1;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.False(Result.isError res)

[<Fact>]
let ExactIsAssignableExactSubtyping () =
  let res =
    result {
      let src =
        """
        let p1 = {x: 5, y: 10};
        let p2: {x: number, y: number} = p1;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.False(Result.isError res)

[<Fact>]
let ExactWithExtraPropertiesIsNotAssignableExact () =
  let res =
    result {
      let src =
        """
        let p1: {x: number, y: number, z: number} = {x: 5, y: 10, z: 15};
        let p2: {x: number, y: number} = p1;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.True(Result.isError res)

[<Fact>]
let ExactWithExtraPropertiesIsAssignableToInexact () =
  let res =
    result {
      let src =
        """
        let p1: {x: number, y: number, z: number} = {x: 5, y: 10, z: 15};
        let p2: {x: number, y: number, ...} = p1;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.False(Result.isError res)

[<Fact>]
let SpreadingTwoExactObjectsIsExact () =
  let res =
    result {
      let src =
        """
        declare let foo: {msg: string};
        declare let bar: {flag: boolean};
        let foobar = {...foo, ...bar};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foobar", "{msg: string, flag: boolean}")
    }

  Assert.False(Result.isError res)

[<Fact>]
let SpreadingAnExactObjectAndInexactObjectIsInexact () =
  let res =
    result {
      let src =
        """
        declare let foo: {msg: string};
        declare let bar: {flag: boolean, ...};
        let foobar = {...foo, ...bar};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foobar", "{msg: string, flag: boolean, ...}")
    }

  Assert.False(Result.isError res)
