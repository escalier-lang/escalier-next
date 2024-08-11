module Escalier.TypeChecker.Tests.Exact

open FsToolkit.ErrorHandling
open Xunit

open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Unify

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

[<Fact>]
let InexactMappedTypesPreserveExactness () =
  let res =
    result {
      let src =
        """
        type MyPartial<T> = {[K]?: T[K] for K in keyof T, ...};
        type Exact = {msg: string, flag: boolean};
        type Inexact = {msg: string, flag: boolean, ...};
        
        type PartialExact = MyPartial<Exact>;
        type PartialInexact = MyPartial<Inexact>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! t =
        expandScheme ctx env None (env.FindScheme "PartialExact") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "{flag?: boolean, msg?: string}")

      let! t =
        expandScheme
          ctx
          env
          None
          (env.FindScheme "PartialInexact")
          Map.empty
          None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "{flag?: boolean, msg?: string, ...}")
    }

  Assert.False(Result.isError res)

[<Fact>]
let ExactMappedTypesPreserveExactness () =
  let res =
    result {
      let src =
        """
        type MyPartial<T> = {[K]?: T[K] for K in keyof T};
        type Exact = {msg: string, flag: boolean};
        type Inexact = {msg: string, flag: boolean, ...};
        
        type PartialExact = MyPartial<Exact>;
        type PartialInexact = MyPartial<Inexact>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! t =
        expandScheme ctx env None (env.FindScheme "PartialExact") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "{flag?: boolean, msg?: string}")

      let! t =
        expandScheme
          ctx
          env
          None
          (env.FindScheme "PartialInexact")
          Map.empty
          None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "{flag?: boolean, msg?: string, ...}")
    }

  Assert.False(Result.isError res)

[<Fact>]
let IntersectionOfExactTypesIsNever () =
  let res =
    result {
      let src =
        """
        type Foo = {foo: string};
        type Bar = {bar: number, ...};
        
        type FooAndBar = Foo & Bar;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! t =
        expandScheme ctx env None (env.FindScheme "FooAndBar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "never")
    }

  Assert.False(Result.isError res)
