module PatternMatching

open FsToolkit.ErrorHandling
open Xunit

open TestUtils

[<Fact>]
let BasicPatternMatching () =
  let result =
    result {
      let src =
        """
        let foo = fn (x: number) =>
          match x {
            0 => "none",
            1 => "one",
            n if n < 0 => "negative",
            _ => "other",
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn (x: number) -> \"none\" | \"one\" | \"negative\" | \"other\""
      )
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let BasicPatternMatchingInferExpr () =
  let result =
    result {
      let src =
        """
        let foo = fn (x: number) =>
          match x {
            0 => "none",
            1 => "one",
            n if n < 0 => "negative",
            _ => "other",
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn (x: number) -> \"none\" | \"one\" | \"negative\" | \"other\""
      )
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let BasicPatternMatchingInferExprWithMultipleTypeVariables () =
  let result =
    result {
      let src =
        """
        let foo = fn (x: number, y: number) =>
          match {x, y} {
            {x: 0, y: 0} => "origin",
            {x is number, y: 0} => "x-axis",
            {x: 0, y is number} => "y-axis",
            _ => "other",
          };
          
        let bar = fn (x: number, y: number) =>
          match {x, y} {
            {x: 0, y: 0} => "origin",
            {x is number, y: 0} => "x-axis",
            {x: 0, y is number} => "y-axis",
            {x is number, y is number} => "other",
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn (x: number, y: number) -> \"origin\" | \"x-axis\" | \"y-axis\" | \"other\""
      )

      Assert.Value(
        env,
        "bar",
        "fn (x: number, y: number) -> \"origin\" | \"x-axis\" | \"y-axis\" | \"other\""
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let PatternMatchingObjects () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        type Shape = {
          type: "circle",
          radius: number,
          center: Point
        } | {
          type: "line",
          start: Point,
          end: Point
        };
        
        declare let shape: Shape;
        
        let centroid =
          match shape {
            {type: "circle", ...rest} => rest.center,
            {type: "line", start, end} => ({
              x: (start.x + end.x) / 2,
              y: (start.y + end.y) / 2
            }),
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      // TODO: Simplify all binary type in a type
      // {x: number / 2, y: number / 2} -> {x: number, y: number}
      // TODO: figure out how to have a type alias subsume a type that's the
      // same as its definition
      Assert.Value(env, "centroid", "Point | {x: number / 2, y: number / 2}")
    }

  match result with
  | Ok resultValue -> printfn $"result = {resultValue}"
  | Error errorValue -> printfn $"error = {errorValue}"

  Assert.False(Result.isError result)

[<Fact>]
let PatternMatchingObjectsWithBlockBody () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        type Shape = {
          type: "circle",
          radius: number,
          center: Point
        } | {
          type: "line",
          start: Point,
          end: Point
        };
        
        declare let shape: Shape;
        
        let centroid =
          match shape {
            {type: "circle", ...rest} => rest.center,
            {type: "line", start, end} => {
              let x = (start.x + end.x) / 2;
              let y = (start.y + end.y) / 2;
              {x, y}
            }
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      // The `number / 2` was simplified to `number` in this case because
      // it's assigned to a variable before being used in the object
      Assert.Value(env, "centroid", "Point | {x: number, y: number}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let PatternMatchingArrays () =
  let result =
    result {
      let src =
        """
        let sum = fn (array: number[]) =>
          match array {
            [] => 0,
            [x] => x,
            [x, y] => x + y,
            [x, y, z] => x + y + z,
            [x, y, z, ...rest] => x + y + z + sum(rest),
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "sum", "fn (arg0: number[]) -> number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let PatternMatchingPrimitiveAssertions () =
  let result =
    result {
      let src =
        """
        declare let value: number | string | boolean;
        
        let result =
          match value {
            n is number => n + 1,
            s is string => s ++ "!",
            _ is boolean => true,
          };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "result", "number | string | true")
    }

  Assert.False(Result.isError result)

[<Fact>]
let PartialPatternMatchingObject () =
  let res =
    result {
      let src =
        """
        declare let value: {a: number, b: string} | [number, string];
        let result = match value {
          [a, _] => a,
          {b} => b,
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "result", "number | string")
    }

  match res with
  | Ok resultValue -> printfn $"res = Ok({resultValue})"
  | Error errorValue -> printfn $"res = Error({errorValue})"

  Assert.False(Result.isError res)

[<Fact>]
let PatternMatchingImmutableTypes () =
  let res =
    result {
      let src =
        """
        declare let value: #[number, string] | #{a: number, b: string};
        let result = match value {
          #[a, b] => a,
          #{a, b} => a,
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "result", "number")
    }

  Assert.False(Result.isError res)


[<Fact>]
let PatternMatchingDisallowsExtraProperties () =
  let res =
    result {
      let src =
        """
        declare let value: {a: number, b: string};
        let result = match value {
          {a, b: _, c: _} => a,
        };
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)

      Assert.Equal(
        ctx.Report.Diagnostics[0].Description,
        "Pattern doesn't match expression in match case"
      )

      Assert.Value(env, "result", "never")
    }

  Assert.False(Result.isError res)

[<Fact>]
let PatternMatchingDisallowsPartialMappingOfTuples () =
  let res =
    result {
      let src =
        """
        declare let value: [number, string];
        let result = match value {
          [a] => a,
        };
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)

      Assert.Equal(
        ctx.Report.Diagnostics[0].Description,
        "Pattern doesn't match expression in match case"
      )

      Assert.Value(env, "result", "never")
    }

  Assert.False(Result.isError res)

[<Fact>]
let PatternMatchingNestedTypes () =
  let res =
    result {
      let src =
        """
        declare let value: {a: [number] | {x: number}} | {b: [number] | {y: number}};
        let result = match value {
          {a: [x]} => x,
          {a: {x}} => x,
          {b: [y]} => y,
          {b: {y}} => y,
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "result", "number")
    }

  match res with
  | Ok resultValue -> printfn $"res = Ok({resultValue})"
  | Error errorValue -> printfn $"res = Error({errorValue})"

  Assert.False(Result.isError res)
