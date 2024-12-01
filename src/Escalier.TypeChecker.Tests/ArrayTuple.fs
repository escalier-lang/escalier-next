module ArrayTuple

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker.Env
open Escalier.TypeChecker

open TestUtils

let inferModule src =
  result {
    let projectRoot = __SOURCE_DIRECTORY__
    let! ctx, env = Prelude.getEnvAndCtx projectRoot |> Async.RunSynchronously

    let prelude =
      """
        type RangeIterator<Min: number, Max: number> = {
          next: fn () -> { done: boolean, value: Min..Max }
        };
      """

    let! ast =
      Parser.parseModule prelude |> Result.mapError CompileError.ParseError

    let env = { env with Filename = "/prelude.esc" }

    let! env =
      InferModule.inferModule ctx env ast
      |> Async.RunSynchronously
      |> Result.mapError CompileError.TypeError

    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    let env = { env with Filename = "input.esc" }

    let! env =
      InferModule.inferModule ctx env ast
      |> Async.RunSynchronously
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

[<Fact>]
let InferTupleLength () =
  let res =
    result {
      let src =
        """
        let tuple = [5, "hello", true];
        let length = tuple.length;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "length", "3")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferArrayLength () =
  let res =
    result {
      let src =
        """
        let array: number[] = [1, 2, 3];
        let length = array.length;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "length", "unique number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferTupleIndexing () =
  let res =
    result {
      let src =
        """
        let tuple = [5, "hello", true];
        let first = tuple[0];
        let second = tuple[1];
        let third = tuple[2];
        let fourth = tuple[3];
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "first", "5")
      Assert.Value(env, "second", "\"hello\"")
      Assert.Value(env, "third", "true")
      Assert.Value(env, "fourth", "undefined")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferArrayIndexing () =
  let res =
    result {
      let src =
        """
        let array: number[] = [1, 2, 3];
        let first = array[0];
        let second = array[1];
        let third = array[2];
        let fourth = array[3];
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "first", "number | undefined")
      Assert.Value(env, "fourth", "number | undefined")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferForIn () =
  let res =
    result {
      let src =
        """
        for x in [1, 2, 3] {
          let y: number = x;
        }
        let array: number[] = [1, 2, 3];
        for x in array {
          let y: number = x;
        }
        """

      let! _ = inferModule src

      ()
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferForInRange () =
  let res =
    result {
      let src =
        """
        for x in 0..3 {
          let a: number = x;
          let b: 0..10 = x;
          let c: 0..3 = x;
        }
        """

      let! _ = inferModule src

      ()
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferRangeTypeAssignment () =
  let res =
    result {
      let src =
        """
        let a: 0..3 = 0;
        let b: 0..3 = 1;
        let c: 0..3 = 2;
        """

      let! _ = inferModule src

      ()
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferOutOfBoundsRangeLiteralAssignment () =
  let res =
    result {
      let src =
        """
        let out_of_bounds: 0..3 = 3;
        """

      let! _ = inferModule src

      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let InferOutOfBoundsRangeRangeAssignment () =
  let res =
    result {
      let src =
        """
        let range: 0..10 = 0;
        let bounds_mismatch: 0..3 = range;
        """

      let! _ = inferModule src

      ()
    }

  Assert.True(Result.isError res)

[<Fact(Skip = "lower (neg (num x))")>]
let InferRangeWithNegativeStart () =
  let res =
    result {
      let src =
        """
        declare let range: -1..1;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "range", "-1..1")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferRangeMath () =
  let res =
    result {
      let src =
        """
        declare let range: 0..3;
        let inc_range = range + 1;
        let dec_range = range - 1;
        let mul_range = 2 * range;
        let range_plus_range = range + range;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "inc_range", "1..4")
      Assert.Value(env, "dec_range", "-1..2")
      Assert.Value(env, "mul_range", "0..6")
      Assert.Value(env, "range_plus_range", "0..6")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferTypeofLength () =
  let res =
    result {
      let src =
        """
        declare let array: number[];
        let length = array.length;
        type A = typeof length;
        type B = typeof array.length;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "length", "unique number")
      Assert.Type(env, "A", "unique number")
      Assert.Type(env, "B", "unique number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferRangeWithArrayLength () =
  let res =
    result {
      let src =
        """
        declare let array: number[];

        let length = array.length;
        for x in 0..length {
          let item: number = array[x];
        }
        
        declare let range: 0..(typeof length);
        let elem = array[range];
        
        let first = array[0];
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "length", "unique number")
      Assert.Value(env, "first", "number | undefined")
      Assert.Value(env, "elem", "number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferRangeWithDifferentArrayLengths () =
  let res =
    result {
      let src =
        """
        declare let array1: number[];
        declare let array2: string[];
        
        let length1 = array1.length;
        let length2 = array2.length;
        
        declare let range1: 0..(typeof length1);
        declare let range2: 0..(typeof length2);
        
        let elem1 = array1[range1];
        let elem2 = array2[range2];
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "elem1", "number")
      Assert.Value(env, "elem2", "string")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferRangeWithDifferentArrayLengthsAreIncompatible () =
  let res =
    result {
      let src =
        """
        declare let array1: number[];
        declare let array2: string[];
        
        let length1 = array1.length;
        let length2 = array2.length;
        
        declare let range1: 0..(typeof length1);
        declare let range2: 0..(typeof length2);
        
        let elem1 = array1[range2];
        let elem2 = array2[range1];
        """

      let! _ = inferModule src
      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let InferDestructureArray () =
  let res =
    result {
      let src =
        """
        declare let array: number[];
        declare let tuple: [number, string, boolean];
        
        let [a, ...rest] = array;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "number | undefined")
      Assert.Value(env, "rest", "number[]")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferDestructureTuple () =
  let res =
    result {
      let src =
        """
        declare let tuple: [number, string, boolean];
        
        let [a, ...rest] = tuple;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "rest", "[string, boolean]")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferBasicImmutableTypes () =
  let res =
    result {
      let src =
        """
        let tuple = #[5, "hello", true];
        let record = #{ a: 5, b: "hello", c: true };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "tuple", "#[5, \"hello\", true]")
      Assert.Value(env, "record", "#{a: 5, b: \"hello\", c: true}")
    }

  Assert.False(Result.isError res)


[<Fact>]
let DestructuringImmutableTypes () =
  let res =
    result {
      let src =
        """
        let [a, b] = #[5, "hello"];
        let #[c, d] = #[5, "hello"];
        let {e, f} = #{e: 5, f: "hello"};
        let #{g, h} = #{g: 5, h: "hello"};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "5")
      Assert.Value(env, "b", "\"hello\"")
      Assert.Value(env, "c", "5")
      Assert.Value(env, "d", "\"hello\"")
      Assert.Value(env, "e", "5")
      Assert.Value(env, "f", "\"hello\"")
      Assert.Value(env, "g", "5")
      Assert.Value(env, "h", "\"hello\"")
    }

  Assert.False(Result.isError res)

[<Fact>]
let PartialDestructuring () =
  let res =
    result {
      let src =
        """
        let [a] = [5, "hello"];
        let {b} = {a: 5, b: "hello"};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "5")
      Assert.Value(env, "b", "\"hello\"")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let ImmutableTuplesAreIncompatibleWithRegularTuples () =
  let res =
    result {
      let src =
        """
        declare let foo: fn (point: #[number, number]) -> undefined;
        foo([5, 10]);
        """

      let! ctx, _ = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)
    }

  Assert.False(Result.isError res)

[<Fact>]
let ImmutableObjectsAreIncompatibleWithRegularObjects () =
  let res =
    result {
      let src =
        """
        declare let foo: fn (point: #{x:number, x:number}) -> undefined;
        foo({x:5, y:10});
        """

      let! ctx, _ = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)
    }

  Assert.False(Result.isError res)

[<Fact>]
let DescructuringArray () =
  let res =
    result {
      let src =
        """
        declare let array: number[];
        let [a, b] = array;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "number | undefined")
    }

  Assert.False(Result.isError res)

// TODO: test .map(), .forEach(), .filter(), .reduce(), etc.
// In particular we want to check that the type of the callback is inferred correctly
// and that the return type of the function is inferred correctly.
