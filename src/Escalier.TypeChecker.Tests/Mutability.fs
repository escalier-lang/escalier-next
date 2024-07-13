module Mutability

open FParsec
open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker.Prune
open Escalier.TypeChecker.Infer

open TestUtils

let infer src =
  result {
    let! ast =
      match run Parser.expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(_s, parserError, _unit) ->
        Result.mapError CompileError.ParseError (Result.Error(parserError))

    let projectRoot = __SOURCE_DIRECTORY__
    let! ctx, env = Prelude.getEnvAndCtx projectRoot

    let! t = Result.mapError CompileError.TypeError (inferExpr ctx env None ast)

    return simplify t
  }

[<Fact>]
let InferBasicMutability () =
  let result =
    result {
      let src =
        """
        let mut x: number = 5;
        x = 10;
        """

      let! _, _ = inferModule src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferBasicObjectMutability () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        let mut p: Point = {x: 0, y: 0};
        p.x = 5;
        p.y = 10;
        """

      let! _, _ = inferModule src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferBasicArrayMutability () =
  let result =
    result {
      let src =
        """
        let mut array: number[] = [1, 2, 3];
        array[3] = 4;
        """

      let! _, _ = inferModule src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let DisallowAssigningToImmutableBindings () =
  let result =
    result {
      let src =
        """
        let x: number = 5;
        x = 10;
        """

      let! _, _ = inferModule src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let InferFnWithMutableParam () =
  let result =
    result {
      let src =
        """
        let foo = fn (mut array: (number | string)[]) {
          array[0] = 5;
          array[1] = "hello";
          array.push(10);
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn (mut array: (number | string)[]) -> undefined"
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableParamsAreInvariant () =
  let result =
    result {
      let src =
        """
        let foo = fn (mut array: (number | string)[]) {
          array[0] = 5;
          array[1] = "hello";
        };
        let mut numbers: (number | string)[] = [1, 2, 3];
        foo(numbers);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn (mut array: (number | string)[]) -> undefined"
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableParamsCantBeCovariant () =
  let result =
    result {
      let src =
        """
        let foo = fn (mut array: (number | string)[]) {
          array[0] = 5;
          array[1] = "hello";
        };
        let mut numbers: number[] = [1, 2, 3];
        foo(numbers);
        """

      let! ctx, _ = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)
    }

  Assert.False(Result.isError result)

[<Fact>]
let ImmutableAssignmentIsBeCovariant () =
  let result =
    result {
      let src =
        """
        let foo: number[] = [1, 2, 3];
        let bar: (number | string)[] = foo;
        """

      let! ctx, _ = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 0)
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableAssignmentIsInvariant () =
  let result =
    result {
      let src =
        """
        let mut foo: number[] = [1, 2, 3];
        let mut bar: number[] = foo;
        """

      let! _ = inferModule src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableAssignmentCantBeCovariant () =
  let result =
    result {
      let src =
        """
        let mut foo: number[] = [1, 2, 3];
        let mut bar: (number | string)[] = foo;
        """

      let! _ = inferModule src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let MutableTupleAssignmentCantBeCovariant () =
  let result =
    result {
      let src =
        """
        let mut foo: number[] = [5, 10];
        let mut bar: string[] = ["hello", "world"];
        let mut foobar: [(number | string)[], (number | string)[]] = [foo, bar];
        """

      let! _ = inferModule src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let CantPassImmutableArgsToMutableParams () =
  let result =
    result {
      let src =
        """
        let foo = fn (mut array: number[]) {
          array[0] = 5;
          array[1] = "hello";
        };
        let numbers: number[] = [1, 2, 3];
        foo(numbers);
        """

      let! _ = inferModule src
      // TODO: update mutability checking code to report recoverable errors
      // Assert.Equal(ctx.Report.Diagnostics.Length, 1)
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let ImmutableParamsAreCovariant () =
  let result =
    result {
      let src =
        """
        let foo = fn (array: (number | string)[]) {
          return array.length;
        };
        let numbers: number[] = [1, 2, 3];
        foo(numbers);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn (array: (number | string)[]) -> unique number"
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableObjectPartialInitialization () =
  let result =
    result {
      let src =
        """
        type Point = {x:number, y:number};
        type Line = {p0: Point, p1: Point};
        
        let mut p0: Point = {x: 0, y: 0};
        let mut p1: Point = {x: 5, y: 10};
        
        let line: Line = {p0, p1};
        let {p0: mut start, p1: mut end}: Line = {p0, p1};
        let {p0: start, p1: mut end}: Line = {p0, p1};
        let {p0: mut start, p1: end}: Line = {p0, p1};
        """

      let! _ = inferModule src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableArrayPartialInitialization () =
  let result =
    result {
      let src =
        """
        type Point = [number, number];
        type Line = [Point, Point];
        
        let mut p0: Point = [0, 0];
        let mut p1: Point = [5, 10];
        
        let line: Line = [p0, p1];
        let [mut start, mut end]: Line = [p0, p1];
        let [start, mut end]: Line = [p0, p1];
        let [mut start, end]: Line = [p0, p1];
        """

      let! _ = inferModule src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableObjectInitializationError () =
  let result =
    result {
      let src =
        """
        type Point = {x:number, y:number};
        type Line = {p0: Point, p1: Point};
        
        let mut p0 = {x: 0, y: 0};
        let p1 = {x: 5, y: 5};
        
        let mut line: Line = {p0, p1};
        """

      let! _ = inferModule src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let MutableArrayInitializationError () =
  let result =
    result {
      let src =
        """
        type Point = [number, number];
        type Line = [Point, Point];
        
        let mut p0 = [0, 0];
        let p1 = [5, 10];
        
        let mut line: Line = [p0, p1];
        """

      let! _ = inferModule src
      ()
    }

  printfn "result = %A" result
  Assert.True(Result.isError result)

[<Fact>]
let MutableObjectPartialInitializationError () =
  let result =
    result {
      let src =
        """
        type Point = {x:number, y:number};
        type Line = {p0: Point, p1: Point};
        
        let mut p0 = {x: 0, y: 0};
        let p1 = {x: 5, y: 5};
        
        let {p0: start, p1: mut end}: Line = {p0, p1};
        """

      let! _ = inferModule src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let MutableArrayPartialInitializationError () =
  let result =
    result {
      let src =
        """
        type Point = [number, number];
        type Line = [Point, Point];
        
        let mut p0 = [0, 0];
        let p1 = [5, 10];
        
        let [start, mut end]: Line = [p0, p1];
        """

      let! _ = inferModule src
      ()
    }

  printfn "result = %A" result
  Assert.True(Result.isError result)
