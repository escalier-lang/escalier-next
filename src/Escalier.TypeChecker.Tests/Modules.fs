module Modules

open FsToolkit.ErrorHandling
open Xunit

open TestUtils

[<Fact>]
let InferBasicModule () =
  let res =
    result {
      let src =
        """
        let add = fn<A: number, B: number>(a: A, b: B) => a + b;
        let sub = fn<A: number, B: number>(a: A, b: B) => a - b;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "add", "fn <A: number, B: number>(a: A, b: B) -> A + B")
      Assert.Value(env, "sub", "fn <A: number, B: number>(a: A, b: B) -> A - B")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferMutuallyRecursiveFunctions () =
  let res =
    result {
      let src =
        """
        let foo = fn() => bar() + 1;
        let bar = fn() => foo() - 1;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn () -> number")
      Assert.Value(env, "bar", "fn () -> number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferMutualRecursion () =
  let res =
    result {
      let src =
        """
        let even = fn (x: number) => if (x == 0) {
            true
        } else {
            !odd(x - 1)
        };

        let odd = fn (x: number) => if (x == 1) {
            true
        } else {
            !even(x - 1)
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "even", "fn (x: number) -> true | !boolean")
      Assert.Value(env, "odd", "fn (arg0: number) -> boolean")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferMutuallyRecursiveTypes () =
  let res =
    result {
      let src =
        """
        type Foo<T> = {bar: Bar<T>};
        type Bar<T> = {foo: Foo<T>};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "Foo", "<T>({bar: Bar<T>})")
      Assert.Type(env, "Bar", "<T>({foo: Foo<T>})")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferModuleWithTopLevelExpressionsNoErrors () =
  let res =
    result {
      let src =
        """
        let foo = fn (x: number) {
          console.log(x);
        };
        foo(5);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferModuleWithTopLevelExpressionsRecoverableErrors () =
  let res =
    result {
      let src =
        """
        let foo = fn (x: number) {
          console.log(x);
        };
        foo("hello");
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferModuleWithTopLevelAssignments () =
  let res =
    result {
      let src =
        """
        let x: number = 5;
        let mut y: number = 0;
        y = x;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferModuleWithTopLevelForLoop () =
  let res =
    result {
      let src =
        """
        let mut sum: number = 0;
        for x in [1, 2, 3] {
          let square = x * x;
          sum = sum + square;
        }
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)
