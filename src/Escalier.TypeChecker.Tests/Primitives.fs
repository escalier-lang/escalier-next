module Escalier.TypeChecker.Tests.Primitives

open FsToolkit.ErrorHandling
open Xunit

open Escalier.TypeChecker.Error

open TestUtils

[<Fact>]
let StringLiteralProperties () =
  let res =
    result {
      let src =
        """
        let msg = "hello";
        let len = msg.length;
        let upper = msg.toUpperCase();
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "len", "number")
      Assert.Value(env, "upper", "string")
    }

  Assert.False(Result.isError res)

[<Fact>]
let StringPropertyOnDirectLiteral () =
  let res =
    result {
      let src =
        """
        let len = "hello".length;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "len", "number")
    }

  Assert.False(Result.isError res)


[<Fact>]
let StringPrimitiveProperties () =
  let res =
    result {
      let src =
        """
        let msg: string = "hello";
        let len = msg.length;
        let upper = msg.toUpperCase();
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "len", "number")
      Assert.Value(env, "upper", "string")
    }

  Assert.False(Result.isError res)

[<Fact>]
let NumberLiteralProperties () =
  let res =
    result {
      let src =
        """
        let num = 5;
        let currency = num.toFixed(2);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "currency", "string")
    }

  Assert.False(Result.isError res)

[<Fact>]
let NumberPropertyDirectOnLiteral () =
  let res =
    result {
      let src =
        """
        let currency = (5).toFixed(2);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "currency", "string")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let NumberPrimitiveProperties () =
  let res =
    result {
      let src =
        """
        let num: number = 5;
        let currency = num.toFixed(2);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "currency", "string")
    }

  Assert.False(Result.isError res)

[<Fact>]
let BooleanLiteralProperties () =
  let res =
    result {
      let src =
        """
        let flag = true;
        let value = flag.valueOf();
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "value", "boolean")
    }

  Assert.False(Result.isError res)

[<Fact>]
let BooleanPrimitiveProperties () =
  let res =
    result {
      let src =
        """
        let flag: boolean = true;
        let value = flag.valueOf();
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "value", "boolean")
    }

  Assert.False(Result.isError res)

[<Fact>]
let SymbolProperties () =
  let res =
    result {
      let src =
        """
        let sym = Symbol();
        let name = sym.toString();
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "name", "string")
    }

  Assert.False(Result.isError res)

[<Fact>]
let UniqueSymbolProperties () =
  let res =
    result {
      let src =
        """
        declare let sym: unique symbol;
        let name = sym.toString();
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "name", "string")
    }

  Assert.False(Result.isError res)
