module Modules

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker.Infer
open Escalier.TypeChecker.Unify

open TestUtils

[<Fact>]
let InferBasicModule () =
  let res =
    result {
      let src =
        """
        let add = fn(a, b) => a + b;
        let sub = fn(a, b) => a - b;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Diagnostics)
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

      Assert.Empty(ctx.Diagnostics)
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
        let even = fn (x) => if (x == 0) {
            true
        } else {
            !odd(x - 1)
        };

        let odd = fn (x) => if (x == 1) {
            true
        } else {
            !even(x - 1)
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "even", "fn (x: number) -> true | !boolean")
      Assert.Value(env, "odd", "fn (x: number) -> true | !(true | !boolean)")
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

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "Foo", "<T>({bar: Bar<T>})")
      Assert.Type(env, "Bar", "<T>({foo: Foo<T>})")
    }

  Assert.False(Result.isError res)


[<Fact>]
let MisorderedDeclsShouldFailInModule () =
  result {
    let src =
      """
      let y = x;
      let x = 5;
      """

    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    let! ctx, env = Prelude.getEnvAndCtx projectRoot

    try
      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.True(false)
    with _ ->
      Assert.True(true)
  }

[<Fact>]
let RecursiveDefinitionsInModule () =
  let result =
    result {
      let src =
        """
        let x = 5;
        let y = x;
        let foo = fn () => "hello";
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
      Assert.Value(env, "foo", "fn () -> \"hello\"")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let UsePreviousObjectInNextDefinition () =
  let result =
    result {
      let src =
        """
        let foo = {a: "hello", b: true};
        let bar = {a: foo.a, b: foo.b};
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "foo", "{a: \"hello\", b: true}")
      Assert.Value(env, "bar", "{a: \"hello\", b: true}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let RecursiveFunctionsInModule () =
  let result =
    result {
      let src =
        """
        let foo = fn (x) => if x > 0 { bar(x) } else { 0 };
        let bar = fn (x) => if x > 0 { x } else { foo(x) };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)

      Assert.Value(env, "foo", "fn (x: t6:number) -> t6:number | 0 throws t21")

      Assert.Value(
        env,
        "bar",
        "fn (x: t6:number) -> t6:number | t6:number | 0 throws t21"
      )
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let RecursiveTypesInModule () =
  let result =
    result {
      let src =
        """
        type Foo = number | Bar[];
        type Bar = string | Foo[];
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "Foo", "number | Bar[]")
      Assert.Type(env, "Bar", "string | Foo[]")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let TypeofInType () =
  let result =
    result {
      let src =
        """
        let p2: Point = {x: 5, y: 10};
        type Point = typeof p1;
        type X = typeof p1.x;
        type Y = typeof p1.y;
        let p1 = {x: 5, y: 10};
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "Point", "{x: 5, y: 10}")
      Assert.Type(env, "X", "5")
      Assert.Type(env, "Y", "10")
      Assert.Value(env, "p1", "{x: 5, y: 10}")
      Assert.Value(env, "p2", "Point")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let ComputedKeysObjectValues () =
  let result =
    result {
      let src =
        """
        let Constants = {
          foo: "foo",
          bar: "bar",
        };
        let obj = {
          [Constants.foo]: "hello",
          [Constants.bar]: "world",
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "obj", "{foo: \"hello\", bar: \"world\"}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let ComputedKeysObjectTypes () =
  let result =
    result {
      let src =
        """
        let Constants = {
          foo: "foo",
          bar: "bar",
        };
        type Obj = {
          [Constants.foo]: number,
          [Constants.bar]: boolean,
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "Obj", "{foo: number, bar: boolean}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let ComputedKeysObjectValuesAndTypes () =
  let result =
    result {
      let src =
        """
        let Constants = {
          foo: "foo",
          bar: "bar",
        };
        let obj: Obj = {
          [Constants.foo]: 5,
          [Constants.bar]: true,
        };
        type Obj = {
          [Constants.foo]: number,
          [Constants.bar]: boolean,
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "obj", "Obj")
      Assert.Type(env, "Obj", "{foo: number, bar: boolean}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)


[<Fact>]
let IndexedAccessTypes () =
  let result =
    result {
      let src =
        """
        type Foo = Obj["foo"];
        type Bar = Obj["bar"];
        type Obj = {
          foo: number,
          bar: boolean,
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        newInferModule ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "Obj", "{foo: number, bar: boolean}")
      Assert.Type(env, "Foo", "Obj[\"foo\"]")
      Assert.Type(env, "Bar", "Obj[\"bar\"]")

      let! t =
        expandScheme ctx env None (env.FindScheme "Foo") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "number")

      let! t =
        expandScheme ctx env None (env.FindScheme "Bar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "boolean")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let NamespaceValues () =
  let result =
    result {
      let src =
        """
        namespace Foo {
          namespace Bar {
            let x = 5;
          }
          let y = Bar.x;
        }
        let x = Foo.Bar.x;
        let y = Foo.y;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
    }

  Assert.False(Result.isError result)

[<Fact>]
let NamespaceTypes () =
  let result =
    result {
      let src =
        """
        type X = Foo.Bar.X;
        type Y = Foo.Y;
        namespace Foo {
          type Y = Bar.X;
          namespace Bar {
            type X = number;
          }
        }
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "X", "Foo.Bar.X")
      Assert.Type(env, "Y", "Foo.Y")

      let! t =
        expandScheme ctx env None (env.FindScheme "X") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "number")

      let! t =
        expandScheme ctx env None (env.FindScheme "Y") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "number")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)


[<Fact>]
let NamespaceValuesAndTypes () =
  let result =
    result {
      let src =
        """
        type X = Foo.Bar.X;
        type Y = Foo.Y;
        namespace Foo {
          type Y = Bar.X;
          namespace Bar {
            type X = number;
            let x = 5;
          }
          let y = Bar.x;
        }
        let x = Foo.Bar.x;
        let y = Foo.y;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
      Assert.Type(env, "X", "Foo.Bar.X")
      Assert.Type(env, "Y", "Foo.Y")

      let! t =
        expandScheme ctx env None (env.FindScheme "X") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "number")

      let! t =
        expandScheme ctx env None (env.FindScheme "Y") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "number")
    }

  Assert.False(Result.isError result)
