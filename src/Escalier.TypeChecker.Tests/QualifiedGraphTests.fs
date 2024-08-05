module Escalier.TypeChecker.Tests.QualifiedGraphTests

open Escalier.Data
open Escalier.Data.Type
open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.QualifiedGraph
open Escalier.TypeChecker.BuildGraph

open TestUtils

let inferModule src =
  result {
    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    let! ctx, env = Prelude.getEnvAndCtx projectRoot

    let! env =
      Infer.inferModule ctx env ast |> Result.mapError CompileError.TypeError

    return ctx, env
  }

[<Fact>]
let AddBinding () =
  let env = Env.empty "input.esc"

  let t =
    { Kind = TypeKind.Primitive Primitive.Number
      Provenance = None }

  let ident = { Parts = [ "Foo"; "Bar"; "x" ] }
  let newEnv = Infer.addBinding env ident (t, false)
  let ident = { Parts = [ "Foo"; "Bar"; "y" ] }
  let newEnv = Infer.addBinding newEnv ident (t, false)
  // printfn $"newEnv = {newEnv}"
  ()

[<Fact>]
let NamespaceShadowingOfVariables () =
  let res =
    result {
      let src =
        """
        let x = 5;
        namespace Foo {
          let x = "hello";
          namespace Bar {
            let y = x;
          }
        }
        let y = Foo.Bar.y;
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "\"hello\"")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let NamespaceShadowingOfTypes () =
  let res =
    result {
      let src =
        """
        type X = 5;
        namespace Foo {
          type X = "hello";
          namespace Bar {
            type Y = X;
          }
        }
        type Y = Foo.Bar.Y;
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "X", "5")
      Assert.Type(env, "Y", "Foo.Bar.Y")

      let! t =
        Unify.expandScheme ctx env None (env.FindScheme "Y") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "\"hello\"")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let NamespaceReferenceOtherNamespaces () =
  let res =
    result {
      let src =
        """
        namespace Foo {
          namespace Bar {
            let x = 5;
          }
          let y = Bar.x + 10;
        }
        let x = Foo.Bar.x;
        let y = Foo.y;
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "15")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let NamespaceBasicValues () =
  let res =
    result {
      let src =
        """
        namespace Foo {
          namespace Bar {
            let x = 5;
          }
        }
        let x = Foo.Bar.x;
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "x", "5")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let NamespaceBasicTypes () =
  let res =
    result {
      let src =
        """
        namespace Foo {
          namespace Bar {
            type X = 5;
          }
        }
        type X = Foo.Bar.X;
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "X", "Foo.Bar.X")

      let! t =
        Unify.expandScheme ctx env None (env.FindScheme "X") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "5")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let NamespaceWithInterface () =
  let res =
    result {
      let src =
        """
        namespace Foo {
          interface Bar {
            msg: string
          }
        }
        interface Baz {
          bar: Foo.Bar
        }
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "Baz", "{bar: Foo.Bar}")

      let! t =
        Unify.expandScheme ctx env None (env.FindScheme "Baz") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "{bar: Foo.Bar}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InterfacesInTheSameNamespace () =
  let res =
    result {
      let src =
        """
        namespace Foo {
          interface Bar {
            msg: string
          }
          
          interface Baz {
            bar: Bar
          }
        }
        """

      let! ctx, env = inferModule src

      ()
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)


[<Fact>]
let BasicGraphInferCompositeValues () =
  let res =
    result {
      let src =
        """
        let [x, y] = [5, "hello"];
        let {a, b} = {a: 10, b: "world"};
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "\"hello\"")
      Assert.Value(env, "a", "10")
      Assert.Value(env, "b", "\"world\"")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BasicGraphInferTypes () =
  let res =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        type Foo<T> = {bar: T | Foo<T>[]};
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "Point", "{x: number, y: number}")
      Assert.Type(env, "Foo", "<T>({bar: T | Foo<T>[]})")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BasicGraphInferFunctionDecl () =
  let res =
    result {
      let src =
        """
        fn add(x: number, y: number) -> number {
          return x + y;
        }
        fn fst<T, U>(x: T, y: U) -> T {
          return x;
        }
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "add", "fn (x: number, y: number) -> number")
      Assert.Value(env, "fst", "fn <T, U>(x: T, y: U) -> T")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let FunctionDeclsWithLocalVariables () =
  let res =
    result {
      let src =
        """
        fn add5<A: number>(x: A) {
          let y = 5;
          return x + y;
        }
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "add5", "fn <A: number>(x: A) -> A + 5")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ReturnFunctionDeclWithCaptures () =
  // TODO: fix this for function decls as well
  let res =
    result {
      let src =
        """
        let getAdd5 = fn () {
          let y = 5;
          return fn<A: number>(x: A) {
            return x + y;
          };
        };
        let add5 = getAdd5();
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "add5", "fn <A: number>(x: A) -> A + 5")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)


[<Fact>]
let BasicDeps () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = x;
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BasicFunctionCaptures () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = 10;
        fn add() {
          return x + y;
        }
        let sum = add();
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "sum", "15")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let OutOfOrderFunctionCaptures () =
  let res =
    result {
      let src =
        """
        fn add() {
          return x + y;
        }
        let x = 5;
        let y = 10;
        let sum = add();
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "sum", "15")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let OutOfOrderTypeDepsWithTypeParamConstraint () =
  let res =
    result {
      let src =
        """
        type Bar<T: Baz> = {bar: T};
        type Baz = string;
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "Bar", "<T: Baz>({bar: T})")
      Assert.Type(env, "Baz", "string")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BasicInterface () =
  let res =
    result {
      // We've been setting the `Inferred` field on declarations to the inferred
      // type, but what should we do when there are multiple declarations that
      // result in a shared typed?
      let src =
        """
        interface FooBar { foo: number }
        interface FooBar { bar: string }
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "FooBar", "{foo: number, bar: string}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let VariableDeclWithoutInit () =
  let res =
    result {
      let src =
        """
        interface Keys {foo: "foo"}
        declare let keys: Keys;
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "Keys", "{foo: \"foo\"}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ComputedInterfaceKeys () =
  let res =
    result {
      let src =
        """
        interface Keys {
          foo: "foo",
          bar: "bar",
        }
        declare let keys: Keys;
        interface Obj {
          [keys.foo]: number,
          [keys.bar]: number,
        }
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "Keys", "{foo: \"foo\", bar: \"bar\"}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let MergeInterfaceBetweenFiles () =
  let res =
    result {
      let src =
        """
        interface Keys {foo: "foo"}
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "Keys", "{foo: \"foo\"}")

      let src =
        """
        interface Keys { bar: "bar"}
        declare let keys: Keys;
        let foo = keys.foo;
        let bar = keys.bar;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let decls =
        List.choose
          (fun (item: Syntax.ModuleItem) ->
            match item with
            | Syntax.ModuleItem.Stmt { Kind = Syntax.Decl decl } -> Some decl
            | _ -> None)
          ast.Items

      let graph = buildGraph env decls

      let! env =
        Infer.inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Value(env, "foo", "\"foo\"")
      Assert.Value(env, "bar", "\"bar\"")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let MergeInterfaceBetweenFilesWithComputedKeys () =
  let res =
    result {
      let src =
        """
        interface Keys {foo: "foo"}
        declare let keys: Keys;
        """

      let! ctx, env = inferModule src

      Assert.Type(env, "Keys", "{foo: \"foo\"}")

      let src =
        """
        interface Keys { bar: "bar"}
        interface Obj {
          [keys.foo]: number,
          [keys.bar]: number,
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let decls =
        List.choose
          (fun (item: Syntax.ModuleItem) ->
            match item with
            | Syntax.ModuleItem.Stmt { Kind = Syntax.Decl decl } -> Some decl
            | _ -> None)
          ast.Items

      let graph = buildGraph env decls

      let! env =
        Infer.inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Keys", "{foo: \"foo\", bar: \"bar\"}")

      Assert.Type(env, "Obj", "{foo: number, bar: number}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)


[<Fact>]
let SelfRecursiveFunctions () =
  let res =
    result {
      let src =
        """
        let fact = fn (n: number) => if n == 0 { 1 } else { n * fact(n - 1) };
        let fib = fn (n: number) => if n <= 1 { n } else { fib(n - 1) + fib(n - 2) };
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "fact", "fn (arg0: number) -> number")
      Assert.Value(env, "fib", "fn (arg0: number) -> number")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let SelfRecursiveFunctionDecls () =
  let res =
    result {
      let src =
        """
        fn fact (n: number) {
          return if n == 0 { 1 } else { n * fact(n - 1) };
        }
        fn fib (n: number) {
          return if n <= 1 { n } else { fib(n - 1) + fib(n - 2) };
        }
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "fact", "fn (arg0: number) -> number")
      Assert.Value(env, "fib", "fn (arg0: number) -> number")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let MutuallysRecursiveFunctions () =
  let res =
    result {
      let src =
        """
        let isEven = fn (n: number) => if n == 0 { true } else { isOdd(n - 1) };
        let isOdd = fn (n: number) => if n == 0 { false } else { isEven(n - 1) };
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "isEven", "fn (n: number) -> true | false | true")
      Assert.Value(env, "isOdd", "fn (arg0: number) -> false | true")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveFunctionDecls () =
  let res =
    result {
      let src =
        """
        fn isEven (n: number) {
          return if n == 0 { true } else { isOdd(n - 1) };
        }
        fn isOdd (n: number) {
          return if n == 0 { false } else { isEven(n - 1) };
        }
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "isEven", "fn (n: number) -> true | false | true")
      Assert.Value(env, "isOdd", "fn (arg0: number) -> false | true")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ReturnSelfRecursiveFunction () =
  let res =
    result {
      let src =
        """
        let ret_fact = fn () {
          let fact = fn (n: number) => if n == 0 { 1 } else { n * fact(n - 1) };
          return fact;
        };
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "ret_fact", "fn () -> fn (arg0: number) -> number")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferFunctionDeclTypeFromBody () =
  let res =
    result {
      let src =
        """
        fn add<A: number, B: number>(x: A, y: B) {
          return x + y;
        }
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "add", "fn <A: number, B: number>(x: A, y: B) -> A + B")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferFunctionDeclTypeFromBodyWrongSig () =
  let res =
    result {
      let src =
        """
        fn add(x: string, y: string) -> string {
          return x + y;
        }
        """

      let! ctx, env = inferModule src

      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let InferFunctionWithTypeRefInsideNamespace () =
  let res =
    result {
      let src =
        """
        namespace Foo {
          type Point = {x: number, y: number};
          fn get(p: Point, axis: keyof Point) -> number {
            if axis == "x" {
              return p.x;
            } else {
              return p.y;
            }
          }
          fn get(p: Point) -> Point {
            return p;
          }
        }
        let p = {x: 5, y: 10};
        let x = Foo.get(p, "x");
        let y = Foo.get(p, "y");
        let get = Foo.get;
        """

      let! ctx, env = inferModule src

      Assert.Value(
        env,
        "get",
        "fn (p: Foo.Point, axis: keyof Foo.Point) -> number & fn (p: Foo.Point) -> Foo.Point"
      )
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact(Skip = "TODO")>]
let InferFunctionReturningValuesFromNamespace () =
  let res =
    result {
      let src =
        """                                                                       
        let getGet = fn () {
          namespace Foo {
            type Point = {x: number, y: number};
            fn get(p: Point, axis: keyof Point) -> number {
              return p.x;
            }
          }
          return fn (p, axis) => Foo.get(p, axis);
        };
        let get = getGet();
        let p = {x: 5, y: 10};
        let x = get(p, "x");
        let y = get(p, "y");
        """

      let! ctx, env = inferModule src

      Assert.Value(env, "get", "")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)
