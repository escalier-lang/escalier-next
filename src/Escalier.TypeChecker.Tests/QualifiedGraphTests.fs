module Escalier.TypeChecker.Tests.QualifiedGraphTests

open Escalier.Data.Type
open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.QualifiedGraph
open Escalier.TypeChecker.BuildGraph
open Escalier.TypeChecker.InferGraph

open TestUtils

[<Fact>]
let AddBinding () =
  let env = Env.empty "input.esc"

  let t =
    { Kind = TypeKind.Primitive Primitive.Number
      Provenance = None }

  let ident =
    { Namespaces = [ "Foo"; "Bar" ]
      Name = "x" }

  let newEnv = addBinding env ident (t, false)

  let ident =
    { Namespaces = [ "Foo"; "Bar" ]
      Name = "y" }

  let newEnv = addBinding newEnv ident (t, false)
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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      printfn "graph.Edges = "

      for KeyValue(key, value) in graph.Edges do
        printfn $"{key} -> {value}"

      printfn "graph.Nodes.Keys = "

      for key in graph.Nodes.Keys do
        printfn $"{key}"

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "\"hello\"")
    }

  Assert.True(Result.isOk res)

[<Fact(Skip = "TODO: make this pass")>]
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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      printfn "graph.Edges = "

      for KeyValue(key, value) in graph.Edges do
        printfn $"{key} -> {value}"

      printfn "graph.Nodes.Keys = "

      for key in graph.Nodes.Keys do
        printfn $"{key}"

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Type(env, "X", "Foo.Bar.X")

      let! t =
        Unify.expandScheme ctx env None (env.FindScheme "X") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "5")
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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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
        fn add5(x) {
          let y = 5;
          return x + y;
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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
          return fn (x) {
            return x + y;
          };
        };
        let add5 = getAdd5();
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Keys", "{foo: \"foo\"}")
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
        declare let keys: Keys;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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
        let fact = fn (n) => if n == 0 { 1 } else { n * fact(n - 1) };
        let fib = fn (n) => if n <= 1 { n } else { fib(n - 1) + fib(n - 2) };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Value(env, "fact", "fn (arg0: number) -> 1 | number")
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
        fn fact (n) {
          return if n == 0 { 1 } else { n * fact(n - 1) };
        }
        fn fib (n) {
          return if n <= 1 { n } else { fib(n - 1) + fib(n - 2) };
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Value(env, "fact", "fn (n: number) -> 1 | number")
      Assert.Value(env, "fib", "fn (n: number) -> number | number")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let MutuallysRecursiveFunctions () =
  let res =
    result {
      let src =
        """
        let isEven = fn (n) => if n == 0 { true } else { isOdd(n - 1) };
        let isOdd = fn (n) => if n == 0 { false } else { isEven(n - 1) };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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
        fn isEven (n) {
          return if n == 0 { true } else { isOdd(n - 1) };
        }
        fn isOdd (n) {
          return if n == 0 { false } else { isEven(n - 1) };
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Value(env, "isEven", "fn (n: number) -> true | false | true")

      Assert.Value(
        env,
        "isOdd",
        "fn (n: number) -> false | true | false | true"
      )
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
          let fact = fn (n) => if n == 0 { 1 } else { n * fact(n - 1) };
          return fact;
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      Assert.Value(env, "ret_fact", "fn () -> fn (arg0: number) -> 1 | number")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferFunctionDeclTypeFromBody () =
  let res =
    result {
      let src =
        """
        fn add(x, y) {
          return x + y;
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let graph = buildGraph env ast

      let! env =
        inferGraph ctx env graph |> Result.mapError CompileError.TypeError

      ()
    }

  Assert.True(Result.isError res)
