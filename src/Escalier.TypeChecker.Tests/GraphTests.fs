module GraphTests

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Parser
open Escalier.Compiler
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Graph

open TestUtils

[<Fact>]
let EmptyGraph () =
  let graph: DeclGraph<unit> =
    { Nodes = Map.empty
      Edges = Map.empty
      Namespaces = Map.empty }

  let components = findStronglyConnectedComponents graph

  Assert.Equal(0, components.Length)

[<Fact>]
let GraphWithSingleCycleThatIsTheEntireGraph () =
  let nodes: Map<DeclIdent, list<unit>> =
    Map.ofList
      [ DeclIdent.Value "A", []
        DeclIdent.Value "B", []
        DeclIdent.Value "C", [] ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A", [ DeclIdent.Value "B" ]
        DeclIdent.Value "B", [ DeclIdent.Value "C" ]
        DeclIdent.Value "C", [ DeclIdent.Value "A" ] ]

  let graph =
    { Nodes = nodes
      Edges = edges
      Namespaces = Map.empty }

  let components = findStronglyConnectedComponents graph

  Assert.Equal(1, components.Length)
  Assert.Equal(3, components.[0].Length)

[<Fact>]
let GraphWithSingleCycleThatIsntTheEntireGraph () =
  let nodes: Map<DeclIdent, list<unit>> =
    Map.ofList
      [ DeclIdent.Value "A", []
        DeclIdent.Value "B", []
        DeclIdent.Value "C", [] ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A", [ DeclIdent.Value "B" ]
        DeclIdent.Value "B", [ DeclIdent.Value "A" ] ]

  let graph =
    { Nodes = nodes
      Edges = edges
      Namespaces = Map.empty }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ DeclIdent.Value "C" ]; [ DeclIdent.Value "A"; DeclIdent.Value "B" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)

[<Fact>]
let GraphWithComponentsWithMultipleDependencies () =
  let nodes: Map<DeclIdent, list<unit>> =
    Map.ofList
      [ DeclIdent.Value "A", []
        DeclIdent.Value "B", []
        DeclIdent.Value "C", []
        DeclIdent.Value "D", [] ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A",
        [ DeclIdent.Value "B"; DeclIdent.Value "C"; DeclIdent.Value "D" ]
        // one of the dependencies of "A" contains a cycle
        DeclIdent.Value "C", [ DeclIdent.Value "D" ]
        DeclIdent.Value "D", [ DeclIdent.Value "C" ] ]

  let graph =
    { Nodes = nodes
      Edges = edges
      Namespaces = Map.empty }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ Value "A" ]; [ Value "C"; Value "D" ]; [ Value "B" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)

  let tree = buildComponentTree graph actual

  Assert.Equal(
    tree.ToString(),
    """map [(set [Value "A"], set [set [Value "B"]; set [Value "C"; Value "D"]]); (set [Value "B"], set []); (set [Value "C"; Value "D"], set [])]"""
  )

  let entryPoints = findEntryPoints tree

  Assert.Equal(entryPoints.ToString(), """set [set [Value "A"]]""")

[<Fact>]
let GraphWithMultipleComponents () =
  let nodes: Map<DeclIdent, list<unit>> =
    Map.ofList
      [ DeclIdent.Value "A", []
        DeclIdent.Value "B", []
        DeclIdent.Value "C", []
        DeclIdent.Value "D", []
        DeclIdent.Value "E", [] ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A", [ DeclIdent.Value "B" ]
        DeclIdent.Value "B", [ DeclIdent.Value "A"; DeclIdent.Value "C" ]
        DeclIdent.Value "C", [ DeclIdent.Value "D" ]
        DeclIdent.Value "D", [ DeclIdent.Value "E" ]
        DeclIdent.Value "E", [ DeclIdent.Value "D" ] ]

  let graph =
    { Nodes = nodes
      Edges = edges
      Namespaces = Map.empty }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ Value "A"; Value "B" ]; [ Value "C" ]; [ Value "D"; Value "E" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)

  let tree = buildComponentTree graph actual

  Assert.Equal(
    tree.ToString(),
    """map [(set [Value "A"; Value "B"], set [set [Value "C"]]); (set [Value "C"], set [set [Value "D"; Value "E"]]); (set [Value "D"; Value "E"], set [])]"""
  )

  let entryPoints = findEntryPoints tree

  Assert.Equal(entryPoints.ToString(), """set [set [Value "A"; Value "B"]]""")

[<Fact>]
let GraphWithMultipleEntryPoints () =
  let nodes: Map<DeclIdent, list<unit>> =
    Map.ofList
      [ DeclIdent.Value "A", []
        DeclIdent.Value "B", []
        DeclIdent.Value "C", []
        DeclIdent.Value "D", []
        DeclIdent.Value "E", [] ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A", [ DeclIdent.Value "B" ]
        DeclIdent.Value "B", [ DeclIdent.Value "A"; DeclIdent.Value "C" ]
        DeclIdent.Value "C", []
        DeclIdent.Value "D", [ DeclIdent.Value "E" ]
        DeclIdent.Value "E", [ DeclIdent.Value "D" ] ]

  let graph =
    { Nodes = nodes
      Edges = edges
      Namespaces = Map.empty }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ Value "D"; Value "E" ]; [ Value "A"; Value "B" ]; [ Value "C" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)

  let tree = buildComponentTree graph actual

  Assert.Equal(
    tree.ToString(),
    """map [(set [Value "A"; Value "B"], set [set [Value "C"]]); (set [Value "C"], set []); (set [Value "D"; Value "E"], set [])]"""
  )

  let entryPoints = findEntryPoints tree

  Assert.Equal(
    entryPoints.ToString(),
    """set [set [Value "A"; Value "B"]; set [Value "D"; Value "E"]]"""
  )

[<Fact>]
let GraphWithNoEdges () =
  let nodes: Map<DeclIdent, list<unit>> =
    Map.ofList
      [ DeclIdent.Value "A", []
        DeclIdent.Value "B", []
        DeclIdent.Value "C", [] ]

  let edges: Map<DeclIdent, list<DeclIdent>> = Map.empty

  let graph =
    { Nodes = nodes
      Edges = edges
      Namespaces = Map.empty }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ Value "C" ]; [ Value "B" ]; [ Value "A" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)


[<Fact>]
let BuildDeclGraph () =
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

  Assert.True(Result.isOk res)

[<Fact(Skip = "TODO: make this an error again")>]
let BuildDeclGraphIncorrectOrder () =
  let res =
    result {
      let src =
        """
        let y = x;
        let x = 5;
        """

      let! ctx, env = inferModule src
      ()
    }

  printfn "res = %A" res

  Assert.Equal(
    res,
    Error(
      TypeError.SemanticError "x has not been initialized yet"
      |> CompileError.TypeError
    )
  )

[<Fact>]
let BuildDeclGraphWithFunction () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = 10;
        let add = fn () => x + y;
        let sum = add();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithFunctionWithParam () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let add = fn (y) => x + y;
        let sum = add(10);
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "add", "fn <A: number>(y: A) -> 5 + A")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let BuildDeclGraphWithGenericFunction () =
  let res =
    result {
      let src =
        """
        let id = fn (x) => x;
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "id", "fn <A>(x: A) -> A")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let BuildDeclGraphWithFunctions () =
  let res =
    result {
      let src =
        """
        let x = 5;
        let y = 10;
        let obj = {foo: fn () => x, bar: fn () => y};
        let {foo, bar} = obj;
        """

      let! ctx, env = inferModule src
      ()
    }

  Assert.True(Result.isOk res)

[<Fact>]
let BuildDeclGraphWithCapturesDefinedAfterClosure () =
  let res =
    result {
      let src =
        """
        let add = fn () => x + y;
        let x = 5;
        let y = 10;
        let sum = add();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithNonFunctionDeps () =
  let res =
    result {
      let src =
        """
        let add = fn () => x + y;
        let x = 5;
        let y = x + 5;
        let sum = add();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithFunctionCallDeps () =
  let res =
    result {
      let src =
        """
        let add = fn () => x + y;
        let sub = fn () => x - y;
        let poly = fn () => sum * diff;
        let x = 5;
        let y = 10;
        let zero = 0;
        let sum = add() + zero;
        let diff = sub() + zero;
        let result = poly();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "sum", "15")
      Assert.Value(env, "diff", "-5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithFunctionCallDepsWithObjects () =
  let res =
    result {
      let src =
        """
        let math = {add: fn () => x + y, sub: fn () => x - y};
        let poly = fn () => values.sum * values.diff;
        let x = 5;
        let y = 10;
        let zero = 0;
        let values = {sum: math.add() + zero, diff: math.sub() + zero};
        let result = poly();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "math", "{add: fn () -> 15, sub: fn () -> -5}")
      // TODO: simplify these values
      Assert.Value(env, "values", "{sum: 15, diff: -5}")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let GraphWithFunctionsInObject () =
  let res =
    result {
      let src =
        """
        let math = {add: fn () => x + y};
        let x = 5;
        let y = 10;
        let sum = math.add();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "sum", "15")
    }

  Assert.True(Result.isOk res)


[<Fact>]
let AcyclicFunctionDeps () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let add = fn () => x + y;
        let sub = fn () => x - y;
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsBuildGraphFirst () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let add = fn () => x + y;
        let sub = fn () => x - y;
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsInObject () =
  let res =
    result {
      let src =
        """
        let poly = fn() => math.add() * math.sub();
        let math = {add: fn () => x + y, sub: fn () => x - y};
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "math", "{add: fn () -> 15, sub: fn () -> -5}")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsInObjectWithDestructuring () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let {add, sub} = {add: fn () => x + y, sub: fn () => x - y};
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsInObjectWithDestructuringInSeparateStatement () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let obj = {add: fn () => x + y, sub: fn () => x - y};
        let {add, sub} = obj;
        let x = 5;
        let y = 10;
        let result = poly();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsInObjectWithDestructuringStress () =
  let res =
    result {
      let src =
        """
        let poly = fn() => add() * sub();
        let {add, x} = {add: fn () => x + y, x: 5};
        let {sub, y} = {sub: fn () => x - y, y: 10};
        let result = poly();
        """

      let! ctx, env = inferModule src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let BuildRecursiveGraph () =
  let res =
    result {
      let src =
        """
        let fact = fn (n) => if n == 0 { 1 } else { n * fact(n - 1) };
        let fib = fn (n) => if n <= 1 { n } else { fib(n - 1) + fib(n - 2) };
        """

      let! ctx, env = inferModule src
      // TODO: merge 1 and number
      // TODO: maintain the name of the function argument
      Assert.Value(env, "fact", "fn (arg0: number) -> 1 | number")
      Assert.Value(env, "fib", "fn (arg0: number) -> number")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveGraph () =
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

      let decls = Infer.getDeclsFromModule ast

      let! graph =
        Graph.buildGraph env [] [] decls
        |> Result.mapError CompileError.TypeError

      printfn "graph.Edges = %A" graph.Edges

      ()
    // let! env =
    //   Infer.inferModuleUsingGraph ctx env ast
    //   |> Result.mapError CompileError.TypeError
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveGraphInObjects () =
  let res =
    result {
      let src =
        """
        let foo = {
          isEven: fn (n) => if n == 0 { true } else { bar.isOdd(n - 1) }
        };
        let bar = {
          isOdd: fn (n) => if n == 0 { false } else { foo.isEven(n - 1) } 
        };
        """

      let! ctx, env = inferModule src
      // TODO: simplify return types to `boolean`
      Assert.Value(env, "foo", "{isEven: fn (arg0: number) -> true | false}")

      Assert.Value(
        env,
        "bar",
        "{isOdd: fn (n: number) -> false | true | false}"
      )
    }

  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveGraphInDepObjects () =
  let res =
    result {
      let src =
        """
        let foo = {
          math: {
            isEven: fn (n) => if n == 0 { true } else { bar.math.isOdd(n - 1) },
          },
        };
        let bar = {
          math: {
            isOdd: fn (n) => if n == 0 { false } else { foo.math.isEven(n - 1) }, 
          },
        };
        """

      let! ctx, env = inferModule src
      // TODO: simplify return types to `boolean`
      Assert.Value(
        env,
        "foo",
        "{math: {isEven: fn (arg0: number) -> true | false}}"
      )

      Assert.Value(
        env,
        "bar",
        "{math: {isOdd: fn (n: number) -> false | true | false}}"
      )
    }

  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveTypeDecl () =
  let res =
    result {
      let src =
        """
        type Foo = {foo: number | Bar["bar"][]};
        type Bar = {bar: string | Foo["foo"][]};
        """

      let! ctx, env = inferModule src
      Assert.Type(env, "Foo", "{foo: number | Bar[\"bar\"][]}")
      Assert.Type(env, "Bar", "{bar: string | Foo[\"foo\"][]}")
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

      let! env =
        InferGraph.inferModule ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Keys", "{foo: \"foo\", bar: \"bar\"}")

      Assert.Type(env, "Obj", "{foo: number, bar: number}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let OutOfOrderDepsInsideNamespace () =
  let res =
    result {
      let src =
        """
        namespace Foo {
          type Bar<T: Baz> = {bar: T};
          type Baz = string;
        }
        """

      let! ctx, env = inferModule src
      ()
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let OutOfOrderDepsOutsideNamespace () =
  let res =
    result {
      let src =
        """
        type Bar<T: Baz> = {bar: T};
        type Baz = string;
        """

      let! ctx, env = inferModule src
      ()
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)
