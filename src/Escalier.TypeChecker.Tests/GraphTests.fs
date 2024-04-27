module GraphTests

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Graph
open Escalier.Compiler
open Escalier.TypeChecker.Error
open TestUtils

[<Fact>]
let BuildDeclGraph () =
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

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let BuildDeclGraphIncorrectOrder () =
  let res =
    result {
      let src =
        """
        let y = x;
        let x = 5;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "math", "{add: fn () -> 15, sub: fn () -> -5}")
      // TODO: simplify these values
      Assert.Value(env, "values", "{sum: 15 + 0, diff: -5 + 0}")
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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  Assert.True(Result.isOk res)

[<Fact(Skip = "Fix stackoverflow")>]
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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

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

      let decls = getDeclsFromModule ast

      let! graph =
        buildGraph env [] [] decls |> Result.mapError CompileError.TypeError

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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      // TODO: simplify return types to `boolean`
      Assert.Value(
        env,
        "foo",
        "{isEven: fn (n: number) -> true | false | true | false}"
      )

      Assert.Value(
        env,
        "bar",
        "{isOdd: fn (n: number) -> false | true | false}"
      )
    }

  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveGraphInDeppObjects () =
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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      // TODO: simplify return types to `boolean`
      Assert.Value(
        env,
        "foo",
        "{math: {isEven: fn (n: number) -> true | false | true | false}}"
      )

      Assert.Value(
        env,
        "bar",
        "{math: {isOdd: fn (n: number) -> false | true | false}}"
      )
    }

  Assert.True(Result.isOk res)

[<Fact>]
let MutuallyRecursiveGraphInTuples () =
  let res =
    result {
      let src =
        """
        let foo = [
          fn (n) => if n == 0 { true } else { bar[0](n - 1) }
        ];
        let bar = [
          fn (n) => if n == 0 { false } else { foo[0](n - 1) } 
        ];
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      // TODO: simplify return types to `boolean`
      Assert.Value(
        env,
        "foo",
        "[fn (n: number) -> true | false | true | false]"
      )

      Assert.Value(env, "bar", "[fn (n: number) -> false | true | false]")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsBuildTreeFirst () =
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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let AcyclicFunctionDepsBuildTreeFirstWithTypeAnnotations () =
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

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "10")
      Assert.Value(env, "add", "fn () -> 15")
      Assert.Value(env, "sub", "fn () -> -5")
      Assert.Value(env, "poly", "fn () -> -75")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferMutuallyRecursiveFunctions () =
  let res =
    result {
      let src =
        """
        let isEven = fn (n) => if n == zero { true } else { isOdd(n - one) };
        let isOdd = fn (n) => if n == zero { false } else { isEven(n - one) };
        let zero = 0;
        let one = 1;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      // TODO: simplify return types to `boolean`
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
let InferMutuallyRecursiveTypes () =
  let res =
    result {
      let src =
        """
        type Foo = string | Bar[];
        type Bar = number | Foo[];
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Foo", "string | Bar[]")
      Assert.Type(env, "Bar", "number | Foo[]")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferTypeofType () =
  let res =
    result {
      let src =
        """
        let msg = "hello";
        type T = typeof msg;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "msg", "\"hello\"")
      Assert.Type(env, "T", "\"hello\"")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferTypeofTypeForObjectProperty () =
  let res =
    result {
      let src =
        """
        let obj = {msg: "hello"};
        type T = typeof obj.msg;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "obj", "{msg: \"hello\"}")
      Assert.Type(env, "T", "\"hello\"")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferIndexedAccessTypes () =
  let res =
    result {
      let src =
        """
        type Obj = {foo: string, bar: number};
        type Foo = Obj["foo"];
        type Bar = Obj["bar"];
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Foo", "Obj[\"foo\"]")
      Assert.Type(env, "Bar", "Obj[\"bar\"]")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferMutuallyRecursiveIndexedAccessTypes () =
  let res =
    result {
      let src =
        """
        type Foo = {foo: number, bar: Bar["bar"]};
        type Bar = {foo: Foo["foo"], bar: string};
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Foo", "{foo: number, bar: Bar[\"bar\"]}")
      Assert.Type(env, "Bar", "{foo: Foo[\"foo\"], bar: string}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferObjectTypeWithComputedKeys () =
  let res =
    result {
      let src =
        """
        let foo = "foo";
        let bar = "bar";
        type Obj = {
          [foo]: string,
          [bar]: number,
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Obj", "{foo: string, bar: number}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferObjectTypeWithQualifiedComputedKeys () =
  let res =
    result {
      let src =
        """
        let Keys = {FOO: "foo", BAR: "bar"};
        type Obj = {
          [Keys.FOO]: string,
          [Keys.BAR]: number,
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Obj", "{foo: string, bar: number}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferFuncDecl () =
  let result =
    result {
      let src =
        """
        fn fst (x, y) {
          return x;
        }
        declare fn snd<A, B>(x: A, y: B) -> B;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "fst", "fn <A, B>(x: A, y: B) -> A")
      Assert.Value(env, "snd", "fn <A, B>(x: A, y: B) -> B")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFuncDeclInModule () =
  let result =
    result {
      let src =
        """
        fn fst (x, y) {
          return x;
        }
        declare fn snd<A, B>(x: A, y: B) -> B;
        fn makePoint (x, y) -> Point {
          return {x, y};
        }
        type Point = {x: number, y: number};
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "fst", "fn <A, B>(x: A, y: B) -> A")
      Assert.Value(env, "snd", "fn <A, B>(x: A, y: B) -> B")
      Assert.Value(env, "makePoint", "fn (x: number, y: number) -> Point")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferInterfaceInModule () =
  let result =
    result {
      let src =
        """
        let p: Point = {x: 5, y: 10};
        interface Point {
          x: number,
        }
        interface Point {
          y: number,
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "Point", "{x: number, y: number}")
      Assert.Value(env, "p", "Point")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferNamespaceInModule () =
  let result =
    result {
      let src =
        """
        type Baz = Foo.Baz;
        namespace Foo {
          namespace Bar {
            let x = 5;
          }
          let y = Bar.x;
          type Baz = string;
        }
        let x = Foo.Bar.x;
        let y = Foo.y;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
      Assert.Type(env, "Baz", "Foo.Baz")

      let! t =
        Unify.expandScheme ctx env None (env.FindScheme "Baz") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "string")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferNamespaceInModuleWithCaptures () =
  let result =
    result {
      let src =
        """
        type Bar = string;
        let bar: Bar = "hello";
        namespace Foo {
          let baz: Bar = bar;
        }
        let baz = Foo.baz;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "baz", "Bar")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferNamespaceInModuleWithInterfaceCapture () =
  let result =
    result {
      let src =
        """
        interface Point {
          x: number,
        }
        interface Point {
          y: number,
        }
        namespace Foo {
          let bar = fn (p: Point) -> undefined {};
        }
        let bar = Foo.bar;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "bar", "fn (p: Point) -> undefined")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

// NOTE: requires updating inferDeclDefinitions to take pairs of declarations
// and placeholder types instead of `placeholderTypes` and `decls`.
[<Fact(Skip = "TODO")>]
let MultipleFuncDecls () =
  let result =
    result {
      let src =
        """
        fn add(x: number, y: number) -> number {
          return x + y;
        }
        fn add(x: string, y: string) -> string {
          return x ++ y;
        }
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "add", "fn (x: string, y: string) -> string")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferNestedFunctionsWithNestedCaptures () =
  let result =
    result {
      let src =
        """
        let x = 5;
        let y = 10;
        let add = fn (a) => fn (b) => a * x + b * y;
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)

      Assert.Value(
        env,
        "add",
        "fn <A: number, B: number>(a: A) -> fn (b: B) -> A * 5 + B * 10"
      )
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFunctionWithTypeParams () =
  let result =
    result {
      let src =
        """
        let add = fn <A: number, B: number>(x: A, y: B) {
          let a: A = x;
          let b: B = y;
          return a + b;
        };
        """

      let! ast =
        Parser.parseModule src |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! env =
        inferModuleUsingTree ctx env ast
        |> Result.mapError CompileError.TypeError

      Assert.Empty(ctx.Diagnostics)

      Assert.Value(env, "add", "fn <A: number, B: number>(x: A, y: B) -> A + B")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)
