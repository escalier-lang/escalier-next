module Tests

open FParsec
open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Compiler
open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.Parser
open Escalier.TypeChecker.Prune
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Infer
open Escalier.TypeChecker.Unify

open TestUtils

let infer src =
  result {
    let! ast =
      match run Parser.expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(_s, parserError, _unit) ->
        Result.mapError CompileError.ParseError (Result.Error(parserError))

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/"

    let! t = Result.mapError CompileError.TypeError (inferExpr ctx env ast)

    return simplify t
  }

let inferWithEnv src env =
  result {
    let! ast =
      match run Parser.expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(_s, parserError, _unit) ->
        Result.mapError CompileError.ParseError (Result.Error(parserError))

    let ctx =
      Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

    let! t = Result.mapError CompileError.TypeError (inferExpr ctx env ast)

    return t
  }


[<Fact>]
let InfersLiterals () =
  result {
    let! num = infer "5"
    Assert.Equal("5", num.ToString())

    let! str = infer "\"hello\""
    Assert.Equal("\"hello\"", str.ToString())

    let! bool = infer "true"
    Assert.Equal("true", bool.ToString())

    let! bool' = infer "false"
    Assert.Equal("false", bool'.ToString())
  }

[<Fact>]
let InferSimpleTypeError () =
  let result =
    result {
      let src =
        """
        let y: string = "hello";
        let x: number = y;
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
    }

  Assert.True(Result.isError result)

[<Fact>]
let InferBinaryOperators () =
  let result =
    result {
      let! sum = infer "5 + 10"
      Assert.Equal("15", sum.ToString())

      let! str = infer "\"Hello, \" ++ \"world!\""
      Assert.Equal("\"Hello, world!\"", str.ToString())

      let! lt = infer "5 < 10"
      Assert.Equal("true", lt.ToString())

      let! eq = infer "5 == 10"
      Assert.Equal("boolean", eq.ToString())

      let! b = infer "true || false"
      Assert.Equal("boolean", b.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIfElse () =
  let result =
    result {
      let! t = infer "if (true) { let x = 5;\nx } else { \"hello\" }"
      Assert.Equal("5 | \"hello\"", t.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIfElseChaining () =
  let result =
    result {
      let src =
        """
      let foo = if (true) {
        5
      } else if (false) {
        "hello"
      } else {
        true
      };
      """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "foo", "5 | \"hello\" | true")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIdentifier () =
  let t: Type =
    { Kind = makeTypeRefKind (QualifiedIdent.Ident "number")
      Provenance = None }

  let env =
    { BinaryOps = Map.empty
      UnaryOps = Map.empty
      Values = Map([ ("foo", (t, false)) ])
      Schemes = Map.empty
      Namespaces = Map.empty
      IsAsync = false
      IsPatternMatching = false }

  let result =
    result {
      let! t = inferWithEnv "foo" env
      Assert.Equal("number", t.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLetStatements () =
  let result =
    result {
      let! ctx, env = inferScript "let foo = 5;\nlet bar =\"hello\";"

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "foo", "5")
      Assert.Value(env, "bar", "\"hello\"")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferBinOpsOnPrimitives () =
  let result =
    result {
      let src =
        """
          let x = 5;
          let y = 10;
          let sum = x + y;
          """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "sum", "15")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferTypeDecls () =
  let result =
    result {
      let src =
        """
          type A = number;
          type B = [string, boolean];
          type C = 5 | "hello";
          type D = fn (x: number) -> number;
          type Nullable<T> = T | undefined;
          """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "A", "number")
      Assert.Type(env, "B", "[string, boolean]")
      Assert.Type(env, "C", "5 | \"hello\"")
      Assert.Type(env, "D", "fn (x: number) -> number")
      Assert.Type(env, "Nullable", "<T>(T | undefined)")
    }


  Assert.False(Result.isError result)

[<Fact>]
let InferPrivateDecl () =
  let result =
    result {
      let src =
        """
          let makePoint = fn (x, y) {
            type Point = {x: number, y: number};
            let point: Point = {x, y};
            return point;
          };
          let p = makePoint(5, 10);
          let {x, y} = p;
          """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "makePoint", "fn (x: number, y: number) -> Point")
      Assert.Value(env, "p", "Point")
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferTypeAliasOfPrimtiveType () =
  let result =
    result {
      let src =
        """
        type Bar = number;
        let x: Bar = 5;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "Bar")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferTypeAnn () =
  let result =
    result {
      let src =
        """
        let a: number = 5;
        let [b, c]: [string, boolean] = ["hello", true];
        type Point = {x: number, y: number};
        let {x, y}: Point = {x: 5, y: 10};
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "a", "number")
      Assert.Value(env, "b", "string")
      Assert.Value(env, "c", "boolean")
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferObjectDestructuring () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        let {x, y}: Point = {x: 5, y: 10};
        let p: Point = {x, y};
        let foo = fn ({x, y}: Point) => x + y;
        let sum = foo({x: 5, y: 10});
        foo({x, y});
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "number")
      Assert.Value(env, "p", "Point")
      Assert.Value(env, "sum", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferObjectRestSpread () =
  let result =
    result {
      let src =
        """
        let obj1 = {a: 5, b: "hello", c: true};
        let {a, ...rest} = obj1;
        let obj2 = {a, ...rest};
        let foo = fn({a, ...rest}: {a: number, b: string, c: boolean}) => a;
        foo(obj2);
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "a", "5")
      Assert.Value(env, "rest", "{b: \"hello\", c: true}")
      Assert.Value(env, "obj2", "{a: 5} & {b: \"hello\", c: true}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferObjProps () =
  let result =
    result {
      let src =
        """
        let obj = {a: {b: 5, c: "hello"}};
        let b = obj.a.b;
        let c = obj.a.c;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "b", "5")
      Assert.Value(env, "c", "\"hello\"")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferOptionalChaining () =
  let result =
    result {
      let src =
        """
        type Obj = {a?: {b?: {c: number}}};
        let obj: Obj = {a: {b: undefined}};
        let a = obj.a;
        let b = obj.a?.b;
        let c = obj.a?.b?.c;
        type Point = {x: number, y: number};
        let p: Point = {x: 5, y: 10};
        let x = p?.x;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "obj", "Obj")
      Assert.Value(env, "a", "{b?: {c: number}} | undefined")
      Assert.Value(env, "b", "{c: number} | undefined")
      Assert.Value(env, "c", "number | undefined")
      Assert.Value(env, "p", "Point")
      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferRecursiveType () =
  let result =
    result {

      let src =
        """
        type Foo = number | Foo[];
        let x: Foo = 5;
        let y: Foo = [5, 10];
        let z: Foo = [5, [10, 15]];
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "Foo", "number | Foo[]")
      Assert.Value(env, "x", "Foo")
      Assert.Value(env, "y", "Foo")
      Assert.Value(env, "z", "Foo")
    }

  // printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferRecursiveTypeUnifyWithDefn () =
  let result =
    result {

      let src =
        """
        type Foo = number | Foo[];
        let foo: Foo = 5;
        let bar: number | Foo[] = foo;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "Foo", "number | Foo[]")
    }

  // printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferRecursiveObjectType () =
  let result =
    result {

      let src =
        """
        type Node = {
          value: number,
          left?: Node,
          right?: Node
        };
        
        let node: Node = {
          value: 5,
          left: {
            value: 10
          },
          right: {
            value: 15
          }
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "node", "Node")
    }

  Assert.False(Result.isError result)


[<Fact>]
let InferRecursiveGenericObjectType () =
  let result =
    result {
      let src =
        """
        type Node<T> = {
          value: T,
          left?: Node<T>,
          right?: Node<T>
        };

        let node: Node<number> = {
          value: 5,
          left: {
            value: 10
          },
          right: {
            value: 15,
            left: {
              value: 20
            }
          }
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "node", "Node<number>")
    }

  Assert.False(Result.isError result)

[<Fact>]
let ReturnRecursivePrivateGenericObjectType () =
  let result =
    result {
      let src =
        """
        let makeTree = fn () {
          type Node<T> = {
            value: T,
            left?: Node<T>,
            right?: Node<T>
          };

          let node: Node<number> = {
            value: 5,
            left: {
              value: 10
            },
            right: {
              value: 15
            }
          };
          
          return node;
        };
        let node = makeTree();
        let x = node.left?.value;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "node", "Node<number>")
      Assert.Value(env, "x", "number | undefined")
    }

  Assert.False(Result.isError result)

[<Fact>]
let ReturnRecursiveGenericObjectType () =
  let result =
    result {
      let src =
        """
        type Node<T> = {
          value: T,
          left?: Node<T>,
          right?: Node<T>
        };
        
        let makeTree = fn () {
          let node: Node<number> = {
            value: 5,
            left: {
              value: 10
            },
            right: {
              value: 15
            }
          };
          
          return node;
        };
        let node = makeTree();
        let x = node.left?.value;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "node", "Node<number>")
      Assert.Value(env, "x", "number | undefined")
    }

  Assert.False(Result.isError result)

[<Fact(Skip = "TODO: unify tuples and arrays")>]
let InferTuple () =
  let result =
    result {
      let src =
        """
        let foo = [1, 2, 3];
        let bar = fn(nums: number[]) => nums;
        let baz = bar(foo);
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "foo", "[1, 2, 3]")
      Assert.Value(env, "bar", "fn (nums: number[]) -> number[]")
      Assert.Value(env, "baz", "number[]")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferDeclare () =
  let result =
    result {
      let src = "declare let [x, y]: [number, string, boolean];"

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "string")
    }


  Assert.False(Result.isError result)

[<Fact>]
let InferTemplateLiteralType () =
  let result =
    result {

      let src =
        """
        type Foo = `foo${number}`;
        let x: Foo = "foo123";
        type Bar = `A${string}B`;
        let y: Bar = "A1B";
        type Baz = `A${string}B${string}C`;
        let z: Baz = "A1B2C";
        let w: Baz = "ABCBC";
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "Foo")
      Assert.Value(env, "y", "Bar")
      Assert.Value(env, "z", "Baz")
      Assert.Value(env, "w", "Baz")
    }


  Assert.False(Result.isError result)

[<Fact>]
let InferTemplateLiteralTypeWithUnions () =
  let result =
    result {

      let src =
        """
        type Dir = `${"top" | "bottom"}-${"left" | "right"}`;
        let a: Dir = "top-left";
        let b: Dir = "top-right";
        let c: Dir = "bottom-right";
        let d: Dir = "bottom-left";
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "a", "Dir")
      Assert.Value(env, "b", "Dir")
      Assert.Value(env, "c", "Dir")
      Assert.Value(env, "d", "Dir")
    }

  Assert.False(Result.isError result)

[<Fact>]
let ParseTemplateLiteralType () =
  let mutable parser: Parser<unit, unit> = eof
  parser <- (pstring "C" |>> ignore) .>> parser
  parser <- many (notFollowedBy parser >>. anyChar) |>> ignore
  parser <- (pstring "B" |>> ignore) .>> parser
  parser <- many (notFollowedBy parser >>. anyChar) |>> ignore
  parser <- (pstring "A" |>> ignore) .>> parser

  let result = run parser "ABCBC"

  match result with
  | Success(value, _, _) -> printfn "value = %A" value
  | Failure(s, _, _) -> printfn "s = %A" s

[<Fact>]
let ParseTemplateLiteralTypeWithUnions () =
  let mutable parser: Parser<unit, unit> = eof
  parser <- (pstring "right" <|> pstring "left" |>> ignore) .>> parser
  parser <- (pstring "-" |>> ignore) .>> parser
  parser <- (pstring "top" <|> pstring "bottom" |>> ignore) .>> parser

  let result = run parser "top-left"

  match result with
  | Success(value, _, _) -> printfn "value = %A" value
  | Failure(s, _, _) -> printfn "s = %A" s

[<Fact>]
let InferTemplateLiteralTypeError () =
  let result =
    result {
      let src =
        """
        type Foo = `foo${number}`;
        let x: Foo = "foo123abc";
        """

      let! _, _ = inferScript src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let InferTemplateLiteralTypeErrorWithUnion () =
  let result =
    result {
      let src =
        """
        type Dir = `${"top" | "bottom"}-${"left" | "right"}`;
        let x: Dir = "top-bottom";
        """

      let! _, _ = inferScript src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let InferUnaryOperations () =
  let result =
    result {

      let src =
        """
        let x = 5;
        let y = -x;
        let z = !x;
        let w = +x;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "-5")
      Assert.Value(env, "z", "!5")
      Assert.Value(env, "w", "+5")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferEnum () =
  let result =
    result {
      let src =
        """
        enum MyEnum {
          | Foo(number, string, boolean)
          | Bar([number, number])
          | Baz(number | string)
        }
        let value = MyEnum.Foo(5, "hello", true);
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Type(
        env,
        "MyEnum",
        "Foo(number, string, boolean) | Bar([number, number]) | Baz((number | string))"
      )

      // TODO: how do we include `MyEnum.` in the type?
      // What does this mean in the context of creating new enums from existings enums
      Assert.Value(env, "value", "Foo(number, string, boolean)")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferEnumVariantIsSubtypeOfEnum () =
  let result =
    result {
      let src =
        """
        enum MyEnum {
          | Foo(number, string, boolean)
          | Bar([number, number])
          | Baz(number | string)
        }
        let value: MyEnum = MyEnum.Foo(5, "hello", true);
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Type(
        env,
        "MyEnum",
        "Foo(number, string, boolean) | Bar([number, number]) | Baz((number | string))"
      )

      Assert.Value(env, "value", "MyEnum")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferGenericEnum () =
  let result =
    result {
      let src =
        """
        enum MyEnum<A, B, C> {
          | Foo(A)
          | Bar(B)
          | Baz(C)
        }
        let value = MyEnum.Foo(5);
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "MyEnum", "<A, B, C>(Foo(A) | Bar(B) | Baz(C))")

      // TODO: how do we include `MyEnum.` in the type?
      // What does this mean in the context of creating new enums from existings enums
      Assert.Value(env, "value", "Foo(5)")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferGenericEnumWithSubtyping () =
  let result =
    result {
      let src =
        """
        enum MyEnum<A, B, C> {
          | Foo(A)
          | Bar(B)
          | Baz(C)
        }
        let value: MyEnum<number, string, boolean> = MyEnum.Foo(5);
        let x = match value {
          | MyEnum.Foo(a) => a
          | MyEnum.Bar(b) => b
          | MyEnum.Baz(c) => c
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Type(env, "MyEnum", "<A, B, C>(Foo(A) | Bar(B) | Baz(C))")
      Assert.Value(env, "value", "MyEnum<number, string, boolean>")
      Assert.Value(env, "x", "number | string | boolean")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferEnumPatternMatching () =
  let result =
    result {
      let src =
        """
        enum MyEnum {
          | Foo(number, string, boolean)
          | Bar([number, number])
          | Baz({x: number, y: number})
        }
        let value: MyEnum = MyEnum.Foo(5, "hello", true);

        let x = match value {
          | MyEnum.Foo(x, y, z) => x
          | MyEnum.Bar([x, y]) => x
          | MyEnum.Baz({x, y}) => x
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Type(
        env,
        "MyEnum",
        "Foo(number, string, boolean) | Bar([number, number]) | Baz({x: number, y: number})"
      )

      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIfLet () =
  let result =
    result {
      let src =
        """
        declare let value: number | undefined;
        let y = if let x = value {
          x + 1;
        } else {
          0;
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "y", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIfLetWithTypeAlias () =
  let result =
    result {
      let src =
        """
        type Nullable<T> = T | undefined;
        declare let value: Nullable<number>;
        let y = if let x = value {
          x + 1;
        } else {
          0;
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "y", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIfLetWithShadowing () =
  let result =
    result {
      let src =
        """
        declare let x: number | undefined;
        let y = if let x = x {
          x + 1;
        } else {
          0;
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      // TODO: fix this shadowing issue
      Assert.Value(env, "y", "t3:number + 1 | 0")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIfLetWithDestructuring () =
  let result =
    result {
      let src =
        """
        declare let point: {x: number, y: number} | undefined;
        let sum = if let {x, y} = point {
          x + y;
        } else {
          0;
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "sum", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIfLetWithChaining () =
  let result =
    result {
      // TODO: figure out why changing `y;` to `y` causes a parse error
      let src =
        """
        declare let value: number | string | undefined;
        let result = if let x is number = value {
          x + 1;
        } else if let y is string = value {
          y;
        } else {
          0;
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "result", "number | string")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLetElse () =
  let result =
    result {
      let src =
        """
        declare let value: number | undefined;
        let x = value else {
          0;
        };
        let y = x + 1;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "y", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLetElseEarlyReturn () =
  let result =
    result {
      let src =
        """
        let foo = fn (x: number | undefined) {
          let y = x else {
            return 0;
          };
          return y + 1;
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "foo", "fn (x: number | undefined) -> number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLetElseWithDestructuring () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        type Line = {start: Point, end: Point};
        declare let value: Point | Line | undefined;
        declare let print: fn (msg: string) -> undefined;
        let {x, y} = value else {
          {x: 0, y: 0};
        };
        let sum = x + y;
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "sum", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLetElseIsPattern () =
  let result =
    result {
      let src =
        """
        declare let value: number | string | undefined;
        declare let print: fn (msg: string) -> undefined;
        let x is number = value else {
          0;
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLetElseWithTypeAnnotation () =
  let result =
    result {
      let src =
        """
        declare let value: number | string | undefined;
        declare let print: fn (msg: string) -> undefined;
        let x: number = value else {
          0;
        };
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)
      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferClassWithMethods () =
  let result =
    result {
      let src =
        """
        let Foo = class {
          msg: string;
          fn bar(self) {
            return self.msg;
          }
          fn baz(mut self, msg: string) {
            self.msg = msg;
          }
        };
        let foo = new Foo();
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Type(
        env,
        "Foo",
        "{bar fn (self: Self) -> string, baz fn (mut self: Self, msg: string) -> undefined, msg: string}"
      )

      Assert.Value(env, "foo", "Foo")

      let fooType, _ = Map.find "foo" env.Values

      let! fooType =
        expandType ctx env None Map.empty fooType
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar fn (self: Self) -> string, baz fn (mut self: Self, msg: string) -> undefined, msg: string}",
        fooType.ToString()
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferClassWithTypeParams () =
  let result =
    result {
      let src =
        """
        let Foo = class<T> {
          bar: T;
          fn map<U>(self, callback: fn (bar: T) -> U) {
            return callback(self.bar);
          }
        };
        let foo = new Foo<string>();
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Type(
        env,
        "Foo",
        "<T>({map fn <U>(self: Self, callback: fn (bar: T) -> U) -> U, bar: T})"
      )

      Assert.Value(env, "foo", "Foo<string>")

      let fooType, _ = Map.find "foo" env.Values

      let! fooType =
        expandType ctx env None Map.empty fooType
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{map fn <U>(self: Self, callback: fn (bar: string) -> U) -> U, bar: string}",
        fooType.ToString()
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferClassWithFluentMethods () =
  let result =
    result {
      let src =
        """
        let Foo = class {
          msg: string;
          fn bar(self) -> Self {
            return self;
          }
        };
        let foo = new Foo();
        let bar = foo.bar();
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Type(env, "Foo", "{bar fn (self: Self) -> Self, msg: string}")
      Assert.Value(env, "foo", "Foo")
      Assert.Value(env, "bar", "Foo")

      // let barType, _ = Map.find "bar" env.Values
      //
      // printfn "barType = %A" barType

      let fooType, _ = Map.find "foo" env.Values

      let! fooType =
        expandType ctx env None Map.empty fooType
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar fn (self: Self) -> Self, msg: string}",
        fooType.ToString()
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferClassWithFluentMethodsWithoutTypeAnn () =
  let result =
    result {
      let src =
        """
        let Foo = class {
          msg: string;
          fn bar(self) {
            return self;
          }
        };
        let foo = new Foo();
        let bar = foo.bar();
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Type(env, "Foo", "{bar fn (self: Self) -> Self, msg: string}")
      Assert.Value(env, "foo", "Foo")
      Assert.Value(env, "bar", "Foo")

      // let barType, _ = Map.find "bar" env.Values
      //
      // printfn "barType = %A" barType

      let fooType, _ = Map.find "foo" env.Values

      let! fooType =
        expandType ctx env None Map.empty fooType
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar fn (self: Self) -> Self, msg: string}",
        fooType.ToString()
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferClassWithFluentMethodsWithoutTypeAnnWithTypeParam () =
  let result =
    result {
      let src =
        """
        let Foo = class<T> {
          msg: T;
          fn bar(self) {
            return self;
          }
        };
        let foo = new Foo<string>();
        let bar = foo.bar();
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Type(env, "Foo", "<T>({bar fn (self: Self) -> Self, msg: T})")
      Assert.Value(env, "foo", "Foo<string>")
      Assert.Value(env, "bar", "Foo<string>")

      // let barType, _ = Map.find "bar" env.Values
      //
      // printfn "barType = %A" barType

      let fooType, _ = Map.find "foo" env.Values

      let! fooType =
        expandType ctx env None Map.empty fooType
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar fn (self: Self) -> Self, msg: string}",
        fooType.ToString()
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferClassMethodsThatTakeOtherSelf () =
  let result =
    result {
      let src =
        """
        let Point = class {
          x: number;
          y: number;
          fn add(self, other: Self) {
            return {x: self.x + other.x, y: self.y + other.y};
          }
        };
        let p1 = new Point();
        let p2 = new Point();
        let p3 = p1.add(p2);
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Value(env, "p1", "Point")
      Assert.Value(env, "p2", "Point")
      Assert.Value(env, "p3", "{x: number, y: number}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferClassMethodsThatCallsTheConstructor () =
  let result =
    result {
      let src =
        """
        let Point = class {
          x: number;
          y: number;
          fn makePoint() {
            let p = new Self();
            return p;
          }
        };
        let p = Point.makePoint();
        """

      let! ctx, env = inferScript src

      Assert.Empty(ctx.Diagnostics)

      Assert.Value(env, "p", "Point")
    }

  Assert.False(Result.isError result)

// TODO: handle explicit constructors
