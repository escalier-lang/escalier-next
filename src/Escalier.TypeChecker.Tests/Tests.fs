module Tests

open Escalier.Data.Common
open FParsec
open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Prune
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

    let projectRoot = __SOURCE_DIRECTORY__
    let! ctx, env = Prelude.getEnvAndCtx projectRoot

    let! t = Result.mapError CompileError.TypeError (inferExpr ctx env None ast)

    return simplify t
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

      let! ctx, env = inferModule src

      printDiagnostics ctx.Report.Diagnostics
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "5 | \"hello\" | true")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLetStatements () =
  let result =
    result {
      let! ctx, env = inferModule "let foo = 5;\nlet bar =\"hello\";"

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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
          let makePoint = fn (x: number, y: number) {
            type Point = {x: number, y: number};
            let point: Point = {x, y};
            return point;
          };
          let p = makePoint(5, 10);
          let {x, y} = p;
          """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "number")
      Assert.Value(env, "b", "string")
      Assert.Value(env, "c", "boolean")
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "number")
    }

  Assert.False(Result.isError result)

[<Fact(Skip = "TODO(#287): Excess property checking")>]
let InferObjectExcessPropertyCheck () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        let p: Point = {x: 5, y: 10, z: 15};
        """

      let! ctx, env = inferModule src

      Assert.Equal<Diagnostic list>(
        ctx.Report.Diagnostics,
        [ { Description = "Excess property 'z' in object literal"
            Reasons = [] } ]
      )

      Assert.Value(env, "p", "Point")
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "number")
      // Assert.Value(env, "p", "Point")
      Assert.Value(env, "sum", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferObjectRest () =
  let result =
    result {
      let src =
        """
        let obj1 = {a: 5, b: "hello", c: true};
        let {a, ...rest} = obj1;
        """

      let! ctx, env = inferModule src

      let rest, _ = env.Namespace.Values["rest"]

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "5")
      Assert.Value(env, "rest", "{b: \"hello\", c: true}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferObjectRestWithTypeAnnotations () =
  let result =
    result {
      let src =
        """
        let obj1 = {a: 5, b: "hello", c: true};
        let {a, ...rest}: {a: number, b: string, c: boolean} = obj1;
        """

      let! ctx, env = inferModule src

      let rest, _ = env.Namespace.Values["rest"]

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "number")
      Assert.Value(env, "rest", "{b: string, c: boolean}")
    }

  printfn "result = %A" result
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "5")
      Assert.Value(env, "rest", "{b: \"hello\", c: true}")
      Assert.Value(env, "obj2", "{a: 5, b: \"hello\", c: true}")
    }

  printfn "result = %A" result
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "b", "5")
      Assert.Value(env, "c", "\"hello\"")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let DontIncludeSettersInRvalues () =
  let result =
    result {
      let src =
        """
        type Obj = {
          get foo() -> string,
          set bar(value: number),
        };
        declare let obj: Obj;
        let foo = obj.foo;
        let bar = obj.bar;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  printfn "result = %A" result

  Assert.Equal(
    result,
    Result.Error(
      Compiler.CompileError.TypeError(
        TypeError.SemanticError "Property bar not found"
      )
    )
  )

[<Fact>]
let DontIncludeSettersInRvaluesDestructuring () =
  let result =
    result {
      let src =
        """
        type Obj = {
          get foo() -> string,
          set bar(value: number),
        };
        declare let obj: Obj;
        let {foo, bar} = obj;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  Assert.Equal(
    result,
    Result.Error(
      Compiler.CompileError.TypeError(
        TypeError.PropertyMissing(Escalier.Data.Type.PropName.String "bar")
      )
    )
  )

[<Fact>]
let DontIncludeGettersInLvalues () =
  let result =
    result {
      let src =
        """
        type Obj = {
          get foo() -> string,
          set bar(value: number),
        };
        declare let mut obj: Obj;
        obj.bar = 5;
        obj.foo = "hello";
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  printfn "result = %A" result

  Assert.Equal(
    result,
    Result.Error(
      Compiler.CompileError.TypeError(
        TypeError.SemanticError "Property bar not found"
      )
    )
  )

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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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
        type MyNode = {
          value: number,
          left?: MyNode,
          right?: MyNode
        };
        
        let node: MyNode = {
          value: 5,
          left: {
            value: 10
          },
          right: {
            value: 15
          }
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "node", "MyNode")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)


[<Fact>]
let InferRecursiveGenericObjectType () =
  let result =
    result {
      let src =
        """
        type MyNode<T> = {
          value: T,
          left?: MyNode<T>,
          right?: MyNode<T>
        };

        let node: MyNode<number> = {
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "node", "MyNode<number>")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferRecursiveGenericObjectTypeInModule () =
  let result =
    result {
      let src =
        """
        type MyNode<T> = {
          value: T,
          left?: MyNode<T>,
          right?: MyNode<T>
        };

        let node: MyNode<number> = {
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "node", "MyNode<number>")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let ReturnRecursivePrivateGenericObjectType () =
  let result =
    result {
      let src =
        """
        let makeTree = fn () {
          type MyNode<T> = {
            value: T,
            left?: MyNode<T>,
            right?: MyNode<T>
          };

          let node: MyNode<number> = {
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "node", "MyNode<number>")
      Assert.Value(env, "x", "number | undefined")
    }

  Assert.False(Result.isError result)

[<Fact>]
let ReturnRecursiveGenericObjectType () =
  let result =
    result {
      let src =
        """
        type MyNode<T> = {
          value: T,
          left?: MyNode<T>,
          right?: MyNode<T>
        };
        
        let makeTree = fn () {
          let node: MyNode<number> = {
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "node", "MyNode<number>")
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "string")
    }

  printfn "result = %A" result
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! _, _ = inferModule src
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

      let! _, _ = inferModule src
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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
          Foo[number, string, boolean],
          Bar[[number, number]],
          Baz[number | string],
        }
        let value = MyEnum.Foo(5, "hello", true);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Type(
        env,
        "MyEnum",
        "{readonly __TAG__: unique symbol} & [number, string, boolean] | {readonly __TAG__: unique symbol} & [[number, number]] | {readonly __TAG__: unique symbol} & [number | string]"
      )

      // TODO: how do we include `MyEnum.` in the type?
      // What does this mean in the context of creating new enums from existings enums
      Assert.Value(
        env,
        "value",
        "{readonly __TAG__: unique symbol} & [number, string, boolean]"
      )
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferEnumVariantIsSubtypeOfEnum () =
  let result =
    result {
      let src =
        """
        enum MyEnum {
          Foo[number, string, boolean],
          Bar[[number, number]],
          Baz[number | string],
        }
        let value: MyEnum = MyEnum.Foo(5, "hello", true);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Type(
        env,
        "MyEnum",
        "{readonly __TAG__: unique symbol} & [number, string, boolean] | {readonly __TAG__: unique symbol} & [[number, number]] | {readonly __TAG__: unique symbol} & [number | string]"
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
          Foo[A],
          Bar[B],
          Baz[C],
        }
        let value = MyEnum.Foo(5);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! enumType =
        env.GetScheme(QualifiedIdent.Ident "MyEnum")
        |> Result.mapError CompileError.TypeError

      printfn $"enumType = {enumType}"

      Assert.Type(
        env,
        "MyEnum",
        "<A, B, C>({readonly __TAG__: unique symbol} & [A] | {readonly __TAG__: unique symbol} & [B] | {readonly __TAG__: unique symbol} & [C])"
      )

      // TODO: how do we include `MyEnum.` in the type?
      // What does this mean in the context of creating new enums from existings enums
      Assert.Value(env, "value", "{readonly __TAG__: unique symbol} & [5]")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferGenericEnumWithSubtyping () =
  let result =
    result {
      let src =
        """
        enum MyEnum<A, B, C> {
          Foo[A],
          Bar[B],
          Baz[C],
        }
        let value: MyEnum<number, string, boolean> = MyEnum.Foo(5);
        let x = match value {
          MyEnum.Foo[a] => a,
          MyEnum.Bar[b] => b,
          MyEnum.Baz[c] => c,
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! enumType =
        env.GetScheme(QualifiedIdent.Ident "MyEnum")
        |> Result.mapError CompileError.TypeError

      printfn $"enumType = {enumType}"

      Assert.Type(
        env,
        "MyEnum",
        "<A, B, C>({readonly __TAG__: unique symbol} & [A] | {readonly __TAG__: unique symbol} & [B] | {readonly __TAG__: unique symbol} & [C])"
      )

      Assert.Value(env, "value", "MyEnum<number, string, boolean>")
      Assert.Value(env, "x", "number | string | boolean")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferEnumPatternMatching () =
  let result =
    result {
      let src =
        """
        enum MyEnum {
          Foo[number, string, boolean],
          Bar[[number, number]],
          Baz {x: number, y: number},
        }
        let value: MyEnum = MyEnum.Foo(5, "hello", true);

        let x = match value {
          MyEnum.Foo[x, y, z] => x,
          MyEnum.Bar[[x, y]] => x,
          MyEnum.Baz {x, y} => x,
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! enumType =
        env.GetScheme(QualifiedIdent.Ident "MyEnum")
        |> Result.mapError CompileError.TypeError

      printfn $"enumType = {enumType}"

      Assert.Type(
        env,
        "MyEnum",
        "{readonly __TAG__: unique symbol} & [number, string, boolean] | {readonly __TAG__: unique symbol} & [[number, number]] | {readonly __TAG__: unique symbol} & {x: number, y: number}"
      )

      Assert.Value(env, "x", "number")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferGenericEnumPatternMatching () =
  let result =
    result {
      let src =
        """
        enum Option<T> {
          Some[T],
          None,
        }
        declare let value: Option<number>;

        let x = match value {
          Option.Some[value] => value,
          Option.None => 0,
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      let! enumType =
        env.GetScheme(QualifiedIdent.Ident "Option")
        |> Result.mapError CompileError.TypeError

      printfn $"enumType = {enumType}"

      Assert.Type(
        env,
        "Option",
        "<T>({readonly __TAG__: unique symbol} & [T] | {readonly __TAG__: unique symbol})"
      )

      Assert.Value(env, "x", "number")
    }

  printfn "result = %A" result
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      // TODO: fix this shadowing issue
      Assert.Value(env, "y", "t6:number + 1 | 0")
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn (x: number | undefined) -> number")
    }

  printfn "result = %A" result
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferNamespaceInScript () =
  let result =
    result {
      let src =
        """
        namespace Foo {
          namespace Bar {
            let x = 5;
          }
          let y = Bar.x;
          type Baz = string;
        }
        let x = Foo.Bar.x;
        let y = Foo.y;
        type Baz = Foo.Baz;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
      Assert.Type(env, "Baz", "Foo.Baz")

      let! t =
        expandScheme ctx env None (env.FindScheme "Baz") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "string")
    }

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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "5")
      Assert.Type(env, "Baz", "Foo.Baz")

      let! t =
        expandScheme ctx env None (env.FindScheme "Baz") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "string")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferMutuallyRecursiveNamespaces () =
  let result =
    result {
      let src =
        """
        namespace Foo {
          type Baz = string;
          type Qux = GlobalQux;
        }
        namespace Bar {
          type Baz = GlobalBaz;
          type Qux = number;
        }
        let x: Foo.Baz = "hello";
        let y: Bar.Qux = 5;
        type GlobalQux = Bar.Qux;
        type GlobalBaz = Foo.Baz;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "x", "Foo.Baz")
      Assert.Value(env, "y", "Bar.Qux")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferInterfaceInScript () =
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
        let p: Point = {x: 5, y: 10};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "Point", "{x: number, y: number}")
      Assert.Value(env, "p", "Point")
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "Point", "{x: number, y: number}")
      Assert.Value(env, "p", "Point")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferTypeParamInIntersection () =
  let result =
    result {
      let src =
        """
        declare fn foo<T: {...}>(props: T & {x: number, ...}) -> T;
        let bar = foo({x: 5, y: "hello", z: true});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "foo", "fn <T: {}>(props: T & {x: number}) -> T")
      Assert.Value(env, "bar", "{y: \"hello\", z: true}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact(Skip = "TODO")>]
let UnifyIntersectionTypesWithIntersectionTypes () =
  let result =
    result {
      let src =
        """
        fn foo<T: {}>(props: T & {x: number}) -> T {
          const {x, ...rest} = props;
          return rest;
        };
        let bar = foo({x: 5, y: "hello", z: true});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "foo", "fn <T: {}>(props: T & {x: number}) -> T")
      Assert.Value(env, "bar", "{y: \"hello\", z: true}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let ExpandSchemeDoesNotExpandValuesInObjectType () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        type Line = {p: Point, q: Point};
        """

      let! ctx, env = inferModule src

      let! t =
        expandScheme ctx env None (env.FindScheme "Line") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "{p: Point, q: Point}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let ExpandSchemeDoesNotExpandValuesInObjectTypeWithGenerics () =
  let result =
    result {
      let src =
        """
        type Point<T> = {x: T, y: T};
        type Line<T> = {p: Point<T>, q: Point<T>};
        declare let line: Line<number>; 
        """

      let! ctx, env = inferModule src

      let t, _ = env.FindValue "line"

      let! t =
        expandType ctx env None Map.empty t
        |> Result.mapError CompileError.TypeError

      Assert.Equal(t.ToString(), "{p: Point<number>, q: Point<number>}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferSpreadTypeAnn () =
  let result =
    result {
      let src =
        """
        type Foo = {foo: number};
        type Bar = {bar: string};
        type FooBar = {...Foo, ...Bar};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "FooBar", "{...Foo, ...Bar}")

      let! t =
        expandScheme ctx env None (env.FindScheme "FooBar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{bar: string, foo: number}", t.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferSpreadTypeAnnWithOverlap () =
  let result =
    result {
      let src =
        """
        type Foo = {a: number, b: number};
        type Bar = {b: string, c: string};
        type FooBar = {...Foo, ...Bar};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "FooBar", "{...Foo, ...Bar}")

      let! t =
        expandScheme ctx env None (env.FindScheme "FooBar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: number, b: string, c: string}", t.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferSpreadTypeAnnWithMappedOverlap () =
  let result =
    result {
      let src =
        """
        type Foo = {[P]: number for P in "a" | "b"};
        type Bar = {[P]: string for P in "b" | "c"};
        type FooBar = {...Foo, ...Bar};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "FooBar", "{...Foo, ...Bar}")

      let! t =
        expandScheme ctx env None (env.FindScheme "FooBar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: number, b: string, c: string}", t.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferSpreadTypeAnnWithOptionalOverlap () =
  let result =
    result {
      let src =
        """
        type Foo = {a: number, b: number};
        type Bar = {b?: string, c: string};
        type FooBar = {...Foo, ...Bar};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "Bar", "{b?: string, c: string}")
      Assert.Type(env, "FooBar", "{...Foo, ...Bar}")

      let! t =
        expandScheme ctx env None (env.FindScheme "FooBar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: number, b: number | string, c: string}", t.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferSpreadTypeAnnWithOptionalOverlapMapped () =
  let result =
    result {
      let src =
        """
        type Foo = {[P]: number for P in "a" | "b"};
        type Bar = {[P]?: string for P in "b" | "c"};
        type FooBar = {...Foo, ...Bar};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "Bar", "{[P]+?: string for P in \"b\" | \"c\"}")
      Assert.Type(env, "FooBar", "{...Foo, ...Bar}")

      let! t =
        expandScheme ctx env None (env.FindScheme "FooBar") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: number, b: number | string, c?: string}", t.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferKeyofTuple () =
  let result =
    result {
      let src =
        """
        type Tuple = [string, number];
        type Keys = keyof Tuple;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "Keys", "keyof Tuple")

      let! t =
        expandScheme ctx env None (env.FindScheme "Keys") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("0 | 1", t.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)


[<Fact>]
let InferMappedTupleType () =
  let result =
    result {
      let src =
        """
        type Foo<T> = {
          [K]: T[K][] for K in keyof T
        };
        type Bar = [string, number];
        type Baz = Foo<Bar>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "Baz", "Foo<Bar>")

      let! t =
        expandScheme ctx env None (env.FindScheme "Baz") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{0: string[], 1: number[]}", t.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferMappedArrayType () =
  let result =
    result {
      let src =
        """
        type Foo<T> = {
          [K]: T[K][] for K in keyof T
        };
        type Bar = string[];
        type Baz = Foo<Bar>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "Baz", "Foo<Bar>")

      let! t =
        expandScheme ctx env None (env.FindScheme "Baz") Map.empty None
        |> Result.mapError CompileError.TypeError

      // TODO: this should be string[][]
      Assert.Equal("{[K]: string[] for K in number}", t.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferMappedObjectType () =
  let result =
    result {
      let src =
        """
        type Foo<T> = {
          [K]: T[K][] for K in keyof T
        };
        type Bar = {a: string, b: number};
        type Baz = Foo<Bar>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Type(env, "Baz", "Foo<Bar>")

      let! t =
        expandScheme ctx env None (env.FindScheme "Baz") Map.empty None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("{a: string[], b: number[]}", t.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)
