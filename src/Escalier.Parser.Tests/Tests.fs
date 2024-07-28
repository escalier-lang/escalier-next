[<VerifyXunit.UsesVerify>]
module Tests

open Xunit
open VerifyTests
open VerifyXunit
open FParsec

open Escalier.Parser

let lit = run (Parser.lit .>> eof)
let typeAnn = run (Parser.typeAnn .>> eof)

let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

[<Fact>]
let ParseArithmetic () =
  let src = "0.1 + 2 * (3 - 4) / -5.6;"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseString () =
  let src = """let msg = "Hello,\n\t\"world!\"";"""
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseOtherLiterals () =
  let src =
    """
    let a = undefined;
    let b = null;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTemplateLiteral () =
  let src = """let msg = `foo ${`bar ${baz}`}`;"""
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTaggedTemplateLiteral () =
  let src = """let foo = gql`query { hello }`;"""
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCall () =
  let src = "add(x, y);"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCallExtraSpaces () =
  let src = "add( x , y );"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseEmptyCall () =
  let src = "add();"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseExprWithTypeParam () =
  let src = "add<number | string>;"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseCallWithTypeParam () =
  let src = "add<number>();"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseConstructorCall () =
  let src = "new Array();"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseConstructorCallWithTypeParam () =
  let src = "new Array<number>();"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexer () =
  let src = "array[0];"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMultipleIndexers () =
  let src = "array[0][1];"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexerThenCall () =
  let src = "array[0]();"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseCallThenIndexer () =
  let src = "foo()[0];"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjProperty () =
  let src = "obj.a.b;"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjPropWithOptChain () =
  let src = "obj?.a?.b;"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncExpr () =
  let src = "fn (x, y) { x };"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncExprWithTypes () =
  let src = "fn (x: number) -> number throws \"RangeError\" { x };"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncExprWithTypeParams () =
  let src = "fn <T: Foo = Bar>(x: T) -> T { x };"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncDecl () =
  let src = "fn fst<T>(x: T, y: T) -> T { return x; }"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTuple () =
  let src = "[1, 2, 3];"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionType () =
  let src = "number | string | boolean"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionTypeWithLiterals () =
  let src = "5 | \"hello\""
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIntersectionType () =
  let src = "number & string & boolean"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionAndIntersectionType () =
  let src = "A & B | C & D"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseArrayType () =
  let src = "number[][]"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseParenthesizedType () =
  let src = "(number | string)[]"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionType () =
  let src = "fn <T: Foo = Bar>(x: T) -> T throws \"RangeError\""
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjLitAndObjPat () =
  let src =
    """
    type Point = {x: number, y: number};
    let {x, y}: Point = {x: 5, y: 10};
    let p: Point = {x, y};
    let foo = fn ({x, y}: Point) => x + y;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjRestSpread () =
  let src =
    """
    let obj = {a: 5, b: "hello", c: true};
    let {a, ...rest} = obj;
  """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseOptionalProps () =
  let src =
    """
    type Obj = {a?: {b?: {c?: number}}};
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseOptionalParams () =
  let src =
    """
    let foo = fn(a?: number, b?: string) => a;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseArrowIdentifier () =
  let src = "let fst = fn (x, y) => x;"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseImports () =
  let src =
    """
    import "./math" {add, sub as subtract};
    import "~/net" as network;
    import "path";
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseConditionalTypeAnn () =
  let src =
    """
    type Exclude<T, U> = if T: U { never } else { T };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseChainedConditionalTypeAnn () =
  let src =
    """
    type Foo<T> = if T: number { "number" } else if T: string { "string" } else { "other" };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMappedTypes () =
  let src = """type Point = {[P]: number for P in "x" | "y"};"""
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexAccessTypes () =
  let src = "type Foo = Bar[\"baz\"];"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseNamespacedType () =
  let src = "type Foo = Intl.NumberFormat;"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseNamespacedValue () =
  let src = """let fmt = new Intl.NumberFormat("en-CA");"""
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseAsyncFunc () =
  let src =
    """
    let bar = async fn () {
      let x = await foo();
      return x + 10;
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseThrowAndTryCatch () =
  let src =
    """
    let foo = fn (x) {
      if x < 0 {
        throw "x must be positive";
      }
      return x;
    };
    let bar = fn (x) {
      let result = try {
        foo(x);
      } catch {
        | _ => 0
      };
      return result;
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParsePatternMatching () =
  let src =
    """
    let foo = fn (x) =>
      match x {
        0 => "none",
        1 => "one",
        n if n < 0 => "negative",
        _ => "other",
      };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseComplexPatternMatching () =
  let src =
    """
    match node {
      {x, y: b} => 1,
      {x is number, y: b is number} => 2,
      {x: _, y: _ is number} => 3,
      {type: "circle", center: {x, y}, radius} => 4,
      {x = 0, y: b = 0} => 5,
      {x is number = 0, y: b is number = 0} => 6,
    }
    """

  // TODO: move the default value from the key-value pattern to the ident pattern
  // so that we can do stuff like
  // | {x: a = 0 is number, y: b = 0 is number} => 6
  // when matching something like {x?: number, y?: number}

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseEnum () =
  let src =
    """
    enum MyEnum {
      Foo[number, string, boolean],
      Bar {x: number, y: number},
    }
    let value = MyEnum.Foo(5, "hello", true);
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseOptionEnum () =
  let src =
    """
    enum Option<T> {
      None,
      Some[T],
    }
    let value: Option<string> = Option.Some("hello");
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseGenericEnum () =
  let src =
    """
    enum MyEnum<A, B, C> {
      Foo[A],
      Bar[B],
      Baz[C],
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseEnumPatternMatching () =
  let src =
    """
    match value {
      MyEnum.Foo[a, b, c] => a + b + c,
      MyEnum.Bar[x, y] => x * y,
      MyEnum.Baz[z] => z,
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseCallableType () =
  let src =
    """
    type Callable = {
      new fn () -> symbol,
      fn () -> symbol,
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParsePropKeysInObjectType () =
  let src =
    """
    type Array = {
      5: string,
      "hello": number,
      [Symbol.iterator]: fn () -> Iterator<T>,
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseGetterSetterInObjectType () =
  let src =
    """
    type Obj = {
      get foo() -> string,
      set foo(value: string),
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask


[<Fact>]
let ParseForLoop () =
  let src =
    """
    for x in [1, 2, 3] {
      print(x);
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseDeclare () =
  let src =
    """
    declare let foo: number;
    declare let bar: string;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseRange () =
  let src =
    """
    type DieRoll = 1..6;
    let range = 0..10;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseRangeIteratorType () =
  let src =
    """
      type RangeIterator<Min: number, Max: number> = {
        next: fn () -> { done: boolean, value: Min..Max }
      };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseSimpleRangeType () =
  let src =
    """
      type Range = Min..Max;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseRangeTypeAndQualifiedTypeRefInObject () =
  let src =
    """
      type Obj = {range: Min..Max, qual: Foo.Bar};
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTemplateLiteralType () =
  let src = """type TemplateLiteral = `foo${number}`;"""

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseInferType () =
  let src =
    "type ReturnType<T> = if T: fn (...args: _) -> infer R { R } else { never };"

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseRecordTuple () =
  let src =
    """
    let p0 = #[5, 10];
    let p1 = #{x: 5, y: 10};
    let line = #{p0, p1};
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMutableBindings () =
  let src =
    """
    type Point = {x: number, y: number};
    type Line = {p0: Point, p1: Point};
    declare let line: Line;
    
    let {mut p0, p1: mut q} = line;
    let mut r = q;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMutableParams () =
  let src =
    """
    let update = fn (mut array: number[]) {
      for i in 0..array.length {
        array[i] = array[i] + 1;
      }
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTypeof () =
  let src =
    """
    let iterator: typeof Symbol.iterator = Symbol.iterator;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIfLet () =
  let src =
    """
    if let Option.Some[x] = maybe {
      print(x);
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIfLetElse () =
  let src =
    """
    let y = if let x = value {
      x + 1;
    } else {
      0;
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIfLetChaining () =
  let src =
    """
    if let x = maybe1?.x {
      print(x);
    } else if let Result.Ok[y] = result {
      print(y);
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseLetElse () =
  let src =
    """
    let {x, y} = point else {
      print("point is not a Point");
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseBlockError () =
  let src =
    """
    if cond {
      x
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseClassWithMethods () =
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
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseClassWithConstructor () =
  let src =
    """
    let Foo = class {
      msg: string;
      new (self, msg: string) {
        self.msg = msg;
      }
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseClassWithGetterSetter () =
  let src =
    """
    let Foo = class {
      _msg: string;
      get msg(self) {
        return self._msg;
      }
      set msg(mut self, msg: string) {
        self._msg = msg;
      }
    };
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseBasicJsx () =
  let src =
    """
    let elem = <Foo bar={bar} baz="baz">Hello, {name}</Foo>;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseNestedJsx () =
  let src =
    """
    let elem = <Foo bar={<Bar>Hello</Bar>}><Baz>world</Baz></Foo>;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseSelfClosingTag () =
  let src =
    """
    let elem = <Foo bar={baz} />;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFragment () =
  let src =
    """
    let frag = <><Foo>Hello, world</Foo></>;
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseNamespaceInScript () =
  let src =
    """
    namespace Foo {
      namespace Bar {
        let x = 5;
      }
      let y = 10;
      type Baz = number;
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseNamespaceInModule () =
  let src =
    """
    namespace Foo {
      namespace Bar {
        let x = 5;
      }
      let y = 10;
      type Baz = number;
    }
    """

  let ast = Parser.parseModule src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseInterface () =
  let src =
    """
    interface Point {
      x: number,
    }
    interface Point {
      y: number,
    }
    """

  let ast = Parser.parseModule src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseInterfaceWithTypeParam () =
  let src =
    """
    interface Array<T> {
      [K]: T for K in number,
    }
    """
  // fn map<T>(callback: fn(x: T) -> U) -> Array<U>,

  let ast = Parser.parseModule src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask
