module Functions

open FsToolkit.ErrorHandling
open Xunit

open TestUtils


[<Fact>]
let InferCallFuncWithSingleWrongArg () =
  let result =
    result {
      let src =
        """
        let add = fn <A: number, B: number>(x: A, y: B) => {value: x + y};
        let sum = add("hello", "world");
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 2) // can't pass strings to function expecting numbers
      // TODO: simplify all binary types inside a complex type
      Assert.Value(env, "sum", "{value: number + number}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferCallFuncWithWrongArgs () =
  let result =
    result {
      let src =
        """
        let add = fn <A: number, B: number>(x: A, y: B) => x + y;
        let sum = "hello" + "world";
        """

      let! ctx, env = inferModule src

      Assert.Equal(2, ctx.Report.Diagnostics.Length) // can't add strings with `+`
      printfn $"diagnostics: %A{ctx.Report.Diagnostics}"
      Assert.Value(env, "sum", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferCallGenericFuncWithWrongArg () =
  let result =
    result {
      let src =
        """
        let foo = fn <T: number>(x: T) -> T => x;
        let bar = foo("bar");
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1) // foo("bar") is an error
      Assert.Value(env, "bar", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferCallGenericFuncWithComplexReturnAndWrongArg () =
  let result =
    result {
      let src =
        """
        let foo = fn <T: number>(x: T) => {value: x};
        let bar = foo("hello");
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1) // foo("hello") is an error
      Assert.Value(env, "bar", "{value: number}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncTypeAnnotation () =
  let result =
    result {
      let src =
        """
        let foo: fn <T: number>(x: T) -> T = fn (x) => x;
        let x = foo(5);
        let y = foo("hello");
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1) // foo("hello") is an error
      Assert.Value(env, "foo", "fn <T: number>(x: T) -> T")
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let PassingTooManyArgsIsOkay () =
  let result =
    result {
      let src =
        """
        let add = fn <A: number, B: number>(x: A, y: B) => x + y;
        let sum = add(5, 10, "hello");
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "sum", "15")
    }

  Assert.False(Result.isError result)

[<Fact>]
let PassingTooFewArgsIsAnError () =
  let result =
    result {
      let src =
        """
        let add = fn (x: number, y: number) => x + y;
        let sum = add(5);
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)

      Assert.Equal(
        ctx.Report.Diagnostics[0].Description,
        "function called with too few arguments"
      )

      Assert.Value(env, "sum", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferBasicFunction () =
  let res =
    result {
      let src =
        """
        let add = fn <A: number, B: number>(a: A, b: B) => a + b;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "add", "fn <A: number, B: number>(a: A, b: B) -> A + B")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferFunctionWithOptionalParms () =
  let res =
    result {
      let src =
        """
        let foo = fn (a: number, b?: string) => a;
        let a = foo(5);
        let b = foo(10, "hello");
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "number")
      Assert.Value(env, "b", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let CheckOptionalParmsErrorsWithIncorrectType () =
  let res =
    result {
      let src =
        """
        let foo = fn (a: number, b?: string) => a;
        let a = foo(5);
        let b = foo(10, true);
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)
      Assert.Value(env, "a", "number")
      Assert.Value(env, "b", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferFunctionWithRestParms () =
  let res =
    result {
      let src =
        """
        let foo = fn (a: number, ...b: string[]) => a;
        let a = foo(5);
        let b = foo(10, "hello");
        let c = foo(10, "hello", "world");
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "a", "number")
      Assert.Value(env, "b", "number")
      Assert.Value(env, "c", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let PassingIncorrectArgsAsRestParm () =
  let res =
    result {
      let src =
        """
        let foo = fn (a: number, ...b: string[]) => a;
        let c = foo(10, "hello", true);
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)

      Assert.Equal(
        ctx.Report.Diagnostics[0].Description,
        "Calling function with incorrect args"
      )

      Assert.Value(env, "c", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferFuncGeneralization () =
  let result =
    result {
      let src = "let fst = fn <A, B>(x: A, y: B) => x;"

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "fst", "fn <A, B>(x: A, y: B) -> A")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFactorial () =
  let result =
    result {

      let src =
        """
        let factorial = fn (n: number) =>
          if (n == 0) { 1 } else { n * factorial(n - 1) };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      // TODO: figure out how to get the param name back
      Assert.Value(env, "factorial", "fn (arg0: number) -> number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferRecursiveFunc () =
  let result =
    result {

      let src =
        """
        let rec = fn () => rec();
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "rec", "fn <A>() -> A")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferRecursiveSequence () =
  let result =
    result {

      let src =
        """
        let seq = fn () => seq() + 1;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "seq", "fn () -> number")
    }

  Assert.False(Result.isError result)


[<Fact>]
let InferTypeAliasOfTypeParam () =
  let result =
    result {
      let src =
        """
        let foo = fn <A>(x: A) {
          type B = A;
          let y: B = x;
          return y;
        };
        let bar = fn <A>(x: A) -> A {
          type B = A;
          let y: B = x;
          return y;
        };
        let z = foo(5);
        """

      // let w: number = z
      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      // TODO: This should be inferred as `fn <A>(x: A) -> A`
      Assert.Value(env, "foo", "fn <A>(x: A) -> B")
      Assert.Value(env, "bar", "fn <A>(x: A) -> A")
      Assert.Value(env, "z", "B")
    }

  Assert.False(Result.isError result)


[<Fact>]
let InferLambda () =
  let result =
    result {
      let src = "let add = fn <A: number, B: number>(x: A, y: B) => x + y;"

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "add", "fn <A: number, B: number>(x: A, y: B) -> A + B")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncParams () =
  let result =
    result {
      let src =
        """
          let addNums = fn <A: number, B: number>(x: A, y: B) {
            return x + y;
          };
          let addStrs = fn <A: string, B: string>(x: A, y: B) {
            return x ++ y;
          };
          let msg = addStrs("Hello, ", "world!");
          let greeting: string = "Bonjour, ";
          let frMsg = addStrs(greeting, "monde!");
          """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "addNums",
        "fn <A: number, B: number>(x: A, y: B) -> A + B"
      )

      Assert.Value(
        env,
        "addStrs",
        "fn <A: string, B: string>(x: A, y: B) -> A ++ B"
      )

      Assert.Value(env, "msg", "\"Hello, world!\"")
      Assert.Value(env, "frMsg", "string")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferBinaryOpStressTest () =
  let result =
    result {
      let src =
        """
          let foo = fn <A: number, B: number, C: number>(a: A, b: B, c: C) {
            return a * b + c;
          };
          let double = fn <A: number>(x: A) {
            return 2 * x;
          };
          let inc = fn <A: number>(x: A) {
            return x + 1;
          };
          let bar = foo(double(1), inc(2), 3);
          let baz: number = 5;
          let qux = double(baz);
          """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "foo",
        "fn <A: number, B: number, C: number>(a: A, b: B, c: C) -> A * B + C"
      )

      Assert.Value(env, "double", "fn <A: number>(x: A) -> 2 * A")
      Assert.Value(env, "inc", "fn <A: number>(x: A) -> A + 1")
      Assert.Value(env, "bar", "9")
      Assert.Value(env, "qux", "number")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFuncParamsWithTypeAnns () =
  let result =
    result {
      let src =
        """
          let addNums = fn (x: number, y: number) -> number {
            return x + y;
          };
          let addStrs = fn (x: string, y: string) -> string {
            return x ++ y;
          };
          """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "addNums", "fn (x: number, y: number) -> number")
      Assert.Value(env, "addStrs", "fn (x: string, y: string) -> string")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncWithMultipleReturns () =
  let result =
    result {
      let src =
        """
          let foo = fn <A: number>(x: A, y: string) {
            if (x > 0) {
              return x;
            }
            return y;
          };
          let bar = foo(5, "hello");
          """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <A: number>(x: A, y: string) -> string | A")
      Assert.Value(env, "bar", "string | 5")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncGenericFunc () =
  let result =
    result {
      let src =
        """
          let foo = fn <A>(x: A) {
            return x;
          };
          let bar = foo(5);
          let baz = foo("hello");
          """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <A>(x: A) -> A")
      Assert.Value(env, "bar", "5")
      Assert.Value(env, "baz", "\"hello\"")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let ApplyGenericTypeArgWithoutCallingFunction () =
  let result =
    result {
      let src =
        """
        let foo = fn <A>(x: A) {
          return x;
        };
        let bar = foo<number>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <A>(x: A) -> A")
      Assert.Value(env, "bar", "fn (x: number) -> number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let ApplyGenericTypeArgWithoutCallingFunctionWithTypeAlias () =
  let result =
    result {
      let src =
        """
        type Identity = fn <A>(x: A) -> A;
        declare let foo: Identity;
        let bar = foo<number>;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "Identity")
      Assert.Value(env, "bar", "fn (x: number) -> number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncGenericFuncWithExplicitTypeParams () =
  let result =
    result {
      let src =
        """
          let foo = fn <T>(x: T) -> T {
            return x;
          };
          let bar = foo(5);
          let baz = foo("hello");
          """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T>(x: T) -> T")
      Assert.Value(env, "bar", "5")
      Assert.Value(env, "baz", "\"hello\"")
    }

  Assert.False(Result.isError result)

[<Fact(Skip = "TODO(#176): Handle calling methods with optional chaining")>]
let InferCallFuncOnOptionalField () =
  let result =
    result {
      let src =
        """
        type Foo = { bar: fn () -> number };
        declare let foo: Foo | undefined;
        let bar = foo?.bar();
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "bar", "number | undefined")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFuncDecl () =
  let result =
    result {
      let src =
        """
        fn fst <A, B>(x: A, y: B) {
          return x;
        }
        declare fn snd<A, B>(x: A, y: B) -> B;
        """

      let! _, env = inferModule src

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
        fn fst <A, B>(x: A, y: B) {
          return x;
        }
        declare fn snd<A, B>(x: A, y: B) -> B;
        type Point = {x: number, y: number};
        fn makePoint (x: number, y: number) -> Point {
          return {x, y};
        }
        """

      let! _, env = inferModule src

      Assert.Value(env, "fst", "fn <A, B>(x: A, y: B) -> A")
      Assert.Value(env, "snd", "fn <A, B>(x: A, y: B) -> B")
      Assert.Value(env, "makePoint", "fn (x: number, y: number) -> Point")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)


[<Fact>]
let InferGenericWithConstraint () =
  let result =
    result {
      let src =
        """
        let foo = fn<T: {...}>(obj: T) => obj;
        let bar = foo({a: 5, b: "hello"});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T: {...}>(obj: T) -> T")
      Assert.Value(env, "bar", "{a: 5, b: \"hello\"}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferGenericWithConstraintRefrencingOtherTypeParam () =
  let result =
    result {
      let src =
        """
        let foo = fn<T: {...}, U: T>(t: T, u: U) => u;
        let bar = foo({a: 5}, {a: 5, b: "hello"});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T: {...}, U: T>(t: T, u: U) -> U")
      Assert.Value(env, "bar", "{a: 5, b: \"hello\"}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferGenericWithConstraintRefrencingOtherTypeParamMisordered () =
  let result =
    result {
      let src =
        """
        let foo = fn<U: T, T: {...}>(t: T, u: U) => u;
        let bar = foo({a: 5}, {a: 5, b: "hello"});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <U: T, T: {...}>(t: T, u: U) -> U")
      Assert.Value(env, "bar", "{a: 5, b: \"hello\"}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferGenericWithConstraintRefrencingOtherTypeParamAndOtherTypeRefs () =
  let result =
    result {
      let src =
        """
        type Obj = {...};
        fn foo<T: Obj, U: T>(t: T, u: U) {
          return u;
        }
        let bar = foo({a: 5}, {a: 5, b: "hello"});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T: Obj, U: T>(t: T, u: U) -> U")
      Assert.Value(env, "bar", "{a: 5, b: \"hello\"}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferGenericWithConstraintRefrencingOtherTypeParamAndOtherTypeRefsUsingLetFn
  ()
  =
  let result =
    result {
      let src =
        """
        type Obj = {...};
        let foo = fn<T: Obj, U: T>(t: T, u: U) => u;
        let bar = foo({a: 5}, {a: 5, b: "hello"});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T: Obj, U: T>(t: T, u: U) -> U")
      Assert.Value(env, "bar", "{a: 5, b: \"hello\"}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferSimpleGenericWithDefault () =
  let result =
    result {
      let src =
        """
        declare fn foo<T: {...} = {b: 10}>(bar?: T) -> {value: T};
        let x = foo();
        let y = foo({a: 5});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T: {...} = {b: 10}>(bar?: T) -> {value: T}")
      Assert.Value(env, "x", "{value: {b: 10}}")
      Assert.Value(env, "y", "{value: {a: 5}}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferSimpleGenericWithConstraint () =
  let result =
    result {
      let src =
        """
        declare fn foo<T: {...}>(bar?: T) -> {value: T};
        let x = foo();
        let y = foo({a: 5});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T: {...}>(bar?: T) -> {value: T}")
      Assert.Value(env, "x", "{value: {...}}")
      Assert.Value(env, "y", "{value: {a: 5}}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferSimpleGenericWithoutDefaultOrConstraint () =
  let result =
    result {
      let src =
        """
        declare fn foo<T>(bar?: T) -> {value: T};
        let x = foo();
        let y = foo({a: 5});
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T>(bar?: T) -> {value: T}")
      Assert.Value(env, "x", "{value: unknown}")
      Assert.Value(env, "y", "{value: {a: 5}}")
    }

  Assert.False(Result.isError result)

[<Fact(Skip = "TODO: check superclass when determining if class instances are assignable")>]
let InferClassSubType () =
  let result =
    result {
      let src =
        """
        declare let div: HTMLDivElement;
        let elem: HTMLElement = div;
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact(Skip = "TODO: check superclass when determining if class instances are assignable")>]
let InferGenericWithDefault () =
  let result =
    result {
      let src =
        """
        type Container<T, U> = {
            element: T,
            children: U,
        };
        
        declare fn create<T: HTMLElement = HTMLDivElement, U: HTMLElement[] = T[]>(
          element?: T,
          children?: U
        ) -> Container<T, U>;

        declare let span: HTMLSpanElement;
        
        let c1 = create();
        let c2 = create(span);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "fn <T = number>(t: T) -> T")
      Assert.Value(env, "bar", "{a: 5, b: \"hello\"}")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferOverloadsWithMatch () =
  let result =
    result {
      let src =
        """
        declare let add: (fn(a: number, b: number) -> number) & (fn(a: string, b: string) -> string);

        let num = add(5, 10);
        let str = add("hello, ", "world");
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "num", "number")
      Assert.Value(env, "str", "string")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferOverloadsWithNoMatch () =
  let result =
    result {
      let src =
        """
        declare let add: (fn(a: number, b: number) -> number) & (fn(a: string, b: string) -> string);

        let result = add(5, "hello");
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 2)
      Assert.Value(env, "result", "number & string")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferParamsFromCallbackType () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        declare let foo: fn (cb: fn (p: Point) -> number) -> number;

        let result = foo(fn (p) => p.x);
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 0)
      Assert.Value(env, "result", "number")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferParamsFromVarDeclTypeAnnotation () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number};
        type Foo = fn (p: Point) -> number;

        let foo: Foo = fn (p) => p.x;
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 0)
      Assert.Value(env, "foo", "Foo")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferOverloadsDeclarations () =
  let result =
    result {
      let src =
        """
        fn add(a: number, b: number) -> number {
          return a + b;
        }
        fn add(a: string, b: string) -> string {
          return a ++ b;
        }
        let sum = add(5, 10);
        let msg = add("hello, ", "world");
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 0)
      Assert.Value(env, "sum", "number")
      Assert.Value(env, "msg", "string")
    }

  Assert.False(Result.isError result)


[<Fact>]
let MutuallyRecursiveFunctions () =
  let result =
    result {
      let src =
        """
        fn foo() {
          return bar();
        }
        fn bar() {
          return foo();
        }
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 0)
      Assert.Value(env, "foo", "fn <A>() -> A")
      Assert.Value(env, "bar", "fn <A>() -> A")
    }

  Assert.False(Result.isError result)


// TODO:
// - write tests for functions with optional params
// - write tests for overloaded functions
// - move function related tests from Tests.fs into this file
