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
        let add = fn (x, y) => {value: x + y}
        let sum = add("hello", "world")
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
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
        let add = fn (x, y) => x + y
        let sum = "hello" + "world"
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
      Assert.Value(env, "sum", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferCallGenericFuncWithWrongArg () =
  let result =
    result {
      let src =
        """
        let foo = fn <T: number>(x: T) -> T => x
        let bar = foo("bar")
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
      Assert.Value(env, "bar", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferCallGenericFuncWithComplexReturnAndWrongArg () =
  let result =
    result {
      let src =
        """
        let foo = fn <T: number>(x: T) => {value: x}
        let bar = foo("hello")
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
      Assert.Value(env, "bar", "{value: number}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncTypeAnnotation () =
  let result =
    result {
      let src =
        """
        let foo: fn <T: number>(x: T) -> T = fn (x) => x
        let x = foo(5)
        let y = foo("hello")
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
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
        let add = fn (x, y) => x + y
        let sum = add(5, 10, "hello")
        """

      let! _, env = inferScript src

      Assert.Value(env, "sum", "15")
    }

  Assert.False(Result.isError result)

[<Fact>]
let PassingTooFewArgsIsAnError () =
  let result =
    result {
      let src =
        """
        let add = fn (x, y) => x + y
        let sum = add(5)
        """

      let! _, _ = inferScript src

      ()
    }

  printfn "result = %A" result
  Assert.True(Result.isError result)


[<Fact>]
let InferBasicFunction () =
  let res =
    result {
      let src =
        """
        let add = fn (a, b) => a + b
        """

      let! _, env = inferScript src

      Assert.Value(env, "add", "fn <A: number, B: number>(a: A, b: B) -> A + B")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferFunctionWithOptionalParms () =
  let res =
    result {
      let src =
        """
        let foo = fn (a: number, b?: string) => a
        let a = foo(5)
        let b = foo(10, "hello")
        """

      let! _, env = inferScript src

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
        let foo = fn (a: number, b?: string) => a
        let a = foo(5)
        let b = foo(10, true)
        """

      let! ctx, env = inferScript src

      Assert.Value(env, "a", "number")
      Assert.Value(env, "b", "number")

      Assert.Equal(ctx.Diagnostics.Length, 1)
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferFunctionWithRestParms () =
  let res =
    result {
      let src =
        """
        let foo = fn (a: number, ...b: string[]) => a
        let a = foo(5)
        let b = foo(10, "hello")
        let c = foo(10, "hello", "world")
        """

      let! _, env = inferScript src

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
        let foo = fn (a: number, ...b: string[]) => a
        let c = foo(10, "hello", true)
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let InferFuncGeneralization () =
  let result =
    result {
      let src = "let fst = fn (x, y) => x"

      let! _, env = inferScript src

      Assert.Value(env, "fst", "fn <B, A>(x: A, y: B) -> A")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)


[<Fact>]
let InferFactorial () =
  let result =
    result {

      let src =
        """
        let factorial = fn (n) =>
          if (n == 0) { 1 } else { n * factorial(n - 1) } 
        """

      let! _, env = inferScript src

      // TODO: figure out how to get the param name back
      Assert.Value(env, "factorial", "fn (arg0: number) -> 1 | number")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferRecursiveFunc () =
  let result =
    result {

      let src =
        """
        let rec = fn () => rec()
        """

      let! _, env = inferScript src

      // TODO: figure out how to get the param name back
      Assert.Value(env, "rec", "fn <A>() -> A")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferRecursiveSequence () =
  let result =
    result {

      let src =
        """
        let seq = fn () => seq() + 1
        """

      let! _, env = inferScript src

      // TODO: figure out how to get the param name back
      Assert.Value(env, "seq", "fn () -> number")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)


[<Fact>]
let InferTypeAliasOfTypeParam () =
  let result =
    result {
      let src =
        """
        let foo = fn <A>(x: A) {
          type B = A
          let y: B = x
          return y
        }
        let bar = fn <A>(x: A) -> A {
          type B = A
          let y: B = x
          return y
        }
        let z = foo(5)
        """

      // let w: number = z
      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn <A>(x: A) -> B")
      // TODO: This should be inferred as `fn <A>(x: A) -> A`
      Assert.Value(env, "bar", "fn <A>(x: A) -> A")
      Assert.Value(env, "z", "B")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)


[<Fact>]
let InferLambda () =
  let result =
    result {
      let src = "let add = fn (x, y) => x + y"

      let! _, env = inferScript src

      Assert.Value(env, "add", "fn <A: number, B: number>(x: A, y: B) -> A + B")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferSKK () =
  let result =
    result {
      let src =
        """
          let S = fn (f) => fn (g) => fn (x) => f(x)(g(x))
          let K = fn (x) => fn (y) => x
          let I = S(K)(K)
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "S",
        "fn <A, C, B>(f: fn (arg0: A) -> fn (arg0: B) -> C) -> fn (g: fn (arg0: A) -> B) -> fn (x: A) -> C"
      )

      Assert.Value(env, "K", "fn <B, A>(x: A) -> fn (y: B) -> A")
      Assert.Value(env, "I", "fn <A>(x: A) -> A")
    }

  Assert.False(Result.isError result)


[<Fact>]
let InferFuncParams () =
  let result =
    result {
      let src =
        """
          let addNums = fn (x, y) {
            return x + y
          }
          let addStrs = fn (x, y) {
            return x ++ y
          }
          let msg = addStrs("Hello, ", "world!")
          let greeting: string = "Bonjour, "
          let frMsg = addStrs(greeting, "monde!")
          """

      let! _, env = inferScript src

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
          let foo = fn (a, b, c) {
            return a * b + c
          }
          let double = fn (x) {
            return 2 * x
          }
          let inc = fn (x) {
            return x + 1
          }
          let bar = foo(double(1), inc(2), 3)
          let baz: number = 5
          let qux = double(baz)
          """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <C: number, A: number, B: number>(a: A, b: B, c: C) -> A * B + C"
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
            return x + y
          }
          let addStrs = fn (x: string, y: string) -> string {
            return x ++ y
          }
          """

      let! _, env = inferScript src

      Assert.Value(env, "addNums", "fn (x: number, y: number) -> number")
      Assert.Value(env, "addStrs", "fn (x: string, y: string) -> string")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFuncWithMultipleReturns () =
  let result =
    result {
      let src =
        """
          let foo = fn <A: number>(x: A, y: string) {
            if (x > 0) {
              return x
            }
            return y
          }
          let bar = foo(5, "hello")
          """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn <A: number>(x: A, y: string) -> string | A")
      Assert.Value(env, "bar", "string | 5")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFuncGenericFunc () =
  let result =
    result {
      let src =
        """
          let foo = fn (x) {
            return x
          }
          let bar = foo(5)
          let baz = foo("hello")
          """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn <A>(x: A) -> A")
      Assert.Value(env, "bar", "5")
      Assert.Value(env, "baz", "\"hello\"")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncGenericFuncWithExplicitTypeParams () =
  let result =
    result {
      let src =
        """
          let foo = fn <T>(x: T) -> T {
            return x
          }
          let bar = foo(5)
          let baz = foo("hello")
          """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn <T>(x: T) -> T")
      Assert.Value(env, "bar", "5")
      Assert.Value(env, "baz", "\"hello\"")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)


// TODO:
// - write tests for functions with optional params
// - write tests for overloaded functions
// - move function related tests from Tests.fs into this file
