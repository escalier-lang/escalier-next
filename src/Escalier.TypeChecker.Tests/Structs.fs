module Structs

open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Infer

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())

type CompileError = Prelude.CompileError


let inferScript src =
  result {
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/"

    let! env =
      inferScript ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

[<Fact>]
let InferBasicStruct () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        """

      let! _, env = inferScript src

      Assert.Value(env, "point", "Point")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferBasicStructIncorrectTypes () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: "hello", y: true}
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let InferGenericStruct () =
  let res =
    result {
      let src =
        """
        struct Point<T> {x: T, y: T}
        let point = Point<number> {x: 5, y: 10}
        """

      let! _, env = inferScript src

      Assert.Value(env, "point", "Point<number>")
    }

  Assert.False(Result.isError res)

[<Fact>]
let StructsAreSubtypesOfObjects () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point: {x: number, y: number} = Point {x: 5, y: 10}
        """

      let! _, env = inferScript src

      Assert.Value(env, "point", "{x: number, y: number}")
    }

  Assert.False(Result.isError res)

[<Fact>]
let PropertyAccessOnStructs () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        let x = point.x
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let SettingPropertiesOnMutableStructs () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let mut point = Point {x: 5, y: 10}
        point.x = 0
        """

      let! _ = inferScript src
      ()
    }

  Assert.False(Result.isError res)

[<Fact>]
let SettingPropertiesOnNonmutableStructsFails () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        point.x = 0
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let PropertyAccessOnPrivateStructs () =
  let res =
    result {
      let src =
        """
        let makePoint = fn (x, y) {
          struct Point {x: number, y: number}
          return Point {x, y}
        }
        let point = makePoint(5, 10)
        let x = point.x
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact(Skip = "update getPropType to return an Result")>]
let PropertyAccessOnPrivateStructsWrongKey () =
  let res =
    result {
      let src =
        """
        let makePoint = fn (x, y) {
          struct Point {x: number, y: number}
          return Point {x, y}
        }
        let point = makePoint(5, 10)
        let z = point.z
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let ObjectDestructuringOfStructs () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        let {x, y} = point
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let StructDestructuring () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}
        let point = Point {x: 5, y: 10}
        let Point {x, y} = point
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let BasicStructAndImpl () =
  let res =
    result {
      let src =
        """
        struct Foo {x: number, y: string}

        impl Foo {
          fn bar(self) {
            return self.x
          }
        }

        impl Foo {
          fn baz(self) {
            return self.y
          }
        }

        let foo = Foo {x: 5, y: "hello"}
        let {x, y} = foo
        let bar = foo.bar()
        let baz = foo.baz()
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "string")
      Assert.Value(env, "bar", "number")
      Assert.Value(env, "baz", "string")
    }

  Assert.False(Result.isError res)

[<Fact>]
let CallingMethodInPreviousImpl () =
  let res =
    result {
      let src =
        """
        struct Foo {x: number}

        impl Foo {
          fn bar(self) {
            return self.x
          }
        }

        impl Foo {
          fn baz(self) {
            return self.bar()
          }
        }

        let foo = Foo {x: 5}
        let bar = foo.bar()
        let baz = foo.baz()
        """

      let! _, env = inferScript src

      Assert.Value(env, "bar", "number")
      Assert.Value(env, "baz", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let GetterSetterImpl () =
  let res =
    result {
      let src =
        """
        struct Foo {x: number, y: string}

        impl Foo {
          get bar(self) {
            return self.x
          }
          set baz(mut self, y) {
            self.y = y
          }
        }

        let mut foo = Foo {x: 5, y: "hello"}
        let bar = foo.bar
        foo.baz = "world"
        """

      let! _, env = inferScript src

      Assert.Value(env, "bar", "number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let CallingMethodInSameImpl () =
  let res =
    result {
      let src =
        """
        struct Foo {x: number}

        impl Foo {
          fn bar(self) {
            return self.x
          }
          
          fn baz(self) {
            return self.bar()
          }
        }

        let foo = Foo {x: 5}
        let bar = foo.bar()
        let baz = foo.baz()
        """

      let! _, env = inferScript src

      Assert.Value(env, "bar", "number")
      Assert.Value(env, "baz", "number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferGenericImpls () =
  let res =
    result {
      let src =
        """
        struct Point<T> {x: T, y: T}
        
        impl Point<T> {
          fn bar(self) -> T {
            return self.x
          }
        }
        
        let point = Point<number> {x: 5, y: 10}
        let bar = point.bar()
        """

      let! _, env = inferScript src

      Assert.Value(env, "point", "Point<number>")
      Assert.Value(env, "bar", "number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferGenericImplsWithParams () =
  let res =
    result {
      let src =
        """
        struct Foo<T> {x: T}

        impl Foo<T> {
          fn bar(mut self, x) {
            return self.x = x
          }
          
          fn baz(mut self, x) {
            return self.bar(x)
          }

          get qux(self) -> T {
            return self.x
          }
        }

        let mut foo = Foo<number>{x: 5}
        foo.bar(10)
        foo.baz(15)
        let qux = foo.qux 
        """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "Foo<number>")
      Assert.Value(env, "qux", "number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferGenericRecursiveStruct () =
  let res =
    result {
      let src =
        """
        struct Node<T> {
          value: T,
          left?: Node<T>,
          right?: Node<T>,
        }
        """

      let! _, env = inferScript src
      Assert.Type(env, "Node", "<T>(Node)")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact(Skip = "TODO: Stack overflow")>]
let InferRecursiveStructType () =
  let result =
    result {

      let src =
        """
        struct Node<T> {
          value: T,
          left?: Node<T>,
          right?: Node<T>
        }
        
        let node = Node<number> {
          value: 5,
          left: Node<number> {
            value: 10
          },
          right: Node<number> {
            value: 15
          }
        }
        """

      let! _, env = inferScript src

      Assert.Type(env, "node", "Node<number>")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact(Skip = "TODO: Stack overflow")>]
let InferGenericRecursiveStructWithImpls () =
  let res =
    result {
      let src =
        """
        struct Node<T> {
          value: T,
          left?: Node<T>,
          right?: Node<T>,
        }
        
        impl Node<T> {
          fn map<U>(self, mapper: fn (x: T) -> U) -> Node<U> {
            return {
              value: mapper(self.value),
              left: self.left?.map(mapper),
              right: self.right?.map(mapper)
            }
          }
        }
        """

      let! _, env = inferScript src
      ()
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let RecursiveMethodsCanBeInferred () =
  let res =
    result {
      let src =
        """
        struct Foo {}

        impl Foo {
          fn fact(self, n) {
            return if n == 0 {
              1
            } else {
              n * self.fact(n - 1)
            }
          }
        }

        let foo = Foo {}
        let res = foo.fact(5)
        let fact = foo.fact
        """

      let! _, env = inferScript src

      Assert.Value(env, "res", "number")
      Assert.Value(env, "fact", "fn (self: Self, n: number) -> number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let MutMethodsCanCallOtherMutMethods () =
  let res =
    result {
      let src =
        """
        struct Foo {x: number}

        impl Foo {
          fn bar(mut self, x) {
            return self.x = x
          }
          
          fn baz(mut self, x) {
            return self.bar(x)
          }
        }

        let mut foo = Foo {x: 5}
        foo.bar(10)
        foo.baz(15)
        let bar = foo.bar
        let baz = foo.baz
        """

      let! _, env = inferScript src
      Assert.Value(env, "bar", "fn (mut self: Self, x: number) -> number")
      Assert.Value(env, "baz", "fn (mut self: Self, x: number) -> number")
    }

  Assert.False(Result.isError res)

[<Fact>]
let CannotCallMutatingMethodOnNonMutableBinding () =
  let res =
    result {
      let src =
        """
        struct Foo {x: number}

        impl Foo {
          fn bar(mut self, x) {
            return self.x = x
          }
        }

        let foo = Foo {x: 5}
        foo.bar(10)
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let NonMutatingMethodsCannotCallOtherMutMethods () =
  let res =
    result {
      let src =
        """
        struct Foo {x: number}

        impl Foo {
          fn bar(mut self, x) {
            return self.x = x
          }
          
          fn baz(self, x) {
            return self.bar(x)
          }
        }

        let mut foo = Foo {x: 5}
        foo.bar(10)
        foo.baz(15)
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError res)

[<Fact>]
let StaticMethods () =
  let res =
    result {
      let src =
        """
        struct Point {x: number, y: number}

        impl Point {
          fn new(x, y) {
            return Self { x, y }
          }
          fn default() {
            return Point { x: 0, y: 0 }
          }
        }

        let p = Point.new(5, 10)
        let q = Point.default()
        let {x, y} = p
        """

      let! _, env = inferScript src
      Assert.Value(env, "p", "Self") // TODO: should be Point
      Assert.Value(env, "q", "Point")
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)
