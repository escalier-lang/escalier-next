module Classes

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler.Compiler
open Escalier.TypeChecker.Unify

open TestUtils

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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Type(
        env,
        "Foo",
        "{bar fn (self: Self) -> string, baz fn (mut self: Self, msg: string) -> undefined, msg: string, ...}"
      )

      Assert.Value(env, "foo", "Foo")

      let binding = env.FindValue "foo"

      let! fooType =
        expandType ctx env None Map.empty binding.Type
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar fn (self: Self) -> string, baz fn (mut self: Self, msg: string) -> undefined, msg: string, ...}",
        fooType.ToString()
      )
    }

  Assert.False(Result.isError result)

[<Fact(Skip = "TODO")>]
let InferClassWithMethodsInModule () =
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Type(
        env,
        "Foo",
        "{bar fn (self: Self) -> string, baz fn (mut self: Self, msg: string) -> undefined, msg: string}"
      )

      Assert.Value(env, "foo", "Foo")

      let binding = env.FindValue "foo"

      let! fooType =
        expandType ctx env None Map.empty binding.Type
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar fn (self: Self) -> string, baz fn (mut self: Self, msg: string) -> undefined, msg: string}",
        fooType.ToString()
      )
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferClassWithConstructor () =
  let result =
    result {
      let src =
        """
        let Foo = class {
          msg: string;
          new (mut self, msg: string) {
            self.msg = msg;
          }
        };
        let foo = new Foo("hello");
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "Foo")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let DisallowCallingMethodsFromConstructor () =
  let result =
    result {
      let src =
        """
        let Foo = class {
          msg: string;
          new (mut self, msg: string) {
            self.msg = self.bar();
          }
          fn bar(self) {
            return self.msg;
          }
        };
        let foo = new Foo("hello");
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)
      Assert.Value(env, "foo", "Foo")
    }

  Assert.False(Result.isError result)

[<Fact>]
let RequireThatAllPropertiesBeAssigned () =
  let result =
    result {
      let src =
        """
        let Point = class {
          x: number;
          y: number;
          new (mut self, x: number, y: number) {
            self.x = x;
          }
        };
        let p = new Point(5, 10);
        """

      let! ctx, env = inferModule src

      Assert.Equal(ctx.Report.Diagnostics.Length, 1)
      Assert.Value(env, "p", "Point")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferClassWithTypeParamAndConstructor () =
  let result =
    result {
      let src =
        """
        let Foo = class<T> {
          msg: T;
          new (mut self, msg: T) {
            self.msg = msg;
          }
        };
        let foo = new Foo<string>("hello");
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)
      Assert.Value(env, "foo", "Foo<string>")
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Type(
        env,
        "Foo",
        "<T>({map fn <U>(self: Self, callback: fn (bar: T) -> U) -> U, bar: T, ...})"
      )

      Assert.Value(env, "Foo", "{new fn <T>() -> Foo<T>}")
      Assert.Value(env, "foo", "Foo<string>")

      let binding = env.FindValue "foo"

      let! fooType =
        expandType ctx env None Map.empty binding.Type
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar: string, map fn <U>(self: Self, callback: fn (bar: string) -> U) -> U, ...}",
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Type(env, "Foo", "{bar fn (self: Self) -> Self, msg: string, ...}")
      Assert.Value(env, "foo", "Foo")
      Assert.Value(env, "bar", "Foo")

      // let barType, _ = Map.find "bar" env.Values
      //
      // printfn "barType = %A" barType

      let binding = env.FindValue "foo"

      let! fooType =
        expandType ctx env None Map.empty binding.Type
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar fn (self: Self) -> Self, msg: string, ...}",
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Type(env, "Foo", "{bar fn (self: Self) -> Self, msg: string, ...}")
      Assert.Value(env, "foo", "Foo")
      Assert.Value(env, "bar", "Foo")

      // let barType, _ = Map.find "bar" env.Values
      //
      // printfn "barType = %A" barType

      let binding = env.FindValue "foo"

      let! fooType =
        expandType ctx env None Map.empty binding.Type
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar fn (self: Self) -> Self, msg: string, ...}",
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Type(env, "Foo", "<T>({bar fn (self: Self) -> Self, msg: T, ...})")
      Assert.Value(env, "foo", "Foo<string>")
      Assert.Value(env, "bar", "Foo<string>")

      // let barType, _ = Map.find "bar" env.Values
      //
      // printfn "barType = %A" barType

      let binding = env.FindValue "foo"

      let! fooType =
        expandType ctx env None Map.empty binding.Type
        |> Result.mapError CompileError.TypeError

      Assert.Equal(
        "{bar fn (self: Self) -> Self, msg: string, ...}",
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
          new (mut self, x: number, y: number) {
            self.x = x;
            self.y = y;
          }
          fn makePoint(x: number, y: number) {
            return new Self(x, y);
          }
          fn add(self, other: Self) -> Self {
            return Self.makePoint(self.x + other.x, self.y + other.y);
          }
        };
        let p1 = new Point(1, 0);
        let p2 = new Point(0, 1);
        let p3 = p1.add(p2);
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "p1", "Point")
      Assert.Value(env, "p2", "Point")
      Assert.Value(env, "p3", "Point")

      Assert.Type(
        env,
        "Point",
        "{add fn (self: Self, other: Self) -> Self, y: number, x: number, ...}"
      )

      Assert.Value(
        env,
        "Point",
        "{new fn (mut self: Self, x: number, y: number) -> Point, makePoint fn (x: number, y: number) -> Self}"
      )
    }

  printfn "result = %A" result
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

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(env, "p", "Point")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferGenericRecursiveClass () =
  let res =
    result {
      let src =
        """
        let Node = class<T> {
          value: T;
          left: Self;
          right: Self;
          fn map<U>(self, mapper: fn (x: T) -> U) {
            let node = {
              value: mapper(self.value),
              left: self.left.map(mapper),
              right: self.right.map(mapper),
            };
          }
        };
        """

      let! _ = inferModule src
      ()
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferGenericMethods () =
  let res =
    result {
      let src =
        """
        let Foo = class {
          fn fst<A, B>(self, a: A, b: B) {
            return a;
          }
          fn snd<A, B>(a: A, b: B) {
            return b;
          }
        };
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Report.Diagnostics)

      Assert.Value(
        env,
        "Foo",
        "{new fn () -> Foo, snd fn <A, B>(a: A, b: B) -> B}"
      )

      Assert.Type(
        env,
        "Foo",
        "{fst fn <A, B>(self: Self, a: A, b: B) -> A, ...}"
      )
    }

  Assert.False(Result.isError res)

[<Fact(Skip = "TODO: fix method calls on optionals")>]
let InferGenericRecursiveClassWithOptionalProperties () =
  let res =
    result {
      let src =
        """
        let Node = class<T> {
          value: T;
          left?: Self;
          right?: Self;
          fn map<U>(self, mapper: fn (x: T) -> U) {
            let node = {
              value: mapper(self.value),
              left: self.left?.map(mapper),
              right: self.right?.map(mapper),
            };
          }
        };
        """

      let! _ = inferModule src
      ()
    }

  Assert.False(Result.isError res)

// TODO: handle explicit constructors
