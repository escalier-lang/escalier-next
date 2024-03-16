[<VerifyXunit.UsesVerify>]
module Tests

open FsToolkit.ErrorHandling
open FParsec.CharParsers
open System.IO
open VerifyTests
open VerifyXunit
open Xunit

open Escalier.Compiler
open Escalier.Parser
open Escalier.Interop.Infer
open Escalier.Interop.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())

type CompileError = Prelude.CompileError

let projectRoot = __SOURCE_DIRECTORY__

let inferScript src =
  result {
    printfn $"projectRoot = {projectRoot}"
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let filename = Path.Combine(projectRoot, "input.src")
    let! ctx, env = Prelude.getEnvAndCtxWithES5 projectRoot

    let! env =
      Infer.inferScript ctx env filename ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

[<Fact>]
let ParseBasicVarDecls () =
  let input =
    """
    declare var a: A;
    declare var b: B.C;
    declare var c: number;
    declare var d: string | boolean;
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTypeAliases () =
  let input =
    """
    declare type Foo = Bar;
    declare type Node = {
      value: T,
      next: Node | null
    };
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseSimpleFunctions () =
  let input =
    """
    declare function eval(x: string): any;
    declare function fst<A, B>(a: A, b: B): A;
    declare async function foo(): Promise<void>;
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMoreComplexFunctions () =
  let input =
    """
    declare function parseInt(string: string, radix?: number): number;
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseInterfaces () =
  let input =
    """
    interface Point {
      x: number;
      y: number;
    }
    interface Array<T> {
      length: number;
      [index: number]: T;
    }
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseInterfacesWithGetterSetter () =
  let input =
    """
    interface Foo {
      get bar(): number;
      set bar(value: number);
    }
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask


[<Fact>]
let ParseInterfaceWithOptionalMethodAndProperty () =
  let input =
    """
    interface PropertyDescriptor {
      foo?: boolean;
      bar?(): any;
      setTime(a: number): boolean;
    }
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseComplexMethodSig () =
  let input =
    """
    interface T {
      stringify(value: any, replacer?: (number | string)[] | null, space?: string | number): string;
      every<S extends T>(predicate: (value: T, index: number, array: readonly T[]) => value is S, thisArg?: any): this is readonly S[];
      [idx: string]: U | null | undefined | object;
    }
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseInterfaceWithComputedPropertyAndComputedMethod () =
  let input =
    """
    interface Foo {
      [Bar.Baz]: number;
      [Symbol.toPrimitive](hint: string): symbol;
    }
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseDomInterface () =
  let input =
    """
    interface HashChangeEventInit extends EventInit {
      newURL?: string;
      oldURL?: string;
    }
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTypeof () =
  let input = "declare var SVGMatrix: typeof DOMMatrix;"

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTemplateStringTypeInUnion () =
  let input =
    """
    type OptionalPostfixToken<T extends string> = ` ${T}` | "";
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMappedType () =
  let input =
    """
    type Partial<T> = {
        [P in keyof T]?: T[P];
    };
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask


[<Fact>]
let ParseConditionalType () =
  let input =
    """
    type ThisParameterType<T> = T extends (this: infer U, ...args: never) => any ? U : unknown;
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseBlockComments () =
  let input =
    """
    /**
     * multiline comment
     */
    declare var a: A;
    declare var b: /* inline comment */ B;
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseLineComments () =
  let input =
    """
    // line 1
    // line 2
    declare var a: A; // trailing
    declare var b: B;
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask


[<Fact>]
let InferBasicVarDecls () =
  let res =
    result {
      let input =
        """
        declare var a: number;
        declare const b: string | undefined;
        declare let c: (a: number) => string;
        declare function d<T>(x: T): T;
        declare let e: [5, "hello", true];
        """

      let! ast =
        match parseModule input with
        | Success(value, _, _) -> Result.Ok(value)
        | Failure(_, parserError, _) ->
          Result.mapError CompileError.ParseError (Result.Error(parserError))

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! newEnv =
        inferModule ctx env ast |> Result.mapError CompileError.TypeError

      Assert.Value(newEnv, "a", "number")
      Assert.Value(newEnv, "b", "string | undefined")
      Assert.Value(newEnv, "c", "fn (a: number) -> string")
      Assert.Value(newEnv, "d", "fn <T>(x: T) -> T")
      Assert.Value(newEnv, "e", "[5, \"hello\", true]")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let InferTypeDecls () =
  let res =
    result {
      // TODO: parse these from lib.es5.d.ts instead
      let input =
        """
        type Pick<T, K extends keyof T> = {
          [P in K]: T[P];
        };
        type Exclude<T, U> = T extends U ? never : T;
        type Omit<T, K extends keyof any> = Pick<T, Exclude<keyof T, K>>;
        type Point = {x: number, y: number};
        """

      let! ast =
        match parseModule input with
        | Success(value, _, _) -> Result.Ok(value)
        | Failure(_, parserError, _) ->
          Result.mapError CompileError.ParseError (Result.Error(parserError))

      let! ctx, env = Prelude.getEnvAndCtx projectRoot

      let! newEnv =
        inferModule ctx env ast |> Result.mapError CompileError.TypeError

      Assert.Type(newEnv, "Pick", "{[P]: T[P] for P in K}")
      Assert.Type(newEnv, "Exclude", "T extends U ? never : T")
      Assert.Type(newEnv, "Omit", "Pick<T, Exclude<keyof T, K>>")
      Assert.Type(newEnv, "Point", "{x: number, y: number}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let InferLibES5 () =
  let result =
    result {
      let! ctx, env = Prelude.getEnvAndCtxWithES5 projectRoot
      // let! newEnv = prelude.loadTypeDefinitions ctx env

      // printfn "---- Schemes ----"
      //
      // for KeyValue(name, scheme) in newEnv.Schemes do
      //   printfn $"{name}"
      //
      // printfn "---- Values ----"
      //
      // for KeyValue(name, t) in newEnv.Values do
      //   printfn $"{name}"

      return env
    }

  Assert.True(Result.isOk result)

[<Fact>]
let InferArrayPrototype () =
  let result =
    result {
      let! ctx, env = Prelude.getEnvAndCtxWithES5 projectRoot

      let scheme = Map.find "Array" env.Schemes
      // printfn $"Array = {scheme}"

      let scheme = Map.find "ArrayConstructor" env.Schemes
      // printfn $"ArrayConstructor = {scheme}"

      return env
    }

  Assert.True(Result.isOk result)

[<Fact>]
let InferInt8ArrayPrototype () =
  let result =
    result {
      let! ctx, env = Prelude.getEnvAndCtxWithES5 projectRoot

      let scheme = Map.find "Int8Array" env.Schemes

      // printfn $"Int8Array = {scheme}"

      return env
    }

  Assert.True(Result.isOk result)

[<Fact>]
let CanCallMutableMethodsOnMutableArray () =
  let result =
    result {
      let src =
        """
        let mut a: number[] = [3, 2, 1];
        a.sort();
        let b = a.map(fn (x) => x * 2);
        """

      let! ctx, env = inferScript src

      Assert.Value(env, "b", "number[]")
    }

  Assert.False(Result.isError result)

[<Fact>]
let CanIndexOnArrays () =
  let result =
    result {
      let src =
        """
        let mut a: number[] = [3, 2, 1];
        let b = a[0];
        let mut len1 = a.length;
        let len2 = a.length;
        len1 = len2;
        """

      let! ctx, env = inferScript src

      Assert.Value(env, "b", "number | undefined")
      Assert.Value(env, "len1", "unique number")
      Assert.Value(env, "len2", "unique number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let CannotCallMutableMethodsOnNonMutableArray () =
  let result =
    result {
      let src =
        """
        let a: number[] = [3, 2, 1];
        a.sort();
        let b = a.map(fn (x) => x * 2);
        """

      let! ctx, env = inferScript src
      ()
    }

  printfn "result = %A" result
  Assert.True(Result.isError result)

[<Fact>]
let CallArrayConstructor () =
  let result =
    result {
      let src =
        """
        let mut a: number[] = new Array();
        a.push(5);
        """

      let! ctx, env = inferScript src

      Assert.Value(env, "a", "number[]")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let CallArrayConstructorWithTypeArgs () =
  let result =
    result {
      let src =
        """
        let mut a = new Array<number>();
        a.push(5);
        """

      let! ctx, env = inferScript src

      Assert.Value(env, "a", "number[]")
    }

  Assert.False(Result.isError result)

[<Fact>]
let CallArrayConstructorWithNoTypeAnnotations () =
  let result =
    result {
      let src =
        """
        let mut a = new Array();
        a.push(5);
        """

      let! ctx, env = inferScript src

      Assert.Value(env, "a", "5[]")
    }

  Assert.False(Result.isError result)

[<Fact>]
let AcessNamespaceType () =
  let result =
    result {
      let src =
        """
        type NumFmt = Intl.NumberFormat;
        """

      let! ctx, env = inferScript src

      Assert.Type(env, "NumFmt", "Intl.NumberFormat")
    }

  Assert.False(Result.isError result)

[<Fact>]
let AcessNamespaceValue () =
  let result =
    result {
      let src =
        """
        let fmt = new Intl.NumberFormat("en-CA");
        fmt.format(1.23);
        """

      let! ctx, env = inferScript src

      // TODO: the type should include the namespace, i.e. Intl.NumberFormat
      Assert.Value(env, "fmt", "Intl.NumberFormat")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)
