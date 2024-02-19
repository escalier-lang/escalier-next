[<VerifyXunit.UsesVerify>]
module Tests

open FsToolkit.ErrorHandling
open FParsec.CharParsers
open System.IO.Abstractions.TestingHelpers
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

let inferScript src =
  result {
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/"

    let! env =
      Infer.inferScript ctx env "input.esc" ast
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

      let mockFileSystem = MockFileSystem()
      let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/"

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

      let mockFileSystem = MockFileSystem()
      let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/"

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
let ParseLibES5 () =
  let input = File.ReadAllText("./lib/lib.es5.d.ts")

  let result = parseModule input

  match result with
  | Success _ -> ()
  | Failure(_, error, _) ->
    printfn "%A" error
    Assert.Fail("failed to parse lib.es5.d.ts")

[<Fact>]
let InferLibES5 () =
  let result =
    result {
      let mockFileSystem = MockFileSystem()
      let! ctx, env = Prelude.getEnvAndCtxWithES5 mockFileSystem "/"
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
      let mockFileSystem = MockFileSystem()
      let! ctx, env = Prelude.getEnvAndCtxWithES5 mockFileSystem "/"

      let scheme = Map.find "Array" env.Schemes

      printfn $"Array = {scheme}"

      return env
    }

  Assert.True(Result.isOk result)

[<Fact(Skip = "TODO")>]
let CallMethodsOnArray () =
  let result =
    result {
      let mockFileSystem = MockFileSystem()
      let! ctx, env = Prelude.getEnvAndCtxWithES5 mockFileSystem "/"

      let scheme = Map.find "Array" env.Schemes
      printfn $"Array = {scheme}"

      let src =
        """
        let a: number[] = [3, 2, 1]
        a.sort()
        let b = a.map(fn (x) => x * 2)
        """

      let! ast =
        Parser.parseScript src |> Result.mapError CompileError.ParseError

      let! env =
        Infer.inferScript ctx env "input.esc" ast
        |> Result.mapError CompileError.TypeError

      return env
    }

  printfn "result = %A" result
  Assert.True(Result.isOk result)
