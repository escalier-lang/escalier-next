[<VerifyXunit.UsesVerify>]
module Tests

open Escalier.TypeChecker
open FsToolkit.ErrorHandling
open FParsec.CharParsers
open FParsec.Error
open VerifyTests
open VerifyXunit
open Xunit
open System.IO

open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Env
open Escalier.Interop.Parser
open Escalier.Interop.Infer

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())


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

type CompileError =
  | ParseError of ParserError
  | TypeError of TypeError

[<Fact>]
let InferBasicVarDecls () =
  let res =
    result {
      let input =
        """
        declare var a: number;
        declare const b: string | undefined;
        """

      // declare let c: (a: number) => string;
      // declare function d<T>(x: T): T;

      let! ast =
        match parseModule input with
        | Success(value, _, _) -> Result.Ok(value)
        | Failure(_, parserError, _) ->
          Result.mapError CompileError.ParseError (Result.Error(parserError))

      let env = Prelude.getEnv ()
      let newEnv = inferModule env ast

      Assert.Value(newEnv, "a", "number")
    }

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
