[<VerifyXunit.UsesVerify>]
module Migrate

open FParsec
open FsToolkit.ErrorHandling
open System.IO
open Xunit

open Escalier.Compiler.Compiler
open Escalier.Interop.Parser
open Escalier.Interop.Migrate
open Escalier.TypeChecker
open Escalier.TypeChecker.Env

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let binding = env.FindValue name
    Assert.Equal(expected, binding.Type.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = env.FindScheme name
    Assert.Equal(expected, scheme.ToString())

let projectRoot = __SOURCE_DIRECTORY__

let rec findNearestAncestorWithNodeModules (currentDir: string) =
  let nodeModulesDir = Path.Combine(currentDir, "node_modules")

  if Directory.Exists(nodeModulesDir) then
    currentDir
  else
    let parentDir = Directory.GetParent(currentDir)

    match parentDir with
    | null ->
      failwith "node_modules directory not found in any ancestor directory."
    | _ -> findNearestAncestorWithNodeModules parentDir.FullName


[<Fact>]
let ParseAndInferBasicDecls () =
  let res =
    result {

      let input =
        """
        declare var a: number;
        declare var b: string;
        declare var c: boolean;
        declare var d: (x: number[]) => boolean;
        declare var e: { a: number, b: string };
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "a", "number")
      Assert.Value(env, "b", "string")
      Assert.Value(env, "c", "boolean")
      Assert.Value(env, "d", "fn (x: Array<number>) -> boolean")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferTypeAliases () =
  let res =
    result {

      let input =
        """
        type Foo = string;
        type Bar<T> = { value: T, next?: Bar<T> };
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Foo", "string")
      Assert.Type(env, "Bar", "<T>({value: T, next?: Bar<T>, ...})")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferInterface () =
  let res =
    result {

      let input =
        """
        interface Foo {
          bar(): number;
          baz(x: string): boolean;
          get qux(): string;
          set qux(x: string);
        }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Type(
        env,
        "Foo",
        "{bar fn (self: Self) -> number, baz fn (self: Self, x: string) -> boolean, get qux fn () -> string, set qux fn (x: string) -> undefined, ...}"
      )
    }

  printfn "%A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferMappedType () =
  let res =
    result {
      let input =
        """
        type Partial<T> = {
            [P in keyof T]?: T[P];
        };
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Type(env, "Partial", "<T>({[P]+?: T[P] for P in keyof T, ...})")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferUnorderedTypeParams () =
  let res =
    result {
      let input =
        """
        interface MyObjectConstructor {
            freeze<T extends { [idx: string]: U | null | undefined | object; }, U extends string | bigint | number | boolean | symbol>(o: T): Readonly<T>;
        }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Type(
        env,
        "MyObjectConstructor",
        "{freeze fn <T: {[idx]+?: U | null | undefined | object for idx in string, ...}, U: string | bigint | number | boolean | symbol>(self: Self, o: T) -> Readonly<T>, ...}"
      )
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferFuncDecl () =
  let res =
    result {
      let input =
        """
        declare function foo(x: number, y: string): boolean;
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "foo", "fn (x: number, y: string) -> boolean")
    }

  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferClassDecl () =
  let res =
    result {
      let input =
        """
        declare class Foo {
          bar(x: number, y: string): boolean;
          baz: string;
        }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "Foo", "{new fn () -> Foo}")

      Assert.Type(
        env,
        "Foo",
        "{bar fn (self: Self, x: number, y: string) -> boolean, baz: string, ...}"
      )
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferClassDeclWithStatics () =
  let res =
    result {
      let input =
        """
        declare class Foo {
          static bar(x: number, y: string): boolean;
          static baz: string;
        }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Value(
        env,
        "Foo",
        "{new fn () -> Foo, bar fn (x: number, y: string) -> boolean, baz: string}"
      )

      Assert.Type(env, "Foo", "{...}")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ImportThirdPartyModules () =
  let result =
    result {
      let input =
        """
        import {Globals, Property} from "csstype";
        import * as PropTypes from "prop-types";
        import {Interaction as SchedulerInteraction} from "scheduler/tracing";
        
        type AccentColor = Property.AccentColor;
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Type(
        env,
        "Globals",
        "\"-moz-initial\" | \"inherit\" | \"initial\" | \"revert\" | \"revert-layer\" | \"unset\""
      )

      Assert.Type(
        env,
        "SchedulerInteraction",
        "{__count: number, id: number, name: string, timestamp: number, ...}"
      )

      Assert.Type(env, "AccentColor", "Property.AccentColor")

      let! result =
        Helpers.expandScheme
          ctx
          env
          None
          (env.FindScheme "AccentColor")
          Map.empty
          None
        |> Result.mapError CompileError.TypeError

      Assert.Equal("Globals | DataType.Color | \"auto\"", result.ToString())
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let ParseAndInferPropertyKey () =
  let res =
    result {
      let input =
        """
        declare type MyPropertyKey = string | number | symbol;

        interface MyPropertyDescriptor {
            configurable?: boolean;
            enumerable?: boolean;
            value?: any;
            writable?: boolean;
            get?(): any;
            set?(v: any): void;
        }

        interface MyPropertyDescriptorMap {
            [key: MyPropertyKey]: MyPropertyDescriptor;
        }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Type(
        env,
        "MyPropertyDescriptorMap",
        "{[key]+?: MyPropertyDescriptor for key in MyPropertyKey, ...}"
      )
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndMigrateExportDeclareFunction () =
  let res =
    result {
      let input =
        """
        type Point = {x: number, y: number};
        export declare function add({x, y}: Point): number;
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Value(env, "add", "fn ({mut x, mut y}: Point) -> number")
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndMigrateCommentOnly () =
  let res =
    result {
      let input = "// This is a comment"

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let _ = migrateModule ast

      ()
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndMigrateEnum () =
  let res =
    result {
      let input =
        """
        export declare const enum CacheWriteBehavior {
          FORBID = 0,
          OVERWRITE = 1,
          MERGE = 2
        }
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let _ = migrateModule ast

      ()
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndMigrateMappedTypeWithRenaming () =
  let res =
    result {
      let input =
        """
        type OnlyRequiredProperties<T> = {
          [K in keyof T as {} extends Pick<T, K> ? never : K]: T[K];
        };
        """


      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let _ = migrateModule ast

      ()
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndMigrateNamedTuples () =
  let res =
    result {
      let input =
        """
        type Point = [x: number, y?: number];
        """

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let _ = migrateModule ast

      ()
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)

[<Fact>]
let ParseAndInferLibEs5 () =
  let res =
    result {
      let projecRoot = findNearestAncestorWithNodeModules __SOURCE_DIRECTORY__
      let nodeModulesDir = Path.Combine(projecRoot, "node_modules")

      let es5LibPath =
        Path.Combine(nodeModulesDir, "typescript", "lib", "lib.es5.d.ts")

      let input = File.ReadAllText(es5LibPath)

      let! ast =
        match parseModule input with
        | Success(ast, _, _) -> Result.Ok ast
        | Failure(_, error, _) -> Result.Error(CompileError.ParseError error)

      let ast = migrateModule ast

      let! ctx, env =
        TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

      let! env =
        InferModule.inferModule ctx env ast
        |> Async.RunSynchronously
        |> Result.mapError CompileError.TypeError

      Assert.Equal(true, true)
    }

  printfn "res = %A" res
  Assert.True(Result.isOk res)
