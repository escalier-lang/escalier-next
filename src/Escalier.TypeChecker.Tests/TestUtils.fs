module TestUtils

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Compiler.Compiler
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Error

let projectRoot = __SOURCE_DIRECTORY__

let inferModule src =
  result {
    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    let! ctx, env =
      TestCompiler.getEnvAndCtx projectRoot |> Async.RunSynchronously

    let! env =
      InferModule.inferModule ctx env ast
      |> Async.RunSynchronously
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let binding = env.FindValue name
    Assert.Equal(expected, binding.Type.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = env.FindScheme name
    Assert.Equal(expected, scheme.ToString())


let printDiagnostic (d: Diagnostic) =
  let rec printReasons (rs: list<TypeError>) =
    match rs with
    | [] -> ()
    | r :: rs ->
      printReason r
      printReasons rs

  and printReason (r: TypeError) =
    match r with
    | NotImplemented s -> printf "- Not implemented: %s\n" s
    | SemanticError s -> printf "- Semantic error: %s\n" s
    | NotInferred -> printf "- Type could not be inferred\n"
    | TypeMismatch(t1, t2) -> printf $"- Type mismatch: {t1} and {t2}\n"
    // printfn "t1.Provenance = %A" (prune t1).Provenance
    // printfn "t2.Provenance = %A" (prune t2).Provenance
    // printfn "t2 = %A" t2
    | RecursiveUnification(t1, t2) ->
      printf "- Recursive unification: {t1} and {t2}\n"
    | WrongNumberOfTypeArgs -> printf "- Wrong number of type arguments\n"
    | PropertyMissing propName -> printf $"- Property missing: {propName}\n"

  printf "ERROR: %s\n" d.Description

  printReasons d.Reasons

let rec printDiagnostics (ds: list<Diagnostic>) =
  match ds with
  | [] -> ()
  | d :: ds ->
    printDiagnostic d
    printDiagnostics ds
