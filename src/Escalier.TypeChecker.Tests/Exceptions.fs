module Exceptions

open FParsec
open FsToolkit.ErrorHandling
open Xunit

open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Infer

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())

type CompileError =
  | ParseError of ParserError
  | TypeError of TypeError


let inferScript src =
  result {
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let env = Prelude.getEnv ()

    let ctx =
      Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

    let! env =
      inferScript ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }


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

  printf "ERROR: %s\n" d.Description

  printReasons d.Reasons

let rec printDiagnostics (ds: list<Diagnostic>) =
  match ds with
  | [] -> ()
  | d :: ds ->
    printDiagnostic d
    printDiagnostics ds


[<Fact>]
let InfersExplicitThrow () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) {
          if (x < 0) {
            throw "RangeError"
          }
          return x
        }
        """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn (x: number) -> number throws \"RangeError\"")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InfersThrowExpression () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if (x < 0) { throw "RangeError" } else { x }
        """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn (x: number) -> number throws \"RangeError\"")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InfersThrowsFromCall () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if (x < 0) { throw "RangeError" } else { x }
          
        let bar = fn (x) => foo(x)
        """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn (x: number) -> number throws \"RangeError\"")
      Assert.Value(env, "bar", "fn (x: number) -> number throws \"RangeError\"")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact(Skip = "TODO")>]
let InfersCatchException () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if (x < 0) { throw "RangeError" } else { x }
          
        let bar = fn (x) =>
          try {
            foo(x)
          } catch (e) {
            0
          }
        """

      let! ctx, env = inferScript src

      Assert.Value(env, "bar", "fn (x: number) -> number")

      printDiagnostics ctx.Diagnostics
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)
