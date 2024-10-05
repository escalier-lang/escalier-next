namespace Escalier.Compiler

open Escalier.Data.Syntax
open FsToolkit.ErrorHandling
open System.IO

open Escalier.Codegen
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Error

module Compiler =
  type CompileError = Prelude.CompileError

  let printDiagnostic (writer: TextWriter) (d: Diagnostic) =
    let rec printReasons (rs: list<TypeError>) =
      match rs with
      | [] -> ()
      | r :: rs ->
        printReason r
        printReasons rs

    and printReason (r: TypeError) =
      match r with
      | NotImplemented s -> fprintf writer "- Not implemented: %s\n" s
      | SemanticError s -> fprintf writer "- Semantic error: %s\n" s
      | NotInferred -> fprintf writer "- Type could not be inferred\n"
      | TypeMismatch(t1, t2) ->
        fprintf writer $"- Type mismatch: {t1} and {t2}\n"
      | RecursiveUnification(t1, t2) ->
        fprintf writer "- Recursive unification: {t1} and {t2}\n"
      | WrongNumberOfTypeArgs ->
        fprintf writer "- Wrong number of type arguments\n"
      | PropertyMissing propName ->
        fprintf writer $"- Property missing: {propName}\n"

    fprintf writer "ERROR: %s\n" d.Description

    printReasons d.Reasons

  let rec printDiagnostics (writer: TextWriter) (ds: list<Diagnostic>) =
    match ds with
    | [] -> ()
    | d :: ds ->
      printDiagnostic writer d
      printDiagnostics writer ds

  /// <summary>
  /// Compiles the source code located at the specified base directory.
  /// </summary>
  /// <param name="textwriter">The text writer to use for diagnostics.</param>
  /// <param name="baseDir">The base directory where the source code is located.</param>
  /// <param name="srcFile">The name of the source code file.</param>
  /// <returns>The compiled code.</returns>
  let compileFile (textwriter: TextWriter) (baseDir: string) (srcFile: string) =
    result {
      let filename = srcFile
      let contents = File.ReadAllText filename
      let! ctx, env = Prelude.getEnvAndCtx baseDir

      let! ast =
        Parser.parseModule contents |> Result.mapError CompileError.ParseError

      let env = { env with Filename = srcFile }

      let! env =
        Infer.inferModule ctx env ast |> Result.mapError CompileError.TypeError

      printDiagnostics textwriter ctx.Report.Diagnostics

      let printCtx: Printer.PrintCtx = { Indent = 0; Precedence = 0 }

      let buildCtx =
        { Codegen.NextTempId = 0
          Codegen.AutoImports = Set.empty }

      let mod' = Codegen.buildModule buildCtx ast
      let js = Printer.printModule printCtx mod'
      let outJsName = Path.ChangeExtension(filename, ".js")
      File.WriteAllText(outJsName, js)

      let mod' = Codegen.buildModuleTypes env buildCtx ast
      let dts = Printer.printModule printCtx mod'
      let outDtsName = Path.ChangeExtension(filename, ".d.ts")
      File.WriteAllText(outDtsName, dts)

      return ()
    }

  let compileString
    (textwriter: TextWriter)
    (baseDir: string)
    (srcCode: string)
    : Result<string * string, CompileError> =
    result {
      let! ctx, env = Prelude.getEnvAndCtx baseDir

      let! ast =
        Parser.parseModule srcCode |> Result.mapError CompileError.ParseError

      let env = { env with Filename = "./entry.esc" }

      let! env =
        Infer.inferModule ctx env ast |> Result.mapError CompileError.TypeError

      printDiagnostics textwriter ctx.Report.Diagnostics

      let printCtx: Printer.PrintCtx = { Indent = 0; Precedence = 0 }

      let mod' =
        Codegen.buildModuleTypes
          env
          { NextTempId = 0
            AutoImports = Set.empty }
          ast

      let dts = Printer.printModule printCtx mod'

      let mod' =
        Codegen.buildModule
          { NextTempId = 0
            AutoImports = Set.empty }
          ast

      let js = Printer.printModule printCtx mod'

      return (js, dts)
    }

  let findFiles (baseDir: string) (entryFile: string) =
    let mutable paths = [ entryFile ]

    let rec findFilesRec (entryFile: string) =
      let contents = File.ReadAllText(entryFile)

      let m =
        match Parser.parseModule contents with
        | Ok value -> value
        | Error _ -> failwith $"failed to parse {entryFile}"

      for item in m.Items do
        match item with
        | Import import ->
          let path =
            match import.Path[0] with
            | '.' ->
              Path.GetFullPath(
                Path.Join(Path.GetDirectoryName(entryFile), import.Path)
              )
            | '~' ->
              Path.GetFullPath(Path.Join(baseDir, import.Path.Substring(2)))
            | _ -> failwith $"TODO - import.Path = {import.Path}"

          let path = Path.ChangeExtension(path, "esc")

          if not (List.contains path paths) then
            paths <- path :: paths
            findFilesRec path
          else
            ()
        | Export export -> printfn "TOOD - handle re-exports"
        | Stmt stmt -> ()

    findFilesRec entryFile

    paths


// TODO:
// typecheckFile
// typecheckFiles
