﻿namespace Escalier.Compiler

open FsToolkit.ErrorHandling
open System.IO
open System.IO.Abstractions

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
  /// <param name="filesystem">The filesystem to use.</param>
  /// <param name="textwriter">The text writer to use for diagnostics.</param>
  /// <param name="baseDir">The base directory where the source code is located.</param>
  /// <param name="srcFile">The name of the source code file.</param>
  /// <returns>The compiled code.</returns>
  let compileFile
    (filesystem: IFileSystem)
    (textwriter: TextWriter)
    (baseDir: string)
    (srcFile: string)
    =
    result {
      let filename = Path.GetFullPath(Path.Join(baseDir, srcFile))
      let contents = filesystem.File.ReadAllText filename

      let! ast =
        Parser.parseScript contents |> Result.mapError CompileError.ParseError

      let! ctx, env = Prelude.getEnvAndCtx filesystem baseDir srcFile

      let! env =
        Infer.inferScript ctx env srcFile ast
        |> Result.mapError CompileError.TypeError

      printDiagnostics textwriter ctx.Diagnostics

      let printCtx: Printer.PrintCtx = { Indent = 0; Precedence = 0 }

      let mod' = Codegen.buildModuleTypes env { NextTempId = 0 } ast
      let dts = Printer.printModule printCtx mod'

      let block = Codegen.buildScript { NextTempId = 0 } ast

      let js =
        block.Body
        |> List.map (Printer.printStmt printCtx)
        |> String.concat "\n"

      let outJsName = Path.ChangeExtension(filename, ".js")
      filesystem.File.WriteAllText(outJsName, js)

      let outDtsName = Path.ChangeExtension(filename, ".d.ts")
      filesystem.File.WriteAllText(outDtsName, dts)

      return ()
    }

  let compileFiles
    (filesystem: IFileSystem)
    (textwriter: TextWriter)
    (baseDir: string) // e.g. src/ or fixtures/basics/test1/
    (entry: string)
    =
    result {
      let! ctx, env = Prelude.getEnvAndCtx filesystem baseDir entry

      let contents = filesystem.File.ReadAllText(entry)

      let! m =
        Parser.parseScript contents |> Result.mapError CompileError.ParseError

      let! env =
        Infer.inferScript ctx env entry m
        |> Result.mapError CompileError.TypeError

      return (ctx, env)
    }

// TODO:
// typecheckFile
// typecheckFiles
