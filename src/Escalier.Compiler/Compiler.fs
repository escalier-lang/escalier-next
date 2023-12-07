namespace Escalier.Compiler

open FsToolkit.ErrorHandling
open System.IO
open System.IO.Abstractions

open Escalier.Parser
open Escalier.TypeChecker
open Escalier.Codegen

module Compiler =
  type CompileError =
    | ParseError of FParsec.Error.ParserError
    | TypeError of Error.TypeError

  /// <summary>
  /// Compiles the source code located at the specified base directory.
  /// </summary>
  /// <param name="filesystem">The filesystem to use.</param>
  /// <param name="baseDir">The base directory where the source code is located.</param>
  /// <param name="source">The name of the source code file.</param>
  /// <returns>The compiled code.</returns>
  let compile (filesystem: IFileSystem) (baseDir: string) (source: string) =
    result {
      let filename = Path.Join(baseDir, source)
      let contents = filesystem.File.ReadAllText filename

      let! ast =
        Parser.parseScript contents |> Result.mapError CompileError.ParseError

      let env = Prelude.getEnv ()
      let ctx = Env.Ctx()

      let! env =
        Infer.inferScript ctx env ast.Stmts
        |> Result.mapError CompileError.TypeError

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
