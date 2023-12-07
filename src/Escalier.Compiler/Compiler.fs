namespace Escalier.Compiler

open System.IO
open FsToolkit.ErrorHandling

open Escalier.Parser
open Escalier.TypeChecker
open Escalier.Codegen

module Compiler =
  // TODO: Implement
  // - parse
  // - typecheck
  // - codegen
  // - report errors

  // We also want to do these steps by reading from a file and
  // then writing the output to disk.  In particular

  type CompileError =
    | ParseError of FParsec.Error.ParserError
    | TypeError of Error.TypeError

  /// <summary>
  /// Compiles the source code located at the specified base directory.
  /// </summary>
  /// <param name="baseDir">The base directory where the source code is located.</param>
  /// <param name="source">The name of the source code file.</param>
  /// <returns>The compiled code.</returns>
  let compile (baseDir: string) (source: string) =
    result {
      let filename = Path.Join(baseDir, source)
      let contents = File.ReadAllText filename

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
      File.WriteAllText(outJsName, js)

      let outDtsName = Path.ChangeExtension(filename, ".d.ts")
      File.WriteAllText(outDtsName, dts)

      return ()
    }
