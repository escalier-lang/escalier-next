namespace Escalier.Compiler

open FsToolkit.ErrorHandling
open System.IO
open System.IO.Abstractions

open Escalier.Data.Syntax
open Escalier.Codegen
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Error

module Compiler =
  type CompileError =
    | ParseError of FParsec.Error.ParserError
    | TypeError of TypeError

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

      let env = Prelude.getEnv ()
      let ctx = Env.Ctx()

      let! env =
        Infer.inferScript ctx env ast |> Result.mapError CompileError.TypeError

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

  let getExports (ctx: Env.Ctx) (m: Module) : Result<Env.Env, CompileError> =
    result {
      let! env =
        Infer.inferScript ctx (Prelude.getEnv ()) m
        |> Result.mapError CompileError.TypeError

      let names = Infer.findModuleBindingNames m

      let mutable exports = Env.Env.empty

      for name in names do
        let binding = env.Values.[name]
        exports <- exports.AddValue name binding

      return exports
    }

  let getImports
    (ctx: Env.Ctx)
    (file: string)
    (depsTree: Map<string, list<string>>)
    (files: Map<string, Module>)
    : Result<Env.Env, CompileError> =
    result {

      let mutable imports = Env.Env.empty

      for dep in depsTree[file] do
        let! exports = getExports ctx files[dep]

        imports <-
          { imports with
              Values = FSharpPlus.Map.union imports.Values exports.Values
              Schemes = FSharpPlus.Map.union imports.Schemes exports.Schemes }

      return imports
    }

  let compileFiles
    (filesystem: IFileSystem)
    (textwriter: TextWriter)
    (baseDir: string) // e.g. src/ or fixtures/basics/test1/
    (srcFiles: list<string>)
    =
    result {
      let mutable files: Map<string, Module> = Map.empty

      // Parse each file
      for srcFile in srcFiles do
        let contents = filesystem.File.ReadAllText(Path.Join(baseDir, srcFile))
        printfn "contents = %s" contents

        let! ast =
          Parser.parseScript contents |> Result.mapError CompileError.ParseError

        files <- files.Add(srcFile, ast)

      let mutable depTree: Map<string, list<string>> = Map.empty

      // TODO: find the imports and build dependency graph
      for KeyValue(filename, ast) in files do
        let imports =
          ast.Items
          |> List.choose (fun item ->
            match item with
            | ModuleItem.Import i -> Some i
            | _ -> None)

        let deps =
          imports
          |> List.map (fun i ->
            let path = i.Source

            let resolvedPath =
              if path.StartsWith "~" then
                Path.GetFullPath(Path.Join(baseDir, path.Substring(1)))
              else if path.StartsWith "." then
                Path.GetFullPath(
                  Path.Join(Path.GetDirectoryName(filename), path)
                )
              else
                path

            Path.ChangeExtension(resolvedPath, ".esc"))

        depTree <- depTree.Add(filename, deps)


      printfn "depTree = %A" depTree

      let ctx = Env.Ctx()
      let! imports = getImports ctx "/entry.esc" depTree files

      printfn "imports for /entry.esc"

      for KeyValue(name, binding) in imports.Values do
        printfn $"{name}: {fst binding}"

      let env = Prelude.getEnv ()

      let env =
        { Prelude.getEnv () with
            Values = FSharpPlus.Map.union env.Values imports.Values
            Schemes = FSharpPlus.Map.union env.Schemes imports.Schemes }

      let _ =
        Infer.inferScript ctx env files["/entry.esc"]
        |> Result.mapError CompileError.TypeError

      ()
    }

// TODO:
// typecheckFile
// typecheckFiles
