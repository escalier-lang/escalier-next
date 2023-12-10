namespace Escalier.Compiler

open FsToolkit.ErrorHandling
open System.IO
open System.IO.Abstractions

open Escalier.Data.Syntax
open Escalier.Codegen
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Visitor

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

      let ctx =
        Env.Ctx(
          (fun ctx filename import -> env),
          (fun ctx filename import -> "")
        ) // TODO: fix this

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

  let getExports
    (ctx: Env.Ctx)
    (filename: string)
    (m: Module)
    : Result<Env.Env, CompileError> =
    result {
      let env = Prelude.getEnv ()

      let! env =
        Infer.inferScript ctx env filename m
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
        let! exports = getExports ctx file files[dep]

        imports <-
          { imports with
              Values = FSharpPlus.Map.union imports.Values exports.Values
              Schemes = FSharpPlus.Map.union imports.Schemes exports.Schemes }

      return imports
    }

  let resolvePath
    (baseDir: string)
    (currentPath: string)
    (importPath: string)
    : string =
    if importPath.StartsWith "~" then
      Path.GetFullPath(Path.Join(baseDir, importPath.Substring(1)))
    else if importPath.StartsWith "." then
      Path.GetFullPath(
        Path.Join(Path.GetDirectoryName(currentPath), importPath)
      )
    else
      importPath

  let findBindingNames (p: Pattern) : list<string> =
    let mutable names: list<string> = []

    let visitor =
      { Visitor.VisitExpr =
          fun expr ->
            match expr.Kind with
            | ExprKind.Function _ -> false
            | _ -> true
        Visitor.VisitStmt = fun _ -> false
        Visitor.VisitPattern =
          fun pat ->
            match pat.Kind with
            | PatternKind.Identifier({ Name = name }) ->
              names <- name :: names
              false
            | _ -> true
        Visitor.VisitTypeAnn = fun _ -> false }

    walkPattern visitor p

    List.rev names

  let findModuleBindingNames (m: Module) : list<string> =
    let mutable names: list<string> = []

    for item in m.Items do
      match item with
      | Stmt stmt ->
        match stmt.Kind with
        | StmtKind.Decl({ Kind = DeclKind.VarDecl(pattern, _, _) }) ->
          names <- List.concat [ names; findBindingNames pattern ]
        | _ -> ()
      | _ -> ()

    names

  let compileFiles
    (filesystem: IFileSystem)
    (textwriter: TextWriter)
    (baseDir: string) // e.g. src/ or fixtures/basics/test1/
    (entry: string)
    =
    result {
      let ctx =
        Env.Ctx(
          (fun ctx filename import ->
            let env = Prelude.getEnv ()

            let resolvedImportPath =
              Path.ChangeExtension(
                resolvePath baseDir filename import.Path,
                ".esc"
              )

            printfn "resolvedImportPath = %s" resolvedImportPath
            let contents = filesystem.File.ReadAllText(resolvedImportPath)

            let m =
              match Parser.parseScript contents with
              | Ok value -> value
              | Error _ -> failwith $"failed to parse {resolvedImportPath}"

            let env =
              match Infer.inferScript ctx env entry m with
              | Ok value -> value
              | Error _ -> failwith $"failed to infer {resolvedImportPath}"

            let mutable newEnv = Env.Env.empty

            let bindings = findModuleBindingNames m

            for name in bindings do
              match env.Values.TryFind(name) with
              | Some(t, isMut) -> newEnv <- newEnv.AddValue name (t, false)
              | None -> failwith $"binding {name} not found"

            newEnv),
          (fun ctx filename import -> resolvePath baseDir filename import.Path)
        )

      let env = Prelude.getEnv ()
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
