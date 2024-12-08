namespace Escalier.Compiler

open FSharp.Data
open FParsec.Error
open FsToolkit.ErrorHandling
open System.IO

open Escalier.Codegen
open Escalier.Data
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.Data.Visitor
open Escalier.TypeChecker
open Escalier.TypeChecker.Error
open Escalier.Interop

open Env
open Prelude
open FileSystem

module Compiler =
  type CompileError =
    | ParseError of ParserError
    | TypeError of TypeError

    override x.ToString() =
      match x with
      | ParseError err -> $"ParseError: {err}"
      | TypeError err -> $"TypeError: {err}"

  type Compiler(fs: IFileSystem) =
    let mutable memoizedNodeModulesDir = Map.empty
    let mutable cachedModules: Map<string, Namespace> = Map.empty

    let mutable memoizedEnvAndCtx: Map<string, Result<Ctx * Env, CompileError>> =
      Map.empty

    member private this.findNearestAncestorWithNodeModules
      (currentDir: string)
      : Async<string> =
      async {
        match memoizedNodeModulesDir.TryFind currentDir with
        | Some(nodeModulesDir) -> return nodeModulesDir
        | None ->
          let nodeModulesDir = Path.Combine(currentDir, "node_modules")
          let! exists = fs.DirExistsAsync nodeModulesDir

          if exists then
            return currentDir
          else
            let! parentDir = fs.GetParentAsync currentDir
            return! this.findNearestAncestorWithNodeModules parentDir
      }

    member private this.packageJsonHasTypes
      (packageJsonPath: string)
      : Async<bool> =
      async {
        let! exists = fs.FileExistsAsync packageJsonPath

        if exists then
          let! packageJson = fs.ReadAllTextAsync packageJsonPath
          let packageJsonObj = JsonValue.Parse packageJson

          match packageJsonObj.TryGetProperty "types" with
          | None -> return false
          | Some _ -> return true
        else
          return false
      }

    member private this.resolvePath
      (packageRoot: string)
      (currentPath: string)
      (importPath: string)
      : Async<string> =
      async {
        if importPath.StartsWith "~" then
          return
            Path.GetFullPath(Path.Join(packageRoot, importPath.Substring 1))
        else if importPath.StartsWith "." then
          let resolvedPath =
            Path.GetFullPath(
              Path.Join(Path.GetDirectoryName currentPath, importPath)
            )

          if currentPath.EndsWith(".d.ts") then
            return Path.ChangeExtension(resolvedPath, ".d.ts")
          else
            return resolvedPath
        else
          // TODO: once this is implemented, move it over to Escalier.Interop
          // TODO: check if there's `/` in the import path, if so, the first
          // part before the `/` is the name of the module and the rost is a
          // path to a .d.ts file within the module.

          // determine if importPath contains a '/' and split on it
          // if it does, the first part is the module name and the second part is
          // the path to the .d.ts file within the module

          // It's possible that the module name is a scoped package, in which case
          // the module name will be the first part of the second part of the split
          // and the second part will be the path to the .d.ts file within the module.
          let moduleName, subpath =
            match importPath.Split('/') |> List.ofArray with
            | [] -> failwith "This should never happen."
            | [ name ] -> name, None
            | name :: path ->
              if name.StartsWith("@") then
                let ns = name

                match path with
                | [] -> failwith "This should never happen."
                | [ name ] ->
                  let moduleName = String.concat "/" [ ns; name ]
                  moduleName, None
                | name :: path ->
                  let moduleName = String.concat "/" [ ns; name ]
                  moduleName, Some(String.concat "/" path)
              else
                name, Some(String.concat "/" path)

          let! rootDir = this.findNearestAncestorWithNodeModules packageRoot
          let nodeModulesDir = Path.Combine(rootDir, "node_modules")

          let pkgJsonPath1 =
            Path.Combine(nodeModulesDir, moduleName, "package.json")

          let pkgJsonPath2 =
            Path.Combine(nodeModulesDir, "@types", moduleName, "package.json")

          let! hasTypes1 = this.packageJsonHasTypes pkgJsonPath1
          let! hasTypes2 = this.packageJsonHasTypes pkgJsonPath2

          let pkgJsonPath =
            if hasTypes1 then
              pkgJsonPath1
            elif hasTypes2 then
              pkgJsonPath2
            else
              failwith
                $"package.json not found for module {moduleName}, rootDir = {rootDir}, nodeModulesDir = {nodeModulesDir}."

          // read package.json and parse it
          let! pkgJson = fs.ReadAllTextAsync pkgJsonPath
          let pkgJsonObj = JsonValue.Parse pkgJson

          match subpath with
          | None ->
            match pkgJsonObj.TryGetProperty("types") with
            | None ->
              return failwith "Invalid package.json: missing `types` field."
            | Some value ->
              let types = value.InnerText()

              if types.EndsWith(".d.ts") then
                return Path.Combine(Path.GetDirectoryName pkgJsonPath, types)
              else
                return
                  Path.Combine(
                    Path.GetDirectoryName pkgJsonPath,
                    $"{types}.d.ts"
                  )
          // Path.Combine(Path.GetDirectoryName pkgJsonPath, types)
          | Some value ->
            return
              Path.Combine(Path.GetDirectoryName pkgJsonPath, $"{value}.d.ts")
      }

    // TODO: dedupe with Escalier.Interop.Infer
    member private this.findBindingNames(p: Syntax.Pattern) : list<string> =
      let mutable names: list<string> = []

      let visitor =
        { ExprVisitor.VisitExpr =
            fun (expr, state) ->
              match expr.Kind with
              | ExprKind.Function _ -> (false, state)
              | _ -> (true, state)
          ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
          ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
          ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
          ExprVisitor.VisitStmt = fun (_, state) -> (false, state)
          ExprVisitor.VisitPattern =
            fun (pat, state) ->
              match pat.Kind with
              | PatternKind.Ident { Name = name } ->
                names <- name :: names
                (false, state)
              | _ -> (true, state)
          ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
          ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

      ExprVisitor.walkPattern visitor () p

      List.rev names

    member this.inferLib
      (ctx: Ctx)
      (env: Env)
      (fullPath: string)
      : Async<Result<Env * Module, CompileError>> =

      asyncResult {
        let! input = fs.ReadAllTextAsync fullPath

        let input =
          input.Replace(
            "readonly readyState: typeof FileReader.EMPTY | typeof FileReader.LOADING | typeof FileReader.DONE;",
            "readonly readyState: 0 | 1 | 2;"
          )

        // TODO: handle <reference path="global.d.ts" /> in @types/react/index.d.ts
        // TrustedHTML is not a valid type in TypeScript so we drop it.
        let input =
          input.Replace("__html: string | TrustedHTML;", "__html: string;")

        // TODO: handle <reference path="global.d.ts" /> in @types/react/index.d.ts
        // webview is only available under React Native so we drop it.
        let input = input.Replace("webview: ", "// webview: ")

        let! ast =
          match Parser.parseModule input with
          | FParsec.CharParsers.Success(value, _, _) -> Result.Ok(value)
          | FParsec.CharParsers.Failure(_, parserError, _) ->
            Result.mapError CompileError.ParseError (Result.Error(parserError))

        let ast = Migrate.migrateModule ast

        let newEnv = { env with Filename = fullPath }

        let! outEnv =
          InferModule.inferModule ctx newEnv ast
          |> AsyncResult.mapError CompileError.TypeError

        return outEnv, ast
      }

    member this.getLibExports
      (ctx: Ctx)
      (env: Env)
      (packageRoot: string)
      (name: string)
      (items: list<ModuleItem>)
      : Async<Result<Namespace, CompileError>> =

      asyncResult {
        let mutable ns: Namespace =
          { Name = name
            Values = Map.empty
            Schemes = Map.empty
            Namespaces = Map.empty }

        for item in items do
          match item with
          | ModuleItem.Import importDecl ->
            // We skip exports because we don't want to automatically re-export
            // everything.
            ()
          | ModuleItem.Export export ->
            // NOTE: This relies on the namespace being defined before it's exported
            match export with
            | NamespaceExport { Name = name } ->
              match env.Namespace.Namespaces.TryFind name with
              | Some value ->
                for KeyValue(key, binding) in value.Values do
                  ns <- ns.AddBinding key binding

                for KeyValue(key, scheme) in value.Schemes do
                  ns <- ns.AddScheme key scheme

                for KeyValue(key, value) in value.Namespaces do
                  ns <- ns.AddNamespace key value
              | None -> failwith $"Couldn't find namespace: '{name}'"
            | NamedExport { Src = Some src
                            Specifiers = specifiers } ->
              let! resolvedPath = this.resolvePath packageRoot env.Filename src
              let mutable resolvedPath = resolvedPath

              if resolvedPath.EndsWith(".js") then
                resolvedPath <- Path.ChangeExtension(resolvedPath, ".d.ts")

              if not (Path.HasExtension resolvedPath) then
                resolvedPath <- Path.ChangeExtension(resolvedPath, ".d.ts")

              let! exportNs =
                asyncResult {
                  if resolvedPath.EndsWith(".d.ts") then
                    if cachedModules.ContainsKey resolvedPath then
                      return cachedModules[resolvedPath]
                    else
                      let! modEnv, modAst =
                        this.inferLib ctx (getGlobalEnv ()) resolvedPath

                      let! ns =
                        this.getLibExports
                          ctx
                          modEnv
                          packageRoot
                          "<exports>"
                          modAst.Items

                      cachedModules <- cachedModules.Add(resolvedPath, ns)
                      return ns
                  else
                    printfn $"resolvedPath = {resolvedPath}"

                    return!
                      AsyncResult.ofResult (
                        Error(
                          TypeError(
                            NotImplemented "TODO: getLibExports - NamedExport"
                          )
                        )
                      )
                }

              for Named { Name = name; Alias = alias } in specifiers do
                let mutable found = false

                let exportName =
                  match alias with
                  | Some a -> a
                  | None -> name

                match exportNs.Values.TryFind name with
                | Some binding ->
                  ns <- ns.AddBinding exportName binding
                  found <- true
                | None -> ()

                match exportNs.Schemes.TryFind name with
                | Some scheme ->
                  ns <- ns.AddScheme exportName scheme
                  found <- true
                | None -> ()

                match exportNs.Namespaces.TryFind name with
                | Some value ->
                  ns <- ns.AddNamespace exportName value
                  found <- true
                | None -> ()

                if found then
                  failwith $"Couldn't find export '{name}' in {resolvedPath}"
            | NamedExport { Src = None; Specifiers = specifiers } ->
              for Named { Name = name; Alias = alias } in specifiers do
                let mutable found = false

                let exportName =
                  match alias with
                  | Some a -> a
                  | None -> name

                match env.TryFindValue name with
                | Some binding ->
                  ns <- ns.AddBinding exportName binding
                  found <- true
                | None -> ()

                match env.TryFindScheme name with
                | Some scheme ->
                  ns <- ns.AddScheme exportName scheme
                  found <- true
                | None -> ()

                match env.Namespace.Namespaces.TryFind name with
                | Some value ->
                  ns <- ns.AddNamespace exportName value
                  found <- true
                | None -> ()

                if found then
                  failwith $"Couldn't find '{name}' to export"
            | ExportDefault expr ->
              match expr.Kind with
              | ExprKind.Identifier { Name = name } ->
                match env.TryFindScheme name with
                | Some scheme -> ns <- ns.AddScheme "default" scheme
                | None -> ()
              | _ -> failwith "TODO: handle default export"
              // TODO: figure out what to do with default exports
              ()
            | ExportAll { Src = src } ->
              let! resolvedPath = this.resolvePath packageRoot env.Filename src
              let mutable resolvedPath = resolvedPath

              if resolvedPath.EndsWith(".js") then
                resolvedPath <- Path.ChangeExtension(resolvedPath, ".d.ts")

              if not (Path.HasExtension resolvedPath) then
                resolvedPath <- Path.ChangeExtension(resolvedPath, ".d.ts")

              printfn $"resolvedPath = {resolvedPath}"

              let! exportNs =
                asyncResult {
                  if resolvedPath.EndsWith(".d.ts") then
                    if cachedModules.ContainsKey resolvedPath then
                      return cachedModules[resolvedPath]
                    else
                      let! modEnv, modAst =
                        this.inferLib ctx (getGlobalEnv ()) resolvedPath

                      let! ns =
                        this.getLibExports
                          ctx
                          modEnv
                          packageRoot
                          "<exports>"
                          modAst.Items

                      cachedModules <- cachedModules.Add(resolvedPath, ns)
                      return ns
                  else
                    printfn $"resolvedPath = {resolvedPath}"

                    return!
                      AsyncResult.ofResult (
                        Error(
                          TypeError(
                            NotImplemented "TODO: getLibExports - ExportAll"
                          )
                        )
                      )
                }

              for KeyValue(key, binding) in exportNs.Values do
                ns <- ns.AddBinding key binding

              for KeyValue(key, scheme) in exportNs.Schemes do
                ns <- ns.AddScheme key scheme

              for KeyValue(key, value) in exportNs.Namespaces do
                ns <- ns.AddNamespace key value
          | ModuleItem.Stmt stmt ->
            match stmt.Kind with
            | StmtKind.Decl decl ->
              match decl.Kind with
              | DeclKind.ClassDecl { Name = name; Export = export } ->
                if export then
                  let! t =
                    AsyncResult.ofResult (
                      env.GetValue name
                      |> Result.mapError CompileError.TypeError
                    )

                  let binding =
                    { Type = t
                      Mutable = false
                      Export = export }

                  ns <- ns.AddBinding name binding

                  let! scheme =
                    AsyncResult.ofResult (
                      env.GetScheme(Common.QualifiedIdent.Ident name)
                      |> Result.mapError CompileError.TypeError
                    )

                  ns <- ns.AddScheme name scheme
              | DeclKind.FnDecl { Name = name; Export = export } ->
                if export then
                  let! t =
                    AsyncResult.ofResult (
                      env.GetValue name
                      |> Result.mapError CompileError.TypeError
                    )

                  let binding =
                    { Type = t
                      Mutable = false
                      Export = export }

                  ns <- ns.AddBinding name binding
              | DeclKind.VarDecl { Pattern = pattern; Export = export } ->
                if export then
                  let names = Helpers.findBindingNames pattern

                  for name in names do
                    let! t =
                      AsyncResult.ofResult (
                        env.GetValue name
                        |> Result.mapError CompileError.TypeError
                      )

                    let binding =
                      { Type = t
                        Mutable = false // TODO: figure out how to determine mutability
                        Export = export }

                    ns <- ns.AddBinding name binding
              // | DeclKind.Using usingDecl -> failwith "TODO: getExports - usingDecl"
              | DeclKind.InterfaceDecl { Name = name; Export = export } ->
                if export then
                  let! scheme =
                    AsyncResult.ofResult (
                      env.GetScheme(Common.QualifiedIdent.Ident name)
                      |> Result.mapError CompileError.TypeError
                    )

                  ns <- ns.AddScheme name scheme
              | DeclKind.TypeDecl { Name = name; Export = export } ->
                if export then
                  let! scheme =
                    AsyncResult.ofResult (
                      env.GetScheme(Common.QualifiedIdent.Ident name)
                      |> Result.mapError CompileError.TypeError
                    )

                  ns <- ns.AddScheme name scheme
              | DeclKind.EnumDecl tsEnumDecl ->
                failwith "TODO: getExports - tsEnumDecl"
              | DeclKind.NamespaceDecl { Name = name; Export = export } ->
                if name = "global" then
                  // TODO: figure out what we want to do with globals
                  // Maybe we can add these to `env` and have `getExports` return
                  // both a namespace and an updated env
                  ()
                else if export then
                  match env.Namespace.Namespaces.TryFind name with
                  | Some value -> ns <- ns.AddNamespace name value
                  | None -> failwith $"Couldn't find namespace: '{name}'"

            | StmtKind.Expr expr -> failwith "todo"
            | StmtKind.For _ -> failwith "todo"
            | StmtKind.Return exprOption -> failwith "todo"

        return ns
      }

    member this.getModuleExports
      (ctx: Ctx)
      (env: Env)
      (resolvedImportPath: string) // TODO: remove
      (m: Module)
      : Async<Result<Namespace, TypeError>> =
      asyncResult {
        let! moduleEnv = InferModule.inferModule ctx env m

        let mutable exports = Namespace.empty

        for item in m.Items do
          match item with
          | Stmt { Kind = Decl decl } ->
            match decl.Kind with
            | TypeDecl { Name = name; Export = export } ->
              if export then
                match moduleEnv.TryFindScheme name with
                | Some(scheme) -> exports <- exports.AddScheme name scheme
                | None -> failwith $"scheme {name} not found"
            | VarDecl { Pattern = pattern; Export = export } ->
              if export then
                let names = this.findBindingNames pattern

                for name in names do
                  match moduleEnv.TryFindValue name with
                  | Some(binding) -> exports <- exports.AddBinding name binding
                  | None -> failwith $"value {name} not found"
            | FnDecl { Name = name; Export = export } ->
              if export then
                match moduleEnv.TryFindValue name with
                | Some(binding) -> exports <- exports.AddBinding name binding
                | None -> failwith $"value {name} not found"
            | ClassDecl { Name = name; Export = export } ->
              if export then
                match moduleEnv.TryFindScheme name with
                | Some(scheme) -> exports <- exports.AddScheme name scheme
                | None -> failwith $"scheme {name} not found"

                match moduleEnv.TryFindValue name with
                | Some(binding) -> exports <- exports.AddBinding name binding
                | None -> failwith $"value {name} not found"
            | InterfaceDecl { Name = name; Export = export } ->
              if export then
                match moduleEnv.TryFindScheme name with
                | Some(scheme) -> exports <- exports.AddScheme name scheme
                | None -> failwith $"scheme {name} not found"
            | EnumDecl { Name = name; Export = export } ->
              if export then
                match moduleEnv.TryFindScheme name with
                | Some(scheme) -> exports <- exports.AddScheme name scheme
                | None -> failwith $"scheme {name} not found"

                match moduleEnv.TryFindValue name with
                | Some(binding) -> exports <- exports.AddBinding name binding
                | None -> failwith $"value {name} not found"
            | NamespaceDecl { Name = name; Export = export } ->
              if export then
                match moduleEnv.Namespace.Namespaces.TryFind name with
                | Some(ns) -> exports <- exports.AddNamespace name ns
                | None -> failwith $"namespace {name} not found"
          | _ -> ()

        return exports
      }

    member this.getCtx
      (packageRoot: string)
      (getGlobalEnv: unit -> Env)
      : Result<Ctx, CompileError> =

      let getExports ctx filename import =
        asyncResult {
          let! resolvedPath = this.resolvePath packageRoot filename import.Path

          let! exportNs =
            asyncResult {
              if resolvedPath.EndsWith(".d.ts") then
                if cachedModules.ContainsKey resolvedPath then
                  return cachedModules[resolvedPath]
                else
                  let! modEnv, modAst =
                    this.inferLib ctx (getGlobalEnv ()) resolvedPath

                  let! ns =
                    this.getLibExports
                      ctx
                      modEnv
                      packageRoot
                      "<exports>"
                      modAst.Items

                  cachedModules <- cachedModules.Add(resolvedPath, ns)
                  return ns
              else
                let! resolvedPath =
                  this.resolvePath packageRoot filename import.Path

                let resolvedImportPath =
                  Path.ChangeExtension(resolvedPath, ".esc")

                let! contents = fs.ReadAllTextAsync resolvedImportPath

                let m =
                  match Escalier.Parser.Parser.parseModule contents with
                  | Ok value -> value
                  | Error _ -> failwith $"failed to parse {resolvedImportPath}"

                let env =
                  { getGlobalEnv () with
                      Filename = filename }

                return!
                  this.getModuleExports ctx env resolvedImportPath m
                  |> AsyncResult.mapError CompileError.TypeError
            }
            |> AsyncResult.mapError (fun err ->
              match err with
              | TypeError err -> err
              | ParseError err -> failwith "This should never happen.")

          return exportNs
        }

      result {
        let ctx =
          Ctx(
            getExports,
            (fun ctx filename import ->
              this.resolvePath packageRoot filename import.Path),
            InferExpr.inferExpr,
            InferModule.inferModuleItems,
            InferPattern.inferPattern,
            InferTypeAnn.inferTypeAnn,
            InferClass.inferClass
          )

        return ctx
      }

    member this.getEnvAndCtx
      (baseDir: string)
      : Async<Result<Ctx * Env, CompileError>> =
      asyncResult {
        match memoizedEnvAndCtx.TryFind baseDir with
        | Some(result) ->
          let! ctx, env = result
          let ctx = ctx.Clone
          return ctx, env
        | None ->
          let env = getGlobalEnvMemoized ()
          let mutable newEnv = env

          // QUESTION: How do we make sure that the environment being used by
          // ctx is update to date.
          let! ctx = this.getCtx baseDir (fun _ -> newEnv)

          let libs =
            [ "lib.es5.d.ts"
              "lib.es2015.core.d.ts"
              "lib.es2015.collection.d.ts"
              "lib.es2015.symbol.d.ts"
              "lib.es2015.symbol.wellknown.d.ts"
              "lib.es2015.iterable.d.ts"
              "lib.es2015.generator.d.ts"
              // TODO: modify Promise types to include type param for rejections
              // "lib.es2015.promise.d.ts"
              "lib.es2015.proxy.d.ts"
              // "lib.es2015.reflect.d.ts"

              // TODO: make inclusion of this file optional
              // "lib.dom.d.ts"
              ]

          let! packageRoot = this.findNearestAncestorWithNodeModules baseDir
          let nodeModulesDir = Path.Combine(packageRoot, "node_modules")
          let tsLibDir = Path.Combine(nodeModulesDir, "typescript/lib")

          for lib in libs do
            let fullPath = Path.Combine(tsLibDir, lib)
            let! env, _ = this.inferLib ctx newEnv fullPath
            newEnv <- env

          // TODO: handle schemes within namespaces
          let readonlySchemes =
            newEnv.Namespace.Schemes
            |> Map.filter (fun k _ ->
              (k.StartsWith "Readonly" || k.EndsWith "ReadOnly")
              && k <> "Readonly")

          for KeyValue(readonlyName, readonlyScheme) in readonlySchemes do
            let name =
              readonlyName.Replace("Readonly", "").Replace("ReadOnly", "")

            match newEnv.TryFindScheme name with
            | Some(scheme) ->
              let merged =
                QualifiedGraph.mergeType readonlyScheme.Type scheme.Type

              // TODO: track which TypeScript interface decls each of the properties
              // come from in the merged type.
              newEnv <- newEnv.AddScheme name { scheme with Type = merged }
              ()
            | _ -> ()

          let result = Result.Ok(ctx, newEnv)
          memoizedEnvAndCtx <- memoizedEnvAndCtx.Add(baseDir, result)

          return! result
      }

    member this.printDiagnostic (writer: TextWriter) (d: Diagnostic) =
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

    member this.printDiagnostics (writer: TextWriter) (ds: list<Diagnostic>) =
      match ds with
      | [] -> ()
      | d :: ds ->
        this.printDiagnostic writer d
        this.printDiagnostics writer ds

    /// <summary>
    /// Compiles the source code located at the specified base directory.
    /// </summary>
    /// <param name="textwriter">The text writer to use for diagnostics.</param>
    /// <param name="baseDir">The base directory where the source code is located.</param>
    /// <param name="srcFile">The name of the source code file.</param>
    /// <returns>The compiled code.</returns>
    member this.compileFile
      (textwriter: TextWriter)
      (baseDir: string)
      (srcFile: string)
      =
      asyncResult {
        let filename = srcFile
        let! contents = fs.ReadAllTextAsync filename
        let! ctx, env = this.getEnvAndCtx baseDir

        let! ast =
          Escalier.Parser.Parser.parseModule contents
          |> Result.mapError CompileError.ParseError

        let env = { env with Filename = srcFile }

        let! env =
          InferModule.inferModule ctx env ast
          |> AsyncResult.mapError CompileError.TypeError

        this.printDiagnostics textwriter ctx.Report.Diagnostics

        let printCtx: Printer.PrintCtx = { Indent = 0; Precedence = 0 }

        let buildCtx =
          { Codegen.NextTempId = 0
            Codegen.AutoImports = Set.empty }

        let mod' = Codegen.buildModule buildCtx ast
        let js = Printer.printModule printCtx mod'
        let outJsName = Path.ChangeExtension(filename, ".js")
        do! fs.WriteAllTextAsync(outJsName, js)

        let mod' = Codegen.buildModuleTypes env buildCtx ast
        let dts = Printer.printModule printCtx mod'
        let outDtsName = Path.ChangeExtension(filename, ".d.ts")
        do! fs.WriteAllTextAsync(outDtsName, dts)

        return ()
      }

    // TODO: dedupe with `compileFile`.
    member this.compileString
      (textwriter: TextWriter)
      (baseDir: string)
      (srcCode: string)
      : Async<Result<string * string, CompileError>> =
      asyncResult {
        let! ctx, env = this.getEnvAndCtx baseDir

        let! ast =
          Escalier.Parser.Parser.parseModule srcCode
          |> Result.mapError CompileError.ParseError

        let env = { env with Filename = "./entry.esc" }

        let! env =
          InferModule.inferModule ctx env ast
          |> AsyncResult.mapError CompileError.TypeError

        this.printDiagnostics textwriter ctx.Report.Diagnostics

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

    member this.findFiles (baseDir: string) (entryFile: string) =
      async {
        let mutable paths = [ entryFile ]

        let rec findFilesRec (entryFile: string) =
          async {
            let! contents = fs.ReadAllTextAsync entryFile

            let m =
              match Escalier.Parser.Parser.parseModule contents with
              | Ok value -> value
              | Error _ -> failwith $"failed to parse {entryFile}"

            for item in m.Items do
              match item with
              | Import import ->
                let path =
                  match import.Path[0] with
                  | '.' ->
                    Path.GetFullPath(
                      Path.Join(Path.GetDirectoryName entryFile, import.Path)
                    )
                  | '~' ->
                    Path.GetFullPath(
                      Path.Join(baseDir, import.Path.Substring 2)
                    )
                  | _ -> failwith $"TODO - import.Path = {import.Path}"

                let path = Path.ChangeExtension(path, "esc")

                if not (List.contains path paths) then
                  paths <- path :: paths
                  do! findFilesRec path
                else
                  ()
              | Export export -> printfn "TOOD - handle re-exports"
              | Stmt stmt -> ()
          }

        do! findFilesRec entryFile

        return paths
      }

  let TestFileSystem = makeFileSystem ()
  let TestCompiler = Compiler TestFileSystem
