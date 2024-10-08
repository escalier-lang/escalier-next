namespace Escalier.Compiler

open Escalier.Data.Syntax
open FParsec.Error
open FsToolkit.ErrorHandling
open FSharp.Data
open System.IO

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.Data.Visitor
open Escalier.TypeChecker
open Escalier.TypeChecker.Error
open Escalier.Interop

open Env

// TODO: move the definitions in the prelude into its own source file so that
// Provenance can be set to something the appropriate AST nodes from that file.
// TODO: turn this into a class
module Prelude =
  type CompileError =
    | ParseError of ParserError
    | TypeError of TypeError

    override x.ToString() =
      match x with
      | ParseError err -> $"ParseError: {err}"
      | TypeError err -> $"TypeError: {err}"

  let mutable memoizedNodeModulesDir: Map<string, string> = Map.empty

  let rec findNearestAncestorWithNodeModules (currentDir: string) =
    match memoizedNodeModulesDir.TryFind currentDir with
    | Some(nodeModulesDir) -> nodeModulesDir
    | None ->
      let nodeModulesDir = Path.Combine(currentDir, "node_modules")

      if Directory.Exists(nodeModulesDir) then
        currentDir
      else
        let parentDir = Directory.GetParent(currentDir)

        match parentDir with
        | null ->
          failwith "node_modules directory not found in any ancestor directory."
        | _ -> findNearestAncestorWithNodeModules parentDir.FullName

  let private packageJsonHasTypes (packageJsonPath: string) : bool =
    if File.Exists packageJsonPath then
      let packageJson = File.ReadAllText(packageJsonPath)
      let packageJsonObj = JsonValue.Parse(packageJson)

      match packageJsonObj.TryGetProperty("types") with
      | None -> false
      | Some _ -> true
    else
      false

  let private resolvePath
    (projectRoot: string)
    (currentPath: string)
    (importPath: string)
    : string =
    if importPath.StartsWith "~" then
      Path.GetFullPath(Path.Join(projectRoot, importPath.Substring(1)))
    else if importPath.StartsWith "." then
      let resolvedPath =
        Path.GetFullPath(
          Path.Join(Path.GetDirectoryName(currentPath), importPath)
        )

      if currentPath.EndsWith(".d.ts") then
        Path.ChangeExtension(resolvedPath, ".d.ts")
      else
        resolvedPath
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

      let rootDir = findNearestAncestorWithNodeModules projectRoot
      let nodeModulesDir = Path.Combine(rootDir, "node_modules")

      let pkgJsonPath1 =
        Path.Combine(nodeModulesDir, moduleName, "package.json")

      let pkgJsonPath2 =
        Path.Combine(nodeModulesDir, "@types", moduleName, "package.json")

      let pkgJsonPath =
        if packageJsonHasTypes pkgJsonPath1 then
          pkgJsonPath1
        elif packageJsonHasTypes pkgJsonPath2 then
          pkgJsonPath2
        else
          failwith
            $"package.json not found for module {moduleName}, rootDir = {rootDir}, nodeModulesDir = {nodeModulesDir}."

      // read package.json and parse it
      let pkgJson = File.ReadAllText(pkgJsonPath)
      let pkgJsonObj = JsonValue.Parse(pkgJson)

      match subpath with
      | None ->
        match pkgJsonObj.TryGetProperty("types") with
        | None -> failwith "Invalid package.json: missing `types` field."
        | Some value ->
          let types = value.InnerText()

          if types.EndsWith(".d.ts") then
            Path.Combine(Path.GetDirectoryName(pkgJsonPath), types)
          else
            Path.Combine(Path.GetDirectoryName(pkgJsonPath), $"{types}.d.ts")
      // Path.Combine(Path.GetDirectoryName(pkgJsonPath), types)
      | Some value ->
        Path.Combine(Path.GetDirectoryName(pkgJsonPath), $"{value}.d.ts")

  // TODO: dedupe with Escalier.Interop.Infer
  let findBindingNames (p: Syntax.Pattern) : list<string> =
    let mutable names: list<string> = []

    let visitor =
      { ExprVisitor.VisitExpr =
          fun (expr, state) ->
            match expr.Kind with
            | Syntax.ExprKind.Function _ -> (false, state)
            | _ -> (true, state)
        ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
        ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
        ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
        ExprVisitor.VisitStmt = fun (_, state) -> (false, state)
        ExprVisitor.VisitPattern =
          fun (pat, state) ->
            match pat.Kind with
            | Syntax.PatternKind.Ident { Name = name } ->
              names <- name :: names
              (false, state)
            | _ -> (true, state)
        ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

    ExprVisitor.walkPattern visitor () p

    List.rev names

  let never =
    { Kind = TypeKind.Keyword Keyword.Never
      Provenance = None }

  let makeParam (name: string) (ty: Type) : FuncParam =
    { Pattern = Pattern.Identifier { Name = name; IsMut = false }
      Type = ty
      Optional = false }

  let getGlobalEnv () : Env =
    let tpA =
      { Name = "A"
        Constraint = Some(numType)
        Default = None }

    let tpB =
      { Name = "B"
        Constraint = Some(numType)
        Default = None }

    let typeRefA =
      { Kind =
          { Name = QualifiedIdent.Ident "A"
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let typeRefB =
      { Kind =
          { Name = QualifiedIdent.Ident "B"
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let arithemtic (op: string) : Binding =
      let t =
        makeFunctionType
          (Some [ tpA; tpB ])
          [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
          { Kind =
              TypeKind.Binary
                { Op = op
                  Left = typeRefA
                  Right = typeRefB }
            Provenance = None }
          never

      { Type = t
        Mutable = false
        Export = false }

    let unaryArithmetic (op: string) : Binding =
      let t =
        makeFunctionType
          (Some [ tpA ])
          [ makeParam "arg" typeRefA ]
          { Kind = TypeKind.Unary { Op = op; Arg = typeRefA }
            Provenance = None }
          never

      { Type = t
        Mutable = false
        Export = false }

    let comparison (op: string) : Binding =
      let t =
        makeFunctionType
          (Some [ tpA; tpB ])
          [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
          { Kind =
              TypeKind.Binary
                { Op = op
                  Left = typeRefA
                  Right = typeRefB }
            Provenance = None }
          never

      { Type = t
        Mutable = false
        Export = false }

    let logical =
      { Type =
          makeFunctionType
            None
            [ makeParam "left" boolType; makeParam "right" boolType ]
            boolType
            never
        Mutable = false
        Export = false }

    let typeRefA =
      { Kind = makeTypeRefKind (QualifiedIdent.Ident "A")
        Provenance = None }

    let typeRefB =
      { Kind = makeTypeRefKind (QualifiedIdent.Ident "B")
        Provenance = None }

    let typeParams: list<TypeParam> =
      [ { Name = "A"
          Constraint = None
          Default = None }
        { Name = "B"
          Constraint = None
          Default = None } ]

    // TODO: figure out how to make quality polymorphic
    let equality =
      { Type =
          makeFunctionType
            (Some(typeParams))
            [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
            boolType
            never
        Mutable = false
        Export = false }

    let typeParams: list<TypeParam> =
      [ { Name = "A"
          Constraint = None
          Default = None } ]

    let unaryLogic (op: string) =
      { Type =
          makeFunctionType
            (Some(typeParams))
            [ makeParam "arg" typeRefA ]
            { Kind = TypeKind.Unary { Op = op; Arg = typeRefA }
              Provenance = None }
            never
        Mutable = false
        Export = false }

    let tpA =
      { Name = "A"
        Constraint = Some(strType)
        Default = None }

    let tpB =
      { Name = "B"
        Constraint = Some(strType)
        Default = None }

    let typeRefA =
      { Kind =
          { Name = (QualifiedIdent.Ident "A")
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let typeRefB =
      { Kind =
          { Name = (QualifiedIdent.Ident "B")
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let stringConcat =
      { Type =
          makeFunctionType
            (Some [ tpA; tpB ])
            [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
            { Kind =
                TypeKind.Binary
                  { Op = "++"
                    Left = typeRefA
                    Right = typeRefB }
              Provenance = None }
            never
        Mutable = false
        Export = false }

    let binaryOps: Map<string, Binding> =
      Map.ofList
        [ ("+", arithemtic "+")
          ("++", stringConcat)
          ("-", arithemtic "-")
          ("*", arithemtic "*")
          ("/", arithemtic "/")
          ("%", arithemtic "%")
          ("**", arithemtic "**")
          ("<", comparison "<")
          ("<=", comparison "<=")
          (">", comparison ">")
          (">=", comparison ">=")
          ("==", equality)
          ("!=", equality)
          ("||", logical)
          ("&&", logical) ]

    let unaryOps =
      Map.ofList
        [ ("-", unaryArithmetic "-")
          ("+", unaryArithmetic "+")
          ("!", unaryLogic "!") ]

    let mutable ns = Namespace.empty

    let t =
      { Kind = TypeKind.Keyword Keyword.GlobalThis
        Provenance = None }

    let binding =
      { Type = t
        Mutable = false
        Export = false }

    ns <- ns.AddBinding "globalThis" binding

    // TODO: add a global `gql` function that returns a typed result
    // gql should return a TypedDocumentNode<TResult, TVariables> from
    // https://github.com/dotansimha/graphql-typed-document-node/blob/master/packages/core/src/index.ts
    // we don't actually care what the shape of the DocumentNode is that
    // TypeDocumentNode<TResult, TVariable> extends.  For our purposes, it's
    // okay if we treat DocumentNode as an opaque type.
    //
    // when dealing with fragments, we need to extract the TRsult from the
    // TypedDocumentNode of the fragment and merge it with the result of the
    // query.
    //
    // To start with we can manually construct TResult and TVariables and test
    // that TypedDocumentNode and types for `useQuery` are working as expected.

    let mutable env =
      { Filename = "<empty>"
        Namespace = ns
        BinaryOps = binaryOps
        UnaryOps = unaryOps
        IsAsync = false
        IsPatternMatching = false }

    env

  let private inferLib
    (ctx: Ctx)
    (env: Env)
    (fullPath: string)
    : Result<Env * Syntax.Module, CompileError> =

    result {
      let input = File.ReadAllText(fullPath)

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
        Infer.inferModule ctx newEnv ast
        |> Result.mapError CompileError.TypeError

      return outEnv, ast
    }

  let mutable cachedModules: Map<string, Namespace> = Map.empty

  let rec getLibExports
    (ctx: Ctx)
    (env: Env)
    (projectRoot: string)
    (name: string)
    (items: list<Syntax.ModuleItem>)
    : Result<Namespace, TypeError> =

    result {
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
            let mutable resolvedPath = resolvePath projectRoot env.Filename src

            if resolvedPath.EndsWith(".js") then
              resolvedPath <- Path.ChangeExtension(resolvedPath, ".d.ts")

            if not (Path.HasExtension resolvedPath) then
              resolvedPath <- Path.ChangeExtension(resolvedPath, ".d.ts")

            let exportNs =
              if resolvedPath.EndsWith(".d.ts") then
                if cachedModules.ContainsKey resolvedPath then
                  cachedModules.[resolvedPath]
                else
                  let modEnv, modAst =
                    match inferLib ctx (getGlobalEnv ()) resolvedPath with
                    | Ok value -> value
                    | Error err ->
                      printfn "err = %A" err
                      failwith $"failed to infer {resolvedPath}"

                  let ns =
                    match
                      getLibExports
                        ctx
                        modEnv
                        projectRoot
                        "<exports>"
                        modAst.Items
                    with
                    | Ok value -> value
                    | Error err ->
                      printfn "err = %A" err
                      failwith $"failed to get exports from {resolvedPath}"

                  cachedModules <- cachedModules.Add(resolvedPath, ns)
                  ns
              else
                printfn $"resolvedPath = {resolvedPath}"
                failwith "TODO: getLibExports - NamedExport"

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
            let mutable resolvedPath = resolvePath projectRoot env.Filename src

            if resolvedPath.EndsWith(".js") then
              resolvedPath <- Path.ChangeExtension(resolvedPath, ".d.ts")

            if not (Path.HasExtension resolvedPath) then
              resolvedPath <- Path.ChangeExtension(resolvedPath, ".d.ts")

            printfn $"resolvedPath = {resolvedPath}"

            let exportNs =
              if resolvedPath.EndsWith(".d.ts") then
                if cachedModules.ContainsKey resolvedPath then
                  cachedModules.[resolvedPath]
                else
                  let modEnv, modAst =
                    match inferLib ctx (getGlobalEnv ()) resolvedPath with
                    | Ok value -> value
                    | Error err ->
                      printfn "err = %A" err
                      failwith $"failed to infer {resolvedPath}"

                  let ns =
                    match
                      getLibExports
                        ctx
                        modEnv
                        projectRoot
                        "<exports>"
                        modAst.Items
                    with
                    | Ok value -> value
                    | Error err ->
                      printfn "err = %A" err
                      failwith $"failed to get exports from {resolvedPath}"

                  cachedModules <- cachedModules.Add(resolvedPath, ns)
                  ns
              else
                printfn $"resolvedPath = {resolvedPath}"
                failwith "TODO: getLibExports - ExportAll"

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
                let! t = env.GetValue name

                let binding =
                  { Type = t
                    Mutable = false
                    Export = export }

                ns <- ns.AddBinding name binding

                let! scheme = env.GetScheme(Common.QualifiedIdent.Ident name)
                ns <- ns.AddScheme name scheme
            | DeclKind.FnDecl { Name = name; Export = export } ->
              if export then
                let! t = env.GetValue name

                let binding =
                  { Type = t
                    Mutable = false
                    Export = export }

                ns <- ns.AddBinding name binding
            | DeclKind.VarDecl { Pattern = pattern; Export = export } ->
              if export then
                let names = Helpers.findBindingNames pattern

                for name in names do
                  let! t = env.GetValue name

                  let binding =
                    { Type = t
                      Mutable = false // TODO: figure out how to determine mutability
                      Export = export }

                  ns <- ns.AddBinding name binding
            // | DeclKind.Using usingDecl -> failwith "TODO: getExports - usingDecl"
            | DeclKind.InterfaceDecl { Name = name; Export = export } ->
              if export then
                let! scheme = env.GetScheme(Common.QualifiedIdent.Ident name)
                ns <- ns.AddScheme name scheme
            | DeclKind.TypeDecl { Name = name; Export = export } ->
              if export then
                let! scheme = env.GetScheme(Common.QualifiedIdent.Ident name)
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

  let getModuleExports
    (ctx: Ctx)
    (env: Env)
    (resolvedImportPath: string)
    (m: Module)
    =
    let moduleEnv =
      match Infer.inferModule ctx env m with
      | Ok value -> value
      | Error err ->
        printfn "err = %A" err
        failwith $"failed to infer {resolvedImportPath}"

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
            let names = findBindingNames pattern

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

    exports

  let mutable envMemoized: Env option = None

  let getGlobalEnvMemoized () =
    match envMemoized with
    | Some(e) -> e
    | None ->
      let env = getGlobalEnv ()
      envMemoized <- Some(env)
      env

  let getCtx
    (projectRoot: string)
    (getGlobalEnv: unit -> Env)
    : Result<Ctx, CompileError> =

    result {
      let ctx =
        Ctx(
          (fun ctx filename import ->
            let resolvedPath = resolvePath projectRoot filename import.Path

            let exportNs =
              if resolvedPath.EndsWith(".d.ts") then
                if cachedModules.ContainsKey resolvedPath then
                  cachedModules.[resolvedPath]
                else
                  let modEnv, modAst =
                    match inferLib ctx (getGlobalEnv ()) resolvedPath with
                    | Ok value -> value
                    | Error err ->
                      printfn "err = %A" err
                      failwith $"failed to infer {resolvedPath}"

                  let ns =
                    match
                      getLibExports
                        ctx
                        modEnv
                        projectRoot
                        "<exports>"
                        modAst.Items
                    with
                    | Ok value -> value
                    | Error err ->
                      printfn "err = %A" err
                      failwith $"failed to get exports from {resolvedPath}"

                  cachedModules <- cachedModules.Add(resolvedPath, ns)
                  ns
              else
                let resolvedImportPath =
                  Path.ChangeExtension(
                    resolvePath projectRoot filename import.Path,
                    ".esc"
                  )

                let contents = File.ReadAllText(resolvedImportPath)

                let m =
                  match Escalier.Parser.Parser.parseModule contents with
                  | Ok value -> value
                  | Error _ -> failwith $"failed to parse {resolvedImportPath}"

                let env =
                  { getGlobalEnv () with
                      Filename = filename }

                getModuleExports ctx env resolvedImportPath m

            exportNs),
          (fun ctx filename import ->
            resolvePath projectRoot filename import.Path)
        )

      return ctx
    }

  let mutable memoizedEnvAndCtx: Map<string, Result<Ctx * Env, CompileError>> =
    Map.empty

  let getEnvAndCtx (baseDir: string) : Result<Ctx * Env, CompileError> =
    result {
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
        let! ctx = getCtx baseDir (fun _ -> newEnv)

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
            "lib.dom.d.ts" ]

        let packageRoot = findNearestAncestorWithNodeModules baseDir
        let nodeModulesDir = Path.Combine(packageRoot, "node_modules")
        let tsLibDir = Path.Combine(nodeModulesDir, "typescript/lib")

        for lib in libs do
          let fullPath = Path.Combine(tsLibDir, lib)
          let! env, _ = inferLib ctx newEnv fullPath
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
