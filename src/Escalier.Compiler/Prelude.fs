namespace Escalier.Compiler

open FParsec.Error
open FsToolkit.ErrorHandling
open FSharp.Data
open System.IO

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.TypeChecker
open Escalier.TypeChecker.Error
open Escalier.TypeChecker.ExprVisitor
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
      Path.GetFullPath(
        Path.Join(Path.GetDirectoryName(currentPath), importPath)
      )
    else
      // TODO: once this is implemented, move it over to Escalier.Interop
      // TODO: check if there's `/` in the import path, if so, the first
      // part before the `/` is the name of the module and the rost is a
      // path to a .d.ts file within the module.

      // determine if importPath contains a '/' and split on it
      // if it does, the first part is the module name and the second part is
      // the path to the .d.ts file within the module

      let moduleName, subpath =
        match importPath.Split('/') |> List.ofArray with
        | [] -> failwith "This should never happen."
        | [ name ] -> name, None
        | name :: path -> name, Some(String.concat "/" path)

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
          Path.Combine(Path.GetDirectoryName(pkgJsonPath), types)
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

    walkPattern visitor () p

    List.rev names

  // TODO: dedupe with Escalier.Interop.Infer
  let private findScriptBindingNames (m: Syntax.Module) : list<string> =
    let mutable names: list<string> = []

    for item in m.Items do
      match item with
      | Syntax.ModuleItem.Stmt stmt ->
        match stmt.Kind with
        | Syntax.StmtKind.Decl({ Kind = Syntax.DeclKind.VarDecl { Pattern = pattern } }) ->
          names <- List.concat [ names; findBindingNames pattern ]
        | _ -> ()
      | _ -> ()

    names

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

    let arithemtic (op: string) =
      (makeFunctionType
        (Some [ tpA; tpB ])
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        { Kind = TypeKind.Binary(typeRefA, op, typeRefB)
          Provenance = None }
        never,
       false)

    let unaryArithmetic (op: string) =
      (makeFunctionType
        (Some [ tpA ])
        [ makeParam "arg" typeRefA ]
        { Kind = TypeKind.Unary(op, typeRefA)
          Provenance = None }
        never,
       false)

    let comparison (op: string) =
      (makeFunctionType
        (Some [ tpA; tpB ])
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        { Kind = TypeKind.Binary(typeRefA, op, typeRefB)
          Provenance = None }
        never,
       false)

    let logical =
      (makeFunctionType
        None
        [ makeParam "left" boolType; makeParam "right" boolType ]
        boolType
        never,
       false)

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
      (makeFunctionType
        (Some(typeParams))
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        boolType
        never,
       false)

    let typeParams: list<TypeParam> =
      [ { Name = "A"
          Constraint = None
          Default = None } ]

    let unaryLogic (op: string) =
      (makeFunctionType
        (Some(typeParams))
        [ makeParam "arg" typeRefA ]
        { Kind = TypeKind.Unary(op, typeRefA)
          Provenance = None }
        never,
       false)

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
      (makeFunctionType
        (Some [ tpA; tpB ])
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        { Kind = TypeKind.Binary(typeRefA, "++", typeRefB)
          Provenance = None }
        never,
       false)

    let binaryOps =
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

    ns <- ns.AddBinding "globalThis" (t, false)

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
      let input =
        input.Replace("__html: string | TrustedHTML;", "__html: string;")

      // TODO: handle <reference path="global.d.ts" /> in @types/react/index.d.ts
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

  let mutable envMemoized: Env option = None

  let getGlobalEnvMemoized () =
    match envMemoized with
    | Some(e) -> e
    | None ->
      let env = getGlobalEnv ()
      envMemoized <- Some(env)
      env

  let mutable cachedModules: Map<string, Namespace> = Map.empty

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
                      QualifiedGraph.getExports
                        ctx
                        modEnv
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
                // TODO: extract into a separate function
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

                // TODO: we should probably be using `inferModule` here
                // TODO: update `inferScript to also return just the new symbols
                // scriptEnv
                let scriptEnv =
                  let env =
                    { getGlobalEnv () with
                        Filename = filename }

                  match Infer.inferModule ctx env m with
                  | Ok value -> value
                  | Error err ->
                    printfn "err = %A" err
                    failwith $"failed to infer {resolvedImportPath}"

                // exportEnv
                let mutable exports = Namespace.empty

                let bindings = findScriptBindingNames m

                for name in bindings do
                  match scriptEnv.TryFindValue name with
                  // NOTE: exports are immutable
                  | Some(t, isMut) ->
                    exports <- exports.AddBinding name (t, false)
                  | None -> failwith $"binding {name} not found"

                for item in m.Items do
                  match item with
                  | Syntax.ModuleItem.Stmt { Kind = Syntax.StmtKind.Decl { Kind = Syntax.DeclKind.TypeDecl { Name = name } } } ->
                    match scriptEnv.TryFindScheme name with
                    | Some(scheme) -> exports <- exports.AddScheme name scheme
                    | None -> failwith $"scheme {name} not found"
                  | _ -> ()

                exports

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

          printfn $"merging {name} with {readonlyName}"

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
