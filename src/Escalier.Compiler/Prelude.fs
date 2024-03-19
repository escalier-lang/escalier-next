namespace Escalier.Compiler

open FParsec.Error
open FsToolkit.ErrorHandling
open FSharp.Data
open System.IO

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.Parser
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
      let rootDir = findNearestAncestorWithNodeModules projectRoot
      let nodeModulesDir = Path.Combine(rootDir, "node_modules")

      let pkgJsonPath =
        Path.Combine(rootDir, "node_modules", importPath, "package.json")

      if File.Exists pkgJsonPath then
        // read package.json and parse it
        let pkgJson = File.ReadAllText(pkgJsonPath)
        let pkgJsonObj = JsonValue.Parse(pkgJson)

        match pkgJsonObj.TryGetProperty("types") with
        | None -> failwith "Invalid package.json: missing `types` field."
        | Some value ->
          let types = value.InnerText()
          Path.Combine(Path.GetDirectoryName(pkgJsonPath), types)
      else
        let pkgJsonPath =
          Path.Combine(nodeModulesDir, "@types", importPath, "package.json")

        // read package.json and parse it
        let pkgJson = File.ReadAllText(pkgJsonPath)
        let pkgJsonObj = JsonValue.Parse(pkgJson)

        match pkgJsonObj.TryGetProperty("types") with
        | None -> failwith "Invalid package.json: missing `types` field."
        | Some value ->
          let types = value.InnerText()
          Path.Combine(Path.GetDirectoryName(pkgJsonPath), types)

  // TODO: dedupe with Escalier.Interop.Infer
  let findBindingNames (p: Syntax.Pattern) : list<string> =
    let mutable names: list<string> = []

    let visitor =
      { ExprVisitor.VisitExpr =
          fun expr ->
            match expr.Kind with
            | Syntax.ExprKind.Function _ -> false
            | _ -> true
        ExprVisitor.VisitStmt = fun _ -> false
        ExprVisitor.VisitPattern =
          fun pat ->
            match pat.Kind with
            | Syntax.PatternKind.Ident { Name = name } ->
              names <- name :: names
              false
            | _ -> true
        ExprVisitor.VisitTypeAnn = fun _ -> false }

    walkPattern visitor p

    List.rev names

  // TODO: dedupe with Escalier.Interop.Infer
  let private findScriptBindingNames (m: Syntax.Script) : list<string> =
    let mutable names: list<string> = []

    for item in m.Items do
      match item with
      | Syntax.Stmt stmt ->
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

    let mutable env =
      { Env.empty with
          Env.BinaryOps = binaryOps
          Env.UnaryOps = unaryOps }

    env

  let private inferLib
    (ctx: Ctx)
    (env: Env)
    (fullPath: string)
    : Result<Env * TypeScript.Module, CompileError> =

    result {
      let input = File.ReadAllText(fullPath)

      let! ast =
        match Parser.parseModule input with
        | FParsec.CharParsers.Success(value, _, _) -> Result.Ok(value)
        | FParsec.CharParsers.Failure(_, parserError, _) ->
          Result.mapError CompileError.ParseError (Result.Error(parserError))

      let! env =
        Infer.inferModule ctx env ast |> Result.mapError CompileError.TypeError

      return env, ast
    }

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
    (globalEnv: Env)
    : Result<Ctx, CompileError> =

    result {
      let ctx =
        Ctx(
          (fun ctx filename import ->
            let resolvedPath = resolvePath projectRoot filename import.Path

            let exportEnv =
              if resolvedPath.EndsWith(".d.ts") then
                let modEnv, modAst =
                  match inferLib ctx globalEnv resolvedPath with
                  | Ok value -> value
                  | Error err ->
                    printfn "err = %A" err
                    failwith $"failed to infer {resolvedPath}"

                let ns =
                  match
                    Infer.getExports ctx modEnv "<exports>" modAst.Body
                  with
                  | Ok value -> value
                  | Error err ->
                    printfn "err = %A" err
                    failwith $"failed to get exports from {resolvedPath}"

                { Env.empty with Namespace = ns }
              else
                // TODO: extract into a separate function
                let resolvedImportPath =
                  Path.ChangeExtension(
                    resolvePath projectRoot filename import.Path,
                    ".esc"
                  )

                let contents = File.ReadAllText(resolvedImportPath)

                let m =
                  match Parser.parseScript contents with
                  | Ok value -> value
                  | Error _ -> failwith $"failed to parse {resolvedImportPath}"

                // TODO: we should probably be using `inferModule` here
                // TODO: update `inferScript to also return just the new symbols
                // scriptEnv
                let scriptEnv =
                  match Infer.inferScript ctx globalEnv filename m with
                  | Ok value -> value
                  | Error err ->
                    printfn "err = %A" err
                    failwith $"failed to infer {resolvedImportPath}"

                // exportEnv
                let mutable exportEnv = Env.Env.empty

                let bindings = findScriptBindingNames m

                for name in bindings do
                  match scriptEnv.TryFindValue name with
                  // NOTE: exports are immutable
                  | Some(t, isMut) ->
                    exportEnv <- exportEnv.AddValue name (t, false)
                  | None -> failwith $"binding {name} not found"

                for item in m.Items do
                  match item with
                  | Syntax.Stmt { Kind = Syntax.StmtKind.Decl { Kind = Syntax.DeclKind.TypeDecl { Name = name } } } ->
                    match scriptEnv.TryFindScheme name with
                    | Some(scheme) ->
                      exportEnv <- exportEnv.AddScheme name scheme
                    | None -> failwith $"scheme {name} not found"
                  | _ -> ()

                exportEnv

            exportEnv),
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
        let! ctx = getCtx baseDir env

        let libs =
          [ "lib.es5.d.ts"
            "lib.es2015.core.d.ts"
            "lib.es2015.symbol.d.ts"
            "lib.es2015.symbol.wellknown.d.ts"
            "lib.es2015.iterable.d.ts"
            "lib.es2015.generator.d.ts"
            // TODO: modify Promise types to include type param for rejections
            // "lib.es2015.promise.d.ts"
            "lib.es2015.proxy.d.ts"
            // "lib.es2015.reflect.d.ts"
            "lib.dom.d.ts" ]

        let mutable newEnv = env

        let packageRoot = findNearestAncestorWithNodeModules baseDir
        let nodeModulesDir = Path.Combine(packageRoot, "node_modules")
        let tsLibDir = Path.Combine(nodeModulesDir, "typescript/lib")

        for lib in libs do
          let fullPath = Path.Combine(tsLibDir, lib)
          let! env, _ = inferLib ctx newEnv fullPath
          newEnv <- env

        // TODO: look for more (Readonly)Foo pairs once we parse lib.es6.d.ts and
        // future versions of the JavaScript standard library type defs
        match
          newEnv.TryFindScheme "ReadonlyArray", newEnv.TryFindScheme "Array"
        with
        | Some(readonlyArray), Some(array) ->
          let merged = Infer.mergeType readonlyArray.Type array.Type
          newEnv <- newEnv.AddScheme "Array" { array with Type = merged }

          // TODO: for type definitions using Array and ReadonlyArray we need to
          // make sure that params are marked with `mut` appropriately and all
          // references to ReadonlyArray must be replaced with Array
          newEnv <-
            { newEnv with
                Namespace =
                  { newEnv.Namespace with
                      Schemes = newEnv.Namespace.Schemes.Remove "ReadonlyArray" } }
        | _ -> ()

        let result = Result.Ok(ctx, newEnv)
        memoizedEnvAndCtx <- memoizedEnvAndCtx.Add(baseDir, result)

        return! result
    }
