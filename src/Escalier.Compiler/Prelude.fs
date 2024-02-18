namespace Escalier.Compiler

open FParsec.Error
open FsToolkit.ErrorHandling
open System.IO
open System.IO.Abstractions

open Escalier.Data.Type
open Escalier.Data
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

  let private resolvePath
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

  let private findModuleBindingNames (m: Syntax.Script) : list<string> =
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
          { Name = "A"
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let typeRefB =
      { Kind =
          { Name = "B"
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
      { Kind = makeTypeRefKind "A"
        Provenance = None }

    let typeRefB =
      { Kind = makeTypeRefKind "B"
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
          { Name = "A"
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let typeRefB =
      { Kind =
          { Name = "B"
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

    // TODO: we need an opaque type for now, or some way not to expand
    // Promise types.
    let promise: Scheme =
      { Type =
          { Kind = makeTypeRefKind "FooBar"
            Provenance = None }
        TypeParams = Some([ "T"; "E" ])
        IsTypeParam = false }

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

    env <- env.AddScheme "Promise" promise
    env

  let mutable envMemoized: Env option = None

  let getGlobalEnvMemoized () =
    match envMemoized with
    | Some(e) -> e
    | None ->
      let env = getGlobalEnv ()
      envMemoized <- Some(env)
      env

  let getCtx
    (filesystem: IFileSystem)
    (baseDir: string)
    (globalEnv: Env)
    : Result<Ctx, CompileError> =

    result {
      let ctx =
        Ctx(
          (fun ctx filename import ->
            let resolvedImportPath =
              Path.ChangeExtension(
                resolvePath baseDir filename import.Path,
                ".esc"
              )

            let contents = filesystem.File.ReadAllText(resolvedImportPath)

            let m =
              match Parser.parseScript contents with
              | Ok value -> value
              | Error _ -> failwith $"failed to parse {resolvedImportPath}"

            // scriptEnv
            let env =
              match Infer.inferScript ctx globalEnv filename m with
              | Ok value -> value
              | Error err ->
                printfn "err = %A" err
                failwith $"failed to infer {resolvedImportPath}"

            // exportEnv
            let mutable newEnv = Env.Env.empty

            let bindings = findModuleBindingNames m

            for name in bindings do
              match env.Values.TryFind(name) with
              // NOTE: exports are immutable
              | Some(t, isMut) -> newEnv <- newEnv.AddValue name (t, false)
              | None -> failwith $"binding {name} not found"

            for item in m.Items do
              match item with
              | Syntax.Stmt { Kind = Syntax.StmtKind.Decl { Kind = Syntax.DeclKind.TypeDecl { Name = name } } } ->
                match env.Schemes.TryFind(name) with
                | Some(scheme) -> newEnv <- newEnv.AddScheme name scheme
                | None -> failwith $"scheme {name} not found"
              | _ -> ()

            newEnv),
          (fun ctx filename import -> resolvePath baseDir filename import.Path)
        )

      return ctx
    }

  // TODO: add memoization
  // This is hard to memoize without reusing the filesystem
  let getEnvAndCtx
    (filesystem: IFileSystem)
    (baseDir: string)
    : Result<Ctx * Env, CompileError> =

    result {
      let env = getGlobalEnvMemoized ()
      let! ctx = getCtx filesystem baseDir env

      return ctx, env
    }

  // TODO: add memoization
  // This is hard to memoize without reusing the filesystem
  let getEnvAndCtxWithES5
    (filesystem: IFileSystem)
    (baseDir: string)
    : Result<Ctx * Env, CompileError> =

    result {
      let env = getGlobalEnvMemoized ()
      let! ctx = getCtx filesystem baseDir env

      let input = File.ReadAllText("./lib/lib.es5.d.ts")

      let! ast =
        match Parser.parseModule input with
        | FParsec.CharParsers.Success(value, _, _) -> Result.Ok(value)
        | FParsec.CharParsers.Failure(_, parserError, _) ->
          Result.mapError CompileError.ParseError (Result.Error(parserError))

      let! env =
        Infer.inferModule ctx env ast |> Result.mapError CompileError.TypeError

      return ctx, env
    }