namespace Escalier.TypeChecker

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

open Env

// TODO: move the prelude into its own file so that Provenance
// can be set to something
module Prelude =
  type CompileError =
    | ParseError of ParserError
    | TypeError of TypeError

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
            | Syntax.PatternKind.Identifier({ Name = name }) ->
              names <- name :: names
              false
            | _ -> true
        ExprVisitor.VisitTypeAnn = fun _ -> false }

    walkPattern visitor p

    List.rev names

  let private findModuleBindingNames (m: Syntax.Module) : list<string> =
    let mutable names: list<string> = []

    for item in m.Items do
      match item with
      | Syntax.Stmt stmt ->
        match stmt.Kind with
        | Syntax.StmtKind.Decl({ Kind = Syntax.DeclKind.VarDecl(pattern, _, _) }) ->
          names <- List.concat [ names; findBindingNames pattern ]
        | _ -> ()
      | _ -> ()

    names

  let never =
    { Kind = TypeKind.Keyword Keyword.Never
      Provenance = None }

  let makeParam (name: string) (ty: Type) : FuncParam =
    { Pattern = Pattern.Identifier name
      Type = ty
      Optional = false }

  let getEnvAndCtx
    (filesystem: IFileSystem)
    (baseDir: string)
    (entry: string)
    : Result<Ctx * Env, CompileError> =
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

    let comparison =
      (makeFunctionType
        None
        [ makeParam "left" numType; makeParam "right" numType ]
        boolType
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

    result {
      let env: Env =
        { Env.Values =
            Map.ofList
              [ ("+", arithemtic "+")
                ("++", stringConcat)
                ("-", arithemtic "-")
                ("*", arithemtic "*")
                ("/", arithemtic "/")
                ("%", arithemtic "%")
                ("**", arithemtic "**")
                ("<", comparison)
                ("<=", comparison)
                (">", comparison)
                (">=", comparison)
                ("==", equality)
                ("!=", equality)
                ("||", logical)
                ("&&", logical) ]

          Env.Schemes = Map([ ("Promise", promise) ])
          Env.IsAsync = false }

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

            let env =
              match Infer.inferScript ctx env entry m with
              | Ok value -> value
              | Error _ -> failwith $"failed to infer {resolvedImportPath}"

            let mutable newEnv = Env.Env.empty

            let bindings = findModuleBindingNames m

            for name in bindings do
              match env.Values.TryFind(name) with
              // NOTE: exports are immutable
              | Some(t, isMut) -> newEnv <- newEnv.AddValue name (t, false)
              | None -> failwith $"binding {name} not found"

            for item in m.Items do
              match item with
              | Syntax.Stmt { Kind = Syntax.StmtKind.Decl { Kind = Syntax.DeclKind.TypeDecl(name,
                                                                                            _,
                                                                                            _) } } ->
                match env.Schemes.TryFind(name) with
                | Some(scheme) -> newEnv <- newEnv.AddScheme name scheme
                | None -> failwith $"scheme {name} not found"
              | _ -> ()

            newEnv),
          (fun ctx filename import -> resolvePath baseDir filename import.Path)
        )

      let! symbolTypeAnn =
        Parser.parseTypeAnn
          """{
            iterator: unique symbol
          }"""
        |> Result.mapError CompileError.ParseError

      let! symbolType =
        Infer.inferTypeAnn ctx env symbolTypeAnn
        |> Result.mapError CompileError.TypeError

      let env = env.AddValue "Symbol" (symbolType, false)

      let prelude =
        """
        type Iterator<T> = {
          next: fn () -> { done: boolean, value: T }
        }
        type Array<T> = {
          [Symbol.iterator]: fn () -> Iterator<T>
        }
        """

      let! ast =
        Parser.parseScript prelude |> Result.mapError CompileError.ParseError

      let! env =
        Infer.inferScript ctx env entry ast
        |> Result.mapError CompileError.TypeError

      return ctx, env
    }
