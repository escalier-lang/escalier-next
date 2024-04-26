namespace Escalier.TypeChecker

open Escalier.TypeChecker.ExprVisitor
open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type

open Error
open Env

module rec Graph =
  let start = FParsec.Position("", 0, 1, 1)
  let stop = FParsec.Position("", 0, 1, 1)
  let DUMMY_SPAN: Span = { Start = start; Stop = stop }

  type DeclIdent =
    | Value of string
    | Type of string

  type DeclGraph =
    { Edges: Map<DeclIdent, list<DeclIdent>>
      Nodes: Map<DeclIdent, list<Decl>> }

    member this.Add(name: DeclIdent, decl: Decl, deps: list<DeclIdent>) =
      let decls =
        match this.Nodes.TryFind name with
        | Some nodes -> nodes @ [ decl ]
        | None -> [ decl ]

      { Edges = this.Edges.Add(name, deps)
        Nodes = this.Nodes.Add(name, decls) }

    static member Empty = { Edges = Map.empty; Nodes = Map.empty }

  // Find identifiers in an expression excluding function expressions.
  let findIdentifiers (expr: Expr) : list<DeclIdent> =
    let mutable ids: list<DeclIdent> = []

    let visitor =
      { ExprVisitor.VisitExpr =
          fun (expr, state) ->
            match expr.Kind with
            // NOTE: we don't have to do any special handling for
            // ExprKind.Member because the property is stored as a
            // string instead of an identifier.
            | ExprKind.Identifier name ->
              ids <- DeclIdent.Value name :: ids
              (false, state)
            | ExprKind.Function _ -> (false, state)
            | _ -> (true, state)
        ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
        ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnn =
          fun (typeAnn, state) ->
            match typeAnn.Kind with
            | TypeAnnKind.TypeRef { Ident = ident } ->
              let baseName = getBaseName ident
              (false, state)
            | TypeAnnKind.Typeof ident ->
              let baseName = getBaseName ident
              ids <- DeclIdent.Value baseName :: ids
              (false, state)
            | _ -> (true, state)
        ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (true, state) }

    walkExpr visitor () expr

    List.rev ids

  let getBaseName (ident: QualifiedIdent) : string =
    match ident with
    | QualifiedIdent.Ident name -> name
    | QualifiedIdent.Member(left, right) -> getBaseName left

  let findInferTypeAnns (typeAnn: TypeAnn) : list<DeclIdent> =

    let mutable idents: list<DeclIdent> = []

    let visitor: SyntaxVisitor<unit> =
      { ExprVisitor.VisitExpr = fun (_, state) -> (false, state)
        ExprVisitor.VisitStmt = fun (_, state) -> (false, state)
        ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnn =
          fun (typeAnn, state) ->
            match typeAnn.Kind with
            | TypeAnnKind.Infer name ->
              idents <- DeclIdent.Type name :: idents
              (false, state)
            | _ -> (true, state)
        ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (true, state) }

    walkTypeAnn visitor () typeAnn

    List.rev idents

  // TODO: dedupe with findTypeRefIdentsInExpr
  let findTypeRefIdents
    (env: Env)
    (localTypeNames: list<string>)
    (typeParams: list<string>)
    (typeAnn: TypeAnn)
    : list<DeclIdent> =
    let mutable idents: list<DeclIdent> = []

    let visitor: SyntaxVisitor<list<string>> =
      { ExprVisitor.VisitExpr = fun (_, state) -> (false, state)
        ExprVisitor.VisitStmt = fun (_, state) -> (false, state)
        ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnn =
          fun (typeAnn, typeParams) ->
            match typeAnn.Kind with
            | TypeAnnKind.TypeRef { Ident = ident } ->
              let baseName = getBaseName ident

              if
                (List.contains baseName localTypeNames)
                && not (List.contains baseName typeParams)
                && not (Map.containsKey baseName env.Namespace.Schemes)
                && not (Map.containsKey baseName env.Namespace.Namespaces)
              then
                idents <- DeclIdent.Type baseName :: idents

              (false, typeParams)
            | TypeAnnKind.Typeof ident ->
              let baseName = getBaseName ident
              idents <- DeclIdent.Value baseName :: idents
              (false, typeParams)
            | TypeAnnKind.Condition { Extends = extends } ->
              let inferNames =
                findInferTypeAnns extends
                |> List.choose (fun ident ->
                  match ident with
                  | DeclIdent.Type name -> Some name
                  | _ -> None)

              (true, typeParams @ inferNames)
            | _ -> (true, typeParams)
        ExprVisitor.VisitTypeAnnObjElem =
          fun (elem, typeParams) ->
            let state =
              match elem with
              | ObjTypeAnnElem.Callable funcSig ->
                match funcSig.TypeParams with
                | None -> typeParams
                | Some funcTypeParams ->
                  typeParams
                  @ (List.map
                    (fun (tp: Syntax.TypeParam) -> tp.Name)
                    funcTypeParams)
              | ObjTypeAnnElem.Constructor funcSig ->
                match funcSig.TypeParams with
                | None -> typeParams
                | Some funcTypeParams ->
                  typeParams
                  @ List.map
                      (fun (tp: Syntax.TypeParam) -> tp.Name)
                      funcTypeParams
              | ObjTypeAnnElem.Method { Type = t } ->
                match t.TypeParams with
                | None -> typeParams
                | Some funcTypeParams ->
                  typeParams
                  @ List.map
                      (fun (tp: Syntax.TypeParam) -> tp.Name)
                      funcTypeParams
              | ObjTypeAnnElem.Getter _ -> typeParams
              | ObjTypeAnnElem.Setter _ -> typeParams
              | ObjTypeAnnElem.Property _ -> typeParams
              | ObjTypeAnnElem.Mapped { TypeParam = typeParam } ->
                typeParam.Name :: typeParams

            (true, state) }

    walkTypeAnn visitor typeParams typeAnn

    List.rev idents

  // TODO: dedupe with findTypeRefIdents
  let findTypeRefIdentsInExpr
    (env: Env)
    (localTypeNames: list<string>)
    (typeParams: list<string>)
    (expr: Expr)
    : list<DeclIdent> =
    let mutable idents: list<DeclIdent> = []

    let visitor: SyntaxVisitor<list<string>> =
      { ExprVisitor.VisitExpr = fun (_, state) -> (true, state)
        ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
        ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnn =
          fun (typeAnn, typeParams) ->
            match typeAnn.Kind with
            | TypeAnnKind.TypeRef { Ident = ident } ->
              let baseName = getBaseName ident
              printfn $"baseName = {baseName}"

              if
                (List.contains baseName localTypeNames)
                && not (List.contains baseName typeParams)
                && not (Map.containsKey baseName env.Namespace.Schemes)
                && not (Map.containsKey baseName env.Namespace.Namespaces)
              then
                idents <- DeclIdent.Type baseName :: idents

              (false, typeParams)
            | TypeAnnKind.Typeof ident ->
              let baseName = getBaseName ident
              idents <- DeclIdent.Value baseName :: idents
              (false, typeParams)
            | TypeAnnKind.Condition { Extends = extends } ->
              let inferNames =
                findInferTypeAnns extends
                |> List.choose (fun ident ->
                  match ident with
                  | DeclIdent.Type name -> Some name
                  | _ -> None)

              (true, typeParams @ inferNames)
            | _ -> (true, typeParams)
        ExprVisitor.VisitTypeAnnObjElem =
          fun (elem, typeParams) ->
            let state =
              match elem with
              | ObjTypeAnnElem.Callable funcSig ->
                match funcSig.TypeParams with
                | None -> typeParams
                | Some funcTypeParams ->
                  typeParams
                  @ (List.map
                    (fun (tp: Syntax.TypeParam) -> tp.Name)
                    funcTypeParams)
              | ObjTypeAnnElem.Constructor funcSig ->
                match funcSig.TypeParams with
                | None -> typeParams
                | Some funcTypeParams ->
                  typeParams
                  @ List.map
                      (fun (tp: Syntax.TypeParam) -> tp.Name)
                      funcTypeParams
              | ObjTypeAnnElem.Method { Type = t } ->
                match t.TypeParams with
                | None -> typeParams
                | Some funcTypeParams ->
                  typeParams
                  @ List.map
                      (fun (tp: Syntax.TypeParam) -> tp.Name)
                      funcTypeParams
              | ObjTypeAnnElem.Getter _ -> typeParams
              | ObjTypeAnnElem.Setter _ -> typeParams
              | ObjTypeAnnElem.Property _ -> typeParams
              | ObjTypeAnnElem.Mapped { TypeParam = typeParam } ->
                typeParam.Name :: typeParams

            (true, state) }

    walkExpr visitor typeParams expr

    List.rev idents

  // TODO: update too look in `env` as well when deciding if something is a
  // local capture of not.
  // TODO: rename to `findLocalCaptures`
  let findCaptures
    (parentLocals: list<DeclIdent>)
    (f: Syntax.Function)
    : list<DeclIdent> =

    let mutable parentLocalNames =
      parentLocals
      |> List.choose (fun (id: DeclIdent) ->
        match id with
        | Value name -> Some name
        | Type name -> None)

    let decls =
      match f.Body with
      | BlockOrExpr.Block block ->
        block.Stmts
        |> List.choose (fun stmt ->
          match stmt.Kind with
          | StmtKind.Decl decl -> Some decl
          | _ -> None)
      | BlockOrExpr.Expr expr -> []

    let locals = findLocals decls

    let mutable localNames =
      locals
      |> List.choose (fun (id: DeclIdent) ->
        match id with
        | Value name -> Some name
        | Type name -> None)

    for p in f.Sig.ParamList do
      localNames <- localNames @ Infer.findBindingNames p.Pattern

    let mutable captures: list<DeclIdent> = []

    let visitor =
      { ExprVisitor.VisitExpr =
          fun (expr, localNames) ->
            match expr.Kind with
            // NOTE: we don't have to do any special handling for
            // ExprKind.Member because the property is stored as a
            // string instead of an identifier.
            | ExprKind.Identifier name ->
              if
                (List.contains name parentLocalNames)
                && not (List.contains name localNames)
              then
                captures <- DeclIdent.Value name :: captures

              (false, localNames)
            | ExprKind.Function f ->
              captures <- findCaptures (parentLocals @ locals) f
              // Don't recurse since `findCaptures` already does that
              (false, localNames)
            | _ -> (true, localNames)
        ExprVisitor.VisitStmt =
          fun (stmt, state) ->
            let bindingNames =
              match stmt.Kind with
              | StmtKind.Decl decl ->
                match decl.Kind with
                | DeclKind.VarDecl { Pattern = pattern } ->
                  Infer.findBindingNames pattern
                | _ -> [] // TODO: handle other kinds of declarations
              | _ -> []

            (true, state)
        ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

    match f.Body with
    | BlockOrExpr.Block block ->
      List.iter (walkStmt visitor localNames) block.Stmts
    | BlockOrExpr.Expr expr -> walkExpr visitor localNames expr

    List.rev captures

  let findFunctions (expr: Expr) : list<Syntax.Function> =
    let mutable fns: list<Syntax.Function> = []

    let visitor =
      { ExprVisitor.VisitExpr =
          fun (expr, state) ->
            match expr.Kind with
            | ExprKind.Function f ->
              fns <- f :: fns
              (false, state)
            | _ -> (true, state)
        ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
        ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

    walkExpr visitor () expr

    List.rev fns

  let findLocals (decls: list<Decl>) : list<DeclIdent> =
    let mutable locals: list<DeclIdent> = []

    for decl in decls do
      match decl.Kind with
      | VarDecl { Pattern = pattern } ->
        let bindingNames =
          Infer.findBindingNames pattern |> List.map DeclIdent.Value

        locals <- locals @ bindingNames
      | FnDecl { Name = name } -> locals <- locals @ [ DeclIdent.Value name ]
      | ClassDecl { Name = name } -> locals <- locals @ [ DeclIdent.Type name ]
      | TypeDecl { Name = name } -> locals <- locals @ [ DeclIdent.Type name ]
      | InterfaceDecl { Name = name } ->
        locals <- locals @ [ DeclIdent.Type name ]
      | EnumDecl { Name = name } ->
        locals <- locals @ [ DeclIdent.Value name ]
        locals <- locals @ [ DeclIdent.Type name ]
      | NamespaceDecl { Name = name } ->
        locals <- locals @ [ DeclIdent.Value name ]
        locals <- locals @ [ DeclIdent.Type name ]

    locals

  let buildGraph
    (env: Env)
    (parentDeclared: list<DeclIdent>)
    (parentLocals: list<DeclIdent>)
    (decls: list<Decl>)
    : Result<DeclGraph, TypeError> =
    result {
      let mutable functions: list<Syntax.Function> = []
      let mutable graph = DeclGraph.Empty
      let mutable declared: list<DeclIdent> = parentDeclared
      let locals = parentLocals @ findLocals decls

      let localTypeNames =
        List.choose
          (fun id ->
            match id with
            | DeclIdent.Type name -> Some name
            | _ -> None)
          locals

      for decl in decls do
        match decl.Kind with
        | VarDecl { Declare = declare
                    Pattern = pattern
                    Init = init
                    TypeAnn = typeAnn } ->
          let bindingNames =
            Infer.findBindingNames pattern |> List.map DeclIdent.Value

          match declare, init with
          | false, Some init ->
            let mutable deps = findIdentifiers init

            let typeDepsInExpr =
              findTypeRefIdentsInExpr env localTypeNames [] init

            let typeDeps =
              match typeAnn with
              | Some typeAnn -> findTypeRefIdents env localTypeNames [] typeAnn
              | None -> []

            deps <- deps @ typeDepsInExpr @ typeDeps

            // TODO: dedupe with the other branch
            for dep in deps do
              match dep with
              | Type _ -> ()
              | Value _ ->
                if not (List.contains dep declared) then
                  let depName =
                    match dep with
                    | DeclIdent.Value name -> name
                    | DeclIdent.Type name -> name

                  return!
                    Error(
                      TypeError.SemanticError
                        $"{depName} has not been initialized yet"
                    )

            let functions = findFunctions init

            for f in functions do
              deps <- deps @ findCaptures locals f

            for name in bindingNames do
              graph <- graph.Add(name, decl, deps)
          | true, None ->
            let deps =
              match typeAnn with
              | Some typeAnn -> findTypeRefIdents env localTypeNames [] typeAnn
              | None -> []

            // TODO: dedupe with the other branch
            for dep in deps do
              match dep with
              | Type _ -> ()
              | Value _ ->
                if not (List.contains dep declared) then
                  let depName =
                    match dep with
                    | DeclIdent.Value name -> name
                    | DeclIdent.Type name -> name

                  return!
                    Error(
                      TypeError.SemanticError
                        $"{depName} has not been initialized yet"
                    )

            for name in bindingNames do
              graph <- graph.Add(name, decl, deps)
          | _, _ ->
            return!
              Error(
                TypeError.SemanticError
                  "Variable declaration must have an initializer or use 'declare'"
              )

          declared <- declared @ bindingNames
        | TypeDecl { Name = name
                     TypeAnn = typeAnn
                     TypeParams = typeParams } ->
          let typeParamNames =
            match typeParams with
            | None -> []
            | Some typeParams ->
              List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

          let deps = findTypeRefIdents env localTypeNames typeParamNames typeAnn

          graph <- graph.Add(DeclIdent.Type name, decl, deps)
        | FnDecl { Declare = _
                   Name = name
                   Sig = fnSig
                   Body = body } ->

          let mutable typeDeps = []

          let typeParamNames =
            match fnSig.TypeParams with
            | None -> []
            | Some typeParams ->
              List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

          for param in fnSig.ParamList do
            match param.TypeAnn with
            | Some typeAnn ->

              typeDeps <-
                typeDeps
                @ findTypeRefIdents env localTypeNames typeParamNames typeAnn
            | None -> ()

          match fnSig.ReturnType with
          | None -> ()
          | Some returnType ->
            let typeParamNames =
              match fnSig.TypeParams with
              | None -> []
              | Some typeParams ->
                List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

            typeDeps <-
              typeDeps
              @ findTypeRefIdents env localTypeNames typeParamNames returnType

          let deps =
            match body with
            | None -> typeDeps
            | Some body ->
              let f: Syntax.Function =
                { Sig = fnSig
                  Body = body
                  Captures = None
                  InferredType = None }

              let captures = findCaptures locals f
              captures @ typeDeps

          graph <- graph.Add(DeclIdent.Value name, decl, deps)
          declared <- declared @ [ DeclIdent.Value name ]
        | ClassDecl { Name = name; Class = cls } ->
          let { TypeParams = typeParams
                Syntax.Elems = elems } =
            cls

          let classTypeParamNames =
            match typeParams with
            | None -> []
            | Some typeParams ->
              List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

          for elem in elems do
            match elem with
            | ClassElem.Property { TypeAnn = typeAnn } ->
              // TODO: if it's a static property then we need to add a dep for
              // DeclIdent.Value name
              let deps =
                findTypeRefIdents env localTypeNames classTypeParamNames typeAnn

              graph <- graph.Add(DeclIdent.Type name, decl, deps)
            | ClassElem.Constructor { Sig = fnSig; Body = body } ->
              let mutable typeDeps = []

              let typeParamNames =
                match fnSig.TypeParams with
                | None -> []
                | Some typeParams ->
                  List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

              for param in fnSig.ParamList do
                match param.TypeAnn with
                | Some typeAnn ->

                  typeDeps <-
                    typeDeps
                    @ findTypeRefIdents
                        env
                        localTypeNames
                        (classTypeParamNames @ typeParamNames)
                        typeAnn
                | None -> ()

              match fnSig.ReturnType with
              | None -> ()
              | Some returnType ->
                let typeParamNames =
                  match fnSig.TypeParams with
                  | None -> []
                  | Some typeParams ->
                    List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

                typeDeps <-
                  typeDeps
                  @ findTypeRefIdents
                      env
                      localTypeNames
                      (classTypeParamNames @ typeParamNames)
                      returnType

              let deps =
                match body with
                | None -> typeDeps
                | Some body ->
                  let f: Syntax.Function =
                    { Sig = fnSig
                      Body = body
                      Captures = None
                      InferredType = None }

                  findCaptures locals f @ typeDeps

              graph <- graph.Add(DeclIdent.Value name, decl, deps)
            | ClassElem.Method { Sig = fnSig; Body = body } ->
              let mutable typeDeps = []

              let typeParamNames =
                match fnSig.TypeParams with
                | None -> []
                | Some typeParams ->
                  List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

              for param in fnSig.ParamList do
                match param.TypeAnn with
                | Some typeAnn ->

                  typeDeps <-
                    typeDeps
                    @ findTypeRefIdents
                        env
                        localTypeNames
                        (classTypeParamNames @ typeParamNames)
                        typeAnn
                | None -> ()

              match fnSig.ReturnType with
              | None -> ()
              | Some returnType ->
                let typeParamNames =
                  match fnSig.TypeParams with
                  | None -> []
                  | Some typeParams ->
                    List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

                typeDeps <-
                  typeDeps
                  @ findTypeRefIdents
                      env
                      localTypeNames
                      (classTypeParamNames @ typeParamNames)
                      returnType

              let deps =
                match body with
                | None -> typeDeps
                | Some body ->
                  let f: Syntax.Function =
                    { Sig = fnSig
                      Body = body
                      Captures = None
                      InferredType = None }

                  findCaptures locals f @ typeDeps

              graph <- graph.Add(DeclIdent.Value name, decl, deps)
            | ClassElem.Getter getter ->
              let fnSig: FuncSig<option<TypeAnn>> =
                { ParamList = []
                  Self = Some getter.Self
                  ReturnType = getter.ReturnType
                  Throws = None
                  TypeParams = None
                  IsAsync = false }

              let mutable typeDeps = []

              let typeParamNames =
                match fnSig.TypeParams with
                | None -> []
                | Some typeParams ->
                  List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

              for param in fnSig.ParamList do
                match param.TypeAnn with
                | Some typeAnn ->

                  typeDeps <-
                    typeDeps
                    @ findTypeRefIdents
                        env
                        localTypeNames
                        (classTypeParamNames @ typeParamNames)
                        typeAnn
                | None -> ()

              match fnSig.ReturnType with
              | None -> ()
              | Some returnType ->
                let typeParamNames =
                  match fnSig.TypeParams with
                  | None -> []
                  | Some typeParams ->
                    List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

                typeDeps <-
                  typeDeps
                  @ findTypeRefIdents
                      env
                      localTypeNames
                      (classTypeParamNames @ typeParamNames)
                      returnType

              let deps =
                match getter.Body with
                | None -> typeDeps
                | Some body ->
                  let f: Syntax.Function =
                    { Sig = fnSig
                      Body = body
                      Captures = None
                      InferredType = None }

                  findCaptures locals f @ typeDeps

              graph <- graph.Add(DeclIdent.Value name, decl, deps)
            | ClassElem.Setter setter ->
              let undefinedTypeAnn: TypeAnn =
                { Span = DUMMY_SPAN
                  Kind = TypeAnnKind.Literal Literal.Undefined
                  InferredType = None }

              let fnSig: FuncSig<option<TypeAnn>> =
                { ParamList = []
                  Self = Some setter.Self
                  ReturnType = Some undefinedTypeAnn
                  Throws = None
                  TypeParams = None
                  IsAsync = false }

              let mutable typeDeps = []

              let typeParamNames =
                match fnSig.TypeParams with
                | None -> []
                | Some typeParams ->
                  List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

              for param in fnSig.ParamList do
                match param.TypeAnn with
                | Some typeAnn ->

                  typeDeps <-
                    typeDeps
                    @ findTypeRefIdents
                        env
                        localTypeNames
                        (classTypeParamNames @ typeParamNames)
                        typeAnn
                | None -> ()

              match fnSig.ReturnType with
              | None -> ()
              | Some returnType ->
                let typeParamNames =
                  match fnSig.TypeParams with
                  | None -> []
                  | Some typeParams ->
                    List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

                typeDeps <-
                  typeDeps
                  @ findTypeRefIdents
                      env
                      localTypeNames
                      (classTypeParamNames @ typeParamNames)
                      returnType

              let deps =
                match setter.Body with
                | None -> typeDeps
                | Some body ->
                  let f: Syntax.Function =
                    { Sig = fnSig
                      Body = body
                      Captures = None
                      InferredType = None }

                  findCaptures locals f @ typeDeps

              graph <- graph.Add(DeclIdent.Type name, decl, deps)

          declared <- declared @ [ DeclIdent.Value name ]
        | InterfaceDecl { Name = name
                          TypeParams = typeParams
                          Elems = elems } ->
          let mutable deps = []

          let interfaceTypeParamNames =
            match typeParams with
            | None -> []
            | Some typeParams ->
              List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

          for elem in elems do
            match elem with
            | ObjTypeAnnElem.Callable fnSig ->
              let typeParamNames =
                match fnSig.TypeParams with
                | None -> []
                | Some typeParams ->
                  List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

              for param in fnSig.ParamList do
                deps <-
                  deps
                  @ findTypeRefIdents
                      env
                      localTypeNames
                      (interfaceTypeParamNames @ typeParamNames)
                      param.TypeAnn

              deps <-
                deps
                @ findTypeRefIdents
                    env
                    localTypeNames
                    (interfaceTypeParamNames @ typeParamNames)
                    fnSig.ReturnType
            | ObjTypeAnnElem.Constructor fnSig ->
              let typeParamNames =
                match fnSig.TypeParams with
                | None -> []
                | Some typeParams ->
                  List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

              for param in fnSig.ParamList do
                deps <-
                  deps
                  @ findTypeRefIdents
                      env
                      localTypeNames
                      (interfaceTypeParamNames @ typeParamNames)
                      param.TypeAnn

              deps <-
                deps
                @ findTypeRefIdents
                    env
                    localTypeNames
                    (interfaceTypeParamNames @ typeParamNames)
                    fnSig.ReturnType
            | ObjTypeAnnElem.Method { Type = fnSig } ->
              let typeParamNames =
                match fnSig.TypeParams with
                | None -> []
                | Some typeParams ->
                  List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

              for param in fnSig.ParamList do
                deps <-
                  deps
                  @ findTypeRefIdents
                      env
                      localTypeNames
                      (interfaceTypeParamNames @ typeParamNames)
                      param.TypeAnn

              deps <-
                deps
                @ findTypeRefIdents
                    env
                    localTypeNames
                    (interfaceTypeParamNames @ typeParamNames)
                    fnSig.ReturnType
            | ObjTypeAnnElem.Getter { ReturnType = returnType } ->
              deps <-
                deps
                @ findTypeRefIdents
                    env
                    localTypeNames
                    interfaceTypeParamNames
                    returnType
            | ObjTypeAnnElem.Setter { Param = { TypeAnn = typeAnn } } ->
              deps <-
                deps
                @ findTypeRefIdents
                    env
                    localTypeNames
                    interfaceTypeParamNames
                    typeAnn
            | ObjTypeAnnElem.Property { TypeAnn = typeAnn } ->
              deps <-
                findTypeRefIdents
                  env
                  localTypeNames
                  interfaceTypeParamNames
                  typeAnn
            | ObjTypeAnnElem.Mapped { TypeParam = typeParam
                                      TypeAnn = typeAnn } ->
              let tp: Syntax.TypeParam =
                { Span = DUMMY_SPAN
                  Name = typeParam.Name
                  Constraint = Some typeParam.Constraint
                  Default = None }

              let typeParams =
                match typeParams with
                | None -> Some [ tp ]
                | Some typeParams -> Some(tp :: typeParams)

              deps <-
                findTypeRefIdents
                  env
                  localTypeNames
                  interfaceTypeParamNames
                  typeParam.Constraint

              deps <-
                findTypeRefIdents
                  env
                  localTypeNames
                  interfaceTypeParamNames
                  typeAnn

          // TODO: check if there's an existing entry for `name` so that
          // we can update its `deps` list instead of overwriting it.
          graph <- graph.Add(DeclIdent.Type name, decl, deps)
        | EnumDecl enumDecl -> failwith "TODO: buildGraph - EnumDecl"
        | NamespaceDecl { Name = name; Body = decls } ->
          let! subgraph = buildGraph env declared locals decls
          let subgraphDeps = subgraph.Edges.Values |> List.concat
          let subgraphIdents = subgraph.Nodes.Keys |> List.ofSeq

          let deps =
            List.filter
              (fun dep -> not (List.contains dep subgraphIdents))
              subgraphDeps

          graph <- graph.Add(DeclIdent.Value name, decl, deps)
          graph <- graph.Add(DeclIdent.Type name, decl, deps)
          declared <- declared @ [ DeclIdent.Value name ]

      return graph
    }

  let rec findCycles (edges: Map<string, list<string>>) : Set<Set<string>> =

    let mutable visited: list<string> = []
    let mutable stack: list<string> = []
    let mutable cycles: Set<Set<string>> = Set.empty

    let rec visit (node: string) (parents: list<string>) =
      if List.contains node parents then
        // find the index of node in parents
        let index = List.findIndex (fun p -> p = node) parents
        let cycle = List.take index parents @ [ node ] |> Set.ofList
        cycles <- Set.add cycle cycles
      else
        let edges = edges[node]

        for next in edges do
          visit next (node :: parents)

    for KeyValue(node, _) in edges do
      visit node []

    cycles

  type DeclTree =
    { Edges: Map<Set<DeclIdent>, Set<Set<DeclIdent>>>
      CycleMap: Map<DeclIdent, Set<DeclIdent>> }

  let rec graphToTree (edges: Map<DeclIdent, list<DeclIdent>>) : DeclTree =
    let mutable visited: list<DeclIdent> = []
    let mutable stack: list<DeclIdent> = []
    let mutable cycles: Set<Set<DeclIdent>> = Set.empty

    let rec visit (node: DeclIdent) (parents: list<DeclIdent>) =
      if List.contains node parents then
        // find the index of node in parents
        let index = List.findIndex (fun p -> p = node) parents
        let cycle = List.take index parents @ [ node ] |> Set.ofList
        cycles <- Set.add cycle cycles
      else
        let edgesOuts =
          match edges.TryFind node with
          | None -> failwith $"Couldn't find edge for {node} in {edges}"
          | Some value -> value

        for next in edgesOuts do
          visit next (node :: parents)

    for KeyValue(node, _) in edges do
      visit node []

    let mutable cycleMap: Map<DeclIdent, Set<DeclIdent>> = Map.empty

    for cycle in cycles do
      for node in cycle do
        cycleMap <- cycleMap.Add(node, cycle)

    let mutable newEdges: Map<Set<DeclIdent>, Set<Set<DeclIdent>>> = Map.empty

    for KeyValue(node, deps) in edges do
      let deps = Set.ofList deps

      let src =
        if Map.containsKey node cycleMap then
          cycleMap[node]
        else
          Set.singleton node

      for dep in deps do
        if not (Set.contains dep src) then
          let dst =
            if Map.containsKey dep cycleMap then
              cycleMap[dep]
            else
              Set.singleton dep

          if Map.containsKey src newEdges then
            let dsts = newEdges[src]
            newEdges <- newEdges.Add(src, dsts.Add(dst))
          else
            newEdges <- newEdges.Add(src, Set.singleton dst)

    { CycleMap = cycleMap
      Edges = newEdges }

  let inferTreeRec
    (ctx: Ctx)
    (env: Env)
    (root: Set<DeclIdent>)
    (nodes: Map<DeclIdent, list<Decl>>)
    (tree: DeclTree)
    : Result<Env, TypeError> =

    result {
      let mutable newEnv = env

      match tree.Edges.TryFind root with
      | Some deps ->
        for dep in deps do
          let! depEnv = inferTreeRec ctx newEnv dep nodes tree
          newEnv <- depEnv
      | None -> ()

      match List.ofSeq root with
      | [] ->
        return!
          Error(
            TypeError.SemanticError "inferTreeRec - rootSet should not be empty"
          )
      | [ name ] ->
        // let decl = nodes[name]
        // let generalize = true
        // return! inferDecl ctx newEnv decl generalize
        let decls = nodes[name]

        let! newEnv, placeholderNS =
          Infer.inferDeclPlaceholders ctx newEnv decls

        let! newEnv, inferredNS =
          Infer.inferDeclDefinitions ctx newEnv placeholderNS decls

        do!
          Infer.unifyPlaceholdersAndInferredTypes
            ctx
            newEnv
            placeholderNS
            inferredNS

        // TODO: update `inferDeclDefinitions` to take a `generalize` flag
        // so that we can avoid generalizing here.
        let bindings = Infer.generalizeBindings inferredNS.Values
        let newEnv = newEnv.AddBindings bindings

        return newEnv
      | names ->
        let decls = names |> List.map (fun name -> nodes[name]) |> List.concat

        let! newEnv, placeholderNS =
          Infer.inferDeclPlaceholders ctx newEnv decls

        let! newEnv, inferredNS =
          Infer.inferDeclDefinitions ctx newEnv placeholderNS decls

        do!
          Infer.unifyPlaceholdersAndInferredTypes
            ctx
            newEnv
            placeholderNS
            inferredNS

        // TODO: update `inferDeclDefinitions` to take a `generalize` flag
        // so that we can avoid generalizing here.
        let bindings = Infer.generalizeBindings inferredNS.Values
        let newEnv = newEnv.AddBindings bindings

        return newEnv
    }

  let inferTree
    (ctx: Ctx)
    (env: Env)
    (nodes: Map<DeclIdent, list<Decl>>)
    (tree: DeclTree)
    : Result<Env, TypeError> =

    result {
      let mutable newEnv = env

      let mutable sets: Set<Set<DeclIdent>> = Set.empty

      for KeyValue(key, value) in nodes do
        match tree.CycleMap.TryFind(key) with
        | Some set -> sets <- sets.Add(set)
        | None -> sets <- sets.Add(Set.singleton key)

      for set in sets do
        try
          let! nextEnv = inferTreeRec ctx newEnv set nodes tree
          newEnv <- nextEnv
        with e ->
          printfn $"Error: {e}"
          return! Error(TypeError.SemanticError(e.ToString()))

      return newEnv
    }

  let getDeclsFromModule (ast: Module) : list<Decl> =
    List.choose
      (fun item ->
        match item with
        | Decl decl -> Some decl
        | _ -> None)
      ast.Items

  let inferModuleUsingTree
    (ctx: Ctx)
    (env: Env)
    (ast: Module)
    : Result<Env, TypeError> =
    result {
      // TODO: update this function to accept a filename
      let mutable newEnv = { env with Filename = "input.esc" }

      let imports =
        List.choose
          (fun item ->
            match item with
            | Import import -> Some import
            | _ -> None)
          ast.Items

      for import in imports do
        let! importEnv = Infer.inferImport ctx newEnv import
        newEnv <- importEnv

      let decls = getDeclsFromModule ast
      let! graph = buildGraph newEnv [] [] decls
      let tree = graphToTree graph.Edges
      return! inferTree ctx newEnv graph.Nodes tree
    }

  let getExports
    (ctx: Ctx)
    (env: Env)
    (name: string)
    (items: list<ModuleItem>)
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
          failwith "TODO: getExports - importDecl"
        | ModuleItem.Decl decl ->
          match decl.Kind with
          | DeclKind.ClassDecl classDecl ->
            failwith "TODO: getExports - classDecl"
          | DeclKind.FnDecl { Name = name } ->
            let! t = env.GetValue name
            let isMut = false
            ns <- ns.AddBinding name (t, isMut)
          | DeclKind.VarDecl { Pattern = pattern } ->
            let names = Infer.findBindingNames pattern

            for name in names do
              let! t = env.GetValue name
              let isMut = false
              ns <- ns.AddBinding name (t, isMut)
          // | DeclKind.Using usingDecl -> failwith "TODO: getExports - usingDecl"
          | DeclKind.InterfaceDecl { Name = name } ->
            let! scheme = env.GetScheme(QualifiedIdent.Ident name)

            ns <- ns.AddScheme name scheme
          | DeclKind.TypeDecl { Name = name } ->
            let! scheme = env.GetScheme(QualifiedIdent.Ident name)

            ns <- ns.AddScheme name scheme
          | DeclKind.EnumDecl tsEnumDecl ->
            failwith "TODO: getExports - tsEnumDecl"
          | DeclKind.NamespaceDecl { Name = name } ->
            match env.Namespace.Namespaces.TryFind name with
            | Some value -> ns <- ns.AddNamespace name value
            | None -> failwith $"Couldn't find namespace: '{name}'"

      return ns
    }
