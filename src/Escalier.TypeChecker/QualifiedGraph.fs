module Escalier.TypeChecker.QualifiedGraph

open Escalier.Data.Type
open Escalier.TypeChecker.Env
open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Syntax

open Error
open ExprVisitor

type QualifiedIdent =
  { Namespaces: list<string>
    Name: string }

  override this.ToString() =
    match this.Namespaces with
    | [] -> this.Name
    | namespaces ->
      let namespaces = String.concat "." namespaces
      $"{namespaces}.{this.Name}"

  static member FromString(name: string) = { Namespaces = []; Name = name }

  static member FromCommonQualifiedIdent
    (qid: Common.QualifiedIdent)
    : QualifiedIdent =
    match qid with
    | Common.QualifiedIdent.Ident name -> { Namespaces = []; Name = name }
    | Common.QualifiedIdent.Member(left, right) ->
      let left = QualifiedIdent.FromCommonQualifiedIdent left
      let namespaces = left.Namespaces @ [ left.Name ]

      { Namespaces = namespaces
        Name = right }

let start = FParsec.Position("", 0, 1, 1)
let stop = FParsec.Position("", 0, 1, 1)
let DUMMY_SPAN: Span = { Start = start; Stop = stop }

// TODO:
// - infer types for all the declarations in each namespace
// - determine the dependences between declarations in each namespace
// - infer them in the correct order

// TODO: use a list instead of QualifiedIdent which is recursive
type QDeclIdent =
  | Type of QualifiedIdent
  | Value of QualifiedIdent

  override this.ToString() =
    match this with
    | Type qid -> $"Type {qid}"
    | Value qid -> $"Value {qid}"

type QGraph =
  // A type can depend on multiple interface declarations
  { Nodes: Map<QDeclIdent, list<Decl>>
    Edges: Map<QDeclIdent, list<QDeclIdent>> }

  member this.Add(name: QDeclIdent, decl: Decl, deps: list<QDeclIdent>) =
    let decls =
      match this.Nodes.TryFind name with
      | Some nodes -> nodes @ [ decl ]
      | None -> [ decl ]

    { Edges = this.Edges.Add(name, deps)
      Nodes = this.Nodes.Add(name, decls) }

type QualifiedNamespace =
  { Values: Map<QualifiedIdent, Binding>
    Schemes: Map<QualifiedIdent, Scheme> }

  static member Empty =
    { Values = Map.empty
      Schemes = Map.empty }

  member this.AddValue (ident: QualifiedIdent) (binding: Binding) =
    { this with
        Values = Map.add ident binding this.Values }

  member this.AddScheme (ident: QualifiedIdent) (scheme: Scheme) =
    { this with
        Schemes = Map.add ident scheme this.Schemes }

type SyntaxNode = Graph.SyntaxNode

let rec memberToQualifiedIdent
  (target: Expr)
  (name: string)
  : option<QualifiedIdent> =
  match target.Kind with
  | ExprKind.Member(target, innerName, _) ->
    match memberToQualifiedIdent target innerName with
    | Some qid ->
      Some(
        { Namespaces = qid.Namespaces @ [ qid.Name ]
          Name = name }
      )
    | None -> None
  | ExprKind.Identifier innerName ->
    Some(
      { Namespaces = [ innerName ]
        Name = name }
    )
  | _ -> None

// Find identifiers in an expression excluding function expressions.
let findIdentifiers
  (locals: list<QDeclIdent>)
  (ident: QDeclIdent)
  (expr: Expr)
  : list<QDeclIdent> =
  let mutable ids: list<QDeclIdent> = []

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | ExprKind.Member(target, name, _) ->
            match memberToQualifiedIdent target name with
            | Some qid ->
              ids <- QDeclIdent.Value qid :: ids
              (false, state)
            | None -> (true, state)
          | ExprKind.Identifier name ->
            ids <- QDeclIdent.Value(QualifiedIdent.FromString name) :: ids
            (false, state)
          | ExprKind.Function _ -> (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn =
        fun (typeAnn, state) ->
          match typeAnn.Kind with
          | TypeAnnKind.TypeRef { Ident = ident } -> (false, state)
          | TypeAnnKind.Typeof ident ->
            ids <-
              QDeclIdent.Value(QualifiedIdent.FromCommonQualifiedIdent ident)
              :: ids

            (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (true, state) }

  walkExpr visitor () expr

  let namespaces =
    match ident with
    | Type { Namespaces = namespaces } -> namespaces
    | Value { Namespaces = namespaces } -> namespaces

  // TODO: loop through all ids and if there's an id that doesn't appear in locals
  // then we need to check if it's in a namespace and if it is try adding that namespace
  // to the id and check again

  // Post-process ids to prepend namespaces if necessary
  let ids =
    ids
    |> List.map (fun id ->
      if List.contains id locals then
        id
      else
        // TODO: handle situations where we only need to prepend some of the
        // namespaces in `namespaces` instead of all of them
        match id with
        | Type qid ->
          Type
            { Namespaces = namespaces @ qid.Namespaces
              Name = qid.Name }
        | Value qid ->
          Value
            { Namespaces = namespaces @ qid.Namespaces
              Name = qid.Name })

  List.rev ids

let findInferTypeAnns (typeAnn: TypeAnn) : list<QDeclIdent> =
  let mutable idents: list<QDeclIdent> = []

  let visitor: SyntaxVisitor<unit> =
    { ExprVisitor.VisitExpr = fun (_, state) -> (false, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (false, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn =
        fun (typeAnn, state) ->
          match typeAnn.Kind with
          | TypeAnnKind.Infer name ->
            idents <- QDeclIdent.Type(QualifiedIdent.FromString name) :: idents
            (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (true, state) }

  walkTypeAnn visitor () typeAnn
  List.rev idents

let findTypeRefIdents
  (env: Env)
  (localTypeNames: list<QualifiedIdent>) // top-level and namespace decls
  (typeParams: list<QualifiedIdent>)
  (syntaxNode: SyntaxNode)
  : list<QDeclIdent> =
  let mutable typeRefIdents: list<QDeclIdent> = []

  let visitor: SyntaxVisitor<list<QualifiedIdent>> =
    { ExprVisitor.VisitExpr = fun (_, state) -> (true, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn =
        fun (typeAnn, typeParams) ->
          let newTypeParams =
            match typeAnn.Kind with
            | TypeAnnKind.TypeRef { Ident = ident } ->
              let ident = QualifiedIdent.FromCommonQualifiedIdent ident

              if
                (List.contains ident localTypeNames)
                && not (List.contains ident typeParams)
              then
                typeRefIdents <- QDeclIdent.Type ident :: typeRefIdents

              []
            | TypeAnnKind.Typeof ident ->
              let ident = QualifiedIdent.FromCommonQualifiedIdent ident
              typeRefIdents <- QDeclIdent.Value ident :: typeRefIdents

              []
            | TypeAnnKind.Condition { Extends = extends } ->
              findInferTypeAnns extends
              |> List.choose (fun ident ->
                match ident with
                | QDeclIdent.Type qid -> Some qid
                | _ -> None)
            | _ -> []

          (true, typeParams @ newTypeParams)
      ExprVisitor.VisitTypeAnnObjElem =
        fun (elem, typeParams) ->
          let newTypeParams: list<QualifiedIdent> =
            match elem with
            | ObjTypeAnnElem.Callable funcSig ->
              match funcSig.TypeParams with
              | None -> []
              | Some funcTypeParams ->
                List.map
                  (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
                  funcTypeParams
            | ObjTypeAnnElem.Constructor funcSig ->
              match funcSig.TypeParams with
              | None -> []
              | Some funcTypeParams ->
                List.map
                  (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
                  funcTypeParams
            | ObjTypeAnnElem.Method { Type = t } ->
              match t.TypeParams with
              | None -> []
              | Some funcTypeParams ->
                List.map
                  (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
                  funcTypeParams
            | ObjTypeAnnElem.Getter _ -> []
            | ObjTypeAnnElem.Setter _ -> []
            | ObjTypeAnnElem.Property _ -> []
            | ObjTypeAnnElem.Mapped { TypeParam = typeParam } ->
              [ QualifiedIdent.FromString typeParam.Name ]

          (true, typeParams @ newTypeParams) }

  match syntaxNode with
  | SyntaxNode.TypeAnn typeAnn -> walkTypeAnn visitor typeParams typeAnn
  | SyntaxNode.Expr expr -> walkExpr visitor typeParams expr

  List.rev typeRefIdents

let findLocals (decls: list<Decl>) : list<QDeclIdent> =
  let mutable locals: list<QDeclIdent> = []

  let rec findLocalsRec
    (decls: list<Decl>)
    (namespaces: list<string>)
    : list<QDeclIdent> =
    let mutable locals: list<QDeclIdent> = []

    for decl in decls do
      match decl.Kind with
      | VarDecl { Pattern = pattern } ->
        let bindingNames =
          Helpers.findBindingNames pattern
          |> List.map (fun name ->
            QDeclIdent.Value({ Namespaces = namespaces; Name = name }))

        locals <- locals @ bindingNames
      | FnDecl { Name = name } ->
        locals <- locals @ [ QDeclIdent.Value(QualifiedIdent.FromString name) ]
      | ClassDecl { Name = name } ->
        locals <- locals @ [ QDeclIdent.Type(QualifiedIdent.FromString name) ]
      | TypeDecl { Name = name } ->
        locals <- locals @ [ QDeclIdent.Type(QualifiedIdent.FromString name) ]
      | InterfaceDecl { Name = name } ->
        locals <- locals @ [ QDeclIdent.Type(QualifiedIdent.FromString name) ]
      | EnumDecl { Name = name } ->
        locals <- locals @ [ QDeclIdent.Value(QualifiedIdent.FromString name) ]
        locals <- locals @ [ QDeclIdent.Type(QualifiedIdent.FromString name) ]
      | NamespaceDecl { Name = name; Body = decls } ->

        locals <- locals @ findLocalsRec decls (namespaces @ [ name ])

    locals

  findLocalsRec decls []

// TODO: update this function to accept a QualifiedNamespace as an argument
// Only items in this namespace can be considered as a potential captures
let rec findCaptures
  (qns: list<QDeclIdent>) // TODO: update this to use QualifiedNamespace
  (parentLocals: list<QDeclIdent>)
  (f: Function)
  : list<QDeclIdent> =

  let qnsNames =
    qns
    |> List.choose (fun (qid: QDeclIdent) ->
      match qid with
      | QDeclIdent.Value name -> Some name
      | QDeclIdent.Type name -> None)

  let parentLocalNames =
    parentLocals
    |> List.choose (fun (qid: QDeclIdent) ->
      match qid with
      | QDeclIdent.Value name -> Some name
      | QDeclIdent.Type name -> None)

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
    |> List.choose (fun (qid: QDeclIdent) ->
      match qid with
      | QDeclIdent.Value name -> Some name
      | QDeclIdent.Type name -> None)

  for p in f.Sig.ParamList do
    let patternIdents =
      Helpers.findBindingNames p.Pattern |> List.map QualifiedIdent.FromString

    localNames <- localNames @ patternIdents

  let mutable captures: list<QDeclIdent> = []

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, localNames: list<QualifiedIdent>) ->
          match expr.Kind with
          // NOTE: we don't have to do any special handling for
          // ExprKind.Member because the property is stored as a
          // string instead of an identifier.
          | ExprKind.Identifier name ->
            let ident = QualifiedIdent.FromString name

            if
              (List.contains ident qnsNames)
              && not (List.contains ident parentLocalNames)
              && not (List.contains ident localNames)
            then
              captures <- QDeclIdent.Value ident :: captures

            (false, localNames)
          | ExprKind.Function f ->
            captures <- findCaptures qns (parentLocals @ locals) f
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
                Helpers.findBindingNames pattern
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

let getDepsForFn
  (env: Env)
  (possibleDeps: list<QDeclIdent>)
  (excludedTypeNames: list<QualifiedIdent>)
  (fnSig: FuncSig)
  (body: option<BlockOrExpr>)
  : list<QDeclIdent> =
  let mutable typeDeps = []

  let possibleTypeNames =
    List.choose
      (fun qid ->
        match qid with
        | QDeclIdent.Type name -> Some name
        | _ -> None)
      possibleDeps

  let typeParamNames: list<QualifiedIdent> =
    match fnSig.TypeParams with
    | None -> []
    | Some typeParams ->
      List.map
        (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
        typeParams

  match fnSig.TypeParams with
  | Some typeParams ->
    for tp in typeParams do
      match tp.Constraint with
      | Some c ->
        typeDeps <-
          typeDeps
          @ findTypeRefIdents
              env
              possibleTypeNames
              (excludedTypeNames @ typeParamNames)
              (SyntaxNode.TypeAnn c)
      | None -> ()

      match tp.Default with
      | Some d ->
        typeDeps <-
          typeDeps
          @ findTypeRefIdents
              env
              possibleTypeNames
              (excludedTypeNames @ typeParamNames)
              (SyntaxNode.TypeAnn d)
      | None -> ()
  | None -> ()

  for param in fnSig.ParamList do
    match param.TypeAnn with
    | Some typeAnn ->

      typeDeps <-
        typeDeps
        @ findTypeRefIdents
            env
            possibleTypeNames
            (excludedTypeNames @ typeParamNames)
            (SyntaxNode.TypeAnn typeAnn)
    | None -> ()

  match fnSig.ReturnType with
  | None -> ()
  | Some returnType ->
    let typeParamNames: list<QualifiedIdent> =
      match fnSig.TypeParams with
      | None -> []
      | Some typeParams ->
        List.map
          (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
          typeParams

    typeDeps <-
      typeDeps
      @ findTypeRefIdents
          env
          possibleTypeNames
          (excludedTypeNames @ typeParamNames)
          (SyntaxNode.TypeAnn returnType)

  let deps =
    match body with
    | None -> typeDeps
    | Some body ->
      let f: Function =
        { Sig = fnSig
          Body = body
          Captures = None
          InferredType = None }

      findCaptures possibleDeps [] f @ typeDeps

  deps

// TODO: try to reuse this for type declarations with type params
let getDepsForInterfaceFn
  (env: Env)
  (possibleDeps: list<QDeclIdent>)
  (interfaceTypeParamNames: list<QualifiedIdent>)
  (fnSig: FuncSig)
  : list<QDeclIdent> =

  let localTypeNames =
    List.choose
      (fun id ->
        match id with
        | QDeclIdent.Type name -> Some name
        | _ -> None)
      possibleDeps

  let mutable deps = []

  let typeParamNames =
    match fnSig.TypeParams with
    | None -> []
    | Some typeParams ->
      List.map
        (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
        typeParams

  for param in fnSig.ParamList do
    match param.TypeAnn with
    | Some typeAnn ->
      deps <-
        deps
        @ findTypeRefIdents
            env
            localTypeNames
            (interfaceTypeParamNames @ typeParamNames)
            (SyntaxNode.TypeAnn typeAnn)
    | None -> ()

  match fnSig.ReturnType with
  | Some returnType ->
    deps <-
      deps
      @ findTypeRefIdents
          env
          localTypeNames
          (interfaceTypeParamNames @ typeParamNames)
          (SyntaxNode.TypeAnn returnType)
  | None -> ()

  deps

// TODO: we should be using this for more than PropName dependencies
let getPropNameDeps
  (env: Env)
  (topLevelDecls: list<QDeclIdent>)
  (name: PropName)
  : list<QDeclIdent> =
  match name with
  | Computed expr ->
    match expr.Kind with
    | ExprKind.Member(target, name, opt_chain) ->
      match target.Kind with
      | ExprKind.Identifier name ->
        let ident = (QualifiedIdent.FromString name)

        if List.contains (QDeclIdent.Value ident) topLevelDecls then
          [ QDeclIdent.Value ident ]
        else
          match env.TryFindValue name with
          | Some(t, _) ->
            match t.Kind with
            | TypeKind.TypeRef { Name = ident } ->
              let ident = QualifiedIdent.FromCommonQualifiedIdent ident

              if List.contains (QDeclIdent.Type ident) topLevelDecls then
                [ QDeclIdent.Type ident ]
              else
                []
            | _ -> [] // This should probably be an error
          | None -> [] // This should probably be an error
      | _ -> []
    | _ -> []
  | _ -> []

let getDeclsFromModule (ast: Module) : list<Decl> =
  List.choose
    (fun item ->
      match item with
      | Decl decl -> Some decl
      | _ -> None)
    ast.Items

let getNodes (decls: list<Decl>) : Map<QDeclIdent, list<Decl>> =
  let mutable nodes: Map<QDeclIdent, list<Decl>> = Map.empty

  let rec getNodesRec (decls: list<Decl>) (namespaces: list<string>) : unit =
    for decl in decls do
      match decl.Kind with
      | VarDecl { Pattern = pattern
                  Init = init
                  TypeAnn = typeAnn } ->
        let idents =
          Helpers.findBindingNames pattern
          |> List.map (fun name ->
            QDeclIdent.Value({ Namespaces = namespaces; Name = name }))

        for ident in idents do
          nodes <- nodes.Add(ident, [ decl ])
      | FnDecl { Name = name } ->
        let key = { Namespaces = namespaces; Name = name }
        // TODO: support function overloading
        nodes <- nodes.Add(QDeclIdent.Value(key), [ decl ])
      | ClassDecl { Name = name }
      | EnumDecl { Name = name } ->
        let key = { Namespaces = namespaces; Name = name }
        nodes <- nodes.Add(QDeclIdent.Value(key), [ decl ])
        nodes <- nodes.Add(QDeclIdent.Type(key), [ decl ])
      | TypeDecl { Name = name } ->
        let key = { Namespaces = namespaces; Name = name }
        nodes <- nodes.Add(QDeclIdent.Type(key), [ decl ])
      | InterfaceDecl { Name = name } ->
        let key = { Namespaces = namespaces; Name = name }

        let decls =
          match nodes.TryFind(QDeclIdent.Type(key)) with
          | Some nodes -> nodes @ [ decl ]
          | None -> [ decl ]

        nodes <- nodes.Add(QDeclIdent.Type(key), decls)
      | NamespaceDecl { Name = name; Body = decls } ->
        getNodesRec decls (namespaces @ [ name ])

  getNodesRec decls []
  nodes

let getEdges
  (env: Env)
  (locals: list<QDeclIdent>)
  (nodes: Map<QDeclIdent, list<Decl>>)
  : Map<QDeclIdent, list<QDeclIdent>> =
  let mutable edges: Map<QDeclIdent, list<QDeclIdent>> = Map.empty

  let localTypeNames =
    nodes.Keys
    |> List.ofSeq
    |> List.choose (fun qid ->
      match qid with
      | Type name -> Some name
      | _ -> None)

  for KeyValue(ident, decls) in nodes do
    for decl in decls do
      match decl.Kind with
      | VarDecl { Pattern = pattern
                  Init = init
                  TypeAnn = typeAnn } ->

        // TODO: cache deps computation for each declaration to optimize decls
        // that introduce multiple bindings
        let deps =
          match init with
          | Some init ->
            let mutable deps = findIdentifiers locals ident init

            // TODO: reimplement findTypeRefIdents to only look at localTypeNames
            let typeDepsInExpr =
              findTypeRefIdents env localTypeNames [] (SyntaxNode.Expr init)

            let typeDeps =
              match typeAnn with
              | Some typeAnn ->
                findTypeRefIdents
                  env
                  localTypeNames
                  []
                  (SyntaxNode.TypeAnn typeAnn)
              | None -> []

            deps <- deps @ typeDepsInExpr @ typeDeps

            // TODO: dedupe with the other branch
            // TODO: check declaration order using a separate pass
            // for dep in deps do
            //   match dep with
            //   | QDeclIdent.Type _ -> ()
            //   | QDeclIdent.Value _ ->
            //     if not (List.contains dep declared) then
            //       let depIdent =
            //         match dep with
            //         | QDeclIdent.Value ident -> ident
            //         | QDeclIdent.Type ident -> ident
            //
            //       failwith $"{depIdent} has not been initialized yet"

            let functions = Graph.findFunctions init

            for f in functions do
              let captures = findCaptures locals [] f
              deps <- deps @ captures

            deps
          | None ->
            let deps =
              match typeAnn with
              | Some typeAnn ->
                findTypeRefIdents
                  env
                  localTypeNames
                  []
                  (SyntaxNode.TypeAnn typeAnn)
              | None -> []

            // TODO: dedupe with the other branch
            // for dep in deps do
            //   match dep with
            //   | Type _ -> ()
            //   | Value _ ->
            //     if not (List.contains dep declared) then
            //       let depName =
            //         match dep with
            //         | DeclIdent.Value name -> name
            //         | DeclIdent.Type name -> name
            //
            //       return!
            //         Error(
            //           TypeError.SemanticError
            //             $"{depName} has not been initialized yet"
            //         )

            deps

        edges <- edges.Add(ident, deps)
      | FnDecl { Name = name
                 Sig = fnSig
                 Body = body } ->
        let deps = getDepsForFn env locals [] fnSig body
        edges <- edges.Add(ident, deps)
      | ClassDecl { Name = name } ->
        let deps =
          match ident with
          | Type _ ->
            // TODO: determine instance deps
            []
          | Value qualifiedIdent ->
            // TODO: determine static deps
            []

        edges <- edges.Add(ident, deps)
      | TypeDecl { Name = name
                   TypeParams = typeParams
                   TypeAnn = typeAnn } ->
        let mutable deps: list<QDeclIdent> = []

        let typeParamNames =
          match typeParams with
          | None -> []
          | Some typeParams ->
            List.map
              (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
              typeParams

        match typeParams with
        | None -> ()
        | Some typeParams ->
          for typeParam in typeParams do
            match typeParam.Constraint with
            | Some c ->
              deps <-
                deps
                @ findTypeRefIdents
                    env
                    localTypeNames
                    typeParamNames
                    (SyntaxNode.TypeAnn c)
            | None -> ()

            match typeParam.Default with
            | Some d ->
              deps <-
                deps
                @ findTypeRefIdents
                    env
                    localTypeNames
                    typeParamNames
                    (SyntaxNode.TypeAnn d)
            | None -> ()

        deps <-
          deps
          @ findTypeRefIdents
              env
              localTypeNames
              typeParamNames
              (SyntaxNode.TypeAnn typeAnn)

        edges <- edges.Add(ident, deps)
      | InterfaceDecl { Name = name
                        TypeParams = typeParams
                        Elems = elems } ->
        let interfaceTypeParamNames =
          match typeParams with
          | None -> []
          | Some typeParams ->
            List.map
              (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
              typeParams

        let deps =
          elems
          |> List.collect (fun elem ->
            match elem with
            | ObjTypeAnnElem.Callable fnSig ->
              getDepsForInterfaceFn env locals interfaceTypeParamNames fnSig
            | ObjTypeAnnElem.Constructor fnSig ->
              getDepsForInterfaceFn env locals interfaceTypeParamNames fnSig
            | ObjTypeAnnElem.Method { Type = fnSig; Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let fnDeps =
                getDepsForInterfaceFn env locals interfaceTypeParamNames fnSig

              propNameDeps @ fnDeps
            | ObjTypeAnnElem.Getter { ReturnType = returnType; Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let fnDeps =
                match returnType with
                | Some returnType ->
                  findTypeRefIdents
                    env
                    localTypeNames
                    interfaceTypeParamNames
                    (SyntaxNode.TypeAnn returnType)
                | None -> []

              propNameDeps @ fnDeps
            | ObjTypeAnnElem.Setter { Param = { TypeAnn = typeAnn }
                                      Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let fnDeps =
                match typeAnn with
                | Some typeAnn ->
                  findTypeRefIdents
                    env
                    localTypeNames
                    interfaceTypeParamNames
                    (SyntaxNode.TypeAnn typeAnn)
                | None -> []

              propNameDeps @ fnDeps
            | ObjTypeAnnElem.Property { TypeAnn = typeAnn; Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let typeAnnDeps =
                findTypeRefIdents
                  env
                  localTypeNames
                  interfaceTypeParamNames
                  (SyntaxNode.TypeAnn typeAnn)

              propNameDeps @ typeAnnDeps
            | ObjTypeAnnElem.Mapped { TypeParam = typeParam
                                      TypeAnn = typeAnn } ->
              let tp: TypeParam =
                { Span = DUMMY_SPAN
                  Name = typeParam.Name
                  Constraint = Some typeParam.Constraint
                  Default = None }

              let typeParams =
                match typeParams with
                | None -> Some [ tp ]
                | Some typeParams -> Some(tp :: typeParams)

              findTypeRefIdents
                env
                localTypeNames
                interfaceTypeParamNames
                (SyntaxNode.TypeAnn typeParam.Constraint)
              @ findTypeRefIdents
                  env
                  localTypeNames
                  interfaceTypeParamNames
                  (SyntaxNode.TypeAnn typeAnn))

        match edges.TryFind(ident) with
        | Some existingDeps -> edges <- edges.Add(ident, existingDeps @ deps)
        | None -> edges <- edges.Add(ident, deps)
      | EnumDecl { Name = name } ->
        let deps =
          match ident with
          | Type _ ->
            // TODO: determine instance deps
            []
          | Value _ ->
            // NOTE: enums have no static deps
            []

        edges <- edges.Add(ident, deps)
      | NamespaceDecl { Name = name; Body = body } ->
        // Namespaces are neither values or types but rather containers for
        // values and types. We don't need to add them to the edges map.
        ()

  edges

let buildGraph (env: Env) (m: Module) : QGraph =

  let mutable graph = { Nodes = Map.empty; Edges = Map.empty }

  let decls = getDeclsFromModule m
  let locals = findLocals decls

  let localTypeNames =
    List.choose
      (fun qid ->
        match qid with
        | Type name -> Some name
        | _ -> None)
      locals

  let decls =
    m.Items
    |> List.choose (fun item ->
      match item with
      | ModuleItem.Decl decl -> Some decl
      | _ -> None)

  let nodes = getNodes decls
  let edges = getEdges env locals nodes

  { Nodes = nodes; Edges = edges }

let getAllBindingPatterns (pattern: Pattern) : Map<string, Type> =
  let mutable result = Map.empty

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | ExprKind.Function _ -> (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (false, state)
      ExprVisitor.VisitPattern =
        fun (pat, state) ->
          match pat.Kind with
          | PatternKind.Ident { Name = name } ->
            match pat.InferredType with
            | Some t -> result <- Map.add name t result
            | None -> ()

            (false, state)
          | PatternKind.Object { Elems = elems } ->
            for elem in elems do
              match elem with
              | ShorthandPat { Name = name; Inferred = inferred } ->
                match inferred with
                | Some t -> result <- Map.add name t result
                | None -> ()
              | _ -> ()

            (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  walkPattern visitor () pattern

  result

let inferDeclPlaceholders
  (ctx: Ctx)
  (env: Env)
  (qns: QualifiedNamespace)
  (idents: list<QDeclIdent>)
  (graph: QGraph)
  : Result<QualifiedNamespace, TypeError> =

  result {
    let mutable qns = qns

    for ident in idents do
      let decls = graph.Nodes[ident]

      for decl in decls do
        match decl.Kind with
        | VarDecl { Pattern = pattern
                    Init = init
                    TypeAnn = _ } ->
          // QUESTION: Should we check the type annotation as we're generating
          // the placeholder type?
          let! bindings, patternType = Infer.inferPattern ctx env pattern

          match init with
          | Some init ->
            let! structuralPlacholderType =
              Infer.inferExprStructuralPlacholder ctx env init

            do! Unify.unify ctx env None patternType structuralPlacholderType
          | None -> ()

          for KeyValue(name, binding) in bindings do
            let key =
              match ident with
              | Type { Namespaces = namespaces } ->
                { Namespaces = namespaces; Name = name }
              | Value { Namespaces = namespaces } ->
                { Namespaces = namespaces; Name = name }

            qns <- qns.AddValue key binding
        | FnDecl({ Declare = false
                   Sig = fnSig
                   Name = name } as decl) ->
          let! f = Infer.inferFuncSig ctx env fnSig

          let t =
            { Kind = TypeKind.Function f
              Provenance = None }

          qns <- qns.AddValue (QualifiedIdent.FromString name) (t, false)
        | FnDecl({ Declare = true
                   Sig = fnSig
                   Name = name } as decl) ->
          // TODO: dedupe with `inferDecl`
          // TODO: capture these errors as diagnostics and infer the missing
          // types as `never`
          for p in fnSig.ParamList do
            if p.TypeAnn.IsNone then
              failwith "Ambient function declarations must be fully typed"

          if fnSig.ReturnType.IsNone then
            failwith "Ambient function declarations must be fully typed"

          let! f = Infer.inferFuncSig ctx env fnSig

          if fnSig.Throws.IsNone then
            f.Throws <-
              { Kind = TypeKind.Keyword Keyword.Never
                Provenance = None }

          let t =
            { Kind = TypeKind.Function f
              Provenance = None }

          qns <- qns.AddValue (QualifiedIdent.FromString name) (t, false)
        | ClassDecl({ Name = name } as decl) ->
          // TODO: treat ClassDecl similar to object types where we create a
          // structural placeholder type instead of an opaque type variable.
          // We should do this for both instance members and statics.
          let instance: Scheme =
            { Type = ctx.FreshTypeVar None
              TypeParams = None // TODO: handle type params
              IsTypeParam = false }

          qns <- qns.AddScheme (QualifiedIdent.FromString name) instance

          let statics: Type = ctx.FreshTypeVar None

          qns <- qns.AddValue (QualifiedIdent.FromString name) (statics, false)
        | EnumDecl _ ->
          return!
            Error(
              TypeError.NotImplemented "TODO: inferDeclPlaceholders - EnumDecl"
            )
        | TypeDecl typeDecl ->
          // TODO: check to make sure we aren't redefining an existing type

          // TODO: replace placeholders, with a reference the actual definition
          // once we've inferred the definition
          let! placeholder =
            Infer.inferTypeDeclPlaceholderScheme ctx env typeDecl.TypeParams

          qns <-
            qns.AddScheme (QualifiedIdent.FromString typeDecl.Name) placeholder
        | InterfaceDecl({ Name = name; TypeParams = typeParams } as decl) ->
          // Instead of looking things up in the environment, we need some way to
          // find the existing type on other declarations.
          let! placeholder =
            match env.TryFindScheme name with
            | Some scheme -> Result.Ok scheme
            | None ->
              match qns.Schemes.TryFind(QualifiedIdent.FromString name) with
              | Some scheme -> Result.Ok scheme
              | None -> Infer.inferTypeDeclPlaceholderScheme ctx env typeParams

          qns <- qns.AddScheme (QualifiedIdent.FromString name) placeholder
        // newEnv <- newEnv.AddScheme name placeholder
        | NamespaceDecl nsDecl ->
          return!
            Error(
              TypeError.NotImplemented
                "TODO: inferDeclPlaceholders - NamespaceDecl"
            )
    // let subgraph = graph.Namespaces[nsDecl.Name]
    // let! _, ns = Infer.inferDeclPlaceholders ctx env nsDecl.Body subgraph
    // let ns = { ns with Name = nsDecl.Name }
    // placeholderNS <- placeholderNS.AddNamespace nsDecl.Name ns
    // newEnv <- newEnv.AddNamespace nsDecl.Name ns

    return qns
  }

// Copies symbols from the nested namespaces listed in `namespaces` into `env`.
// For example if `namespaces` contains `["Foo", "Bar"]` then this function will
// copy symbols from the `Foo` and `Foo.Bar` namespaces into `env`.
let rec openNamespaces (env: Env) (namespaces: list<string>) : Env =
  let mutable newEnv = env

  let rec openNamespace (parentNS: Namespace) (namespaces: list<string>) =
    match namespaces with
    | [] -> ()
    | head :: rest ->
      let nextNS = parentNS.Namespaces[head]

      for KeyValue(name, scheme) in nextNS.Schemes do
        newEnv <- newEnv.AddScheme name scheme

      for KeyValue(name, binding) in nextNS.Values do
        newEnv <- newEnv.AddValue name binding

      for KeyValue(name, ns) in nextNS.Namespaces do
        newEnv <- newEnv.AddNamespace name ns

      openNamespace nextNS rest

  openNamespace env.Namespace namespaces
  newEnv

let inferDeclDefinitions
  (ctx: Ctx)
  (env: Env)
  (qns: QualifiedNamespace)
  (names: list<QDeclIdent>)
  (graph: QGraph)
  : Result<QualifiedNamespace, TypeError> =

  result {
    let mutable qns = qns

    for name in names do
      let decls = graph.Nodes[name]

      // TODO: check if we're inside a namespace and update the env accordingly
      let namespaces =
        match name with
        | Type { Namespaces = namespaces } -> namespaces
        | Value { Namespaces = namespaces } -> namespaces

      let mutable newEnv = env

      newEnv <- openNamespaces newEnv namespaces

      for decl in decls do
        match decl.Kind with
        | VarDecl varDecl ->
          let placeholderTypes = getAllBindingPatterns varDecl.Pattern

          // NOTE: We explicitly don't generalize here because we want other
          // declarations to be able to unify with any free type variables
          // from this declaration.  We generalize things in `inferModule` and
          // `inferTreeRec`.
          let! newBindings, newSchemes = Infer.inferVarDecl ctx newEnv varDecl

          let inferredTypes = getAllBindingPatterns varDecl.Pattern

          for KeyValue(name, inferredType) in inferredTypes do
            let placeholderType =
              match Map.tryFind name placeholderTypes with
              | Some t -> t
              | None -> failwith "Missing placeholder type"

            do! Unify.unify ctx newEnv None placeholderType inferredType

          // Schemes can be generated for things like class expressions, e.g.
          // let Foo = class { ... }
          for KeyValue(name, scheme) in newSchemes do
            qns <- qns.AddScheme (QualifiedIdent.FromString name) scheme

        | FnDecl({ Declare = false
                   Name = name
                   Sig = fnSig
                   Body = Some body } as decl) ->
          let placeholderType, _ =
            match env.TryFindValue name with
            | Some t -> t
            | None -> failwith "Missing placeholder type"

          // NOTE: We explicitly don't generalize here because we want other
          // declarations to be able to unify with any free type variables
          // from this declaration.  We generalize things in `inferModule` and
          // `inferTreeRec`.
          // NOTE: `inferFunction` also calls unify
          let! f = Infer.inferFunction ctx env fnSig body

          let inferredType =
            { Kind = TypeKind.Function f
              Provenance = None }

          do! Unify.unify ctx env None placeholderType inferredType

          qns <-
            qns.AddValue (QualifiedIdent.FromString name) (inferredType, false)
        | FnDecl { Declare = true } ->
          // Nothing to do since ambient function declarations don't have
          // function bodies.
          ()
        | FnDecl fnDecl ->
          return! Error(TypeError.SemanticError "Invalid function declaration")
        | ClassDecl { Declare = declare
                      Name = name
                      Class = cls } ->
          let! t, scheme = Infer.inferClass ctx env cls declare

          // TODO: update `Statics` and `Instance` on `placeholders`

          ()
        | EnumDecl _ ->
          return!
            Error(
              TypeError.NotImplemented "TODO: inferDeclDefinitions - EnumDecl"
            )
        | TypeDecl { Name = name; TypeAnn = typeAnn } ->
          let placeholder = qns.Schemes[(QualifiedIdent.FromString name)]
          // TODO: when computing the decl graph, include self-recursive types in
          // the deps set so that we don't have to special case this here.
          // Handles self-recursive types
          let newEnv = env.AddScheme name placeholder
          let getType = fun env -> Infer.inferTypeAnn ctx env typeAnn

          let! scheme = Infer.inferTypeDeclDefn ctx newEnv placeholder getType

          // Replace the placeholder's type with the actual type.
          // NOTE: This is a bit hacky and we may want to change this later to use
          // `foldType` to replace any uses of the placeholder with the actual type.
          // Required for the following test cases:
          // - InferRecursiveGenericObjectTypeInModule
          // - InferNamespaceInModule
          // placeholder.Value.Type <- scheme.Type
          qns.Schemes[(QualifiedIdent.FromString name)].Type <- scheme.Type
        | InterfaceDecl { Name = name
                          TypeParams = typeParams
                          Elems = elems } ->
          let placeholder = qns.Schemes[(QualifiedIdent.FromString name)]

          // TODO: when computing the decl graph, include self-recursive types in
          // the deps set so that we don't have to special case this here.
          // Handles self-recursive types
          let newEnv = env.AddScheme name placeholder

          let getType (env: Env) : Result<Type, TypeError> =
            result {
              let! elems =
                List.traverseResultM (Infer.inferObjElem ctx newEnv) elems

              let kind =
                TypeKind.Object
                  { Elems = elems
                    Immutable = false
                    Interface = true }

              return { Kind = kind; Provenance = None }
            }

          let! newScheme =
            Infer.inferTypeDeclDefn ctx newEnv placeholder getType

          match placeholder.Type.Kind, newScheme.Type.Kind with
          | TypeKind.Object { Elems = existingElems },
            TypeKind.Object { Elems = newElems } ->
            // TODO: remove duplicates
            let mergedElems = existingElems @ newElems

            let kind =
              TypeKind.Object
                { Elems = mergedElems
                  Immutable = false
                  Interface = false }

            // NOTE: We explicitly don't generalize here because we want other
            // declarations to be able to unify with any free type variables
            // from this declaration.  We generalize things in `inferModule` and
            // `inferTreeRec`.
            // TODO: suport multiple provenances
            let t = { Kind = kind; Provenance = None }

            // We modify the existing scheme in place so that existing values
            // with this type are updated.
            placeholder.Type <- t
            qns.Schemes[(QualifiedIdent.FromString name)].Type <- t
          | _ ->
            // Replace the placeholder's type with the actual type.
            // NOTE: This is a bit hacky and we may want to change this later to use
            // `foldType` to replace any uses of the placeholder with the actual type.
            placeholder.Type <- newScheme.Type
            qns.Schemes[(QualifiedIdent.FromString name)].Type <- newScheme.Type
        | NamespaceDecl { Name = name; Body = decls } ->
          return!
            Error(
              TypeError.NotImplemented
                "TODO: inferDeclDefinitions - NamespaceDecl"
            )

    return qns
  }

type QDeclTree =
  { Edges: Map<Set<QDeclIdent>, Set<Set<QDeclIdent>>>
    CycleMap: Map<QDeclIdent, Set<QDeclIdent>> }

let rec graphToTree (edges: Map<QDeclIdent, list<QDeclIdent>>) : QDeclTree =
  let mutable visited: list<QDeclIdent> = []
  let mutable stack: list<QDeclIdent> = []
  let mutable cycles: Set<Set<QDeclIdent>> = Set.empty

  let rec visit (node: QDeclIdent) (parents: list<QDeclIdent>) =
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

  let mutable cycleMap: Map<QDeclIdent, Set<QDeclIdent>> = Map.empty

  for cycle in cycles do
    for node in cycle do
      cycleMap <- cycleMap.Add(node, cycle)

  let mutable newEdges: Map<Set<QDeclIdent>, Set<Set<QDeclIdent>>> = Map.empty

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

let addBinding (env: Env) (ident: QualifiedIdent) (binding: Binding) : Env =

  let rec addValueRec (ns: Namespace) (namespaces: list<string>) : Namespace =
    match namespaces with
    | [] -> ns.AddBinding ident.Name binding
    | headNS :: restNS ->
      match ns.Namespaces.TryFind(headNS) with
      | None ->
        let newNS = { Namespace.empty with Name = headNS }
        ns.AddNamespace headNS (addValueRec newNS restNS)
      | Some existingNS ->
        ns.AddNamespace headNS (addValueRec existingNS restNS)

  match ident.Namespaces with
  | [] -> env.AddValue ident.Name binding
  | namespaces ->
    { env with
        Namespace = addValueRec env.Namespace namespaces }

let addScheme (env: Env) (ident: QualifiedIdent) (scheme: Scheme) : Env =

  let rec addValueRec (ns: Namespace) (namespaces: list<string>) : Namespace =
    match namespaces with
    | [] -> ns.AddScheme ident.Name scheme
    | headNS :: restNS ->
      match ns.Namespaces.TryFind(headNS) with
      | None -> ns.AddNamespace headNS (addValueRec Namespace.empty restNS)
      | Some ns ->
        ns.AddNamespace headNS (addValueRec ns.Namespaces[headNS] restNS)

  match ident.Namespaces with
  | [] -> env.AddScheme ident.Name scheme
  | head :: rest -> env.AddNamespace head (addValueRec env.Namespace rest)

let updateEnvWithQualifiedNamespace
  (env: Env)
  (inferredLocals: QualifiedNamespace)
  : Env =
  let mutable newEnv = env

  for KeyValue(ident, binding) in inferredLocals.Values do
    newEnv <- addBinding newEnv ident binding

  for KeyValue(ident, scheme) in inferredLocals.Schemes do
    newEnv <- addScheme newEnv ident scheme

  newEnv

let updateQualifiedNamespace
  (src: QualifiedNamespace)
  (dst: QualifiedNamespace)
  (generalize: bool)
  : QualifiedNamespace =
  let mutable dst = dst

  for KeyValue(key, binding) in src.Values do
    if generalize then
      let t, isMut = binding
      let t = Helpers.generalizeFunctionsInType t
      dst <- dst.AddValue key (t, isMut)
    else
      dst <- dst.AddValue key binding

  for KeyValue(key, scheme) in src.Schemes do
    dst <- dst.AddScheme key scheme

  dst

let rec inferTreeRec
  (ctx: Ctx)
  (env: Env)
  (root: Set<QDeclIdent>)
  (graph: QGraph)
  (tree: QDeclTree)
  (fullQns: QualifiedNamespace)
  : Result<QualifiedNamespace * QualifiedNamespace, TypeError> =

  result {
    let mutable fullQns = fullQns
    let mutable partialQns = QualifiedNamespace.Empty

    // Infer dependencies
    match tree.Edges.TryFind root with
    | Some deps ->
      for dep in deps do
        // TODO: avoid re-inferring types if they've already been inferred
        let! newFullQns, newPartialQns =
          inferTreeRec ctx env dep graph tree fullQns

        // Update partialQns with new values and types
        partialQns <- updateQualifiedNamespace newPartialQns partialQns false

    | None -> ()

    // Update environment to include dependencies
    let mutable newEnv = updateEnvWithQualifiedNamespace env partialQns

    let names = Set.toList root

    // Infer declarations
    let! newPartialQns = inferDeclPlaceholders ctx newEnv partialQns names graph
    newEnv <- updateEnvWithQualifiedNamespace newEnv newPartialQns // mutually recursive functions

    let! newPartialQns =
      inferDeclDefinitions ctx newEnv newPartialQns names graph

    // Update fullQns with new values and types
    fullQns <- updateQualifiedNamespace newPartialQns fullQns true

    return fullQns, newPartialQns
  }

let inferGraph (ctx: Ctx) (env: Env) (graph: QGraph) : Result<Env, TypeError> =
  result {
    let tree = graphToTree graph.Edges
    let deps = tree.Edges.Values |> Set.unionMany |> Set.unionMany

    let mutable entryPoints: Set<Set<QDeclIdent>> = Set.empty

    for KeyValue(key, value) in graph.Nodes do
      match tree.CycleMap.TryFind(key) with
      | Some set -> entryPoints <- entryPoints.Add(set)
      | None ->
        // Anything that appears in deps can't be an entry point
        if not (Set.contains key deps) then
          entryPoints <- entryPoints.Add(Set.singleton key)

    let mutable qns = QualifiedNamespace.Empty

    // Infer entry points and all their dependencies
    for set in entryPoints do
      try
        let! fullQns, _ = inferTreeRec ctx env set graph tree qns
        qns <- fullQns
      with e ->
        printfn $"Error: {e}"
        return! Error(TypeError.SemanticError(e.ToString()))

    // NOTE: We could also return inferredLocals instead of adding them to the
    // environment.  The only time we actually need to add things to the
    // environment is when we're dealing with globals.

    // Update environment with all new values and types
    return updateEnvWithQualifiedNamespace env qns
  }
