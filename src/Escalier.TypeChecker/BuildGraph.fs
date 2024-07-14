module rec Escalier.TypeChecker.BuildGraph

open Escalier.Data.Type
open Escalier.TypeChecker.Env

open Escalier.Data.Syntax

open Escalier.TypeChecker.QualifiedGraph
open ExprVisitor

let start = FParsec.Position("", 0, 1, 1)
let stop = FParsec.Position("", 0, 1, 1)
let DUMMY_SPAN: Span = { Start = start; Stop = stop }

let rec memberToQualifiedIdent
  (target: Expr)
  (name: string)
  : option<QualifiedIdent> =
  match target.Kind with
  | ExprKind.Member(target, innerName, _) ->
    match memberToQualifiedIdent target innerName with
    | Some qid -> Some({ Parts = qid.Parts @ [ name ] })
    | None -> None
  | ExprKind.Identifier innerName -> Some({ Parts = [ innerName; name ] })
  | _ -> None

type QDeclTree =
  { Values: Map<string, QDeclIdent>
    Types: Map<string, QDeclIdent>
    Namespaces: Map<string, QDeclTree> }

let localsToDeclTree (locals: Set<QDeclIdent>) : QDeclTree =

  let mutable tree: QDeclTree =
    { Values = Map.empty
      Types = Map.empty
      Namespaces = Map.empty }

  let rec processLocal (tree: QDeclTree) (rest: QDeclIdent) (full: QDeclIdent) =
    let parts = rest.GetParts()

    match parts with
    | [] -> tree // this shouldn't happen
    | [ name ] ->
      match full with
      | Type _ ->
        { tree with
            Types = Map.add name full tree.Types }
      | Value _ ->
        { tree with
            Values = Map.add name full tree.Values }
    // { tree with
    //     Decls = Map.add name full tree.Decls }
    | head :: tail ->
      let ns =
        match Map.tryFind head tree.Namespaces with
        | Some ns -> ns
        | None ->
          { Values = Map.empty
            Types = Map.empty
            Namespaces = Map.empty }

      let rest =
        match full with
        | Type _ -> QDeclIdent.Type({ Parts = tail })
        | Value _ -> QDeclIdent.Value({ Parts = tail })

      { tree with
          Namespaces = Map.add head (processLocal ns rest full) tree.Namespaces }

  for local in locals do
    tree <- processLocal tree local local

  tree

let getLocalForDep (tree: QDeclTree) (local: QDeclIdent) : option<QDeclIdent> =
  let rec getLocalForDepRec
    (tree: QDeclTree)
    (local: QDeclIdent)
    : option<QDeclIdent> =
    let parts = local.GetParts()

    match parts with
    | [] -> None
    | [ name ] ->
      match local with
      | Type _ -> Map.tryFind name tree.Types
      | Value _ -> Map.tryFind name tree.Values
    | name :: rest ->
      match local with
      | Type _ ->
        match Map.tryFind name tree.Types with
        | Some local -> Some local
        | None ->
          match Map.tryFind name tree.Namespaces with
          | Some tree ->
            getLocalForDepRec tree (QDeclIdent.Type({ Parts = rest }))
          | None -> None
      | Value _ ->
        match Map.tryFind name tree.Values with
        | Some local -> Some local
        | None ->
          match Map.tryFind name tree.Namespaces with
          | Some tree ->
            getLocalForDepRec tree (QDeclIdent.Value({ Parts = rest }))
          | None -> None

  getLocalForDepRec tree local

let postProcessDeps
  (ns: Namespace)
  (locals: Set<QDeclIdent>)
  (localsTree: QDeclTree)
  (ident: QDeclIdent)
  (deps: list<QDeclIdent>)
  : Set<QDeclIdent> =

  let identNamespaces =
    match ident with
    | Type { Parts = namespaces } -> namespaces
    | Value { Parts = namespaces } -> namespaces

  let identNamespaces = List.take (identNamespaces.Length - 1) identNamespaces

  let globalNS = Map.tryFind "global" ns.Namespaces
  let maybeGlobalTree = Map.tryFind "global" localsTree.Namespaces

  let mutable hasLength = false

  // TODO: we want to allow modules to declare variables that shadow globals
  // since redeclaring variables in a module is allowed.
  // Filter out deps that are in the environment
  // let deps =
  //   deps
  //   |> List.filter (fun dep ->
  //
  //     match dep with
  //     | Type { Parts = parts } ->
  //       let head = List.head parts
  //
  //       if
  //         Map.containsKey head ns.Schemes || Map.containsKey head ns.Namespaces
  //       then
  //         false
  //       else
  //         true
  //     | Value { Parts = parts } ->
  //       let head = List.head parts
  //
  //       if head = "length" then
  //         hasLength <- Map.containsKey head ns.Values
  //         printfn $"hasLength (for length) = {hasLength}"
  //
  //       if
  //         Map.containsKey head ns.Values || Map.containsKey head ns.Namespaces
  //       then
  //         false
  //       else
  //         true)

  if hasLength then
    printfn $"deps = {deps}"

  if ident.GetParts().Length > 1 then
    deps
    |> List.choose (fun dep ->
      let mutable candidateNamespaces: list<list<string>> = [ [] ]

      for ns in identNamespaces do
        let prevNS = candidateNamespaces.Head
        candidateNamespaces <- (ns :: prevNS) :: candidateNamespaces

      let mutable result = None

      for ns in candidateNamespaces do
        if result.IsNone then
          let candidateDep =
            match dep with
            | Type qid -> Type { Parts = ns @ qid.Parts }
            | Value qid -> Value { Parts = ns @ qid.Parts }

          result <- getLocalForDep localsTree candidateDep

      result)
    |> Set.ofList
  else
    List.choose
      (fun dep ->
        match getLocalForDep localsTree dep with
        | Some local -> Some local
        | None ->
          match maybeGlobalTree with
          | Some tree -> getLocalForDep tree dep
          | None -> None)
      deps
    |> Set.ofList

let findDepsForValueIdent
  (env: Env)
  (locals: Set<QDeclIdent>)
  (localsTree: QDeclTree)
  (ident: QDeclIdent)
  (expr: Expr)
  : Set<QDeclIdent> =
  let mutable idents: Set<QDeclIdent> = Set.empty

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | ExprKind.Member(target, name, _) ->
            match memberToQualifiedIdent target name with
            | Some qid ->
              idents <- Set.add (QDeclIdent.Value qid) idents
              (false, state)
            | None -> (true, state)
          | ExprKind.Identifier name ->
            idents <-
              Set.add (QDeclIdent.Value(QualifiedIdent.FromString name)) idents

            (false, state)
          | ExprKind.Function f ->
            idents <-
              Set.union
                idents
                (getDepsForFn env ident locals localsTree [] f.Sig (Some f.Body))

            (false, state)
          | ExprKind.Object { Elems = elems } ->
            for elem in elems do
              match elem with
              | ObjElem.Property(span, name, value) -> () // handled by visiting `value`
              | ObjElem.Shorthand(span, name) ->
                idents <-
                  Set.add
                    (QDeclIdent.Value(QualifiedIdent.FromString name))
                    idents
              | ObjElem.Spread(span, value) -> () // handled by visiting `value`

            (true, state)
          | _ -> (true, state)
      ExprVisitor.VisitJsxElement =
        fun (jsxElement, state) ->
          let name = jsxElement.Opening.Name

          idents <-
            Set.add
              (QDeclIdent.Value(QualifiedIdent.FromCommonQualifiedIdent name))
              idents

          (true, state)
      ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn =
        fun (typeAnn, state) ->
          match typeAnn.Kind with
          | TypeAnnKind.TypeRef { Ident = ident } -> (false, state)
          | TypeAnnKind.Typeof ident ->
            idents <-
              Set.add
                (QDeclIdent.Value(QualifiedIdent.FromCommonQualifiedIdent ident))
                idents

            (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (true, state) }

  walkExpr visitor () expr

  postProcessDeps env.Namespace locals localsTree ident (Set.toList idents)

let findInferTypeAnns (typeAnn: TypeAnn) : list<QDeclIdent> =
  let mutable idents: list<QDeclIdent> = []

  let visitor: SyntaxVisitor<unit> =
    { ExprVisitor.VisitExpr = fun (_, state) -> (false, state)
      ExprVisitor.VisitJsxElement = fun (_, state) -> (false, state)
      ExprVisitor.VisitJsxFragment = fun (_, state) -> (false, state)
      ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
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

let findDepsForTypeIdent
  (env: Env)
  (possibleDeps: Set<QDeclIdent>)
  (localsTree: QDeclTree)
  (typeParams: list<QualifiedIdent>)
  (ident: QDeclIdent)
  (syntaxNode: SyntaxNode)
  : Set<QDeclIdent> =

  let mutable typeRefIdents: list<QDeclIdent> = []

  let visitor: SyntaxVisitor<list<QualifiedIdent>> =
    { ExprVisitor.VisitExpr = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn =
        fun (typeAnn, typeParams) ->
          let newTypeParams =
            match typeAnn.Kind with
            | TypeAnnKind.TypeRef { Ident = ident; TypeArgs = typeArgs } ->
              let ident = QualifiedIdent.FromCommonQualifiedIdent ident

              if not (List.contains ident typeParams) then
                typeRefIdents <- QDeclIdent.Type ident :: typeRefIdents

              // TODO: handle typeArgs

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
  | SyntaxNode.TypeRef typeRef ->
    let ident = QualifiedIdent.FromCommonQualifiedIdent typeRef.Ident

    typeRefIdents <- QDeclIdent.Type ident :: typeRefIdents

    match typeRef.TypeArgs with
    | Some typeArgs ->
      for typeAnn in typeArgs do
        walkTypeAnn visitor typeParams typeAnn
    | None -> ()
  | SyntaxNode.Expr expr -> walkExpr visitor typeParams expr

  postProcessDeps
    env.Namespace
    possibleDeps
    localsTree
    ident
    (List.rev typeRefIdents)


let findLocals (decls: list<Decl>) : Set<QDeclIdent> =
  let mutable locals: list<QDeclIdent> = []

  let rec findLocalsRec
    (decls: list<Decl>)
    (namespaces: list<string>)
    : Set<QDeclIdent> =
    let mutable locals: Set<QDeclIdent> = Set.empty

    for decl in decls do
      match decl.Kind with
      | VarDecl { Pattern = pattern } ->
        let bindingNames =
          Helpers.findBindingNames pattern
          |> Set.map (fun name ->
            QDeclIdent.Value({ Parts = namespaces @ [ name ] }))

        locals <- Set.union locals bindingNames
      | FnDecl { Name = name } ->
        locals <-
          Set.add (QDeclIdent.Value({ Parts = namespaces @ [ name ] })) locals

      | ClassDecl { Name = name } ->
        locals <-
          Set.union
            locals
            (Set.ofList
              [ QDeclIdent.Value({ Parts = namespaces @ [ name ] })
                QDeclIdent.Type({ Parts = namespaces @ [ name ] }) ])
      | TypeDecl { Name = name } ->
        locals <-
          Set.add (QDeclIdent.Type({ Parts = namespaces @ [ name ] })) locals
      | InterfaceDecl { Name = name } ->
        locals <-
          Set.add (QDeclIdent.Type({ Parts = namespaces @ [ name ] })) locals
      | EnumDecl { Name = name } ->
        locals <-
          Set.union
            locals
            (Set.ofList
              [ QDeclIdent.Value({ Parts = namespaces @ [ name ] })
                QDeclIdent.Type({ Parts = namespaces @ [ name ] }) ])
      | NamespaceDecl { Name = name; Body = decls } ->
        locals <- Set.union locals (findLocalsRec decls (namespaces @ [ name ]))

    locals

  findLocalsRec decls []

// TODO: update this function to accept a QualifiedNamespace as an argument
// Only items in this namespace can be considered as a potential captures
let rec findCaptures
  (qns: Set<QDeclIdent>) // TODO: update this to use QualifiedNamespace
  (parentLocals: Set<QDeclIdent>)
  (f: Function)
  : Set<QDeclIdent> =

  let qnsNames =
    qns
    |> Set.toList
    |> List.choose (fun (qid: QDeclIdent) ->
      match qid with
      | QDeclIdent.Value name -> Some name
      | QDeclIdent.Type name -> None)

  let parentLocalNames =
    parentLocals
    |> Set.toList
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
    |> Set.toList
    |> List.choose (fun (qid: QDeclIdent) ->
      match qid with
      | QDeclIdent.Value name -> Some name
      | QDeclIdent.Type name -> None)
    |> Set.ofList

  for p in f.Sig.ParamList do
    let patternIdents =
      Helpers.findBindingNames p.Pattern |> Set.map QualifiedIdent.FromString

    localNames <- Set.union localNames patternIdents

  let mutable captures: Set<QDeclIdent> = Set.empty

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, localNames: Set<QualifiedIdent>) ->
          match expr.Kind with
          // NOTE: we don't have to do any special handling for
          // ExprKind.Member because the property is stored as a
          // string instead of an identifier.
          | ExprKind.Identifier name ->
            let ident = QualifiedIdent.FromString name

            if
              (List.contains ident qnsNames)
              && not (List.contains ident parentLocalNames)
              && not (Set.contains ident localNames)
            then
              captures <- Set.add (QDeclIdent.Value ident) captures

            (false, localNames)
          | ExprKind.Function f ->
            captures <- findCaptures qns (Set.union parentLocals locals) f
            // Don't recurse since `findCaptures` already does that
            (false, localNames)
          | _ -> (true, localNames)
      ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
      ExprVisitor.VisitStmt =
        fun (stmt, state) ->
          let bindingNames =
            match stmt.Kind with
            | StmtKind.Decl decl ->
              match decl.Kind with
              | DeclKind.VarDecl { Pattern = pattern } ->
                Helpers.findBindingNames pattern
              | _ -> Set.empty // TODO: handle other kinds of declarations
            | _ -> Set.empty

          (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  match f.Body with
  | BlockOrExpr.Block block ->
    List.iter (walkStmt visitor localNames) block.Stmts
  | BlockOrExpr.Expr expr -> walkExpr visitor localNames expr

  captures

let getDepsForFn
  (env: Env)
  (ident: QDeclIdent)
  (possibleDeps: Set<QDeclIdent>)
  (localsTree: QDeclTree)
  (excludedTypeNames: list<QualifiedIdent>)
  (fnSig: FuncSig)
  (body: option<BlockOrExpr>)
  : Set<QDeclIdent> =
  let mutable typeDeps: Set<QDeclIdent> = Set.empty

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
          Set.union
            typeDeps
            (findDepsForTypeIdent
              env
              possibleDeps
              localsTree
              (excludedTypeNames @ typeParamNames)
              ident
              (SyntaxNode.TypeAnn c))
      | None -> ()

      match tp.Default with
      | Some d ->
        typeDeps <-
          Set.union
            typeDeps
            (findDepsForTypeIdent
              env
              possibleDeps
              localsTree
              (excludedTypeNames @ typeParamNames)
              ident
              (SyntaxNode.TypeAnn d))
      | None -> ()
  | None -> ()

  for param in fnSig.ParamList do
    match param.TypeAnn with
    | Some typeAnn ->
      typeDeps <-
        Set.union
          typeDeps
          (findDepsForTypeIdent
            env
            possibleDeps
            localsTree
            (excludedTypeNames @ typeParamNames)
            ident
            (SyntaxNode.TypeAnn typeAnn))
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
      Set.union
        typeDeps
        (findDepsForTypeIdent
          env
          possibleDeps
          localsTree
          (excludedTypeNames @ typeParamNames)
          ident
          (SyntaxNode.TypeAnn returnType))

  let deps =
    match body with
    | None -> typeDeps
    | Some body ->
      let f: Function =
        { Sig = fnSig
          Body = body
          Captures = None
          InferredType = None }

      Set.union (findCaptures possibleDeps Set.empty f) typeDeps

  deps

// TODO: try to reuse this for type declarations with type params
let getDepsForInterfaceFn
  (env: Env)
  (possibleDeps: Set<QDeclIdent>)
  (localsTree: QDeclTree)
  (interfaceTypeParamNames: list<QualifiedIdent>)
  (ident: QDeclIdent)
  (fnSig: FuncSig)
  : Set<QDeclIdent> =

  let mutable deps: Set<QDeclIdent> = Set.empty

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
        Set.union
          deps
          (findDepsForTypeIdent
            env
            possibleDeps
            localsTree
            (interfaceTypeParamNames @ typeParamNames)
            ident
            (SyntaxNode.TypeAnn typeAnn))
    | None -> ()

  match fnSig.ReturnType with
  | Some returnType ->
    deps <-
      Set.union
        deps
        (findDepsForTypeIdent
          env
          possibleDeps
          localsTree
          (interfaceTypeParamNames @ typeParamNames)
          ident
          (SyntaxNode.TypeAnn returnType))
  | None -> ()

  deps

// TODO: we should be using this for more than PropName dependencies
let getPropNameDeps
  (env: Env)
  (topLevelDecls: Set<QDeclIdent>)
  (name: PropName)
  : Set<QDeclIdent> =
  match name with
  | Computed expr ->
    match expr.Kind with
    | ExprKind.Member(target, name, opt_chain) ->
      match target.Kind with
      | ExprKind.Identifier name ->
        let ident = (QualifiedIdent.FromString name)

        if Set.contains (QDeclIdent.Value ident) topLevelDecls then
          Set.singleton (QDeclIdent.Value ident)
        else
          match env.TryFindValue name with
          | Some(t, _) ->
            match (prune t).Kind with
            | TypeKind.TypeRef { Name = ident } ->
              let ident = QualifiedIdent.FromCommonQualifiedIdent ident

              if Set.contains (QDeclIdent.Type ident) topLevelDecls then
                Set.singleton (QDeclIdent.Type ident)
              else
                Set.empty
            | _ -> Set.empty // This should probably be an error
          | None -> Set.empty // This should probably be an error
      | _ -> Set.empty
    | _ -> Set.empty
  | _ -> Set.empty

let getDeclsFromModule (ast: Module) : list<Decl> =
  List.choose
    (fun (item: ModuleItem) ->
      match item with
      | ModuleItem.Stmt { Kind = StmtKind.Decl decl } -> Some decl
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
          |> Set.map (fun name ->
            QDeclIdent.Value({ Parts = namespaces @ [ name ] }))

        for ident in idents do
          nodes <- nodes.Add(ident, [ decl ])
      | FnDecl { Name = name } ->
        let key = { Parts = namespaces @ [ name ] }

        match nodes.TryFind(QDeclIdent.Value(key)) with
        | Some decls ->
          nodes <- nodes.Add(QDeclIdent.Value(key), decls @ [ decl ])
        | None -> nodes <- nodes.Add(QDeclIdent.Value(key), [ decl ])
      | ClassDecl { Name = name }
      | EnumDecl { Name = name } ->
        let key = { Parts = namespaces @ [ name ] }
        nodes <- nodes.Add(QDeclIdent.Value(key), [ decl ])
        nodes <- nodes.Add(QDeclIdent.Type(key), [ decl ])
      | TypeDecl { Name = name } ->
        let key = { Parts = namespaces @ [ name ] }
        nodes <- nodes.Add(QDeclIdent.Type(key), [ decl ])
      | InterfaceDecl { Name = name } ->
        let key = { Parts = namespaces @ [ name ] }

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
  (locals: Set<QDeclIdent>)
  (localsTree: QDeclTree)
  (nodes: Map<QDeclIdent, list<Decl>>)
  : Map<QDeclIdent, Set<QDeclIdent>> =
  let mutable edges: Map<QDeclIdent, Set<QDeclIdent>> = Map.empty

  let possibleDeps = nodes.Keys |> Set.ofSeq

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
            let deps = findDepsForValueIdent env locals localsTree ident init

            let typeDepsInExpr =
              findDepsForTypeIdent
                env
                possibleDeps
                localsTree
                []
                ident
                (SyntaxNode.Expr init)

            let typeDeps =
              match typeAnn with
              | Some typeAnn ->
                findDepsForTypeIdent
                  env
                  possibleDeps
                  localsTree
                  []
                  ident
                  (SyntaxNode.TypeAnn typeAnn)
              | None -> Set.empty

            Set.unionMany [ deps; typeDepsInExpr; typeDeps ]
          | None ->
            let deps =
              match typeAnn with
              | Some typeAnn ->
                findDepsForTypeIdent
                  env
                  possibleDeps
                  localsTree
                  []
                  ident
                  (SyntaxNode.TypeAnn typeAnn)
              | None -> Set.empty

            deps

        edges <- edges.Add(ident, deps)
      | FnDecl { Name = name
                 Sig = fnSig
                 Body = body } ->
        let deps = getDepsForFn env ident locals localsTree [] fnSig body

        match edges.TryFind(ident) with
        | Some existingDeps ->
          edges <- edges.Add(ident, Set.union existingDeps deps)
        | None -> edges <- edges.Add(ident, deps)
      | ClassDecl { Name = name
                    Class = { Elems = elems
                              TypeParams = typeParams
                              Extends = extends } } ->
        let typeParamNames =
          match typeParams with
          | None -> []
          | Some typeParams ->
            List.map
              (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
              typeParams

        let deps: Set<QDeclIdent> =
          match ident with
          | Type _ ->
            elems
            |> List.map (fun elem ->
              match elem with
              | ClassElem.Method { Sig = fnSig
                                   Name = name
                                   Static = false } ->
                let propNameDeps = getPropNameDeps env locals name

                let fnDeps =
                  getDepsForInterfaceFn
                    env
                    locals
                    localsTree
                    typeParamNames
                    ident
                    fnSig

                Set.union propNameDeps fnDeps
              | ClassElem.Getter { ReturnType = returnType
                                   Name = name
                                   Static = false } ->
                let propNameDeps = getPropNameDeps env locals name

                let fnDeps =
                  match returnType with
                  | Some returnType ->
                    findDepsForTypeIdent
                      env
                      possibleDeps
                      localsTree
                      typeParamNames
                      ident
                      (SyntaxNode.TypeAnn returnType)
                  | None -> Set.empty

                Set.union propNameDeps fnDeps
              | ClassElem.Setter { Param = { TypeAnn = typeAnn }
                                   Name = name
                                   Static = false } ->
                let propNameDeps = getPropNameDeps env locals name

                let fnDeps =
                  match typeAnn with
                  | Some typeAnn ->
                    findDepsForTypeIdent
                      env
                      possibleDeps
                      localsTree
                      typeParamNames
                      ident
                      (SyntaxNode.TypeAnn typeAnn)
                  | None -> Set.empty

                Set.union propNameDeps fnDeps
              | ClassElem.Property { TypeAnn = typeAnn
                                     Name = name
                                     Static = false } ->
                let propNameDeps = getPropNameDeps env locals name

                let typeAnnDeps =
                  findDepsForTypeIdent
                    env
                    possibleDeps
                    localsTree
                    typeParamNames
                    ident
                    (SyntaxNode.TypeAnn typeAnn)

                Set.union propNameDeps typeAnnDeps
              | _ -> Set.empty)
            |> Set.unionMany
          | Value qualifiedIdent ->
            elems
            |> List.map (fun elem ->
              match elem with
              | ClassElem.Constructor { Sig = fnSig } ->
                getDepsForInterfaceFn
                  env
                  locals
                  localsTree
                  typeParamNames
                  ident
                  fnSig
              | ClassElem.Method { Sig = fnSig
                                   Name = name
                                   Static = true } ->
                let propNameDeps = getPropNameDeps env locals name

                let fnDeps =
                  getDepsForInterfaceFn
                    env
                    locals
                    localsTree
                    typeParamNames
                    ident
                    fnSig

                Set.union propNameDeps fnDeps
              | ClassElem.Getter { ReturnType = returnType
                                   Name = name
                                   Static = true } ->
                let propNameDeps = getPropNameDeps env locals name

                let fnDeps =
                  match returnType with
                  | Some returnType ->
                    findDepsForTypeIdent
                      env
                      possibleDeps
                      localsTree
                      typeParamNames
                      ident
                      (SyntaxNode.TypeAnn returnType)
                  | None -> Set.empty

                Set.union propNameDeps fnDeps
              | ClassElem.Setter { Param = { TypeAnn = typeAnn }
                                   Name = name
                                   Static = true } ->
                let propNameDeps = getPropNameDeps env locals name

                let fnDeps =
                  match typeAnn with
                  | Some typeAnn ->
                    findDepsForTypeIdent
                      env
                      possibleDeps
                      localsTree
                      typeParamNames
                      ident
                      (SyntaxNode.TypeAnn typeAnn)
                  | None -> Set.empty

                Set.union propNameDeps fnDeps
              | ClassElem.Property { TypeAnn = typeAnn
                                     Name = name
                                     Static = true } ->
                let propNameDeps = getPropNameDeps env locals name

                let typeAnnDeps =
                  findDepsForTypeIdent
                    env
                    possibleDeps
                    localsTree
                    typeParamNames
                    ident
                    (SyntaxNode.TypeAnn typeAnn)

                Set.union propNameDeps typeAnnDeps
              | _ -> Set.empty)
            |> Set.unionMany

        // We need to infer the type and value of the class as the same time.
        let deps =
          match ident with
          | Type qualifiedIdent ->

            let deps =
              match extends with
              | Some typeRef ->
                Set.union
                  deps
                  (findDepsForTypeIdent
                    env
                    possibleDeps
                    localsTree
                    typeParamNames
                    ident
                    (SyntaxNode.TypeRef typeRef))
              | None -> deps

            Set.add (QDeclIdent.Value qualifiedIdent) deps
          | Value qualifiedIdent ->
            Set.add (QDeclIdent.Type qualifiedIdent) deps

        edges <- edges.Add(ident, deps)
      | TypeDecl { Name = name
                   TypeParams = typeParams
                   TypeAnn = typeAnn } ->
        let mutable deps: Set<QDeclIdent> = Set.empty

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
                Set.union
                  deps
                  (findDepsForTypeIdent
                    env
                    possibleDeps
                    localsTree
                    typeParamNames
                    ident
                    (SyntaxNode.TypeAnn c))
            | None -> ()

            match typeParam.Default with
            | Some d ->
              deps <-
                Set.union
                  deps
                  (findDepsForTypeIdent
                    env
                    possibleDeps
                    localsTree
                    typeParamNames
                    ident
                    (SyntaxNode.TypeAnn d))
            | None -> ()

        deps <-
          Set.union
            deps
            (findDepsForTypeIdent
              env
              possibleDeps
              localsTree
              typeParamNames
              ident
              (SyntaxNode.TypeAnn typeAnn))

        edges <- edges.Add(ident, deps)
      | InterfaceDecl { Name = name
                        TypeParams = typeParams
                        Extends = extends
                        Elems = elems } ->
        let typeParamNames =
          match typeParams with
          | None -> []
          | Some typeParams ->
            List.map
              (fun (tp: TypeParam) -> QualifiedIdent.FromString tp.Name)
              typeParams

        let mutable deps =
          elems
          |> List.map (fun elem ->
            match elem with
            | ObjTypeAnnElem.Callable fnSig ->
              getDepsForInterfaceFn
                env
                locals
                localsTree
                typeParamNames
                ident
                fnSig
            | ObjTypeAnnElem.Constructor fnSig ->
              getDepsForInterfaceFn
                env
                locals
                localsTree
                typeParamNames
                ident
                fnSig
            | ObjTypeAnnElem.Method { Type = fnSig; Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let fnDeps =
                getDepsForInterfaceFn
                  env
                  locals
                  localsTree
                  typeParamNames
                  ident
                  fnSig

              Set.union propNameDeps fnDeps
            | ObjTypeAnnElem.Getter { ReturnType = returnType; Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let fnDeps =
                findDepsForTypeIdent
                  env
                  possibleDeps
                  localsTree
                  typeParamNames
                  ident
                  (SyntaxNode.TypeAnn returnType)

              Set.union propNameDeps fnDeps
            | ObjTypeAnnElem.Setter { Param = { TypeAnn = typeAnn }
                                      Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let fnDeps =
                match typeAnn with
                | Some typeAnn ->
                  findDepsForTypeIdent
                    env
                    possibleDeps
                    localsTree
                    typeParamNames
                    ident
                    (SyntaxNode.TypeAnn typeAnn)
                | None -> Set.empty

              Set.union propNameDeps fnDeps
            | ObjTypeAnnElem.Property { TypeAnn = typeAnn; Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let typeAnnDeps =
                findDepsForTypeIdent
                  env
                  possibleDeps
                  localsTree
                  typeParamNames
                  ident
                  (SyntaxNode.TypeAnn typeAnn)

              Set.union propNameDeps typeAnnDeps
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

              Set.union
                (findDepsForTypeIdent
                  env
                  possibleDeps
                  localsTree
                  typeParamNames
                  ident
                  (SyntaxNode.TypeAnn typeParam.Constraint))
                (findDepsForTypeIdent
                  env
                  possibleDeps
                  localsTree
                  typeParamNames
                  ident
                  (SyntaxNode.TypeAnn typeAnn)))
          |> Set.unionMany

        match typeParams with
        | None -> ()
        | Some typeParams ->
          for typeParam in typeParams do
            match typeParam.Constraint with
            | Some c ->
              deps <-
                Set.union
                  deps
                  (findDepsForTypeIdent
                    env
                    possibleDeps
                    localsTree
                    typeParamNames
                    ident
                    (SyntaxNode.TypeAnn c))
            | None -> ()

            match typeParam.Default with
            | Some d ->
              deps <-
                Set.union
                  deps
                  (findDepsForTypeIdent
                    env
                    possibleDeps
                    localsTree
                    typeParamNames
                    ident
                    (SyntaxNode.TypeAnn d))
            | None -> ()

        match extends with
        | Some extends ->
          for typeRef in extends do
            deps <-
              Set.union
                deps
                (findDepsForTypeIdent
                  env
                  possibleDeps
                  localsTree
                  typeParamNames
                  ident
                  (SyntaxNode.TypeRef typeRef))
        | None -> ()

        match edges.TryFind(ident) with
        | Some existingDeps ->
          edges <- edges.Add(ident, Set.union existingDeps deps)
        | None -> edges <- edges.Add(ident, deps)
      | EnumDecl { Name = name } ->
        let deps =
          match ident with
          | Type qualifiedIdent ->
            // TODO: determine instance deps
            Set.singleton (QDeclIdent.Value qualifiedIdent)
          | Value qualifiedIdent ->
            Set.singleton (QDeclIdent.Type qualifiedIdent)

        edges <- edges.Add(ident, deps)
      | NamespaceDecl { Name = name; Body = body } ->
        // Namespaces are neither values or types but rather containers for
        // values and types. We don't need to add them to the edges map.
        ()

  edges

let buildGraph (env: Env) (decls: list<Decl>) : QGraph<Decl> =

  let mutable graph: QGraph<Decl> = { Nodes = Map.empty; Edges = Map.empty }

  let locals = findLocals decls

  // printfn "--- LOCALS ---"
  //
  // for local in locals do
  //   printfn $"{local}"

  let localTypeNames =
    locals
    |> Set.toList
    |> List.choose (fun qid ->
      match qid with
      | Type name -> Some name
      | _ -> None)

  let nodes = getNodes decls
  // We compute localsTree once here because it's expensive to compute
  let localsTree = localsToDeclTree locals
  let edges = getEdges env locals localsTree nodes

  // printfn "--- EDGES ---"
  //
  // for KeyValue(k, v) in edges do
  //   printfn $"{k} -> {v}"

  { Nodes = nodes; Edges = edges }
