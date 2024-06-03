module Escalier.TypeChecker.BuildGraph

open Escalier.Data.Type
open Escalier.TypeChecker.Env

open Escalier.Data.Syntax

open ExprVisitor
open QualifiedGraph

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

let postProcessValueDeps
  (locals: list<QDeclIdent>)
  (ident: QDeclIdent)
  (deps: list<QDeclIdent>)
  : list<QDeclIdent> =

  let identNamespaces =
    match ident with
    | Type { Parts = namespaces } -> namespaces
    | Value { Parts = namespaces } -> namespaces

  let identNamespaces = List.take (identNamespaces.Length - 1) identNamespaces

  let deps =
    deps
    |> List.map (fun dep ->
      if List.contains dep locals then
        let mutable candidateNamespaces = []
        let mutable actualID = dep

        // Handles shadowing if identifiers in nested namespaces.
        for ns in identNamespaces do
          candidateNamespaces <- candidateNamespaces @ [ ns ]

          // TODO: Handle the situation where an identifier is already partially
          // qualified
          let candidateID =
            match dep with
            | Type qid -> Type { Parts = candidateNamespaces @ qid.Parts }
            | Value qid -> Value { Parts = candidateNamespaces @ qid.Parts }

          if List.contains candidateID locals then
            actualID <- candidateID

        actualID
      else
        let depNamespaces =
          match dep with
          | Type { Parts = namespaces } -> namespaces
          | Value { Parts = namespaces } -> namespaces

        let depNamespaces = List.take (depNamespaces.Length - 1) depNamespaces

        let minLength = min depNamespaces.Length identNamespaces.Length

        let commonPrefix =
          List.zip
            (List.take minLength identNamespaces)
            (List.take minLength depNamespaces)
          |> List.takeWhile (fun (ns1, ns2) -> ns1 = ns2)
          |> List.map (fun (ns, _) -> ns)

        // remove commonPrefix from namespaces
        let namespaces = List.skip commonPrefix.Length identNamespaces

        match dep with
        | Type { Parts = parts } -> Type { Parts = namespaces @ parts }
        | Value { Parts = parts } -> Value { Parts = namespaces @ parts })

  deps

let findDepsForValueIdent
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

  postProcessValueDeps locals ident (List.rev ids)

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
  (possibleDeps: list<QDeclIdent>)
  (typeParams: list<QualifiedIdent>)
  (ident: QDeclIdent)
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
                (List.contains (QDeclIdent.Type ident) possibleDeps)
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

  let namespaces =
    match ident with
    | Type { Parts = namespaces } -> namespaces
    | Value { Parts = namespaces } -> namespaces

  // remove the last item from the list
  let namespaces = List.take (namespaces.Length - 1) namespaces

  let typeRefIdents =
    typeRefIdents
    |> List.map (fun id ->
      if List.contains id possibleDeps then
        let mutable candidateNamespaces = []
        let mutable actualID = id

        // Handles shadowing if identifiers in nested namespaces.
        for ns in namespaces do
          candidateNamespaces <- candidateNamespaces @ [ ns ]

          // TODO: Handle the situation where an identifier is already partially
          // qualified
          let candidateID =
            match id with
            | Type qid -> Type { Parts = candidateNamespaces @ qid.Parts }
            | Value qid -> Value { Parts = candidateNamespaces @ qid.Parts }

          if List.contains candidateID possibleDeps then
            actualID <- candidateID

        actualID
      else
        // TODO: handle situations where we only need to prepend some of the
        // namespaces in `namespaces` instead of all of them
        match id with
        | Type qid -> Type { Parts = namespaces @ qid.Parts }
        | Value qid -> Value { Parts = namespaces @ qid.Parts })

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
            QDeclIdent.Value({ Parts = namespaces @ [ name ] }))

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
  (ident: QDeclIdent)
  (possibleDeps: list<QDeclIdent>)
  (excludedTypeNames: list<QualifiedIdent>)
  (fnSig: FuncSig)
  (body: option<BlockOrExpr>)
  : list<QDeclIdent> =
  let mutable typeDeps = []

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
              possibleDeps
              (excludedTypeNames @ typeParamNames)
              ident
              (SyntaxNode.TypeAnn c)
      | None -> ()

      match tp.Default with
      | Some d ->
        typeDeps <-
          typeDeps
          @ findTypeRefIdents
              env
              possibleDeps
              (excludedTypeNames @ typeParamNames)
              ident
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
            possibleDeps
            (excludedTypeNames @ typeParamNames)
            ident
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
          possibleDeps
          (excludedTypeNames @ typeParamNames)
          ident
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
  (ident: QDeclIdent)
  (fnSig: FuncSig)
  : list<QDeclIdent> =

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
            possibleDeps
            (interfaceTypeParamNames @ typeParamNames)
            ident
            (SyntaxNode.TypeAnn typeAnn)
    | None -> ()

  match fnSig.ReturnType with
  | Some returnType ->
    deps <-
      deps
      @ findTypeRefIdents
          env
          possibleDeps
          (interfaceTypeParamNames @ typeParamNames)
          ident
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
            QDeclIdent.Value({ Parts = namespaces @ [ name ] }))

        for ident in idents do
          nodes <- nodes.Add(ident, [ decl ])
      | FnDecl { Name = name } ->
        let key = { Parts = namespaces @ [ name ] }
        // TODO: support function overloading
        nodes <- nodes.Add(QDeclIdent.Value(key), [ decl ])
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
  (locals: list<QDeclIdent>)
  (nodes: Map<QDeclIdent, list<Decl>>)
  : Map<QDeclIdent, list<QDeclIdent>> =
  let mutable edges: Map<QDeclIdent, list<QDeclIdent>> = Map.empty

  let possibleDeps = nodes.Keys |> List.ofSeq

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
            let mutable deps = findDepsForValueIdent locals ident init
            printfn $"locals for {ident}: {locals}"
            printfn $"deps for {ident}: {deps}"

            // TODO: reimplement findTypeRefIdents to only look at localTypeNames
            let typeDepsInExpr =
              findTypeRefIdents env possibleDeps [] ident (SyntaxNode.Expr init)

            let typeDeps =
              match typeAnn with
              | Some typeAnn ->
                findTypeRefIdents
                  env
                  possibleDeps
                  []
                  ident
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
                  possibleDeps
                  []
                  ident
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
        let deps = getDepsForFn env ident locals [] fnSig body
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
                    possibleDeps
                    typeParamNames
                    ident
                    (SyntaxNode.TypeAnn c)
            | None -> ()

            match typeParam.Default with
            | Some d ->
              deps <-
                deps
                @ findTypeRefIdents
                    env
                    possibleDeps
                    typeParamNames
                    ident
                    (SyntaxNode.TypeAnn d)
            | None -> ()

        deps <-
          deps
          @ findTypeRefIdents
              env
              possibleDeps
              typeParamNames
              ident
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
              getDepsForInterfaceFn
                env
                locals
                interfaceTypeParamNames
                ident
                fnSig
            | ObjTypeAnnElem.Constructor fnSig ->
              getDepsForInterfaceFn
                env
                locals
                interfaceTypeParamNames
                ident
                fnSig
            | ObjTypeAnnElem.Method { Type = fnSig; Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let fnDeps =
                getDepsForInterfaceFn
                  env
                  locals
                  interfaceTypeParamNames
                  ident
                  fnSig

              propNameDeps @ fnDeps
            | ObjTypeAnnElem.Getter { ReturnType = returnType; Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let fnDeps =
                match returnType with
                | Some returnType ->
                  findTypeRefIdents
                    env
                    possibleDeps
                    interfaceTypeParamNames
                    ident
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
                    possibleDeps
                    interfaceTypeParamNames
                    ident
                    (SyntaxNode.TypeAnn typeAnn)
                | None -> []

              propNameDeps @ fnDeps
            | ObjTypeAnnElem.Property { TypeAnn = typeAnn; Name = name } ->
              let propNameDeps = getPropNameDeps env locals name

              let typeAnnDeps =
                findTypeRefIdents
                  env
                  possibleDeps
                  interfaceTypeParamNames
                  ident
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
                possibleDeps
                interfaceTypeParamNames
                ident
                (SyntaxNode.TypeAnn typeParam.Constraint)
              @ findTypeRefIdents
                  env
                  possibleDeps
                  interfaceTypeParamNames
                  ident
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

let buildGraph (env: Env) (m: Module) : QGraph<Decl> =

  let mutable graph: QGraph<Decl> = { Nodes = Map.empty; Edges = Map.empty }

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
