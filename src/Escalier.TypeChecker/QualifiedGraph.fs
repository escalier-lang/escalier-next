module Escalier.TypeChecker.QualifiedGraph

open Escalier.Data.Type
open Escalier.TypeChecker.Env
open FsToolkit.ErrorHandling

open Escalier.Data.Common
open Escalier.Data.Syntax

open Error
open ExprVisitor

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

type SyntaxNode = Graph.SyntaxNode

// Find identifiers in an expression excluding function expressions.
let findIdentifiers (expr: Expr) : list<QDeclIdent> =
  let mutable ids: list<QDeclIdent> = []

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          // NOTE: we don't have to do any special handling for
          // ExprKind.Member because the property is stored as a
          // string instead of an identifier.
          | ExprKind.Identifier name ->
            let ident = QualifiedIdent.Ident name
            ids <- QDeclIdent.Value ident :: ids
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
            ids <- QDeclIdent.Value ident :: ids
            (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (true, state) }

  walkExpr visitor () expr

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
            idents <- QDeclIdent.Type(QualifiedIdent.Ident name) :: idents
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
              if
                (List.contains ident localTypeNames)
                && not (List.contains ident typeParams)
              then
                typeRefIdents <- QDeclIdent.Type ident :: typeRefIdents

              []
            | TypeAnnKind.Typeof ident ->
              let baseName = Graph.getBaseName ident
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
                  (fun (tp: TypeParam) -> QualifiedIdent.Ident tp.Name)
                  funcTypeParams
            | ObjTypeAnnElem.Constructor funcSig ->
              match funcSig.TypeParams with
              | None -> []
              | Some funcTypeParams ->
                List.map
                  (fun (tp: TypeParam) -> QualifiedIdent.Ident tp.Name)
                  funcTypeParams
            | ObjTypeAnnElem.Method { Type = t } ->
              match t.TypeParams with
              | None -> []
              | Some funcTypeParams ->
                List.map
                  (fun (tp: TypeParam) -> QualifiedIdent.Ident tp.Name)
                  funcTypeParams
            | ObjTypeAnnElem.Getter _ -> []
            | ObjTypeAnnElem.Setter _ -> []
            | ObjTypeAnnElem.Property _ -> []
            | ObjTypeAnnElem.Mapped { TypeParam = typeParam } ->
              [ QualifiedIdent.Ident typeParam.Name ]

          (true, typeParams @ newTypeParams) }

  match syntaxNode with
  | SyntaxNode.TypeAnn typeAnn -> walkTypeAnn visitor typeParams typeAnn
  | SyntaxNode.Expr expr -> walkExpr visitor typeParams expr

  List.rev typeRefIdents

let findLocals (decls: list<Decl>) : list<QDeclIdent> =
  let mutable locals: list<QDeclIdent> = []

  for decl in decls do
    match decl.Kind with
    | VarDecl { Pattern = pattern } ->
      let bindingNames =
        Helpers.findBindingNames pattern
        |> List.map QualifiedIdent.Ident
        |> List.map QDeclIdent.Value

      locals <- locals @ bindingNames
    | FnDecl { Name = name } ->
      locals <- locals @ [ QDeclIdent.Value(QualifiedIdent.Ident name) ]
    | ClassDecl { Name = name } ->
      locals <- locals @ [ QDeclIdent.Type(QualifiedIdent.Ident name) ]
    | TypeDecl { Name = name } ->
      locals <- locals @ [ QDeclIdent.Type(QualifiedIdent.Ident name) ]
    | InterfaceDecl { Name = name } ->
      locals <- locals @ [ QDeclIdent.Type(QualifiedIdent.Ident name) ]
    | EnumDecl { Name = name } ->
      locals <- locals @ [ QDeclIdent.Value(QualifiedIdent.Ident name) ]
      locals <- locals @ [ QDeclIdent.Type(QualifiedIdent.Ident name) ]
    | NamespaceDecl { Name = name } ->
      locals <- locals @ [ QDeclIdent.Value(QualifiedIdent.Ident name) ]
      locals <- locals @ [ QDeclIdent.Type(QualifiedIdent.Ident name) ]

  locals

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
      Helpers.findBindingNames p.Pattern |> List.map QualifiedIdent.Ident

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
            let ident = QualifiedIdent.Ident name

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
      List.map (fun (tp: TypeParam) -> QualifiedIdent.Ident tp.Name) typeParams

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
          (fun (tp: TypeParam) -> QualifiedIdent.Ident tp.Name)
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
      List.map (fun (tp: TypeParam) -> QualifiedIdent.Ident tp.Name) typeParams

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
        let ident = (QualifiedIdent.Ident name)

        if List.contains (QDeclIdent.Value ident) topLevelDecls then
          [ QDeclIdent.Value ident ]
        else
          match env.TryFindValue name with
          | Some(t, _) ->
            match t.Kind with
            | TypeKind.TypeRef { Name = ident } ->

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

let buildGraph (env: Env) (m: Module) : QGraph =

  let mutable graph = { Nodes = Map.empty; Edges = Map.empty }

  let decls = getDeclsFromModule m
  // TODO: fork findLocals to instead return a list of QualifiedIdents
  // It should also handle namespaces appropriately
  let locals = findLocals decls

  let localTypeNames =
    List.choose
      (fun qid ->
        match qid with
        | Type name -> Some name
        | _ -> None)
      locals

  for item in m.Items do
    match item with
    | ModuleItem.Import _ -> ()
    | ModuleItem.Decl decl ->
      let nsIdent = None

      match decl.Kind with
      | VarDecl { Pattern = pattern
                  Init = init
                  TypeAnn = typeAnn } ->
        let idents =
          Helpers.findBindingNames pattern
          |> List.map (fun name ->
            match nsIdent with
            | Some left -> QDeclIdent.Value(QualifiedIdent.Member(left, name))
            | None -> QDeclIdent.Value(QualifiedIdent.Ident name))

        // TODO: determine deps
        let deps =
          match init with
          | Some init ->
            let mutable deps = findIdentifiers init

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

        for ident in idents do
          graph <- graph.Add(ident, decl, deps)

      | FnDecl { Name = name
                 Sig = fnSig
                 Body = body } ->
        let ident =
          match nsIdent with
          | Some left -> QDeclIdent.Value(QualifiedIdent.Member(left, name))
          | None -> QDeclIdent.Value(QualifiedIdent.Ident name)

        let deps = getDepsForFn env locals [] fnSig body
        graph <- graph.Add(ident, decl, deps)
      | ClassDecl { Name = name } ->
        let ident =
          match nsIdent with
          | Some left -> QualifiedIdent.Member(left, name)
          | None -> QualifiedIdent.Ident name

        // TODO: determine static deps
        let staticDeps = []
        graph <- graph.Add((Value ident), decl, staticDeps)

        // TODO: determine instance deps
        let instanceDeps = []
        graph <- graph.Add((Type ident), decl, instanceDeps)
      | TypeDecl { Name = name
                   TypeParams = typeParams
                   TypeAnn = typeAnn } ->
        let ident =
          match nsIdent with
          | Some left -> QualifiedIdent.Member(left, name)
          | None -> QualifiedIdent.Ident name

        let mutable deps: list<QDeclIdent> = []

        let typeParamNames =
          match typeParams with
          | None -> []
          | Some typeParams ->
            List.map
              (fun (tp: TypeParam) -> QualifiedIdent.Ident tp.Name)
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

        graph <- graph.Add((Type ident), decl, deps)
      | InterfaceDecl { Name = name
                        TypeParams = typeParams
                        Elems = elems } ->
        let ident =
          match nsIdent with
          | Some left -> QualifiedIdent.Member(left, name)
          | None -> QualifiedIdent.Ident name

        let interfaceName = ident

        let interfaceTypeParamNames =
          match typeParams with
          | None -> []
          | Some typeParams ->
            List.map
              (fun (tp: TypeParam) -> QualifiedIdent.Ident tp.Name)
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

        match graph.Edges.TryFind(QDeclIdent.Type ident) with
        | Some existingDeps ->
          graph <- graph.Add(QDeclIdent.Type ident, decl, existingDeps @ deps)
        | None -> graph <- graph.Add(QDeclIdent.Type ident, decl, deps)
      | EnumDecl { Name = name } ->
        let ident =
          match nsIdent with
          | Some left -> QualifiedIdent.Member(left, name)
          | None -> QualifiedIdent.Ident name

        graph <-
          { graph with
              Nodes = Map.add (Value ident) [ decl ] graph.Nodes }

        graph <-
          { graph with
              Nodes = Map.add (Type ident) [ decl ] graph.Nodes }
      | NamespaceDecl { Name = name; Body = body } ->
        // let nsIdent =
        //   match nsIdent with
        //   | Some left -> QualifiedIdent.Member(left, name)
        //   | None -> QualifiedIdent.Ident name
        // for decl in body do
        //   let subGraph = buildGraph (Some nsIdent) decl
        //
        //   for KeyValue(ident, decl) in subGraph.Nodes do
        //     graph <-
        //       { graph with
        //           Nodes = Map.add ident decl graph.Nodes }
        //
        //   for KeyValue(ident, deps) in subGraph.Edges do
        //     graph <-
        //       { graph with
        //           Edges = Map.add ident deps graph.Edges }
        failwith "TODO: buildGraph - NamespaceDecl"

  graph

// TODO: split this into separate functions
// - one to create the namespace if it doesn't exist and update `env.Namespace` appropriately
// - one to return the existing or newly created namespace
// - one function to add a valud to a given namespace

let rec getOrCreateNamespace
  (nsIdent: QualifiedIdent)
  (ns: Namespace)
  : Result<Namespace, TypeError> =
  result {
    match nsIdent with
    | QualifiedIdent.Ident name ->
      let childNS =
        match Map.tryFind name ns.Namespaces with
        | None ->
          let ns: Namespace =
            { Name = name
              Namespaces = Map.empty
              Schemes = Map.empty
              Values = Map.empty }

          ns
        | Some ns -> ns

      return
        { ns with
            Namespaces = Map.add childNS.Name childNS ns.Namespaces }
    | QualifiedIdent.Member(left, right) ->
      let! parentNS = getOrCreateNamespace left ns

      let childNS =
        match Map.tryFind right parentNS.Namespaces with
        | None ->
          let ns: Namespace =
            { Name = right
              Namespaces = Map.empty
              Schemes = Map.empty
              Values = Map.empty }

          ns
        | Some ns -> ns

      let parentNS =
        { parentNS with
            Namespaces = Map.add right childNS parentNS.Namespaces }

      return
        { ns with
            Namespaces = Map.add parentNS.Name parentNS ns.Namespaces }
  }


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
  (decls: list<Decl>)
  : Result<unit, TypeError> =

  result {
    let mutable newEnv = env

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
          newEnv <- newEnv.AddValue name binding
      | FnDecl({ Declare = false
                 Sig = fnSig
                 Name = name } as decl) ->
        let! f = Infer.inferFuncSig ctx newEnv fnSig

        let t =
          { Kind = TypeKind.Function f
            Provenance = None }

        newEnv <- newEnv.AddValue name (t, false)

        decl.Inferred <- Some t
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

        let! f = Infer.inferFuncSig ctx newEnv fnSig

        if fnSig.Throws.IsNone then
          f.Throws <-
            { Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }

        let t =
          { Kind = TypeKind.Function f
            Provenance = None }

        newEnv <- newEnv.AddValue name (t, false)

        decl.Inferred <- Some t
      | ClassDecl({ Name = name } as decl) ->
        // TODO: treat ClassDecl similar to object types where we create a
        // structural placeholder type instead of an opaque type variable.
        // We should do this for both instance members and statics.
        let instance: Scheme =
          { Type = ctx.FreshTypeVar None
            TypeParams = None // TODO: handle type params
            IsTypeParam = false }

        newEnv <- newEnv.AddScheme name instance

        let statics: Type = ctx.FreshTypeVar None

        newEnv <- newEnv.AddValue name (statics, false)

        decl.Inferred <-
          Some
            { Instance = instance
              Statics = statics }
      | EnumDecl _ ->
        return!
          Error(
            TypeError.NotImplemented "TODO: inferDeclPlaceholders - EnumDecl"
          )
      | TypeDecl typeDecl ->
        // TODO: replace placeholders, with a reference the actual definition
        // once we've inferred the definition
        let! placeholder =
          Infer.inferTypeDeclPlaceholderScheme ctx env typeDecl.TypeParams

        // TODO: update AddScheme to return a Result<Env, TypeError> and
        // return an error if the name already exists since we can't redefine
        // types.
        typeDecl.Inferred <- Some placeholder
        newEnv <- newEnv.AddScheme typeDecl.Name placeholder
      | InterfaceDecl({ Name = name; TypeParams = typeParams } as decl) ->
        // Instead of looking things up in the environment, we need some way to
        // find the existing type on other declarations.
        let! placeholder =
          match newEnv.TryFindScheme name with
          | Some scheme -> Result.Ok scheme
          | None -> Infer.inferTypeDeclPlaceholderScheme ctx env typeParams

        decl.Inferred <- Some placeholder
        newEnv <- newEnv.AddScheme name placeholder
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

    return ()
  }

let inferDeclDefinitions
  (ctx: Ctx)
  (env: Env)
  (decls: list<Decl>)
  : Result<unit, TypeError> =

  result {
    for decl in decls do
      match decl.Kind with
      | VarDecl varDecl ->
        let placeholderTypes = getAllBindingPatterns varDecl.Pattern

        // NOTE: We explicitly don't generalize here because we want other
        // declarations to be able to unify with any free type variables
        // from this declaration.  We generalize things in `inferModule` and
        // `inferTreeRec`.
        let! newBindings, newSchemes = Infer.inferVarDecl ctx env varDecl

        let inferredTypes = getAllBindingPatterns varDecl.Pattern

        for KeyValue(name, inferredType) in inferredTypes do
          let placeholderType =
            match Map.tryFind name placeholderTypes with
            | Some t -> t
            | None -> failwith "Missing placeholder type"

          do! Unify.unify ctx env None placeholderType inferredType

      // TODO: handle any schemes that are generated in a similar way to how
      // we're handling inferred value types.
      // Schemes can be generated for things like class expressions, e.g.
      // let Foo = class { ... }
      // for KeyValue(name, scheme) in newSchemes do
      //   newEnv <- newEnv.AddScheme name scheme
      | FnDecl({ Declare = false
                 Name = name
                 Sig = fnSig
                 Body = Some body } as decl) ->
        // NOTE: We explicitly don't generalize here because we want other
        // declarations to be able to unify with any free type variables
        // from this declaration.  We generalize things in `inferModule` and
        // `inferTreeRec`.
        let! f = Infer.inferFunction ctx env fnSig body

        let t =
          { Kind = TypeKind.Function f
            Provenance = None }

        decl.Inferred <- Some t
      | FnDecl { Declare = true } ->
        // Nothing to do since ambient function declarations don't have
        // function bodies.
        ()
      | FnDecl fnDecl ->
        return! Error(TypeError.SemanticError "Invalid function declaration")
      | ClassDecl { Declare = declare
                    Name = name
                    Class = cls
                    Inferred = placeholders } ->
        let! t, scheme = Infer.inferClass ctx env cls declare

        // TODO: update `Statics` and `Instance` on `placeholders`

        ()
      | EnumDecl _ ->
        return!
          Error(
            TypeError.NotImplemented "TODO: inferDeclDefinitions - EnumDecl"
          )
      | TypeDecl { Name = name
                   TypeAnn = typeAnn
                   Inferred = placeholder } ->
        // TODO: when computing the decl graph, include self-recursive types in
        // the deps set so that we don't have to special case this here.
        // Handles self-recursive types
        let newEnv = env.AddScheme name placeholder.Value
        let getType = fun env -> Infer.inferTypeAnn ctx env typeAnn

        let! scheme =
          Infer.inferTypeDeclDefn ctx newEnv placeholder.Value getType

        // Replace the placeholder's type with the actual type.
        // NOTE: This is a bit hacky and we may want to change this later to use
        // `foldType` to replace any uses of the placeholder with the actual type.
        // Required for the following test cases:
        // - InferRecursiveGenericObjectTypeInModule
        // - InferNamespaceInModule
        placeholder.Value.Type <- scheme.Type
      | InterfaceDecl { Name = name
                        TypeParams = typeParams
                        Elems = elems
                        Inferred = placeholder } ->

        // TODO: when computing the decl graph, include self-recursive types in
        // the deps set so that we don't have to special case this here.
        // Handles self-recursive types
        let newEnv = env.AddScheme name placeholder.Value

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
          Infer.inferTypeDeclDefn ctx newEnv placeholder.Value getType

        match placeholder.Value.Type.Kind, newScheme.Type.Kind with
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
          placeholder.Value.Type <- t
        | _ ->
          // Replace the placeholder's type with the actual type.
          // NOTE: This is a bit hacky and we may want to change this later to use
          // `foldType` to replace any uses of the placeholder with the actual type.
          placeholder.Value.Type <- newScheme.Type
      | NamespaceDecl { Name = name; Body = decls } ->
        return!
          Error(
            TypeError.NotImplemented
              "TODO: inferDeclDefinitions - NamespaceDecl"
          )

    return ()
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

type InferredLocals =
  { Values: Map<QualifiedIdent, Binding>
    Type: Map<QualifiedIdent, Scheme> }

  member this.AddValue (ident: QualifiedIdent) (binding: Binding) =
    { this with
        Values = Map.add ident binding this.Values }

  member this.AddScheme (ident: QualifiedIdent) (scheme: Scheme) =
    { this with
        Type = Map.add ident scheme this.Type }

let copyInferredLocalsToEnv (inferredLocals: InferredLocals) (env: Env) : Env =
  let mutable newEnv = env

  for KeyValue(ident, binding) in inferredLocals.Values do
    match ident with
    | QualifiedIdent.Ident name -> newEnv <- newEnv.AddValue name binding
    | QualifiedIdent.Member(left, right) ->
      failwith "TODO: inferTreeRec - Member"

  for KeyValue(ident, scheme) in inferredLocals.Type do
    match ident with
    | QualifiedIdent.Ident name -> newEnv <- newEnv.AddScheme name scheme
    | QualifiedIdent.Member(left, right) ->
      failwith "TODO: inferTreeRec - Member"

  newEnv

let updateInferredLocals
  (inferredLocals: InferredLocals)
  (decls: list<Decl>)
  : InferredLocals =

  let mutable inferredLocals = inferredLocals

  for decl in decls do
    match decl.Kind with
    | VarDecl varDecl ->
      // TODO: store the binding instead of just the types
      let valueTypes = getAllBindingPatterns varDecl.Pattern

      for KeyValue(name, t) in valueTypes do
        let ident = QualifiedIdent.Ident name
        inferredLocals <- inferredLocals.AddValue ident (t, false)
    | FnDecl fnDecl ->
      let ident = QualifiedIdent.Ident fnDecl.Name

      inferredLocals <-
        inferredLocals.AddValue ident (fnDecl.Inferred.Value, false)
    | ClassDecl classDecl ->
      let ident = QualifiedIdent.Ident classDecl.Name

      inferredLocals <-
        inferredLocals.AddValue ident (classDecl.Inferred.Value.Statics, false)

      inferredLocals <-
        inferredLocals.AddScheme ident classDecl.Inferred.Value.Instance
    | TypeDecl typeDecl ->
      let ident = QualifiedIdent.Ident typeDecl.Name

      inferredLocals <- inferredLocals.AddScheme ident typeDecl.Inferred.Value
    | InterfaceDecl interfaceDecl ->
      let ident = QualifiedIdent.Ident interfaceDecl.Name

      inferredLocals <-
        inferredLocals.AddScheme ident interfaceDecl.Inferred.Value
    | EnumDecl enumDecl -> failwith "TODO: inferGraph - EnumDecl"
    | NamespaceDecl namespaceDecl -> failwith "TODO: inferGraph - NamespaceDecl"

  inferredLocals

let rec inferTreeRec
  (ctx: Ctx)
  (env: Env)
  (root: Set<QDeclIdent>)
  (graph: QGraph)
  (tree: QDeclTree)
  (inferredLocals: InferredLocals)
  : Result<InferredLocals, TypeError> =

  result {
    let mutable inferredLocals = inferredLocals

    // Infer dependencies
    match tree.Edges.TryFind root with
    | Some deps ->
      for dep in deps do
        let! newInferredLocals =
          inferTreeRec ctx env dep graph tree inferredLocals

        inferredLocals <- newInferredLocals
    | None -> ()

    // Update environment to include dependencies
    let newEnv = copyInferredLocalsToEnv inferredLocals env

    match List.ofSeq root with
    | [] ->
      return!
        Error(
          TypeError.SemanticError "inferTreeRec - rootSet should not be empty"
        )
    | names ->
      let decls =
        names |> List.map (fun name -> graph.Nodes[name]) |> List.concat

      do! inferDeclPlaceholders ctx newEnv decls
      // TODO: add decls to the environment after inferring placeholders so
      // they're available when inferring mutually recursive function definitions
      do! inferDeclDefinitions ctx newEnv decls

      inferredLocals <- updateInferredLocals inferredLocals decls

      let mutable bindings: Map<string, Binding> = Map.empty

      for KeyValue(key, binding) in inferredLocals.Values do
        let t, isMut = binding
        let t = Helpers.generalizeFunctionsInType t
        inferredLocals <- inferredLocals.AddValue key (t, isMut)

      return inferredLocals
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

    let mutable inferredLocals: InferredLocals =
      { Values = Map.empty; Type = Map.empty }

    // Infer entry points and all their dependencies
    for set in entryPoints do
      try
        let! newInferredLocals =
          inferTreeRec ctx env set graph tree inferredLocals

        inferredLocals <- newInferredLocals
      with e ->
        printfn $"Error: {e}"
        return! Error(TypeError.SemanticError(e.ToString()))

    // NOTE: We could also return inferredLocals instead of adding them to the
    // environment.  The only time we actually need to add things to the
    // environment is when we're dealing with globals.

    // Update environment with all new values and types
    return copyInferredLocalsToEnv inferredLocals env
  }
