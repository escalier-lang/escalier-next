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

  // TODO: file this out some more
  type SyntaxNode =
    | TypeAnn of TypeAnn
    | Expr of Expr

  // TODO: Find deps in computed property names
  let findTypeRefIdents
    (env: Env)
    (localTypeNames: list<string>) // top-level and namespace decls
    (typeParams: list<string>)
    (syntaxNode: SyntaxNode)
    : list<DeclIdent> =
    let mutable typeRefIdents: list<DeclIdent> = []

    let visitor: SyntaxVisitor<list<string>> =
      { ExprVisitor.VisitExpr = fun (_, state) -> (true, state)
        ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
        ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnn =
          fun (typeAnn, typeParams) ->
            let newTypeParams =
              match typeAnn.Kind with
              | TypeAnnKind.TypeRef { Ident = ident } ->
                let baseName = getBaseName ident

                if
                  (List.contains baseName localTypeNames)
                  && not (List.contains baseName typeParams)
                  && not (Map.containsKey baseName env.Namespace.Schemes)
                  && not (Map.containsKey baseName env.Namespace.Namespaces)
                then
                  typeRefIdents <- DeclIdent.Type baseName :: typeRefIdents

                []
              | TypeAnnKind.Typeof ident ->
                let baseName = getBaseName ident
                typeRefIdents <- DeclIdent.Value baseName :: typeRefIdents

                []
              | TypeAnnKind.Condition { Extends = extends } ->
                findInferTypeAnns extends
                |> List.choose (fun ident ->
                  match ident with
                  | DeclIdent.Type name -> Some name
                  | _ -> None)
              | _ -> []

            (true, newTypeParams @ typeParams)
        ExprVisitor.VisitTypeAnnObjElem =
          fun (elem, typeParams) ->
            let newTypeParams =
              match elem with
              | ObjTypeAnnElem.Callable funcSig ->
                match funcSig.TypeParams with
                | None -> []
                | Some funcTypeParams ->
                  List.map
                    (fun (tp: Syntax.TypeParam) -> tp.Name)
                    funcTypeParams
              | ObjTypeAnnElem.Constructor funcSig ->
                match funcSig.TypeParams with
                | None -> []
                | Some funcTypeParams ->
                  List.map
                    (fun (tp: Syntax.TypeParam) -> tp.Name)
                    funcTypeParams
              | ObjTypeAnnElem.Method { Type = t } ->
                match t.TypeParams with
                | None -> []
                | Some funcTypeParams ->
                  List.map
                    (fun (tp: Syntax.TypeParam) -> tp.Name)
                    funcTypeParams
              | ObjTypeAnnElem.Getter _ -> []
              | ObjTypeAnnElem.Setter _ -> []
              | ObjTypeAnnElem.Property _ -> []
              | ObjTypeAnnElem.Mapped { TypeParam = typeParam } ->
                [ typeParam.Name ]

            (true, typeParams @ newTypeParams) }

    match syntaxNode with
    | TypeAnn typeAnn -> walkTypeAnn visitor typeParams typeAnn
    | Expr expr -> walkExpr visitor typeParams expr

    List.rev typeRefIdents

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
      localNames <- localNames @ Helpers.findBindingNames p.Pattern

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
          Helpers.findBindingNames pattern |> List.map DeclIdent.Value

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

  let getDepsForFn
    (env: Env)
    (possibleDeps: list<DeclIdent>)
    (excludedTypeNames: list<string>)
    (fnSig: FuncSig)
    (body: option<BlockOrExpr>)
    : DeclIdent list =
    let mutable typeDeps = []

    let possibleTypeNames =
      List.choose
        (fun id ->
          match id with
          | DeclIdent.Type name -> Some name
          | _ -> None)
        possibleDeps

    let typeParamNames =
      match fnSig.TypeParams with
      | None -> []
      | Some typeParams ->
        List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

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
      let typeParamNames =
        match fnSig.TypeParams with
        | None -> []
        | Some typeParams ->
          List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

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
        let f: Syntax.Function =
          { Sig = fnSig
            Body = body
            Captures = None
            InferredType = None }

        findCaptures possibleDeps f @ typeDeps

    deps

  let getDepsForInterfaceFn
    (env: Env)
    (possibleDeps: list<DeclIdent>)
    (interfaceTypeParamNames: list<string>)
    (fnSig: FuncSig)
    : DeclIdent list =

    let localTypeNames =
      List.choose
        (fun id ->
          match id with
          | DeclIdent.Type name -> Some name
          | _ -> None)
        possibleDeps

    let mutable deps = []

    let typeParamNames =
      match fnSig.TypeParams with
      | None -> []
      | Some typeParams ->
        List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

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
    (topLevelDecls: list<DeclIdent>)
    (name: Syntax.PropName)
    : list<DeclIdent> =
    match name with
    | Computed expr ->
      match expr.Kind with
      | ExprKind.Member(target, name, opt_chain) ->
        match target.Kind with
        | ExprKind.Identifier name ->
          if List.contains (DeclIdent.Value name) topLevelDecls then
            [ DeclIdent.Value name ]
          else
            match env.TryFindValue name with
            | Some(t, _) ->
              match t.Kind with
              | TypeKind.TypeRef { Name = name } ->
                let baseName = getBaseName name

                if List.contains (DeclIdent.Type baseName) topLevelDecls then
                  [ DeclIdent.Type baseName ]
                else
                  []
              | _ -> [] // This should probably be an error
            | None -> [] // This should probably be an error
        | _ -> []
      | _ -> []
    | _ -> []

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
      // These are top-level decls in the module and top-level
      // decls inside any namespaces we're inside of.
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
            Helpers.findBindingNames pattern |> List.map DeclIdent.Value

          match declare, init with
          | false, Some init ->
            let mutable deps = findIdentifiers init

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
              | Some typeAnn ->
                findTypeRefIdents
                  env
                  localTypeNames
                  []
                  (SyntaxNode.TypeAnn typeAnn)
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

          let deps =
            findTypeRefIdents
              env
              localTypeNames
              typeParamNames
              (SyntaxNode.TypeAnn typeAnn)

          graph <- graph.Add(DeclIdent.Type name, decl, deps)
        | FnDecl { Declare = _
                   Name = name
                   Sig = fnSig
                   Body = body } ->
          let deps = getDepsForFn env locals [] fnSig body
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

          let deps =
            elems
            |> List.collect (fun elem ->
              match elem with
              | ClassElem.Property { TypeAnn = typeAnn } ->
                // TODO: if it's a static property then we need to add a dep for
                // DeclIdent.Value name
                findTypeRefIdents
                  env
                  localTypeNames
                  classTypeParamNames
                  (SyntaxNode.TypeAnn typeAnn)
              | ClassElem.Constructor { Sig = fnSig; Body = body } ->
                getDepsForFn env locals classTypeParamNames fnSig body
              | ClassElem.Method { Sig = fnSig; Body = body } ->
                getDepsForFn env locals classTypeParamNames fnSig body
              | ClassElem.Getter getter ->
                let fnSig: FuncSig =
                  { ParamList = []
                    Self = Some getter.Self
                    ReturnType = getter.ReturnType
                    Throws = None
                    TypeParams = None
                    IsAsync = false }

                getDepsForFn env locals classTypeParamNames fnSig getter.Body
              | ClassElem.Setter setter ->
                let undefinedTypeAnn: TypeAnn =
                  { Span = DUMMY_SPAN
                    Kind = TypeAnnKind.Literal Literal.Undefined
                    InferredType = None }

                let fnSig: FuncSig =
                  { ParamList = []
                    Self = Some setter.Self
                    ReturnType = Some undefinedTypeAnn
                    Throws = None
                    TypeParams = None
                    IsAsync = false }

                getDepsForFn env locals classTypeParamNames fnSig setter.Body)

          graph <- graph.Add(DeclIdent.Type name, decl, deps)
          declared <- declared @ [ DeclIdent.Value name ]
        | InterfaceDecl { Name = name
                          TypeParams = typeParams
                          Elems = elems } ->
          let interfaceName = name

          let interfaceTypeParamNames =
            match typeParams with
            | None -> []
            | Some typeParams ->
              List.map (fun (tp: Syntax.TypeParam) -> tp.Name) typeParams

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
                  getDepsForInterfaceFn
                    env
                    locals
                    interfaceTypeParamNames
                    fnSig

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
                let tp: Syntax.TypeParam =
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

          match graph.Edges.TryFind(DeclIdent.Type name) with
          | Some existingDeps ->
            graph <- graph.Add(DeclIdent.Type name, decl, existingDeps @ deps)
          | None -> graph <- graph.Add(DeclIdent.Type name, decl, deps)
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
          graph <- graph.AddNamespace(name, subgraph)

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
            let names = Helpers.findBindingNames pattern

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

  // TODO: Merge ReadonlyFoo and Foo as part Escalier.Interop.Migrate
  let mergeType (imutType: Type) (mutType: Type) : Type =
    // If a method exists on both `imutType` and `mutType` then it's a mutable method
    // If it only exists on `imutType` then it's an immutable method
    // There should never be a method that only exists on `mutType`

    match imutType.Kind, mutType.Kind with
    | TypeKind.Object imutElems, TypeKind.Object mutElems ->
      // TODO: figure out how to handle overloaded methods
      let mutable imutNamedElems: Map<PropName, ObjTypeElem> =
        imutElems.Elems
        |> List.choose (fun elem ->
          match elem with
          | ObjTypeElem.Property p ->
            match p.Name with
            | PropName.String s -> Some(PropName.String s, elem)
            | PropName.Number n -> Some(PropName.Number n, elem)
            | PropName.Symbol i -> Some(PropName.Symbol i, elem)
          | ObjTypeElem.Method(name, fn) ->
            match name with
            | PropName.String s -> Some(PropName.String s, elem)
            | PropName.Number n -> Some(PropName.Number n, elem)
            | PropName.Symbol i -> Some(PropName.Symbol i, elem)
          | ObjTypeElem.Getter(name, fn) ->
            match name with
            | PropName.String s -> Some(PropName.String s, elem)
            | PropName.Number n -> Some(PropName.Number n, elem)
            | PropName.Symbol i -> Some(PropName.Symbol i, elem)
          | ObjTypeElem.Setter(name, fn) ->
            match name with
            | PropName.String s -> Some(PropName.String s, elem)
            | PropName.Number n -> Some(PropName.Number n, elem)
            | PropName.Symbol i -> Some(PropName.Symbol i, elem)
          | _ -> None)
        |> Map.ofSeq

      let mutable unnamedElems: list<ObjTypeElem> = []

      let mutable mutNamedElems: Map<PropName, ObjTypeElem> =
        mutElems.Elems
        |> List.choose (fun elem ->
          match elem with
          | ObjTypeElem.Property p ->
            match p.Name with
            | PropName.String s -> Some(PropName.String s, elem)
            | PropName.Number n -> Some(PropName.Number n, elem)
            | PropName.Symbol i -> Some(PropName.Symbol i, elem)
          | ObjTypeElem.Method(name, fn) ->
            match name with
            | PropName.String s -> Some(PropName.String s, elem)
            | PropName.Number n -> Some(PropName.Number n, elem)
            | PropName.Symbol i -> Some(PropName.Symbol i, elem)
          | ObjTypeElem.Getter(name, fn) ->
            match name with
            | PropName.String s -> Some(PropName.String s, elem)
            | PropName.Number n -> Some(PropName.Number n, elem)
            | PropName.Symbol i -> Some(PropName.Symbol i, elem)
          | ObjTypeElem.Setter(name, fn) ->
            match name with
            | PropName.String s -> Some(PropName.String s, elem)
            | PropName.Number n -> Some(PropName.Number n, elem)
            | PropName.Symbol i -> Some(PropName.Symbol i, elem)
          | elem ->
            // This assumes that indexed/mapped signatures are on both the
            // readonly and non-readonly interfaces.  We ignore the readonly
            // one because the signature isn't responsible for preventing
            // mutation of the object in this way.  Instead we have a special
            // check for this.
            unnamedElems <- elem :: unnamedElems
            None)
        |> Map.ofSeq

      let elems =
        mutNamedElems
        |> Map.toSeq
        |> Seq.map (fun (key, value) ->
          match imutNamedElems.TryFind key with
          | Some(imutValue) -> imutValue
          | None ->
            match value with
            | ObjTypeElem.Method(name, fn) ->
              let self =
                match fn.Self with
                | None -> None
                | Some self ->
                  Some
                    { self with
                        Pattern =
                          Pattern.Identifier { Name = "self"; IsMut = true } }

              ObjTypeElem.Method(name, { fn with Self = self })
            | elem -> elem)

      let kind =
        TypeKind.Object
          { Elems = List.ofSeq elems @ unnamedElems
            Immutable = false
            Interface = false }

      { Kind = kind; Provenance = None }
    | _ -> failwith "both types must be objects to merge them"
