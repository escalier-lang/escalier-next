module Escalier.TypeChecker.InferGraph

open Escalier.Data.Type
open Escalier.TypeChecker.Env
open FsToolkit.ErrorHandling

open Escalier.Data.Syntax

open Error
open ExprVisitor
open QualifiedGraph

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
        | TypeDecl { TypeParams = typeParams; Name = name } ->
          // TODO: check to make sure we aren't redefining an existing type

          // TODO: replace placeholders, with a reference the actual definition
          // once we've inferred the definition
          let! placeholder =
            Infer.inferTypeDeclPlaceholderScheme ctx env typeParams

          let key =
            match ident with
            | Type { Namespaces = namespaces } ->
              { Namespaces = namespaces; Name = name }
            | Value { Namespaces = namespaces } ->
              { Namespaces = namespaces; Name = name }

          qns <- qns.AddScheme key placeholder
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
  (idents: list<QDeclIdent>)
  (graph: QGraph)
  : Result<QualifiedNamespace, TypeError> =

  result {
    let mutable qns = qns

    for ident in idents do
      let decls = graph.Nodes[ident]

      // TODO: check if we're inside a namespace and update the env accordingly
      let namespaces =
        match ident with
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
        | TypeDecl { Name = name
                     TypeAnn = typeAnn
                     TypeParams = typeParams } ->
          let key =
            match ident with
            | Type { Namespaces = namespaces } ->
              { Namespaces = namespaces; Name = name }
            | Value { Namespaces = namespaces } ->
              { Namespaces = namespaces; Name = name }

          let placeholder = qns.Schemes[key]
          // TODO: when computing the decl graph, include self-recursive types in
          // the deps set so that we don't have to special case this here.
          // Handles self-recursive types
          newEnv <- newEnv.AddScheme name placeholder
          let getType = fun env -> Infer.inferTypeAnn ctx env typeAnn

          let! scheme =
            Infer.inferTypeDeclDefn ctx newEnv placeholder typeParams getType

          // Replace the placeholder's type with the actual type.
          // NOTE: This is a bit hacky and we may want to change this later to use
          // `foldType` to replace any uses of the placeholder with the actual type.
          // Required for the following test cases:
          // - InferRecursiveGenericObjectTypeInModule
          // - InferNamespaceInModule
          // placeholder.Value.Type <- scheme.Type
          qns.Schemes[key].Type <- scheme.Type
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
            Infer.inferTypeDeclDefn ctx newEnv placeholder typeParams getType

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

  let rec addSchemeRec (ns: Namespace) (namespaces: list<string>) : Namespace =
    match namespaces with
    | [] -> ns.AddScheme ident.Name scheme
    | headNS :: restNS ->
      match ns.Namespaces.TryFind(headNS) with
      | None ->
        let newNS = { Namespace.empty with Name = headNS }
        ns.AddNamespace headNS (addSchemeRec newNS restNS)
      | Some existingNS ->
        ns.AddNamespace headNS (addSchemeRec existingNS restNS)

  match ident.Namespaces with
  | [] -> env.AddScheme ident.Name scheme
  | namespaces ->
    { env with
        Namespace = addSchemeRec env.Namespace namespaces }

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
