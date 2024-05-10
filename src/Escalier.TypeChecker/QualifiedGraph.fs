module Escalier.TypeChecker.QualifiedGraph

open Escalier.Data.Type
open FsToolkit.ErrorHandling

open Escalier.Data.Common
open Escalier.Data.Syntax

open Error
open Env
open ExprVisitor

// TODO:
// - infer types for all the declarations in each namespace
// - determine the dependences between declarations in each namespace
// - infer them in the correct order

type QDeclIDent =
  | Type of QualifiedIdent
  | Value of QualifiedIdent

type QGraph =
  { Nodes: Map<QDeclIDent, Decl>
    Edges: Map<QDeclIDent, list<QDeclIDent>> }

let rec buildGraph (nsIdent: option<QualifiedIdent>) (decl: Decl) : QGraph =
  let mutable graph = { Nodes = Map.empty; Edges = Map.empty }

  match decl.Kind with
  | VarDecl { Pattern = pattern } ->
    let idents =
      Helpers.findBindingNames pattern
      |> List.map (fun name ->
        match nsIdent with
        | Some left -> QualifiedIdent.Member(left, name)
        | None -> QualifiedIdent.Ident name)

    for ident in idents do
      graph <-
        { graph with
            Nodes = Map.add (Value ident) decl graph.Nodes }
  | FnDecl { Name = name } ->
    let ident =
      match nsIdent with
      | Some left -> QualifiedIdent.Member(left, name)
      | None -> QualifiedIdent.Ident name

    graph <-
      { graph with
          Nodes = Map.add (Value ident) decl graph.Nodes }
  | ClassDecl { Name = name } ->
    let ident =
      match nsIdent with
      | Some left -> QualifiedIdent.Member(left, name)
      | None -> QualifiedIdent.Ident name

    graph <-
      { graph with
          Nodes = Map.add (Value ident) decl graph.Nodes }

    graph <-
      { graph with
          Nodes = Map.add (Type ident) decl graph.Nodes }
  | TypeDecl { Name = name } ->
    let ident =
      match nsIdent with
      | Some left -> QualifiedIdent.Member(left, name)
      | None -> QualifiedIdent.Ident name

    graph <-
      { graph with
          Nodes = Map.add (Type ident) decl graph.Nodes }
  | InterfaceDecl { Name = name } ->
    let ident =
      match nsIdent with
      | Some left -> QualifiedIdent.Member(left, name)
      | None -> QualifiedIdent.Ident name

    // TODO: check if there's an existing node with the same name
    // and merge their decls
    graph <-
      { graph with
          Nodes = Map.add (Type ident) decl graph.Nodes }
  | EnumDecl { Name = name } ->
    let ident =
      match nsIdent with
      | Some left -> QualifiedIdent.Member(left, name)
      | None -> QualifiedIdent.Ident name

    graph <-
      { graph with
          Nodes = Map.add (Value ident) decl graph.Nodes }

    graph <-
      { graph with
          Nodes = Map.add (Type ident) decl graph.Nodes }
  | NamespaceDecl { Name = name; Body = body } ->
    let nsIdent =
      match nsIdent with
      | Some left -> QualifiedIdent.Member(left, name)
      | None -> QualifiedIdent.Ident name

    for decl in body do
      let subGraph = buildGraph (Some nsIdent) decl

      for KeyValue(ident, decl) in subGraph.Nodes do
        graph <-
          { graph with
              Nodes = Map.add ident decl graph.Nodes }

      for KeyValue(ident, deps) in subGraph.Edges do
        graph <-
          { graph with
              Edges = Map.add ident deps graph.Edges }

  graph

let getIdentsForModule (m: Module) : QGraph =

  let mutable graph = { Nodes = Map.empty; Edges = Map.empty }

  for item in m.Items do
    match item with
    | ModuleItem.Import _ -> ()
    | ModuleItem.Decl decl ->
      let subGraph = buildGraph None decl

      for KeyValue(ident, decl) in subGraph.Nodes do
        graph <-
          { graph with
              Nodes = Map.add ident decl graph.Nodes }

      for KeyValue(ident, deps) in subGraph.Edges do
        graph <-
          { graph with
              Edges = Map.add ident deps graph.Edges }

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
      | InterfaceDecl { Name = name; TypeParams = typeParams } ->
        let! placeholder =
          match newEnv.TryFindScheme name with
          | Some scheme -> Result.Ok scheme
          | None -> Infer.inferTypeDeclPlaceholderScheme ctx env typeParams

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
      | FnDecl { Declare = false
                 Name = name
                 Sig = fnSig
                 Body = Some body } ->
        // NOTE: We explicitly don't generalize here because we want other
        // declarations to be able to unify with any free type variables
        // from this declaration.  We generalize things in `inferModule` and
        // `inferTreeRec`.
        let! f = Infer.inferFunction ctx env fnSig body

        let _ =
          { Kind = TypeKind.Function f
            Provenance = None }

        // TODO: add inferred type to the function decl
        ()
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

let inferDecls
  (ctx: Ctx)
  (env: Env)
  (ident: QDeclIDent)
  (decls: list<Decl>)
  : Result<unit, TypeError> =
  result {
    // in the future we'll generalize each set of recursive decls once we're
    // computing dependencies correctly
    let generalize = true

    match ident with
    | Type qualifiedIdent ->
      match qualifiedIdent with
      | QualifiedIdent.Ident s ->
        do! inferDeclPlaceholders ctx env decls
        do! inferDeclDefinitions ctx env decls
        ()
      | QualifiedIdent.Member(left, right) ->
        // let! ns = getOrCreateNamespace left env.Namespace
        // printfn $"ns = {ns}"
        // TODO: get namespace from `env` for `left`
        do! inferDeclPlaceholders ctx env decls
        do! inferDeclDefinitions ctx env decls
        ()
    | Value qualifiedIdent ->
      match qualifiedIdent with
      | QualifiedIdent.Ident s ->
        do! inferDeclPlaceholders ctx env decls
        do! inferDeclDefinitions ctx env decls
        ()
      | QualifiedIdent.Member(left, right) ->
        // let! ns = getOrCreateNamespace left env.Namespace
        // printfn $"ns = {ns}"
        // TODO: get namespace from `env` for `left`
        do! inferDeclPlaceholders ctx env decls
        do! inferDeclDefinitions ctx env decls
        ()

    return ()
  }

let inferGraph (ctx: Ctx) (env: Env) (graph: QGraph) : Result<Env, TypeError> =
  result {
    for KeyValue(ident, decl) in graph.Nodes do
      do! inferDecls ctx env ident [ decl ]

    let mutable newEnv = env

    // NOTE: We don't necessarily need to add these to the environment, we could
    // return a namespace with the inferred types.
    for KeyValue(ident, decl) in graph.Nodes do
      match decl.Kind with
      | VarDecl varDecl ->
        // TODO: store the binding instead of just the types
        let valueTypes = getAllBindingPatterns varDecl.Pattern

        for KeyValue(name, t) in valueTypes do
          newEnv <- newEnv.AddValue name (t, false)
      | FnDecl fnDecl ->
        newEnv <- newEnv.AddValue fnDecl.Name (fnDecl.Inferred.Value, false)
      | ClassDecl classDecl ->
        newEnv <-
          newEnv.AddValue
            classDecl.Name
            (classDecl.Inferred.Value.Statics, false)

        newEnv <-
          newEnv.AddScheme classDecl.Name classDecl.Inferred.Value.Instance
      | TypeDecl typeDecl ->
        newEnv <- newEnv.AddScheme typeDecl.Name typeDecl.Inferred.Value
      | InterfaceDecl interfaceDecl ->
        newEnv <-
          newEnv.AddScheme interfaceDecl.Name interfaceDecl.Inferred.Value
      | EnumDecl enumDecl -> failwith "TODO: inferGraph - EnumDecl"
      | NamespaceDecl namespaceDecl ->
        failwith "TODO: inferGraph - NamespaceDecl"

    // TODO: implement
    // - infer placeholders for all nodes in the graph
    // - infer type definitions for all nodes in the graph

    return newEnv
  }
