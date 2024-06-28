module Escalier.TypeChecker.InferGraph

open Escalier.Data.Type
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.QualifiedGraph
open FsToolkit.ErrorHandling

open Escalier.Data.Syntax

open BuildGraph
open Error
open ExprVisitor

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

let rec inferExprStructuralPlacholder
  (ctx: Ctx)
  (env: Env)
  (expr: Expr)
  : Result<Type, TypeError> =
  result {
    match expr.Kind with
    | ExprKind.Object { Elems = elems } ->
      let! elems =
        elems
        |> List.traverseResultM (fun elem ->
          result {
            match elem with
            | ObjElem.Property(span, name, value) ->
              let! name = Infer.inferPropName ctx env name
              let! t = inferExprStructuralPlacholder ctx env value

              return
                ObjTypeElem.Property
                  { Name = name
                    Optional = false
                    Readonly = false
                    Type = t }
            | ObjElem.Shorthand(span, name) ->
              match env.TryFindValue name with
              | Some(t, _) ->
                return
                  ObjTypeElem.Property
                    { Name = Escalier.Data.Type.PropName.String name
                      Optional = false
                      Readonly = false
                      Type = t }
              | None ->
                return! Error(TypeError.SemanticError $"{name} not found")
            | ObjElem.Spread(span, value) ->
              return!
                Error(
                  TypeError.NotImplemented
                    "TODO: inferExprStructuralPlacholder - Spread"
                )
          })

      let kind =
        TypeKind.Object
          { Extends = None
            Implements = None
            Elems = elems
            Immutable = false
            Interface = false }

      return { Kind = kind; Provenance = None }
    | ExprKind.Tuple { Elems = elems } ->
      let! elems =
        elems
        |> List.traverseResultM (fun elem ->
          inferExprStructuralPlacholder ctx env elem)

      let kind = TypeKind.Tuple { Elems = elems; Immutable = false }

      return { Kind = kind; Provenance = None }
    | _ -> return ctx.FreshTypeVar None None
  }

let inferDeclPlaceholders
  (ctx: Ctx)
  (env: Env)
  (qns: QualifiedNamespace)
  (idents: list<QDeclIdent>)
  (graph: QGraph<Decl>)
  : Result<QualifiedNamespace, TypeError> =

  result {
    let mutable qns = qns

    for ident in idents do
      if not (graph.Nodes.ContainsKey ident) then
        // TODO: make sure that the name space exists when determine if a decl
        // is qualified with a namespace or not.
        // printfn "ident = %A" ident
        return! Error(TypeError.SemanticError "Missing node in graph")

      let decls = graph.Nodes[ident]

      for decl in decls do
        match decl.Kind with
        | VarDecl { Pattern = pattern
                    Init = init
                    TypeAnn = typeAnn } ->
          // QUESTION: Should we check the type annotation as we're generating
          // the placeholder type?
          let! bindings, patternType = Infer.inferPattern ctx env pattern

          match typeAnn, init with
          | None, Some init ->
            // Used by the following test cases:
            // - MutuallyRecursiveGraphInDepObjects
            // - MutuallyRecursiveGraphInObjects
            // - MutuallyRecursiveGraphInObjects
            let! structuralPlacholderType =
              inferExprStructuralPlacholder ctx env init

            do! Unify.unify ctx env None patternType structuralPlacholderType
          | _, _ -> ()

          for KeyValue(name, binding) in bindings do
            let key =
              match ident with
              | Type { Parts = parts } ->
                { Parts = List.take (parts.Length - 1) parts @ [ name ] }
              | Value { Parts = parts } ->
                { Parts = List.take (parts.Length - 1) parts @ [ name ] }

            qns <- qns.AddValue key binding
        | FnDecl({ Declare = declare
                   Sig = fnSig
                   Name = name }) ->
          if declare then
            // TODO: capture these errors as diagnostics and infer the missing
            // types as `never`
            for p in fnSig.ParamList do
              if p.TypeAnn.IsNone then
                failwith "Ambient function declarations must be fully typed"

            if fnSig.ReturnType.IsNone then
              failwith "Ambient function declarations must be fully typed"

          let t = ctx.FreshTypeVar None None

          let key =
            match ident with
            | Type { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }
            | Value { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }

          qns <- qns.AddValue key (t, false)
        | ClassDecl({ Name = name
                      Class = { TypeParams = typeParams } } as decl) ->
          // TODO: treat ClassDecl similar to object types where we create a
          // structural placeholder type instead of an opaque type variable.
          // We should do this for both instance members and statics.
          let! instance =
            Infer.inferTypeDeclPlaceholderScheme ctx env typeParams
          // let instance: Scheme =
          //   { Type = ctx.FreshTypeVar None
          //     TypeParams = None // TODO: handle type params
          //     IsTypeParam = false }

          let key =
            match ident with
            | Type { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }
            | Value { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }

          qns <- qns.AddScheme key instance

          let statics: Type = ctx.FreshTypeVar None None

          qns <- qns.AddValue key (statics, false)
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
            | Type { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }
            | Value { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }

          qns <- qns.AddScheme key placeholder
        | InterfaceDecl({ Name = name
                          TypeParams = typeParams
                          Extends = extends } as decl) ->
          let key =
            match ident with
            | Type { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }
            | Value { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }

          let parts =
            match ident with
            | Type { Parts = parts } -> parts
            | Value { Parts = parts } -> parts

          // Instead of looking things up in the environment, we need some way to
          // find the existing type on other declarations.
          let! placeholder =
            // NOTE: looking up the scheme using `name` works here because we
            // called `openNamespaces` at the top of this function.
            match parts, env.TryFindScheme name with
            // TODO: handle the case where the qualifier is `global` to handle
            // declare global { ... } statements.
            | [ _ ], Some scheme -> Result.Ok scheme
            | _, _ ->
              match qns.Schemes.TryFind(key) with
              | Some scheme -> Result.Ok scheme
              | None -> Infer.inferTypeDeclPlaceholderScheme ctx env typeParams

          if key = QualifiedIdent.FromString "Bar" then
            printfn $"extends = {extends}"

          qns <- qns.AddScheme key placeholder
        | NamespaceDecl nsDecl ->
          return!
            Error(
              TypeError.NotImplemented
                "TODO: inferDeclPlaceholders - NamespaceDecl"
            )

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
      match parentNS.Namespaces.TryFind(head) with
      | None -> printfn $"namespace {head} not found"
      | Some _ -> ()

      let nextNS = parentNS.Namespaces[head]

      for KeyValue(name, scheme) in nextNS.Schemes do
        newEnv <- newEnv.AddScheme name scheme

      for KeyValue(name, binding) in nextNS.Values do
        newEnv <- newEnv.AddValue name binding

      for KeyValue(name, ns) in nextNS.Namespaces do
        newEnv <- newEnv.AddNamespace name ns

      openNamespace nextNS rest

  match namespaces with
  | "global" :: rest -> openNamespace env.Namespace rest
  | namespaces -> openNamespace env.Namespace namespaces

  newEnv

let inferDeclDefinitions
  (ctx: Ctx)
  (env: Env)
  (qns: QualifiedNamespace)
  (idents: list<QDeclIdent>)
  (graph: QGraph<Decl>)
  : Result<QualifiedNamespace, TypeError> =

  result {
    let mutable qns = qns

    for ident in idents do
      // printfn $"inferring ident: {ident}"
      let decls = graph.Nodes[ident]

      // TODO: check if we're inside a namespace and update the env accordingly
      let parts =
        match ident with
        | Type { Parts = parts } -> parts
        | Value { Parts = parts } -> parts

      let namespaces = List.take (List.length parts - 1) parts
      let name = List.last parts

      let mutable newEnv = env
      newEnv <- openNamespaces newEnv namespaces

      // Strategy:
      // - separate decls into groups based on their kind
      // - if there are multiple non-empty groups, error
      // - if any of the groups other than FnDecl or InterfaceDecl have more
      //   than one decl, error
      // - infer each group
      let mutable varDecls = []
      let mutable fnDecls = []
      let mutable classDecls = []
      let mutable typeDecls = []
      let mutable interfaceDecls = []
      let mutable enumDecls = []
      let mutable namespaceDecls = []

      for decl in decls do
        match decl.Kind with
        | VarDecl varDecl -> varDecls <- varDecl :: varDecls
        | FnDecl fnDecl -> fnDecls <- fnDecl :: fnDecls
        | ClassDecl classDecl -> classDecls <- classDecl :: classDecls
        | TypeDecl typeDecl -> typeDecls <- typeDecl :: typeDecls
        | InterfaceDecl interfaceDecl ->
          interfaceDecls <- interfaceDecl :: interfaceDecls
        | EnumDecl enumDecl -> enumDecls <- enumDecl :: enumDecls
        | NamespaceDecl namespaceDecl ->
          namespaceDecls <- namespaceDecl :: namespaceDecls

      fnDecls <- List.rev fnDecls
      interfaceDecls <- List.rev interfaceDecls

      let mutable count = 0

      if varDecls.Length > 0 then
        count <- count + 1

      if fnDecls.Length > 0 then
        count <- count + 1

      if classDecls.Length > 0 then
        count <- count + 1

      if typeDecls.Length > 0 then
        count <- count + 1

      if interfaceDecls.Length > 0 then
        count <- count + 1

      if enumDecls.Length > 0 then
        count <- count + 1

      if namespaceDecls.Length > 0 then
        count <- count + 1

      if count > 1 then
        return!
          Error(
            TypeError.SemanticError "more than one kind of decl found for ident"
          )

      match varDecls with
      | [] -> ()
      | [ varDecl ] ->
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
      | _ ->
        return!
          Error(TypeError.SemanticError "multiple var decls found for ident")

      if fnDecls.Length > 0 then
        let! fns =
          List.traverseResultM
            (fun (fnDecl: FnDecl) ->
              result {
                let! f =
                  match fnDecl.Declare, fnDecl.Body with
                  | false, Some body ->
                    // NOTE: `inferFunction` also calls unify
                    Infer.inferFunction ctx newEnv fnDecl.Sig body
                  | true, None -> Infer.inferFuncSig ctx newEnv fnDecl.Sig
                  | _, _ ->
                    Result.Error(
                      TypeError.SemanticError "Invalid function declaration"
                    )

                return f
              })
            fnDecls

        let types =
          fns
          |> List.map (fun f ->
            { Kind = TypeKind.Function f
              Provenance = None })

        let inferredType =
          match types with
          | [] -> failwith "No types found"
          | [ t ] -> t
          | _ ->
            { Kind = TypeKind.Intersection types
              Provenance = None }

        let placeholderType, _ =
          match newEnv.TryFindValue name with
          | Some t -> t
          | None -> failwith "Missing placeholder type"

        // NOTE: We explicitly don't generalize here because we want other
        // declarations to be able to unify with any free type variables
        // from this declaration.  We generalize things in `inferModule` and
        // `inferTreeRec`.
        do! Unify.unify ctx newEnv None placeholderType inferredType


      // TODO: handle remaining decl kind lists
      match classDecls with
      | [] -> ()
      | [ classDecl ] ->
        let { Declare = declare
              Name = name
              Class = cls } =
          classDecl

        // TODO: figure out how to only call inferClass once instead of twice
        let! inferredType, inferredScheme =
          Infer.inferClass ctx newEnv cls declare

        match ident with
        | Type { Parts = parts } ->
          let key = { Parts = List.take (parts.Length - 1) parts @ [ name ] }
          let placeholderScheme = qns.Schemes[key]

          do!
            Unify.unify
              ctx
              newEnv
              None
              placeholderScheme.Type
              inferredScheme.Type
        | Value { Parts = parts } ->
          let key = { Parts = List.take (parts.Length - 1) parts @ [ name ] }
          let placeholderType, _ = qns.Values[key]
          do! Unify.unify ctx newEnv None placeholderType inferredType
      | _ ->
        return!
          Error(TypeError.SemanticError "More than one class decl for ident")

      match enumDecls with
      | [] -> ()
      | [ enumDecl ] -> return! Error(TypeError.NotImplemented "EnumDecl")
      | _ -> return! Error(TypeError.SemanticError "More than one enum decl")

      match typeDecls with
      | [] -> ()
      | [ typeDecl ] ->
        let { Name = name
              TypeAnn = typeAnn
              TypeParams = typeParams } =
          typeDecl

        let key =
          match ident with
          | Type { Parts = parts } ->
            { Parts = List.take (parts.Length - 1) parts @ [ name ] }
          | Value { Parts = parts } ->
            { Parts = List.take (parts.Length - 1) parts @ [ name ] }

        let placeholder = qns.Schemes[key]
        // TODO: when computing the decl graph, include self-recursive types in
        // the deps set so that we don't have to special case this here.
        // Handles self-recursive types
        newEnv <- newEnv.AddScheme name placeholder
        let getType = fun env -> Infer.inferTypeAnn ctx env typeAnn

        let! scheme =
          Infer.inferTypeDeclDefn ctx newEnv placeholder typeParams getType

        match placeholder.TypeParams, scheme.TypeParams with
        | Some typeParams1, Some typeParams2 ->
          for typeParam1, typeParam2 in List.zip typeParams1 typeParams2 do
            match typeParam1.Constraint, typeParam2.Constraint with
            | Some c1, Some c2 -> do! Unify.unify ctx newEnv None c1 c2
            | None, None -> ()
            | _, _ ->
              return!
                Error(
                  TypeError.SemanticError
                    "One scheme has a constraint type while the other doesn't"
                )

            match typeParam1.Default, typeParam2.Default with
            | Some d1, Some d2 -> do! Unify.unify ctx newEnv None d1 d2
            | None, None -> ()
            | _, _ ->
              return!
                Error(
                  TypeError.SemanticError
                    "One scheme has a default type while the other doesn't"
                )
        | None, None -> ()
        | _, _ ->
          return!
            Error(
              TypeError.SemanticError
                "One scheme has type params while the other doesn't"
            )

        // Replace the placeholder's type with the actual type.
        // NOTE: This is a bit hacky and we may want to change this later to use
        // `foldType` to replace any uses of the placeholder with the actual type.
        // Required for the following test cases:
        // - InferRecursiveGenericObjectTypeInModule
        // - InferNamespaceInModule
        // placeholder.Value.Type <- scheme.Type
        qns.Schemes[key].Type <- scheme.Type

      | _ ->
        return!
          Error(TypeError.SemanticError "More than one type decl for ident")

      match interfaceDecls with
      | [] -> ()
      | interfaceDecls ->

        for interfaceDecl in interfaceDecls do
          let { Name = name
                TypeParams = typeParams
                Extends = extends
                Elems = elems } =
            interfaceDecl

          let key =
            match ident with
            | Type { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }
            | Value { Parts = parts } ->
              { Parts = List.take (parts.Length - 1) parts @ [ name ] }

          let placeholder = qns.Schemes[key]

          // TODO: when computing the decl graph, include self-recursive types in
          // the deps set so that we don't have to special case this here.
          // Handles self-recursive types
          let newEnv = newEnv.AddScheme name placeholder

          let getType (env: Env) : Result<Type, TypeError> =
            result {
              let! elems =
                List.traverseResultM (Infer.inferObjElem ctx env) elems

              let! extends =
                match extends with
                | Some typeRefs ->
                  result {

                    let! extends =
                      List.traverseResultM (Infer.inferTypeRef ctx env) typeRefs

                    let extends =
                      List.map
                        (fun kind ->
                          match kind with
                          | TypeKind.TypeRef typeRef -> typeRef
                          | _ -> failwith "Invalid type for extends")
                        extends

                    return Some extends
                  }
                | None -> Result.Ok None

              let kind =
                TypeKind.Object
                  { Extends = extends
                    Implements = None // TODO
                    Elems = elems
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
                { Extends = None
                  Implements = None // TODO
                  Elems = mergedElems
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
            qns.Schemes[key].Type <- t
          | _ ->
            // Replace the placeholder's type with the actual type.
            // NOTE: This is a bit hacky and we may want to change this later to use
            // `foldType` to replace any uses of the placeholder with the actual type.
            placeholder.Type <- newScheme.Type
            qns.Schemes[key].Type <- newScheme.Type

      match namespaceDecls with
      | [] -> ()
      | [ namespaceDecl ] ->
        return! Error(TypeError.NotImplemented "NamespaceDecl")
      | _ ->
        return!
          Error(
            TypeError.SemanticError "More than one namespace decl for ident"
          )

    return qns
  }

type QDeclTree =
  { Edges: Map<Set<QDeclIdent>, Set<Set<QDeclIdent>>>
    CycleMap: Map<QDeclIdent, Set<QDeclIdent>> }

// Based on the algorithm from https://en.wikipedia.org/wiki/Path-based_strong_component_algorithm
let findStronglyConnectedComponents<'T>
  (graph: QGraph<'T>)
  : list<list<QDeclIdent>> =

  let mutable S: list<QDeclIdent> = [] // not yet assigned to a SCC
  let mutable P: list<QDeclIdent> = [] // not yet in different SCCs
  let mutable preorder: Map<QDeclIdent, int> = Map.empty
  let mutable C: int = 0
  let mutable components: list<list<QDeclIdent>> = []

  let rec visit (v: QDeclIdent) : unit =
    // 1. Set the preorder number of v to C, and increment C.
    preorder <- Map.add v C preorder
    C <- C + 1

    // 2. Push v onto S and also onto P.
    S <- v :: S
    P <- v :: P

    let deps =
      match graph.Edges.TryFind v with
      | None -> Set.empty
      | Some deps -> deps

    // 3. For each edge from v to a neighboring vertex w:
    for dep in deps do
      let w = dep

      match preorder.TryFind w with
      | None ->
        // If the preorder number of w has not yet been assigned (the edge is a
        // tree edge), recursively search w;
        visit w
      | Some _ ->
        // Otherwise, if w has not yet been assigned to a strongly connected
        // component (the edge is a forward/back/cross edge):
        if List.contains w S then
          // Repeatedly pop vertices from P until the top element of P has a
          // preorder number less than or equal to the preorder number of w
          while preorder[List.head P] > preorder[w] do
            P <- List.tail P // pop from P

    let mutable comp: list<QDeclIdent> = []

    // 4. If v is the top element of P:
    if v = List.head P then
      // Pop vertices from S until v has been popped, and assign the popped
      // vertices to a new component.
      while v <> List.head S do
        comp <- List.head S :: comp
        S <- List.tail S

      comp <- List.head S :: comp
      S <- List.tail S

      // Pop v from P.
      P <- List.tail P

      components <- comp :: components

  for v in graph.Nodes.Keys do
    if not (preorder.ContainsKey v) then
      visit v

  components


type QCompTree = Map<Set<QDeclIdent>, Set<Set<QDeclIdent>>>

let buildComponentTree<'T>
  (graph: QGraph<'T>)
  (components: list<list<QDeclIdent>>)
  : QCompTree =

  let comps = List.map (fun comp -> Set.ofList comp) components
  let mutable compMap: Map<QDeclIdent, Set<QDeclIdent>> = Map.empty

  for comp in comps do
    for v in comp do
      compMap <- Map.add v comp compMap

  let mutable tree: QCompTree = Map.empty

  for comp in comps do
    let mutable targets = Set.empty

    let mutable compDepNodes = Set.empty

    for node in comp do
      let nodeDeps =
        match graph.Edges.TryFind node with
        | None -> Set.empty
        | Some deps -> deps

      compDepNodes <- Set.union (Set.difference nodeDeps comp) compDepNodes

    let compDeps = Set.map (fun dep -> Map.find dep compMap) compDepNodes
    tree <- Map.add comp compDeps tree

  tree

let findEntryPoints (tree: QCompTree) : Set<Set<QDeclIdent>> =
  let mutable allDeps = Set.empty

  for KeyValue(_, deps) in tree do
    allDeps <- Set.union allDeps deps

  Set.difference (Set.ofSeq tree.Keys) allDeps

let addBinding (env: Env) (ident: QualifiedIdent) (binding: Binding) : Env =

  let rec addValueRec (ns: Namespace) (parts: list<string>) : Namespace =
    match parts with
    | [] -> failwith "Invalid qualified ident"
    | [ name ] -> ns.AddBinding name binding
    | headNS :: restNS ->
      match ns.Namespaces.TryFind(headNS) with
      | None ->
        let newNS = { Namespace.empty with Name = headNS }
        ns.AddNamespace headNS (addValueRec newNS restNS)
      | Some existingNS ->
        ns.AddNamespace headNS (addValueRec existingNS restNS)

  let parts =
    match ident.Parts with
    | "global" :: rest -> rest
    | parts -> parts

  { env with
      Namespace = addValueRec env.Namespace parts }

let addScheme (env: Env) (ident: QualifiedIdent) (scheme: Scheme) : Env =

  let rec addSchemeRec (ns: Namespace) (parts: list<string>) : Namespace =
    match parts with
    | [] -> failwith "Invalid qualified ident"
    | [ name ] -> ns.AddScheme name scheme
    | headNS :: restNS ->
      match ns.Namespaces.TryFind(headNS) with
      | None ->
        let newNS = { Namespace.empty with Name = headNS }
        ns.AddNamespace headNS (addSchemeRec newNS restNS)
      | Some existingNS ->
        ns.AddNamespace headNS (addSchemeRec existingNS restNS)

  let parts =
    match ident.Parts with
    | "global" :: rest -> rest
    | parts -> parts

  { env with
      Namespace = addSchemeRec env.Namespace parts }

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
  : QualifiedNamespace =
  let mutable dst = dst

  for KeyValue(key, binding) in src.Values do
    dst <- dst.AddValue key binding

  for KeyValue(key, scheme) in src.Schemes do
    dst <- dst.AddScheme key scheme

  dst

let generalizeBindings
  (bindings: Map<QualifiedIdent, Binding>)
  : Map<QualifiedIdent, Binding> =
  let mutable newBindings = Map.empty

  for KeyValue(name, (t, isMut)) in bindings do
    let t = Helpers.generalizeType t
    newBindings <- newBindings.Add(name, (t, isMut))

  newBindings

let inferTree
  (ctx: Ctx)
  (env: Env)
  (graph: QGraph<Decl>)
  (tree: QCompTree)
  : Result<Env, TypeError> =

  result {
    let mutable newEnv = env
    let entryPoints = findEntryPoints tree

    let mutable processed: Map<Set<QDeclIdent>, QualifiedNamespace> = Map.empty

    let rec inferTreeRec
      (ctx: Ctx)
      (env: Env)
      (root: Set<QDeclIdent>)
      (graph: QGraph<Decl>)
      (tree: QCompTree)
      (fullQns: QualifiedNamespace)
      : Result<QualifiedNamespace * QualifiedNamespace, TypeError> =

      result {
        if Map.containsKey root processed then
          return fullQns, processed[root]
        else
          let mutable fullQns = fullQns
          let mutable partialQns = QualifiedNamespace.Empty

          // Infer dependencies
          match tree.TryFind root with
          | Some deps ->
            for dep in deps do
              // TODO: avoid re-inferring types if they've already been inferred
              let! newFullQns, newPartialQns =
                inferTreeRec ctx env dep graph tree fullQns

              // Update partialQns with new values and types
              partialQns <- updateQualifiedNamespace newPartialQns partialQns

          | None -> ()

          // Update environment to include dependencies
          let mutable newEnv = updateEnvWithQualifiedNamespace env partialQns

          let names = Set.toList root

          // Infer declarations
          let! newPartialQns =
            inferDeclPlaceholders ctx newEnv partialQns names graph

          newEnv <- updateEnvWithQualifiedNamespace newEnv newPartialQns // mutually recursive functions

          let! newPartialQns =
            inferDeclDefinitions ctx newEnv newPartialQns names graph

          // Generalize bindings
          let newPartialQns =
            { newPartialQns with
                Values = generalizeBindings newPartialQns.Values }

          // Update fullQns with new values and types
          fullQns <- updateQualifiedNamespace newPartialQns fullQns

          processed <- Map.add root newPartialQns processed

          return fullQns, newPartialQns
      }

    let mutable qns = QualifiedNamespace.Empty

    // Infer entry points and all their dependencies
    for entryPoint in entryPoints do
      try
        let! fullQns, _ = inferTreeRec ctx env entryPoint graph tree qns
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

let inferGraph
  (ctx: Ctx)
  (env: Env)
  (graph: QGraph<Decl>)
  : Result<Env, TypeError> =
  result {
    // TODO: handle imports

    let components = findStronglyConnectedComponents graph
    let tree = buildComponentTree graph components
    let! newEnv = inferTree ctx env graph tree

    return newEnv
  }

let inferModule (ctx: Ctx) (env: Env) (ast: Module) : Result<Env, TypeError> =
  result {
    // TODO: update this function to accept a filename
    let mutable newEnv = env // { env with Filename = "input.esc" }

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
    let graph = buildGraph newEnv ast
    let components = findStronglyConnectedComponents graph
    let tree = buildComponentTree graph components

    let! newEnv = inferTree ctx newEnv graph tree

    for item in ast.Items do
      match item with
      | Import _ -> () // Already inferred
      | Stmt stmt ->
        match stmt.Kind with
        | Expr expr ->
          let! _ = Infer.inferExpr ctx newEnv expr
          ()
        | For(left, right, body) ->
          // NOTE: this introduces a new scope and new variables
          failwith "TODO: inferModule - For"
        | Return exprOption ->
          failwith "'return' statements aren't allowed outside of functions"
        | Decl _ -> () // Already inferred

    return newEnv
  }
