namespace Escalier.TypeChecker

open Escalier.TypeChecker.QualifiedGraph
open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.Data.Visitor

open Error
open Env
open Unify
open Helpers
open BuildGraph
open InferExpr
open InferTypeAnn
open InferClass
open InferPattern

module rec InferModule =
  let getAllBindingPatterns (pattern: Syntax.Pattern) : Map<string, Type> =
    let mutable result = Map.empty

    let visitor =
      { ExprVisitor.VisitExpr =
          fun (expr, state) ->
            match expr.Kind with
            | ExprKind.Function _ -> (false, state)
            | _ -> (true, state)
        ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
        ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
        ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
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
                | Syntax.ObjPatElem.ShorthandPat { Name = name
                                                   Inferred = inferred } ->
                  match inferred with
                  | Some t -> result <- Map.add name t result
                  | None -> ()
                | _ -> ()

              (true, state) // visit rest RestPats and KeyValuePats
            | _ -> (true, state)
        ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

    ExprVisitor.walkPattern visitor () pattern

    result

  let getKey
    (ident: QDeclIdent)
    (name: string)
    : QualifiedGraph.QualifiedIdent =
    let key: QualifiedGraph.QualifiedIdent =
      match ident with
      | QDeclIdent.Type { Filename = filename; Parts = parts } ->
        { Filename = filename
          Parts = List.take (parts.Length - 1) parts @ [ name ] }
      | QDeclIdent.Value { Filename = filename; Parts = parts } ->
        { Filename = filename
          Parts = List.take (parts.Length - 1) parts @ [ name ] }

    key

  // Infers a placeholder scheme from a type declaration
  // It has the proper type params but the type definition itself is a
  // fresh type variable.
  let inferTypeDeclPlaceholderScheme
    (ctx: Ctx)
    (env: Env)
    (typeParams: option<list<Syntax.TypeParam>>)
    : Result<Scheme, TypeError> =

    result {
      let typeParams =
        typeParams
        |> Option.map (fun typeParams ->
          List.map
            (fun (typeParam: Syntax.TypeParam) ->
              // The fresh type variables here eventually get unified
              // with the real types for the Constraint and Default when
              // we unify them later.
              let c =
                match typeParam.Constraint with
                | Some(c) -> Some(ctx.FreshTypeVar None None)
                | None -> None

              let d =
                match typeParam.Default with
                | Some(d) -> Some(ctx.FreshTypeVar None None)
                | None -> None

              { Name = typeParam.Name
                Constraint = c
                Default = d })
            typeParams)

      let scheme =
        { TypeParams = typeParams
          Type = ctx.FreshTypeVar None None }

      return scheme
    }

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
        if not (graph.Nodes.ContainsKey ident) then
          // TODO: make sure that the name space exists when determine if a decl
          // is qualified with a namespace or not.
          // printfn "ident = %A" ident
          return! Error(TypeError.SemanticError "Missing node in graph")

        let declsOrImports = graph.Nodes[ident]

        for declOrImport in declsOrImports do
          match declOrImport with
          | DeclOrImport.Decl decl ->
            match decl.Kind with
            | VarDecl { Export = export
                        Declare = declare
                        Pattern = pattern
                        Init = init
                        TypeAnn = typeAnn } ->
              // QUESTION: Should we check the type annotation as we're generating
              // the placeholder type?
              let! bindings, patternType = inferPattern ctx env pattern

              match typeAnn, init with
              | None, Some init ->
                let placeholderType = ctx.FreshTypeVar None None
                do! unify ctx env None patternType placeholderType
              | _, _ -> ()

              for KeyValue(name, binding) in bindings do
                let binding = { binding with Export = export }
                qns <- qns.AddValue (getKey ident name) binding
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

              let binding =
                { Type = ctx.FreshTypeVar None None
                  Mutable = false
                  Export = false }

              qns <- qns.AddValue (getKey ident name) binding
            | ClassDecl({ Name = name
                          Class = { TypeParams = typeParams } } as decl) ->
              let key = getKey ident name

              if not (qns.Schemes.ContainsKey key) then
                // TODO: treat ClassDecl similar to object types where we create a
                // structural placeholder type instead of an opaque type variable.
                // We should do this for both instance members and statics.
                let! instance =
                  inferTypeDeclPlaceholderScheme ctx env typeParams

                let statics: Type = ctx.FreshTypeVar None None

                qns <- qns.AddScheme key instance

                let binding =
                  { Type = statics
                    Mutable = false
                    Export = false }

                qns <- qns.AddValue key binding
            | EnumDecl { Variants = variants
                         Name = name
                         TypeParams = typeParams } ->

              let key = getKey ident name

              if not (qns.Schemes.ContainsKey key) then

                let! scheme = inferTypeDeclPlaceholderScheme ctx env typeParams

                qns <- qns.AddScheme key scheme

                let binding =
                  { Type = ctx.FreshTypeVar None None
                    Mutable = false
                    Export = false }

                qns <- qns.AddValue key binding
            | TypeDecl { TypeParams = typeParams; Name = name } ->
              // TODO: check to make sure we aren't redefining an existing type
              // TODO: replace placeholders, with a reference the actual definition
              // once we've inferred the definition
              let! placeholder =
                inferTypeDeclPlaceholderScheme ctx env typeParams

              qns <- qns.AddScheme (getKey ident name) placeholder
            | InterfaceDecl({ Name = name
                              TypeParams = typeParams
                              Extends = extends } as decl) ->
              let key = getKey ident name

              let parts =
                match ident with
                | QDeclIdent.Type { Parts = parts } -> parts
                | QDeclIdent.Value { Parts = parts } -> parts

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
                  | None -> inferTypeDeclPlaceholderScheme ctx env typeParams

              qns <- qns.AddScheme key placeholder
            | NamespaceDecl nsDecl ->
              return!
                Error(
                  TypeError.NotImplemented
                    "TODO: inferDeclPlaceholders - NamespaceDecl"
                )
          | DeclOrImport.Import import ->
            failwith "TODO: inferDeclPlaceholders - Import"

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
    (graph: QGraph)
    : Result<QualifiedNamespace, TypeError> =

    result {
      let mutable qns = qns

      // There are separate identifiers for the instance (type) and statics
      // (value).  This set is used to avoid inferring classes more than once.
      let mutable inferredClasses = Set.empty
      let mutable inferredEnums = Set.empty

      for ident in idents do
        let declsOrImports = graph.Nodes[ident]

        // TODO: check if we're inside a namespace and update the env accordingly
        let parts =
          match ident with
          | QDeclIdent.Type { Parts = parts } -> parts
          | QDeclIdent.Value { Parts = parts } -> parts

        let namespaces = List.take (List.length parts - 1) parts
        let name = List.last parts

        let mutable newEnv = openNamespaces env namespaces

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

        for declOrImport in declsOrImports do
          match declOrImport with
          | DeclOrImport.Decl decl ->
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
          | DeclOrImport.Import import ->
            failwith "TODO: inferDeclDefinitions - Import"

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
              TypeError.SemanticError
                "more than one kind of decl found for ident"
            )

        match varDecls with
        | [] -> ()
        | [ varDecl ] ->
          let placeholderTypes = getAllBindingPatterns varDecl.Pattern

          // NOTE: We explicitly don't generalize here because we want other
          // declarations to be able to unify with any free type variables
          // from this declaration.  We generalize things in `inferModule` and
          // `inferTreeRec`.
          let! newBindings, newSchemes = inferVarDecl ctx newEnv varDecl

          let inferredTypes = getAllBindingPatterns varDecl.Pattern

          for KeyValue(name, inferredType) in inferredTypes do
            let placeholderType =
              match Map.tryFind name placeholderTypes with
              | Some t -> t
              | None -> failwith "Missing placeholder type"

            do! unify ctx newEnv None placeholderType inferredType

          // Schemes can be generated for things like class expressions, e.g.
          // let Foo = class { ... }
          for KeyValue(name, scheme) in newSchemes do
            qns <- qns.AddScheme (getKey ident name) scheme
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
                      InferExpr.inferFunction ctx newEnv fnDecl.Sig body
                    | true, None ->
                      InferExpr.inferFuncSig ctx newEnv fnDecl.Sig None
                    | _, _ ->
                      Result.Error(
                        TypeError.SemanticError "Invalid function declaration"
                      )

                  fnDecl.InferredFunction <- Some(f)

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

          let placeholderType =
            match newEnv.TryFindValue name with
            | Some binding -> binding.Type
            | None -> failwith "Missing placeholder type"

          // NOTE: We explicitly don't generalize here because we want other
          // declarations to be able to unify with any free type variables
          // from this declaration.  We generalize things in `inferModule` and
          // `inferTreeRec`.
          do! unify ctx newEnv None placeholderType inferredType

        // TODO: handle remaining decl kind lists
        match classDecls with
        | [] -> ()
        | [ classDecl ] ->
          let { Declare = declare
                Name = name
                Class = cls } =
            classDecl

          let key = getKey ident name

          if not (inferredClasses.Contains key) then
            let! inferredType, inferredScheme =
              inferClass ctx newEnv cls declare

            let placeholderScheme = qns.Schemes[key]
            let placeholderBinding = qns.Values[key]

            do! unify ctx newEnv None placeholderScheme.Type inferredScheme.Type
            do! unify ctx newEnv None placeholderBinding.Type inferredType

            inferredClasses <- inferredClasses.Add key
        | _ ->
          return!
            Error(TypeError.SemanticError "More than one class decl for ident")

        match enumDecls with
        | [] -> ()
        | [ enumDecl ] ->
          let { Name = name
                Variants = variants
                TypeParams = typeParams } =
            enumDecl

          let key = getKey ident name

          // Prevents inferring the enum more than once
          if not (inferredEnums.Contains key) then
            let placeholderScheme = qns.Schemes[key]

            let { Name = name
                  TypeParams = typeParams
                  Variants = variants } =
              enumDecl

            // TODO: do the same thing we do for type decls
            match placeholderScheme.TypeParams with
            | None -> ()
            | Some typeParams ->
              for typeParam in typeParams do
                let unknown =
                  { Kind = TypeKind.Keyword Keyword.Unknown
                    Provenance = None }

                newEnv <-
                  newEnv.AddScheme
                    typeParam.Name
                    { TypeParams = None; Type = unknown }

            let mutable variantTypes = []
            let mutable variantTags = []

            for variant in variants do
              if variant.Init.IsSome then
                failwith "TODO: enum variants where .Init.IsSome is true"

              let tag = ctx.FreshSymbol()

              variantTags <-
                ObjTypeElem.Property
                  { Name = PropName.String variant.Name
                    Optional = false
                    Readonly = true
                    Type = tag }
                :: variantTags

              let elems =
                [ ObjTypeElem.Property
                    { Name = PropName.String "__TAG__"
                      Optional = false
                      Readonly = true
                      Type = tag } ]

              let tagType =
                { Kind =
                    TypeKind.Object
                      { Elems = elems
                        Exact = true
                        Immutable = false
                        Extends = None
                        Implements = None
                        Mutable = false
                        Interface = false
                        Nominal = false }
                  Provenance = None }

              let! variantType =
                result {
                  match variant.TypeAnn with
                  | Some typeAnn ->
                    let! inferredType = inferTypeAnn ctx newEnv typeAnn

                    return
                      { Kind = TypeKind.Intersection [ tagType; inferredType ]
                        Provenance = None }
                  | None -> return tagType
                }

              variantTypes <- variantType :: variantTypes

            let unionType = List.rev variantTypes |> union

            let inferredType =
              { Kind =
                  TypeKind.Object
                    { Elems = variantTags
                      Exact = true
                      Immutable = false
                      Extends = None
                      Implements = None
                      Mutable = false
                      Interface = false
                      Nominal = false }
                Provenance = None }

            let placeholderScheme = qns.Schemes[key]
            let placeholderBinding = qns.Values[key]

            placeholderScheme.Type <- unionType
            do! unify ctx newEnv None placeholderBinding.Type inferredType

            // avoid inferring the enum more than once
            inferredEnums <- inferredEnums.Add key
        | _ -> return! Error(TypeError.SemanticError "More than one enum decl")

        match typeDecls with
        | [] -> ()
        | [ typeDecl ] ->
          let { Name = name
                TypeAnn = typeAnn
                TypeParams = typeParams } =
            typeDecl

          let key = getKey ident name
          let placeholder = qns.Schemes[key]

          // TODO: when computing the decl graph, include self-recursive types in
          // the deps set so that we don't have to special case this here.
          // Handles self-recursive types
          newEnv <- newEnv.AddScheme name placeholder
          let getType = fun env -> InferTypeAnn.inferTypeAnn ctx env typeAnn

          let! scheme =
            InferExpr.inferTypeDeclDefn
              ctx
              newEnv
              placeholder
              typeParams
              getType

          match placeholder.TypeParams, scheme.TypeParams with
          | Some typeParams1, Some typeParams2 ->
            for typeParam1, typeParam2 in List.zip typeParams1 typeParams2 do
              match typeParam1.Constraint, typeParam2.Constraint with
              | Some c1, Some c2 -> do! unify ctx newEnv None c1 c2
              | None, None -> ()
              | _, _ ->
                return!
                  Error(
                    TypeError.SemanticError
                      "One scheme has a constraint type while the other doesn't"
                  )

              match typeParam1.Default, typeParam2.Default with
              | Some d1, Some d2 -> do! unify ctx newEnv None d1 d2
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
            let { InterfaceDecl.Name = name
                  TypeParams = typeParams
                  Extends = extends
                  Elems = elems } =
              interfaceDecl

            let key = getKey ident name
            let placeholder = qns.Schemes[key]

            // TODO: when computing the decl graph, include self-recursive types in
            // the deps set so that we don't have to special case this here.
            // Handles self-recursive types
            let newEnv = newEnv.AddScheme name placeholder

            let getType (env: Env) : Result<Type, TypeError> =
              result {
                let! elems =
                  List.traverseResultM
                    (InferTypeAnn.inferObjTypeAnnElem ctx env)
                    elems

                let! extends =
                  match extends with
                  | Some typeRefs ->
                    result {

                      let! extends =
                        List.traverseResultM
                          (InferTypeAnn.inferTypeRef ctx env)
                          typeRefs

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
                      Exact = false
                      Immutable = false
                      Mutable = false
                      Interface = true
                      Nominal = false }

                return { Kind = kind; Provenance = None }
              }

            let! newScheme =
              InferExpr.inferTypeDeclDefn
                ctx
                newEnv
                placeholder
                typeParams
                getType

            match placeholder.Type.Kind, newScheme.Type.Kind with
            | TypeKind.Object({ Elems = existingElems } as placeholderKind),
              TypeKind.Object { Elems = newElems } ->
              // TODO: remove duplicates
              // TODO: track which TypeScript interface decls each of the properties
              // come from in the merged type.
              let mergedElems = existingElems @ newElems

              // The reason why we mutate the kind's elems is so that if a TypeRef's
              // Schema field has already been set, we'll be able to access any
              // new fields in the merged interface type.
              placeholderKind.Elems <- mergedElems
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
  let findStronglyConnectedComponents (graph: QGraph) : list<list<QDeclIdent>> =

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

  let buildComponentTree
    (graph: QGraph)
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

  let addBinding
    (env: Env)
    (ident: QualifiedGraph.QualifiedIdent)
    (binding: Binding)
    : Env =

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

  let addScheme
    (env: Env)
    (ident: QualifiedGraph.QualifiedIdent)
    (scheme: Scheme)
    : Env =

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
    (bindings: Map<QualifiedGraph.QualifiedIdent, Binding>)
    : Map<QualifiedGraph.QualifiedIdent, Binding> =
    let mutable newBindings = Map.empty

    for KeyValue(name, binding) in bindings do
      let binding =
        { binding with
            Type = generalizeType binding.Type }

      newBindings <- newBindings.Add(name, binding)

    newBindings

  let inferTree
    (ctx: Ctx)
    (env: Env)
    (shouldGeneralize: bool)
    (graph: QGraph)
    (tree: QCompTree)
    : Result<Env, TypeError> =

    result {
      let mutable newEnv = env
      let entryPoints = findEntryPoints tree

      let mutable processed: Map<Set<QDeclIdent>, QualifiedNamespace> =
        Map.empty

      let rec inferTreeRec
        (ctx: Ctx)
        (env: Env)
        (root: Set<QDeclIdent>)
        (graph: QGraph)
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
              if shouldGeneralize then
                { newPartialQns with
                    Values = generalizeBindings newPartialQns.Values }
              else
                newPartialQns

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
    (graph: QGraph)
    : Result<Env, TypeError> =
    result {
      // TODO: handle imports

      let components = findStronglyConnectedComponents graph
      let tree = buildComponentTree graph components
      let! newEnv = inferTree ctx env true graph tree

      return newEnv
    }

  let inferStmt (ctx: Ctx) (newEnv: Env) (stmt: Stmt) =
    result {
      match stmt.Kind with
      | StmtKind.Expr expr ->
        let! _ = inferExpr ctx newEnv None expr
        ()
      | StmtKind.For { Left = pattern
                       Right = right
                       Body = body } ->
        let mutable blockEnv = newEnv

        let! patBindings, patType = inferPattern ctx blockEnv pattern
        let! rightType = inferExpr ctx blockEnv None right

        let symbol =
          match newEnv.TryFindValue "Symbol" with
          | Some binding -> binding.Type
          | None -> failwith "Symbol not in scope"

        let! symbolIterator =
          getPropType
            ctx
            blockEnv
            symbol
            (PropName.String "iterator")
            false
            ValueCategory.RValue

        // TODO: only lookup Symbol.iterator on Array for arrays and tuples
        let arrayScheme =
          match newEnv.TryFindScheme "Array" with
          | Some scheme -> scheme
          | None -> failwith "Array not in scope"

        let propName =
          match symbolIterator.Kind with
          | TypeKind.UniqueSymbol id -> PropName.Symbol id
          | _ -> failwith "Symbol.iterator is not a unique symbol"

        let! _ =
          getPropType
            ctx
            blockEnv
            arrayScheme.Type
            propName
            false
            ValueCategory.RValue

        // TODO: add a variant of `ExpandType` that allows us to specify a
        // predicate that can stop the expansion early.
        let! expandedRightType =
          expandType ctx blockEnv None Map.empty rightType true

        let! elemType =
          result {
            match expandedRightType.Kind with
            | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                                 TypeArgs = Some [ elem ] } -> return elem
            | TypeKind.Tuple { Elems = elems } -> return union elems
            | TypeKind.Object _ ->
              // TODO: try using unify and/or an utility type to extract the
              // value type from an iterator

              // TODO: add a `tryGetPropType` function that returns an option
              let! next =
                getPropType
                  ctx
                  blockEnv
                  rightType
                  (PropName.String "next")
                  false
                  ValueCategory.RValue

              match next.Kind with
              | TypeKind.Function f ->
                return!
                  getPropType
                    ctx
                    blockEnv
                    f.Return
                    (PropName.String "value")
                    false
                    ValueCategory.RValue
              | _ ->
                return!
                  Error(
                    TypeError.SemanticError $"{rightType} is not an iterator"
                  )
            | _ ->
              return!
                Error(
                  TypeError.NotImplemented
                    "TODO: for loop over non-iterable type"
                )
          }

        do! unify ctx blockEnv None elemType patType

        for KeyValue(name, binding) in patBindings do
          blockEnv <- newEnv.AddValue name binding

        let items = body.Stmts |> List.map ModuleItem.Stmt
        let! _ = ctx.InferModuleItems ctx blockEnv false items
        ()
      | StmtKind.Return expr ->
        match expr with
        | Some(expr) ->
          let! _ = inferExpr ctx newEnv None expr
          ()
        | None -> ()
      | StmtKind.Decl decl -> () // Already inferred by `inferTree`
    }

  let inferModuleItems
    (ctx: Ctx)
    (env: Env)
    (shouldGeneralize: bool)
    (items: List<ModuleItem>)
    =
    result {
      let stmts =
        items
        |> List.choose (fun (item: ModuleItem) ->
          match item with
          | ModuleItem.Stmt stmt -> Some stmt
          | _ -> None)

      let decls =
        stmts
        |> List.choose (fun (stmt: Stmt) ->
          match stmt.Kind with
          | StmtKind.Decl decl -> Some decl
          | _ -> None)

      let graph = buildGraph env decls
      let components = findStronglyConnectedComponents graph
      let tree = buildComponentTree graph components

      let! newEnv = inferTree ctx env shouldGeneralize graph tree

      for stmt in stmts do
        do! inferStmt ctx newEnv stmt

      return newEnv
    }


  let inferImport
    (ctx: Ctx)
    (env: Env)
    (import: Import)
    : Async<Result<Env, TypeError>> =

    asyncResult {
      let! exports = ctx.GetExports env.Filename import

      let mutable imports = Namespace.empty

      for specifier in import.Specifiers do
        match specifier with
        | ImportSpecifier.Named { Name = name; Alias = alias } ->
          let source = name

          let target =
            match alias with
            | Some(alias) -> alias
            | None -> source

          let valueLookup =
            match exports.Values.TryFind source with
            | Some(binding) ->
              imports <- imports.AddBinding target binding
              Ok(())
            | None -> Error("not found")

          let schemeLookup =
            match exports.Schemes.TryFind source with
            | Some(scheme) ->
              imports <- imports.AddScheme target scheme
              Ok(())
            | None -> Error("not found")

          let namespaceLookup =
            match exports.Namespaces.TryFind source with
            | Some(ns) ->
              imports <- imports.AddNamespace target ns
              Ok(())
            | None -> Error("not found")

          match valueLookup, schemeLookup, namespaceLookup with
          // If we can't find the symbol in either the values or schemes
          // we report an error
          | Error _, Error _, Error _ ->
            let resolvedPath = ctx.ResolvePath env.Filename import

            return!
              Error(
                TypeError.SemanticError
                  $"{resolvedPath} doesn't export '{name}'"
              )
          | _, _, _ -> ()
        | ModuleAlias { Alias = name } ->
          let ns: Namespace = { exports with Name = name }

          imports <- imports.AddNamespace name ns

      return
        { env with
            Namespace = env.Namespace.Merge imports }
    }

  let inferModule
    (ctx: Ctx)
    (env: Env)
    (ast: Module)
    : Async<Result<Env, TypeError>> =
    asyncResult {
      // TODO: update this function to accept a filename
      let mutable newEnv = env // { env with Filename = "input.esc" }

      // TODO: traverse the AST and flag all function params that don't have a
      // type annotation.
      // TODO: in order to allow type checking to proceed when there is a function
      // param without a type annotation, we can infer it as `never` or `unknown`

      let imports =
        List.choose
          (fun item ->
            match item with
            | Import import -> Some import
            | _ -> None)
          ast.Items

      for import in imports do
        let! importEnv = inferImport ctx newEnv import
        newEnv <- importEnv

      return! ctx.InferModuleItems ctx newEnv true ast.Items
    }

  let inferPackage (ctx: Ctx) (env: Env) (entry: Module) =

    failwith "TODO"
