namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data.Type
open Escalier.Data.Common
open Escalier.Data.Visitor

open Error
open Folder
open Prune
open Env

module Poly =

  let generalizeFunc (f: Function) : Function =
    // We replace all type variables that appear in a throws clause with
    // never. The only time `throws` should be generic is if it's been
    // explicitly specified.
    let replaceTypeVarsInThrows (t: Type) : Type =
      let folder t =
        match (prune t).Kind with
        // NOTE: If we get a type var after pruning it should, by definition,
        // not have an instance.
        | TypeKind.TypeVar tvar ->
          Some(
            { Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }
          )
        | _ -> None

      foldType folder f.Throws

    let updateAllFunctionTypes (t: Type) : Type =
      let folder t =
        match (prune t).Kind with
        | TypeKind.Function f ->
          let f =
            { f with
                Throws = replaceTypeVarsInThrows f.Throws }

          Some(
            { Kind = TypeKind.Function f
              Provenance = None }
          )
        | _ -> None

      foldType folder t

    let paramList =
      List.map
        (fun (p: FuncParam) ->
          { p with
              Type = updateAllFunctionTypes p.Type })
        f.ParamList

    let retType = updateAllFunctionTypes f.Return
    let throws = replaceTypeVarsInThrows f.Throws

    let mutable mapping: Map<int, string * option<Type>> = Map.empty
    let mutable nextId = 0

    let replaceTypeVarsWithTypeRefs (t: Type) : Type =
      let folder t =
        match (prune t).Kind with
        | TypeKind.TypeVar { Id = id; Bound = bound } ->
          match Map.tryFind id mapping with
          | Some(name, _) ->
            Some(
              { Kind =
                  TypeKind.TypeRef
                    { Name = QualifiedIdent.Ident name
                      TypeArgs = None
                      Mutable = false
                      Scheme = None }
                Provenance = None }
            )
          | None ->
            let tpName = 65 + nextId |> char |> string
            nextId <- nextId + 1
            mapping <- mapping |> Map.add id (tpName, bound)

            Some(
              { Kind =
                  TypeKind.TypeRef
                    { Name = QualifiedIdent.Ident tpName
                      TypeArgs = None
                      Mutable = false
                      Scheme = None }
                Provenance = None }
            )
        | _ -> None

      foldType folder t

    let paramList =
      List.map
        (fun (p: FuncParam) ->
          { p with
              Type = replaceTypeVarsWithTypeRefs p.Type })
        paramList

    let ret = replaceTypeVarsWithTypeRefs retType

    let values = mapping.Values |> List.ofSeq

    let mutable newTypeParams: list<TypeParam> =
      List.map
        (fun (name, bound) ->
          { Name = name
            Constraint = bound
            Default = None })
        values

    Option.iter
      (fun typeParams ->
        for param in typeParams do
          newTypeParams <- newTypeParams @ [ param ])
      f.TypeParams

    { TypeParams = if newTypeParams.IsEmpty then None else Some(newTypeParams)
      Self = f.Self
      ParamList = paramList
      Return = ret
      Throws = throws }

  let topoSort (edges: Map<string, Set<string>>) : list<string> =
    let rec topoSortRec
      (key: string)
      (visited: Set<string>)
      (sorted: list<string>)
      =
      if visited.Contains(key) then
        sorted
      else
        let deps =
          match Map.tryFind key edges with
          | Some deps -> deps
          | None -> Set.empty

        let mutable sorted = sorted

        for dep in deps do
          sorted <- topoSortRec dep (visited.Add(key)) sorted

        sorted @ [ key ]

    let deps = edges.Values |> Seq.collect id |> Set.ofSeq
    let roots = Set.difference (edges.Keys |> Set.ofSeq) deps

    let mutable sortedTypeParamNames = []

    for root in roots do
      sortedTypeParamNames <-
        sortedTypeParamNames @ topoSortRec root Set.empty []

    sortedTypeParamNames

  let buildTypeArgMapping
    (ctx: Ctx)
    (typeParams: list<TypeParam>)
    (typeArgs: option<list<Type>>)
    : Result<Map<string, Type>, TypeError> =
    let mutable mapping: Map<string, Type> = Map.empty

    // Replaces type variables with their corresponding type arguments.
    let folder t =
      match t.Kind with
      | TypeKind.TypeRef({ Name = QualifiedIdent.Ident name }) ->
        match Map.tryFind name mapping with
        | Some(tv) -> Some(tv)
        | None -> None
      | _ -> None

    result {
      let mutable edges: Map<string, Set<string>> = Map.empty
      let paramNames = typeParams |> List.map (fun (p: TypeParam) -> p.Name)

      for tp in typeParams do
        let key = tp.Name
        let mutable deps = Set.empty

        let visit t =
          match t.Kind with
          | TypeKind.TypeRef({ Name = QualifiedIdent.Ident name }) ->
            if List.contains name paramNames then
              deps <- deps.Add(name)
          | _ -> ()

        match tp.Constraint with
        | Some c -> TypeVisitor.walkType visit c
        | None -> ()

        match tp.Default with
        | Some d -> TypeVisitor.walkType visit d
        | None -> ()

        edges <- edges.Add(key, deps)

      // Maps type parameter names to the corresponding type parameters.
      let mutable typeParamMap: Map<string, TypeParam> = Map.empty

      for tp in typeParams do
        typeParamMap <- typeParamMap.Add(tp.Name, tp)

      let sortedTypeParamNames = topoSort edges

      match typeArgs with
      | Some(typeArgs) ->
        // TODO: Handle typeArgs.Length < typeParams.Length
        // by checking whether the type params which don't have a corresponding
        // type arg have a default value.
        if typeArgs.Length <> typeParams.Length then
          return! Error(TypeError.WrongNumberOfTypeArgs)

        for name in sortedTypeParamNames do
          let index =
            List.findIndex (fun (tp: TypeParam) -> tp.Name = name) typeParams

          let typeArg = typeArgs[index]

          mapping <- mapping.Add(name, typeArg)
      | None ->
        for name in sortedTypeParamNames do
          let typeParam = typeParamMap[name]

          let c = typeParam.Constraint |> Option.map (foldType folder)
          let d = typeParam.Default |> Option.map (foldType folder)

          mapping <- mapping.Add(name, ctx.FreshTypeVar c d)

      return mapping
    }

  let instantiateFunc
    (ctx: Ctx)
    (f: Function)
    (typeArgs: option<list<Type>>)
    : Result<Function, TypeError> =

    result {
      let! mapping =
        match f.TypeParams with
        | Some typeParams -> buildTypeArgMapping ctx typeParams typeArgs
        | None -> Result.Ok Map.empty

      let folder t =
        match t.Kind with
        | TypeKind.TypeRef({ Name = QualifiedIdent.Ident name }) ->
          match Map.tryFind name mapping with
          | Some(tv) -> Some(tv)
          | None -> None
        | _ -> None

      return
        { TypeParams = None
          Self = f.Self
          ParamList =
            List.map
              (fun param ->
                { param with
                    Type = foldType folder param.Type })
              f.ParamList
          Return = foldType folder f.Return
          Throws = foldType folder f.Throws }
    }

  let instantiateType
    (ctx: Ctx)
    (t: Type)
    (typeParams: option<list<TypeParam>>)
    (typeArgs: option<list<Type>>)
    : Result<Type, TypeError> =

    result {
      let mutable mapping: Map<string, Type> = Map.empty

      let folder t =
        match t.Kind with
        | TypeKind.TypeRef({ Name = QualifiedIdent.Ident name }) ->
          match Map.tryFind name mapping with
          | Some(tv) -> Some(tv)
          | None -> None
        | _ -> None

      match typeParams with
      | Some(typeParams) ->
        match typeArgs with
        | Some(typeArgs) ->
          if typeArgs.Length <> typeParams.Length then
            return! Error(TypeError.WrongNumberOfTypeArgs)

          for tp, ta in List.zip typeParams typeArgs do
            mapping <- mapping.Add(tp.Name, ta)
        | None ->
          for tp in typeParams do
            mapping <-
              mapping.Add(tp.Name, ctx.FreshTypeVar tp.Constraint tp.Default)
      | None -> ()

      return foldType folder t
    }
