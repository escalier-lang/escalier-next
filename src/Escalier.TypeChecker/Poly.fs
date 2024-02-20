namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data.Type

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
        | TypeKind.TypeVar _ ->
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
                    { Name = name
                      TypeArgs = None
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
                    { Name = tpName
                      TypeArgs = None
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

  let instantiateFunc
    (ctx: Ctx)
    (f: Function)
    (typeArgs: option<list<Type>>)
    : Result<Function, TypeError> =

    result {
      let mutable mapping: Map<string, Type> = Map.empty

      let folder t =
        match t.Kind with
        | TypeKind.TypeRef({ Name = name }) ->
          match Map.tryFind name mapping with
          | Some(tv) -> Some(tv)
          | None -> None
        | _ -> None

      match f.TypeParams with
      | Some(typeParams) ->
        match typeArgs with
        | Some(typeArgs) ->
          if typeArgs.Length <> typeParams.Length then
            return! Error(TypeError.WrongNumberOfTypeArgs)

          for tp, ta in List.zip typeParams typeArgs do
            mapping <- mapping.Add(tp.Name, ta)
        | None ->
          for tp in typeParams do
            mapping <- mapping.Add(tp.Name, ctx.FreshTypeVar tp.Constraint)
      | None -> ()

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
        | TypeKind.TypeRef({ Name = name }) ->
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
            mapping <- mapping.Add(tp.Name, ctx.FreshTypeVar tp.Constraint)
      | None -> ()

      return foldType folder t
    }
