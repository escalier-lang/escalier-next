namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data.Type

open Error
open TypeVariable

module Poly =

  let foldType (f: Type -> option<Type>) (t: Type) : Type =
    let rec fold (t: Type) : Type =
      let t = prune t

      let t =
        match t.Kind with
        | TypeKind.TypeVar _ -> t
        | TypeKind.Function f ->
          { Kind =
              TypeKind.Function
                { f with
                    ParamList =
                      List.map
                        (fun param -> { param with Type = fold param.Type })
                        f.ParamList
                    // TODO: fold TypeParams
                    Return = fold f.Return }
            Provenance = None }
        | TypeKind.Tuple(elems) ->
          let elems = List.map fold elems

          { Kind = TypeKind.Tuple(elems)
            Provenance = None }
        | TypeKind.TypeRef({ Name = name
                             TypeArgs = typeArgs
                             Scheme = scheme }) ->
          let typeArgs = Option.map (List.map fold) typeArgs

          let scheme =
            Option.map
              (fun (scheme: Scheme) -> { scheme with Type = fold scheme.Type })
              scheme

          { Kind =
              TypeKind.TypeRef(
                { Name = name
                  TypeArgs = typeArgs
                  Scheme = scheme }
              )
            Provenance = None }
        | TypeKind.Literal _ -> t
        | TypeKind.Wildcard -> t
        | TypeKind.Object elems ->
          let elems =
            List.map
              (fun elem ->
                match elem with
                | Property p -> Property { p with Type = fold p.Type }
                | _ -> failwith "TODO: foldType - ObjTypeElem")
              elems

          { Kind = TypeKind.Object(elems)
            Provenance = None }
        | TypeKind.Rest t ->
          { Kind = TypeKind.Rest(fold t)
            Provenance = None }
        | TypeKind.Union types ->
          { Kind = TypeKind.Union(List.map fold types)
            Provenance = None }
        | TypeKind.Intersection types ->
          { Kind = TypeKind.Intersection(List.map fold types)
            Provenance = None }
        | TypeKind.Array t ->
          { Kind = TypeKind.Array(fold t)
            Provenance = None }
        | TypeKind.KeyOf t ->
          { Kind = TypeKind.KeyOf(fold t)
            Provenance = None }
        | TypeKind.Index(target, index) ->
          { Kind = TypeKind.Index(fold target, fold index)
            Provenance = None }
        | TypeKind.Condition(check, extends, trueType, falseType) ->
          { Kind =
              TypeKind.Condition(
                fold check,
                fold extends,
                fold trueType,
                fold falseType
              )
            Provenance = None }
        | TypeKind.Infer _ -> t
        | TypeKind.Binary(left, op, right) ->
          { Kind = TypeKind.Binary(fold left, op, fold right)
            Provenance = None }

      match f t with
      | Some(t) -> t
      | None -> t

    fold t

  let generalizeFunc (f: Function) : Function =
    let mutable mapping: Map<int, string> = Map.empty
    let mutable nextId = 0

    // QUESTION: should we call `prune` inside the folder as well?
    let folder t =
      match t.Kind with
      | TypeKind.TypeVar { Id = id } ->
        match Map.tryFind id mapping with
        | Some(name) ->
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
          mapping <- mapping |> Map.add id tpName

          Some(
            { Kind =
                TypeKind.TypeRef
                  { Name = tpName
                    TypeArgs = None
                    Scheme = None }
              Provenance = None }
          )
      | _ -> None

    let paramList =
      List.map
        (fun (p: FuncParam) -> { p with Type = foldType folder p.Type })
        f.ParamList

    let ret = foldType folder f.Return

    // TODO: find throws in the body
    let throws = foldType folder f.Throws

    let values = mapping.Values |> List.ofSeq

    let mutable newTypeParams: list<TypeParam> =
      List.map
        (fun name ->
          { Name = name
            Constraint = None
            Default = None })
        values

    Option.iter
      (fun typeParams ->
        for param in typeParams do
          newTypeParams <- newTypeParams @ [ param ])
      f.TypeParams

    { TypeParams = if newTypeParams.IsEmpty then None else Some(newTypeParams)
      ParamList = paramList
      Return = ret
      Throws = throws }

  let instantiateFunc
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
            mapping <-
              mapping.Add(tp.Name, TypeVariable.makeVariable tp.Constraint)
      | None -> ()

      return
        { TypeParams = None
          ParamList =
            List.map
              (fun param ->
                { param with
                    Type = foldType folder param.Type })
              f.ParamList
          Return = foldType folder f.Return
          Throws = foldType folder f.Throws }
    }
