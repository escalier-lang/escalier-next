namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data.Type

open Prune
open Error
open Env

module Poly =

  let foldType (f: Type -> option<Type>) (t: Type) : Type =
    let rec fold (t: Type) : Type =
      let t = prune t

      let t =
        match t.Kind with
        | TypeKind.TypeVar _ -> t
        | TypeKind.Primitive _ -> t
        | TypeKind.Keyword _ -> t
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
            Mutable = false
            Provenance = None }
        | TypeKind.Tuple { Elems = elems; Immutable = immutable } ->
          let elems = List.map fold elems

          { Kind = TypeKind.Tuple { Elems = elems; Immutable = immutable }
            Mutable = false
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
            Mutable = false
            Provenance = None }
        | TypeKind.Literal _ -> t
        | TypeKind.Wildcard -> t
        | TypeKind.Object { Elems = elems; Immutable = immutable } ->
          let elems =
            List.map
              (fun elem ->
                match elem with
                | Property p -> Property { p with Type = fold p.Type }
                | _ -> failwith "TODO: foldType - ObjTypeElem")
              elems

          { Kind = TypeKind.Object { Elems = elems; Immutable = immutable }
            Mutable = false
            Provenance = None }
        | TypeKind.Rest t ->
          { Kind = TypeKind.Rest(fold t)
            Mutable = false
            Provenance = None }
        | TypeKind.Union types ->
          { Kind = TypeKind.Union(List.map fold types)
            Mutable = false
            Provenance = None }
        | TypeKind.Intersection types ->
          { Kind = TypeKind.Intersection(List.map fold types)
            Mutable = false
            Provenance = None }
        | TypeKind.Array { Elem = elem; Length = length } ->
          { Kind =
              TypeKind.Array
                { Elem = fold elem
                  Length = fold length }
            Mutable = false
            Provenance = None }
        | TypeKind.KeyOf t ->
          { Kind = TypeKind.KeyOf(fold t)
            Mutable = false
            Provenance = None }
        | TypeKind.Index(target, index) ->
          { Kind = TypeKind.Index(fold target, fold index)
            Mutable = false
            Provenance = None }
        | TypeKind.Condition { Check = check
                               Extends = extends
                               TrueType = trueType
                               FalseType = falseType } ->
          { Kind =
              TypeKind.Condition
                { Check = fold check
                  Extends = fold extends
                  TrueType = fold trueType
                  FalseType = fold falseType }
            Mutable = false
            Provenance = None }
        | TypeKind.Infer _ -> t
        | TypeKind.Binary(left, op, right) ->
          { Kind = TypeKind.Binary(fold left, op, fold right)
            Mutable = false
            Provenance = None }
        | TypeKind.UniqueNumber _ -> t
        | TypeKind.Range { Min = min; Max = max } ->
          { Kind = TypeKind.Range { Min = fold min; Max = fold max }
            Mutable = false
            Provenance = None }
        | TypeKind.Struct { TypeRef = typeRef; Elems = elems } ->
          let typeArgs = Option.map (List.map fold) typeRef.TypeArgs

          let elems =
            List.map
              (fun elem ->
                match elem with
                | Property p -> Property { p with Type = fold p.Type }
                | _ -> failwith "TODO: foldType - ObjTypeElem")
              elems

          { Kind =
              TypeKind.Struct
                { TypeRef = { typeRef with TypeArgs = typeArgs }
                  Elems = elems }
            Mutable = false
            Provenance = None }
        | _ -> failwith $"TODO: foldType - {t.Kind}"

      match f t with
      | Some(t) -> t
      | None -> t

    fold t

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
              Mutable = false
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
              Mutable = false
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
                Mutable = false
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
                Mutable = false
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
          ParamList =
            List.map
              (fun param ->
                { param with
                    Type = foldType folder param.Type })
              f.ParamList
          Return = foldType folder f.Return
          Throws = foldType folder f.Throws }
    }
