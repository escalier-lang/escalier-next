namespace Escalier.TypeChecker

open Escalier.Data.Type

open Prune

module rec Folder =

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
            Provenance = None }
        | TypeKind.Tuple { Elems = elems; Immutable = immutable } ->
          let elems = List.map fold elems

          { Kind = TypeKind.Tuple { Elems = elems; Immutable = immutable }
            Provenance = None }
        | TypeKind.TypeRef({ Name = name
                             TypeArgs = typeArgs
                             Scheme = scheme }) ->
          let typeArgs = Option.map (List.map fold) typeArgs

          // NOTE: We explicitly do not fold the scheme here, because
          //       we want to preserve the original type alias definition.

          { Kind =
              TypeKind.TypeRef(
                { Name = name
                  TypeArgs = typeArgs
                  Scheme = scheme }
              )
            Provenance = None }
        | TypeKind.Literal _ -> t
        | TypeKind.Wildcard -> t
        | TypeKind.Object { Elems = elems; Immutable = immutable } ->
          let elems =
            List.map
              (fun elem ->
                match elem with
                | Property p -> Property { p with Type = fold p.Type }
                | Mapped m ->
                  Mapped
                    { m with
                        TypeParam =
                          { m.TypeParam with
                              Constraint = fold m.TypeParam.Constraint }
                        NameType = Option.map fold m.NameType
                        TypeAnn = fold m.TypeAnn }
                | _ -> failwith "TODO: foldType - ObjTypeElem")
              elems

          { Kind = TypeKind.Object { Elems = elems; Immutable = immutable }
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
        | TypeKind.Array { Elem = elem; Length = length } ->
          { Kind =
              TypeKind.Array
                { Elem = fold elem
                  Length = fold length }
            Provenance = None }
        | TypeKind.KeyOf t ->
          { Kind = TypeKind.KeyOf(fold t)
            Provenance = None }
        | TypeKind.Index(target, index) ->
          { Kind = TypeKind.Index(fold target, fold index)
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
            Provenance = None }
        | TypeKind.Infer _ -> t
        | TypeKind.Binary(left, op, right) ->
          { Kind = TypeKind.Binary(fold left, op, fold right)
            Provenance = None }
        | TypeKind.Range { Min = min; Max = max } ->
          { Kind = TypeKind.Range { Min = fold min; Max = fold max }
            Provenance = None }

      match f t with
      | Some(t) -> t
      | None -> t

    fold t
