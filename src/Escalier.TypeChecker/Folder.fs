namespace Escalier.TypeChecker

open Escalier.Data.Type

open Prune

module Folder =
  let foldType (f: Type -> option<Type>) (t: Type) : Type =
    let rec fold (t: Type) : Type =
      let t = prune t

      let foldFn (fn: Function) : Function =
        { fn with
            ParamList =
              List.map
                (fun param -> { param with Type = fold param.Type })
                fn.ParamList
            // TODO: fold TypeParams
            Return = fold fn.Return }

      let foldObjElem (elem: ObjTypeElem) : ObjTypeElem =
        match elem with
        | Property p -> Property { p with Type = fold p.Type }
        | Method(name, fn) -> Method(name, foldFn fn)
        | Getter(name, fn) -> Getter(name, foldFn fn)
        | Setter(name, fn) -> Setter(name, foldFn fn)
        | Constructor fn -> Constructor(foldFn fn)
        | Callable fn -> Callable(foldFn fn)
        | Mapped mapped ->
          Mapped
            { mapped with
                TypeAnn = fold mapped.TypeAnn
                NameType = Option.map fold mapped.NameType }
        | RestSpread t -> RestSpread(fold t)

      let foldTypeRef (typeRef: TypeRef) : TypeRef =
        let { Name = name
              TypeArgs = typeArgs
              Scheme = scheme } =
          typeRef

        let typeArgs = Option.map (List.map fold) typeArgs

        // NOTE: We explicitly don't fold the scheme type here because
        // this can cause an infinite loop with generic rercusive types.
        // let scheme =
        //   Option.map
        //     (fun (scheme: Scheme) -> { scheme with Type = fold scheme.Type })
        //     scheme

        { Name = name
          TypeArgs = typeArgs
          Scheme = scheme }

      let t =
        match t.Kind with
        | TypeKind.TypeVar _ -> t
        | TypeKind.Primitive _ -> t
        | TypeKind.Keyword _ -> t
        | TypeKind.Function f ->
          { Kind = TypeKind.Function(foldFn f)
            Provenance = None }
        | TypeKind.Tuple { Elems = elems; Immutable = immutable } ->
          let elems = List.map fold elems

          { Kind = TypeKind.Tuple { Elems = elems; Immutable = immutable }
            Provenance = None }
        | TypeKind.TypeRef typeRef ->
          // NOTE: We explicitly don't fold the scheme type here because
          // this can cause an infinite loop with generic rercusive types.
          // let scheme =
          //   Option.map
          //     (fun (scheme: Scheme) -> { scheme with Type = fold scheme.Type })
          //     scheme

          { Kind = foldTypeRef typeRef |> TypeKind.TypeRef
            Provenance = None }
        | TypeKind.Literal _ -> t
        | TypeKind.Wildcard -> t
        | TypeKind.Object { Extends = extends
                            Implements = impls
                            Elems = elems
                            Exact = exact
                            Immutable = immutable
                            Interface = int } ->
          let elems = List.map foldObjElem elems

          let extends = extends |> Option.map (List.map foldTypeRef)
          let impls = impls |> Option.map (List.map foldTypeRef)

          { Kind =
              TypeKind.Object
                { Extends = extends
                  Implements = impls
                  Elems = elems
                  Exact = exact
                  Immutable = immutable
                  Interface = int }
            Provenance = None }
        | TypeKind.RestSpread t ->
          { Kind = TypeKind.RestSpread(fold t)
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
        | TypeKind.Unary(op, arg) ->
          { Kind = TypeKind.Unary(op, fold arg)
            Provenance = None }
        | TypeKind.UniqueNumber _ -> t
        | TypeKind.UniqueSymbol _ -> t
        | TypeKind.Range { Min = min; Max = max } ->
          { Kind = TypeKind.Range { Min = fold min; Max = fold max }
            Provenance = None }
        | TypeKind.TemplateLiteral { Exprs = exprs; Parts = parts } ->
          { Kind =
              TypeKind.TemplateLiteral
                { Exprs = List.map fold exprs
                  Parts = parts }
            Provenance = None }
        | _ -> failwith $"TODO: foldType - {t.Kind}"

      match f t with
      | Some(t) -> t
      | None -> t

    fold t
