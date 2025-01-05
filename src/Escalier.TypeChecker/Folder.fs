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
        | Method { Name = name; Fn = fn } ->
          Method { Name = name; Fn = foldFn fn }
        | Getter { Name = name; Fn = fn } ->
          Getter { Name = name; Fn = foldFn fn }
        | Setter { Name = name; Fn = fn } ->
          Setter { Name = name; Fn = foldFn fn }
        | Constructor fn -> Constructor(foldFn fn)
        | Callable fn -> Callable(foldFn fn)
        | Mapped mapped ->
          Mapped
            { mapped with
                TypeAnn = fold mapped.TypeAnn
                NameType = Option.map fold mapped.NameType }
        | RestSpread t -> RestSpread(fold t)

      let foldTypeRef (typeRef: TypeRef) : TypeRef =
        let typeArgs = Option.map (List.map fold) typeRef.TypeArgs

        // NOTE: We explicitly don't fold the scheme type here because
        // this can cause an infinite loop with generic rercusive types.
        // let scheme =
        //   Option.map
        //     (fun (scheme: Scheme) -> { scheme with Type = fold scheme.Type })
        //     scheme

        { typeRef with TypeArgs = typeArgs }

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
        | TypeKind.Array { Elem = elem } ->
          { Kind = TypeKind.Array { Elem = fold elem }
            Provenance = None }
        | TypeKind.KeyOf t ->
          { Kind = TypeKind.KeyOf(fold t)
            Provenance = None }
        | TypeKind.Index { Target = target; Index = index } ->
          { Kind =
              TypeKind.Index
                { Target = fold target
                  Index = fold index }
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
        | TypeKind.Binary { Op = op; Left = left; Right = right } ->
          { Kind =
              TypeKind.Binary
                { Op = op
                  Left = fold left
                  Right = fold right }
            Provenance = None }
        | TypeKind.Unary { Op = op; Arg = arg } ->
          { Kind = TypeKind.Unary { Op = op; Arg = fold arg }
            Provenance = None }
        | TypeKind.UniqueSymbol _ -> t
        | TypeKind.TemplateLiteral { Exprs = exprs; Parts = parts } ->
          { Kind =
              TypeKind.TemplateLiteral
                { Exprs = List.map fold exprs
                  Parts = parts }
            Provenance = None }
        | TypeKind.Namespace _ -> failwith "TODO: foldType - Namespace"
        | TypeKind.Typeof _ -> t
        | TypeKind.Intrinsic -> t
        | TypeKind.IntrinsicInstance { Name = name; TypeArgs = typeArgs } ->
          let typeArgs = Option.map (List.map fold) typeArgs

          { Kind =
              TypeKind.IntrinsicInstance { Name = name; TypeArgs = typeArgs }
            Provenance = None }

      match f t with
      | Some(t) -> t
      | None -> t

    fold t
