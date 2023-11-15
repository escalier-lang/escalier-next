namespace Escalier.Data

open Escalier.Data.Type

module Folder =
  type TypeFolder(folder: Type -> Type) =
    member this.FoldType(t: Type) =
      let t = this.WalkType(t)
      folder t

    member this.WalkType(t: Type) =
      let kind =
        match t.Kind with
        | Array elem -> Array(this.FoldType elem)
        | TypeVar tv ->
          TypeVar
            { tv with
                Instance = Option.map this.FoldType tv.Instance
                Bound = Option.map this.FoldType tv.Bound }
        | TypeRef { Name = name
                    TypeArgs = typeArgs
                    Scheme = scheme } ->
          let scheme =
            Option.map
              (fun (scheme: Scheme) ->
                { scheme with
                    Type = this.FoldType scheme.Type
                    TypeParams =
                      List.map
                        (fun (tp: TypeParam) ->
                          { tp with
                              Constraint =
                                Option.map this.FoldType tp.Constraint
                              Default = Option.map this.FoldType tp.Default })
                        scheme.TypeParams })
              scheme

          { Name = name
            TypeArgs = Option.map (List.map this.FoldType) typeArgs
            Scheme = scheme }
          |> TypeRef
        | Literal _ -> t.Kind // leaf node
        | Primitive _ -> t.Kind // leaf node
        | Tuple types -> List.map this.FoldType types |> Tuple
        | Union types -> List.map this.FoldType types |> Union
        | Intersection types -> List.map this.FoldType types |> Intersection
        | Keyword _ -> t.Kind // leaf node
        | Function f ->
          { ParamList =
              List.map
                (fun fp -> { fp with Type = this.FoldType fp.Type })
                f.ParamList
            ReturnType = this.FoldType f.ReturnType
            TypeParams =
              Option.map
                (List.map (fun tp ->
                  { tp with
                      Constraint = Option.map this.FoldType tp.Constraint
                      Default = Option.map this.FoldType tp.Default }))
                f.TypeParams
            Throws = this.FoldType f.Throws }
          |> Function
        | Object _objTypeElems -> failwith "not implemented"
        // for elem in objTypeElems do
        //   match elem with
        //   | Callable(callable) ->
        //     List.iter
        //       (walk_type v)
        //       (List.map (fun p -> p.type_) callable.param_list)
        //
        //     walk_type v callable.return_type
        //   | _ -> ()
        | Rest t -> this.FoldType t |> Rest
        | KeyOf t -> this.FoldType t |> KeyOf
        | Index(target, index) ->
          (this.FoldType target, this.FoldType index) |> Index
        | Condition(check, extends, trueType, falseType) ->
          (this.FoldType check,
           this.FoldType extends,
           this.FoldType trueType,
           this.FoldType falseType)
          |> Condition
        | Infer _ -> t.Kind // leaf node
        | Wildcard -> t.Kind // leaf node
        | Binary(left, op, right) ->
          (this.FoldType left, op, this.FoldType right) |> Binary

      // Question: should provenence be updated too?
      { t with Type.Kind = kind }
