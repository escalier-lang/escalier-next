namespace Escalier.Data

open Escalier.Data.Type

module Folder =
  type TypeFolder(folder: Type -> Type) =
    member this.FoldType(t: Type) =
      let t = this.WalkType(t)
      folder t

    member this.WalkType(t: Type) =
      let kind =
        match t.kind with
        | Array elem -> Array(this.FoldType elem)
        | TypeVar tv ->
          TypeVar
            { tv with
                instance = Option.map this.FoldType tv.instance
                bound = Option.map this.FoldType tv.bound }
        | TypeRef { name = name
                    type_args = typeArgs
                    scheme = scheme } ->
          let scheme =
            Option.map
              (fun (scheme: Scheme) ->
                { scheme with
                    type_ = this.FoldType scheme.type_
                    type_params =
                      List.map
                        (fun (tp: TypeParam) ->
                          { tp with
                              constraint_ =
                                Option.map this.FoldType tp.constraint_
                              default_ = Option.map this.FoldType tp.default_ })
                        scheme.type_params })
              scheme

          { name = name
            type_args = Option.map (List.map this.FoldType) typeArgs
            scheme = scheme }
          |> TypeRef
        | Literal _ -> t.kind // leaf node
        | Primitive _ -> t.kind // leaf node
        | Tuple types -> List.map this.FoldType types |> Tuple
        | Union types -> List.map this.FoldType types |> Union
        | Intersection types -> List.map this.FoldType types |> Intersection
        | Keyword _ -> t.kind // leaf node
        | Function f ->
          { param_list =
              List.map
                (fun fp ->
                  { fp with
                      type_ = this.FoldType fp.type_ })
                f.param_list
            return_type = this.FoldType f.return_type
            type_params =
              Option.map
                (List.map (fun tp ->
                  { tp with
                      constraint_ = Option.map this.FoldType tp.constraint_
                      default_ = Option.map this.FoldType tp.default_ }))
                f.type_params
            throws = this.FoldType f.throws }
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
        | Infer _ -> t.kind // leaf node
        | Wildcard -> t.kind // leaf node
        | Binary(left, op, right) ->
          (this.FoldType left, op, this.FoldType right) |> Binary

      // Question: should provenence be updated too?
      { t with Type.kind = kind }
