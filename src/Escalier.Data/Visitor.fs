namespace Escalier.Data

open Escalier.Data.Type

module Visitor =
  let rec walk_type (v: Type -> unit) (t: Type) : unit =
    v t

    match t.kind with
    | Array elem -> walk_type v elem
    | TypeVar tv ->
      maybe_walk_type v tv.instance |> ignore
      maybe_walk_type v tv.bound |> ignore
    | TypeRef(_, typeArgs, scheme) ->
      Option.map (List.iter (walk_type v)) typeArgs |> ignore

      Option.map
        (fun (scheme: Scheme) ->
          walk_type v scheme.type_
          walk_type_params v scheme.type_params)
        scheme
      |> ignore
    | Literal _ -> () // leaf node
    | Primitive _ -> () // leaf node
    | Tuple types -> List.iter (walk_type v) types
    | Union types -> List.iter (walk_type v) types
    | Intersection types -> List.iter (walk_type v) types
    | Keyword _ -> () // leaf node
    | Function f ->
      List.iter (walk_type v) (List.map (fun p -> p.type_) f.param_list)
    | Object objTypeElems ->
      for elem in objTypeElems do
        match elem with
        | Callable(callable) ->
          List.iter
            (walk_type v)
            (List.map (fun p -> p.type_) callable.param_list)

          walk_type v callable.return_type
        | _ -> ()
    | Rest t -> walk_type v t
    | KeyOf t -> walk_type v t
    | Index(target, index) ->
      walk_type v target
      walk_type v index
    | Condition(check, extends, trueType, falseType) ->
      walk_type v check
      walk_type v extends
      walk_type v trueType
      walk_type v falseType
    | Infer _ -> () // leaf node
    | Wildcard -> () // leaf node
    | Binary(left, op, right) ->
      walk_type v left
      walk_type v right

  and maybe_walk_type (v: Type -> unit) (ot: option<Type>) : unit =
    Option.map (walk_type v) ot |> ignore

  and walk_func (v: Type -> unit) (f: Function) : unit =
    List.iter (walk_type v) (List.map (fun p -> p.type_) f.param_list)
    walk_type v f.return_type
    walk_type v f.throws
    Option.map (walk_type_params v) f.type_params |> ignore

  and walk_type_params (v: Type -> unit) (tp: list<TypeParam>) : unit =
    List.iter
      (fun (tp: TypeParam) ->
        maybe_walk_type v tp.bound
        maybe_walk_type v tp.default_)
      tp
