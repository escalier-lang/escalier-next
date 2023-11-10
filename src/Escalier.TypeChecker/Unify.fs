namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.TypeChecker.Errors
open Escalier.Data

module Unify =
  let rec prune (t: Type) : Type =
    match t.kind with
    | TypeVar({ instance = Some(instance) } as v) ->
      let new_instance = prune instance in
      v.instance <- Some(new_instance)
      new_instance
    | _ -> t

  let rec occurs_in_type v t2 =
    let t2' = prune t2

    if t2' = v then
      true
    else
      match t2'.kind with
      | TypeKind.TypeVar _ -> false // leaf node
      | TypeKind.TypeRef { name = name
                           type_args = typeArgs
                           scheme = scheme } ->
        let typeArgs =
          Option.defaultValue [] typeArgs
          |> List.exists (fun t -> occurs_in_type v t)

        let scheme =
          match scheme with
          | Some(scheme) -> occurs_in_type v scheme.type_
          | None -> false

        typeArgs || scheme
      | TypeKind.Literal _ -> false // leaf node
      | TypeKind.Primitive _ -> false // leaf node
      | TypeKind.Tuple types -> occurs_in v types
      | TypeKind.Array t -> occurs_in_type v t
      | TypeKind.Union types -> occurs_in v types
      | TypeKind.Intersection types -> occurs_in v types
      | TypeKind.Keyword _ -> false // leaf node
      | TypeKind.Function func ->
        let param_types = List.map (fun p -> p.type_) func.param_list

        (occurs_in v param_types)
        || (occurs_in_type v func.return_type)
        || (occurs_in_type v func.throws)
      | TypeKind.Object objTypeElems -> failwith "todo"
      | TypeKind.Rest t -> occurs_in_type v t
      | TypeKind.KeyOf t -> occurs_in_type v t
      | TypeKind.Index(target, index) -> failwith "todo"
      | TypeKind.Condition(check, extends, trueType, falseType) ->
        (occurs_in_type v check)
        || (occurs_in_type v extends)
        || (occurs_in_type v trueType)
        || (occurs_in_type v falseType)
      | TypeKind.Infer _ -> false // leaf node
      | TypeKind.Wildcard -> false // leaf node
      | TypeKind.Binary(left, _, right) ->
        (occurs_in_type v left) || (occurs_in_type v right)

  and occurs_in t types =
    List.exists (fun t2 -> occurs_in_type t t2) types

  let bind (t1: Type) (t2: Type) =
    result {
      if t1.kind <> t2.kind then
        if occurs_in_type t1 t2 then
          return! Error(TypeError.RecursiveUnification)

        match t1.kind with
        | TypeKind.TypeVar(v) ->
          v.instance <- Some(t2)
          return ()
        | _ -> return! Error(TypeError.NotImplemented)
    }

  let rec unify (t1: Type) (t2: Type) : Result<unit, TypeError> =
    let t1 = prune t1
    let t2 = prune t2

    result {
      match t1.kind, t2.kind with
      | TypeKind.TypeVar(v), _ -> do! bind t1 t2
      | _, TypeKind.TypeVar(v) -> do! bind t2 t1
      | TypeKind.Primitive(p1), TypeKind.Primitive(p2) ->
        match p1, p2 with
        | Primitive.Number, Primitive.Number -> return ()
        | Primitive.String, Primitive.String -> return ()
        | Primitive.Boolean, Primitive.Boolean -> return ()
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Literal(lit), TypeKind.Primitive(prim) ->
        match lit, prim with
        | Literal.Number _, Primitive.Number -> return ()
        | Literal.String _, Primitive.String -> return ()
        | Literal.Boolean _, Primitive.Boolean -> return ()
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Keyword(kw1), TypeKind.Keyword(kw2) ->
        if kw1 = kw2 then
          return ()
        else
          return! Error(TypeError.TypeMismatch(t1, t2))
      | _, TypeKind.Keyword(KeywordType.Unknown) -> return ()
      | TypeKind.Union(types), _ ->
        for t in types do
          do! unify t t2

        return ()
      | _, TypeKind.Union(types) ->
        // If t1 is a subtype of any of the types in the union, then it is a
        // subtype of the union.
        for t in types do
          if Result.isOk (unify t1 t) then return () else ()

        return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Tuple(tt1), TypeKind.Tuple(tt2) ->
        if tt1.Length < tt2.Length then
          // TODO: check for rest patterns
          return! Error(TypeMismatch(t1, t2))
        else
          ()

        for (p, q) in List.zip tt1 tt2 do
          // TODO: check for rest patterns
          do! unify p q
      | TypeKind.Tuple tt, TypeKind.Array at ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Array at, TypeKind.Tuple tt ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Rest rest, TypeKind.Array _ -> return! unify rest t2
      | TypeKind.Rest rest, TypeKind.Tuple _ -> return! unify rest t2
      | TypeKind.Array _, TypeKind.Rest rest -> return! unify t1 rest
      | TypeKind.Tuple _, TypeKind.Rest rest -> return! unify t1 rest
      | TypeKind.Object _, TypeKind.Object _ ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Intersection _, TypeKind.Object _ ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Object _, TypeKind.Intersection _ ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Function fn1, TypeKind.Function fn2 ->
        return! Error(TypeError.NotImplemented)
      | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
    }

  and unify_call
    (args: list<Expr>)
    (typeArgs: option<list<Type>>)
    (callee: Type)
    (inferExpr: Expr -> Result<Type, TypeError>)
    : Result<(Type * Type), TypeError> =

    let retType = TypeVariable.fresh None
    let throwsType = TypeVariable.fresh None

    match callee.kind with
    | TypeKind.Function func ->
      unify_func_call args typeArgs retType throwsType func inferExpr
    | _ -> failwith "todo"

  and unify_func_call
    (args: list<Expr>)
    (typeArgs: option<list<Type>>)
    (retType: Type)
    (throwsType: Type)
    (callee: Function)
    (inferExpr: Expr -> Result<Type, TypeError>)
    : Result<(Type * Type), TypeError> =

    result {
      let! callee =
        result {
          if callee.type_params.IsSome then
            return! instantiate_func callee typeArgs
          else
            return callee
        }

      let! args =
        List.traverseResultM
          (fun (arg: Expr) ->
            result {
              let! argType = inferExpr arg
              return arg, argType
            })
          args

      for ((arg, argType), param) in List.zip args callee.param_list do
        if
          param.optional && argType.kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          // TODO: check_mutability of `arg`
          do! unify argType param.type_

      do! unify retType callee.return_type
      do! unify throwsType callee.throws

      return (retType, throwsType)
    }

  and instantiate_func
    (func: Function)
    (typeArgs: option<list<Type>>)
    : Result<Function, TypeError> =

    result {
      let mutable mapping: Map<string, Type> = Map.empty

      match func.type_params with
      | Some(typeParams) ->
        match typeArgs with
        | Some(typeArgs) ->
          if typeArgs.Length <> typeParams.Length then
            return! Error(TypeError.WrongNumberOfTypeArgs)

          for tp, ta in List.zip typeParams typeArgs do
            mapping <- mapping.Add(tp.name, ta)
        | None ->
          for tp in typeParams do
            mapping <- mapping.Add(tp.name, TypeVariable.fresh tp.constraint_)
      | None -> ()

      let instantiate (t: Type) : Type =
        let t = prune t

        match t.kind with
        | TypeKind.TypeRef { name = name } ->
          match mapping.TryFind(name) with
          | Some(t) -> t // uses definition in mapping
          | None -> t // keeps TypeRef
        | _ -> t

      let folder = Folder.TypeFolder(instantiate)

      let returnType = folder.FoldType(func.return_type)
      let throwsType = folder.FoldType(func.throws)

      let paramList =
        List.map
          (fun (fp: FuncParam) ->
            let t = folder.FoldType(fp.type_)
            { fp with type_ = t })
          func.param_list

      return
        { func with
            type_params = None
            param_list = paramList
            return_type = returnType
            throws = throwsType }
    }
