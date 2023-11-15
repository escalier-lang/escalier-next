namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.Data

open Errors

module Unify =
  let rec prune (t: Type) : Type =
    match t.Kind with
    | TypeVar({ Instance = Some(instance) } as v) ->
      let newInstance = prune instance in
      v.Instance <- Some(newInstance)
      newInstance
    | _ -> t

  let rec occursInType v t2 =
    let t2' = prune t2

    if t2' = v then
      true
    else
      match t2'.Kind with
      | TypeKind.TypeVar _ -> false // leaf node
      | TypeKind.TypeRef { Name = name
                           TypeArgs = typeArgs
                           Scheme = scheme } ->
        let typeArgs =
          Option.defaultValue [] typeArgs
          |> List.exists (fun t -> occursInType v t)

        let scheme =
          match scheme with
          | Some(scheme) -> occursInType v scheme.Type
          | None -> false

        typeArgs || scheme
      | TypeKind.Literal _ -> false // leaf node
      | TypeKind.Primitive _ -> false // leaf node
      | TypeKind.Tuple types -> occursIn v types
      | TypeKind.Array t -> occursInType v t
      | TypeKind.Union types -> occursIn v types
      | TypeKind.Intersection types -> occursIn v types
      | TypeKind.Keyword _ -> false // leaf node
      | TypeKind.Function func ->
        let paramTypes = List.map (fun p -> p.Type) func.ParamList

        (occursIn v paramTypes)
        || (occursInType v func.ReturnType)
        || (occursInType v func.Throws)
      | TypeKind.Object objTypeElems -> failwith "TODO: occursInType Object"
      | TypeKind.Rest t -> occursInType v t
      | TypeKind.KeyOf t -> occursInType v t
      | TypeKind.Index(target, index) -> failwith "TODO: occursInType Index"
      | TypeKind.Condition(check, extends, trueType, falseType) ->
        (occursInType v check)
        || (occursInType v extends)
        || (occursInType v trueType)
        || (occursInType v falseType)
      | TypeKind.Infer _ -> false // leaf node
      | TypeKind.Wildcard -> false // leaf node
      | TypeKind.Binary(left, _, right) ->
        (occursInType v left) || (occursInType v right)

  and occursIn t types =
    List.exists (fun t2 -> occursInType t t2) types

  let bind (t1: Type) (t2: Type) =
    result {
      if t1.Kind <> t2.Kind then
        if occursInType t1 t2 then
          return! Error(TypeError.RecursiveUnification)

        match t1.Kind with
        | TypeKind.TypeVar(v) ->
          v.Instance <- Some(t2)
          return ()
        | _ ->
          printfn "bind error"
          return! Error(TypeError.NotImplemented)
    }

  let rec unify (t1: Type) (t2: Type) : Result<unit, TypeError> =
    let t1 = prune t1
    let t2 = prune t2

    result {
      match t1.Kind, t2.Kind with
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
        let! fn1 = instantiateFunc fn1 None
        let! fn2 = instantiateFunc fn2 None

        // TODO: handle `self` params in methods
        // TODO: extract rest params

        let minParams1 = fn1.ParamList.Length
        let minParams2 = fn2.ParamList.Length

        if minParams1 > minParams2 then
          // t1 needs to have at least as many params as t2
          return! Error(TypeError.TypeMismatch(t1, t2))

        for i in 0 .. minParams1 - 1 do
          let p1 = fn1.ParamList.[i]
          let p2 = fn2.ParamList.[i]

          // NOTE: the order is reversed because fn1 has to accept
          // at least the same params as fn2 but can accept more.
          do! unify p2.Type p1.Type

        do! unify fn1.ReturnType fn2.ReturnType
        do! unify fn1.Throws fn2.Throws
      | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
    }

  and unifyCall
    (args: list<Expr>)
    (typeArgs: option<list<Type>>)
    (callee: Type)
    (inferExpr: Expr -> Result<Type, TypeError>)
    : Result<(Type * Type), TypeError> =

    result {
      let callee = prune callee

      let retType = TypeVariable.newTypeVar None
      let throwsType = TypeVariable.newTypeVar None

      match callee.Kind with
      | TypeKind.Function func ->
        return! unifyFuncCall args typeArgs retType throwsType func inferExpr
      | TypeKind.TypeVar _ ->

        // TODO: use a `result {}` CE here
        let! argTypes = List.traverseResultM inferExpr args

        let paramList =
          List.mapi
            (fun i t ->
              let p: Pattern = Pattern.Identifier $"arg{i}"

              { Pattern = p
                Type = t
                Optional = false })
            argTypes

        let callType =
          { Type.Kind =
              TypeKind.Function
                { ParamList = paramList
                  ReturnType = retType
                  Throws = throwsType
                  TypeParams = None } // TODO
            Provenance = None }

        match bind callee callType with
        | Ok _ -> return (prune retType, prune throwsType)
        | Error e -> return! Error e
      | kind ->
        printfn "kind = %A" kind
        return! Error(TypeError.NotImplemented)
    }

  and unifyFuncCall
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
          if callee.TypeParams.IsSome then
            return! instantiateFunc callee typeArgs
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

      for ((arg, argType), param) in List.zip args callee.ParamList do
        if
          param.Optional && argType.Kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          // TODO: check_mutability of `arg`
          do! unify argType param.Type

      do! unify retType callee.ReturnType
      do! unify throwsType callee.Throws

      return (retType, throwsType)
    }

  and instantiateFunc
    (func: Function)
    (typeArgs: option<list<Type>>)
    : Result<Function, TypeError> =

    result {
      let mutable mapping: Map<string, Type> = Map.empty

      match func.TypeParams with
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
              mapping.Add(tp.Name, TypeVariable.newTypeVar tp.Constraint)
      | None -> ()

      let instantiate (t: Type) : Type =
        let t = prune t

        match t.Kind with
        | TypeKind.TypeRef { Name = name } ->
          match mapping.TryFind(name) with
          | Some(t) -> t // uses definition in mapping
          | None -> t // keeps TypeRef
        | _ -> t

      let folder = Folder.TypeFolder(instantiate)

      let returnType = folder.FoldType(func.ReturnType)
      let throwsType = folder.FoldType(func.Throws)

      let paramList =
        List.map
          (fun (fp: FuncParam) ->
            let t = folder.FoldType(fp.Type)
            { fp with Type = t })
          func.ParamList

      return
        { TypeParams = None
          ParamList = paramList
          ReturnType = returnType
          Throws = throwsType }
    }
