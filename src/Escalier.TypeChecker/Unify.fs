namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Type

open Error
open Prune
open Env
open Poly

module rec Unify =

  // Checks that t1 is assignable to t2
  let unify
    (ctx: Ctx)
    (env: Env)
    (t1: Type)
    (t2: Type)
    : Result<unit, TypeError> =
    // printfn $"unify({t1}, {t2})"

    result {
      match (prune t1).Kind, (prune t2).Kind with
      | TypeKind.TypeVar _, _ -> do! bind ctx env unify t1 t2
      | _, TypeKind.TypeVar _ -> do! unify ctx env t2 t1
      | TypeKind.Primitive p1, TypeKind.Primitive p2 when p1 = p2 -> ()
      | _, TypeKind.Keyword Keyword.Unknown -> () // All types are assignable to `unknown`
      | TypeKind.Keyword Keyword.Never, _ -> () // `never` is assignable to all types
      | TypeKind.Tuple elems1, TypeKind.Tuple elems2 ->
        if List.length elems1 <> List.length elems2 then
          return! Error(TypeError.TypeMismatch(t1, t2))

        ignore (List.map2 (unify ctx env) elems1 elems2)
      | TypeKind.Array elemType1, TypeKind.Array elemType2 ->
        do! unify ctx env elemType1 elemType2
      | TypeKind.Tuple tupleElemTypes, TypeKind.Array arrayElemType ->
        do! unify ctx env (union tupleElemTypes) arrayElemType
      | TypeKind.Function(f1), TypeKind.Function(f2) ->
        // TODO: check if `f1` and `f2` have the same type params
        let! f1 = instantiateFunc ctx f1 None
        let! f2 = instantiateFunc ctx f2 None

        let paramList1 =
          List.map (fun (param: FuncParam) -> param.Type) f1.ParamList

        let paramList2 =
          List.map (fun (param: FuncParam) -> param.Type) f2.ParamList

        if paramList1.Length > paramList2.Length then
          // t1 needs to have at least as many params as t2
          return! Error(TypeError.TypeMismatch(t1, t2))

        for i in 0 .. paramList1.Length - 1 do
          let param1 = paramList1[i]
          let param2 = paramList2[i]
          do! unify ctx env param2 param1 // params are contravariant

        do! unify ctx env f1.Return f2.Return // returns are covariant
        do! unify ctx env f1.Throws f2.Throws // throws are covariant
      | TypeKind.TypeRef({ Name = name1; TypeArgs = types1 }),
        TypeKind.TypeRef({ Name = name2; TypeArgs = types2 }) when name1 = name2 ->

        // if (name1 <> name2) then
        //   return! Error(TypeError.TypeMismatch(t1, t2))

        match (types1, types2) with
        | None, None -> ()
        | Some(types1), Some(types2) ->
          if List.length types1 <> List.length types2 then
            return! Error(TypeError.TypeMismatch(t1, t2))

          ignore (List.map2 (unify ctx env) types1 types2)
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Literal lit, TypeKind.Primitive prim ->
        // TODO: check that `typeArgs` is `None`
        match lit, prim with
        | Literal.Number _, Primitive.Number -> ()
        | Literal.String _, Primitive.String -> ()
        | Literal.Boolean _, Primitive.Boolean -> ()
        // TODO: expand the type ref
        // TODO: making `number`, `string`, `boolean`, primitives
        // instead of type refs so that we don't have to have exceptions
        // those types everywhere
        | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Literal l1, TypeKind.Literal l2 ->
        match l1, l2 with
        | Literal.Number n1, Literal.Number n2 when n1 = n2 -> ()
        | Literal.String s1, Literal.String s2 when s1 = s2 -> ()
        | Literal.Boolean b1, Literal.Boolean b2 when b1 = b2 -> ()
        | Literal.Null, Literal.Null -> ()
        | Literal.Undefined, Literal.Undefined -> ()
        | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.UniqueSymbol id1, TypeKind.UniqueSymbol id2 when id1 = id2 ->
        ()
      | TypeKind.Object elems1, TypeKind.Object elems2 ->

        let namedProps1 =
          List.choose
            (fun (elem: ObjTypeElem) ->
              match elem with
              // TODO: handle methods, setters, and getters
              | Property p -> Some(p.Name, p)
              | _ -> None)
            elems1
          |> Map.ofList

        let namedProps2 =
          List.choose
            (fun (elem: ObjTypeElem) ->
              match elem with
              // TODO: handle methods, setters, and getters
              | Property p -> Some(p.Name, p)
              | _ -> None)
            elems2
          |> Map.ofList

        let undefined =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Provenance = None }

        for KeyValue(name, prop2) in namedProps2 do
          match namedProps1.TryFind name with
          | Some(prop1) ->
            let p1Type =
              match prop1.Optional with
              | true -> union [ prop1.Type; undefined ]
              | false -> prop1.Type

            let p2Type =
              match prop2.Optional with
              | true -> union [ prop2.Type; undefined ]
              | false -> prop2.Type

            do! unify ctx env p1Type p2Type
          | None ->
            if not prop2.Optional then
              return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Object allElems, TypeKind.Intersection types ->
        let mutable combinedElems = []
        let mutable restTypes = []

        for t in types do
          match t.Kind with
          | TypeKind.Object elems -> combinedElems <- combinedElems @ elems
          | TypeKind.Rest t -> restTypes <- t :: restTypes
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        let objType =
          { Kind = TypeKind.Object(combinedElems)
            Provenance = None }

        match restTypes with
        | [] -> do! unify ctx env t1 objType
        | [ restType ] ->
          let objElems, restElems =
            List.partition
              (fun (ae: ObjTypeElem) ->
                List.exists
                  (fun ce ->
                    match ae, ce with
                    | Property ap, Property cp -> ap.Name = cp.Name
                    | _ -> false)
                  combinedElems)
              allElems

          let newObjType =
            { Kind = TypeKind.Object(objElems)
              Provenance = None }

          do! unify ctx env newObjType objType

          let newRestType =
            { Kind = TypeKind.Object(restElems)
              Provenance = None }

          do! unify ctx env newRestType restType
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Intersection types, TypeKind.Object allElems ->
        let mutable combinedElems = []
        let mutable restTypes = []

        for t in types do
          match t.Kind with
          | TypeKind.Object elems -> combinedElems <- combinedElems @ elems
          | TypeKind.Rest t -> restTypes <- t :: restTypes
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        let objType =
          { Kind = TypeKind.Object(combinedElems)
            Provenance = None }

        match restTypes with
        | [] -> do! unify ctx env objType t2
        | [ restType ] ->
          let objElems, restElems =
            List.partition
              (fun (ae: ObjTypeElem) ->
                List.exists
                  (fun ce ->
                    match ae, ce with
                    | Property ap, Property cp -> ap.Name = cp.Name
                    | _ -> false)
                  combinedElems)
              allElems

          let newObjType =
            { Kind = TypeKind.Object(objElems)
              Provenance = None }

          do! unify ctx env objType newObjType

          let newRestType =
            { Kind = TypeKind.Object(restElems)
              Provenance = None }

          do! unify ctx env restType newRestType
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Union(types), _ ->
        let! _ = types |> List.traverseResultM (fun t -> unify ctx env t t2)

        return ()
      | _, TypeKind.Union(types) ->

        let unifier =
          List.tryFind (fun t -> unify ctx env t1 t |> Result.isOk) types

        match unifier with
        | Some _ -> return ()
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Binary(left, op, right), TypeKind.Primitive Primitive.Number when
        op = "+" || op = "-" || op = "*" || op = "/" || op = "%" || op = "**"
        ->
        return ()
      | TypeKind.Binary(left, op, right), TypeKind.Primitive Primitive.String when
        op = "++"
        ->
        return ()
      | _, _ ->
        let t1' = env.ExpandType (unify ctx) t1
        let t2' = env.ExpandType (unify ctx) t2

        if t1' <> t1 || t2' <> t2 then
          return! unify ctx env t1' t2'
        else
          return! Error(TypeError.TypeMismatch(t1, t2))
    }

  let unifyCall<'a>
    (ctx: Ctx)
    (env: Env)
    (inferExpr: Ctx -> Env -> 'a -> Result<Type, TypeError>)
    (args: list<'a>)
    (typeArgs: option<list<Type>>)
    (callee: Type)
    : Result<(Type * Type), TypeError> =

    result {
      let callee = prune callee

      match callee.Kind with
      | TypeKind.Function func ->
        return! unifyFuncCall ctx env inferExpr args typeArgs func
      | TypeKind.Intersection types ->
        let mutable result = None

        for t in types do
          match t.Kind with
          | TypeKind.Function func ->
            match unifyCall ctx env inferExpr args typeArgs t with
            | Result.Ok value ->
              printfn $"unifyCall: {t} -> {value}"
              result <- Some(value)
            | Result.Error _ -> ()
          | _ -> ()

        match result with
        | Some(value) -> return value
        | None ->
          return! Error(TypeError.NotImplemented $"kind = {callee.Kind}")
      | TypeKind.TypeVar _ ->

        // TODO: use a `result {}` CE here
        let! argTypes = List.traverseResultM (inferExpr ctx env) args

        let paramList =
          List.mapi
            (fun i t ->
              let p: Pattern = Pattern.Identifier $"arg{i}"

              { Pattern = p
                Type = t
                Optional = false })
            argTypes

        let retType = ctx.FreshTypeVar None
        let throwsType = ctx.FreshTypeVar None

        let callType =
          { Type.Kind =
              TypeKind.Function
                { ParamList = paramList
                  Return = retType
                  Throws = throwsType
                  TypeParams = None } // TODO
            Provenance = None }

        match bind ctx env unify callee callType with
        | Ok _ -> return (prune retType, prune throwsType)
        | Error e -> return! Error e
      | kind -> return! Error(TypeError.NotImplemented $"kind = {kind}")
    }

  let unifyFuncCall<'a>
    (ctx: Ctx)
    (env: Env)
    (inferExpr: Ctx -> Env -> 'a -> Result<Type, TypeError>)
    (args: list<'a>)
    (typeArgs: option<list<Type>>)
    (callee: Function)
    : Result<Type * Type, TypeError> =

    result {
      let! callee =
        result {
          if callee.TypeParams.IsSome then
            return! instantiateFunc ctx callee typeArgs
          else
            return callee
        }

      let! args =
        List.traverseResultM
          (fun arg ->
            result {
              let! argType = inferExpr ctx env arg
              return arg, argType
            })
          args

      if args.Length < callee.ParamList.Length then
        // TODO: make this into a diagnostic instead of an error
        return!
          Error(
            TypeError.SemanticError "function called with too few arguments"
          )

      // List.zip requires that both lists have the same length
      let args = List.take callee.ParamList.Length args

      for ((arg, argType), param) in List.zip args callee.ParamList do
        if
          param.Optional && argType.Kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          // TODO: collect errors and turn them into diagnostics
          // TODO: check_mutability of `arg`
          // contravariant
          match unify ctx env argType param.Type with
          | Ok _ -> ()
          | Error(reason) ->
            let never =
              { Kind = TypeKind.Keyword Keyword.Never
                Provenance = None }

            do! unify ctx env never param.Type

            ctx.AddDiagnostic(
              { Description =
                  $"arg type '{argType}' doesn't satisfy param '{param.Pattern}' type '{param.Type}' in function call"
                Reasons = [ reason ] }
            )

            ()

      // do! unify ctx env retType callee.Return // covariant
      // do! unify ctx env throwsType callee.Throws // covariant

      return (callee.Return, callee.Throws)
    }
