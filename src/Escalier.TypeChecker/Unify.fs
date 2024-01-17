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
    // printfn $"IsPatternMatching = {env.IsPatternMatching}"

    result {
      match (prune t1).Kind, (prune t2).Kind with
      | TypeKind.TypeVar _, _ -> do! bind ctx env unify t1 t2
      | _, TypeKind.TypeVar _ -> do! unify ctx env t2 t1
      | TypeKind.Primitive p1, TypeKind.Primitive p2 when p1 = p2 -> ()
      | TypeKind.Wildcard, _ -> ()
      | _, TypeKind.Wildcard -> ()
      | _, TypeKind.Keyword Keyword.Unknown -> () // All types are assignable to `unknown`
      | TypeKind.Keyword Keyword.Never, _ -> () // `never` is assignable to all types
      | TypeKind.Tuple tuple1, TypeKind.Tuple tuple2 ->
        if not tuple1.Immutable && tuple2.Immutable then
          return! Error(TypeError.TypeMismatch(t1, t2))

        let elemTypes, restTypes =
          List.partition
            (fun (elem: Type) ->
              match elem.Kind with
              | TypeKind.Rest _ -> false
              | _ -> true)
            tuple2.Elems

        match restTypes with
        | [] ->
          // elems1 can have more elements than elems2 since it's a subtype
          if List.length tuple1.Elems < List.length elemTypes then
            return! Error(TypeError.TypeMismatch(t1, t2))

          // List.map2 only works if the lists have the same length so make
          // elems1 the same length as elemTypes (elems2)
          let elems1 = List.take elemTypes.Length tuple1.Elems
          List.map2 (unify ctx env) elems1 elemTypes |> ignore
        | [ { Kind = TypeKind.Rest t } ] ->
          // TODO: verify that the rest element comes last in the tuple

          let elems1, restElems1 = List.splitAt elemTypes.Length tuple1.Elems
          List.map2 (unify ctx env) elems1 elemTypes |> ignore

          let restTuple =
            { Kind = TypeKind.Tuple { tuple1 with Elems = restElems1 }
              Mutable = false
              Provenance = None }

          do! unify ctx env restTuple t
        | _ ->
          // Multiple rest elements in undeciable
          // TODO: create an Undecable error type
          return! Error(TypeError.SemanticError("Too many rest elements!"))
      | TypeKind.Array elemType1, TypeKind.Array elemType2 ->
        // TODO: unify the lengths of the arrays
        // An array whose length is `unique number` is a subtype of an array
        // whose length is `number`.
        do! unify ctx env elemType1.Elem elemType2.Elem
      | TypeKind.Tuple tuple, TypeKind.Array array ->
        // TODO: check if array.Elem is `unique number`
        // If it is, then we can't unify these since the tuple could be
        // longer or short than the array.
        // An array with length `number` represents arrays of all possible
        // length whereas a length of `unique number` represents a single array
        // of unknown length.

        let elemTypes, restTypes =
          List.partition
            (fun (elem: Type) ->
              match elem.Kind with
              | TypeKind.Rest _ -> false
              | _ -> true)
            tuple.Elems

        // TODO: check for `Rest` types in tupleElemTypes, if we find one at the
        // end of the tuple, we can unify the rest of the array with it.
        do! unify ctx env (union elemTypes) array.Elem

        let restTypes =
          List.map
            (fun (t: Type) ->
              match t.Kind with
              | TypeKind.Rest t -> t
              | _ -> t)
            restTypes

        for restType in restTypes do
          do! unify ctx env restType t2
      | TypeKind.Array array, TypeKind.Tuple tuple ->
        let elemTypes, restTypes =
          List.partition
            (fun (elem: Type) ->
              match elem.Kind with
              | TypeKind.Rest _ -> false
              | _ -> true)
            tuple.Elems

        match restTypes with
        | [] ->
          // if env.IsPatternMatching then
          //   for elemType in elemTypes do
          //     do! unify ctx env array.Elem elemType
          // else
          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Mutable = false
              Provenance = None }

          let arrayElem = union [ array.Elem; undefined ]

          for elemType in elemTypes do
            do! unify ctx env arrayElem elemType
        | [ { Kind = TypeKind.Rest t } ] ->
          for elemType in elemTypes do
            do! unify ctx env array.Elem elemType

          do! unify ctx env t1 t
        | _ ->
          // Multiple rest elements in undeciable
          // TODO: create an Undecable error type
          return! Error(TypeError.SemanticError("Too many rest elements!"))
      | TypeKind.Function(f1), TypeKind.Function(f2) ->
        // TODO: check if `f1` and `f2` have the same type params
        // TODO: check if the type params have the same variance
        let! f1 = instantiateFunc ctx f1 None
        let! f2 = instantiateFunc ctx f2 None

        let nonRestParams2, restParams2 =
          List.partition
            (fun param ->
              match param.Pattern with
              | Pattern.Rest _ -> false
              | _ -> true)
            f2.ParamList

        match restParams2 with
        | [] ->
          if f1.ParamList.Length > f2.ParamList.Length then
            // t1 needs to have at least as many params as t2
            return! Error(TypeError.TypeMismatch(t1, t2))

          for i in 0 .. f1.ParamList.Length - 1 do
            let param1 = f1.ParamList[i]
            let param2 = f2.ParamList[i]

            do! unify ctx env param2.Type param1.Type // params are contravariant
        | [ restParam2 ] ->
          for i in 0 .. nonRestParams2.Length - 1 do
            let param1 = f1.ParamList[i]
            let param2 = f2.ParamList[i]
            do! unify ctx env param2.Type param1.Type // params are contravariant

          let restParam1 =
            { Kind =
                TypeKind.Tuple
                  { Elems =
                      List.map
                        (fun (param: FuncParam) -> param.Type)
                        (List.skip nonRestParams2.Length f1.ParamList)
                    Immutable = false }
              Mutable = false
              Provenance = None }

          do! unify ctx env restParam2.Type restParam1
        | _ -> return! Error(TypeError.SemanticError("Too many rest params!"))

        do! unify ctx env f1.Return f2.Return // returns are covariant
        do! unify ctx env f1.Throws f2.Throws // throws are covariant
      | TypeKind.TypeRef({ Name = name1; TypeArgs = types1 }),
        TypeKind.TypeRef({ Name = name2; TypeArgs = types2 }) when name1 = name2 ->

        match (types1, types2) with
        | None, None -> ()
        | Some(types1), Some(types2) ->
          if List.length types1 <> List.length types2 then
            return! Error(TypeError.TypeMismatch(t1, t2))

          List.map2 (unify ctx env) types1 types2 |> ignore
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Range range1, TypeKind.Range range2 ->
        match
          range1.Min.Kind, range1.Max.Kind, range2.Min.Kind, range2.Max.Kind
        with
        | TypeKind.Literal(Literal.Number min1),
          TypeKind.Literal(Literal.Number max1),
          TypeKind.Literal(Literal.Number min2),
          TypeKind.Literal(Literal.Number max2) ->

          if min1 >= min2 && max1 <= max2 then
            ()
          else
            return! Error(TypeError.TypeMismatch(t1, t2))
        | _ ->
          printfn $"unify({t1}, {t2})"
          printfn "TODO: expand `min` and `max` before unifying with `n`"
          return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Range _, TypeKind.Primitive Primitive.Number -> ()
      | TypeKind.Literal(Literal.Number n),
        TypeKind.Range { Min = min; Max = max } ->

        match min.Kind, max.Kind with
        | TypeKind.Literal(Literal.Number min),
          TypeKind.Literal(Literal.Number max) ->
          if n >= min && n < max then
            ()
          else
            return! Error(TypeError.TypeMismatch(t1, t2))
        | _, _ ->
          printfn "TODO: expand `min` and `max` before unifying with `n`"
          return! Error(TypeError.TypeMismatch(t1, t2))
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
      | TypeKind.Literal(Literal.String s), TypeKind.TemplateLiteral tl ->
        if TemplateLiteral.check s tl then
          ()
        else
          return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.UniqueSymbol id1, TypeKind.UniqueSymbol id2 when id1 = id2 ->
        ()
      | TypeKind.UniqueNumber id1, TypeKind.UniqueNumber id2 when id1 = id2 ->
        ()
      | TypeKind.Object obj1, TypeKind.Object obj2 ->
        if not obj1.Immutable && obj2.Immutable then
          return! Error(TypeError.TypeMismatch(t1, t2))

        let namedProps1 =
          List.choose
            (fun (elem: ObjTypeElem) ->
              match elem with
              // TODO: handle methods, setters, and getters
              | Property p -> Some(p.Name, p)
              | _ -> None)
            obj1.Elems
          |> Map.ofList

        let namedProps2 =
          List.choose
            (fun (elem: ObjTypeElem) ->
              match elem with
              // TODO: handle methods, setters, and getters
              | Property p -> Some(p.Name, p)
              | _ -> None)
            obj2.Elems
          |> Map.ofList

        let undefined =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Mutable = false
            Provenance = None }

        if env.IsPatternMatching then
          // Allow partial unification of object types when pattern matching
          for KeyValue(name, prop1) in namedProps1 do
            match namedProps2.TryFind name with
            | Some(prop2) ->
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
              // TODO: double check that this is correct
              if not prop1.Optional then
                return! Error(TypeError.TypeMismatch(t1, t2))
        else
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

      | TypeKind.Object obj, TypeKind.Intersection types ->
        let mutable combinedElems = []
        let mutable restTypes = []

        for t in types do
          match t.Kind with
          | TypeKind.Object { Elems = elems } ->
            combinedElems <- combinedElems @ elems
          | TypeKind.Rest t -> restTypes <- t :: restTypes
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        let objType =
          { Kind =
              TypeKind.Object
                { Elems = combinedElems
                  // TODO: figure out what do do with `Immutable`
                  Immutable = false }
            Mutable = false
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
              obj.Elems

          let newObjType =
            // TODO: figure out what do do with `Immutable`
            { Kind = TypeKind.Object { Elems = objElems; Immutable = false }
              Mutable = false
              Provenance = None }

          do! unify ctx env newObjType objType

          let newRestType =
            // TODO: figure out what do do with `Immutable`
            { Kind = TypeKind.Object { Elems = restElems; Immutable = false }
              Mutable = false
              Provenance = None }

          do! unify ctx env newRestType restType
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Intersection types, TypeKind.Object obj ->
        let mutable combinedElems = []
        let mutable restTypes = []

        for t in types do
          match t.Kind with
          | TypeKind.Object { Elems = elems } ->
            combinedElems <- combinedElems @ elems
          | TypeKind.Rest t -> restTypes <- t :: restTypes
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        let objType =
          { Kind =
              TypeKind.Object
                { Elems = combinedElems
                  // TODO: figure out what do do with `Immutable`
                  Immutable = false }
            Mutable = false
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
              obj.Elems

          let newObjType =
            // TODO: figure out what do do with `Immutable`
            { Kind = TypeKind.Object { Elems = objElems; Immutable = false }
              Mutable = false
              Provenance = None }

          do! unify ctx env objType newObjType

          let newRestType =
            // TODO: figure out what do do with `Immutable`
            { Kind = TypeKind.Object { Elems = restElems; Immutable = false }
              Mutable = false
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
      | TypeKind.Binary(left, op, right), TypeKind.Primitive Primitive.Boolean when
        op = "<" || op = "<=" || op = ">" || op = ">="
        ->
        return ()
      | TypeKind.Binary(left, op, right), TypeKind.Primitive Primitive.String when
        op = "++"
        ->
        return ()
      | _, _ ->
        let t1' = expandType ctx env (unify ctx) Map.empty t1
        let t2' = expandType ctx env (unify ctx) Map.empty t2

        if t1' <> t1 || t2' <> t2 then
          return! unify ctx env t1' t2'
        else
          printfn $"failed to unify {t1} and {t2}"
          return! Error(TypeError.TypeMismatch(t1, t2))
    }

  let unifyCall
    (ctx: Ctx)
    (env: Env)
    (inferExpr: Ctx -> Env -> Syntax.Expr -> Result<Type, TypeError>)
    (args: list<Syntax.Expr>)
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
              let name = $"arg{i}"

              let p: Pattern =
                Pattern.Identifier { Name = name; IsMut = false }

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
            Mutable = false
            Provenance = None }

        match bind ctx env unify callee callType with
        | Ok _ -> return (prune retType, prune throwsType)
        | Error e -> return! Error e
      | kind -> return! Error(TypeError.NotImplemented $"kind = {kind}")
    }

  let unifyFuncCall
    (ctx: Ctx)
    (env: Env)
    (inferExpr: Ctx -> Env -> Syntax.Expr -> Result<Type, TypeError>)
    (args: list<Syntax.Expr>)
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

      for (arg, argType), param in List.zip args callee.ParamList do
        if
          param.Optional && argType.Kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          // TODO: split this into a two step process:
          // - do a first pass unification with subtyping
          // - do a second pass which finds bindings in the arg and matches
          //   those up with bindings in the param.  if any of those pairs
          //   have a mutable param binding then check if the arg is also
          //   mutable.  If it is then use `unifyInvariant` to check if those
          //   two types are invariant.
          // See https://github.com/escalier-lang/escalier-next/issues/124
          let! isArgMut =
            match arg.Kind with
            | Syntax.ExprKind.Identifier name ->
              Result.map (fun (_, isMut) -> isMut) (env.GetBinding name)
            | _ -> Ok(false)

          match param.Pattern with
          | Identifier { Name = name; IsMut = true } when isArgMut ->
            match unifyInvariant ctx env argType param.Type with
            | Ok _ -> ()
            | Error reason ->
              let never =
                { Kind = TypeKind.Keyword Keyword.Never
                  Mutable = false
                  Provenance = None }

              do! unify ctx env never param.Type

              ctx.AddDiagnostic(
                { Description =
                    $"arg type '{argType}' doesn't satisfy param '{param.Pattern}' type '{param.Type}' in function call"
                  Reasons = [ reason ] }
              )
          | Identifier { Name = name; IsMut = true } when not isArgMut ->
            let reason =
              TypeError.SemanticError "param is mutable but arg is not"

            ctx.AddDiagnostic(
              { Description =
                  $"arg type '{argType}' doesn't satisfy param '{param.Pattern}' type '{param.Type}' in function call"
                Reasons = [ reason ] }
            )
          | _ ->
            // TODO: check_mutability of `arg`
            // contravariant
            match unify ctx env argType param.Type with
            | Ok _ -> ()
            | Error(reason) ->
              let never =
                { Kind = TypeKind.Keyword Keyword.Never
                  Mutable = false
                  Provenance = None }

              do! unify ctx env never param.Type

              ctx.AddDiagnostic(
                { Description =
                    $"arg type '{argType}' doesn't satisfy param '{param.Pattern}' type '{param.Type}' in function call"
                  Reasons = [ reason ] }
              )

      return (callee.Return, callee.Throws)
    }

  // TODO: don't allow `mut` on variables whose type is a literal or primitive
  let unifyInvariant
    (ctx: Ctx)
    (env: Env)
    (t1: Type)
    (t2: Type)
    : Result<unit, TypeError> =

    result {
      let t1 = prune t1
      let t2 = prune t2

      match t1.Kind, t2.Kind with
      // We special case array because lengths are usually `unique number` which
      // don't unify.
      // TODO: generalize `.Length` in params that are arrays
      | TypeKind.Array { Elem = elem1; Length = len1 },
        TypeKind.Array { Elem = elem2; Length = len2 } ->
        do! unifyInvariant ctx env elem1 elem2
      // TODO: allow array and tuples to unify when the tuple has a rest element
      | _ ->
        if t1 = t2 then
          return ()
        else
          let t1' = expandType ctx env (unify ctx) Map.empty t1
          let t2' = expandType ctx env (unify ctx) Map.empty t2

          if t1' <> t1 || t2' <> t2 then
            return! unifyInvariant ctx env t1' t2'
          else
            return! Error(TypeError.TypeMismatch(t1, t2))
    }
