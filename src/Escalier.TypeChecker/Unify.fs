namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data.Common
open Escalier.Data.Type

open Error
open Folder
open Prune
open Env
open Mutability
open Poly

module rec Unify =

  // Checks that t1 is assignable to t2
  let unify
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>) // invariant paths
    (t1: Type)
    (t2: Type)
    : Result<unit, TypeError> =

    // TODO: update printType to work with template string types
    // printfn $"unify({t1}, {t2})"

    match ips with
    | Some [ [] ] -> unifyInvariant ctx env ips t1 t2
    | _ -> unifySubtyping ctx env ips t1 t2

  // Checks that t1 is assignable to t2, t1 must be a subtype of t2
  let unifySubtyping
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>) // invariant paths
    (t1: Type)
    (t2: Type)
    : Result<unit, TypeError> =

    result {
      match (prune t1).Kind, (prune t2).Kind with
      | TypeKind.TypeVar _, _ -> do! bind ctx env ips t1 t2
      | _, TypeKind.TypeVar _ -> do! bind ctx env ips t2 t1
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

          for i in 0 .. elemTypes.Length - 1 do
            let elem1 = tuple1.Elems.[i]
            let elem2 = elemTypes.[i]

            let newIps = tryFindPathTails (i.ToString()) ips
            do! unify ctx env newIps elem1 elem2
        | [ { Kind = TypeKind.Rest t } ] ->
          // TODO: verify that the rest element comes last in the tuple

          let elems1, restElems1 = List.splitAt elemTypes.Length tuple1.Elems

          for i in 0 .. elems1.Length - 1 do
            let elem1 = elems1.[i]
            let elem2 = elemTypes.[i]

            let newIps = tryFindPathTails (i.ToString()) ips
            do! unify ctx env newIps elem1 elem2

          let restTuple =
            { Kind = TypeKind.Tuple { tuple1 with Elems = restElems1 }
              Provenance = None }

          do! unify ctx env ips restTuple t
        | _ ->
          // Multiple rest elements in undeciable
          // TODO: create an Undecable error type
          return! Error(TypeError.SemanticError("Too many rest elements!"))
      | TypeKind.Array elemType1, TypeKind.Array elemType2 ->
        // TODO: unify the lengths of the arrays
        // An array whose length is `unique number` is a subtype of an array
        // whose length is `number`.
        do! unify ctx env ips elemType1.Elem elemType2.Elem
      | TypeKind.Tuple tuple, TypeKind.Array array ->
        // TODO: check if array.Elem is `unique number`
        // If it is, then we can't unify these since the tuple could be
        // longer or short than the array.
        // An array with length `number` represents arrays of all possible
        // length whereas a length of `unique number` represents a single array
        // of unknown length.

        let elemTypes, spreadTypes =
          List.partition
            (fun (elem: Type) ->
              match elem.Kind with
              | TypeKind.Rest _ -> false
              | _ -> true)
            tuple.Elems

        // TODO: check for `Rest` types in tupleElemTypes, if we find one at the
        // end of the tuple, we can unify the rest of the array with it.
        do! unify ctx env ips (union elemTypes) array.Elem

        let spreadTypes =
          List.map
            (fun (t: Type) ->
              match t.Kind with
              | TypeKind.Rest t -> t
              | _ -> t)
            spreadTypes

        for spreadType in spreadTypes do
          do! unify ctx env ips spreadType t2
      | TypeKind.Array array, TypeKind.Tuple tuple ->
        let elemTypes, restTypes =
          List.partition
            (fun (elem: Type) ->
              match elem.Kind with
              | TypeKind.Rest _ -> false
              | _ -> true)
            tuple.Elems

        let undefined =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Provenance = None }

        match restTypes with
        | [] ->
          let arrayElem = union [ array.Elem; undefined ]

          for i in 0 .. elemTypes.Length - 1 do
            let newIps = tryFindPathTails (i.ToString()) ips
            do! unify ctx env newIps arrayElem elemTypes[i]
        | [ { Kind = TypeKind.Rest t } ] ->
          let arrayElem = union [ array.Elem; undefined ]

          for i in 0 .. elemTypes.Length - 1 do
            let newIps = tryFindPathTails (i.ToString()) ips
            do! unify ctx env newIps arrayElem elemTypes[i]

          do! unify ctx env ips t1 t
        | _ ->
          // Multiple rest elements in undeciable
          // TODO: create an Undecable error type
          return! Error(TypeError.SemanticError("Too many rest elements!"))
      | TypeKind.Rest rest, TypeKind.Array array ->
        do! unify ctx env ips rest t2
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

            do! unify ctx env ips param2.Type param1.Type // params are contravariant
        | [ restParam2 ] ->
          for i in 0 .. nonRestParams2.Length - 1 do
            let param1 = f1.ParamList[i]
            let param2 = f2.ParamList[i]
            do! unify ctx env ips param2.Type param1.Type // params are contravariant

          let restParam1 =
            { Kind =
                TypeKind.Tuple
                  { Elems =
                      List.map
                        (fun (param: FuncParam) -> param.Type)
                        (List.skip nonRestParams2.Length f1.ParamList)
                    Immutable = false }
              Provenance = None }

          printfn $"restParam2.Type = {restParam2.Type}"
          printfn $"restParam1 = {restParam1}"
          do! unify ctx env ips restParam2.Type restParam1
        | _ -> return! Error(TypeError.SemanticError("Too many rest params!"))

        do! unify ctx env ips f1.Return f2.Return // returns are covariant
        do! unify ctx env ips f1.Throws f2.Throws // throws are covariant
      | TypeKind.TypeRef({ Name = name1; TypeArgs = types1 }),
        TypeKind.TypeRef({ Name = name2; TypeArgs = types2 }) when name1 = name2 ->

        match (types1, types2) with
        | None, None -> ()
        | Some(types1), Some(types2) ->
          if List.length types1 <> List.length types2 then
            return! Error(TypeError.TypeMismatch(t1, t2))

          List.map2 (unify ctx env ips) types1 types2 |> ignore
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.TypeRef { TypeArgs = typeArgs; Scheme = scheme }, _ ->
        match scheme with
        | Some scheme ->
          // // TODO: dedupe the same code in generalizeFunc
          // let mutable mapping: Map<string, Type> = Map.empty
          //
          // match scheme.TypeParams with
          // | Some(typeParams) ->
          //   match typeArgs with
          //   | Some(typeArgs) ->
          //     if typeArgs.Length <> typeParams.Length then
          //       return! Error(TypeError.WrongNumberOfTypeArgs)
          //
          //     for tp, ta in List.zip typeParams typeArgs do
          //       mapping <- mapping.Add(tp.Name, ta)
          //   | None ->
          //     for tp in typeParams do
          //       mapping <- mapping.Add(tp.Name, ctx.FreshTypeVar None None)
          // | None -> ()
          // let! t = expandScheme ctx env ips scheme mapping typeArgs
          let! t = expandScheme ctx env ips scheme Map.empty typeArgs
          do! unify ctx env ips t t2
        | _ -> return! unifyFallThrough ctx env ips t1 t2
      | _, TypeKind.TypeRef { TypeArgs = typeArgs; Scheme = scheme } ->
        match scheme with
        | Some scheme ->
          // // TODO: dedupe the same code in generalizeFunc
          // let mutable mapping: Map<string, Type> = Map.empty
          //
          // match scheme.TypeParams with
          // | Some(typeParams) ->
          //   match typeArgs with
          //   | Some(typeArgs) ->
          //     if typeArgs.Length <> typeParams.Length then
          //       return! Error(TypeError.WrongNumberOfTypeArgs)
          //
          //     for tp, ta in List.zip typeParams typeArgs do
          //       mapping <- mapping.Add(tp.Name, ta)
          //   | None ->
          //     for tp in typeParams do
          //       mapping <- mapping.Add(tp.Name, ctx.FreshTypeVar None None)
          // | None -> ()
          //
          // let! t = expandScheme ctx env ips scheme mapping typeArgs
          let! t = expandScheme ctx env ips scheme Map.empty typeArgs
          do! unify ctx env ips t1 t
        | _ -> return! unifyFallThrough ctx env ips t1 t2
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
      | TypeKind.UniqueNumber _, TypeKind.Primitive Primitive.Number -> ()
      | TypeKind.EnumVariant v1, TypeKind.EnumVariant v2 when v1.Tag = v2.Tag ->
        if v1.Types.Length <> v2.Types.Length then
          return!
            Error(TypeError.SemanticError "Enum variant types don't match")

        for t1, t2 in List.zip v1.Types v2.Types do
          do! unify ctx env ips t1 t2

        ()
      | TypeKind.Object obj1, TypeKind.Object obj2 ->
        if not obj1.Immutable && obj2.Immutable then
          return! Error(TypeError.TypeMismatch(t1, t2))

        let namedProps1 = getNamedProps obj1.Elems
        let namedProps2 = getNamedProps obj2.Elems
        do! unifyObjProps ctx env ips namedProps1 namedProps2

      | TypeKind.Object obj, TypeKind.Intersection types ->
        let mutable combinedElems = []
        let mutable restTypes = []

        // TODO: dedupe with TypeKind.Intersection, TypeKind.Object case
        for t in types do
          match t.Kind with
          | TypeKind.Object { Elems = elems } ->
            combinedElems <- combinedElems @ elems
          | TypeKind.Rest t -> restTypes <- t :: restTypes
          | TypeKind.TypeVar { Bound = Some bound } ->
            restTypes <- t :: restTypes
          | TypeKind.TypeRef _ ->
            let! t = expandType ctx env ips Map.empty t

            match t.Kind with
            | TypeKind.Object { Elems = elems } ->
              combinedElems <- combinedElems @ elems
            | TypeKind.Rest t -> restTypes <- t :: restTypes
            | _ ->
              printfn $"t is not an object types - {t}"
              return! Error(TypeError.TypeMismatch(t1, t2))
          | _ ->
            printfn $"t is not an object types - {t}"
            return! Error(TypeError.TypeMismatch(t1, t2))

        let objType =
          { Kind =
              TypeKind.Object
                { Extends = None
                  Implements = None
                  Elems = combinedElems
                  // TODO: figure out what do do with `Immutable`
                  Immutable = false
                  Interface = false }
            Provenance = None }

        match restTypes with
        | [] -> do! unify ctx env ips t1 objType
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
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = objElems
                    Immutable = false
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips newObjType objType

          let newRestType =
            // TODO: figure out what do do with `Immutable`
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = restElems
                    Immutable = false
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips newRestType restType
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Intersection types, TypeKind.Object obj ->
        let mutable combinedElems = []
        let mutable restTypes = []

        // TODO: dedupe with TypeKind.Object, TypeKind.Intersection case
        for t in types do
          match t.Kind with
          | TypeKind.Object { Elems = elems } ->
            combinedElems <- combinedElems @ elems
          | TypeKind.Rest t -> restTypes <- t :: restTypes
          | TypeKind.TypeVar { Bound = Some bound } ->
            restTypes <- t :: restTypes
          | TypeKind.TypeRef { Name = name } ->
            let! t = expandType ctx env ips Map.empty t

            match t.Kind with
            | TypeKind.Object { Elems = elems } ->
              combinedElems <- combinedElems @ elems
            | TypeKind.Rest t -> restTypes <- t :: restTypes
            | _ -> failwith $"TODO: handle type {t} in intersection"
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        let objType =
          { Kind =
              TypeKind.Object
                { Extends = None
                  Implements = None
                  Elems = combinedElems
                  // TODO: figure out what do do with `Immutable`
                  Immutable = false
                  Interface = false }
            Provenance = None }

        match restTypes with
        | [] -> do! unify ctx env ips objType t2
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
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = objElems
                    Immutable = false
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips objType newObjType

          let newRestType =
            // TODO: figure out what do do with `Immutable`
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = restElems
                    Immutable = false
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips restType newRestType
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Intersection types1, TypeKind.Intersection types2 ->
        printfn $"types1 = {types1}, types2 = {types2}"
        failwith "TODO: handle unify(intersection, intersection)"

      | TypeKind.Union(types), _ ->
        let! _ = types |> List.traverseResultM (fun t -> unify ctx env ips t t2)

        return ()
      | _, TypeKind.Union(types) ->

        let unifier =
          List.tryFind (fun t -> unify ctx env ips t1 t |> Result.isOk) types

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
      | _, _ -> return! unifyFallThrough ctx env ips t1 t2
    }

  let unifyFallThrough
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (t1: Type)
    (t2: Type)
    : Result<unit, TypeError> =

    result {
      let! t1' = expandType ctx env ips Map.empty t1
      let! t2' = expandType ctx env ips Map.empty t2

      if t1' <> t1 || t2' <> t2 then
        return! unify ctx env ips t1' t2'
      else
        printfn $"failed to unify {t1} and {t2}"
        return! Error(TypeError.TypeMismatch(t1, t2))
    }

  let getNamedProps (elems: list<ObjTypeElem>) : Map<PropName, Property> =
    elems
    |> List.choose (fun (elem: ObjTypeElem) ->
      match elem with
      | Property p -> Some(p.Name, p)
      | _ -> None)
    |> Map.ofList

  let unifyObjProps
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (namedProps1: Map<PropName, Property>)
    (namedProps2: Map<PropName, Property>)
    : Result<unit, TypeError> =
    result {
      let undefined =
        { Kind = TypeKind.Literal(Literal.Undefined)
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

            let newIps = tryFindPathTails (name.ToString()) ips
            do! unify ctx env newIps p1Type p2Type
          | None ->
            // TODO: double check that this is correct
            if not prop1.Optional then
              return! Error(TypeError.PropertyMissing(name))
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

            let newIps = tryFindPathTails (name.ToString()) ips
            do! unify ctx env newIps p1Type p2Type
          | None ->
            if not prop2.Optional then
              return! Error(TypeError.PropertyMissing(name))
    }

  // TODO: don't allow `mut` on variables whose type is a literal or primitive
  let unifyInvariant
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
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
        do! unifyInvariant ctx env ips elem1 elem2
      // TODO: allow array and tuples to unify when the tuple has a rest element
      | _ ->
        if t1 = t2 then
          return ()
        else
          let! t1' = expandType ctx env ips Map.empty t1
          let! t2' = expandType ctx env ips Map.empty t2

          if t1' <> t1 || t2' <> t2 then
            return! unifyInvariant ctx env ips t1' t2'
          else
            return! Error(TypeError.TypeMismatch(t1, t2))
    }

  let rec bind
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (t1: Type)
    (t2: Type)
    : Result<unit, TypeError> =
    let t1 = prune t1
    let t2 = prune t2

    // printfn $"bind({t1}, {t2})"

    result {
      if t1.Kind <> t2.Kind then
        if occursInType t1 t2 then
          match t2.Kind with
          | TypeKind.Union types ->
            let types =
              types
              |> flatten
              |> List.filter (fun t -> t <> t1)
              |> List.map (fun t ->
                // TODO: make this recursive
                match t.Kind with
                | TypeKind.Binary _ ->
                  { Kind = TypeKind.Primitive Primitive.Number
                    Provenance = None }
                | TypeKind.Unary(op, _) ->
                  match op with
                  | "+"
                  | "-" ->
                    { Kind = TypeKind.Primitive Primitive.Number
                      Provenance = None }
                  | "!" ->
                    { Kind = TypeKind.Primitive Primitive.Boolean
                      Provenance = None }
                  | _ -> failwith $"Invalid unary operator {op}"
                | _ -> t)

            match types with
            | [] -> return ()
            | [ t ] -> return! bind ctx env ips t1 t
            | types ->
              let t = union types
              return! bind ctx env ips t1 t
          | TypeKind.Binary _ ->
            let t =
              { Kind = TypeKind.Primitive Primitive.Number
                Provenance = None }

            let! _ = bind ctx env ips t1 t
            return ()
          | TypeKind.Unary(op, _) ->
            let t =
              match op with
              | "+"
              | "-" ->
                { Kind = TypeKind.Primitive Primitive.Number
                  Provenance = None }
              | "!" ->
                { Kind = TypeKind.Primitive Primitive.Boolean
                  Provenance = None }
              | _ -> failwith $"Invalid unary operator {op}"

            let! _ = bind ctx env ips t1 t
            return ()
          | _ ->
            printfn "recursive unification error"
            return! Error(TypeError.RecursiveUnification(t1, t2))
        else
          match t1.Kind with
          | TypeKind.TypeVar(v1) ->
            match v1.Bound with
            | Some bound1 ->
              // printfn $"unify({t2}, {bound1})"
              // If t2 is a TypeVar then we want to check if the bounds
              // unify

              // Type params are contravariant for similar reasons to
              // why function params are contravariant
              // do! unify ctx env ips t2 bound1

              match t2.Kind with
              | TypeKind.TypeVar v2 ->
                match v2.Bound with
                | Some bound2 ->
                  // TODO: we need to know the directionality of the bind
                  // call so that we can unify the bounds in the correct direction
                  do! unify ctx env ips bound2 bound1
                | None -> ()

                v2.Bound <- v1.Bound
                v2.Default <- v1.Default
                v1.Instance <- Some(t2)
              | TypeKind.Keyword Keyword.Never ->
                // TODO: figure out when the bound is never so I can write
                // some docs here
                v1.Instance <- Some(bound1)
              | _ ->
                do! unify ctx env ips t2 bound1
                v1.Instance <- Some(t2)
            | None -> v1.Instance <- Some(t2)

            return ()
          | _ -> return! Error(TypeError.NotImplemented "bind error")
    }

  and occursInType (t1: Type) (t2: Type) : bool =
    let mutable result: bool = false

    let visitor =
      fun (t: Type) ->
        match (prune t).Kind with
        | pruned when pruned = t1.Kind -> result <- true
        | _ -> ()

    TypeVisitor.walkType visitor t2

    result


  let expandScheme
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (scheme: Scheme)
    (mapping: Map<string, Type>)
    (typeArgs: option<list<Type>>)
    : Result<Type, TypeError> =

    result {
      match scheme.TypeParams, typeArgs with
      | None, None -> return! expandType ctx env ips mapping scheme.Type
      | Some(typeParams), Some(typeArgs) ->
        let mutable newEnv = env

        // TODO: augment the incoming mapping with this new mapping
        // any conflicts should be resolved with the new mapping winning
        let! mapping = buildTypeArgMapping ctx typeParams (Some typeArgs)

        for KeyValue(name, t) in mapping do
          newEnv <- newEnv.AddScheme name { Type = t; TypeParams = None }

        return! expandType ctx newEnv ips mapping scheme.Type
      | typeParams, typeArgs ->
        printfn $"typeParams = {typeParams}"
        printfn $"typeArgs = {typeArgs}"

        return!
          Error(
            TypeError.NotImplemented "TODO: expandScheme with type params/args"
          )
    }

  // `mapping` must be distict from `env` because type params that are union
  // types distribute across conditional types.
  let expandType
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (mapping: Map<string, Type>) // type param names -> type args
    (t: Type)
    : Result<Type, TypeError> =

    let rec expand
      (mapping: Map<string, Type>)
      (t: Type)
      : Result<Type, TypeError> =

      // TODO: only define `fold` when we actually need to use it
      let fold =
        fun t ->
          let result =
            match t.Kind with
            | TypeKind.TypeRef { Name = QualifiedIdent.Ident name } ->
              match Map.tryFind name mapping with
              | Some typeArg -> typeArg
              | None -> t
            | _ -> t

          Some(result)

      result {

        match t.Kind with
        | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Promise" } ->
          printfn $"t = {t}"
        | _ -> ()

        let t = prune t

        match t.Kind with
        | TypeKind.KeyOf t ->
          let! t = expandType ctx env ips mapping t

          match t.Kind with
          | TypeKind.Object { Elems = elems } ->
            let keys =
              elems
              |> List.choose (fun elem ->
                // TODO: handle mapped types
                match elem with
                | Property p -> Some(p.Name)
                | _ -> None)

            let keys =
              keys
              |> List.map (fun key ->
                match key with
                | PropName.String s ->
                  { Kind = TypeKind.Literal(Literal.String s)
                    Provenance = None }
                | PropName.Number n ->
                  { Kind = TypeKind.Literal(Literal.Number n)
                    Provenance = None }
                | PropName.Symbol id ->
                  { Kind = TypeKind.UniqueSymbol id
                    Provenance = None })

            return union keys
          | _ ->
            printfn "t = %A" t
            return! Error(TypeError.NotImplemented $"TODO: expand keyof {t}")
        | TypeKind.Index(target, index) ->
          let! target = expandType ctx env ips mapping target
          let! index = expandType ctx env ips mapping index

          let key =
            match index.Kind with
            | TypeKind.Literal(Literal.String s) -> PropName.String s
            | TypeKind.Literal(Literal.Number n) -> PropName.Number n
            | TypeKind.UniqueSymbol id -> PropName.Symbol id
            | _ -> failwith "TODO: expand index - key type"

          match target.Kind with
          | TypeKind.Object { Elems = elems } ->
            let mutable t = None

            for elem in elems do
              match elem with
              | Property p when p.Name = key -> t <- Some(p.Type)
              | _ -> ()

            match t with
            | Some t -> return t
            | None ->
              return! Error(TypeError.SemanticError $"Property {key} not found")
          | _ ->
            // TODO: Handle the case where the type is a primitive and use a
            // special function to expand the type
            // TODO: Handle different kinds of index types, e.g. number, symbol
            return! Error(TypeError.NotImplemented "TODO: expand index")
        | TypeKind.Condition { Check = check
                               Extends = extends
                               TrueType = trueType
                               FalseType = falseType } ->

          let infers = findInfers extends
          let mutable newMapping = mapping

          for name in infers do
            let t = ctx.FreshTypeVar None None
            newMapping <- Map.add name t newMapping

          let extends = replaceInfers extends newMapping

          match check.Kind with
          | TypeKind.TypeRef { Name = QualifiedIdent.Ident name } ->
            let! check = expand newMapping check

            match check.Kind with
            | TypeKind.Union types ->
              let! extends = expand newMapping extends

              let! types =
                types
                |> List.traverseResultM (fun check ->
                  let newMapping = Map.add name check newMapping

                  match unify ctx env ips check extends with
                  | Ok _ -> expand newMapping trueType
                  | Error _ -> expand newMapping falseType)

              return union types
            | _ ->
              match unify ctx env ips check extends with
              | Ok _ -> return! expand newMapping trueType
              | Error _ -> return! expand newMapping falseType
          | _ ->
            match unify ctx env ips check extends with
            | Ok _ -> return! expand newMapping trueType
            | Error _ -> return! expand newMapping falseType

        | TypeKind.Binary _ -> return simplify t
        // TODO: instead of expanding object types, we should try to
        // look up properties on the object type without expanding it
        // since expansion can be quite expensive
        | TypeKind.Object { Elems = elems
                            Extends = extends
                            Immutable = immutable } ->

          let mutable allElems = elems

          let rec processExtends
            (extends: option<list<TypeRef>>)
            : Result<unit, TypeError> =

            result {
              match extends with
              | Some typeRefs ->
                for typeRef in typeRefs do
                  let { Name = typeRefName
                        Scheme = scheme
                        TypeArgs = typeArgs } =
                    typeRef

                  let! objType =
                    result {
                      match scheme with
                      | Some scheme ->
                        return!
                          expandScheme ctx env None scheme mapping typeArgs
                      | None ->
                        let! scheme = env.GetScheme typeRefName

                        return!
                          expandScheme ctx env None scheme mapping typeArgs
                    }

                  match objType.Kind with
                  | TypeKind.Object { Elems = elems; Extends = extends } ->
                    allElems <- allElems @ elems

                    do! processExtends extends
                  | _ -> failwith $"expected ${objType} to be an object type"
              | None -> ()
            }

          do! processExtends extends

          let! elems =
            allElems
            |> List.traverseResultM (fun elem ->
              result {
                match elem with
                | Mapped m ->
                  let! c =
                    expandType ctx env ips mapping m.TypeParam.Constraint

                  match c.Kind with
                  | TypeKind.Union types ->
                    let! elems =
                      types
                      |> List.traverseResultM (fun keyType ->
                        result {
                          // let key =
                          //   match keyType.Kind with
                          //   | TypeKind.Literal(Literal.String s) ->
                          //     PropKey.String s
                          //   | TypeKind.Literal(Literal.Number n) ->
                          //     PropKey.Number n
                          //   | TypeKind.UniqueSymbol id -> PropKey.Symbol id
                          //   | _ -> failwith "TODO: expand mapped type - key type"

                          match keyType.Kind with
                          | TypeKind.Literal(Literal.String name) ->
                            let typeAnn = m.TypeAnn

                            let folder t =
                              match t.Kind with
                              | TypeKind.TypeRef({ Name = QualifiedIdent.Ident name }) when
                                name = m.TypeParam.Name
                                ->
                                Some(keyType)
                              | _ -> None

                            let typeAnn = foldType folder typeAnn
                            let! t = expandType ctx env ips mapping typeAnn

                            return
                              Property
                                { Name = PropName.String name
                                  Type = t
                                  Optional = false // TODO
                                  Readonly = false // TODO
                                }
                          // TODO: handle other valid key types, e.g. number, symbol
                          | _ ->
                            return!
                              Error(
                                TypeError.NotImplemented
                                  "TODO: expand mapped type - key type"
                              )
                        })

                    return elems
                  | TypeKind.Literal(Literal.String key) ->
                    let typeAnn = m.TypeAnn

                    let folder t =
                      match t.Kind with
                      | TypeKind.TypeRef({ Name = QualifiedIdent.Ident name }) when
                        name = m.TypeParam.Name
                        ->
                        Some(m.TypeParam.Constraint)
                      | _ -> None

                    let typeAnn = foldType folder typeAnn
                    let! t = expandType ctx env ips mapping typeAnn

                    return
                      [ Property
                          { Name = PropName.String key
                            Type = t
                            Optional = false // TODO
                            Readonly = false // TODO
                          } ]
                  | _ -> return [ elem ]
                // printfn $"constraint - c = {c}"
                // printfn $"mapped type - m = {m}"
                // failwith "TODO: expand mapped type - constraint type"
                | _ -> return [ elem ]
              })

          let elems = List.collect id elems

          let t =
            { Kind =
                TypeKind.Object
                  { Extends = extends
                    Implements = None // TODO
                    Elems = elems
                    Immutable = immutable
                    Interface = false }
              Provenance = None // TODO: set provenance
            }

          // Replaces type parameters with their corresponding type arguments
          // TODO: do this more consistently
          if mapping = Map.empty then
            return t
          else
            return foldType fold t
        | TypeKind.TypeRef { Name = name
                             TypeArgs = typeArgs
                             Scheme = scheme } ->
          // Replaces type refs appearing in type args with their definitions
          // from `mapping`.
          let typeArgs =
            match typeArgs with
            | Some typeArgs -> typeArgs |> List.map (foldType fold) |> Some
            | None -> None

          // TODO: Take this a setep further and update ExpandType and ExpandScheme
          // to be functions that accept an `env: Env` param.  We can then augment
          // the `env` instead of using the `mapping` param.
          // TODO: check if `name` is a qualified ident first
          // only unqualified idents can appear in the mapping
          let! t =
            match name with
            | Ident name ->
              match Map.tryFind name mapping with
              | Some t -> Result.Ok t
              | None ->
                match scheme with
                | Some scheme ->
                  expandScheme ctx env ips scheme mapping typeArgs
                | None ->
                  match env.TryFindScheme name with
                  | Some scheme ->
                    expandScheme ctx env ips scheme mapping typeArgs
                  | None -> failwith $"{name} is not in scope"
            | Member _ ->
              match scheme with
              | Some scheme -> expandScheme ctx env ips scheme mapping typeArgs
              | None ->
                match env.GetScheme name with
                | Ok scheme -> expandScheme ctx env ips scheme mapping typeArgs
                | Error errorValue -> failwith $"{name} is not in scope"

          return! expand mapping t
        | _ ->
          // Replaces type parameters with their corresponding type arguments
          // TODO: do this more consistently
          if mapping = Map.empty then
            return t
          else
            return foldType fold t
      }

    expand mapping t

  // TODO: dedupe with findInfers in Infer.fs
  let findInfers (t: Type) : list<string> =
    // TODO: disallow multiple `infer`s with the same identifier
    let mutable infers: list<string> = []

    let visitor =
      fun (t: Type) ->
        match t.Kind with
        | TypeKind.Infer name -> infers <- name :: infers
        | _ -> ()

    TypeVisitor.walkType visitor t

    infers

  let replaceInfers (t: Type) (mapping: Map<string, Type>) : Type =
    let fold =
      fun t ->
        let result =
          match t.Kind with
          | TypeKind.Infer name ->
            match Map.tryFind name mapping with
            | Some t -> t
            | None -> t
          | _ -> t

        Some(result)

    foldType fold t
