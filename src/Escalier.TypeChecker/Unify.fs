namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling
open FParsec.CharParsers

open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.Data.Visitor

open Error
open Prune
open Env
open Mutability
open Poly
open Helpers

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
              | TypeKind.RestSpread _ -> false
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
        | [ { Kind = TypeKind.RestSpread t } ] ->
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
      | TypeKind.Tuple tuple,
        TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                           TypeArgs = Some [ elem ] } ->
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
              | TypeKind.RestSpread _ -> false
              | _ -> true)
            tuple.Elems

        // TODO: check for `Rest` types in tupleElemTypes, if we find one at the
        // end of the tuple, we can unify the rest of the array with it.
        do! unify ctx env ips (union elemTypes) elem

        let spreadTypes =
          List.map
            (fun (t: Type) ->
              match t.Kind with
              | TypeKind.RestSpread t -> t
              | _ -> t)
            spreadTypes

        for spreadType in spreadTypes do
          do! unify ctx env ips spreadType t2
      | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                           TypeArgs = Some [ arrayElem ] },
        TypeKind.Tuple tuple ->
        let elemTypes, restTypes =
          List.partition
            (fun (elem: Type) ->
              match elem.Kind with
              | TypeKind.RestSpread _ -> false
              | _ -> true)
            tuple.Elems

        let undefined =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Provenance = None }

        match restTypes with
        | [] ->
          let arrayElem = union [ arrayElem; undefined ]

          for i in 0 .. elemTypes.Length - 1 do
            let newIps = tryFindPathTails (i.ToString()) ips
            do! unify ctx env newIps arrayElem elemTypes[i]
        | [ { Kind = TypeKind.RestSpread t } ] ->
          let arrayElem = union [ arrayElem; undefined ]

          for i in 0 .. elemTypes.Length - 1 do
            let newIps = tryFindPathTails (i.ToString()) ips
            do! unify ctx env newIps arrayElem elemTypes[i]

          do! unify ctx env ips t1 t
        | _ ->
          // Multiple rest elements in undeciable
          // TODO: create an Undecable error type
          return! Error(TypeError.SemanticError("Too many rest elements!"))
      | TypeKind.RestSpread rest,
        TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                           TypeArgs = Some [ _ ] } ->
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
                    Mutable = false
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
      | TypeKind.TypeRef _, _ ->
        let! t = expandType ctx env ips Map.empty t1
        do! unify ctx env ips t t2
      | _, TypeKind.TypeRef _ ->
        let! t = expandType ctx env ips Map.empty t2
        do! unify ctx env ips t1 t
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
        if checkTemplateLiteral ctx env s tl then
          ()
        else
          return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Literal(Literal.String s1),
        TypeKind.IntrinsicInstance { Name = name
                                     TypeArgs = Some typeArgs } ->

        let! t = expandType ctx env ips Map.empty typeArgs[0]

        // TODO: if the type arg is a union then we have to distribute the
        // intrinsic over the union.
        match t.Kind with
        | TypeKind.Literal(Literal.String s2) ->
          let s2 =
            match name with
            | QualifiedIdent.Ident "Uppercase" -> s2.ToUpper()
            | QualifiedIdent.Ident "Lowercase" -> s2.ToLower()
            | QualifiedIdent.Ident "Capitalize" ->
              match s2 with
              | "" -> ""
              | _ -> s2.[0].ToString().ToUpper() + s2.[1..].ToLower()
            | QualifiedIdent.Ident "Uncapitalize" ->
              match s2 with
              | "" -> ""
              | _ -> s2.[0].ToString().ToLower() + s2.[1..].ToUpper()
            | _ -> failwith $"Invalid intrinsic: {name}"

          if s1 = s2 then
            ()
          else
            return! Error(TypeError.TypeMismatch(t1, t2))
        | TypeKind.Primitive(Primitive.String) ->
          match name with
          | QualifiedIdent.Ident "Uppercase" when isUppercase s1 -> ()
          | QualifiedIdent.Ident "Lowercase" when isLowercase s1 -> ()
          | QualifiedIdent.Ident "Capitalize" when isCapitalize s1 -> ()
          | QualifiedIdent.Ident "Uncapitalize" when isUncapitalize s1 -> ()
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
        | TypeKind.Union types ->
          let types =
            types
            |> List.map (fun t ->
              let kind =
                TypeKind.IntrinsicInstance
                  { Name = name; TypeArgs = Some [ t ] }

              { Kind = kind; Provenance = None })

          let t2 =
            { Kind = TypeKind.Union types
              Provenance = None }

          do! unify ctx env ips t1 t2
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.UniqueSymbol id1, TypeKind.UniqueSymbol id2 when id1 = id2 ->
        ()
      | TypeKind.Object obj, TypeKind.Extractor extractor ->

        let symbolGlobal =
          match env.TryFindValue "Symbol" with
          | Some binding -> binding.Type
          | None -> failwith "Symbol not in scope"

        let! propName =
          match
            getPropType
              ctx
              env
              symbolGlobal
              (PropName.String "customMatcher")
              false
              ValueCategory.RValue
          with
          | Result.Ok sym ->
            match (prune sym).Kind with
            | TypeKind.UniqueSymbol id -> PropName.Symbol id |> Result.Ok
            | _ ->
              Result.Error(
                TypeError.SemanticError
                  "Symbol.customMatcher is not a unique symbol"
              )
          | Result.Error _ ->
            Result.Error(
              TypeError.SemanticError "Symbol.customMatcher not found"
            )

        let! method = getPropType ctx env t1 propName false ValueCategory.RValue

        match method.Kind with
        | TypeKind.Function f ->
          let retType = f.Return

          match (prune retType).Kind with
          | TypeKind.Tuple { Elems = elems } ->
            if
              elems.Length > extractor.Args.Length && extractor.Args.Length > 1
            then
              let last = List.last extractor.Args

              match last.Kind with
              | TypeKind.RestSpread arg ->
                let index = extractor.Args.Length - 1
                let nonRestElems, restElems = List.splitAt index elems

                let args = extractor.Args.GetSlice(Some 0, Some(index - 1))

                for elem, arg in List.zip nonRestElems args do
                  do! unify ctx env ips arg elem

                let tuple =
                  { Kind =
                      TypeKind.Tuple
                        { Elems = restElems
                          Mutable = false
                          Immutable = false }
                    Provenance = None }

                do! unify ctx env ips arg tuple
              | _ ->
                return!
                  Error(TypeError.SemanticError "Extractor arity mismatch A")
            else if elems.Length = extractor.Args.Length then
              // Check that each element in the tuple is assignable to the corresponding
              // element in the extractor args.  This matches what we do for destructuring.
              for elem, arg in List.zip elems extractor.Args do
                do! unify ctx env ips arg elem
            else
              return!
                Error(TypeError.SemanticError "Extractor arity mismatch B")
          | _ ->
            return!
              Error(
                TypeError.SemanticError "Extractor return type is not a tuple"
              )
        | _ ->
          return!
            Error(
              TypeError.SemanticError
                "[Symbol.customMatcher] isn't a function/method"
            )
      | TypeKind.Extractor extractor, TypeKind.Object obj ->
        // This case comes up when there are nested extractors
        let symbolGlobal =
          match env.TryFindValue "Symbol" with
          | Some binding -> binding.Type
          | None -> failwith "Symbol not in scope"

        let! propName =
          match
            getPropType
              ctx
              env
              symbolGlobal
              (PropName.String "customMatcher")
              false
              ValueCategory.RValue
          with
          | Result.Ok sym ->
            match (prune sym).Kind with
            | TypeKind.UniqueSymbol id -> PropName.Symbol id |> Result.Ok
            | _ ->
              Result.Error(
                TypeError.SemanticError
                  "Symbol.customMatcher is not a unique symbol"
              )
          | Result.Error _ ->
            Result.Error(
              TypeError.SemanticError "Symbol.customMatcher not found"
            )

        let! method = getPropType ctx env t2 propName false ValueCategory.RValue

        match method.Kind with
        | TypeKind.Function f ->
          let retType = f.Return

          match (prune retType).Kind with
          | TypeKind.Tuple { Elems = elems } ->
            if elems.Length <> extractor.Args.Length then
              return!
                Error(TypeError.SemanticError "Extractor arity mismatch 2")
            else
              // Check that each element in the tuple is assignable to the corresponding
              // element in the extractor args.  This matches what we do for destructuring.
              for arg, elem in List.zip extractor.Args elems do
                do! unify ctx env ips elem arg
          | _ ->
            return!
              Error(
                TypeError.SemanticError "Extractor return type is not a tuple"
              )
        | _ ->
          return!
            Error(
              TypeError.SemanticError
                "[Symbol.customMatcher] isn't a function/method"
            )
      | TypeKind.Object obj1, TypeKind.Object obj2 ->
        if not obj1.Immutable && obj2.Immutable then
          return! Error(TypeError.TypeMismatch(t1, t2))

        let isPattern =
          match t2.Provenance with
          | Some(Provenance.Pattern _) -> true
          | _ -> false

        do! unifyObjProps ctx env ips obj1 obj2 isPattern

      | TypeKind.Object obj, TypeKind.Intersection types ->
        let mutable combinedElems = []
        let mutable spreadTypes = []
        let mutable hasExactObjects = false

        // TODO: dedupe with TypeKind.Intersection, TypeKind.Object case
        for t in types do
          match t.Kind with
          | TypeKind.Object { Elems = elems; Exact = exact } ->
            combinedElems <- combinedElems @ elems
            hasExactObjects <- hasExactObjects || exact
          | TypeKind.RestSpread t -> spreadTypes <- t :: spreadTypes
          | TypeKind.TypeVar { Bound = Some bound } ->
            spreadTypes <- t :: spreadTypes
          | TypeKind.TypeRef _ ->
            let! t = expandType ctx env ips Map.empty t

            match t.Kind with
            | TypeKind.Object { Elems = elems; Exact = exact } ->
              combinedElems <- combinedElems @ elems
              hasExactObjects <- hasExactObjects || exact
            | TypeKind.RestSpread t -> spreadTypes <- t :: spreadTypes
            | _ ->
              printfn $"t is not an object types - {t}"
              return! Error(TypeError.TypeMismatch(t1, t2))
          | _ ->
            printfn $"t is not an object types - {t}"
            return! Error(TypeError.TypeMismatch(t1, t2))

        // TODO: Figure out how to check if the types wrapped by RestSpread are
        // exact or not

        let objType =
          if hasExactObjects then
            // Intersections with exact objects can't be satisfied unless all of
            // the types are exact and have the same properties.
            { Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }
          else
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = combinedElems
                    Exact = false
                    Immutable = false // TODO: figure out what do do with `Immutable`
                    Mutable = false // TODO: figure out what do do with `Mutable`
                    Interface = false }
              Provenance = None }

        match spreadTypes with
        | [] -> do! unify ctx env ips t1 objType
        | [ spreadType ] ->
          // objElems contains all the properties that exist in the t1 object
          // type and also in properties that are in one of the intersection types.
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
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = objElems
                    Exact = false
                    Immutable = false // TODO: figure out what do do with `Immutable`
                    Mutable = false // TODO: figure out what do do with `Mutable`
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips newObjType objType

          let newRestType =
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = restElems
                    Exact = false
                    Immutable = false // TODO: figure out what do do with `Immutable`
                    Mutable = false // TODO: figure out what do do with `Mutable`
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips newRestType spreadType
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Intersection types, TypeKind.Object obj ->
        let mutable combinedElems = []
        let mutable restTypes = []
        let mutable hasExactObjects = false

        // TODO: dedupe with TypeKind.Object, TypeKind.Intersection case
        for t in types do
          match t.Kind with
          | TypeKind.Object { Elems = elems; Exact = exact } ->
            combinedElems <- combinedElems @ elems
            hasExactObjects <- hasExactObjects || exact
          | TypeKind.RestSpread t -> restTypes <- t :: restTypes
          | TypeKind.TypeVar { Bound = Some bound } ->
            restTypes <- t :: restTypes
          | TypeKind.TypeRef { Name = name } ->
            let! t = expandType ctx env ips Map.empty t

            match t.Kind with
            | TypeKind.Object { Elems = elems; Exact = exact } ->
              combinedElems <- combinedElems @ elems
              hasExactObjects <- hasExactObjects || exact
            | TypeKind.RestSpread t -> restTypes <- t :: restTypes
            | _ -> failwith $"TODO: handle type {t} in intersection"
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        // TODO: Figure out how to check if the types wrapped by RestSpread are
        // exact or not

        let objType =
          if hasExactObjects then
            { Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }
          else
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = combinedElems
                    Exact = false
                    Immutable = false // TODO: figure out what do do with `Immutable`
                    Mutable = false // TODO: figure out what do do with `Mutable`
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
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = objElems
                    Exact = false
                    Immutable = false // TODO: figure out what do do with `Immutable`
                    Mutable = false // TODO: figure out what do do with `Mutable`
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips objType newObjType

          let newRestType =
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = restElems
                    Exact = false
                    Immutable = false // TODO: figure out what do do with `Immutable`
                    Mutable = false // TODO: figure out what do do with `Mutable`
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips restType newRestType
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | TypeKind.Intersection types1, TypeKind.Intersection types2 ->
        if types1.Length <> types2.Length then
          failwith "TODO: handle unify(intersection, intersection)"
        // return! Error(TypeError.TypeMismatch(t1, t2))
        else
          // NOTE: this doesn't handle all cases correctly but is sufficient
          // for enum variants
          for t1, t2 in List.zip types1 types2 do
            do! unify ctx env ips t1 t2
      | TypeKind.Union(types), _ ->
        let! _ = types |> List.traverseResultM (fun t -> unify ctx env ips t t2)

        return ()
      | _, TypeKind.Union(types) ->

        // TODO: check if ctx.Report.Diagnostics is empty
        let unifier =
          List.tryFind (fun t -> unify ctx env ips t1 t |> Result.isOk) types

        match unifier with
        | Some _ -> return ()
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
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

  let getNamedProps
    (valueCategory: ValueCategory)
    (elems: list<ObjTypeElem>)
    : Map<PropName, Property> =
    elems
    |> List.choose (fun (elem: ObjTypeElem) ->
      match elem with
      | Property p -> Some(p.Name, p)
      | Method { Name = name; Fn = fn } ->
        match valueCategory with
        | LValue -> None
        | RValue ->
          let t =
            { Kind = TypeKind.Function fn
              Provenance = None }

          let p =
            { Name = name
              Optional = false
              Readonly = false
              Type = t }

          Some(name, p)
      | Getter { Name = name; Fn = fn } ->
        match valueCategory with
        | LValue -> None
        | RValue ->
          let t =
            { Kind = TypeKind.Function fn
              Provenance = None }

          let p =
            { Name = name
              Optional = false
              // TODO: check there's also a setter with the same name
              Readonly = true
              Type = t }

          Some(name, p)
      | Setter { Name = name; Fn = fn } ->
        match valueCategory with
        | LValue ->
          let t =
            { Kind = TypeKind.Function fn
              Provenance = None }

          let p =
            { Name = name
              Optional = false
              Readonly = false
              Type = t }

          Some(name, p)
        | RValue -> None
      | _ -> None)
    |> Map.ofList

  let unifyObjProps
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (obj1: Object)
    (obj2: Object)
    (isPattern: bool)
    : Result<unit, TypeError> =
    result {
      if not obj1.Exact && obj2.Exact && not isPattern then
        return!
          Error(
            TypeError.SemanticError
              "Inexact object can't assigned to an exact object"
          )

      // TODO: rework how we check if an object is a subtype of another object
      // setters should have reversed variance
      // For now we're use `RValue` since we don't have many test cases
      // involving setters
      let namedProps1 = getNamedProps ValueCategory.RValue obj1.Elems
      let namedProps2 = getNamedProps ValueCategory.RValue obj2.Elems

      let mutable rest: option<Type> = None
      let mutable spread: option<Type> = None

      let undefined =
        { Kind = TypeKind.Literal(Literal.Undefined)
          Provenance = None }

      // When `IsPatternMatching` is true, the direction of assignability for
      // obj1 and obj2 is reversed.  This is because the type of the expression
      // being assigned is a union of types and the pattern we're assigning it
      // to is only one of those types.
      if env.IsPatternMatching then
        for elem in obj2.Elems do
          match elem with
          | ObjTypeElem.RestSpread t -> rest <- Some(t)
          | _ -> ()

        for elem in obj1.Elems do
          match elem with
          | ObjTypeElem.RestSpread t -> spread <- Some(t)
          | _ -> ()

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

        let mutable spreadProps = []

        for KeyValue(name, prop2) in namedProps2 do
          match namedProps1.TryFind name with
          | Some _ -> ()
          | None ->
            spreadProps <- (prop2 |> ObjTypeElem.Property) :: spreadProps

        match spread with
        | Some t ->
          let spreadObj =
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = List.rev spreadProps
                    Exact = false // TODO
                    Immutable = false // TODO
                    Mutable = false // TODO
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips spreadObj t
        | None -> ()
      else
        for elem in obj2.Elems do
          match elem with
          | ObjTypeElem.RestSpread t -> rest <- Some(t)
          | _ -> ()

        for elem in obj1.Elems do
          match elem with
          | ObjTypeElem.RestSpread t -> spread <- Some(t)
          | _ -> ()

        // TODO: collect all prop2's that don't have a matcher in namedProps1
        // and stuff them into the rest element type if there is one

        let mutable leftoverProperties = []

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
            if spread.IsSome then
              leftoverProperties <-
                (prop2 |> ObjTypeElem.Property) :: leftoverProperties
            else if not prop2.Optional then
              return! Error(TypeError.PropertyMissing(name))

        let mutable restProps = []

        for KeyValue(name, prop1) in namedProps1 do
          match namedProps2.TryFind name with
          | Some _ -> ()
          | None -> restProps <- (prop1 |> ObjTypeElem.Property) :: restProps

        match rest with
        | Some t ->
          let restObj =
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = List.rev restProps
                    Exact = obj1.Exact
                    Immutable = false // TODO
                    Mutable = false // TODO
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips t restObj
        | None ->
          if obj2.Exact && restProps.Length > 0 && not isPattern then
            return!
              Error(
                TypeError.SemanticError("Exact object has extra properties")
              )

          ()

        match spread with
        | Some spreadType ->
          let leftoverObjType =
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = List.rev leftoverProperties
                    Exact = false // TODO
                    Immutable = false // TODO
                    Mutable = false // TODO
                    Interface = false }
              Provenance = None }

          do! unify ctx env ips leftoverObjType spreadType
        | None -> ()
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
      | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                           TypeArgs = Some [ arrayElem1 ] },
        TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                           TypeArgs = Some [ arrayElem2 ] } ->
        do! unifyInvariant ctx env ips arrayElem1 arrayElem2
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
            // TODO: include that this is an invariant mis-match
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
            let types = types |> flatten |> List.filter (fun t -> t <> t1)

            match types with
            | [] -> return ()
            | [ t ] -> return! bind ctx env ips t1 t
            | types ->
              let t = union types
              return! bind ctx env ips t1 t
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

              // If `bound1` is a type variable with a bound that's an inexact
              // object type, we make the instance also inexact.  This is to
              // handle cases like:
              //   let foo = fn<U: T, T: {...}>(t: T, u: U) => u;
              //   let bar = foo({a: 5}, {a: 5, b: "hello"});
              let! bound1 =
                result {
                  match bound1.Kind with
                  | TypeKind.TypeVar({ Bound = Some b
                                       Instance = Some i
                                       Default = d } as tv) ->
                    let! b = expandType ctx env ips Map.empty b

                    match b.Kind, i.Kind with
                    | TypeKind.Object bObj, TypeKind.Object iObj ->
                      if not bObj.Exact then
                        // Force the instance to be an inexact object type to
                        // match the exactness of bound1's bound.
                        let i =
                          { i with
                              Kind = TypeKind.Object { iObj with Exact = false } }

                        return
                          { bound1 with
                              Kind =
                                TypeKind.TypeVar { tv with Instance = Some i } }
                      else
                        return bound1
                    | _ -> return bound1
                  | _ -> return bound1
                }

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

  let number: FParsec.Primitives.Parser<Number, unit> =
    fun stream ->
      let intReply = many1Satisfy isDigit stream

      match intReply.Status with
      | FParsec.Primitives.Ok ->
        if stream.PeekString(2) = ".." then
          FParsec.Reply(Number.Int(int intReply.Result))
        else if stream.PeekString(1) = "." then
          let index = stream.Index
          stream.Skip(1)
          let decReply = many1Satisfy isDigit stream

          match decReply.Status with
          | FParsec.Primitives.Ok ->
            let number = intReply.Result + "." + decReply.Result
            FParsec.Reply(Number.Float(float number))
          | FParsec.Primitives.Error ->
            stream.Seek(index)
            FParsec.Reply(Number.Int(int intReply.Result))
          | _ -> FParsec.Reply(decReply.Status, decReply.Error)
        else
          FParsec.Reply(Number.Int(int intReply.Result))
      | _ -> FParsec.Reply(intReply.Status, intReply.Error)

  let (|>>) = FParsec.Primitives.(|>>)
  let (<|>) = FParsec.Primitives.(<|>)
  let (.>>) = FParsec.Primitives.(.>>)
  let (>>.) = FParsec.Primitives.(>>.)

  let parserForType
    (ctx: Ctx)
    (env: Env)
    (parser: FParsec.Primitives.Parser<unit, unit>)
    (t: Type)
    : FParsec.Primitives.Parser<unit, unit> =

    let rec fold (t: Type) : FParsec.Primitives.Parser<unit, unit> =
      match t.Kind with
      | TypeKind.Literal lit ->
        match lit with
        | Literal.String s -> pstring s
        | Literal.Boolean b -> pstring (string b)
        | Literal.Number n -> pstring (string n)
        | _ -> failwith $"TODO: parserForType - lit = ${lit}"
        |>> ignore
      | TypeKind.Primitive Primitive.Number -> number |>> ignore
      | TypeKind.Primitive Primitive.Boolean ->
        (pstring "true" <|> pstring "false") |>> ignore
      | TypeKind.Primitive Primitive.String ->
        // This produces the same behaviour as `*.?` would in a regex
        FParsec.Primitives.many (
          FParsec.Primitives.notFollowedBy parser >>. anyChar
        )
        |>> ignore
      | TypeKind.Union elems -> FParsec.Primitives.choice (List.map fold elems)
      | TypeKind.IntrinsicInstance { Name = name; TypeArgs = typeArgs } ->

        let t =
          match typeArgs with
          | Some [ t ] -> t
          | _ -> failwith $"TODO: parserForType - typeArgs = {typeArgs}"

        let t =
          match expandType ctx env None Map.empty t with
          | Ok t -> t
          | Error _ -> failwith $"parserForType - failed to expand type {t}"

        match t.Kind with
        | TypeKind.Primitive Primitive.String ->
          match name with
          | QualifiedIdent.Ident "Uppercase" ->
            FParsec.Primitives.many (
              FParsec.Primitives.notFollowedBy parser >>. satisfy isUpper
            )
            |>> ignore
          | QualifiedIdent.Ident "Lowercase" ->
            FParsec.Primitives.many (
              FParsec.Primitives.notFollowedBy parser >>. satisfy isLower
            )
            |>> ignore
          | QualifiedIdent.Ident "Capitalize" ->
            satisfy isUpper
            >>. FParsec.Primitives.many (
              FParsec.Primitives.notFollowedBy parser >>. satisfy isLower
            )
            |>> ignore
          | QualifiedIdent.Ident "Uncapitalize" ->
            satisfy isLower
            >>. FParsec.Primitives.many (
              FParsec.Primitives.notFollowedBy parser >>. satisfy isUpper
            )
            |>> ignore
          | _ ->
            fun (stream: FParsec.CharStream<_>) ->
              FParsec.Reply(
                FParsec.Primitives.Error,
                FParsec.Error.messageError (
                  $"Invalid intrinsic instance: {name}"
                )
              )
        | TypeKind.Literal(Literal.String s) ->
          match name with
          | QualifiedIdent.Ident "Uppercase" -> pstring (s.ToUpper()) |>> ignore
          | QualifiedIdent.Ident "Lowercase" -> pstring (s.ToUpper()) |>> ignore
          | QualifiedIdent.Ident "Capitalize" ->
            pstring (s[0].ToString().ToUpper() + s[1..].ToLower()) |>> ignore
          | QualifiedIdent.Ident "Uncapitalize" ->
            pstring (s[0].ToString().ToLower() + s[1..].ToUpper()) |>> ignore
          | _ ->
            fun (stream: FParsec.CharStream<_>) ->
              FParsec.Reply(
                FParsec.Primitives.Error,
                FParsec.Error.messageError (
                  $"Invalid intrinsic instance: {name}"
                )
              )
        | TypeKind.Union types ->
          let types =
            types
            |> List.map (fun t ->
              let kind =
                TypeKind.IntrinsicInstance
                  { Name = name; TypeArgs = Some [ t ] }

              { Kind = kind; Provenance = None })

          fold (union types)
        | _ ->
          fun (stream: FParsec.CharStream<_>) ->
            FParsec.Reply(
              FParsec.Primitives.Error,
              FParsec.Error.messageError ($"Invalid intrinsic instance: {name}")
            )
      | TypeKind.TypeRef _ ->
        match expandType ctx env None Map.empty t with
        | Ok t -> fold t
        | Error _ ->
          fun (stream: FParsec.CharStream<_>) ->
            FParsec.Reply(
              FParsec.Primitives.Error,
              FParsec.Error.messageError ($"Failed to expand type: {t}")
            )
      | TypeKind.TypeVar _ ->
        FParsec.Primitives.many (
          FParsec.Primitives.notFollowedBy parser >>. anyChar
        )
        |>> (fun chars ->
          let stringType =
            { Kind =
                TypeKind.Literal(Literal.String(System.String.Concat(chars)))
              Provenance = None }

          match unify ctx env None t stringType with
          | Ok _ -> ()
          | Error err -> failwith "TODO: report this error correctly")
      | _ ->
        printfn "t = %A" t
        failwith $"TODO: parserForType - t = {t}"

    fold t .>> parser

  let checkTemplateLiteral
    (ctx: Ctx)
    (env: Env)
    (s: string)
    (tl: TemplateLiteral<Type>)
    : bool =
    let { Parts = parts; Exprs = exprs } = tl

    let firstPart, restParts = List.head parts, List.tail parts
    let pairs = List.zip exprs restParts

    let mutable parser: FParsec.Primitives.Parser<unit, unit> = eof

    for expr, part in List.rev pairs do
      if part.Length > 0 then
        parser <- (pstring part |>> ignore) .>> parser

      parser <- parserForType ctx env parser expr

    if firstPart.Length > 0 then
      parser <- (pstring firstPart |>> ignore) .>> parser

    match run parser s with
    | Success((), _, pos) -> true
    | Failure(str, err, state) ->
      // TODO: Do a better job of bubbling up errors from the parser so that
      // they can be reported to the user.
      printfn $"failed to parse {str}"
      let head = err.Messages.Head
      printfn $"message: {head}"
      false

  let isUppercase (s: string) : bool = s |> Seq.forall isUpper
  let isLowercase (s: string) : bool = s |> Seq.forall isLower

  let isCapitalize (s: string) : bool =
    match s with
    | "" -> true
    | _ -> isUpper s.[0]

  let isUncapitalize (s: string) : bool =
    match s with
    | "" -> true
    | _ -> isLower s.[0]
