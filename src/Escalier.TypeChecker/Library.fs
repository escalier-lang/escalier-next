namespace Escalier.TypeChecker

open System.Collections.Generic
open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Syntax
open Escalier.Data.Type

module Errors =
  type TypeError =
    | NotImplemented of string
    | SemanticError of string
    | NotInferred
    | TypeMismatch of Type * Type
    | RecursiveUnification
    | WrongNumberOfTypeArgs

module TypeVariable =
  let mutable nextVariableId = 0

  let makeVariable bound =
    let newVar =
      { Id = nextVariableId
        Bound = bound
        Instance = None }

    nextVariableId <- nextVariableId + 1

    { Kind = TypeVar(newVar)
      Provenance = None }

module rec TypeChecker =
  open Errors

  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>
  type SchemeAssump = string * Scheme

  type Env =
    { Values: Map<string, Binding>
      Schemes: Map<string, Scheme>
      IsAsync: bool }

    member this.AddValue (name: string) (binding: Binding) =
      { this with
          Values = Map.add name binding this.Values }

    member this.AddScheme (name: string) (s: Scheme) =
      { this with
          Schemes = Map.add name s this.Schemes }

  let makePrimitiveKind name =
    { Name = name
      TypeArgs = None
      Scheme = None }
    |> TypeKind.TypeRef

  let makeFunctionType typeParams paramList ret =
    let never =
      { Kind = makePrimitiveKind "never"
        Provenance = None }

    { Kind =
        Function
          { TypeParams = typeParams
            ParamList = paramList
            Return = ret
            Throws = never }
      Provenance = None }

  let numType =
    { Kind = makePrimitiveKind "number"
      Provenance = None }

  let boolType =
    { Kind = makePrimitiveKind "boolean"
      Provenance = None }

  let strType =
    { Kind = makePrimitiveKind "string"
      Provenance = None }

  /// Returns the currently defining instance of t.
  /// As a side effect, collapses the list of type instances. The function Prune
  /// is used whenever a type expression has to be inspected: it will always
  /// return a type expression which is either an uninstantiated type variable or
  /// a type operator; i.e. it will skip instantiated variables, and will
  /// prune them from expressions to remove long chains of instantiated variables.
  let prune (t: Type) : Type =
    match t.Kind with
    | TypeVar({ Instance = Some(instance) } as v) ->
      let newInstance = prune instance
      v.Instance <- Some(newInstance)
      newInstance
    | _ -> t

  let foldType (f: Type -> option<Type>) (t: Type) : Type =
    let rec fold (t: Type) : Type =
      let t = prune t

      let t =
        match t.Kind with
        | TypeVar _ -> t
        | Function f ->
          { Kind =
              TypeKind.Function
                { f with
                    ParamList =
                      List.map
                        (fun param -> { param with Type = fold param.Type })
                        f.ParamList
                    // TODO: fold TypeParams
                    Return = fold f.Return }
            Provenance = None }
        | Tuple(elems) ->
          let elems = List.map fold elems

          { Kind = Tuple(elems)
            Provenance = None }
        | TypeRef({ Name = name
                    TypeArgs = typeArgs
                    Scheme = scheme }) ->
          let typeArgs = Option.map (List.map fold) typeArgs

          let scheme =
            Option.map
              (fun (scheme: Scheme) -> { scheme with Type = fold scheme.Type })
              scheme

          { Kind =
              TypeRef(
                { Name = name
                  TypeArgs = typeArgs
                  Scheme = scheme }
              )
            Provenance = None }
        | Literal _ -> t
        | Wildcard -> t
        | Object elems ->
          let elems =
            List.map
              (fun elem ->
                match elem with
                | Property p -> Property { p with Type = fold p.Type }
                | _ -> failwith "TODO: foldType - ObjTypeElem")
              elems

          { Kind = Object(elems)
            Provenance = None }
        | Rest t ->
          { Kind = Rest(fold t)
            Provenance = None }
        | Union types ->
          { Kind = Union(List.map fold types)
            Provenance = None }
        | Intersection types ->
          { Kind = Intersection(List.map fold types)
            Provenance = None }
        | Array t ->
          { Kind = Array(fold t)
            Provenance = None }
        | KeyOf t ->
          { Kind = KeyOf(fold t)
            Provenance = None }
        | Index(target, index) ->
          { Kind = Index(fold target, fold index)
            Provenance = None }
        | Condition(check, extends, trueType, falseType) ->
          { Kind =
              Condition(fold check, fold extends, fold trueType, fold falseType)
            Provenance = None }
        | Infer _ -> t
        | Binary(left, op, right) ->
          { Kind = Binary(fold left, op, fold right)
            Provenance = None }

      match f t with
      | Some(t) -> t
      | None -> t

    fold t

  let generalizeFunc (f: Function) : Function =
    let mutable mapping: Map<int, string> = Map.empty
    let mutable nextId = 0

    // QUESTION: should we call `prune` inside the folder as well?
    let folder t =
      match t.Kind with
      | TypeVar { Id = id } ->
        match Map.tryFind id mapping with
        | Some(name) ->
          Some(
            { Kind =
                TypeRef
                  { Name = name
                    TypeArgs = None
                    Scheme = None }
              Provenance = None }
          )
        | None ->
          let tpName = 65 + nextId |> char |> string
          nextId <- nextId + 1
          mapping <- mapping |> Map.add id tpName

          Some(
            { Kind =
                TypeRef
                  { Name = tpName
                    TypeArgs = None
                    Scheme = None }
              Provenance = None }
          )
      | _ -> None

    let paramList =
      List.map
        (fun (p: FuncParam) -> { p with Type = foldType folder p.Type })
        f.ParamList

    let ret = foldType folder f.Return
    let throws = foldType folder f.Throws

    let values = mapping.Values |> List.ofSeq

    let mutable newTypeParams: list<TypeParam> =
      List.map
        (fun name ->
          { Name = name
            Constraint = None
            Default = None })
        values

    Option.iter
      (fun typeParams ->
        for param in typeParams do
          newTypeParams <- newTypeParams @ [ param ])
      f.TypeParams

    { TypeParams = if newTypeParams.IsEmpty then None else Some(newTypeParams)
      ParamList = paramList
      Return = ret
      Throws = throws }

  let instantiateFunc
    (f: Function)
    (typeArgs: option<list<Type>>)
    : Result<Function, TypeError> =

    result {
      let mutable mapping: Map<string, Type> = Map.empty

      let folder t =
        match t.Kind with
        | TypeRef({ Name = name }) ->
          match Map.tryFind name mapping with
          | Some(tv) -> Some(tv)
          | None -> None
        | _ -> None

      match f.TypeParams with
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
              mapping.Add(tp.Name, TypeVariable.makeVariable tp.Constraint)
      | None -> ()

      return
        { TypeParams = None
          ParamList =
            List.map
              (fun param ->
                { param with
                    Type = foldType folder param.Type })
              f.ParamList
          Return = foldType folder f.Return
          Throws = foldType folder f.Throws }
    }

  let occursInType (v: Type) (t2: Type) : bool =
    match (prune t2).Kind with
    | pruned when pruned = v.Kind -> true
    | TypeRef({ TypeArgs = typeArgs }) ->
      match typeArgs with
      | Some(typeArgs) -> occursIn v typeArgs
      | None -> false
    | _ -> false

  let occursIn (t: Type) (types: list<Type>) : bool =
    List.exists (occursInType t) types

  let bind (t1: Type) (t2: Type) =
    result {
      if t1.Kind <> t2.Kind then
        if occursInType t1 t2 then
          return! Error(TypeError.RecursiveUnification)

        match t1.Kind with
        | TypeKind.TypeVar(v) ->
          v.Instance <- Some(t2)
          return ()
        | _ -> return! Error(TypeError.NotImplemented "bind error")
    }

  let isIntegerLiteral (name: string) =
    match
      (try
        Some(int name)
       with _ex ->
         None)
    with
    | None -> false
    | Some _ -> true

  let isGeneric v nonGeneric = not (nonGeneric |> Set.contains v)

  ///Makes a copy of a type expression.
  ///The type t is copied. The the generic variables are duplicated and the
  ///nonGeneric variables are shared.
  let fresh (t: Type) (nonGeneric: Set<int>) : Type =
    let table = Dictionary<int, Type>()

    let rec loop tp =
      let t = prune tp

      match t.Kind with
      | TypeVar p ->
        if isGeneric p.Id nonGeneric then
          match table.ContainsKey p.Id with
          | false ->
            let newVar = TypeVariable.makeVariable None
            table.Add(p.Id, newVar)
            newVar
          | true -> table[p.Id]
        else
          t
      | Tuple elems ->
        { Kind = Tuple(List.map loop elems)
          Provenance = None }
      | Function f ->
        makeFunctionType
          f.TypeParams
          (List.map
            (fun param -> { param with Type = loop param.Type })
            f.ParamList)
          (loop f.Return)
      | TypeRef({ TypeArgs = typeArgs } as op) ->
        let kind =
          TypeRef(
            { op with
                TypeArgs = Option.map (List.map loop) typeArgs }
          )

        { Kind = kind; Provenance = None }
      | Literal _ -> t
      | _ -> t

    loop t

  ///Get the type of identifier name from the type environment env
  let getType (name: string) (env: Env) (nonGeneric: Set<int>) : Type =
    match env.Values |> Map.tryFind name with
    | Some(var) ->
      // TODO: check `isMut` and return an immutable type if necessary
      let (t, isMut) = var
      fresh t nonGeneric
    | None ->
      if isIntegerLiteral name then
        numType
      else
        failwithf $"Undefined symbol {name}"

  ///Unify the two types t1 and t2. Makes the types t1 and t2 the same.
  let unify (env: Env) (t1: Type) (t2: Type) : Result<unit, TypeError> =
    // printfn $"unify {t1} {t2}"

    result {
      match (prune t1).Kind, (prune t2).Kind with
      | TypeVar _, _ -> do! bind t1 t2
      | _, TypeVar _ -> do! unify env t2 t1
      | Tuple(elems1), Tuple(elems2) ->
        if List.length elems1 <> List.length elems2 then
          return! Error(TypeError.TypeMismatch(t1, t2))

        ignore (List.map2 (unify env) elems1 elems2)
      | Function(f1), Function(f2) ->
        // TODO: check if `f1` and `f2` have the same type params
        let! f1 = instantiateFunc f1 None
        let! f2 = instantiateFunc f2 None

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
          do! unify env param2 param1 // params are contravariant

        do! unify env f1.Return f2.Return // returns are covariant
      | TypeRef({ Name = name1; TypeArgs = types1 }),
        TypeRef({ Name = name2; TypeArgs = types2 }) ->

        if (name1 <> name2) then
          return! Error(TypeError.TypeMismatch(t1, t2))

        match (types1, types2) with
        | None, None -> ()
        | Some(types1), Some(types2) ->
          if List.length types1 <> List.length types2 then
            return! Error(TypeError.TypeMismatch(t1, t2))

          ignore (List.map2 (unify env) types1 types2)
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | Literal lit, TypeRef({ Name = name; TypeArgs = typeArgs }) ->
        // TODO: check that `typeArgs` is `None`
        match lit, name with
        | Literal.Number _, "number" -> ()
        | Literal.String _, "string" -> ()
        | Literal.Boolean _, "boolean" -> ()
        | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | Literal l1, Literal l2 ->
        match l1, l2 with
        | Literal.Number n1, Literal.Number n2 when n1 = n2 -> ()
        | Literal.String s1, Literal.String s2 when s1 = s2 -> ()
        | Literal.Boolean b1, Literal.Boolean b2 when b1 = b2 -> ()
        | Literal.Null, Literal.Null -> ()
        | Literal.Undefined, Literal.Undefined -> ()
        | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | Object elems1, Object elems2 ->

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

            do! unify env p1Type p2Type
          | None ->
            if not prop2.Optional then
              return! Error(TypeError.TypeMismatch(t1, t2))

      | Object allElems, Intersection types ->
        let mutable combinedElems = []
        let mutable restTypes = []

        for t in types do
          match t.Kind with
          | Object elems -> combinedElems <- combinedElems @ elems
          | Rest t -> restTypes <- t :: restTypes
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        let objType =
          { Kind = TypeKind.Object(combinedElems)
            Provenance = None }

        match restTypes with
        | [] -> do! unify env t1 objType
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

          do! unify env newObjType objType

          let newRestType =
            { Kind = TypeKind.Object(restElems)
              Provenance = None }

          do! unify env newRestType restType
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | Intersection types, Object allElems ->
        let mutable combinedElems = []
        let mutable restTypes = []

        for t in types do
          match t.Kind with
          | Object elems -> combinedElems <- combinedElems @ elems
          | Rest t -> restTypes <- t :: restTypes
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        let objType =
          { Kind = TypeKind.Object(combinedElems)
            Provenance = None }

        match restTypes with
        | [] -> do! unify env objType t2
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

          do! unify env objType newObjType

          let newRestType =
            { Kind = TypeKind.Object(restElems)
              Provenance = None }

          do! unify env restType newRestType
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | _, TypeKind.Union(types) ->

        let unifier =
          List.tryFind (fun t -> unify env t1 t |> Result.isOk) types

        match unifier with
        | Some _ -> return ()
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

      | _, _ ->

        let t1' = expandType env t1
        let t2' = expandType env t2

        if t1' <> t1 || t2' <> t2 then
          return! unify env t1' t2'
        else
          return! Error(TypeError.TypeMismatch(t1, t2))
    }

  let unifyCall
    (args: list<Expr>)
    (typeArgs: option<list<Type>>)
    (callee: Type)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<(Type * Type), TypeError> =

    result {
      let callee = prune callee

      let retType = TypeVariable.makeVariable None
      let throwsType = TypeVariable.makeVariable None

      match callee.Kind with
      | TypeKind.Function func ->
        return!
          unifyFuncCall args typeArgs retType throwsType func env nonGeneric
      | TypeKind.TypeVar _ ->

        // TODO: use a `result {}` CE here
        let! argTypes =
          List.traverseResultM (fun arg -> inferExpr arg env nonGeneric) args

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
                  Return = retType
                  Throws = throwsType
                  TypeParams = None } // TODO
            Provenance = None }

        match bind callee callType with
        | Ok _ -> return (prune retType, prune throwsType)
        | Error e -> return! Error e
      | kind -> return! Error(TypeError.NotImplemented $"kind = {kind}")
    }

  let unifyFuncCall
    (args: list<Expr>)
    (typeArgs: option<list<Type>>)
    (retType: Type)
    (throwsType: Type)
    (callee: Function)
    (env: Env)
    (nonGeneric: Set<int>)
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
              let! argType = inferExpr arg env nonGeneric
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
          do! unify env argType param.Type // contravariant

      do! unify env retType callee.Return // covariant
      do! unify env throwsType callee.Throws // covariant

      return (retType, throwsType)
    }

  let expandScheme
    (env: Env)
    (scheme: Scheme)
    (typeArgs: option<list<Type>>)
    : Type =

    match scheme.TypeParams, typeArgs with
    | None, None -> expandType env scheme.Type
    | _ -> failwith "TODO: expandScheme with type params/args"

  let expandType (env: Env) (t: Type) : Type =
    let rec expand t =
      let t = prune t

      match t.Kind with
      | KeyOf t -> failwith "TODO: expand keyof"
      | Index(target, index) -> failwith "TODO: expand index"
      | Condition _ -> failwith "TODO: expand condition"
      | Binary(left, op, right) -> failwith "TODO: expand binary"
      // TODO: instead of expanding object types, we should try to
      // look up properties on the object type without expanding it
      // since expansion can be quite expensive
      | Object _ -> t
      | TypeRef { Name = name
                  TypeArgs = typeArgs
                  Scheme = scheme } ->
        let t =
          match scheme with
          | Some scheme -> expandScheme env scheme typeArgs
          | None ->
            match env.Schemes.TryFind name with
            | Some scheme -> expandScheme env scheme typeArgs
            | None -> failwith $"{name} is not in scope"

        expand t
      | _ -> t

    expand t

  ///Computes the type of the expression given by node.
  ///The type of the node is computed in the context of the
  ///supplied type environment env. Data types can be introduced into the
  ///language simply by having a predefined set of identifiers in the initial
  ///environment. environment; this way there is no need to change the syntax or, more
  ///importantly, the type-checking program when extending the language.
  let inferExpr
    (expr: Expr)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<Type, TypeError> =
    let r =
      result {
        match expr.Kind with
        | ExprKind.Identifier(name) -> return getType name env nonGeneric
        | ExprKind.Literal(literal) ->
          return
            { Type.Kind = Literal(literal)
              Provenance = None }
        | ExprKind.Call call ->
          let! callee = inferExpr call.Callee env nonGeneric

          let! result, throws = unifyCall call.Args None callee env nonGeneric

          // TODO: handle throws

          return result
        | ExprKind.Binary(op, left, right) ->
          let funTy = getType op env nonGeneric

          let! result, throws =
            unifyCall [ left; right ] None funTy env nonGeneric

          // TODO: handle throws

          return result
        | ExprKind.Function f ->
          let mutable newEnv = env
          let mutable newNonGeneric = nonGeneric

          let! paramList =
            List.traverseResultM
              (fun (param: Syntax.FuncParam<option<TypeAnn>>) ->
                result {
                  let! paramType =
                    match param.TypeAnn with
                    | Some(typeAnn) -> inferTypeAnn env typeAnn
                    | None -> Result.Ok(TypeVariable.makeVariable None)
                  // TypeVariable.makeVariable None in
                  let! assumps, patternType = inferPattern env param.Pattern
                  do! unify env patternType paramType

                  for KeyValue(name, binding) in assumps do
                    // TODO: update `Env.types` to store `Binding`s insetad of `Type`s
                    newEnv <- newEnv.AddValue name binding

                  match paramType.Kind with
                  | TypeVar { Id = id } ->
                    newNonGeneric <- newNonGeneric |> Set.add id
                  | _ -> ()

                  return
                    { Pattern = patternToPattern param.Pattern
                      Type = paramType
                      Optional = false }
                })
              f.Sig.ParamList

          inferBlockOrExpr newEnv newNonGeneric f.Body |> ignore

          let retExprs = findReturns f

          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          let! retType =
            result {
              match retExprs with
              | [] -> return undefined
              | [ expr ] ->
                match expr.InferredType with
                | Some(t) -> return t
                | None -> return! Error(TypeError.SemanticError "")
              | exprs ->
                let! types =
                  List.traverseResultM
                    (fun (expr: Expr) ->
                      match expr.InferredType with
                      | Some(t) -> Ok t
                      | None -> Error(TypeError.SemanticError ""))
                    exprs

                return
                  { Kind = TypeKind.Union types
                    Provenance = None }
            }

          // TODO: move this up so that we can reference any type params in the
          // function params, body, return or throws
          let! typeParams =
            match f.Sig.TypeParams with
            | Some(typeParams) ->
              List.traverseResultM (inferTypeParam env) typeParams
              |> Result.map Some
            | None -> Ok None

          return makeFunctionType typeParams paramList retType
        | ExprKind.Tuple elems ->
          let! elems =
            List.traverseResultM
              (fun elem -> inferExpr elem env nonGeneric)
              elems

          return
            { Type.Kind = TypeKind.Tuple(elems)
              Provenance = None }
        | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
          let! conditionTy = inferExpr condition env nonGeneric

          let! thenBranchTy =
            inferBlockOrExpr env nonGeneric (thenBranch |> BlockOrExpr.Block)

          let! elseBranchTy =
            Option.traverseResult (inferBlockOrExpr env nonGeneric) elseBranch

          do! unify env conditionTy boolType

          return
            match elseBranchTy with
            | Some(elseBranchTy) -> union [ thenBranchTy; elseBranchTy ]
            | None ->
              { Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }
        | ExprKind.Object elems ->
          let mutable spreadTypes = []

          let! elems =
            List.traverseResultM
              (fun (elem: ObjElem) ->
                result {
                  match elem with
                  | ObjElem.Property(_span, key, value) ->
                    let! t = inferExpr value env nonGeneric

                    return
                      Some(
                        Property
                          { Name = key
                            Optional = false
                            Readonly = false
                            Type = t }
                      )
                  | ObjElem.Shorthand(_span, key) ->
                    let value = getType key env nonGeneric

                    return
                      Some(
                        Property
                          { Name = key
                            Optional = false
                            Readonly = false
                            Type = value }
                      )
                  | ObjElem.Spread(span, value) ->
                    let! t = inferExpr value env nonGeneric
                    spreadTypes <- t :: spreadTypes
                    return None
                })
              elems

          let elems = elems |> List.choose id

          let objType =
            { Kind = Object(elems)
              Provenance = None }

          match spreadTypes with
          | [] -> return objType
          | _ ->
            return
              { Kind = Intersection([ objType ] @ spreadTypes)
                Provenance = None }
        | Member(obj, prop, optChain) ->
          let! objType = inferExpr obj env nonGeneric

          // TODO: handle optional chaining
          // TODO: lookup properties on object type
          return getPropType env objType prop optChain
        | _ ->
          printfn "expr.Kind = %A" expr.Kind

          return!
            Error(
              TypeError.NotImplemented "TODO: finish implementing infer_expr"
            )
      }

    Result.map
      (fun t ->
        expr.InferredType <- Some(t)
        t)
      r

  let getPropType (env: Env) (t: Type) (name: string) (optChain: bool) : Type =
    let t = prune t

    match t.Kind with
    | Object elems ->
      let elems =
        List.choose
          (fun (elem: ObjTypeElem) ->
            match elem with
            | Property p -> Some(p.Name, p)
            | _ -> None)
          elems
        |> Map.ofList

      match elems.TryFind name with
      | Some(p) ->
        match p.Optional with
        | true ->
          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          union [ p.Type; undefined ]
        | false -> p.Type
      | None -> failwithf $"Property {name} not found"
    | TypeRef { Name = typeRefName
                Scheme = scheme
                TypeArgs = typeArgs } ->
      match scheme with
      | Some scheme ->
        getPropType env (expandScheme env scheme typeArgs) name optChain
      | None ->
        match env.Schemes.TryFind typeRefName with
        | Some scheme ->
          getPropType env (expandScheme env scheme typeArgs) name optChain
        | None -> failwithf $"{name} not in scope"
    | Union types ->
      let undefinedTypes, definedTypes =
        List.partition
          (fun t -> t.Kind = TypeKind.Literal(Literal.Undefined))
          types

      if undefinedTypes.IsEmpty then
        failwith "TODO: lookup member on union type"
      else if not optChain then
        failwith "Can't lookup property on undefined"
      else
        match definedTypes with
        | [ t ] ->
          let t = getPropType env t name optChain

          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          union [ t; undefined ]
        | _ -> failwith "TODO: lookup member on union type"

    // TODO: intersection types
    // TODO: union types
    | _ -> failwith $"TODO: lookup member on type - {t}"

  let inferBlockOrExpr
    (env: Env)
    (nonGeneric: Set<int>)
    (blockOrExpr: BlockOrExpr)
    : Result<Type, TypeError> =
    result {
      let mutable newEnv = env

      match blockOrExpr with
      | BlockOrExpr.Block({ Stmts = stmts }) ->
        let! stmtResults =
          List.traverseResultM
            (fun stmt ->
              result {
                let! t, stmtResult = inferStmt stmt newEnv nonGeneric

                match stmtResult with
                | Some(StmtResult.Bindings assumps) ->
                  for KeyValue(name, binding) in assumps do
                    newEnv <- newEnv.AddValue name binding
                | Some(StmtResult.Scheme(name, scheme)) ->
                  newEnv <- newEnv.AddScheme name scheme
                | None -> ()

                return t
              })
            stmts

        let undefined =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Provenance = None }

        match List.tryLast stmtResults with
        | Some(Some(t)) -> return t
        | _ -> return undefined
      | BlockOrExpr.Expr expr -> return! inferExpr expr newEnv nonGeneric
    }

  let inferTypeParam
    (env: Env)
    (tp: Syntax.TypeParam)
    : Result<TypeParam, TypeError> =
    result {
      let! c =
        match tp.Constraint with
        | Some(c) -> inferTypeAnn env c |> Result.map Some
        | None -> Ok None

      let! d =
        match tp.Default with
        | Some(d) -> inferTypeAnn env d |> Result.map Some
        | None -> Ok None

      return
        { Name = tp.Name
          Constraint = c
          Default = d }
    }

  let inferTypeAnn (env: Env) (typeAnn: TypeAnn) : Result<Type, TypeError> =
    let kind: Result<TypeKind, TypeError> =
      result {
        match typeAnn.Kind with
        | TypeAnnKind.Array elem ->
          let! elem = inferTypeAnn env elem
          return TypeKind.Array elem
        | TypeAnnKind.Literal lit -> return TypeKind.Literal(lit)
        | TypeAnnKind.Keyword keyword ->
          match keyword with
          | KeywordTypeAnn.Boolean -> return makePrimitiveKind "boolean"
          | KeywordTypeAnn.Number -> return makePrimitiveKind "number"
          | KeywordTypeAnn.String -> return makePrimitiveKind "string"
          | KeywordTypeAnn.Symbol -> return makePrimitiveKind "symbol"
          | KeywordTypeAnn.Null -> return TypeKind.Literal(Literal.Null)
          | KeywordTypeAnn.Undefined ->
            return TypeKind.Literal(Literal.Undefined)
          | KeywordTypeAnn.Unknown -> return makePrimitiveKind "unknown"
          | KeywordTypeAnn.Never -> return makePrimitiveKind "never"
          | KeywordTypeAnn.Object -> return makePrimitiveKind "object"
        | TypeAnnKind.Object elems ->
          let! elems =
            List.traverseResultM
              (fun (elem: ObjTypeAnnElem) ->
                result {
                  match elem with
                  | ObjTypeAnnElem.Property { Name = name
                                              TypeAnn = typeAnn
                                              Optional = optional
                                              Readonly = readonly } ->
                    let! t = inferTypeAnn env typeAnn

                    return
                      Property
                        { Name = name
                          Type = t
                          Optional = optional
                          Readonly = readonly }
                  | ObjTypeAnnElem.Callable ``function`` ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Constructor ``function`` ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Method(name, isMut, ``type``) ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Getter(name, returnType, throws) ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Setter(name, param, throws) ->
                    return! Error(TypeError.NotImplemented "todo")
                })
              elems

          return TypeKind.Object(elems)
        | TypeAnnKind.Tuple elems ->
          let! elems = List.traverseResultM (inferTypeAnn env) elems
          return TypeKind.Tuple(elems)
        | TypeAnnKind.Union types ->
          let! types = List.traverseResultM (inferTypeAnn env) types
          return TypeKind.Union(types)
        | TypeAnnKind.Intersection types ->
          let! types = List.traverseResultM (inferTypeAnn env) types
          return TypeKind.Intersection types
        | TypeAnnKind.TypeRef(name, typeArgs) ->
          match typeArgs with
          | Some(typeArgs) ->
            let! typeArgs = List.traverseResultM (inferTypeAnn env) typeArgs

            return
              { Name = name
                TypeArgs = Some(typeArgs)
                Scheme = None }
              |> TypeKind.TypeRef
          | None ->
            return
              { Name = name
                TypeArgs = None
                Scheme = None }
              |> TypeKind.TypeRef
        | TypeAnnKind.Function functionType ->
          let! returnType = inferTypeAnn env functionType.ReturnType

          let! throws =
            match functionType.Throws with
            | Some(throws) -> inferTypeAnn env throws
            | None ->
              Result.Ok(
                { Type.Kind = makePrimitiveKind "never"
                  Provenance = None }
              )

          let! paramList =
            List.traverseResultM
              (fun (p: FuncParam<TypeAnn>) ->
                result {
                  let! t = inferTypeAnn env p.TypeAnn
                  let pattern = patternToPattern p.Pattern

                  return
                    { Pattern = pattern
                      Type = t
                      Optional = false }
                })
              functionType.ParamList

          let f =
            { TypeParams = None
              ParamList = paramList
              Return = returnType
              Throws = throws }

          return TypeKind.Function(f)
        | TypeAnnKind.Keyof target ->
          return! inferTypeAnn env target |> Result.map TypeKind.KeyOf
        | TypeAnnKind.Rest target ->
          return! inferTypeAnn env target |> Result.map TypeKind.Rest
        | TypeAnnKind.Typeof target ->
          return! Error(TypeError.NotImplemented "TODO: inferTypeAnn - Typeof") // TODO: add Typeof to TypeKind
        | TypeAnnKind.Index(target, index) ->
          let! target = inferTypeAnn env target
          let! index = inferTypeAnn env index
          return TypeKind.Index(target, index)
        | TypeAnnKind.Condition conditionType ->
          let! check = inferTypeAnn env conditionType.Check
          let! extends = inferTypeAnn env conditionType.Extends
          let! trueType = inferTypeAnn env conditionType.TrueType
          let! falseType = inferTypeAnn env conditionType.FalseType
          return TypeKind.Condition(check, extends, trueType, falseType)
        | TypeAnnKind.Match matchType ->
          return! Error(TypeError.NotImplemented "TODO: inferTypeAnn - Match") // TODO
        | TypeAnnKind.Infer name -> return TypeKind.Infer name
        | TypeAnnKind.Wildcard -> return TypeKind.Wildcard
        | TypeAnnKind.Binary(left, op, right) ->
          let! left = inferTypeAnn env left
          let! right = inferTypeAnn env right
          return TypeKind.Binary(left, op, right)
      }

    let t: Result<Type, TypeError> =
      Result.map
        (fun kind ->
          let t = { Kind = kind; Provenance = None }
          typeAnn.InferredType <- Some(t)
          t)
        kind

    t

  type StmtResult =
    | Bindings of BindingAssump
    | Scheme of SchemeAssump

  let inferStmt
    (stmt: Stmt)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<option<Type> * option<StmtResult>, TypeError> =
    result {
      match stmt.Kind with
      | StmtKind.Expr expr ->
        let! t = inferExpr expr env nonGeneric
        return (Some(t), None)
      | StmtKind.For(pattern, right, block) ->
        return! Error(TypeError.NotImplemented "TODO: infer for")
      | StmtKind.Decl({ Kind = DeclKind.VarDecl(pattern, init, typeAnn) }) ->
        let! patBindings, patType = inferPattern env pattern

        let mutable newEnv = env

        for KeyValue(name, binding) in patBindings do
          newEnv <- newEnv.AddValue name binding

        let! initType = inferExpr init newEnv nonGeneric

        match typeAnn with
        | Some(typeAnn) ->
          let! typeAnnType = inferTypeAnn env typeAnn
          do! unify env initType typeAnnType
          do! unify env typeAnnType patType
        | None -> do! unify env initType patType

        return (None, Some(StmtResult.Bindings patBindings))
      | StmtKind.Decl({ Kind = DeclKind.TypeDecl(name, typeAnn, typeParams) }) ->

        let! t = inferTypeAnn env typeAnn

        let typeParams =
          Option.map
            (List.map (fun (param: Syntax.TypeParam) -> param.Name))
            typeParams

        let scheme = { TypeParams = typeParams; Type = t }
        return (None, Some(StmtResult.Scheme(name, scheme)))
      | StmtKind.Return expr ->
        match expr with
        | Some(expr) ->
          let! t = inferExpr expr env nonGeneric
          return (Some(t), None)
        | None -> return (None, None)
    }

  let inferPattern
    (env: Env)
    (pat: Syntax.Pattern)
    : Result<BindingAssump * Type, TypeError> =
    let mutable assump = BindingAssump([])

    let rec infer_pattern_rec (pat: Syntax.Pattern) : Type =
      match pat.Kind with
      | PatternKind.Identifier({ Name = name; IsMut = isMut }) ->
        let t = TypeVariable.makeVariable None

        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(name, (t, isMut))
        t
      | PatternKind.Literal(_, literal) ->
        { Type.Kind = TypeKind.Literal(literal)
          Provenance = None }
      | PatternKind.Object elems ->
        let mutable restType: option<Type> = None

        let elems: list<ObjTypeElem> =
          List.choose
            (fun (elem: Syntax.ObjPatElem) ->
              match elem with
              | Syntax.ObjPatElem.KeyValuePat(_span, key, value, _init) ->
                let t = infer_pattern_rec value

                Some(
                  ObjTypeElem.Property
                    { Name = key
                      Optional = false
                      Readonly = false
                      Type = t }
                )
              | Syntax.ObjPatElem.ShorthandPat(_span, name, _init, _is_mut) ->
                let t = TypeVariable.makeVariable None

                // TODO: check if `name` already exists in `assump`
                let isMut = false
                assump <- assump.Add(name, (t, isMut))

                Some(
                  ObjTypeElem.Property
                    { Name = name
                      Optional = false
                      Readonly = false
                      Type = t }
                )

              | Syntax.ObjPatElem.RestPat(span, pattern, isMut) ->
                restType <-
                  Some(
                    { Type.Kind = infer_pattern_rec pattern |> TypeKind.Rest
                      Provenance = None }
                  )

                None)
            elems

        let objType =
          { Type.Kind = TypeKind.Object(elems)
            Provenance = None }

        match restType with
        | Some(restType) ->
          { Type.Kind = TypeKind.Intersection([ objType; restType ])
            Provenance = None }
        | None -> objType
      | PatternKind.Tuple elems ->
        let elems' = List.map infer_pattern_rec elems

        { Type.Kind = TypeKind.Tuple(elems')
          Provenance = None }
      | PatternKind.Wildcard ->
        { Type.Kind = TypeKind.Wildcard
          Provenance = None }
      | PatternKind.Is(span, binding, isName, isMut) ->
        match Map.tryFind isName env.Schemes with
        | Some(scheme) ->
          assump <- assump.Add(binding.Name, (scheme.Type, binding.IsMut))
          scheme.Type
        | None -> failwith "todo"

    let t = infer_pattern_rec pat
    Result.Ok((assump, t))

  let rec patternToPattern (pat: Syntax.Pattern) : Pattern =
    match pat.Kind with
    | PatternKind.Identifier({ Name = name; IsMut = isMut }) ->
      Pattern.Identifier(name)
    | PatternKind.Is(span, bindingIdent, isName, isMut) ->
      Pattern.Is(bindingIdent, isName)
    | PatternKind.Object elems ->
      Pattern.Object(
        List.map
          (fun (elem: Syntax.ObjPatElem) ->
            match elem with
            | Syntax.ObjPatElem.KeyValuePat(span, key, value, init) ->
              ObjPatElem.KeyValuePat(key, patternToPattern value, init)
            | Syntax.ObjPatElem.ShorthandPat(span, name, init, isMut) ->
              // TODO: isMut
              ObjPatElem.ShorthandPat(name, init)
            | Syntax.ObjPatElem.RestPat(span, target, isMut) ->
              ObjPatElem.RestPat(patternToPattern target))
          elems
      )
    | PatternKind.Tuple elems -> Pattern.Tuple(List.map patternToPattern elems)
    | PatternKind.Wildcard -> Pattern.Wildcard
    | PatternKind.Literal(span, lit) -> Pattern.Literal(lit)

  let inferScript (stmts: list<Stmt>) (env: Env) : Result<Env, TypeError> =
    result {

      let nonGeneric = Set.empty
      let mutable newEnv = env

      let! _ =
        List.traverseResultM
          (fun stmt ->
            result {
              let! _, stmtResult = inferStmt stmt newEnv nonGeneric

              match stmtResult with
              | Some(StmtResult.Bindings assumps) ->
                for KeyValue(name, binding) in assumps do

                  let t = prune (fst binding)

                  match t.Kind with
                  | TypeKind.Function f ->
                    let t =
                      { t with
                          Kind = generalizeFunc f |> TypeKind.Function }

                    newEnv <- newEnv.AddValue name (t, snd binding)
                  | _ -> newEnv <- newEnv.AddValue name (t, snd binding)
              | Some(StmtResult.Scheme(name, scheme)) ->
                newEnv <- newEnv.AddScheme name scheme
              | None -> ()
            })
          stmts

      return newEnv
    }

  let findReturns (f: Syntax.Function) : list<Expr> =
    let mutable returns: list<Expr> = []

    let visitor =
      { VisitExpr =
          fun expr ->
            match expr.Kind with
            | ExprKind.Function _ -> false
            | _ -> true
        VisitStmt =
          fun stmt ->
            match stmt.Kind with
            | StmtKind.Return expr ->
              match expr with
              | Some expr -> returns <- expr :: returns
              | None -> ()
            | _ -> ()

            true
        VisitPattern = fun _ -> false
        VisitTypeAnn = fun _ -> false }

    match f.Body with
    | BlockOrExpr.Block block -> List.iter (walkStmt visitor) block.Stmts
    | BlockOrExpr.Expr expr ->
      walkExpr visitor expr // There might be early returns in match expression
      returns <- expr :: returns // We treat the expression as a return in this case

    returns

  // TODO: add VisitTypeAnn
  type SyntaxVisitor =
    { VisitExpr: Expr -> bool
      VisitStmt: Stmt -> bool
      VisitPattern: Syntax.Pattern -> bool
      VisitTypeAnn: TypeAnn -> bool }

  let walkExpr (visitor: SyntaxVisitor) (expr: Expr) : unit =
    let rec walk (expr: Expr) : unit =
      if visitor.VisitExpr expr then
        match expr.Kind with
        | ExprKind.Identifier _ -> ()
        | ExprKind.Literal _ -> ()
        | ExprKind.Function f ->
          match f.Body with
          | BlockOrExpr.Block block -> List.iter (walkStmt visitor) block.Stmts
          | BlockOrExpr.Expr expr -> walk expr
        | ExprKind.Call call ->
          walk call.Callee
          List.iter walk call.Args
        | ExprKind.Tuple elements -> List.iter walk elements
        | ExprKind.Index(target, index, _optChain) ->
          walk target
          walk index
        | ExprKind.Member(target, _name, _optChain) -> walk target
        | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
          walk condition

          List.iter (walkStmt visitor) thenBranch.Stmts

          Option.iter
            (fun elseBranch ->
              match elseBranch with
              | BlockOrExpr.Block block ->
                List.iter (walkStmt visitor) block.Stmts
              | BlockOrExpr.Expr expr -> walk expr)
            elseBranch
        | ExprKind.Match(target, cases) ->
          walk target

          List.iter
            (fun (case: MatchCase) ->
              walkPattern visitor case.Pattern
              walk case.Body
              Option.iter walk case.Guard)
            cases

          failwith "todo"
        | ExprKind.Assign(_op, left, right)
        | ExprKind.Binary(_op, left, right) ->
          walk left
          walk right
        | ExprKind.Unary(_op, value) -> walk value
        | ExprKind.Object elems -> failwith "todo"
        | ExprKind.Try(body, catch, fin) ->
          List.iter (walkStmt visitor) body.Stmts

          Option.iter
            (fun (e, body) ->
              walk e
              List.iter (walkStmt visitor) body.Stmts)
            catch

          Option.iter (fun body -> List.iter (walkStmt visitor) body.Stmts) fin
        | ExprKind.Do body -> List.iter (walkStmt visitor) body.Stmts
        | ExprKind.Await value -> walk value
        | ExprKind.Throw value -> walk value
        | ExprKind.TemplateLiteral { Exprs = exprs } -> List.iter walk exprs
        | ExprKind.TaggedTemplateLiteral(tag, template, throws) ->
          List.iter walk template.Exprs

    walk expr

  let walkStmt (visitor: SyntaxVisitor) (stmt: Stmt) : unit =
    let rec walk (stmt: Stmt) : unit =
      if visitor.VisitStmt stmt then
        match stmt.Kind with
        | StmtKind.Expr expr -> walkExpr visitor expr
        | StmtKind.For(left, right, body) ->
          walkPattern visitor left
          walkExpr visitor right
          List.iter walk body.Stmts
        | StmtKind.Decl({ Kind = DeclKind.VarDecl(_name, init, typeAnn) }) ->
          // TODO: walk typeAnn
          walkExpr visitor init
        | StmtKind.Decl({ Kind = DeclKind.TypeDecl _ }) ->
          failwith "TODO: walkStmt - TypeDecl"
        | StmtKind.Return exprOption ->
          Option.iter (walkExpr visitor) exprOption

    walk stmt

  let walkPattern (visitor: SyntaxVisitor) (pat: Syntax.Pattern) : unit =
    let rec walk (pat: Syntax.Pattern) : unit =
      if visitor.VisitPattern pat then
        match pat.Kind with
        | PatternKind.Identifier _ -> ()
        | PatternKind.Object elems ->
          List.iter
            (fun (elem: Syntax.ObjPatElem) ->
              match elem with
              | Syntax.ObjPatElem.KeyValuePat(span, key, value, init) ->
                walk value
              | Syntax.ObjPatElem.ShorthandPat(span, name, init, isMut) ->
                Option.iter (walkExpr visitor) init
              | Syntax.ObjPatElem.RestPat(span, target, isMut) -> walk target)
            elems
        | PatternKind.Tuple elems -> List.iter walk elems
        | PatternKind.Wildcard -> ()
        | PatternKind.Literal(span, value) -> ()
        | PatternKind.Is(span, ident, isName, isMut) -> ()

    walk pat

  let walkTypeAnn (visitor: SyntaxVisitor) (typeAnn: TypeAnn) : unit =
    let rec walk (typeAnn: TypeAnn) : unit =
      if visitor.VisitTypeAnn typeAnn then
        match typeAnn.Kind with
        | TypeAnnKind.Array elem -> walk elem
        | TypeAnnKind.Literal _ -> ()
        | TypeAnnKind.Keyword _ -> ()
        | TypeAnnKind.Object elems -> failwith "todo"
        | TypeAnnKind.Tuple elems -> List.iter walk elems
        | TypeAnnKind.Union types -> List.iter walk types
        | TypeAnnKind.Intersection types -> List.iter walk types
        | TypeAnnKind.TypeRef(_name, typeArgs) ->
          Option.iter (List.iter walk) typeArgs
        | TypeAnnKind.Function f ->
          walk f.ReturnType
          Option.iter walk f.Throws

          List.iter
            (fun (param: FuncParam<TypeAnn>) -> walk param.TypeAnn)
            f.ParamList
        | TypeAnnKind.Keyof target -> walk target
        | TypeAnnKind.Rest target -> walk target
        | TypeAnnKind.Typeof target -> walkExpr visitor target
        | TypeAnnKind.Index(target, index) ->
          walk target
          walk index
        | TypeAnnKind.Condition conditionType ->
          walk conditionType.Check
          walk conditionType.Extends
          walk conditionType.TrueType
          walk conditionType.FalseType
        | TypeAnnKind.Match matchType -> failwith "todo"
        | TypeAnnKind.Infer _name -> ()
        | TypeAnnKind.Wildcard -> ()
        | TypeAnnKind.Binary(left, op, right) ->
          walk left
          walk right

    walk typeAnn


  let rec flatten (types: list<Type>) : list<Type> =
    List.collect
      (fun t ->
        match t.Kind with
        | TypeKind.Union ts -> flatten ts
        | _ -> [ t ])
      types

  let union (types: list<Type>) : Type =
    match flatten types with
    | [] ->
      { Kind = makePrimitiveKind "never"
        Provenance = None }
    | [ t ] -> t
    | types ->
      let types =
        types
        |> List.map prune
        |> Seq.distinctBy (fun t -> t.Kind)
        |> Seq.toList

      { Kind = TypeKind.Union(types)
        Provenance = None }
