﻿namespace Escalier.HindleyMilner

open Escalier.HindleyMilner.Syntax
open FsToolkit.ErrorHandling
open System.Collections.Generic

open Type
open Errors

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
  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>
  type SchemeAssump = (string * Scheme)

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

  type Assump = string * Binding

  let makeFunctionType typeParams paramList ret =
    { Kind =
        Function
          { TypeParams = typeParams
            ParamList = paramList
            Ret = ret }
      Provenance = None }

  let numType =
    { Kind = TypeRef({ Name = "number"; TypeArgs = None })
      Provenance = None }

  let boolType =
    { Kind = TypeRef({ Name = "boolean"; TypeArgs = None })
      Provenance = None }

  let strType =
    { Kind = TypeRef({ Name = "string"; TypeArgs = None })
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
                    Ret = fold f.Ret }
            Provenance = None }
        | Tuple(elems) ->
          let elems = List.map fold elems

          { Kind = Tuple(elems)
            Provenance = None }
        | TypeRef({ Name = name; TypeArgs = typeArgs }) ->
          let typeArgs = Option.map (List.map fold) typeArgs

          { Kind = TypeRef({ Name = name; TypeArgs = typeArgs })
            Provenance = None }
        | Literal _ -> t
        | Wildcard -> t

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
            { Kind = TypeRef { Name = name; TypeArgs = None }
              Provenance = None }
          )
        | None ->
          let tpName = 65 + nextId |> char |> string
          nextId <- nextId + 1
          mapping <- mapping |> Map.add id tpName

          Some(
            { Kind = TypeRef { Name = tpName; TypeArgs = None }
              Provenance = None }
          )
      | _ -> None

    let paramList =
      List.map
        (fun (p: FuncParam) -> { p with Type = foldType folder p.Type })
        f.ParamList

    // TODO: f.throws
    let ret = foldType folder f.Ret

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
      Ret = ret }

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
          Ret = foldType folder f.Ret }
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
          (loop f.Ret)
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
  let unify (t1: Type) (t2: Type) : Result<unit, TypeError> =
    result {
      match (prune t1).Kind, (prune t2).Kind with
      | TypeVar _, _ -> do! bind t1 t2
      | _, TypeVar _ -> do! unify t2 t1
      | Tuple(elems1), Tuple(elems2) ->
        if List.length elems1 <> List.length elems2 then
          return! Error(TypeError.TypeMismatch(t1, t2))

        ignore (List.map2 unify elems1 elems2)
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
          do! unify param2 param1 // params are contravariant

        do! unify f1.Ret f2.Ret // returns are covariant
      | TypeRef({ Name = name1; TypeArgs = types1 }),
        TypeRef({ Name = name2; TypeArgs = types2 }) ->

        if (name1 <> name2) then
          return! Error(TypeError.TypeMismatch(t1, t2))

        match (types1, types2) with
        | None, None -> ()
        | Some(types1), Some(types2) ->
          if List.length types1 <> List.length types2 then
            return! Error(TypeError.TypeMismatch(t1, t2))

          ignore (List.map2 unify types1 types2)
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | Literal lit, TypeRef({ Name = name; TypeArgs = typeArgs }) ->
        // TODO: check that `typeArgs` is `None`
        match lit, name with
        | Literal.Number _, "number" -> ()
        | Literal.String _, "string" -> ()
        | Literal.Boolean _, "boolean" -> ()
        | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
    }

  let unify_call
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
          unify_func_call args typeArgs retType throwsType func env nonGeneric
      | TypeKind.TypeVar _ ->

        // TODO: use a `result {}` CE here
        let! argTypes =
          List.traverseResultM (fun arg -> infer_expr arg env nonGeneric) args

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
                  Ret = retType
                  // throws = throwsType
                  TypeParams = None } // TODO
            Provenance = None }

        match bind callee callType with
        | Ok _ -> return (prune retType, prune throwsType)
        | Error e -> return! Error e
      | kind -> return! Error(TypeError.NotImplemented $"kind = {kind}")
    }

  let unify_func_call
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
              let! argType = infer_expr arg env nonGeneric
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
          do! unify argType param.Type // contravariant

      do! unify retType callee.Ret // covariant
      // do! unify throwsType callee.throws // TODO

      return (retType, throwsType)
    }

  ///Computes the type of the expression given by node.
  ///The type of the node is computed in the context of the
  ///supplied type environment env. Data types can be introduced into the
  ///language simply by having a predefined set of identifiers in the initial
  ///environment. environment; this way there is no need to change the syntax or, more
  ///importantly, the type-checking program when extending the language.
  let infer_expr
    (expr: Expr)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<Type, TypeError> =
    result {
      match expr.Kind with
      | ExprKind.Ident(name) -> return getType name env nonGeneric
      | ExprKind.Literal(literal) ->
        return
          { Type.Kind = Literal(literal)
            Provenance = None }
      | ExprKind.Call(callee, args) ->
        let! callee = infer_expr callee env nonGeneric

        let! result, throws = unify_call args None callee env nonGeneric

        // TODO: handle throws

        return result
      | ExprKind.Binary(op, left, right) ->
        let funTy = getType op env nonGeneric

        let! result, throws =
          unify_call [ left; right ] None funTy env nonGeneric

        // TODO: handle throws

        return result
      | ExprKind.Function f ->
        let mutable newEnv = env
        let mutable newNonGeneric = nonGeneric

        let! paramList =
          List.traverseResultM
            (fun (param: Syntax.FuncParam<option<TypeAnn>>) ->
              result {
                let paramType = TypeVariable.makeVariable None in
                let! assumps, patternType = infer_pattern env param.Pattern
                do! unify patternType paramType

                for KeyValue(name, binding) in assumps do
                  // TODO: update `Env.types` to store `Binding`s insetad of `Type`s
                  newEnv <- newEnv.AddValue name binding

                match paramType.Kind with
                | TypeVar { Id = id } ->
                  newNonGeneric <- newNonGeneric |> Set.add id
                | _ -> ()

                return
                  { Pattern = Pattern.Identifier "arg"
                    Type = paramType
                    Optional = false }
              })
            f.Sig.ParamList

        let! stmtTypes =
          List.traverseResultM
            (fun stmt ->
              result {
                let! t, assump = infer_stmt stmt newEnv newNonGeneric

                match assump with
                | Some(name, t) -> newEnv <- newEnv.AddValue name t
                | None -> ()

                return t
              })
            f.Body.Stmts

        let retTy =
          match List.tryLast stmtTypes with
          | Some(t) ->
            match t with
            | Some(t) -> t
            | None -> failwith "Last statement must be an expression"
          | None -> failwith "Empty lambda body"

        return makeFunctionType None paramList retTy // TODO: handle explicit type params
      | ExprKind.Tuple elems ->
        let! elems =
          List.traverseResultM
            (fun elem -> infer_expr elem env nonGeneric)
            elems

        return
          { Type.Kind = TypeKind.Tuple(elems)
            Provenance = None }
      | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
        let retTy = TypeVariable.makeVariable None

        let! conditionTy = infer_expr condition env nonGeneric
        let! thenBranchTy = infer_expr thenBranch env nonGeneric
        let! elseBranchTy = infer_expr elseBranch env nonGeneric

        do! unify conditionTy boolType
        do! unify thenBranchTy retTy
        do! unify elseBranchTy retTy

        return retTy
      | _ ->
        return!
          Error(TypeError.NotImplemented "TODO: finish implementing infer_expr")
    }

  let infer_stmt
    (stmt: Stmt)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<option<Type> * option<Assump>, TypeError> =
    result {
      match stmt with
      | Stmt.Expr expr ->
        let! t = infer_expr expr env nonGeneric
        return (Some(t), None)
      | For(pattern, right, block) ->
        return! Error(TypeError.NotImplemented "TODO: infer for")
      | Let(name, definition) ->
        let! defnTy = infer_expr definition env nonGeneric
        let assump = (name, (defnTy, false)) // TODO: isMut
        return (None, Some(assump))
      | LetRec(name, defn) ->
        let newTy = TypeVariable.makeVariable None
        let newEnv = env.AddValue name (newTy, false) // TODO: isMut

        let newNonGeneric =
          match newTy.Kind with
          | TypeVar { Id = id } -> nonGeneric |> Set.add id
          | _ -> nonGeneric

        let! defnTy = infer_expr defn newEnv newNonGeneric
        do! unify newTy defnTy

        let binding = (newTy, false) // TODO: isMut
        return (None, Some(name, binding))
    }

  let infer_pattern
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
      | PatternKind.Object elems -> failwith "todo"
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


  let infer_script (stmts: list<Stmt>) (env: Env) : Result<Env, TypeError> =
    result {

      let nonGeneric = Set.empty
      let mutable newEnv = env

      let! _ =
        List.traverseResultM
          (fun stmt ->
            result {
              let! _, assump = infer_stmt stmt newEnv nonGeneric

              match assump with
              | Some(assump) ->
                let name, binding = assump
                let t = prune (fst binding)

                match t.Kind with
                | TypeKind.Function f ->
                  let t =
                    { t with
                        Kind = generalizeFunc f |> TypeKind.Function }

                  newEnv <- newEnv.AddValue name (t, snd binding)
                | _ -> newEnv <- newEnv.AddValue name (t, snd binding)
              | None -> ()
            })
          stmts

      return newEnv
    }
