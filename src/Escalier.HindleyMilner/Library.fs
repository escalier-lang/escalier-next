namespace Escalier.HindleyMilner

open System.Collections.Generic
open Syntax
open Type

module TypeChecker =
  let mutable nextVariableId = 0

  let makeVariable () =
    let newVar = { id = nextVariableId; instance = None }

    nextVariableId <- nextVariableId + 1
    { kind = TypeVar(newVar) }

  let nextUniqueName = ref "a"

  type env = list<(string * Type)>

  let makeFunctionType args ret =
    { kind =
        Function
          { typeParams = None
            args = args
            ret = ret } }

  let intType = { kind = TypeOp({ name = "int"; types = [] }) }
  let boolType = { kind = TypeOp({ name = "bool"; types = [] }) }

  /// Returns the currently defining instance of t.
  /// As a side effect, collapses the list of type instances. The function Prune
  /// is used whenever a type expression has to be inspected: it will always
  /// return a type expression which is either an uninstantiated type variable or
  /// a type operator; i.e. it will skip instantiated variables, and will
  /// prune them from expressions to remove long chains of instantiated variables.
  let rec prune t =
    match t.kind with
    | TypeVar({ instance = Some(instance) } as v) ->
      let newInstance = prune instance
      v.instance <- Some(newInstance)
      newInstance
    | _ -> t

  let rec occursInType (v: Type) (t2: Type) =
    match (prune t2).kind with
    | pruned when pruned = v.kind -> true
    | TypeOp({ types = types }) -> occursIn v types
    | _ -> false

  and occursIn t types =
    List.exists (fun t2 -> occursInType t t2) types

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

      match t.kind with
      | TypeVar p ->
        if isGeneric p.id nonGeneric then
          match table.ContainsKey p.id with
          | false ->
            let newVar = makeVariable ()
            table.Add(p.id, newVar)
            newVar
          | true -> table[p.id]
        else
          t
      | Tuple elems -> { kind = Tuple(List.map loop elems) }
      | Function f -> makeFunctionType (List.map loop f.args) (loop f.ret)
      | TypeOp({ types = tyopTypes } as op) ->
        let kind =
          TypeOp(
            { op with
                types = List.map loop tyopTypes }
          )

        { kind = kind }

    loop t

  ///Get the type of identifier name from the type environment env
  let getType name env nonGeneric =
    match env |> List.tryFind (fun (n, _) -> n = name) with
    | Some(_name, var) -> fresh var nonGeneric
    | None ->
      if isIntegerLiteral name then
        intType
      else
        failwithf $"Undefined symbol {name}"

  ///Unify the two types t1 and t2. Makes the types t1 and t2 the same.
  let rec unify t1 t2 =
    match (prune t1).kind, (prune t2).kind with
    | TypeVar(v) as a, b ->
      if a <> b then
        if occursInType t1 t2 then
          failwith "Recursive unification"

        v.instance <- Some(t2)
    | _, TypeVar _ -> unify t2 t1
    | Tuple(elems1), Tuple(elems2) ->
      if List.length elems1 <> List.length elems2 then
        failwithf $"Type mismatch {t1} != {t2}"

      ignore (List.map2 unify elems1 elems2)
    | Function(f1), Function(f2) ->
      List.iter2 (fun arg1 arg2 -> unify arg2 arg1) f1.args f2.args // args are contravariant
      unify f1.ret f2.ret // retruns are covariant
    | TypeOp({ name = name1; types = types1 }),
      TypeOp({ name = name2; types = types2 }) ->
      if (name1 <> name2 || List.length types1 <> List.length types2) then
        failwith $"Type mismatch {t1} != {t2}"

      ignore (List.map2 unify types1 types2)
    | _, _ -> failwith $"Type mismatch {t1} != {t2}"

  ///Computes the type of the expression given by node.
  ///The type of the node is computed in the context of the
  ///supplied type environment env. Data types can be introduced into the
  ///language simply by having a predefined set of identifiers in the initial
  ///environment. environment; this way there is no need to change the syntax or, more
  ///importantly, the type-checking program when extending the language.
  let rec infer_expr exp env nonGeneric =
    // let rec loop exp env nonGeneric =
    match exp with
    | Ident(name) -> getType name env nonGeneric
    | Apply(fn, args) ->
      let funTy = infer_expr fn env nonGeneric
      let args = List.map (fun arg -> infer_expr arg env nonGeneric) args
      let retTy = makeVariable ()
      unify (makeFunctionType args retTy) funTy
      retTy
    | Binary(op, left, right) ->
      let funTy = getType op env nonGeneric
      // let funTy = infer_expr fn env nonGeneric
      let args =
        List.map (fun arg -> infer_expr arg env nonGeneric) [ left; right ]
      // let args = List.map (fun arg -> infer_expr arg env nonGeneric) args
      let retTy = makeVariable ()
      unify (makeFunctionType args retTy) funTy
      retTy
    | Lambda f ->
      let mutable newEnv = env

      let args =
        List.map
          (fun arg ->
            let newArgTy = makeVariable () in
            newEnv <- (arg, newArgTy) :: newEnv
            newArgTy)
          f.args

      let mutable newNonGeneric = nonGeneric

      List.iter
        (fun argTy ->
          match argTy.kind with
          | TypeVar { id = id } -> newNonGeneric <- newNonGeneric |> Set.add id
          | _ -> ())
        args

      let stmtTypes =
        List.map
          (fun stmt ->
            let t, newNewEnv = infer_stmt stmt newEnv newNonGeneric
            newEnv <- newNewEnv
            t)
          f.body

      // for stmt in body do
      //   let _, newNewEnv = infer_stmt stmt newEnv newNonGeneric
      //   newEnv <- newNewEnv

      let retTy =
        match List.tryLast stmtTypes with
        | Some(t) ->
          match t with
          | Some(t) -> t
          | None -> failwith "Last statement must be an expression"
        | None -> failwith "Empty lambda body"

      makeFunctionType args retTy
    | Expr.Tuple elems ->
      let elems = List.map (fun elem -> infer_expr elem env nonGeneric) elems
      { Type.kind = TypeKind.Tuple(elems) }
    | IfElse(condition, thenBranch, elseBranch) ->
      let retTy = makeVariable ()

      let conditionTy = infer_expr condition env nonGeneric
      let thenBranchTy = infer_expr thenBranch env nonGeneric
      let elseBranchTy = infer_expr elseBranch env nonGeneric

      unify conditionTy boolType
      unify thenBranchTy retTy
      unify elseBranchTy retTy

      retTy

  and infer_stmt stmt env nonGeneric =
    match stmt with
    | Expr expr ->
      let t = infer_expr expr env nonGeneric
      (Some(t), env)
    | Let(name, definition) ->
      let defnTy = infer_expr definition env nonGeneric
      let newEnv = (name, defnTy) :: env
      (None, newEnv)
    | LetRec(name, defn) ->
      let newTy = makeVariable ()
      let newEnv = (name, newTy) :: env

      let newNonGeneric =
        match newTy.kind with
        | TypeVar { id = id } -> nonGeneric |> Set.add id
        | _ -> nonGeneric

      let defnTy = infer_expr defn newEnv newNonGeneric
      unify newTy defnTy

      (None, newEnv)

  let infer_script stmts env =
    let nonGeneric = Set.empty
    let mutable newEnv = env

    List.iter
      (fun stmt ->
        let _, newNewEnv = infer_stmt stmt newEnv nonGeneric
        newEnv <- newNewEnv)
      stmts

    newEnv

  let fold_type (t: Type) (f: Type -> option<Type>) : Type =
    let rec fold (t: Type) : Type =
      let t = prune t

      let t =
        match t.kind with
        | TypeVar _ -> t
        | Function f ->
          { kind =
              TypeKind.Function
                { f with
                    args = List.map fold f.args
                    ret = fold f.ret } }
        | Tuple(elems) ->
          let elems = List.map fold elems
          { kind = Tuple(elems) }
        | TypeOp({ name = name; types = types }) ->
          let newTypes = List.map fold types
          { kind = TypeOp({ name = name; types = newTypes }) }

      match f t with
      | Some(t) -> t
      | None -> t

    fold t

  let generalize_func (t: Type) : Type =
    let mutable mapping: Map<int, string> = Map.empty
    let mutable nextId = 0

    // QUESTION: should we call `pr=une` inside the folder as well?
    let folder =
      fun t ->
        match t.kind with
        | TypeVar { id = id } ->
          match Map.tryFind id mapping with
          | Some(name) -> Some({ kind = TypeOp { name = name; types = [] } })
          | None ->
            let tpName = 65 + nextId |> char |> string
            nextId <- nextId + 1
            mapping <- mapping |> Map.add id tpName
            Some({ kind = TypeOp { name = tpName; types = [] } })
        | _ -> None

    let t = prune t

    match t.kind with
    | Function _ ->
      let t = fold_type t folder
      let typeParams = mapping |> Map.toList |> List.map snd
      printfn "new t = {t}"
      printfn "typeParams = %A" typeParams

      match t.kind with
      | Function f ->
        { kind = TypeKind.Function { f with typeParams = Some(typeParams) } }
      | _ -> failwith "Expected function type"
    | _ -> t
