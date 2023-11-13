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

  type env = list<string * Type>

  let makeFunctionType typeParams args ret =
    { kind =
        Function
          { typeParams = typeParams
            args = args
            ret = ret } }

  let numType = { kind = TypeRef({ name = "number"; typeArgs = None }) }
  let boolType = { kind = TypeRef({ name = "boolean"; typeArgs = None }) }
  let strType = { kind = TypeRef({ name = "string"; typeArgs = None }) }

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

  let fold_type (f: Type -> option<Type>) (t: Type) : Type =
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
        | TypeRef({ name = name; typeArgs = typeArgs }) ->
          let typeArgs = Option.map (List.map fold) typeArgs
          { kind = TypeRef({ name = name; typeArgs = typeArgs }) }

      match f t with
      | Some(t) -> t
      | None -> t

    fold t

  let generalize_func (t: Type) : Type =
    let mutable mapping: Map<int, string> = Map.empty
    let mutable nextId = 0

    // QUESTION: should we call `prune` inside the folder as well?
    let folder t =
      match t.kind with
      | TypeVar { id = id } ->
        match Map.tryFind id mapping with
        | Some(name) ->
          Some({ kind = TypeRef { name = name; typeArgs = None } })
        | None ->
          let tpName = 65 + nextId |> char |> string
          nextId <- nextId + 1
          mapping <- mapping |> Map.add id tpName
          Some({ kind = TypeRef { name = tpName; typeArgs = None } })
      | _ -> None

    let t = prune t

    match t.kind with
    | Function _ ->
      let t = fold_type folder t
      let typeParams = mapping |> Map.toList |> List.map snd

      match t.kind with
      | Function f ->
        { kind = TypeKind.Function { f with typeParams = Some(typeParams) } }
      | _ -> failwith "Expected function type"
    | _ -> t

  let rec instantiate_func (f: Function) : Function =
    let mutable mapping: Map<string, Type> = Map.empty

    let folder t =
      match t.kind with
      | TypeRef({ name = name }) ->
        match Map.tryFind name mapping with
        | Some(tv) -> Some(tv)
        | None -> None
      | _ -> None

    match f.typeParams with
    | Some(typeParams) ->
      for tp in typeParams do
        let v = makeVariable ()
        mapping <- mapping |> Map.add tp v
    | None -> ()

    { f with
        typeParams = None
        args = List.map (fold_type folder) f.args
        ret = fold_type folder f.ret }

  let rec occursInType (v: Type) (t2: Type) =
    match (prune t2).kind with
    | pruned when pruned = v.kind -> true
    | TypeRef({ typeArgs = typeArgs }) ->
      match typeArgs with
      | Some(typeArgs) -> occursIn v typeArgs
      | None -> false
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
      | Function f ->
        makeFunctionType f.typeParams (List.map loop f.args) (loop f.ret)
      | TypeRef({ typeArgs = typeArgs } as op) ->
        let kind =
          TypeRef(
            { op with
                typeArgs = Option.map (List.map loop) typeArgs }
          )

        { kind = kind }

    loop t

  ///Get the type of identifier name from the type environment env
  let getType name env nonGeneric =
    match env |> List.tryFind (fun (n, _) -> n = name) with
    | Some(_name, var) -> fresh var nonGeneric
    | None ->
      if isIntegerLiteral name then
        numType
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
      let f1 = instantiate_func f1
      let f2 = instantiate_func f2
      List.iter2 (fun arg1 arg2 -> unify arg2 arg1) f1.args f2.args // args are contravariant
      unify f1.ret f2.ret // retruns are covariant
    | TypeRef({ name = name1; typeArgs = types1 }),
      TypeRef({ name = name2; typeArgs = types2 }) ->

      if (name1 <> name2) then
        failwith $"Type mismatch {t1} != {t2}"

      match (types1, types2) with
      | None, None -> ()
      | Some(types1), Some(types2) ->
        if List.length types1 <> List.length types2 then
          failwithf $"Type mismatch {t1} != {t2}"

        ignore (List.map2 unify types1 types2)
      | _ -> failwith $"Type mismatch {t1} != {t2}"
    | _, _ -> failwith $"Type mismatch {t1} != {t2}"

  ///Computes the type of the expression given by node.
  ///The type of the node is computed in the context of the
  ///supplied type environment env. Data types can be introduced into the
  ///language simply by having a predefined set of identifiers in the initial
  ///environment. environment; this way there is no need to change the syntax or, more
  ///importantly, the type-checking program when extending the language.
  let rec infer_expr (expr: Expr) env nonGeneric =
    match expr.kind with
    | ExprKind.Ident(name) -> getType name env nonGeneric
    | ExprKind.Literal(value) ->
      match value with
      | Literal.Number _ -> numType // TODO: infer a literal type
      | Literal.Boolean _ -> boolType // TODO: infer a literal type
      | Literal.String _ -> strType // TODO: infer a literal type
      | _ -> failwith "TODO: handle null and undefined"
    | ExprKind.Call(fn, args) ->
      let funTy = infer_expr fn env nonGeneric
      let args = List.map (fun arg -> infer_expr arg env nonGeneric) args
      let retTy = makeVariable ()
      unify (makeFunctionType None args retTy) funTy
      retTy
    | ExprKind.Binary(op, left, right) ->
      let funTy = getType op env nonGeneric

      let args =
        List.map (fun arg -> infer_expr arg env nonGeneric) [ left; right ]

      let retTy = makeVariable ()
      unify (makeFunctionType None args retTy) funTy
      retTy
    | ExprKind.Function f ->
      let mutable newEnv = env

      let args =
        List.map
          (fun param ->
            let newArgTy = makeVariable () in
            // TODO: replace with infer_pattern
            newEnv <- (param.ToString(), newArgTy) :: newEnv
            newArgTy)
          f.sig'.paramList

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
            let t, assump = infer_stmt stmt newEnv newNonGeneric

            match assump with
            | Some(assump) -> newEnv <- assump :: newEnv
            | None -> ()

            t)
          f.body.stmts

      let retTy =
        match List.tryLast stmtTypes with
        | Some(t) ->
          match t with
          | Some(t) -> t
          | None -> failwith "Last statement must be an expression"
        | None -> failwith "Empty lambda body"

      makeFunctionType None args retTy // TODO: handle explicit type params
    | ExprKind.Tuple elems ->
      let elems = List.map (fun elem -> infer_expr elem env nonGeneric) elems
      { Type.kind = TypeKind.Tuple(elems) }
    | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
      let retTy = makeVariable ()

      let conditionTy = infer_expr condition env nonGeneric
      let thenBranchTy = infer_expr thenBranch env nonGeneric
      let elseBranchTy = infer_expr elseBranch env nonGeneric

      unify conditionTy boolType
      unify thenBranchTy retTy
      unify elseBranchTy retTy

      retTy
    | _ -> failwith "TODO: finish implementing infer_expr"

  and infer_stmt stmt env nonGeneric =
    match stmt with
    | Expr expr ->
      let t = infer_expr expr env nonGeneric
      (Some(t), None)
    | For(pattern, right, block) -> failwith "TODO: infer for"
    | Let(name, definition) ->
      let defnTy = infer_expr definition env nonGeneric
      let assump = (name, defnTy)
      (None, Some(assump))
    | LetRec(name, defn) ->
      let newTy = makeVariable ()
      let assump = (name, newTy)
      let newEnv = assump :: env

      let newNonGeneric =
        match newTy.kind with
        | TypeVar { id = id } -> nonGeneric |> Set.add id
        | _ -> nonGeneric

      let defnTy = infer_expr defn newEnv newNonGeneric
      unify newTy defnTy

      (None, Some(assump))

  let infer_script stmts env =
    let nonGeneric = Set.empty
    let mutable newEnv = env

    List.iter
      (fun stmt ->
        let _, assump = infer_stmt stmt newEnv nonGeneric

        match assump with
        | Some(assump) ->
          let name, t = assump
          newEnv <- (name, generalize_func t) :: newEnv
        | None -> ())
      stmts

    newEnv
