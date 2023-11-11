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

  let makeFunctionType fromTy toTy =
    { kind =
        TypeOp(
          { name = "->"
            types = [ fromTy; toTy ] }
        ) }

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
  let infer exp env =
    let rec loop exp env nonGeneric =
      match exp with
      | Ident(name) -> getType name env nonGeneric
      | Apply(fn, arg) ->
        let funTy = loop fn env nonGeneric
        let argTy = loop arg env nonGeneric
        let retTy = makeVariable ()
        unify (makeFunctionType argTy retTy) funTy
        retTy
      | Lambda(arg, body) ->
        let argTy = makeVariable ()
        let newEnv = (arg, argTy) :: env

        let newNonGeneric =
          match argTy.kind with
          | TypeVar { id = id } -> nonGeneric |> Set.add id
          | _ -> nonGeneric

        let retTy = loop body newEnv newNonGeneric
        makeFunctionType argTy retTy
      | Let(v, defn, body) ->
        let defnTy = loop defn env nonGeneric
        loop body ((v, defnTy) :: env) nonGeneric
      | LetRec(v, defn, body) ->
        let newTy = makeVariable ()
        let newEnv = (v, newTy) :: env

        let newNonGeneric =
          match newTy.kind with
          | TypeVar { id = id } -> nonGeneric |> Set.add id
          | _ -> nonGeneric

        let defnTy = loop defn newEnv newNonGeneric
        unify newTy defnTy
        loop body newEnv nonGeneric
      | Expr.Tuple elems ->
        let elems = List.map (fun elem -> loop elem env nonGeneric) elems
        { Type.kind = TypeKind.Tuple(elems) }

    loop exp env Set.empty
