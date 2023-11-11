namespace Escalier

open System.Collections

module HindleyMilner =
  type Expr =
    | Ident of name: string
    | Lambda of name: string * body: Expr
    | Apply of func: Expr * argument: Expr
    | Let of name: string * definition: Expr * body: Expr
    | LetRec of name: string * definition: Expr * body: Expr

    override this.ToString() =
      match this with
      | Ident name -> name
      | Lambda(v, body) -> $"fun {v} -> {body}"
      | Apply(fn, arg) -> $"{fn} {arg}"
      | Let(v, def, body) -> $"let {v} = {def} in {body}"
      | LetRec(v, def, body) -> $"let rec {v} = {def} in {body}"

  ///A type variable standing for an arbitrary type.
  ///All type variables have a unique id, but names are only assigned lazily, when required.
  type tyvar =
    { id: int; mutable instance: ty option }

  ///An n-ary type constructor which builds a new type from old
  and tyop = { name: string; types: ty list }

  and ty =
    | TypeVariable of tyvar
    | TypeOperator of tyop

    override this.ToString() =
      match this with
      | TypeVariable({ instance = Some(instance) }) -> instance.ToString()
      | TypeVariable({ instance = None } as v) -> $"t{v.id}"
      | TypeOperator({ name = tyopName; types = tyopTypes }) ->
        match List.length tyopTypes with
        | 0 -> tyopName
        | 2 ->
          sprintf
            "(%s %s %s)"
            ((List.item 0 tyopTypes).ToString())
            tyopName
            ((List.item 1 tyopTypes).ToString())
        | _ ->
          sprintf
            "%s %s"
            tyopName
            (String.concat
              " "
              (List.map (fun item -> item.ToString()) tyopTypes))

  let mutable nextVariableId = 0

  let makeVariable () =
    let newVar = { id = nextVariableId; instance = None }

    nextVariableId <- nextVariableId + 1
    TypeVariable(newVar)

  let nextUniqueName = ref "a"

  type env = (string * ty) list

  let makeFunctionType fromTy toTy =
    TypeOperator(
      { name = "->"
        types = [ fromTy; toTy ] }
    )

  let intType = TypeOperator({ name = "int"; types = [] })
  let boolType = TypeOperator({ name = "bool"; types = [] })

  /// Returns the currently defining instance of t.
  /// As a side effect, collapses the list of type instances. The function Prune
  /// is used whenever a type expression has to be inspected: it will always
  /// return a type expression which is either an uninstantiated type variable or
  /// a type operator; i.e. it will skip instantiated variables, and will
  /// prune them from expressions to remove long chains of instantiated variables.
  let rec prune t =
    match t with
    | TypeVariable({ instance = Some(instance) } as v) ->
      let newInstance = prune instance
      v.instance <- Some(newInstance)
      newInstance
    | _ -> t

  let rec occursInType v t2 =
    match prune t2 with
    | pruned when pruned = v -> true
    | TypeOperator({ types = types }) -> occursIn v types
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
  let fresh t nonGeneric =
    let table = Hashtable()

    let rec loop tp =
      match prune tp with
      | TypeVariable _ as p ->
        if isGeneric p nonGeneric then
          match table.ContainsKey p with
          | false ->
            let newVar = makeVariable ()
            table.Add(p, newVar)
            newVar
          | true -> table[p] :?> ty
        else
          p
      | TypeOperator({ types = tyopTypes } as op) ->
        TypeOperator(
          { op with
              types = List.map loop tyopTypes }
        )

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
    match prune t1, prune t2 with
    | TypeVariable(v) as a, b ->
      if a <> b then
        if occursInType a b then
          failwith "Recursive unification"

        v.instance <- Some(b)
    | TypeOperator _ as a, (TypeVariable _ as b) -> unify b a
    | TypeOperator({ name = name1; types = types1 }) as a,
      (TypeOperator({ name = name2; types = types2 }) as b) ->
      if (name1 <> name2 || List.length types1 <> List.length types2) then
        failwith $"Type mismatch {a} != {b}"

      ignore (List.map2 unify types1 types2)

  ///Computes the type of the expression given by node.
  ///The type of the node is computed in the context of the
  ///supplied type environment env. Data types can be introduced into the
  ///language simply by having a predefined set of identifiers in the initial
  ///environment. environment; this way there is no need to change the syntax or, more
  ///importantly, the type-checking program when extending the language.
  let analyse exp env =
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
        let newNonGeneric = nonGeneric |> Set.add argTy
        let retTy = loop body newEnv newNonGeneric
        makeFunctionType argTy retTy
      | Let(v, defn, body) ->
        let defnTy = loop defn env nonGeneric
        loop body ((v, defnTy) :: env) nonGeneric
      | LetRec(v, defn, body) ->
        let newTy = makeVariable ()
        let newEnv = (v, newTy) :: env
        let newNonGeneric = nonGeneric |> Set.add newTy
        let defnTy = loop defn newEnv newNonGeneric
        unify newTy defnTy
        loop body newEnv nonGeneric

    loop exp env Set.empty
