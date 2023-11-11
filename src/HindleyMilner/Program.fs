// Based on https://github.com/7sharp9/write-you-an-inference-in-fsharp/blob/master/HMBasic/HMBasic.fs

open System.Collections

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
          (String.concat " " (List.map (fun item -> item.ToString()) tyopTypes))

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

let () =
  let pair =
    Apply(
      Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))),
      Apply(Ident("f"), Ident("true"))
    )

  let example1 =
    "example1",
    LetRec(
      "factorial", (* letrec factorial = *)
      Lambda(
        "n", (* fn n => *)
        Apply(
          Apply( (* cond (zero n) 1 *)
            Apply(
              Ident("cond"), (* cond (zero n) *)
              Apply(Ident("zero"), Ident("n"))
            ),
            Ident("1")
          ),
          Apply( (* times n *)
            Apply(Ident("times"), Ident("n")),
            Apply(Ident("factorial"), Apply(Ident("pred"), Ident("n")))
          )
        )
      ), (* in *)
      Ident("factorial")
    // Apply(Ident("factorial"), Ident("5"))
    )

  (* Should fail: *)
  (* fn x => (pair(x(3) (x(true))) *)
  let example2 =
    "example2",
    Lambda(
      "x",
      Apply(
        Apply(Ident("pair"), Apply(Ident("x"), Ident("3"))),
        Apply(Ident("x"), Ident("true"))
      )
    )

  (* pair(f(3), f(true)) *)
  let example3 =
    "example3",
    Apply(
      Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))),
      Apply(Ident("f"), Ident("true"))
    )

  (* letrec f = (fn x => x) in ((pair (f 4)) (f true)) *)
  let example4 = "example4", Let("f", Lambda("x", Ident("x")), pair)

  (* fn f => f f (fail) *)
  let example5 = "example5", Lambda("f", Apply(Ident("f"), Ident("f")))

  (* let g = fn f => 5 in g g *)
  let example6 =
    "example6", Let("g", Lambda("f", Ident("5")), Apply(Ident("g"), Ident("g")))

  (* example that demonstrates generic and non-generic variables: *)
  (* fn g => let f = fn x => g in pair (f 3, f true) *)
  let example7 =
    "example7",
    Lambda(
      "g",
      Let(
        "f",
        Lambda("x", Ident("g")),
        Apply(
          Apply(Ident("pair"), Apply(Ident("f"), Ident("3"))),
          Apply(Ident("f"), Ident("true"))
        )
      )
    )

  (* Function composition *)
  (* fn f (fn g (fn arg (f g arg))) *)
  let example8 =
    "example8",
    Lambda(
      "f",
      Lambda(
        "g",
        Lambda("arg", Apply(Ident("g"), Apply(Ident("f"), Ident("arg"))))
      )
    )

  let mutable basicEnv =
    let var1 = makeVariable ()
    let var2 = makeVariable ()
    let pairTy = TypeOperator({ name = "*"; types = [ var1; var2 ] })
    let var3 = makeVariable ()

    [ ("pair", makeFunctionType var1 (makeFunctionType var2 pairTy))
      ("true", boolType)
      ("cond",
       makeFunctionType
         boolType
         (makeFunctionType var3 (makeFunctionType var3 var3)))
      ("zero", makeFunctionType intType boolType)
      ("pred", makeFunctionType intType intType)
      ("times", makeFunctionType intType (makeFunctionType intType intType)) ]

  let testBank =
    [ example1
      example2
      example3
      example4
      example5
      example6
      example7
      example8 ]

  let runTestBank bank printResult =
    bank
    |> List.iter (fun (name, exp) ->
      nextVariableId <- 0

      let result =
        try
          Ok(analyse exp basicEnv)
        with ex ->
          Result.Error(ex.Message)

      if printResult then
        printfn "%s" name
        printfn $"Expression: {exp}"

        match result with
        | Ok result -> printfn $"inferred: {result}\n"
        | Result.Error error -> printfn $"inferred: {error}\n")

  //run through the tests as a warm up
  runTestBank testBank true

  let S =
    Lambda(
      "f",
      Lambda(
        "g",
        Lambda(
          "x",
          Apply(Apply(Ident("f"), Ident("x")), Apply(Ident("g"), Ident("x")))
        )
      )
    )

  let t = analyse S basicEnv
  basicEnv <- ("S", t) :: basicEnv
  printfn $"S = {t}"

  let K1 = Lambda("x", Lambda("y", Ident("x")))
  let t = analyse K1 basicEnv
  basicEnv <- ("K1", t) :: basicEnv
  printfn $"K1 = {t}"

  let K2 = Lambda("x", Lambda("y", Ident("x")))
  let t = analyse K2 basicEnv
  basicEnv <- ("K2", t) :: basicEnv
  printfn $"K2 = {t}"

  // NOTE: using K1, K2 simulates generalization/instantation of K
  let I = Apply(Apply(Ident("S"), Ident("K1")), Ident("K2"))
  let t = analyse I basicEnv
  printfn $"I = {t}"

  ()
