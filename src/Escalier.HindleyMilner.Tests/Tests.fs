module Tests

open Xunit

open Escalier.HindleyMilner.Syntax
open Escalier.HindleyMilner.TypeChecker

let getEnv () =
  [ ("true", boolType)
    ("zero", makeFunctionType None [ intType ] boolType)
    ("pred", makeFunctionType None [ intType ] intType)
    ("times", makeFunctionType None [ intType; intType ] intType) ]

let ident x = { kind = Ident(x) }
let apply (f, args) = { kind = Apply(f, args) }
let lambda f = { kind = Lambda(f) }

let ifelse (cond, thenExpr, elseExpr) =
  { kind = IfElse(cond, thenExpr, elseExpr) }

let binary (op, left, right) = { kind = Binary(op, left, right) }
let tuple exprs = { kind = Tuple(exprs) }

[<Fact>]
let InferFactorial () =
  nextVariableId <- 0

  (* letrec factorial =
      fn n =>
        if (zero n) 
          then 1 
          else (times n (factorial (pred n)))
      in factorial
   *)
  let ast =
    LetRec(
      "factorial",
      lambda
        { typeParams = None
          args = [ "n" ] (* fn n => *)
          body =
            [ Stmt.Expr(
                ifelse (
                  apply (ident "zero", [ ident "n" ]),
                  ident "1",
                  binary (
                    "times", // op
                    ident "n",
                    apply (
                      ident "factorial",
                      [ apply (ident "pred", [ ident "n" ]) ]
                    )
                  )
                )
              ) ] }
    )

  let mutable env = getEnv ()
  let nonGeneric = Set.empty

  let _, assump = infer_stmt ast env nonGeneric

  match assump with
  | Some(assump) -> env <- assump :: env
  | None -> ()

  let t = getType "factorial" env nonGeneric

  Assert.Equal("fn (int) -> int", t.ToString())

[<Fact>]
let UnificationFailure () =
  nextVariableId <- 0
  (* fn x => [x(3) x(true)] *)
  let ast =
    lambda
      { typeParams = None
        args = [ "x" ]
        body =
          [ Stmt.Expr(
              tuple
                [ apply (ident "x", [ ident "3" ])
                  apply (ident "x", [ ident "true" ]) ]
            ) ] }

  let env = getEnv ()
  let nonGeneric = Set.empty

  try
    infer_expr ast env nonGeneric |> ignore
  with ex ->
    Assert.Equal("Type mismatch int != bool", ex.Message)

[<Fact>]
let UndefinedSymbol () =
  nextVariableId <- 0
  let ast = ident "foo"
  let env = getEnv ()
  let nonGeneric = Set.empty

  try
    infer_expr ast env nonGeneric |> ignore
  with ex ->
    Assert.Equal("Undefined symbol foo", ex.Message)

[<Fact>]
let InferPair () =
  nextVariableId <- 0

  (* letrec f = (fn x => x) in [f 4, f true] *)
  let ast =
    [ Stmt.Let(
        "f",
        lambda
          { typeParams = None
            args = [ "x" ]
            body = [ Stmt.Expr(ident "x") ] }
      )
      Stmt.Let(
        "pair",
        tuple
          [ apply (ident "f", [ ident "4" ])
            apply (ident "f", [ ident "true" ]) ]
      ) ]

  let env = getEnv ()

  let newEnv = infer_script ast env

  let f = getType "f" newEnv Set.empty
  let pair = getType "pair" newEnv Set.empty

  Assert.Equal("fn <A>(A) -> A", f.ToString())
  Assert.Equal("[int, bool]", pair.ToString())

[<Fact>]
let RecursiveUnification () =
  (* fn f => f f (fail) *)
  let ast =
    lambda
      { typeParams = None
        args = [ "f" ]
        body = [ Stmt.Expr(apply (ident "f", [ ident "f" ])) ] }

  let env = getEnv ()
  let nonGeneric = Set.empty

  try
    infer_expr ast env nonGeneric |> ignore
  with ex ->
    Assert.Equal("Recursive unification", ex.Message)

[<Fact>]
let InferGenericAndNonGeneric () =
  nextVariableId <- 0

  let ast =
    lambda
      { typeParams = None
        args = [ "g" ]
        body =
          [ Stmt.Let(
              "f",
              lambda
                { typeParams = None
                  args = [ "x" ]
                  body = [ Stmt.Expr(ident "g") ] }
            )
            Stmt.Expr(
              tuple
                [ apply (ident "f", [ ident "3" ])
                  apply (ident "f", [ ident "true" ]) ]
            ) ] }

  let env = getEnv ()
  let nonGeneric = Set.empty

  let t = infer_expr ast env nonGeneric

  (* fn g => let f = fn x => g in [f 3, f true] *)
  Assert.Equal("fn (t0) -> [t0, t0]", t.ToString())

[<Fact>]
let InferFuncComposition () =
  nextVariableId <- 0

  let ast =
    lambda
      { typeParams = None
        args = [ "f" ]
        body =
          [ Stmt.Expr(
              lambda
                { typeParams = None
                  args = [ "g" ]
                  body =
                    [ Stmt.Expr(
                        lambda
                          { typeParams = None
                            args = [ "arg" ]
                            body =
                              [ Stmt.Expr(
                                  apply (
                                    ident "g",
                                    [ apply (ident "f", [ ident "arg" ]) ]
                                  )
                                ) ] }
                      ) ] }
            ) ] }

  let env = getEnv ()
  let nonGeneric = Set.empty

  let t = infer_expr ast env nonGeneric

  (* fn f (fn g (fn arg (f g arg))) *)
  Assert.Equal(
    "fn (fn (t2) -> t3) -> fn (fn (t3) -> t4) -> fn (t2) -> t4",
    t.ToString()
  )

[<Fact>]
let InferSKK () =
  nextVariableId <- 0
  let mutable env = getEnv ()
  let nonGeneric = Set.empty

  let S =
    lambda
      { typeParams = None
        args = [ "f" ]
        body =
          [ Stmt.Expr(
              lambda
                { typeParams = None
                  args = [ "g" ]
                  body =
                    [ Stmt.Expr(
                        lambda
                          { typeParams = None
                            args = [ "x" ]
                            body =
                              [ Stmt.Expr(
                                  apply (
                                    apply (ident "f", [ ident "x" ]),
                                    [ apply (ident "g", [ ident "x" ]) ]
                                  )
                                ) ] }
                      ) ] }
            ) ] }

  let t = infer_expr S env nonGeneric
  env <- ("S", t) :: env

  Assert.Equal(
    "fn (fn (t2) -> fn (t4) -> t5) -> fn (fn (t2) -> t4) -> fn (t2) -> t5",
    t.ToString()
  )

  let t = generalize_func t

  Assert.Equal(
    "fn <A, B, C>(fn (A) -> fn (B) -> C) -> fn (fn (A) -> B) -> fn (A) -> C",
    t.ToString()
  )

  let K1 =
    lambda
      { typeParams = None
        args = [ "x" ]
        body =
          [ Stmt.Expr(
              lambda
                { typeParams = None
                  args = [ "y" ]
                  body = [ Stmt.Expr(ident "x") ] }
            ) ] }

  let t = infer_expr K1 env nonGeneric
  env <- ("K1", t) :: env

  Assert.Equal("fn (t6) -> fn (t7) -> t6", t.ToString())
  let t = generalize_func t
  Assert.Equal("fn <A, B>(A) -> fn (B) -> A", t.ToString())

  let K2 =
    lambda
      { typeParams = None
        args = [ "x" ]
        body =
          [ Stmt.Expr(
              lambda
                { typeParams = None
                  args = [ "y" ]
                  body = [ Stmt.Expr(ident "x") ] }
            ) ] }

  let t = infer_expr K2 env nonGeneric
  env <- ("K2", t) :: env

  Assert.Equal("fn (t8) -> fn (t9) -> t8", t.ToString())
  let t = generalize_func t
  Assert.Equal("fn <A, B>(A) -> fn (B) -> A", t.ToString())

  let I = apply (apply (ident "S", [ ident "K1" ]), [ ident "K2" ])
  let t = infer_expr I env nonGeneric

  Assert.Equal("fn (t10) -> t10", t.ToString())
  let t = generalize_func t
  Assert.Equal("fn <A>(A) -> A", t.ToString())

[<Fact>]
let InferScriptSKK () =
  nextVariableId <- 0
  let mutable env = getEnv ()
  let nonGeneric = Set.empty

  let S =
    lambda
      { typeParams = None
        args = [ "f" ]
        body =
          [ Stmt.Expr(
              lambda
                { typeParams = None
                  args = [ "g" ]
                  body =
                    [ Stmt.Expr(
                        lambda
                          { typeParams = None
                            args = [ "x" ]
                            body =
                              [ Stmt.Expr(
                                  apply (
                                    apply (ident "f", [ ident "x" ]),
                                    [ apply (ident "g", [ ident "x" ]) ]
                                  )
                                ) ] }
                      ) ] }
            ) ] }

  let K =
    lambda
      { typeParams = None
        args = [ "x" ]
        body =
          [ Stmt.Expr(
              lambda
                { typeParams = None
                  args = [ "y" ]
                  body = [ Stmt.Expr(ident "x") ] }

            ) ] }


  let I = apply (apply (ident "S", [ ident "K" ]), [ ident "K" ])

  let script = [ Stmt.Let("S", S); Stmt.Let("K", K); Stmt.Let("I", I) ]

  let newEnv = infer_script script env

  let t = getType "S" newEnv nonGeneric

  Assert.Equal(
    "fn <A, B, C>(fn (A) -> fn (B) -> C) -> fn (fn (A) -> B) -> fn (A) -> C",
    t.ToString()
  )

  let t = getType "K" newEnv nonGeneric
  Assert.Equal("fn <A, B>(A) -> fn (B) -> A", t.ToString())
  let t = getType "I" newEnv nonGeneric
  Assert.Equal("fn <A>(A) -> A", t.ToString())
