module Tests

open Xunit
open FParsec
open FsToolkit.ErrorHandling

open Escalier.HindleyMilner.Syntax
open Escalier.HindleyMilner.TypeChecker
open Escalier.HindleyMilner.TypeVariable

let getEnv () =
  [ ("true", boolType)
    ("zero", makeFunctionType None [ numType ] boolType)
    ("pred", makeFunctionType None [ numType ] numType)
    ("times", makeFunctionType None [ numType; numType ] numType) ]

let ident x = { kind = ExprKind.Ident(x) }

let number x =
  { kind = ExprKind.Literal(Literal.Number x) }

let call (f, args) = { kind = ExprKind.Call(f, args) }

let dummy_span =
  { start = Position("", 0, 0, 0)
    stop = Position("", 0, 0, 0) }

let func paramList stmts =
  let paramList =
    List.map
      (fun name ->
        let pattern =
          { Pattern.kind =
              PatternKind.Identifier(
                { span = dummy_span
                  name = name
                  isMut = false }
              )
            span = dummy_span
            inferred_type = None }

        let param =
          { pattern = pattern
            typeAnn = None
            optional = false }

        param)
      paramList

  { kind =
      ExprKind.Function(
        { sig' =
            { paramList = paramList
              typeParams = None
              ret = None
              throws = None }
          body = { stmts = stmts; span = dummy_span } }
      ) }

let ifelse (cond, thenExpr, elseExpr) =
  { kind = IfElse(cond, thenExpr, elseExpr) }

let binary (op, left, right) =
  { kind = ExprKind.Binary(op, left, right) }

let tuple exprs = { kind = ExprKind.Tuple(exprs) }

[<Fact>]
let InferFactorial () =
  result {
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
        func
          [ "n" ] (* fn n => *)
          [ Stmt.Expr(
              ifelse (
                call (ident "zero", [ ident "n" ]),
                ident "1",
                binary (
                  "times", // op
                  ident "n",
                  call (
                    ident "factorial",
                    [ call (ident "pred", [ ident "n" ]) ]
                  )
                )
              )
            ) ]
      )

    let mutable env = getEnv ()
    let nonGeneric = Set.empty

    let! _, assump = infer_stmt ast env nonGeneric

    match assump with
    | Some(assump) -> env <- assump :: env
    | None -> ()

    let t = getType "factorial" env nonGeneric

    Assert.Equal("fn (number) -> number", t.ToString())
  }

[<Fact>]
let UnificationFailure () =
  nextVariableId <- 0
  (* fn x => [x(3) x(true)] *)
  let ast =
    func
      [ "x" ]
      [ Stmt.Expr(
          tuple
            [ call (ident "x", [ number "3" ])
              call (ident "x", [ ident "true" ]) ]
        ) ]

  let env = getEnv ()
  let nonGeneric = Set.empty

  try
    infer_expr ast env nonGeneric |> ignore
  with ex ->
    Assert.Equal("Type mismatch number != boolean", ex.Message)

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
  result {
    nextVariableId <- 0

    (* letrec f = (fn x => x) in [f 4, f true] *)
    let ast =
      [ Stmt.Let("f", func [ "x" ] [ Stmt.Expr(ident "x") ])
        Stmt.Let(
          "pair",
          tuple
            [ call (ident "f", [ number "4" ])
              call (ident "f", [ ident "true" ]) ]
        ) ]

    let env = getEnv ()

    let! newEnv = infer_script ast env

    let f = getType "f" newEnv Set.empty
    let pair = getType "pair" newEnv Set.empty

    Assert.Equal("fn <A>(A) -> A", f.ToString())
    Assert.Equal("[number, boolean]", pair.ToString())
  }

[<Fact>]
let RecursiveUnification () =
  (* fn f => f f (fail) *)
  let ast = func [ "f" ] [ Stmt.Expr(call (ident "f", [ ident "f" ])) ]

  let env = getEnv ()
  let nonGeneric = Set.empty

  try
    infer_expr ast env nonGeneric |> ignore
  with ex ->
    Assert.Equal("Recursive unification", ex.Message)

[<Fact>]
let InferGenericAndNonGeneric () =
  result {
    nextVariableId <- 0

    let ast =
      func
        [ "g" ]
        [ Stmt.Let("f", func [ "x" ] [ Stmt.Expr(ident "g") ])
          Stmt.Expr(
            tuple
              [ call (ident "f", [ number "3" ])
                call (ident "f", [ ident "true" ]) ]
          ) ]

    let env = getEnv ()
    let nonGeneric = Set.empty

    let! t = infer_expr ast env nonGeneric

    (* fn g => let f = fn x => g in [f 3, f true] *)
    Assert.Equal("fn (t0) -> [t0, t0]", t.ToString())
  }

[<Fact>]
let InferFuncComposition () =
  result {
    nextVariableId <- 0

    let ast =
      func
        [ "f" ]
        [ Stmt.Expr(
            func
              [ "g" ]
              [ Stmt.Expr(
                  func
                    [ "arg" ]
                    [ Stmt.Expr(
                        call (ident "g", [ call (ident "f", [ ident "arg" ]) ])
                      ) ]
                ) ]
          ) ]

    let env = getEnv ()
    let nonGeneric = Set.empty

    let! t = infer_expr ast env nonGeneric

    (* fn f (fn g (fn arg (f g arg))) *)
    Assert.Equal(
      "fn (fn (t2) -> t3) -> fn (fn (t3) -> t4) -> fn (t2) -> t4",
      t.ToString()
    )
  }

[<Fact>]
let InferSKK () =
  result {

    nextVariableId <- 0
    let mutable env = getEnv ()
    let nonGeneric = Set.empty

    let S =
      func
        [ "f" ]
        [ Stmt.Expr(
            func
              [ "g" ]
              [ Stmt.Expr(
                  func
                    [ "x" ]
                    [ Stmt.Expr(
                        call (
                          call (ident "f", [ ident "x" ]),
                          [ call (ident "g", [ ident "x" ]) ]
                        )
                      ) ]
                ) ]
          ) ]

    let! t = infer_expr S env nonGeneric
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

    let K1 = func [ "x" ] [ Stmt.Expr(func [ "y" ] [ Stmt.Expr(ident "x") ]) ]

    let! t = infer_expr K1 env nonGeneric
    env <- ("K1", t) :: env

    Assert.Equal("fn (t6) -> fn (t7) -> t6", t.ToString())
    let t = generalize_func t
    Assert.Equal("fn <A, B>(A) -> fn (B) -> A", t.ToString())

    let K2 = func [ "x" ] [ Stmt.Expr(func [ "y" ] [ Stmt.Expr(ident "x") ]) ]

    let! t = infer_expr K2 env nonGeneric
    env <- ("K2", t) :: env

    Assert.Equal("fn (t8) -> fn (t9) -> t8", t.ToString())
    let t = generalize_func t
    Assert.Equal("fn <A, B>(A) -> fn (B) -> A", t.ToString())

    let I = call (call (ident "S", [ ident "K1" ]), [ ident "K2" ])
    let! t = infer_expr I env nonGeneric

    Assert.Equal("fn (t10) -> t10", t.ToString())
    let t = generalize_func t
    Assert.Equal("fn <A>(A) -> A", t.ToString())
  }

[<Fact>]
let InferScriptSKK () =
  result {
    nextVariableId <- 0
    let mutable env = getEnv ()
    let nonGeneric = Set.empty

    let S =
      func
        [ "f" ]
        [ Stmt.Expr(
            func
              [ "g" ]
              [ Stmt.Expr(
                  func
                    [ "x" ]
                    [ Stmt.Expr(
                        call (
                          call (ident "f", [ ident "x" ]),
                          [ call (ident "g", [ ident "x" ]) ]
                        )
                      ) ]
                ) ]
          ) ]

    let K = func [ "x" ] [ Stmt.Expr(func [ "y" ] [ Stmt.Expr(ident "x") ]) ]
    let I = call (call (ident "S", [ ident "K" ]), [ ident "K" ])

    let script = [ Stmt.Let("S", S); Stmt.Let("K", K); Stmt.Let("I", I) ]

    let! newEnv = infer_script script env

    let t = getType "S" newEnv nonGeneric

    Assert.Equal(
      "fn <A, B, C>(fn (A) -> fn (B) -> C) -> fn (fn (A) -> B) -> fn (A) -> C",
      t.ToString()
    )

    let t = getType "K" newEnv nonGeneric
    Assert.Equal("fn <A, B>(A) -> fn (B) -> A", t.ToString())
    let t = getType "I" newEnv nonGeneric
    Assert.Equal("fn <A>(A) -> A", t.ToString())
  }
