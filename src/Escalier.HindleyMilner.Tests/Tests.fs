module Tests

open Xunit

open Escalier.HindleyMilner.Syntax
open Escalier.HindleyMilner.TypeChecker

let getEnv () =
  [ ("true", boolType)
    ("zero", makeFunctionType [ intType ] boolType)
    ("pred", makeFunctionType [ intType ] intType)
    ("times", makeFunctionType [ intType; intType ] intType) ]

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
      Lambda(
        [ "n" ], (* fn n => *)
        [ Stmt.Expr(
            IfElse(
              Apply(Ident("zero"), [ Ident("n") ]),
              Ident("1"),
              Binary(
                "times", // op
                Ident("n"),
                Apply(
                  Ident("factorial"),
                  [ Apply(Ident("pred"), [ Ident("n") ]) ]
                )
              )
            )
          ) ]
      )
    )

  let env = getEnv ()
  let nonGeneric = Set.empty

  let (_, env) = infer_stmt ast env nonGeneric
  let t = getType "factorial" env nonGeneric

  Assert.Equal("fn (int) -> int", t.ToString())

[<Fact>]
let UnificationFailure () =
  nextVariableId <- 0
  (* fn x => [x(3) x(true)] *)
  let ast =
    Lambda(
      [ "x" ],
      [ Stmt.Expr(
          Expr.Tuple(
            [ Apply(Ident("x"), [ Ident("3") ])
              Apply(Ident("x"), [ Ident("true") ]) ]
          )
        ) ]
    )

  let env = getEnv ()
  let nonGeneric = Set.empty

  try
    infer_expr ast env nonGeneric |> ignore
  with ex ->
    Assert.Equal("Type mismatch int != bool", ex.Message)

[<Fact>]
let UndefinedSymbol () =
  nextVariableId <- 0
  let ast = Ident("foo")
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
    [ Stmt.Let("f", Lambda([ "x" ], [ Stmt.Expr(Ident("x")) ]))
      Stmt.Let(
        "pair",
        Expr.Tuple(
          [ Apply(Ident("f"), [ Ident("4") ])
            Apply(Ident("f"), [ Ident("true") ]) ]
        )
      ) ]

  let env = getEnv ()

  let newEnv = infer_script ast env

  let f = getType "f" newEnv Set.empty
  let pair = getType "pair" newEnv Set.empty

  Assert.Equal("fn (t5) -> t5", f.ToString())
  Assert.Equal("[int, bool]", pair.ToString())

[<Fact>]
let RecursiveUnification () =
  (* fn f => f f (fail) *)
  let ast = Lambda([ "f" ], [ Stmt.Expr(Apply(Ident("f"), [ Ident("f") ])) ])
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
    Lambda(
      [ "g" ],
      [ Stmt.Let("f", Lambda([ "x" ], [ Stmt.Expr(Ident("g")) ]))
        Stmt.Expr(
          Expr.Tuple(
            [ Apply(Ident("f"), [ Ident("3") ])
              Apply(Ident("f"), [ Ident("true") ]) ]
          )
        ) ]
    )

  let env = getEnv ()
  let nonGeneric = Set.empty

  let t = infer_expr ast env nonGeneric

  (* fn g => let f = fn x => g in [f 3, f true] *)
  Assert.Equal("fn (t0) -> [t0, t0]", t.ToString())

[<Fact>]
let InferFuncComposition () =
  nextVariableId <- 0

  let ast =
    Lambda(
      [ "f" ],
      [ Stmt.Expr(
          Lambda(
            [ "g" ],
            [ Stmt.Expr(
                Lambda(
                  [ "arg" ],
                  [ Stmt.Expr(
                      Apply(Ident("g"), [ Apply(Ident("f"), [ Ident("arg") ]) ])
                    ) ]
                )
              ) ]
          )
        ) ]
    )

  let env = getEnv ()
  let nonGeneric = Set.empty

  let t = infer_expr ast env nonGeneric

  (* fn f (fn g (fn arg (f g arg))) *)
  Assert.Equal(
    "fn (fn (t2) -> t3) -> fn (fn (t3) -> t4) -> fn (t2) -> t4",
    t.ToString()
  )

[<Fact>]
let InfersSKK () =
  nextVariableId <- 0
  let mutable env = getEnv ()
  let nonGeneric = Set.empty

  let S =
    Lambda(
      [ "f" ],
      [ Stmt.Expr(
          Lambda(
            [ "g" ],
            [ Stmt.Expr(
                Lambda(
                  [ "x" ],
                  [ Stmt.Expr(
                      Apply(
                        Apply(Ident("f"), [ Ident("x") ]),
                        [ Apply(Ident("g"), [ Ident("x") ]) ]
                      )
                    ) ]
                )
              ) ]
          )
        ) ]
    )

  let t = infer_expr S env nonGeneric
  env <- ("S", t) :: env

  Assert.Equal(
    "fn (fn (t2) -> fn (t4) -> t5) -> fn (fn (t2) -> t4) -> fn (t2) -> t5",
    t.ToString()
  )

  let K1 =
    Lambda([ "x" ], [ Stmt.Expr(Lambda([ "y" ], [ Stmt.Expr(Ident("x")) ])) ])

  let t = infer_expr K1 env nonGeneric
  env <- ("K1", t) :: env

  Assert.Equal("fn (t6) -> fn (t7) -> t6", t.ToString())

  let K2 =
    Lambda([ "x" ], [ Stmt.Expr(Lambda([ "y" ], [ Stmt.Expr(Ident("x")) ])) ])

  let t = infer_expr K2 env nonGeneric
  env <- ("K2", t) :: env

  Assert.Equal("fn (t8) -> fn (t9) -> t8", t.ToString())

  let I = Apply(Apply(Ident("S"), [ Ident("K1") ]), [ Ident("K2") ])
  let t = infer_expr I env nonGeneric

  Assert.Equal("fn (t10) -> t10", t.ToString())
