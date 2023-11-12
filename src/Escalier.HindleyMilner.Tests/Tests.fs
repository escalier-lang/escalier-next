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
  let env = getEnv ()

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
        IfElse(
          Apply(Ident("zero"), [ Ident("n") ]),
          Ident("1"),
          Apply(
            Ident("times"),
            [ Ident("n")
              Apply(
                Ident("factorial"),
                [ Apply(Ident("pred"), [ Ident("n") ]) ]
              ) ]
          )
        )
      ),
      Ident("factorial") // Apply(Ident("factorial"), Ident("5"))
    )

  let t = infer ast env

  Assert.Equal("fn (int) -> int", t.ToString())

[<Fact>]
let UnificationFailure () =
  nextVariableId <- 0
  (* fn x => [x(3) x(true)] *)
  let ast =
    Lambda(
      [ "x" ],
      Expr.Tuple(
        [ Apply(Ident("x"), [ Ident("3") ])
          Apply(Ident("x"), [ Ident("true") ]) ]
      )
    )

  let env = getEnv ()

  try
    infer ast env |> ignore
  with ex ->
    Assert.Equal("Type mismatch int != bool", ex.Message)

[<Fact>]
let UndefinedSymbol () =
  nextVariableId <- 0
  let ast = Ident("foo")
  let env = getEnv ()

  try
    infer ast env |> ignore
  with ex ->
    Assert.Equal("Undefined symbol foo", ex.Message)

[<Fact>]
let InferPair () =
  nextVariableId <- 0

  let pair =
    Expr.Tuple(
      [ Apply(Ident("f"), [ Ident("4") ])
        Apply(Ident("f"), [ Ident("true") ]) ]
    )

  (* letrec f = (fn x => x) in [f 4, f true] *)
  let ast = Let("f", Lambda([ "x" ], Ident("x")), pair)
  let env = getEnv ()

  let t = infer ast env

  Assert.Equal("[int, bool]", t.ToString())

[<Fact>]
let RecursiveUnification () =
  (* fn f => f f (fail) *)
  let ast = Lambda([ "f" ], Apply(Ident("f"), [ Ident("f") ]))
  let env = getEnv ()

  try
    infer ast env |> ignore
  with ex ->
    Assert.Equal("Recursive unification", ex.Message)

[<Fact>]
let InferGenericAndNonGeneric () =
  nextVariableId <- 0

  let ast =
    Lambda(
      [ "g" ],
      Let(
        "f",
        Lambda([ "x" ], Ident("g")),
        Expr.Tuple(
          [ Apply(Ident("f"), [ Ident("3") ])
            Apply(Ident("f"), [ Ident("true") ]) ]
        )
      )
    )

  let env = getEnv ()

  let t = infer ast env

  (* fn g => let f = fn x => g in [f 3, f true] *)
  Assert.Equal("fn (t0) -> [t0, t0]", t.ToString())

[<Fact>]
let InferFuncComposition () =
  nextVariableId <- 0

  let ast =
    Lambda(
      [ "f" ],
      Lambda(
        [ "g" ],
        Lambda(
          [ "arg" ],
          Apply(Ident("g"), [ Apply(Ident("f"), [ Ident("arg") ]) ])
        )
      )
    )

  let env = getEnv ()

  let t = infer ast env

  (* fn f (fn g (fn arg (f g arg))) *)
  Assert.Equal(
    "fn (fn (t2) -> t3) -> fn (fn (t3) -> t4) -> fn (t2) -> t4",
    t.ToString()
  )

[<Fact>]
let InfersSKK () =
  nextVariableId <- 0
  let mutable env = getEnv ()

  let S =
    Lambda(
      [ "f" ],
      Lambda(
        [ "g" ],
        Lambda(
          [ "x" ],
          Apply(
            Apply(Ident("f"), [ Ident("x") ]),
            [ Apply(Ident("g"), [ Ident("x") ]) ]
          )
        )
      )
    )

  let t = infer S env
  env <- ("S", t) :: env

  Assert.Equal(
    "fn (fn (t2) -> fn (t4) -> t5) -> fn (fn (t2) -> t4) -> fn (t2) -> t5",
    t.ToString()
  )

  let K1 = Lambda([ "x" ], Lambda([ "y" ], Ident("x")))
  let t = infer K1 env
  env <- ("K1", t) :: env

  Assert.Equal("fn (t6) -> fn (t7) -> t6", t.ToString())

  let K2 = Lambda([ "x" ], Lambda([ "y" ], Ident("x")))
  let t = infer K2 env
  env <- ("K2", t) :: env

  Assert.Equal("fn (t8) -> fn (t9) -> t8", t.ToString())

  let I = Apply(Apply(Ident("S"), [ Ident("K1") ]), [ Ident("K2") ])
  let t = infer I env

  Assert.Equal("fn (t10) -> t10", t.ToString())
