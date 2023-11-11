module Tests

open Xunit

open Escalier.HindleyMilner.Syntax
open Escalier.HindleyMilner.Type
open Escalier.HindleyMilner.TypeChecker

let getEnv () =
  let var1 = makeVariable ()
  let var2 = makeVariable ()
  let pairTy = { kind = TypeOp({ name = "*"; types = [ var1; var2 ] }) }
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

[<Fact>]
let InferFactorial () =
  nextVariableId <- 0
  let env = getEnv ()

  let ast =
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

  let t = infer ast env

  Assert.Equal("(int -> int)", t.ToString())

[<Fact>]
let UnificationFailure () =
  nextVariableId <- 0
  (* fn x => (pair(x(3) (x(true))) *)
  let ast =
    Lambda(
      "x",
      Apply(
        Apply(Ident("pair"), Apply(Ident("x"), Ident("3"))),
        Apply(Ident("x"), Ident("true"))
      )
    )

  let env = getEnv ()

  try
    infer ast env |> ignore
  with ex ->
    Assert.Equal("Type mismatch bool != int", ex.Message)

[<Fact>]
let UndefinedSymbol () =
  nextVariableId <- 0
  (* pair(f(3), f(true)) *)
  let ast =
    Apply(
      Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))),
      Apply(Ident("f"), Ident("true"))
    )

  let env = getEnv ()

  try
    infer ast env |> ignore
  with ex ->
    Assert.Equal("Undefined symbol f", ex.Message)

[<Fact>]
let InferPair () =
  nextVariableId <- 0
  (* letrec f = (fn x => x) in ((pair (f 4)) (f true)) *)
  let pair =
    Apply(
      Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))),
      Apply(Ident("f"), Ident("true"))
    )

  let ast = Let("f", Lambda("x", Ident("x")), pair)
  let env = getEnv ()

  let t = infer ast env

  Assert.Equal("(int * bool)", t.ToString())

[<Fact>]
let RecursiveUnification () =
  (* fn f => f f (fail) *)
  let ast = Lambda("f", Apply(Ident("f"), Ident("f")))
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

  let env = getEnv ()

  let t = infer ast env

  (* fn g => let f = fn x => g in pair (f 3, f true) *)
  Assert.Equal("(t5 -> (t5 * t6))", t.ToString())

[<Fact>]
let InferFuncComposition () =
  nextVariableId <- 0

  let ast =
    Lambda(
      "f",
      Lambda(
        "g",
        Lambda("arg", Apply(Ident("g"), Apply(Ident("f"), Ident("arg"))))
      )
    )

  let env = getEnv ()

  let t = infer ast env

  (* fn f (fn g (fn arg (f g arg))) *)
  Assert.Equal("((t5 -> t6) -> ((t6 -> t7) -> (t5 -> t7)))", t.ToString())

[<Fact>]
let InfersSKK () =
  nextVariableId <- 0
  let mutable env = getEnv ()

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

  let t = infer S env
  env <- ("S", t) :: env

  Assert.Equal(
    "((t5 -> (t7 -> t8)) -> ((t5 -> t7) -> (t5 -> t8)))",
    t.ToString()
  )

  let K1 = Lambda("x", Lambda("y", Ident("x")))
  let t = infer K1 env
  env <- ("K1", t) :: env

  Assert.Equal("(t9 -> (t10 -> t9))", t.ToString())

  let K2 = Lambda("x", Lambda("y", Ident("x")))
  let t = infer K2 env
  env <- ("K2", t) :: env

  Assert.Equal("(t11 -> (t12 -> t11))", t.ToString())

  let I = Apply(Apply(Ident("S"), Ident("K1")), Ident("K2"))
  let t = infer I env

  Assert.Equal("(t15 -> t15)", t.ToString())
