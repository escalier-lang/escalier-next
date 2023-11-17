// TODO: get rid of shared `nextVariableId`
[<Xunit.Collection("Sequential")>]
module Tests

open Xunit
open FParsec
open FsToolkit.ErrorHandling

open Escalier.HindleyMilner.Syntax
open Escalier.HindleyMilner.TypeChecker
open Escalier.HindleyMilner.TypeVariable

let makeParam
  (name: string)
  (ty: Escalier.HindleyMilner.Type.Type)
  : Escalier.HindleyMilner.Type.FuncParam =
  { Pattern = Escalier.HindleyMilner.Type.Pattern.Identifier name
    Type = ty
    Optional = false }

let getEnv () =
  let values =
    Map.ofList
      [ ("true", (boolType, false))
        ("zero",
         (makeFunctionType None [ makeParam "arg" numType ] boolType, false))
        ("pred",
         (makeFunctionType None [ makeParam "arg" numType ] numType, false))
        ("times",
         (makeFunctionType
           None
           [ makeParam "left" numType; makeParam "right" numType ]
           numType,
          false)) ]

  { Values = values
    Schemes = Map.empty
    IsAsync = false }

let dummySpan =
  { Start = Position("", 0, 0, 0)
    Stop = Position("", 0, 0, 0) }

let ident x =
  { Expr.Kind = ExprKind.Identifier(x)
    Span = dummySpan
    InferredType = None }

let number x =
  { Expr.Kind = ExprKind.Literal(Literal.Number x)
    Span = dummySpan
    InferredType = None }

let boolean x =
  { Expr.Kind = ExprKind.Literal(Literal.Boolean x)
    Span = dummySpan
    InferredType = None }

let call (f, args) =
  let c =
    { Callee = f
      TypeArgs = None
      Args = args
      OptChain = false
      Throws = None }

  { Expr.Kind = ExprKind.Call(c)
    Span = dummySpan
    InferredType = None }

let func paramList stmts =
  let paramList =
    List.map
      (fun name ->
        let pattern =
          { Pattern.Kind =
              PatternKind.Identifier(
                { Span = dummySpan
                  Name = name
                  IsMut = false }
              )
            Span = dummySpan
            InferredType = None }

        let param =
          { Pattern = pattern
            TypeAnn = None
            Optional = false }

        param)
      paramList

  { Expr.Kind =
      ExprKind.Function(
        { Sig =
            { ParamList = paramList
              TypeParams = None
              ReturnType = None
              Throws = None }
          Body = BlockOrExpr.Block { Stmts = stmts; Span = dummySpan } }
      )
    Span = dummySpan
    InferredType = None }

let fatArrow paramList expr =
  let paramList =
    List.map
      (fun name ->
        let pattern =
          { Pattern.Kind =
              PatternKind.Identifier(
                { Span = dummySpan
                  Name = name
                  IsMut = false }
              )
            Span = dummySpan
            InferredType = None }

        let param =
          { Pattern = pattern
            TypeAnn = None
            Optional = false }

        param)
      paramList

  { Expr.Kind =
      ExprKind.Function(
        { Sig =
            { ParamList = paramList
              TypeParams = None
              ReturnType = None
              Throws = None }
          Body = BlockOrExpr.Expr expr }
      )
    Span = dummySpan
    InferredType = None }

let ifelse (cond, thenExpr, elseExpr) =
  { Expr.Kind = IfElse(cond, thenExpr, elseExpr)
    Span = dummySpan
    InferredType = None }

let binary (op, left, right) =
  { Expr.Kind = ExprKind.Binary(op, left, right)
    Span = dummySpan
    InferredType = None }

let tuple exprs =
  { Expr.Kind = ExprKind.Tuple(exprs)
    Span = dummySpan
    InferredType = None }

let block stmts =
  BlockOrExpr.Block { Stmts = stmts; Span = dummySpan }

let stmt stmtKind =
  { Stmt.Kind = stmtKind
    Span = dummySpan }

let letrec (name, expr) =
  { Kind =
      StmtKind.Decl(
        { Kind = DeclKind.LetRec(name, expr)
          Span = dummySpan }
      )
    Span = dummySpan }

let let' (name, expr) =
  let pattern =
    { Pattern.Kind =
        PatternKind.Identifier(
          { Span = dummySpan
            Name = name
            IsMut = false }
        )
      Span = dummySpan
      InferredType = None }

  { Kind =
      StmtKind.Decl(
        { Kind = DeclKind.Let(pattern, expr)
          Span = dummySpan }
      )
    Span = dummySpan }

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
      letrec (
        "factorial",
        fatArrow
          [ "n" ] (* fn n => *)
          (ifelse (
            call (ident "zero", [ ident "n" ]),
            (block [ ident "1" |> StmtKind.Expr |> stmt ]),
            Some(
              BlockOrExpr.Block
                { Stmts =
                    [ binary (
                        "times", // op
                        ident "n",
                        call (
                          ident "factorial",
                          [ call (ident "pred", [ ident "n" ]) ]
                        )
                      )
                      |> Some
                      |> StmtKind.Return
                      |> stmt ]
                  Span = dummySpan }
            )
          ))
      )

    let mutable env = getEnv ()
    let nonGeneric = Set.empty

    let! _, assumps = inferStmt ast env nonGeneric

    match assumps with
    | Some(assumps) ->
      for KeyValue(name, t) in assumps do
        env <- env.AddValue name t
    | None -> ()

    let t = getType "factorial" env nonGeneric

    // TODO: figure out how to preserve the param name
    Assert.Equal("fn (arg0: number) -> number", t.ToString())
  }

[<Fact>]
let UnificationFailure () =
  nextVariableId <- 0
  (* fn x => [x(3) x(true)] *)
  let ast =
    func
      [ "x" ]
      [ tuple
          [ call (ident "x", [ number "3" ])
            call (ident "x", [ boolean true ]) ]
        |> StmtKind.Expr
        |> stmt ]

  let env = getEnv ()
  let nonGeneric = Set.empty

  try
    inferExpr ast env nonGeneric |> ignore
  with ex ->
    Assert.Equal("Type mismatch 3 != true", ex.Message)

[<Fact>]
let UndefinedSymbol () =
  nextVariableId <- 0
  let ast = ident "foo"
  let env = getEnv ()
  let nonGeneric = Set.empty

  try
    inferExpr ast env nonGeneric |> ignore
  with ex ->
    Assert.Equal("Undefined symbol foo", ex.Message)

[<Fact>]
let InferPair () =
  result {
    nextVariableId <- 0

    (* letrec f = (fn x => x) in [f 4, f true] *)
    let ast =
      [ let' ("f", fatArrow [ "x" ] (ident "x"))
        let' (
          "pair",
          tuple
            [ call (ident "f", [ number "4" ])
              call (ident "f", [ boolean true ]) ]
        ) ]

    let env = getEnv ()

    let! newEnv = inferScript ast env

    let f = getType "f" newEnv Set.empty
    let pair = getType "pair" newEnv Set.empty

    Assert.Equal("fn <A>(x: A) -> A", f.ToString())
    Assert.Equal("[4, true]", pair.ToString())
  }

[<Fact>]
let RecursiveUnification () =
  (* fn f => f f (fail) *)
  let ast =
    func [ "f" ] [ StmtKind.Expr(call (ident "f", [ ident "f" ])) |> stmt ]

  let env = getEnv ()
  let nonGeneric = Set.empty

  try
    inferExpr ast env nonGeneric |> ignore
  with ex ->
    Assert.Equal("Recursive unification", ex.Message)

[<Fact>]
let InferGenericAndNonGeneric () =
  result {
    printfn "InferGenericAndNonGeneric - start"
    nextVariableId <- 0

    let ast =
      [ let' (
          "foo",
          func
            [ "g" ]
            [ (let' ("f", fatArrow [ "x" ] (ident "g")))
              stmt (
                StmtKind.Return(
                  Some(
                    tuple
                      [ call (ident "f", [ number "3" ])
                        call (ident "f", [ boolean true ]) ]
                  )
                )
              ) ]
        ) ]

    let env = getEnv ()
    let nonGeneric = Set.empty

    let! newEnv = inferScript ast env

    let t = getType "foo" newEnv nonGeneric
    (* fn g => let f = fn x => g in [f 3, f true] *)
    Assert.Equal("fn <A>(g: A) -> [A, A]", t.ToString())
    printfn "InferGenericAndNonGeneric - end"
  }

[<Fact>]
let InferFuncComposition () =
  result {
    nextVariableId <- 0

    let ast =
      fatArrow
        [ "f" ]
        (fatArrow
          [ "g" ]
          (fatArrow
            [ "arg" ]
            (call (ident "g", [ call (ident "f", [ ident "arg" ]) ]))))


    let env = getEnv ()
    let nonGeneric = Set.empty

    let! t = inferExpr ast env nonGeneric

    (* fn f (fn g (fn arg (f g arg))) *)
    Assert.Equal(
      "fn (f: fn (arg0: t4) -> t8) -> fn (g: fn (arg0: t8) -> t6) -> fn (arg: t4) -> t6",
      t.ToString()
    )
  }

[<Fact>]
let InferScriptSKK () =
  result {
    nextVariableId <- 0
    let mutable env = getEnv ()
    let nonGeneric = Set.empty

    let s =
      func
        [ "f" ]
        [ func
            [ "g" ]
            [ func
                [ "x" ]
                [ call (
                    call (ident "f", [ ident "x" ]),
                    [ call (ident "g", [ ident "x" ]) ]
                  )
                  |> StmtKind.Expr
                  |> stmt ]
              |> StmtKind.Expr
              |> stmt ]
          |> StmtKind.Expr
          |> stmt ]

    let k =
      func
        [ "x" ]
        [ func [ "y" ] [ ident "x" |> StmtKind.Expr |> stmt ]
          |> StmtKind.Expr
          |> stmt ]

    let i = call (call (ident "S", [ ident "K" ]), [ ident "K" ])

    let script = [ let' ("S", s); let' ("K", k); let' ("I", i) ]

    let! newEnv = inferScript script env

    let t = getType "S" newEnv nonGeneric

    Assert.Equal(
      "fn <A, C, B>(f: fn (arg0: A) -> fn (arg0: B) -> C) -> fn (g: fn (arg0: A) -> B) -> fn (x: A) -> C",
      t.ToString()
    )

    let t = getType "K" newEnv nonGeneric
    Assert.Equal("fn <A, B>(x: A) -> fn (y: B) -> A", t.ToString())
    let t = getType "I" newEnv nonGeneric
    Assert.Equal("fn <A>(x: A) -> A", t.ToString())
  }
