// TODO: get rid of shared `nextVariableId`
[<Xunit.Collection("Sequential")>]
module NoParseTests

open Xunit
open FParsec
open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Syntax
open Escalier.TypeChecker.TypeChecker
open Escalier.TypeChecker.TypeVariable

let makeParam (name: string) (ty: Type.Type) : Type.FuncParam =
  { Pattern = Type.Pattern.Identifier name
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

let varDecl (name, expr) =
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
        { Kind = DeclKind.VarDecl(pattern, expr, None)
          Span = dummySpan }
      )
    Span = dummySpan }

[<Fact>]
let InferFactorial () =
  result {
    nextVariableId <- 0

    (* let factorial =
        fn n =>
          if (zero n) 
            then 1 
            else (times n (factorial (pred n)))
        in factorial
     *)
    let ast =
      varDecl (
        "factorial",
        fatArrow
          [ "n" ] (* fn n => *)
          (ifelse (
            call (ident "zero", [ ident "n" ]),
            { Stmts = [ ident "1" |> StmtKind.Expr |> stmt ]
              Span = dummySpan },
            Some(
              BlockOrExpr.Expr(
                binary (
                  "times", // op
                  ident "n",
                  call (
                    ident "factorial",
                    [ call (ident "pred", [ ident "n" ]) ]
                  )
                )
              )
            )
          ))
      )

    let mutable env = getEnv ()
    let nonGeneric = Set.empty

    let! _, stmtResult = inferStmt ast env nonGeneric

    match stmtResult with
    | Some(StmtResult.Bindings assumps) ->
      for KeyValue(name, t) in assumps do
        env <- env.AddValue name t
    | Some(StmtResult.Scheme(name, scheme)) -> env <- env.AddScheme name scheme
    | None -> ()

    let t = getType "factorial" env nonGeneric

    Assert.Equal("fn (n: number) -> number", t.ToString())
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
      [ varDecl ("f", fatArrow [ "x" ] (ident "x"))
        varDecl (
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
    nextVariableId <- 0

    let ast =
      [ varDecl (
          "foo",
          func
            [ "g" ]
            [ (varDecl ("f", fatArrow [ "x" ] (ident "g")))
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
  }

[<Fact>]
let InferFuncComposition () =
  result {
    nextVariableId <- 0

    let ast =
      [ varDecl (
          "foo",
          fatArrow
            [ "f" ]
            (fatArrow
              [ "g" ]
              (fatArrow
                [ "arg" ]
                (call (ident "g", [ call (ident "f", [ ident "arg" ]) ]))))

        ) ]


    let env = getEnv ()

    let! newEnv = inferScript ast env

    let t = getType "foo" newEnv Set.empty
    (* fn f (fn g (fn arg (f g arg))) *)
    Assert.Equal(
      "fn <A, C, B>(f: fn (arg0: A) -> B) -> fn (g: fn (arg0: B) -> C) -> fn (arg: A) -> C",
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

    let script = [ varDecl ("S", s); varDecl ("K", k); varDecl ("I", i) ]

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
