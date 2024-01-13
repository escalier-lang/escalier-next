// TODO: get rid of shared `nextVariableId`
[<Xunit.Collection("Sequential")>]
module NoParseTests

open Xunit
open FParsec
open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Infer
open Escalier.TypeChecker.Prelude

let makeParam (name: string) (ty: Type.Type) : Type.FuncParam =
  { Pattern = Type.Pattern.Identifier name
    Type = ty
    Optional = false }

let getEnv () =
  let values =
    Map.ofList
      [ ("true", (boolType, false))
        ("zero",
         (makeFunctionType None [ makeParam "arg" numType ] boolType never,
          false))
        ("pred",
         (makeFunctionType None [ makeParam "arg" numType ] numType never, false))
        ("times",
         (makeFunctionType
           None
           [ makeParam "left" numType; makeParam "right" numType ]
           numType
           never,
          false)) ]

  { Values = values
    Schemes = Map.empty
    IsAsync = false
    IsPatternMatching = false }

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
              PatternKind.Ident
                { Name = name
                  IsMut = false
                  Assertion = None }
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
              Throws = None
              IsAsync = false }
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
              PatternKind.Ident(
                { Name = name
                  IsMut = false
                  Assertion = None }
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
              Throws = None
              IsAsync = false }
          Body = BlockOrExpr.Expr expr }
      )
    Span = dummySpan
    InferredType = None }

let ifelse (cond, thenExpr, elseExpr) =
  { Expr.Kind = ExprKind.IfElse(cond, thenExpr, elseExpr)
    Span = dummySpan
    InferredType = None }

let binary (op, left, right) =
  { Expr.Kind = ExprKind.Binary(op, left, right)
    Span = dummySpan
    InferredType = None }

let tuple elems =
  { Expr.Kind = ExprKind.Tuple { Elems = elems; Immutable = false }
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
        PatternKind.Ident
          { Name = name
            IsMut = false
            Assertion = None }
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

    let ctx =
      Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

    let! stmtEnv = inferStmt ctx env ast false
    env <- stmtEnv

    let! t = env.GetType "factorial"

    // It's fine that `throws` is `t5` since we don't call `generalizeFunc` in
    // this test.
    Assert.Equal("fn (arg0: number) -> number throws t5", t.ToString())
  }

[<Fact>]
let UnificationFailure () =
  (* fn x => [x(3) x(true)] *)
  let ast =
    func
      [ "x" ]
      [ tuple
          [ call (ident "x", [ number (Number.Int 3) ])
            call (ident "x", [ boolean true ]) ]
        |> StmtKind.Expr
        |> stmt ]

  let env = getEnv ()

  let ctx =
    Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

  try
    inferExpr ctx env ast |> ignore
  with ex ->
    Assert.Equal("Type mismatch 3 != true", ex.Message)

[<Fact>]
let UndefinedSymbol () =
  let ast = ident "foo"
  let env = getEnv ()

  let ctx =
    Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

  try
    inferExpr ctx env ast |> ignore
  with ex ->
    Assert.Equal("Undefined symbol foo", ex.Message)

[<Fact>]
let InferPair () =
  result {
    (* letrec f = (fn x => x) in [f 4, f true] *)
    let ast =
      { Items =
          [ varDecl ("f", fatArrow [ "x" ] (ident "x")) |> ModuleItem.Stmt
            varDecl (
              "pair",
              tuple
                [ call (ident "f", [ number (Number.Int 4) ])
                  call (ident "f", [ boolean true ]) ]
            )
            |> ModuleItem.Stmt ] }

    let env = getEnv ()

    let ctx =
      Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

    let! newEnv = inferScript ctx env "input.esc" ast

    let! f = newEnv.GetType "f"
    let! pair = newEnv.GetType "pair"

    Assert.Equal("fn <A>(x: A) -> A", f.ToString())
    Assert.Equal("[4, true]", pair.ToString())
  }

[<Fact>]
let RecursiveUnification () =
  (* fn f => f f (fail) *)
  let ast =
    func [ "f" ] [ StmtKind.Expr(call (ident "f", [ ident "f" ])) |> stmt ]

  let env = getEnv ()

  let ctx =
    Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

  try
    inferExpr ctx env ast |> ignore
  with ex ->
    Assert.Equal("Recursive unification", ex.Message)

[<Fact>]
let InferGenericAndNonGeneric () =
  result {
    let ast =
      { Items =
          [ varDecl (
              "foo",
              func
                [ "g" ]
                [ (varDecl ("f", fatArrow [ "x" ] (ident "g")))
                  stmt (
                    StmtKind.Return(
                      Some(
                        tuple
                          [ call (ident "f", [ number (Number.Int 3) ])
                            call (ident "f", [ boolean true ]) ]
                      )
                    )
                  ) ]
            )
            |> ModuleItem.Stmt ] }

    let env = getEnv ()

    let ctx =
      Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

    let! newEnv = inferScript ctx env "input.esc" ast

    let! t = newEnv.GetType "foo"
    (* fn g => let f = fn x => g in [f 3, f true] *)
    Assert.Equal("fn <A>(g: A) -> [A, A]", t.ToString())
  }

[<Fact>]
let InferFuncComposition () =
  result {
    let ast =
      {

        Items =
          [ varDecl (
              "foo",
              fatArrow
                [ "f" ]
                (fatArrow
                  [ "g" ]
                  (fatArrow
                    [ "arg" ]
                    (call (ident "g", [ call (ident "f", [ ident "arg" ]) ]))))

            )
            |> ModuleItem.Stmt ] }

    let env = getEnv ()

    let ctx =
      Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

    let! newEnv = inferScript ctx env "input.esc" ast

    let! t = newEnv.GetType "foo"
    (* fn f (fn g (fn arg (f g arg))) *)
    Assert.Equal(
      "fn <A, B, C>(f: fn (arg0: A) -> B) -> fn (g: fn (arg0: B) -> C) -> fn (arg: A) -> C",
      t.ToString()
    )
  }

[<Fact>]
let InferScriptSKK () =
  result {
    let mutable env = getEnv ()

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

    let script =
      { Items =
          [ varDecl ("S", s) |> ModuleItem.Stmt
            varDecl ("K", k) |> ModuleItem.Stmt
            varDecl ("I", i) |> ModuleItem.Stmt ] }

    let ctx =
      Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

    let! newEnv = inferScript ctx env "input.esc" script

    let! t = newEnv.GetType "S"

    Assert.Equal(
      "fn <A, B, C>(f: fn (arg0: A) -> fn (arg0: B) -> C) -> fn (g: fn (arg0: A) -> B) -> fn (x: A) -> C",
      t.ToString()
    )

    let! t = newEnv.GetType "K"
    Assert.Equal("fn <A, B>(x: A) -> fn (y: B) -> A", t.ToString())
    let! t = newEnv.GetType "I"
    Assert.Equal("fn <A>(x: A) -> A", t.ToString())
  }
