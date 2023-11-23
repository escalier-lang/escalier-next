namespace Escalier.Codegen

open Escalier.Codegen.TypeScript
open Escalier.Data.Syntax

module rec Codegen =
  module TS = TypeScript

  type Ctx = { mutable NextTempId: int }

  type Finalizer =
    | Assign of string
    | Return
    | Empty

  let buildScript (ctx: Ctx) (block: Block) =
    buildBlock ctx block Finalizer.Empty

  let buildExpr (ctx: Ctx) (expr: Expr) : TS.Expr * list<TS.Stmt> =

    match expr.Kind with
    | ExprKind.Call { Callee = callee; Args = args } ->
      let calleeExpr, calleeStmts = buildExpr ctx callee
      let argExprs, argStmts = args |> List.map (buildExpr ctx) |> List.unzip

      let callExpr =
        Expr.Call
          { Callee = calleeExpr
            Arguments = argExprs
            Loc = None }

      (callExpr, calleeStmts @ (argStmts |> List.concat))
    | ExprKind.Identifier name ->
      Expr.Identifier { Name = name; Loc = None }, []
    | ExprKind.Literal lit ->
      let litVal =
        match lit with
        | Literal.Boolean b -> LiteralValue.Boolean b
        | Literal.String s -> LiteralValue.String s
        | Literal.Number n -> LiteralValue.Number(n |> float)
        | Literal.Null -> LiteralValue.Null
        | Literal.Undefined -> LiteralValue.Undefined

      Expr.Literal { Value = litVal; Loc = None }, []
    | ExprKind.Binary(op, left, right) ->
      let binaryOp =
        match op with
        | "+" -> Some(BinaryOperator.Plus)
        | "-" -> Some(BinaryOperator.Minus)
        | "*" -> Some(BinaryOperator.Multiply)
        | "/" -> Some(BinaryOperator.Divide)
        | "==" -> Some(BinaryOperator.Equal)
        | "!=" -> Some(BinaryOperator.NotEqual)
        | "<" -> Some(BinaryOperator.LessThan)
        | "<=" -> Some(BinaryOperator.LessThanOrEqual)
        | ">" -> Some(BinaryOperator.GreaterThan)
        | ">=" -> Some(BinaryOperator.GreaterThanOrEqual)
        | _ -> None

      let leftExpr, leftStmts = buildExpr ctx left
      let rightExpr, rightStmts = buildExpr ctx right

      let binExpr =
        match binaryOp with
        | Some(op) ->
          Expr.Binary
            { Operator = op
              Left = leftExpr
              Right = rightExpr
              Loc = None }
        | None -> failwith "TODO"

      (binExpr, leftStmts @ rightStmts)
    | ExprKind.Do block ->
      let tempId = $"temp{ctx.NextTempId}"
      ctx.NextTempId <- ctx.NextTempId + 1

      let tempDecl =
        { Declarations =
            [ { Id =
                  Pat.Ident
                    { Id = { Name = tempId; Loc = None }
                      TypeAnn = None
                      Loc = None }
                Init = None } ]
          Kind = VariableDeclarationKind.Var }

      let finalizer = Finalizer.Assign tempId
      let blockStmt = buildBlock ctx block finalizer
      let expr = Expr.Identifier { Name = tempId; Loc = None }

      let stmts =
        [ Stmt.Declaration(Declaration.Variable tempDecl)
          Stmt.Block blockStmt ]

      (expr, stmts)
    | ExprKind.Function { Sig = s; Body = body } ->
      let ps = s.ParamList |> List.map (fun p -> buildPattern ctx p.Pattern)

      match body with
      | BlockOrExpr.Block block ->
        let body = buildBlock ctx block Finalizer.Empty
        let expr = Expr.Function { Id = None; Params = ps; Body = body }
        let stmts = []

        (expr, stmts)
      | BlockOrExpr.Expr expr ->
        let bodyExpr, bodyStmts = buildExpr ctx expr

        let body =
          bodyStmts @ [ Stmt.Return { Argument = Some bodyExpr; Loc = None } ]

        let body = { Body = body; Loc = None }
        let expr = Expr.ArrowFunction { Params = ps; Body = body }

        (expr, [])
    | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
      let tempId = $"temp{ctx.NextTempId}"
      ctx.NextTempId <- ctx.NextTempId + 1
      let finalizer = Finalizer.Assign tempId

      let tempDecl =
        { Declarations =
            [ { Id =
                  Pat.Ident
                    { Id = { Name = tempId; Loc = None }
                      TypeAnn = None
                      Loc = None }
                Init = None } ]
          Kind = VariableDeclarationKind.Var }

      let conditionExpr, conditionStmts = buildExpr ctx condition
      let thenBlock = buildBlock ctx thenBranch finalizer

      let alt =
        Option.map
          (fun elseBranch ->
            match elseBranch with
            | BlockOrExpr.Block block -> buildBlock ctx block finalizer
            | BlockOrExpr.Expr expr ->
              let expr, stmts = buildExpr ctx expr
              let finalizer = buildFinalizer ctx expr finalizer
              { Body = stmts @ finalizer; Loc = None })
          elseBranch

      let ifStmt =
        Stmt.If
          { Test = conditionExpr
            Consequent = Stmt.Block thenBlock
            Alternate = Option.map Stmt.Block alt
            Loc = None }

      let stmts =
        [ Stmt.Declaration(Declaration.Variable tempDecl) ]
        @ conditionStmts
        @ [ ifStmt ]

      let expr = Expr.Identifier { Name = tempId; Loc = None }

      (expr, stmts)
    | _ -> failwith (sprintf "TODO: buildExpr - %A" expr)

  let buildBlock
    (ctx: Ctx)
    (body: Block)
    (finalizer: Finalizer)
    : BlockStatement =
    // TODO: check if the last statement is an expression statement
    // and use the appropriate finalizer with it

    let mutable stmts: list<TS.Stmt> = []
    let lastStmt = body.Stmts |> List.last

    for stmt in body.Stmts do
      let stmts' =
        match stmt.Kind with
        | StmtKind.Expr expr ->
          let expr, stmts = buildExpr ctx expr

          if stmt = lastStmt then
            stmts @ buildFinalizer ctx expr finalizer
          else
            stmts
        | StmtKind.Decl decl ->
          match decl.Kind with
          | VarDecl(pattern, init, typeAnnOption) ->
            let pattern = buildPattern ctx pattern
            let initExpr, initStmts = buildExpr ctx init

            let decl =
              { Declarations = [ { Id = pattern; Init = Some initExpr } ]
                Kind = VariableDeclarationKind.Var }

            let declStmt = Stmt.Declaration(Declaration.Variable decl)

            initStmts @ [ declStmt ]
          | TypeDecl _ -> [] // Ignore types when generating JS code
        | StmtKind.Return expr -> failwith "TODO"
        | StmtKind.For(left, right, body) -> failwith "todo"

      stmts <- stmts @ stmts'

    { Body = stmts; Loc = None }

  let buildFinalizer
    (ctx: Ctx)
    (expr: TS.Expr)
    (finalizer: Finalizer)
    : list<TS.Stmt> =
    match finalizer with
    | Finalizer.Assign id ->
      let left = Expr.Identifier { Name = id; Loc = None }

      let assignStmt =
        Stmt.Expression
          { Expr =
              Expr.Assignment
                { Operator = AssignmentOperator.Assign
                  Left = left
                  Right = expr
                  Loc = None }
            Loc = None }

      [ assignStmt ]

    | Finalizer.Return -> [ Stmt.Return { Argument = None; Loc = None } ]
    | Finalizer.Empty -> []

  let buildPattern (ctx: Ctx) (pattern: Pattern) : TS.Pat =
    match pattern.Kind with
    | PatternKind.Identifier id ->
      Pat.Ident
        { Id = { Name = id.Name; Loc = None }
          TypeAnn = None
          Loc = None }
    | _ -> failwith "TODO"
