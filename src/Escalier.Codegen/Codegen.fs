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

  let buildExpr (ctx: Ctx) (expr: Expr) : (Expression * list<Statement>) =

    match expr.Kind with
    | ExprKind.Call { Callee = callee; Args = args } ->
      let calleeExpr, calleeStmts = buildExpr ctx callee
      let argExprs, argStmts = args |> List.map (buildExpr ctx) |> List.unzip

      let callExpr =
        Expression.Call
          { Callee = calleeExpr
            Arguments = argExprs
            Loc = None }

      (callExpr, calleeStmts @ (argStmts |> List.concat))
    | ExprKind.Identifier name ->
      Expression.Identifier { Name = name; Loc = None }, []
    | ExprKind.Literal lit ->
      let litVal =
        match lit with
        | Literal.Boolean b -> LiteralValue.Boolean b
        | Literal.String s -> LiteralValue.String s
        | Literal.Number n -> LiteralValue.Number(n |> float)
        | Literal.Null -> LiteralValue.Null
        | Literal.Undefined -> LiteralValue.Undefined

      Expression.Literal { Value = litVal; Loc = None }, []
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
          Expression.Binary
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
            [ { Id = Pattern.Identifier { Name = tempId; Loc = None }
                Init = None } ]
          Kind = VariableDeclarationKind.Var }

      let finalizer = Finalizer.Assign tempId
      let blockStmt = buildBlock ctx block finalizer
      let expr = Expression.Identifier { Name = tempId; Loc = None }

      let stmts =
        [ Statement.Declaration(Declaration.Variable tempDecl)
          Statement.Block blockStmt ]

      (expr, stmts)
    | ExprKind.Function { Sig = s; Body = body } ->
      let ps = s.ParamList |> List.map (fun p -> buildPattern ctx p.Pattern)

      match body with
      | BlockOrExpr.Block block ->
        let body = buildBlock ctx block Finalizer.Empty
        let expr = Expression.Function { Id = None; Params = ps; Body = body }
        let stmts = []

        (expr, stmts)
      | BlockOrExpr.Expr expr ->
        let (bodyExpr, bodyStmts) = buildExpr ctx expr

        let body =
          bodyStmts
          @ [ Statement.Return { Argument = Some bodyExpr; Loc = None } ]

        let body = { Body = body; Loc = None }
        let expr = Expression.ArrowFunction { Params = ps; Body = body }

        (expr, [])
    | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
      // TODO: figure out how to do chaining

      let tempId = $"temp{ctx.NextTempId}"
      ctx.NextTempId <- ctx.NextTempId + 1
      let finalizer = Finalizer.Assign tempId

      let tempDecl =
        { Declarations =
            [ { Id = Pattern.Identifier { Name = tempId; Loc = None }
                Init = None } ]
          Kind = VariableDeclarationKind.Var }

      let conditionExpr, conditionStmts = buildExpr ctx condition
      let thenBlock = buildBlock ctx thenBranch finalizer

      let alt =
        Option.map
          (fun elseBranch ->
            match elseBranch with
            | BlockOrExpr.Block block -> buildBlock ctx block finalizer
            | BlockOrExpr.Expr expr -> failwith "TODO: if-else chaining")
          elseBranch

      let ifStmt =
        Statement.If
          { Test = conditionExpr
            Consequent = Statement.Block thenBlock
            Alternate = Option.map Statement.Block alt
            Loc = None }

      let stmts =
        [ Statement.Declaration(Declaration.Variable tempDecl) ]
        @ conditionStmts
        @ [ ifStmt ]

      let expr = Expression.Identifier { Name = tempId; Loc = None }

      (expr, stmts)
    | _ -> failwith (sprintf "TODO: buildExpr - %A" expr)

  let buildBlock
    (ctx: Ctx)
    (body: Block)
    (finalizer: Finalizer)
    : BlockStatement =
    // TODO: check if the last statement is an expression statement
    // and use the appropriate finalizer with it

    let mutable stmts: list<Statement> = []
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

            let declStmt = Statement.Declaration(Declaration.Variable decl)

            initStmts @ [ declStmt ]
          | TypeDecl _ -> [] // Ignore types when generating JS code
        | StmtKind.Return expr -> failwith "TODO"
        | StmtKind.For(left, right, body) -> failwith "todo"

      stmts <- stmts @ stmts'

    { Body = stmts; Loc = None }

  let buildFinalizer
    (ctx: Ctx)
    (expr: Expression)
    (finalizer: Finalizer)
    : list<Statement> =
    match finalizer with
    | Finalizer.Assign id ->
      let left = Expression.Identifier { Name = id; Loc = None }

      let assignStmt =
        Statement.Expression
          { Expr =
              Expression.Assignment
                { Operator = AssignmentOperator.Assign
                  Left = left
                  Right = expr
                  Loc = None }
            Loc = None }

      [ assignStmt ]

    | Finalizer.Return -> [ Statement.Return { Argument = None; Loc = None } ]

  let buildPattern (ctx: Ctx) (pattern: Pattern) : TS.Pattern =
    match pattern.Kind with
    | PatternKind.Identifier id ->
      Pattern.Identifier { Name = id.Name; Loc = None }
    | _ -> failwith "TODO"
