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
    | ExprKind.Identifier name -> Expr.Ident { Name = name; Loc = None }, []
    | ExprKind.Literal lit ->
      let lit =
        match lit with
        | Literal.Boolean b -> Lit.Bool { Value = b; Loc = None } |> Expr.Lit
        | Literal.String s ->
          Lit.Str { Value = s; Raw = None; Loc = None } |> Expr.Lit
        | Literal.Number n ->
          Lit.Num
            { Value = n |> float
              Raw = None
              Loc = None }
          |> Expr.Lit
        | Literal.Null -> Lit.Null { Loc = None } |> Expr.Lit
        | Literal.Undefined ->
          { Ident.Name = "undefined"; Loc = None } |> Expr.Ident

      lit, []
    | ExprKind.Binary(op, left, right) ->
      let binaryOp =
        match op with
        | "+" -> Some(BinOp.Add)
        | "-" -> Some(BinOp.Sub)
        | "*" -> Some(BinOp.Mul)
        | "/" -> Some(BinOp.Div)
        | "==" -> Some(BinOp.EqEq)
        | "!=" -> Some(BinOp.NotEq)
        | "<" -> Some(BinOp.Lt)
        | "<=" -> Some(BinOp.LtEq)
        | ">" -> Some(BinOp.Gt)
        | ">=" -> Some(BinOp.GtEq)
        | _ -> None

      let leftExpr, leftStmts = buildExpr ctx left
      let rightExpr, rightStmts = buildExpr ctx right

      let binExpr =
        match binaryOp with
        | Some(op) ->
          Expr.Bin
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
      let expr = Expr.Ident { Name = tempId; Loc = None }

      let stmts = [ Stmt.Decl(Decl.Var tempDecl); Stmt.Block blockStmt ]

      (expr, stmts)
    | ExprKind.Function { Sig = s; Body = body } ->


      match body with
      | BlockOrExpr.Block block ->
        let ps =
          s.ParamList
          |> List.map (fun p ->
            let pat = buildPattern ctx p.Pattern
            { Pat = pat; Loc = None })

        let body = buildBlock ctx block Finalizer.Empty

        let func: TS.Function =
          { Params = ps
            Body = Some({ Body = body.Body; Loc = None })
            IsGenerator = false
            IsAsync = false
            TypeParams = None
            ReturnType = None
            Loc = None }

        let expr = Expr.Fn { Id = None; Fn = func }
        let stmts = []

        (expr, stmts)
      | BlockOrExpr.Expr expr ->
        let ps = s.ParamList |> List.map (fun p -> buildPattern ctx p.Pattern)
        let bodyExpr, bodyStmts = buildExpr ctx expr

        let body =
          bodyStmts @ [ Stmt.Return { Argument = Some bodyExpr; Loc = None } ]

        let body: BlockStmt = { Body = body; Loc = None }
        let expr = Expr.Arrow { Params = ps; Body = body }

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

      let stmts = [ Stmt.Decl(Decl.Var tempDecl) ] @ conditionStmts @ [ ifStmt ]

      let expr = Expr.Ident { Name = tempId; Loc = None }

      (expr, stmts)
    | _ -> failwith (sprintf "TODO: buildExpr - %A" expr)

  let buildBlock (ctx: Ctx) (body: Block) (finalizer: Finalizer) : BlockStmt =
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

            let declStmt = Stmt.Decl(Decl.Var decl)

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
      let left = Expr.Ident { Name = id; Loc = None }

      let assignStmt =
        Stmt.Expr
          { Expr =
              Expr.Assign
                { Operator = AssignOp.Assign
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
