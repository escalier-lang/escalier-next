namespace Escalier.Codegen

open System.Globalization

open Escalier.Codegen.TypeScript

module Printer =
  let ci = CultureInfo("en-US", true)

  type PrintCtx = { Precedence: int; Indent: int }

  let getBinaryPrecedence (op: BinOp) : int =
    match op with
    | BinOp.BitOr -> 6
    | BinOp.BitXor -> 7
    | BinOp.BitAnd -> 8
    | BinOp.EqEq -> 9
    | BinOp.NotEq -> 9
    | BinOp.EqEqEq -> 9
    | BinOp.NotEqEq -> 9
    | BinOp.Lt -> 10
    | BinOp.LtEq -> 10
    | BinOp.Gt -> 10
    | BinOp.GtEq -> 10
    | BinOp.In -> 10
    | BinOp.InstanceOf -> 10
    | BinOp.LShift -> 11
    | BinOp.RShift -> 11
    | BinOp.ZeroFillRShift -> 11
    | BinOp.Add -> 12
    | BinOp.Sub -> 12
    | BinOp.Mul -> 13
    | BinOp.Div -> 13
    | BinOp.Mod -> 13
    | BinOp.LogicalOr -> 2
    | BinOp.LogicalAnd -> 3
    | BinOp.Exp -> 14
    | BinOp.NullishCoalescing -> 2

  let printLit (lit: Lit) : string =
    match lit with
    | Lit.Str { Value = value } -> value
    | Lit.Num { Value = value } -> value.ToString(ci)
    | Lit.Bool { Value = value } -> if value then "true" else "false"
    | Lit.Regex { Exp = exp; Flags = flags } -> $"/{exp}/{flags}"
    | Lit.Null _ -> "null"
    | Lit.JSXText jsxText -> failwith "todo"

  let rec printExpr (ctx: PrintCtx) (e: Expr) : string =

    match e with
    | Expr.Ident { Name = name } -> name
    | Expr.Lit lit -> printLit lit
    | Expr.This _ -> "this"
    | Expr.Array { Elements = elements } ->
      let elements =
        List.map
          (fun (e: option<ExprOrSpread>) ->
            match e with
            | Some(e) -> printExpr { ctx with Precedence = 0 } e.Expr
            | None -> "")
          elements

        |> String.concat ", "

      $"[{elements}]"
    // TODO: support shorthand syntax
    | Expr.Object { Properties = props } ->

      let props =
        props
        |> List.map
          (fun
               { Key = key
                 Value = value
                 Kind = _kind } ->

            let key =
              match key with
              | PropertyKey.Lit lit -> printLit lit // wrap this?
              | PropertyKey.Ident id -> id.Name

            let value = printExpr { ctx with Precedence = 0 } value

            // TODO: handle getter/setter kinds
            $"{key}: {value}")
        |> String.concat ", "

      $"{{{props}}}"
    | Expr.Fn { Id = id
                Fn = { Params = ps; Body = body } } ->
      let id =
        match id with
        | Some(id) -> id.Name
        | None -> ""

      let ctx = { ctx with Precedence = 0 }

      let ps =
        ps |> List.map (fun p -> printPattern ctx p.Pat) |> String.concat ", "

      match body with
      | Some(body) -> $"function {id}({ps}) {printBlock ctx body}"
      | None -> $"function {id}({ps}) {{}}"
    | Expr.Arrow { Params = ps; Body = body } ->
      let ctx = { ctx with Precedence = 0 }
      let ps = ps |> List.map (printPattern ctx) |> String.concat ", "

      $"({ps}) => {printBlock ctx body}"
    | Expr.Unary { Operator = op
                   Prefix = _prefix
                   Argument = arg } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 15

      let op =
        match op with
        | UnaryOperator.Minus -> "-"
        | UnaryOperator.Plus -> "+"
        | UnaryOperator.Not -> "!"
        | UnaryOperator.BitwiseNot -> "~"
        | UnaryOperator.Typeof -> "typeof "
        | UnaryOperator.Void -> "void "
        | UnaryOperator.Delete -> "delete "
        | UnaryOperator.Await -> "await "

      let arg = printExpr { ctx with Precedence = innerPrec } arg
      let expr = $"{op}{arg}"

      if innerPrec < outerPrec then $"({expr})" else expr

    | Expr.Update { Operator = op
                    Argument = arg
                    Prefix = prefix } ->

      let outerPrec = ctx.Precedence
      let innerPrec = if prefix then 15 else 16

      let op =
        match op with
        | Decrement -> "--"
        | Increment -> "++"

      let arg = printExpr { ctx with Precedence = innerPrec } arg
      let expr = if prefix then $"{op}{arg}" else $"{arg}{op}"

      if innerPrec < outerPrec then $"({expr})" else expr
    | Expr.Bin { Operator = op
                 Left = left
                 Right = right } ->

      let outerPrec = ctx.Precedence
      let innerPrec = getBinaryPrecedence op

      let op =
        match op with
        | BinOp.EqEq -> "=="
        | BinOp.NotEq -> "!="
        | BinOp.EqEqEq -> "==="
        | BinOp.NotEqEq -> "!=="
        | BinOp.Lt -> "<"
        | BinOp.LtEq -> "<="
        | BinOp.Gt -> ">"
        | BinOp.GtEq -> ">="
        | BinOp.LShift -> "<<"
        | BinOp.RShift -> ">>"
        | BinOp.ZeroFillRShift -> ">>>"
        | BinOp.Add -> "+"
        | BinOp.Sub -> "-"
        | BinOp.Mul -> "*"
        | BinOp.Div -> "/"
        | BinOp.Mod -> "%"
        | BinOp.BitAnd -> "&"
        | BinOp.BitOr -> "|"
        | BinOp.BitXor -> "^"
        | BinOp.In -> "in"
        | BinOp.InstanceOf -> "instanceof"
        | BinOp.LogicalOr -> "||"
        | BinOp.LogicalAnd -> "&&"
        | BinOp.Exp -> "**"
        | BinOp.NullishCoalescing -> "??"

      let left = printExpr { ctx with Precedence = innerPrec } left
      let right = printExpr { ctx with Precedence = innerPrec } right
      let expr = $"{left} {op} {right}"

      if innerPrec < outerPrec then $"({expr})" else expr

    | Expr.Assign { Operator = op
                    Left = left
                    Right = right } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 2

      let op =
        match op with
        | AssignOp.Assign -> "="
        | AssignOp.PlusAssign -> "+="
        | AssignOp.MinusAssign -> "-="
        | AssignOp.MultiplyAssign -> "*="
        | AssignOp.DivideAssign -> "/="
        | AssignOp.ModuloAssign -> "%="
        | AssignOp.LeftShiftAssign -> "<<="
        | AssignOp.RightShiftAssign -> ">>="
        | AssignOp.UnsignedRightShiftAssign -> ">>>="
        | AssignOp.BitwiseAndAssign -> "&="
        | AssignOp.BitwiseOrAssign -> "|="
        | AssignOp.BitwiseXorAssign -> "^="

      let left = printExpr { ctx with Precedence = innerPrec } left
      let right = printExpr { ctx with Precedence = innerPrec } right
      let expr = $"{left} {op} {right}"

      if innerPrec < outerPrec then $"({expr})" else expr

    | Expr.Member { Object = obj
                    Property = prop
                    Computed = computed } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 18

      let obj = printExpr { ctx with Precedence = innerPrec } obj
      let prop = printExpr { ctx with Precedence = innerPrec } prop
      let expr = if computed then $"{obj}[{prop}]" else $"{obj}.{prop}"

      if innerPrec < outerPrec then $"({expr})" else expr
    | Expr.Cond { Test = test
                  Consequent = cons
                  Alternate = alt } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 3

      let test = printExpr { ctx with Precedence = innerPrec } test
      let cons = printExpr { ctx with Precedence = innerPrec } cons
      let alt = printExpr { ctx with Precedence = innerPrec } alt

      let expr = $"{test} ? {cons} : {alt}"

      if innerPrec < outerPrec then $"({expr}" else expr

    | Expr.Call { Callee = callee; Arguments = args } ->
      let outerPrec = ctx.Precedence
      let innerPrec = 18

      let callee = printExpr { ctx with Precedence = innerPrec } callee

      let args =
        args
        |> List.map (printExpr { ctx with Precedence = innerPrec })
        |> String.concat ", "

      let expr = $"{callee}({args})"

      if innerPrec < outerPrec then $"({expr}" else expr
    | Expr.New { Callee = callee; Arguments = args } ->
      let outerPrec = ctx.Precedence
      let innerPrec = 18

      let callee = printExpr { ctx with Precedence = innerPrec } callee

      let args =
        args
        |> List.map (printExpr { ctx with Precedence = innerPrec })
        |> String.concat ", "

      let expr = $"{callee}({args})"

      if innerPrec < outerPrec then $"new ({expr}" else expr
    | Expr.Seq { Exprs = exprs } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 1

      let exprs =
        exprs
        |> List.map (printExpr { ctx with Precedence = innerPrec })
        |> String.concat ", "

      if innerPrec < outerPrec then $"({exprs})" else exprs

  and printStmt (ctx: PrintCtx) (stmt: Stmt) : string =
    match stmt with
    | Stmt.Block block -> printBlock ctx block
    // let oldIdent = String.replicate ctx.Indent " "
    //
    // let ctx = { ctx with Indent = ctx.Indent + 2 }
    // let ident = String.replicate ctx.Indent " "
    //
    // let body =
    //   body
    //   |> List.map (fun stmt -> ident + printStmt ctx stmt)
    //   |> String.concat "\n"
    //
    // $"{{\n{body}\n{oldIdent}}}"
    | Stmt.Expr { Expr = expr } ->
      let ctx = { ctx with Precedence = 0 }
      $"{printExpr ctx expr};"
    | Stmt.Empty _ -> ";"
    | Stmt.Debugger _ -> "debugger;"
    | Stmt.Return { Argument = arg } ->
      let ctx = { ctx with Precedence = 0 }

      match arg with
      | Some(arg) -> $"return {printExpr ctx arg};"
      | None -> "return;"
    | Stmt.Labeled { Label = label; Body = body } ->
      let label = label.Name
      let body = printStmt ctx body
      $"{label}: {body}"
    | Stmt.Break { Label = label } ->
      match label with
      | Some(label) -> $"break {label.Name};"
      | None -> "break;"
    | Stmt.Continue { Label = label } ->
      match label with
      | Some(label) -> $"continue {label.Name};"
      | None -> "continue;"
    | Stmt.If { Test = test
                Consequent = cons
                Alternate = alt } ->
      let ctx = { ctx with Precedence = 0 }
      let test = printExpr ctx test
      let cons = printStmt ctx cons

      match alt with
      | Some(alt) -> $"if ({test}) {cons} else {printStmt ctx alt}"
      | None -> $"if ({test}) {cons}"
    | Stmt.Switch { Discriminant = disc; Cases = cases } ->
      let ctx = { ctx with Precedence = 0 }
      let disc = printExpr ctx disc

      let cases =
        cases
        |> List.map (fun case ->
          let test =
            match case.Test with
            | Some(test) -> $"case {printExpr ctx test}:"
            | None -> "default:"

          let cons =
            match case.Consequent with
            | [ stmt ] -> printStmt ctx stmt
            | stmts -> stmts |> List.map (printStmt ctx) |> String.concat "\n"

          $"{test} {cons}")
        |> String.concat "\n"

      $"switch ({disc}) {{\n{cases}\n}}"
    | Stmt.Throw { Argument = arg } ->
      let ctx = { ctx with Precedence = 0 }
      let arg = printExpr ctx arg
      $"throw {arg};"
    | Stmt.Try { Block = block
                 Handler = handler
                 Finalizer = finalizer } ->
      let ctx = { ctx with Precedence = 0 }

      let block = block.Body |> List.map (printStmt ctx) |> String.concat "\n"
      let block = $"try {{\n{block}\n}}"

      let handler =
        match handler with
        | Some(handler) ->
          let param = printPattern ctx handler.Param

          let body =
            handler.Body.Body |> List.map (printStmt ctx) |> String.concat "\n"

          $"catch ({param}) {{\n{body}\n}}"
        | None -> ""

      let finalizer =
        match finalizer with
        | Some(finalizer) ->
          let body =
            finalizer.Body |> List.map (printStmt ctx) |> String.concat "\n"

          $"finally {{\n{body}\n}}"
        | None -> ""

      $"{block}{handler}{finalizer}"
    | Stmt.While { Test = test; Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      let test = printExpr ctx test
      let body = printStmt ctx body

      $"while ({test}) {body}"
    | Stmt.DoWhile { Test = test; Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      let test = printExpr ctx test
      let body = printStmt ctx body

      $"do {body} while ({test});"
    | Stmt.For { Init = init
                 Test = test
                 Update = update
                 Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      let init =
        match init with
        | Some(ForInit.Variable decl) ->
          printStmt ctx (Stmt.Decl(Decl.Var decl))
        | Some(ForInit.Expr expr) -> printExpr ctx expr
        | None -> ""

      let test =
        match test with
        | Some(test) -> printExpr ctx test
        | None -> ""

      let update =
        match update with
        | Some(update) -> printExpr ctx update
        | None -> ""

      let body = printStmt ctx body

      $"for ({init}; {test}; {update}) {body}"
    | Stmt.ForIn { Left = left
                   Right = right
                   Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      let left =
        match left with
        | ForHead.VarDecl decl -> printStmt ctx (Stmt.Decl(Decl.Var decl))
        | ForHead.Pat p -> printPattern ctx p
        | ForHead.UsingDecl decl -> failwith "TODO"

      let right = printExpr ctx right
      let body = printStmt ctx body

      $"for ({left} in {right}) {body}"
    | Stmt.Decl decl ->
      let ctx = { ctx with Precedence = 0 }

      match decl with
      | Decl.Fn { Id = id
                  Fn = { Params = ps; Body = body } } ->
        let id = id.Name

        let ps =
          ps |> List.map (fun p -> printPattern ctx p.Pat) |> String.concat ", "

        match body with
        | Some(body) ->
          let body = body.Body |> List.map (printStmt ctx) |> String.concat "\n"
          $"function {id}({ps}) {{\n{body}\n}}"
        | None -> $"function {id}({ps}) {{}}"
      | Decl.Var { Declarations = decls; Kind = kind } ->
        let decls =
          List.map
            (fun { Id = id; Init = init } ->
              let id = printPattern ctx id

              match init with
              | Some(init) -> $"{id} = {printExpr ctx init}"
              | None -> id)
            decls
          |> String.concat ", "

        match kind with
        | VariableDeclarationKind.Var -> $"var {decls};"
        | VariableDeclarationKind.Let -> $"let {decls};"
        | VariableDeclarationKind.Const -> $"const {decls};"

  and printPattern (ctx: PrintCtx) (p: Pat) : string =

    match p with
    | Pat.Ident { Id = id } -> id.Name
    | _ -> failwith "TODO"
  // | Pat.Member { Object = obj
  //                Property = prop
  //                Computed = computed } ->
  //
  //   let ctx = { ctx with Precedence = 18 }
  //
  //   let obj = printExpr ctx obj
  //   let prop = printExpr ctx prop
  //   if computed then $"{obj}[{prop}]" else $"{obj}.{prop}"

  and printBlock (ctx: PrintCtx) (block: BlockStmt) =
    let oldIdent = String.replicate ctx.Indent " "

    let ctx = { ctx with Indent = ctx.Indent + 2 }
    let ident = String.replicate ctx.Indent " "

    let body =
      block.Body
      |> List.map (fun stmt -> ident + printStmt ctx stmt)
      |> String.concat "\n"

    $"{{\n{body}\n{oldIdent}}}"
