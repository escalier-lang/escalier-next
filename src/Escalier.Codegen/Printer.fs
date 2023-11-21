namespace Escalier.Codegen

open System.Globalization

open Escalier.Codegen.TypeScript

module Printer =
  let ci = CultureInfo("en-US", true)

  let getBinaryPrecedence (op: BinaryOperator) : int =
    match op with
    | BinaryOperator.BitwiseOr -> 6
    | BinaryOperator.BitwiseXor -> 7
    | BinaryOperator.BitwiseAnd -> 8
    | BinaryOperator.Equal -> 9
    | BinaryOperator.NotEqual -> 9
    | BinaryOperator.StrictEqual -> 9
    | BinaryOperator.StrictNotEqual -> 9
    | BinaryOperator.LessThan -> 10
    | BinaryOperator.LessThanOrEqual -> 10
    | BinaryOperator.GreaterThan -> 10
    | BinaryOperator.GreaterThanOrEqual -> 10
    | BinaryOperator.In -> 10
    | BinaryOperator.InstanceOf -> 10
    | BinaryOperator.LeftShift -> 11
    | BinaryOperator.RightShift -> 11
    | BinaryOperator.UnsignedRightShift -> 11
    | BinaryOperator.Plus -> 12
    | BinaryOperator.Minus -> 12
    | BinaryOperator.Multiply -> 13
    | BinaryOperator.Divide -> 13
    | BinaryOperator.Modulo -> 13

  let getLogicalPrecendence (op: LogicalOperator) : int =
    match op with
    | LogicalOperator.And -> 3
    | LogicalOperator.Or -> 2

  let printLit (lit: Literal) : string =
    match lit.Value with
    | LiteralValue.String s -> s
    | LiteralValue.Number f -> f.ToString(ci)
    | LiteralValue.Boolean b -> if b then "true" else "false"
    | LiteralValue.Regex { Pattern = pat; Flags = flags } -> $"/{pat}/{flags}"
    | LiteralValue.Null -> "null"
    | LiteralValue.Undefined -> "undefined"

  let rec printExpr (prec: int) (e: Expression) : string =

    match e with
    | Expression.Identifier { Name = name } -> name
    | Expression.Literal lit -> printLit lit
    | Expression.This _ -> "this"
    | Expression.Array { Elements = elements } ->
      let elements =
        List.map
          (fun e ->
            match e with
            | Some(e) -> printExpr prec e
            | None -> "")
          elements

        |> String.concat ", "

      $"[{elements}]"
    // TODO: support shorthand syntax
    | Expression.Object { Properties = props } ->

      let props =
        props
        |> List.map
          (fun
               { Key = key
                 Value = value
                 Kind = _kind } ->

            let key =
              match key with
              | PropertyKey.Literal lit -> printLit lit // wrap this?
              | PropertyKey.Identifier id -> id.Name

            let value = printExpr prec value

            // TODO: handle getter/setter kinds
            $"{key}: {value}")
        |> String.concat ", "

      $"{{{props}}}"
    | Expression.Function { Id = id; Params = ps; Body = body } ->
      let id =
        match id with
        | Some(id) -> id.Name
        | None -> ""

      let ps = ps |> List.map printPattern |> String.concat ", "
      let body = body.Body |> List.map printStmt |> String.concat "\n"

      $"function {id}({ps}) {{\n{body}\n}}"
    | Expression.Unary { Operator = op
                         Prefix = _prefix
                         Argument = arg } ->

      let outerPrec = prec
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

      let arg = printExpr prec arg
      let expr = $"{op}{arg}"

      if innerPrec < outerPrec then $"({expr})" else expr

    | Expression.Update { Operator = op
                          Argument = arg
                          Prefix = prefix } ->

      let outerPrec = prec
      let innerPrec = if prefix then 15 else 16

      let op =
        match op with
        | Decrement -> "--"
        | Increment -> "++"

      let arg = printExpr innerPrec arg
      let expr = if prefix then $"{op}{arg}" else $"{arg}{op}"

      if innerPrec < outerPrec then $"({expr})" else expr
    | Expression.Binary { Operator = op
                          Left = left
                          Right = right } ->

      let outerPrec = prec
      let innerPrec = getBinaryPrecedence op

      let op =
        match op with
        | Equal -> "=="
        | NotEqual -> "!="
        | StrictEqual -> "==="
        | StrictNotEqual -> "!=="
        | LessThan -> "<"
        | LessThanOrEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanOrEqual -> ">="
        | LeftShift -> "<<"
        | RightShift -> ">>"
        | UnsignedRightShift -> ">>>"
        | Plus -> "+"
        | Minus -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulo -> "%"
        | BitwiseAnd -> "&"
        | BitwiseOr -> "|"
        | BitwiseXor -> "^"
        | In -> "in"
        | InstanceOf -> "instanceof"

      let left = printExpr innerPrec left
      let right = printExpr innerPrec right
      let expr = $"{left} {op} {right}"

      if innerPrec < outerPrec then $"({expr})" else expr

    | Expression.Assignment { Operator = op
                              Left = left
                              Right = right } ->

      let outerPrec = prec
      let innerPrec = 2

      let op =
        match op with
        | AssignmentOperator.Assign -> "="
        | AssignmentOperator.PlusAssign -> "+="
        | AssignmentOperator.MinusAssign -> "-="
        | AssignmentOperator.MultiplyAssign -> "*="
        | AssignmentOperator.DivideAssign -> "/="
        | AssignmentOperator.ModuloAssign -> "%="
        | AssignmentOperator.LeftShiftAssign -> "<<="
        | AssignmentOperator.RightShiftAssign -> ">>="
        | AssignmentOperator.UnsignedRightShiftAssign -> ">>>="
        | AssignmentOperator.BitwiseAndAssign -> "&="
        | AssignmentOperator.BitwiseOrAssign -> "|="
        | AssignmentOperator.BitwiseXorAssign -> "^="

      let left = printExpr prec left
      let right = printExpr prec right
      let expr = $"{left} {op} {right}"

      if innerPrec < outerPrec then $"({expr})" else expr
    | Expression.Logical { Operator = op
                           Left = left
                           Right = right } ->
      let outerPrec = prec
      let innerPrec = getLogicalPrecendence op

      let op =
        match op with
        | And -> "&&"
        | Or -> "||"

      let left = printExpr innerPrec left
      let right = printExpr innerPrec right
      let expr = $"{left} {op} {right}"

      if innerPrec < outerPrec then $"({expr})" else expr
    | Expression.Member { Object = obj
                          Property = prop
                          Computed = computed } ->

      let outerPrec = prec
      let innerPrec = 18

      let obj = printExpr innerPrec obj
      let prop = printExpr innerPrec prop
      let expr = if computed then $"{obj}[{prop}]" else $"{obj}.{prop}"

      if innerPrec < outerPrec then $"({expr})" else expr
    | Expression.Conditional { Test = test
                               Consequent = cons
                               Alternate = alt } ->

      let outerPrec = prec
      let innerPrec = 3

      let test = printExpr innerPrec test
      let cons = printExpr innerPrec cons
      let alt = printExpr innerPrec alt

      let expr = $"{test} ? {cons} : {alt}"

      if innerPrec < outerPrec then $"({expr}" else expr

    | Expression.Call { Callee = callee; Arguments = args } ->
      let outerPrec = prec
      let innerPrec = 18

      let callee = printExpr innerPrec callee
      let args = args |> List.map (printExpr innerPrec) |> String.concat ", "
      let expr = $"{callee}({args})"

      if innerPrec < outerPrec then $"({expr}" else expr
    | Expression.New { Callee = callee; Arguments = args } ->
      let outerPrec = prec
      let innerPrec = 18

      let callee = printExpr innerPrec callee
      let args = args |> List.map (printExpr innerPrec) |> String.concat ", "
      let expr = $"{callee}({args})"

      if innerPrec < outerPrec then $"new ({expr}" else expr
    | Expression.Sequence { Expressions = exprs } ->

      let outerPrec = prec
      let innerPrec = 1

      let exprs = exprs |> List.map (printExpr innerPrec) |> String.concat ", "

      if innerPrec < outerPrec then $"({exprs})" else exprs

  and printStmt (stmt: Statement) : string =

    match stmt with
    | Statement.Block { Body = body } ->
      let body = body |> List.map printStmt |> String.concat "\n"
      $"{{\n{body}\n}}"
    | Statement.Expression { Expr = expr } -> $"{printExpr 0 expr};"
    | Statement.Empty _ -> ";"
    | Statement.Debugger _ -> "debugger;"
    | Statement.Return { Argument = arg } ->
      match arg with
      | Some(arg) -> $"return {printExpr 0 arg};"
      | None -> "return;"
    | Statement.Labeled { Label = label; Body = body } ->
      let label = label.Name
      let body = printStmt body
      $"{label}: {body}"
    | Statement.Break { Label = label } ->
      match label with
      | Some(label) -> $"break {label.Name};"
      | None -> "break;"
    | Statement.Continue { Label = label } ->
      match label with
      | Some(label) -> $"continue {label.Name};"
      | None -> "continue;"
    | Statement.If { Test = test
                     Consequent = cons
                     Alternate = alt } ->

      let test = printExpr 0 test
      let cons = printStmt cons

      match alt with
      | Some(alt) -> $"if ({test}) {cons} else {printStmt alt}"
      | None -> $"if ({test}) {cons}"
    | Statement.Switch { Discriminant = disc; Cases = cases } ->
      let disc = printExpr 0 disc

      let cases =
        cases
        |> List.map (fun case ->
          let test =
            match case.Test with
            | Some(test) -> $"case {printExpr 0 test}:"
            | None -> "default:"

          let cons =
            match case.Consequent with
            | [ stmt ] -> printStmt stmt
            | stmts -> stmts |> List.map printStmt |> String.concat "\n"

          $"{test} {cons}")
        |> String.concat "\n"

      $"switch ({disc}) {{\n{cases}\n}}"
    | Statement.Throw { Argument = arg } ->
      let arg = printExpr 0 arg
      $"throw {arg};"
    | Statement.Try { Block = block
                      Handler = handler
                      Finalizer = finalizer } ->
      let block = block.Body |> List.map printStmt |> String.concat "\n"
      let block = $"try {{\n{block}\n}}"

      let handler =
        match handler with
        | Some(handler) ->
          let param = printPattern handler.Param

          let body =
            handler.Body.Body |> List.map printStmt |> String.concat "\n"

          $"catch ({param}) {{\n{body}\n}}"
        | None -> ""

      let finalizer =
        match finalizer with
        | Some(finalizer) ->
          let body = finalizer.Body |> List.map printStmt |> String.concat "\n"

          $"finally {{\n{body}\n}}"
        | None -> ""

      $"{block}{handler}{finalizer}"
    | Statement.While { Test = test; Body = body } ->
      let test = printExpr 0 test
      let body = printStmt body

      $"while ({test}) {body}"
    | Statement.DoWhile { Test = test; Body = body } ->
      let test = printExpr 0 test
      let body = printStmt body

      $"do {body} while ({test});"
    | Statement.For { Init = init
                      Test = test
                      Update = update
                      Body = body } ->

      let init =
        match init with
        | Some(ForInit.Variable decl) ->
          printStmt (Statement.Declaration(Declaration.Variable decl))
        | Some(ForInit.Expression expr) -> printExpr 0 expr
        | None -> ""

      let test =
        match test with
        | Some(test) -> printExpr 0 test
        | None -> ""

      let update =
        match update with
        | Some(update) -> printExpr 0 update
        | None -> ""

      let body = printStmt body

      $"for ({init}; {test}; {update}) {body}"
    | Statement.ForIn { Left = left
                        Right = right
                        Body = body } ->
      let left =
        match left with
        | ForInLeft.Variable decl ->
          printStmt (Statement.Declaration(Declaration.Variable decl))
        | ForInLeft.Pattern p -> printPattern p

      let right = printExpr 0 right
      let body = printStmt body

      $"for ({left} in {right}) {body}"
    | Statement.Declaration decl ->
      match decl with
      | Declaration.Function { Id = id; Params = ps; Body = body } ->
        let id = id.Name
        let ps = ps |> List.map printPattern |> String.concat ", "
        let body = body.Body |> List.map printStmt |> String.concat "\n"

        $"function {id}({ps}) {{\n{body}\n}}"
      | Declaration.Variable { Declarations = decls; Kind = kind } ->
        let decls =
          List.map
            (fun { Id = id; Init = init } ->
              let id = printPattern id

              match init with
              | Some(init) -> $"{id} = {printExpr 0 init}"
              | None -> id)
            decls
          |> String.concat ", "

        match kind with
        | VariableDeclarationKind.Var -> $"var {decls};"
        | VariableDeclarationKind.Let -> $"let {decls};"
        | VariableDeclarationKind.Const -> $"const {decls};"

  and printPattern (p: Pattern) : string =

    match p with
    | Pattern.Identifier { Name = name } -> name
    | Pattern.Member { Object = obj
                       Property = prop
                       Computed = computed } ->

      let prec = 18

      let obj = printExpr prec obj
      let prop = printExpr prec prop
      if computed then $"{obj}[{prop}]" else $"{obj}.{prop}"
