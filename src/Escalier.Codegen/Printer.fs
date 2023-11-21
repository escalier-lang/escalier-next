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
                               Alternate = alt
                               Consequent = cons } ->

      let outerPrec = prec
      let innerPrec = 3

      let test = printExpr innerPrec test
      let alt = printExpr innerPrec alt
      let cons = printExpr innerPrec cons

      let expr = $"{test} ? {alt} : {cons}"

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
