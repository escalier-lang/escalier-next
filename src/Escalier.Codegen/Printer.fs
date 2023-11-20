namespace Escalier.Codegen

open System.Text
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

  let getUnaryPrecedence (op: UnaryOperator) : int =
    match op with
    | UnaryOperator.Not -> 15
    | UnaryOperator.BitwiseNot -> 15
    | UnaryOperator.Plus -> 15
    | UnaryOperator.Minus -> 15
    | UnaryOperator.Typeof -> 15
    | UnaryOperator.Void -> 15
    | UnaryOperator.Delete -> 15
    | UnaryOperator.Await -> 15

  let rec printExpr
    (sb: StringBuilder)
    (prec: int)
    (e: Expression)
    : StringBuilder =

    match e with
    | Expression.Identifier { Name = name } -> sb.Append(name)
    | Expression.Literal { Value = value } ->
      match value with
      | LiteralValue.String s -> sb.Append(s)
      | LiteralValue.Number f -> sb.Append(f.ToString(ci))
      | LiteralValue.Boolean b -> sb.Append(b)
      | LiteralValue.Regex regex -> failwith "TODO - regex"
      | LiteralValue.Null -> sb.Append("null")
      | LiteralValue.Undefined -> sb.Append("undefined")

    | Expression.This _ -> sb.Append("this")
    | Expression.Array { Elements = elements } ->
      sb.Append("[") |> ignore

      List.iteri
        (fun i e ->
          sb.Append(if i > 0 then ", " else "") |> ignore

          match e with
          | Some(e) -> printExpr sb prec e
          | None -> sb.Append("")
          |> ignore)
        elements

      sb.Append("]")

    | Expression.Object objectExpression -> failwith "todo"
    | Expression.Unary { Operator = op
                         Prefix = prefix
                         Argument = args } ->
      let _ =
        match op with
        | UnaryOperator.Minus -> sb.Append("-")
        | UnaryOperator.Plus -> sb.Append("+")
        | UnaryOperator.Not -> sb.Append("!")
        | UnaryOperator.BitwiseNot -> sb.Append("~")
        | UnaryOperator.Typeof -> sb.Append("typeof ")
        | UnaryOperator.Void -> sb.Append("void ")
        | UnaryOperator.Delete -> sb.Append("delete ")

      let prec = getUnaryPrecedence op

      printExpr sb prec args
    | Expression.Update { Operator = op
                          Argument = arg
                          Prefix = prefix } ->
      if prefix then
        printExpr sb prec arg |> ignore

      let _ =
        match op with
        | Decrement -> sb.Append("--")
        | Increment -> sb.Append("++")

      if not prefix then
        printExpr sb prec arg |> ignore

      sb
    | Expression.Binary { Operator = op
                          Left = left
                          Right = right } ->

      let outerPrec = prec
      let innerprec = getBinaryPrecedence op

      if innerprec < outerPrec then
        sb.Append("(") |> ignore

      printExpr sb innerprec left |> ignore

      let _ =
        match op with
        | Equal -> sb.Append(" == ")
        | NotEqual -> sb.Append(" != ")
        | StrictEqual -> sb.Append(" === ")
        | StrictNotEqual -> sb.Append(" !== ")
        | LessThan -> sb.Append(" < ")
        | LessThanOrEqual -> sb.Append(" <= ")
        | GreaterThan -> sb.Append(" > ")
        | GreaterThanOrEqual -> sb.Append(" >= ")
        | LeftShift -> sb.Append(" << ")
        | RightShift -> sb.Append(" >> ")
        | UnsignedRightShift -> sb.Append(" >>> ")
        | Plus -> sb.Append(" + ")
        | Minus -> sb.Append(" - ")
        | Multiply -> sb.Append(" * ")
        | Divide -> sb.Append(" / ")
        | Modulo -> sb.Append(" % ")
        | BitwiseAnd -> sb.Append(" & ")
        | BitwiseOr -> sb.Append(" | ")
        | BitwiseXor -> sb.Append(" ^ ")
        | In -> sb.Append(" in ")
        | InstanceOf -> sb.Append(" instanceof ")

      printExpr sb innerprec right |> ignore

      if innerprec < outerPrec then
        sb.Append(")") |> ignore

      sb

    | Expression.Assignment assignmentExpression -> failwith "todo"
    | Expression.Logical logicalExpression -> failwith "todo"
    | Expression.Member memberExpression -> failwith "todo"
    | Expression.Conditional conditionalExpression -> failwith "todo"
    | Expression.Call callExpression -> failwith "todo"
    | Expression.New newExpression -> failwith "todo"
    | Expression.Sequence sequenceExpression -> failwith "todo"
