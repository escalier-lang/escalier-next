namespace Escalier.TypeChecker

open FParsec

open Escalier.Data.Common
open Escalier.Data.Type

module TemplateLiteral =
  let number: Parser<Number, unit> =
    fun stream ->
      let intReply = many1Satisfy isDigit stream

      match intReply.Status with
      | Ok ->
        if stream.PeekString(2) = ".." then
          Reply(Number.Int(int intReply.Result))
        else if stream.PeekString(1) = "." then
          let index = stream.Index
          stream.Skip(1)
          let decReply = many1Satisfy isDigit stream

          match decReply.Status with
          | Ok ->
            let number = intReply.Result + "." + decReply.Result
            Reply(Number.Float(float number))
          | Error ->
            stream.Seek(index)
            Reply(Number.Int(int intReply.Result))
          | _ -> Reply(decReply.Status, decReply.Error)
        else
          Reply(Number.Int(int intReply.Result))
      | _ -> Reply(intReply.Status, intReply.Error)

  let check (s: string) (tl: TemplateLiteral<Type>) : bool =
    let { Parts = parts; Exprs = exprs } = tl

    let firstPart, restParts = List.head parts, List.tail parts
    let pairs = List.zip exprs restParts

    let mutable parser: Parser<unit, unit> = eof

    for expr, part in List.rev pairs do
      if part.Length > 0 then
        parser <- (pstring part |>> ignore) .>> parser

      match expr.Kind with
      | TypeKind.Primitive Primitive.Number ->
        parser <- (number |>> ignore) .>> parser
      | TypeKind.Primitive Primitive.Boolean ->
        parser <- ((pstring "true" <|> pstring "false") |>> ignore) .>> parser
      | TypeKind.Primitive Primitive.String ->
        // This produces the same behaviour as `*.?` would in a regex
        parser <- many (notFollowedBy parser >>. anyChar) |>> ignore

    if firstPart.Length > 0 then
      parser <- (pstring firstPart |>> ignore) .>> parser

    match run parser s with
    | Success((), _, pos) ->
      printfn "pos = %A" pos
      true
    | Failure _ -> false
