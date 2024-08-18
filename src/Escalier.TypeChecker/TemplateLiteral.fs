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

  let parserForType
    (parser: Parser<unit, unit>)
    (t: Type)
    : Parser<unit, unit> =

    let rec fold (t: Type) : Parser<unit, unit> =
      match t.Kind with
      | TypeKind.Literal lit ->
        match lit with
        | Literal.String s -> pstring s
        | Literal.Boolean b -> pstring (string b)
        | Literal.Number n -> pstring (string n)
        | _ -> failwith $"TODO: parserForType - lit = ${lit}"
        |>> ignore
      | TypeKind.Primitive Primitive.Number -> number |>> ignore
      | TypeKind.Primitive Primitive.Boolean ->
        (pstring "true" <|> pstring "false") |>> ignore
      | TypeKind.Primitive Primitive.String ->
        // This produces the same behaviour as `*.?` would in a regex
        many (notFollowedBy parser >>. anyChar) |>> ignore
      | TypeKind.Union elems -> choice (List.map fold elems)
      | TypeKind.IntrinsicInstance { Name = name; TypeArgs = typeArgs } ->
        failwith "TODO: parserForType - IntrinsicInstance"

      | _ -> failwith $"TODO: parserForType - t = {t}"

    fold t .>> parser

  let check (s: string) (tl: TemplateLiteral<Type>) : bool =
    let { Parts = parts; Exprs = exprs } = tl

    let firstPart, restParts = List.head parts, List.tail parts
    let pairs = List.zip exprs restParts

    let mutable parser: Parser<unit, unit> = eof

    for expr, part in List.rev pairs do
      if part.Length > 0 then
        parser <- (pstring part |>> ignore) .>> parser

      parser <- parserForType parser expr
    // match expr.Kind with
    // | TypeKind.Primitive Primitive.Number ->
    //   parser <- (number |>> ignore) .>> parser
    // | TypeKind.Primitive Primitive.Boolean ->
    //   parser <- ((pstring "true" <|> pstring "false") |>> ignore) .>> parser
    // | TypeKind.Primitive Primitive.String ->
    //   // This produces the same behaviour as `*.?` would in a regex
    //   parser <- many (notFollowedBy parser >>. anyChar) |>> ignore
    // | TypeKind.Union elems ->
    //   printfn $"union = ${expr}"
    //   failwith "TODO: generate parser for union of types"

    if firstPart.Length > 0 then
      parser <- (pstring firstPart |>> ignore) .>> parser

    match run parser s with
    | Success((), _, pos) -> true
    | Failure _ -> false

  let isUppercase (s: string) : bool = s |> Seq.forall isUpper
  let isLowercase (s: string) : bool = s |> Seq.forall isLower

  let isCapitalize (s: string) : bool =
    match s with
    | "" -> true
    | _ -> isUpper s.[0]

  let isUncapitalize (s: string) : bool =
    match s with
    | "" -> true
    | _ -> isLower s.[0]
