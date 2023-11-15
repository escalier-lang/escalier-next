namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax
open Shared

module private Patterns =
  let lit = ParserRefs.lit
  let expr = ParserRefs.expr
  let stmt = ParserRefs.stmt
  let typeAnn = ParserRefs.typeAnn
  let pattern = ParserRefs.pattern

  let ws = spaces
  let strWs s = pstring s .>> ws

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let withSpan p =
    pipe3 getPosition p getPosition
    <| fun start value stop -> (value, { Start = start; Stop = stop })

  let private identPattern =
    withSpan ident
    |>> fun (id, span) ->
      { Pattern.Kind =
          PatternKind.Identifier(
            { Name = id
              Span = span
              IsMut = false }
          )
        Span = span
        InferredType = None }

  let private literalPattern =
    withSpan lit
    |>> fun (lit, span) ->
      { Pattern.Kind = PatternKind.Literal(span = span, value = lit)
        Span = span
        InferredType = None }

  let private tuplePattern =
    tuple pattern |> withSpan
    |>> fun (patterns, span) ->
      { Pattern.Kind = PatternKind.Tuple(patterns)
        Span = span
        InferredType = None }

  let private wildcardPattern =
    withSpan (strWs "_")
    |>> fun (_, span) ->
      { Pattern.Kind = PatternKind.Wildcard
        Span = span
        InferredType = None }

  let private objPatKeyValue =
    pipe4 getPosition ident pattern getPosition
    <| fun start id pat stop ->
      let span = { Start = start; Stop = stop }
      KeyValuePat(span = span, key = id, value = pat, init = None)

  let private objPatElem = objPatKeyValue

  let private objectPattern =
    withSpan (between (strWs "{") (strWs "}") (sepBy objPatElem (strWs ",")))
    |>> fun (objElems, span) ->
      { Pattern.Kind = PatternKind.Object(objElems)
        Span = span
        InferredType = None }

  ParserRefs.patternRef.Value <-
    choice
      [ identPattern
        literalPattern
        wildcardPattern
        objectPattern
        tuplePattern ]
