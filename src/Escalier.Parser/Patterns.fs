namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax

module private Patterns =
  let expr = ParserRefs.expr
  let stmt = ParserRefs.stmt
  let typeAnn = ParserRefs.typeAnn
  let pattern = ParserRefs.pattern

  let ws = spaces
  let str_ws s = pstring s .>> ws

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let withSpan p =
    pipe3 getPosition p getPosition
    <| fun start value stop -> (value, { start = start; stop = stop })

  let private identPattern =
    withSpan ident
    |>> fun (id, span) ->
      { Pattern.kind = PatternKind.Identifier(span, id, false)
        span = span
        inferred_type = None }

  let private literalPattern =
    withSpan Literals.literal
    |>> fun (lit, span) ->
      { Pattern.kind = PatternKind.Literal(span = span, value = lit)
        span = span
        inferred_type = None }

  let private tuplePattern =
    withSpan (between (str_ws "[") (str_ws "]") (sepBy pattern (str_ws ",")))
    |>> fun (patterns, span) ->
      { Pattern.kind = PatternKind.Tuple(patterns)
        span = span
        inferred_type = None }

  let private wildcardPattern =
    withSpan (str_ws "_")
    |>> fun (_, span) ->
      { Pattern.kind = PatternKind.Wildcard
        span = span
        inferred_type = None }

  let private objPatKeyValue =
    pipe4 getPosition ident pattern getPosition
    <| fun start id pat stop ->
      let span = { start = start; stop = stop }
      KeyValuePat(span = span, key = id, value = pat, init = None)

  let private objPatElem = objPatKeyValue

  let private objectPattern =
    withSpan (between (str_ws "{") (str_ws "}") (sepBy objPatElem (str_ws ",")))
    |>> fun (objElems, span) ->
      { Pattern.kind = PatternKind.Object(objElems)
        span = span
        inferred_type = None }

  ParserRefs.patternRef.Value <-
    choice
      [ identPattern
        literalPattern
        wildcardPattern
        objectPattern
        tuplePattern ]
