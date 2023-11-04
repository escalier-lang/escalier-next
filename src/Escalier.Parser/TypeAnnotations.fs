namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax

module private TypeAnnotations =
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

  let private parenthesizedTypeAnn = (between (str_ws "(") (str_ws ")") typeAnn)

  let private keywordTypeAnn =

    let keyword =
      choice
        [ (str_ws "object" |>> fun _ -> KeywordType.Object)
          (str_ws "never" |>> fun _ -> Never)
          (str_ws "unknown" |>> fun _ -> Unknown)
          (str_ws "boolean" |>> fun _ -> Boolean)
          (str_ws "number" |>> fun _ -> Number)
          (str_ws "string" |>> fun _ -> String)
          (str_ws "symbol" |>> fun _ -> Symbol)
          (str_ws "null" |>> fun _ -> Null)
          (str_ws "undefined" |>> fun _ -> Undefined) ]

    withSpan keyword
    |>> fun (keyword, span) ->
      { TypeAnn.kind = TypeAnnKind.Keyword(keyword)
        span = span
        inferred_type = None }

  let private tupleTypeAnn =
    withSpan (between (str_ws "[") (str_ws "]") (sepBy typeAnn (str_ws ",")))
    |>> fun (typeAnns, span) ->
      { TypeAnn.kind = TypeAnnKind.Tuple(typeAnns)
        span = span
        inferred_type = None }

  let private keyofTypeAnn =
    withSpan (str_ws "keyof" >>. typeAnn)
    |>> fun (typeAnn, span) ->
      { TypeAnn.kind = TypeAnnKind.Keyof(typeAnn)
        span = span
        inferred_type = None }

  let private restTypeAnn =
    withSpan (str_ws "..." >>. typeAnn)
    |>> fun (typeAnn, span) ->
      { TypeAnn.kind = TypeAnnKind.Rest(typeAnn)
        span = span
        inferred_type = None }

  let private typeofTypeAnn =
    withSpan (str_ws "typeof" >>. expr)
    |>> fun (e, span) ->
      { TypeAnn.kind = TypeAnnKind.Typeof(e)
        span = span
        inferred_type = None }

  let private typeRef =
    pipe4
      getPosition
      ident
      (opt (between (str_ws "<") (str_ws ">") (sepBy typeAnn (str_ws ","))))
      getPosition
    <| fun start name typeArgs stop ->
      { TypeAnn.kind = TypeAnnKind.TypeRef(name, typeArgs)
        span = { start = start; stop = stop }
        inferred_type = None }

  let opp = OperatorPrecedenceParser<TypeAnn, Position, unit>()

  let primaryType = opp.ExpressionParser

  // NOTE(kevinb): We use an operator precedence parser to workaround the
  // fact that `[]` is left recursive.
  opp.AddOperator(
    PostfixOperator(
      "[]",
      getPosition .>> ws,
      1,
      true,
      (),
      (fun stop target ->
        { TypeAnn.kind = Array(target)
          span =
            { start = target.span.start
              stop = stop }
          inferred_type = None })
    )
  )

  opp.TermParser <-
    choice
      [ parenthesizedTypeAnn
        keywordTypeAnn // aka PredefinedType
        // TODO: objectTypeAnn
        tupleTypeAnn
        typeofTypeAnn // aka TypeQuery
        keyofTypeAnn
        restTypeAnn
        // TODO: thisTypeAnn
        // NOTE: should come last since any identifier can be a type reference
        typeRef ]

  // NOTE: We don't use InfixOperator here because that only supports
  // binary operators and intersection types are n-ary.
  let intersectionOrPrimaryType: Parser<TypeAnn, unit> =
    withSpan (sepBy1 primaryType (str_ws "&"))
    |>> fun (typeAnns, span) ->
      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | _ ->
        { TypeAnn.kind = TypeAnnKind.Intersection(typeAnns)
          span = span
          inferred_type = None }

  // NOTE: We don't use InfixOperator here because that only supports
  // binary operators and union types are n-ary.
  let unionOrIntersectionOrPrimaryType: Parser<TypeAnn, unit> =
    withSpan (sepBy1 intersectionOrPrimaryType (str_ws "|"))
    |>> fun (typeAnns, span) ->
      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | _ ->
        { TypeAnn.kind = TypeAnnKind.Union(typeAnns)
          span = span
          inferred_type = None }

  // TODO: handle function types
  ParserRefs.typeAnnRef.Value <- unionOrIntersectionOrPrimaryType