namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax
open Shared

module private TypeAnnotations =
  let lit = ParserRefs.lit
  let expr = ParserRefs.expr
  let stmt = ParserRefs.stmt
  let typeAnn = ParserRefs.typeAnn
  let pattern = ParserRefs.pattern

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let private parenthesizedTypeAnn = (between (strWs "(") (strWs ")") typeAnn)

  let private litTypeAnn =
    withSpan lit
    |>> fun (lit, span) ->
      { TypeAnn.Kind = TypeAnnKind.Literal(lit)
        Span = span
        InferredType = None }

  let private keywordTypeAnn =

    let keyword =
      choice
        [ (strWs "object" |>> fun _ -> KeywordTypeAnn.Object)
          (strWs "never" |>> fun _ -> Never)
          (strWs "unknown" |>> fun _ -> Unknown)
          (strWs "boolean" |>> fun _ -> Boolean)
          (strWs "number" |>> fun _ -> Number)
          (strWs "string" |>> fun _ -> String)
          (strWs "symbol" |>> fun _ -> Symbol)
          (strWs "null" |>> fun _ -> Null)
          (strWs "undefined" |>> fun _ -> Undefined) ]

    withSpan keyword
    |>> fun (keyword, span) ->
      { TypeAnn.Kind = TypeAnnKind.Keyword(keyword)
        Span = span
        InferredType = None }

  let private tupleTypeAnn =
    tuple typeAnn |> withSpan
    |>> fun (typeAnns, span) ->
      { TypeAnn.Kind = TypeAnnKind.Tuple(typeAnns)
        Span = span
        InferredType = None }

  let private funcTypeAnn =
    funcSig id |> withSpan
    |>> fun (f, span) ->
      { TypeAnn.Kind = TypeAnnKind.Function(f)
        Span = span
        InferredType = None }

  let private keyofTypeAnn =
    withSpan (strWs "keyof" >>. typeAnn)
    |>> fun (typeAnn, span) ->
      { TypeAnn.Kind = TypeAnnKind.Keyof(typeAnn)
        Span = span
        InferredType = None }

  let private restTypeAnn =
    withSpan (strWs "..." >>. typeAnn)
    |>> fun (typeAnn, span) ->
      { TypeAnn.Kind = TypeAnnKind.Rest(typeAnn)
        Span = span
        InferredType = None }

  let private typeofTypeAnn =
    withSpan (strWs "typeof" >>. expr)
    |>> fun (e, span) ->
      { TypeAnn.Kind = TypeAnnKind.Typeof(e)
        Span = span
        InferredType = None }

  let private typeRef =
    pipe4
      getPosition
      ident
      (opt (between (strWs "<") (strWs ">") (sepBy typeAnn (strWs ","))))
      getPosition
    <| fun start name typeArgs stop ->
      { TypeAnn.Kind = TypeAnnKind.TypeRef(name, typeArgs)
        Span = { Start = start; Stop = stop }
        InferredType = None }

  // let private func: Parser<TypeAnn, unit> = failwith "todo"

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
        { TypeAnn.Kind = Array(target)
          Span =
            { Start = target.Span.Start
              Stop = stop }
          InferredType = None })
    )
  )

  opp.TermParser <-
    choice
      [ litTypeAnn
        parenthesizedTypeAnn
        keywordTypeAnn // aka PredefinedType
        tupleTypeAnn
        funcTypeAnn
        typeofTypeAnn // aka TypeQuery
        keyofTypeAnn
        restTypeAnn
        // TODO: objectTypeAnn
        // TODO: thisTypeAnn
        // NOTE: should come last since any identifier can be a type reference
        typeRef ]
    .>> ws

  // NOTE: We don't use InfixOperator here because that only supports
  // binary operators and intersection types are n-ary.
  let intersectionOrPrimaryType: Parser<TypeAnn, unit> =
    withSpan (sepBy1 primaryType (strWs "&"))
    |>> fun (typeAnns, span) ->
      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | _ ->
        { TypeAnn.Kind = TypeAnnKind.Intersection(typeAnns)
          Span = span
          InferredType = None }

  // NOTE: We don't use InfixOperator here because that only supports
  // binary operators and union types are n-ary.
  let unionOrIntersectionOrPrimaryType: Parser<TypeAnn, unit> =
    withSpan (sepBy1 intersectionOrPrimaryType (strWs "|"))
    |>> fun (typeAnns, span) ->
      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | _ ->
        { TypeAnn.Kind = TypeAnnKind.Union(typeAnns)
          Span = span
          InferredType = None }

  ParserRefs.typeAnnRef.Value <- unionOrIntersectionOrPrimaryType
