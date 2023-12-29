module rec Pratt

open FParsec

let ws = spaces

let ident: Parser<string, unit> =
  let isIdentifierFirstChar c = isLetter c || c = '_'
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

let identExpr = ident |>> fun ident -> Expr.Ident ident

type Expr =
  | Ident of string
  | Literal of int
  | Binary of Expr * string * Expr
  | Nary of Expr list * string
  | Unary of string * Expr
  | Postfix of Expr * string

type PrefixParselet =
  { Parse: PrattParser * CharStream<unit> * string -> Reply<Expr>
    Precedence: int }

type InfixParselet =
  { Parse: PrattParser * CharStream<unit> * Expr * string -> Reply<Expr>
    Precedence: int }

type PrattParser() =
  let mutable prefixParselets: Map<string, PrefixParselet> = Map.empty
  let mutable infixParselets: Map<string, InfixParselet> = Map.empty

  member this.RegisterPrefix(operator: string, parselet: PrefixParselet) =
    prefixParselets <- Map.add operator parselet prefixParselets
  // prefixPrecedence <- Map.add operator precedence prefixPrecedence

  member this.RegisterInfix(operator: string, parselet: InfixParselet) =
    infixParselets <- Map.add operator parselet infixParselets

  member this.RegisterPostfix(operator: string, parselet: InfixParselet) =
    infixParselets <- Map.add operator parselet infixParselets

  member this.GetPrecedence(operator: string) =
    match infixParselets.TryFind operator with
    | Some parselet -> parselet.Precedence
    | None -> 0

  // TODO: use a `stream` directly to implement a pratt parser
  // with a stream we can peek, skip, and rewind
  // we can also pass the stream to other parsers combinators
  // e.g. expr stream
  member this.ParseExpr(precedence: int) : Parser<Expr, unit> =
    fun stream ->
      let operator = stream.PeekString(1)

      let mutable left =
        match prefixParselets.TryFind operator with
        | Some parselet ->
          stream.Skip(1)
          parselet.Parse(this, stream, operator)
        | _ -> identExpr stream

      while precedence < this.GetPrecedence(stream.PeekString(1)) do
        left <-
          let operator = stream.PeekString(1)

          match infixParselets.TryFind operator with
          | Some parselet ->
            stream.Skip(1)
            ws stream |> ignore // always succeeds
            parselet.Parse(this, stream, left.Result, operator)
          | _ ->
            printfn "no infix parselet for %s" operator
            left

      left

let exprParser = PrattParser()

let prefixParselet (precedence: int) : PrefixParselet =
  { Parse =
      fun (parser, stream, operator) ->
        let precedence = parser.GetPrecedence(operator)
        let operand = parser.ParseExpr precedence stream
        Reply(Unary(operator, operand.Result))
    Precedence = precedence }


let infixParselet (precedence: int) : InfixParselet =
  { Parse =
      fun (parser, stream, left, operator) ->
        let precedence = parser.GetPrecedence(operator)
        let right = parser.ParseExpr precedence stream
        Reply(Binary(left, operator, right.Result))
    Precedence = precedence }

let naryInfixParselet (precedence: int) : InfixParselet =
  { Parse =
      fun (parser, stream, left, operator) ->
        let mutable operands = [ left ]
        let precedence = parser.GetPrecedence(operator)

        let operand = parser.ParseExpr precedence stream
        operands <- operand.Result :: operands

        while stream.PeekString(1) = operator do
          stream.Skip(1)
          ws stream |> ignore // always succeeds
          let operand = parser.ParseExpr precedence stream
          operands <- operand.Result :: operands

        Reply(Nary(List.rev operands, operator))
    Precedence = precedence }

let postfixParselet (precedence: int) : InfixParselet =
  { Parse =
      fun (parser, stream, left, operator) -> Reply(Postfix(left, operator))
    Precedence = precedence }


exprParser.RegisterPrefix("+", prefixParselet 14)
exprParser.RegisterPrefix("-", prefixParselet 14)

exprParser.RegisterInfix("*", infixParselet 12)
exprParser.RegisterInfix("/", infixParselet 12)
exprParser.RegisterInfix("+", infixParselet 11)
exprParser.RegisterInfix("-", infixParselet 11)

exprParser.RegisterInfix("&", naryInfixParselet 4)
exprParser.RegisterInfix("|", naryInfixParselet 3)

exprParser.RegisterPostfix("!", postfixParselet 15)
