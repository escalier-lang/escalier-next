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

type PrefixParselet<'T> =
  { Parse: PrattParser<'T> * CharStream<unit> * string -> Reply<Expr>
    Precedence: int }

type InfixParselet<'T> =
  { Parse: PrattParser<'T> * CharStream<unit> * Expr * string -> Reply<Expr>
    Precedence: int }

type PrattParser<'T>(term: Parser<Expr, unit>) =
  let mutable prefixParselets: Map<string, PrefixParselet<'T>> = Map.empty
  let mutable infixParselets: Map<string, InfixParselet<'T>> = Map.empty

  member this.RegisterPrefix(operator: string, parselet: PrefixParselet<'T>) =
    prefixParselets <- Map.add operator parselet prefixParselets

  member this.RegisterInfix(operator: string, parselet: InfixParselet<'T>) =
    infixParselets <- Map.add operator parselet infixParselets

  member this.RegisterPostfix(operator: string, parselet: InfixParselet<'T>) =
    infixParselets <- Map.add operator parselet infixParselets

  member this.GetPrecedence(operator: string) =
    match infixParselets.TryFind operator with
    | Some parselet -> parselet.Precedence
    | None -> 0

  member this.NextPrefixOperator
    (stream: CharStream<unit>)
    : Option<string * PrefixParselet<'T>> =
    let nextTwoChars = stream.PeekString(2)
    let nextOneChar = stream.PeekString(1)

    match prefixParselets.TryFind nextTwoChars with
    | Some parselet -> Some(nextTwoChars, parselet)
    | None ->
      match prefixParselets.TryFind nextOneChar with
      | Some parselet -> Some(nextOneChar, parselet)
      | None -> None

  member this.NextInfixOperator
    (stream: CharStream<unit>)
    : Option<string * InfixParselet<'T>> =
    let nextTwoChars = stream.PeekString(2)
    let nextOneChar = stream.PeekString(1)

    match infixParselets.TryFind nextTwoChars with
    | Some parselet -> Some(nextTwoChars, parselet)
    | None ->
      match infixParselets.TryFind nextOneChar with
      | Some parselet -> Some(nextOneChar, parselet)
      | None -> None

  member this.ParseExpr(precedence: int) : Parser<Expr, unit> =
    fun stream ->

      let nud () =
        match this.NextPrefixOperator(stream) with
        | Some(operator, parselet) ->
          stream.Skip(operator.Length)
          parselet.Parse(this, stream, operator)
        | None -> term stream

      let left = nud ()

      let rec led (left: Reply<Expr>) =
        match precedence < this.GetPrecedence(stream.PeekString(1)) with
        | true ->

          let left =
            match this.NextInfixOperator(stream) with
            | Some(operator, parselet) ->
              stream.Skip(operator.Length)
              ws stream |> ignore // always succeeds
              parselet.Parse(this, stream, left.Result, operator)
            | _ -> left

          led left
        | false -> left

      led left

let prefixParselet (precedence: int) : PrefixParselet<'T> =
  { Parse =
      fun (parser, stream, operator) ->
        let precedence = parser.GetPrecedence(operator)
        let operand = parser.ParseExpr precedence stream
        Reply(Unary(operator, operand.Result))
    Precedence = precedence }


let infixParselet (precedence: int) : InfixParselet<'T> =
  { Parse =
      fun (parser, stream, left, operator) ->
        let precedence = parser.GetPrecedence(operator)
        let right = parser.ParseExpr precedence stream
        Reply(Binary(left, operator, right.Result))
    Precedence = precedence }

let naryInfixParselet (precedence: int) : InfixParselet<'T> =
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

let postfixParselet (precedence: int) : InfixParselet<'T> =
  { Parse =
      fun (parser, stream, left, operator) -> Reply(Postfix(left, operator))
    Precedence = precedence }


let exprParser = PrattParser<Expr>(identExpr)

exprParser.RegisterPrefix("+", prefixParselet 14)
exprParser.RegisterPrefix("-", prefixParselet 14)

exprParser.RegisterInfix("*", infixParselet 12)
exprParser.RegisterInfix("/", infixParselet 12)
exprParser.RegisterInfix("+", infixParselet 11)
exprParser.RegisterInfix("++", infixParselet 11)
exprParser.RegisterInfix("-", infixParselet 11)

exprParser.RegisterInfix("==", infixParselet 9)
exprParser.RegisterInfix("!=", infixParselet 9)
exprParser.RegisterInfix("<", infixParselet 9)
exprParser.RegisterInfix("<=", infixParselet 9)
exprParser.RegisterInfix(">", infixParselet 9)
exprParser.RegisterInfix(">=", infixParselet 9)

exprParser.RegisterInfix("&", naryInfixParselet 4)
exprParser.RegisterInfix("|", naryInfixParselet 3)

exprParser.RegisterPostfix("!", postfixParselet 15)
