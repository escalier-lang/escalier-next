module rec Pratt

open FParsec

let ws = spaces
let strWs s = pstring s .>> ws

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
  | Call of Expr * list<Expr>

type PrefixParselet<'T> =
  { Parse: PrattParser<'T> * CharStream<unit> * string -> Reply<'T>
    Precedence: int }

type InfixParselet<'T> =
  { Parse: PrattParser<'T> * CharStream<unit> * 'T * string -> Reply<'T>
    Precedence: int }

type PrattParser<'T>(term: Parser<'T, unit>) =
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

  member this.Parse(precedence: int) : Parser<'T, unit> =
    fun stream ->

      let nud () =
        match this.NextPrefixOperator(stream) with
        | Some(operator, parselet) ->
          stream.Skip(operator.Length)
          parselet.Parse(this, stream, operator)
        | None -> term stream

      let left = nud ()

      let rec led (left: Reply<'T>) =
        match this.NextInfixOperator(stream) with
        | Some(operator, parselet) ->
          if precedence < this.GetPrecedence(operator) then
            stream.Skip(operator.Length)
            ws stream |> ignore // always succeeds
            led (parselet.Parse(this, stream, left.Result, operator))
          else
            left
        | None -> left

      led left

let prefixParselet (precedence: int) : PrefixParselet<Expr> =
  { Parse =
      fun (parser, stream, operator) ->
        let precedence = parser.GetPrecedence(operator)
        let operand = parser.Parse precedence stream
        Reply(Unary(operator, operand.Result))
    Precedence = precedence }

let groupingParselet (precedence: int) : PrefixParselet<'T> =
  { Parse =
      fun (parser, stream, operator) ->
        let operand = parser.Parse 0 stream
        stream.Skip(1) // skip ')'
        Reply(operand.Result)
    Precedence = precedence }

let infixParselet (precedence: int) : InfixParselet<Expr> =
  { Parse =
      fun (parser, stream, left, operator) ->
        let precedence = parser.GetPrecedence(operator)
        let right = parser.Parse precedence stream
        Reply(Binary(left, operator, right.Result))
    Precedence = precedence }

let naryInfixParselet (precedence: int) : InfixParselet<Expr> =
  { Parse =
      fun (parser, stream, left, operator) ->
        let right = parser.Parse precedence stream

        let rec led (left: Reply<Expr>) =
          match parser.NextInfixOperator(stream) with
          | Some(nextOperator, parselet) ->
            if operator = nextOperator then
              stream.Skip(nextOperator.Length)
              ws stream |> ignore // always succeeds
              left.Result :: led (parser.Parse precedence stream)
            else
              [ left.Result ]
          | None -> [ left.Result ]

        let operands = left :: led right

        Reply(Nary(operands, operator))
    Precedence = precedence }

let callParselet (precedence: int) : InfixParselet<Expr> =
  { Parse =
      fun (parser, stream, left, operator) ->

        let args =
          if stream.PeekString(1) = ")" then
            stream.Skip(1)
            ws stream |> ignore // always succeeds
            []
          else
            let parseArgs = sepBy (parser.Parse(0)) (strWs ",")
            let reply = (parseArgs .>> (strWs ")")) stream
            reply.Result

        Reply(Call(left, args))
    Precedence = precedence }

let postfixParselet (precedence: int) : InfixParselet<Expr> =
  { Parse =
      fun (parser, stream, left, operator) -> Reply(Postfix(left, operator))
    Precedence = precedence }


let exprParser = PrattParser<Expr>(identExpr)

exprParser.RegisterPrefix("(", groupingParselet 18)

exprParser.RegisterInfix("(", callParselet 17)

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

exprParser.RegisterInfix("&&", infixParselet 4)
exprParser.RegisterInfix("||", infixParselet 3)
exprParser.RegisterInfix("&", naryInfixParselet 4)
exprParser.RegisterInfix("|", naryInfixParselet 3)

exprParser.RegisterPostfix("!", postfixParselet 15)
