module rec Pratt

open FParsec

type Number =
  | Float of float
  | Int of int

type Expr =
  | Ident of string
  | Literal of Number
  | Binary of Expr * string * Expr
  | Nary of Expr list * string
  | Unary of string * Expr
  | Postfix of Expr * string
  | Call of Expr * list<Expr>
  | Index of Expr * Expr

let ws = spaces
let strWs s = pstring s .>> ws

let ident: Parser<string, unit> =
  let isIdentifierFirstChar c = isLetter c || c = '_'
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

let number: Parser<Number, unit> =
  let parser =
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

  parser .>> ws

let identExpr: Parser<Expr, unit> = ident |>> Expr.Ident
let numberExpr: Parser<Expr, unit> = number |>> Expr.Literal

type PrefixParselet<'T> =
  { Parse: PrattParser<'T> * CharStream<unit> * string -> Reply<'T>
    Precedence: int }

type InfixParselet<'T> =
  { Parse: PrattParser<'T> * CharStream<unit> * 'T * string -> Reply<'T>
    Precedence: int }

type PrattParser<'T>(term: Parser<'T, unit>) =
  // TODO: sort keys based on length
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

    // TODO: optimize this by only doing this grouping once after we've defined
    // all of the operators we care about.
    let groups =
      prefixParselets
      |> Map.toSeq
      // group keys based on length
      |> Seq.groupBy (fun (k, v) -> k.Length)
      // sort by the length
      |> Seq.sortBy (fun (k, v) -> k)
      // in descending order
      |> Seq.rev
      |> List.ofSeq

    let rec find (groups: list<int * seq<string * PrefixParselet<'T>>>) =
      match groups with
      | [] -> None
      | (length, group) :: rest ->
        let nextChars = stream.PeekString(length)

        match group |> Seq.tryFind (fun (k, v) -> k = nextChars) with
        | Some(operator, parselet) -> Some(operator, parselet)
        | None -> find rest

    find groups

  member this.NextInfixOperator
    (stream: CharStream<unit>)
    : Option<string * InfixParselet<'T>> =

    // TODO: optimize this by only doing this grouping once after we've defined
    // all of the operators we care about.
    let groups =
      infixParselets
      |> Map.toSeq
      // group keys based on length
      |> Seq.groupBy (fun (k, v) -> k.Length)
      // sort by the length
      |> Seq.sortBy (fun (k, v) -> k)
      // in descending order
      |> Seq.rev
      |> List.ofSeq

    let rec find (groups: list<int * seq<string * InfixParselet<'T>>>) =
      match groups with
      | [] -> None
      | (length, group) :: rest ->
        let nextChars = stream.PeekString(length)

        match group |> Seq.tryFind (fun (k, v) -> k = nextChars) with
        | Some(operator, parselet) -> Some(operator, parselet)
        | None -> find rest

    find groups

  member this.Parse(precedence: int) : Parser<'T, unit> =
    fun stream ->

      let nud () =
        match this.NextPrefixOperator(stream) with
        | Some(operator, parselet) ->
          stream.Skip(operator.Length)

          if operator |> Seq.forall System.Char.IsLetter then
            ws stream |> ignore // skip over trailing whitespace after operator
          else
            ()

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

      match left.Status with
      | Ok -> led left
      | _ -> left

let prefixParselet (precedence: int) : PrefixParselet<Expr> =
  { Parse =
      fun (parser, stream, operator) ->
        let precedence = parser.GetPrecedence(operator)
        let operand = parser.Parse precedence stream

        match operand.Status with
        | Ok -> Reply(Unary(operator, operand.Result))
        | _ -> operand
    Precedence = precedence }

let groupingParselet (precedence: int) : PrefixParselet<'T> =
  { Parse =
      fun (parser, stream, operator) ->
        let operand = parser.Parse 0 stream
        stream.Skip(1) // skip ')'

        operand
    Precedence = precedence }

let infixParselet (precedence: int) : InfixParselet<Expr> =
  { Parse =
      fun (parser, stream, left, operator) ->
        let precedence = parser.GetPrecedence(operator)
        let right = parser.Parse precedence stream

        match right.Status with
        | Ok -> Reply(Binary(left, operator, right.Result))
        | _ -> right
    Precedence = precedence }

let naryInfixParselet (precedence: int) : InfixParselet<Expr> =
  { Parse =
      fun (parser, stream, left, operator) ->
        let right = parser.Parse precedence stream

        let rec led (acc: list<Expr>) (left: Reply<Expr>) =
          match parser.NextInfixOperator(stream) with
          | Some(nextOperator, parselet) ->
            if operator = nextOperator then
              stream.Skip(nextOperator.Length)
              ws stream |> ignore // always succeeds
              led (left.Result :: acc) (parser.Parse precedence stream)
            else
              left.Result :: acc
          | None -> left.Result :: acc

        let operands = led [ left ] right

        Reply(Nary(List.rev operands, operator))
    Precedence = precedence }

let callParselet (precedence: int) : InfixParselet<Expr> =
  { Parse =
      fun (parser, stream, left, operator) ->

        let args =
          if stream.PeekString(1) = ")" then
            stream.Skip(1)
            ws stream |> ignore // always succeeds
            Reply([])
          else
            let parseArgs = sepBy (parser.Parse(0)) (strWs ",")
            let reply = (parseArgs .>> (strWs ")")) stream
            reply

        match args.Status with
        | Ok -> Reply(Call(left, args.Result))
        | _ -> Reply(args.Status, args.Error)
    Precedence = precedence }

let indexParselet (precedence: int) : InfixParselet<Expr> =
  { Parse =
      fun (parser, stream, left, operator) ->
        let index = parser.Parse(0)
        let reply = (index .>> (strWs "]")) stream

        match reply.Status with
        | Ok -> Reply(Index(left, reply.Result))
        | _ -> Reply(reply.Status, reply.Error)
    Precedence = precedence }

let postfixParselet (precedence: int) : InfixParselet<Expr> =
  { Parse =
      fun (parser, stream, left, operator) -> Reply(Postfix(left, operator))
    Precedence = precedence }
