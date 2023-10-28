namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax

module ExprParser =
  let mergeSpans (x: Span) (y: Span) = { start = x.start; stop = y.stop }

  let opp = new OperatorPrecedenceParser<Expr, unit, unit>()
  let expr: Parser<Expr, unit> = opp.ExpressionParser

  let ws = spaces

  let str_ws s = pstring s .>> ws

  let number: Parser<Expr, unit> =
    pipe3 getPosition pfloat getPosition
    <| fun p1 nl p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { kind = Literal(Literal.Number(nl |> string))
        span = { start = start; stop = stop }
        inferred_type = ref None }

  let identifier: Parser<Expr, unit> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    let _ident =
      many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

    pipe3 getPosition _ident getPosition
    <| fun p1 sl p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { kind = Identifer(sl)
        span = { start = start; stop = stop }
        inferred_type = ref None }

  let stringLiteral: Parser<Expr, unit> =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')

    let unescape c =
      match c with
      | 'n' -> "\n"
      | 'r' -> "\r"
      | 't' -> "\t"
      | c -> string c

    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)

    let _string =
      (between
        (pstring "\"")
        (pstring "\"")
        (stringsSepBy normalCharSnippet escapedChar))

    pipe3 getPosition _string getPosition
    <| fun p1 sl p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { kind = ExprKind.Literal(Literal.String(sl))
        span = { start = start; stop = stop }
        inferred_type = ref None }


  let templateStringLiteral: Parser<Expr, unit> =
    fun stream ->
      let mutable currentString = ""
      let mutable parts: list<string> = []
      let mutable exprs: list<Expr> = []
      let mutable reply: option<Reply<Expr>> = None

      if stream.Peek() = '`' then
        let start = stream.Index |> int
        stream.Skip() // '`'

        while stream.Peek() <> '`' && reply = None do
          if stream.PeekString(2) = "${" then
            stream.Skip(2) // '${'
            parts <- currentString :: parts
            currentString <- ""
            let e = expr stream

            if e.Status = ReplyStatus.Ok then
              if stream.Peek() = '}' then
                stream.Skip()
                exprs <- e.Result :: exprs
              else
                reply <- Some(Reply(Error, messageError "Expected '}'"))
            else
              reply <- Some(e)
          else
            currentString <- currentString + string (stream.Read())

        match reply with
        | None ->
          stream.Skip() // '`'
          let stop = stream.Index |> int
          parts <- currentString :: parts

          let result: Expr =
            { kind =
                TemplateLiteral(
                  { parts = List.rev parts
                    exprs = List.rev exprs }
                )
              span = { start = start; stop = stop }
              inferred_type = ref None }

          Reply(result)
        | Some(value) -> value
      else
        Reply(Error, messageError "Expected '`'")

  let atom = number <|> identifier <|> stringLiteral <|> templateStringLiteral
  let term = (atom .>> ws) <|> between (str_ws "(") (str_ws ")") expr

  opp.TermParser <- term

  type Assoc = Associativity

  // logical not (14)
  // bitwise not (14)

  opp.AddOperator(
    PrefixOperator(
      "+",
      ws,
      14,
      true,
      (fun x ->
        { Expr.kind = Unary("+", x)
          span = x.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    PrefixOperator(
      "-",
      ws,
      14,
      true,
      (fun x ->
        { Expr.kind = Unary("-", x)
          span = x.span
          inferred_type = ref None })
    )
  )

  // typeof (14)
  // delete (14)
  // await (14)

  opp.AddOperator(
    InfixOperator(
      "**",
      ws,
      13,
      Assoc.Right,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Exp, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "*",
      ws,
      12,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Mul, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "/",
      ws,
      12,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Div, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "%",
      ws,
      12,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Mod, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "+",
      ws,
      11,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Add, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "-",
      ws,
      11,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Sub, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "<",
      ws,
      9,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, LessThan, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "<=",
      ws,
      9,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, LessThanOrEqual, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      ">",
      ws,
      9,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, GreaterThan, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      ">=",
      ws,
      9,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, GreaterThanOrEqual, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "==",
      ws,
      8,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Equal, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "!=",
      ws,
      8,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, NotEqual, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  // bitwise and (7)
  // bitwise xor (6)
  // bitwise or (5)

  opp.AddOperator(
    InfixOperator(
      "&&",
      ws,
      4,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, And, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "||",
      ws,
      3,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Or, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "=",
      ws,
      2,
      Assoc.Right,
      (fun x y ->
        { Expr.kind = ExprKind.Assign(x, AssignOp.Assign, y)
          span = mergeSpans x.span y.span
          inferred_type = ref None })
    )
  )
