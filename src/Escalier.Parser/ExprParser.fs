namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax
open System.Text

module ExprParser =
  let mergeSpans (x: Span) (y: Span) = { start = x.start; stop = y.stop }

  let opp = new OperatorPrecedenceParser<Expr, list<Expr>, unit>()
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
        inferred_type = None }

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
        inferred_type = None }

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
        inferred_type = None }


  let templateStringLiteral: Parser<Expr, unit> =
    fun stream ->
      let sb = new StringBuilder()
      let mutable parts: list<string> = []
      let mutable exprs: list<Expr> = []
      let mutable reply: voption<Reply<Expr>> = ValueNone

      if stream.Peek() = '`' then
        let start = stream.Index |> int
        stream.Skip() // '`'

        while stream.Peek() <> '`' && reply = ValueNone do
          if stream.PeekString(2) = "${" then
            stream.Skip(2) // '${'
            parts <- sb.ToString() :: parts
            sb.Clear() |> ignore
            let e = expr stream

            if e.Status = ReplyStatus.Ok then
              if stream.Peek() = '}' then
                stream.Skip()
                exprs <- e.Result :: exprs
              else
                reply <- ValueSome(Reply(Error, messageError "Expected '}'"))
            else
              reply <- ValueSome(e)
          else
            sb.Append(stream.Read()) |> ignore

        match reply with
        | ValueNone ->
          stream.Skip() // '`'
          let stop = stream.Index |> int
          parts <- sb.ToString() :: parts

          let result: Expr =
            { kind =
                TemplateLiteral(
                  { parts = List.rev parts
                    exprs = List.rev exprs }
                )
              span = { start = start; stop = stop }
              inferred_type = None }

          Reply(result)
        | ValueSome(value) -> value
      else
        Reply(Error, messageError "Expected '`'")

  let atom = number <|> identifier <|> stringLiteral <|> templateStringLiteral
  let term = (atom .>> ws) <|> between (str_ws "(") (str_ws ")") expr

  opp.TermParser <- term

  type Assoc = Associativity

  opp.AddOperator(
    PostfixOperator(
      "[",
      ((ws >>. expr) .>> (str_ws "]") |>> fun expr -> [ expr ]), // indices
      18,
      true,
      (),
      (fun indices target ->
        { Expr.kind = ExprKind.Index(target, indices[0], false)
          span = { start = 0; stop = 0 } // TODO
          inferred_type = None })
    )
  )

  opp.AddOperator(
    PostfixOperator(
      "(",
      sepBy (ws >>. expr) (str_ws ",") .>> (str_ws ")"), // args
      18,
      true,
      (),
      (fun args callee ->
        { Expr.kind =
            ExprKind.Call(
              callee = callee,
              type_args = None,
              args = args,
              opt_chain = false,
              throws = None
            )
          span = { start = 0; stop = 0 } // TODO
          inferred_type = None })
    )
  )

  // logical not (14)
  // bitwise not (14)

  opp.AddOperator(
    PrefixOperator(
      "+",
      ws >>. preturn [],
      14,
      true,
      (fun x ->
        { Expr.kind = Unary("+", x)
          span = x.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    PrefixOperator(
      "-",
      ws >>. preturn [],
      14,
      true,
      (fun x ->
        { Expr.kind = Unary("-", x)
          span = x.span
          inferred_type = None })
    )
  )

  // typeof (14)
  // delete (14)
  // await (14)

  opp.AddOperator(
    InfixOperator(
      "**",
      ws >>. preturn [],
      13,
      Assoc.Right,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Exp, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "*",
      ws >>. preturn [],
      12,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Mul, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "/",
      ws >>. preturn [],
      12,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Div, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "%",
      ws >>. preturn [],
      12,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Mod, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "+",
      ws >>. preturn [],
      11,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Add, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "-",
      ws >>. preturn [],
      11,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Sub, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "<",
      ws >>. preturn [],
      9,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, LessThan, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "<=",
      ws >>. preturn [],
      9,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, LessThanOrEqual, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      ">",
      ws >>. preturn [],
      9,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, GreaterThan, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      ">=",
      ws >>. preturn [],
      9,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, GreaterThanOrEqual, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "==",
      ws >>. preturn [],
      8,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Equal, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "!=",
      ws >>. preturn [],
      8,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, NotEqual, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  // bitwise and (7)
  // bitwise xor (6)
  // bitwise or (5)

  opp.AddOperator(
    InfixOperator(
      "&&",
      ws >>. preturn [],
      4,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, And, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "||",
      ws >>. preturn [],
      3,
      Assoc.Left,
      (fun x y ->
        { Expr.kind = ExprKind.Binary(x, Or, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  opp.AddOperator(
    InfixOperator(
      "=",
      ws >>. preturn [],
      2,
      Assoc.Right,
      (fun x y ->
        { Expr.kind = ExprKind.Assign(x, AssignOp.Assign, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )
