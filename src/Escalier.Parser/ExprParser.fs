#nowarn "40"
namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax
open System.Text

exception ParseError of string

module rec ExprParser =
  let mergeSpans (x: Span) (y: Span) = { start = x.start; stop = y.stop }

  let opp = new OperatorPrecedenceParser<Expr, list<Expr>, unit>()
  let expr: Parser<Expr, unit> = opp.ExpressionParser

  let ws = spaces

  let str_ws s = pstring s .>> ws

  let number: Parser<Literal, unit> =
    pipe3 getPosition pfloat getPosition
    <| fun p1 nl p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      Literal.Number(nl |> string)

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let identExpr: Parser<Expr, unit> =
    pipe3 getPosition ident getPosition
    <| fun p1 sl p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { kind = Identifer(sl)
        span = { start = start; stop = stop }
        inferred_type = None }

  let stringLiteral: Parser<Literal, unit> =
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

      Literal.String(sl)

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

  let func_param = ident //  .>> (str_ws ":" >>. ident)

  let func: Parser<Expr, unit> =
    pipe5
      getPosition
      (str_ws "fn")
      (between (str_ws "(") (str_ws ")") (sepBy func_param (str_ws ",")))
      (between (str_ws "{") (str_ws "}") (many stmt))
      getPosition
    <| fun p1 _ params_ stmts p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      let body =
        BlockOrExpr.Block(
          { span = { start = 0; stop = 0 }
            stmts = stmts }
        )

      { kind = ExprKind.Function(params_, body)
        span = { start = start; stop = stop }
        inferred_type = None }

  let literal = number <|> stringLiteral

  let literalExpr: Parser<Expr, unit> =
    pipe3 getPosition literal getPosition
    <| fun p1 lit p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { kind = ExprKind.Literal(lit)
        span = { start = start; stop = stop }
        inferred_type = None }

  let atom = literalExpr <|> identExpr <|> templateStringLiteral

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

  let expr_stmt: Parser<Stmt, unit> =
    pipe3 getPosition expr getPosition
    <| fun p1 e p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { kind = StmtKind.Expr(e)
        span = { start = start; stop = stop } }

  let return_stmt: Parser<Stmt, unit> =
    pipe3 getPosition ((str_ws "return") >>. opt (expr)) getPosition
    <| fun p1 e p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { kind = StmtKind.Return(e)
        span = { start = start; stop = stop } }

  // `let <expr> = <expr>`
  let var_decl =
    pipe4 getPosition (str_ws "let" >>. ident) (str_ws "=" >>. expr) getPosition
    <| fun p1 id e p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      let p: Pattern =
        { kind =
            PatternKind.Identifier(
              name = id,
              is_mut = false,
              span = { start = 0; stop = 0 }
            )
          span = { start = start; stop = stop }
          inferred_type = None }

      let decl: Decl =
        { kind = VarDecl(p, Some(e), None, false)
          span = { start = 0; stop = 0 } }

      { kind = StmtKind.Decl(decl)
        span = { start = start; stop = stop } }

  let type_decl () =
    raise (ParseError("not implemented yet"))

  let stmt: Parser<Stmt, unit> = var_decl <|> return_stmt <|> expr_stmt

  let private ident_pattern =
    pipe3 getPosition ident getPosition
    <| fun p1 id p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { Pattern.kind =
          PatternKind.Identifier(
            name = id,
            is_mut = false,
            span = { start = start; stop = stop }
          )
        span = { start = start; stop = stop }
        inferred_type = None }

  let private literal_pattern =
    pipe3 getPosition literal getPosition
    <| fun p1 lit p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { Pattern.kind =
          PatternKind.Literal(
            span = { start = start; stop = stop },
            value = lit
          )
        span = { start = start; stop = stop }
        inferred_type = None }

  let private tuple_pattern =
    pipe3
      getPosition
      (between (str_ws "[") (str_ws "]") (sepBy pattern (str_ws ",")))
      getPosition
    <| fun p1 patterns p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { Pattern.kind = PatternKind.Tuple(patterns)
        span = { start = start; stop = stop }
        inferred_type = None }

  let private wildcard_pattern =
    pipe3 getPosition (str_ws "_") getPosition
    <| fun p1 _ p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { Pattern.kind = PatternKind.Wildcard
        span = { start = start; stop = stop }
        inferred_type = None }

  let private objPatKeyValue =
    pipe4 getPosition ident pattern getPosition
    <| fun p1 id pat p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int
      let span = { start = start; stop = stop }
      KeyValue(span = span, key = id, value = pat, init = None)

  let private objPatElem = objPatKeyValue

  let private object_pattern =
    pipe3
      getPosition
      (between (str_ws "{") (str_ws "}") (sepBy objPatElem (str_ws ",")))
      getPosition
    <| fun p1 objElems p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int
      let span = { start = start; stop = stop }

      { Pattern.kind = PatternKind.Object(objElems)
        span = span
        inferred_type = None }

  let pattern: Parser<Pattern, unit> =
    ident_pattern
    <|> literal_pattern
    <|> wildcard_pattern
    <|> parse.Delay(fun () -> object_pattern) // avoid recursive data structure
    <|> parse.Delay(fun () -> tuple_pattern) // avoid recursive data structure
