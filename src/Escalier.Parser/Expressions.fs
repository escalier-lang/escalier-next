namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax
open System.Text

exception ParseError of string

module private Expressions =
  let expr = ParserRefs.expr
  let stmt = ParserRefs.stmt
  let typeAnn = ParserRefs.typeAnn
  let pattern = ParserRefs.pattern

  let ws = spaces
  let str_ws s = pstring s .>> ws

  let mergeSpans (x: Span) (y: Span) = { start = x.start; stop = y.stop }

  let opp = OperatorPrecedenceParser<Expr, list<Expr> * Position, unit>()

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let identExpr: Parser<Expr, unit> =
    pipe3 getPosition ident getPosition
    <| fun start sl stop ->
      { kind = Identifer(sl)
        span = { start = start; stop = stop }
        inferred_type = None }

  let templateStringLiteral: Parser<Expr, unit> =
    fun stream ->
      let sb = StringBuilder()
      let mutable parts: list<string> = []
      let mutable exprs: list<Expr> = []
      let mutable reply: voption<Reply<Expr>> = ValueNone

      if stream.Peek() = '`' then
        let start = stream.Position
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
          let stop = stream.Position
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

  let funcParam = ident //  .>> (str_ws ":" >>. ident)

  let block: Parser<BlockOrExpr, unit> =
    pipe3
      getPosition
      (between (str_ws "{") (str_ws "}") (many stmt))
      getPosition
    <| fun start stmts stop ->
      BlockOrExpr.Block(
        { span = { start = start; stop = stop }
          stmts = stmts }
      )

  let func: Parser<Expr, unit> =
    pipe5
      getPosition
      (str_ws "fn")
      (between (str_ws "(") (str_ws ")") (sepBy funcParam (str_ws ",")))
      block
      getPosition
    <| fun start _ params_ body stop ->
      { kind = ExprKind.Function(params_, body)
        span = { start = start; stop = stop }
        inferred_type = None }

  let ifElse: Parser<Expr, unit> =
    pipe5
      getPosition
      ((str_ws "if") >>. expr)
      block
      (str_ws "else" >>. block)
      getPosition
    <| fun start cond then_ else_ stop ->
      { kind = ExprKind.If(cond, then_, else_)
        span = { start = start; stop = stop }
        inferred_type = None }

  let literalExpr: Parser<Expr, unit> =
    pipe3 getPosition Literals.literal getPosition
    <| fun start lit stop ->
      { kind = ExprKind.Literal(lit)
        span = { start = start; stop = stop }
        inferred_type = None }

  let atom =
    choice [ literalExpr; func; ifElse; templateStringLiteral; identExpr ]

  let term = (atom .>> ws) <|> between (str_ws "(") (str_ws ")") expr

  opp.TermParser <- term

  type Assoc = Associativity

  let binary op x y =
    { Expr.kind = ExprKind.Binary(x, op, y)
      span = mergeSpans x.span y.span
      inferred_type = None }

  let after = getPosition .>> ws |>> fun pos -> ([], pos)

  opp.AddOperator(
    PostfixOperator(
      "[",
      (pipe2 getPosition ((ws >>. expr) .>> (str_ws "]"))
       <| fun p1 expr -> ([ expr ], p1)), // (indices, position)
      18,
      true,
      (),
      (fun (indices, stop) target ->
        { Expr.kind = ExprKind.Index(target, indices[0], false)
          span =
            { start = target.span.start
              stop = stop }
          inferred_type = None })
    )
  )

  opp.AddOperator(
    PostfixOperator(
      "(",
      (pipe2 getPosition (sepBy (ws >>. expr) (str_ws ",") .>> (str_ws ")"))
       <| fun p1 args -> (args, p1)), // args
      18,
      true,
      (),
      (fun (args, stop) callee ->
        { Expr.kind =
            ExprKind.Call(
              callee = callee,
              type_args = None,
              args = args,
              opt_chain = false,
              throws = None
            )
          span =
            { start = callee.span.start
              stop = stop }
          inferred_type = None })
    )
  )

  // logical not (14)
  // bitwise not (14)

  opp.AddOperator(
    PrefixOperator(
      "+",
      after,
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
      after,
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

  opp.AddOperator(InfixOperator("**", after, 13, Assoc.Right, binary Exp))

  opp.AddOperator(InfixOperator("*", after, 12, Assoc.Left, binary Mul))

  opp.AddOperator(InfixOperator("/", after, 12, Assoc.Left, binary Div))

  opp.AddOperator(InfixOperator("%", after, 12, Assoc.Left, binary Mod))

  opp.AddOperator(InfixOperator("+", after, 11, Assoc.Left, binary Add))

  opp.AddOperator(InfixOperator("-", after, 11, Assoc.Left, binary Sub))

  opp.AddOperator(InfixOperator("<", after, 9, Assoc.Left, binary LessThan))

  opp.AddOperator(
    InfixOperator("<=", after, 9, Assoc.Left, binary LessThanOrEqual)
  )

  opp.AddOperator(InfixOperator(">", after, 9, Assoc.Left, binary GreaterThan))

  opp.AddOperator(
    InfixOperator(">=", after, 9, Assoc.Left, binary GreaterThanOrEqual)
  )

  opp.AddOperator(InfixOperator("==", after, 8, Assoc.Left, binary Equal))

  opp.AddOperator(InfixOperator("!=", after, 8, Assoc.Left, binary NotEqual))

  // bitwise and (7)
  // bitwise xor (6)
  // bitwise or (5)

  opp.AddOperator(InfixOperator("&&", after, 4, Assoc.Left, binary Add))

  opp.AddOperator(InfixOperator("||", after, 3, Assoc.Left, binary Or))

  opp.AddOperator(
    InfixOperator(
      "=",
      after,
      2,
      Assoc.Right,
      (fun x y ->
        { Expr.kind = Assign(x, AssignOp.Assign, y)
          span = mergeSpans x.span y.span
          inferred_type = None })
    )
  )

  ParserRefs.exprRef.Value <- opp.ExpressionParser
