namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax
open System.Text
open Shared

module private Expressions =
  let lit = ParserRefs.lit
  let expr = ParserRefs.expr
  let stmt = ParserRefs.stmt
  let typeAnn = ParserRefs.typeAnn
  let pattern = ParserRefs.pattern

  let mergeSpans (x: Span) (y: Span) = { start = x.start; stop = y.stop }

  let opp = OperatorPrecedenceParser<Expr, list<Expr> * Position, unit>()

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let identExpr: Parser<Expr, unit> =
    withSpan ident
    |>> fun (sl, span) ->
      { kind = Identifer(sl)
        span = span
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

  let block: Parser<BlockOrExpr, unit> =
    withSpan (between (str_ws "{") (str_ws "}") (many stmt))
    |>> fun (stmts, span) -> BlockOrExpr.Block({ span = span; stmts = stmts })

  let func: Parser<Function, unit> =
    pipe2 (func_sig opt) block <| fun sig' body -> { sig' = sig'; body = body }

  let funcExpr: Parser<Expr, unit> =
    withSpan func
    |>> fun (f, span) ->
      { kind = ExprKind.Function f
        span = span
        inferred_type = None }

  let tupleExpr: Parser<Expr, unit> =
    tuple expr |> withSpan
    |>> fun (exprs, span) ->
      { kind = ExprKind.Tuple(exprs)
        span = span
        inferred_type = None }

  let ifElse, ifElseRef = createParserForwardedToRef<Expr, unit> ()

  ifElseRef.Value <-
    pipe5
      getPosition
      ((str_ws "if") >>. expr)
      block
      (opt (
        str_ws "else"
        >>. ((ifElse |>> (fun e -> BlockOrExpr.Expr(e))) <|> block)
      ))
      getPosition
    <| fun start cond then_ else_ stop ->
      { kind = ExprKind.IfElse(cond, then_, else_)
        span = { start = start; stop = stop }
        inferred_type = None }

  let literalExpr: Parser<Expr, unit> =
    withSpan lit
    |>> fun (lit, span) ->
      { kind = ExprKind.Literal(lit)
        span = span
        inferred_type = None }

  let atom =
    choice
      [ literalExpr
        funcExpr
        ifElse
        tupleExpr
        templateStringLiteral
        identExpr ]

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
              { callee = callee
                typeArgs = None
                args = args
                optChain = false
                throws = None }
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
