namespace Escalier.HindleyMilner

open FParsec
open System.Text

open Syntax

module Parser =
  let lit, litRef = createParserForwardedToRef<Literal, unit> ()
  let expr, exprRef = createParserForwardedToRef<Expr, unit> ()
  let stmt, stmtRef = createParserForwardedToRef<Stmt, unit> ()
  let pattern, patternRef = createParserForwardedToRef<Pattern, unit> ()
  let typeAnn, typeAnnRef = createParserForwardedToRef<TypeAnn, unit> ()

  let ws = spaces
  let strWs s = pstring s .>> ws

  let withSpan p =
    pipe3 getPosition p getPosition
    <| fun start value stop -> (value, { Start = start; Stop = stop })

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let tuple<'A> (parser: Parser<'A, unit>) =
    between (strWs "[") (strWs "]") (sepBy parser (strWs ","))

  let typeParam: Parser<TypeParam, unit> =
    pipe5
      getPosition
      ident
      (opt (strWs ":" >>. typeAnn))
      (opt (strWs "=" >>. typeAnn))
      getPosition
    <| fun start name constraint_ default_ stop ->
      { Name = name
        Constraint = constraint_
        Default = default_
        Span = { Start = start; Stop = stop } }

  let typeParams: Parser<list<TypeParam>, unit> =
    between (strWs "<") (strWs ">") (sepBy typeParam (strWs ","))

  let funcParam<'A>
    (opt_or_id: Parser<TypeAnn, unit> -> Parser<'A, unit>)
    : Parser<FuncParam<'A>, unit> =
    pipe3 pattern (opt (strWs "?")) (opt_or_id (strWs ":" >>. typeAnn))
    <| fun pattern optional typeAnn ->
      { Pattern = pattern
        Optional = optional.IsSome
        TypeAnn = typeAnn }

  let paramList<'A>
    (opt_or_id: Parser<TypeAnn, unit> -> Parser<'A, unit>)
    : Parser<list<FuncParam<'A>>, unit> =
    between (strWs "(") (strWs ")") (sepBy (funcParam opt_or_id) (strWs ","))

  let funcSig<'A>
    (opt_or_id: Parser<TypeAnn, unit> -> Parser<'A, unit>)
    : Parser<FuncSig<'A>, unit> =
    pipe4
      (strWs "fn" >>. (opt typeParams))
      (paramList opt_or_id)
      (opt_or_id (strWs "->" >>. typeAnn))
      (opt (ws .>> strWs "throws" >>. typeAnn))
    <| fun type_params param_list return_type throws ->
      { TypeParams = type_params
        ParamList = param_list
        ReturnType = return_type
        Throws = throws }

  let number: Parser<Literal, unit> = pfloat |>> (string >> Literal.Number)

  let string: Parser<Literal, unit> =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')

    let unescape c =
      match c with
      | 'n' -> "\n"
      | 'r' -> "\r"
      | 't' -> "\t"
      | c -> string c

    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)

    (between
      (pstring "\"")
      (pstring "\"")
      (stringsSepBy normalCharSnippet escapedChar))
    |>> fun sl -> Literal.String(sl)

  let boolean =
    (pstring "true" |>> fun _ -> Literal.Boolean true)
    <|> (pstring "false" |>> fun _ -> Literal.Boolean false)

  litRef.Value <- number <|> string <|> boolean

  let mergeSpans (x: Span) (y: Span) = { Start = x.Start; Stop = y.Stop }

  let opp = OperatorPrecedenceParser<Expr, list<Expr> * Position, unit>()

  let identExpr: Parser<Expr, unit> =
    withSpan ident
    |>> fun (sl, span) ->
      { Kind = Identifier(sl)
        Span = span
        InferredType = None }

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
            { Kind =
                TemplateLiteral(
                  { Parts = List.rev parts
                    Exprs = List.rev exprs }
                )
              Span = { Start = start; Stop = stop }
              InferredType = None }

          Reply(result)
        | ValueSome(value) -> value
      else
        Reply(Error, messageError "Expected '`'")

  let block: Parser<BlockOrExpr, unit> =
    withSpan (between (strWs "{") (strWs "}") (many stmt))
    |>> fun (stmts, span) -> BlockOrExpr.Block({ Span = span; Stmts = stmts })

  let func: Parser<Function, unit> =
    pipe2
      (funcSig opt)
      (block <|> (strWs "=>" >>. expr |>> fun e -> BlockOrExpr.Expr(e)))
    <| fun sig' body -> { Sig = sig'; Body = body }

  let funcExpr: Parser<Expr, unit> =
    withSpan func
    |>> fun (f, span) ->
      { Kind = ExprKind.Function f
        Span = span
        InferredType = None }

  let tupleExpr: Parser<Expr, unit> =
    tuple expr |> withSpan
    |>> fun (exprs, span) ->
      { Kind = ExprKind.Tuple(exprs)
        Span = span
        InferredType = None }

  let ifElse, ifElseRef = createParserForwardedToRef<Expr, unit> ()

  ifElseRef.Value <-
    pipe5
      getPosition
      ((strWs "if") >>. expr)
      block
      (opt (
        strWs "else" >>. ((ifElse |>> (fun e -> BlockOrExpr.Expr(e))) <|> block)
      ))
      getPosition
    <| fun start cond then_ else_ stop ->
      { Kind = ExprKind.IfElse(cond, then_, else_)
        Span = { Start = start; Stop = stop }
        InferredType = None }

  let literalExpr: Parser<Expr, unit> =
    withSpan lit
    |>> fun (lit, span) ->
      { Kind = ExprKind.Literal(lit)
        Span = span
        InferredType = None }

  let atom =
    choice
      [ literalExpr
        funcExpr
        ifElse
        tupleExpr
        templateStringLiteral
        identExpr ]

  let term = (atom .>> ws) <|> between (strWs "(") (strWs ")") expr

  opp.TermParser <- term

  type Assoc = Associativity

  let binary op x y =
    { Expr.Kind = ExprKind.Binary(op, x, y)
      Span = mergeSpans x.Span y.Span
      InferredType = None }

  let after = getPosition .>> ws |>> fun pos -> ([], pos)

  opp.AddOperator(
    PostfixOperator(
      "[",
      (pipe2 getPosition ((ws >>. expr) .>> (strWs "]"))
       <| fun p1 expr -> ([ expr ], p1)), // (indices, position)
      18,
      true,
      (),
      (fun (indices, stop) target ->
        { Expr.Kind = ExprKind.Index(target, indices[0], false)
          Span =
            { Start = target.Span.Start
              Stop = stop }
          InferredType = None })
    )
  )

  opp.AddOperator(
    PostfixOperator(
      "(",
      (pipe2 getPosition (sepBy (ws >>. expr) (strWs ",") .>> (strWs ")"))
       <| fun p1 args -> (args, p1)), // args
      18,
      true,
      (),
      (fun (args, stop) callee ->
        { Expr.Kind =
            ExprKind.Call(
              { Callee = callee
                TypeArgs = None
                Args = args
                OptChain = false
                Throws = None }
            )
          Span =
            { Start = callee.Span.Start
              Stop = stop }
          InferredType = None })
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
        { Expr.Kind = Unary("+", x)
          Span = x.Span
          InferredType = None })
    )
  )

  opp.AddOperator(
    PrefixOperator(
      "-",
      after,
      14,
      true,
      (fun x ->
        { Expr.Kind = Unary("-", x)
          Span = x.Span
          InferredType = None })
    )
  )

  // typeof (14)
  // delete (14)
  // await (14)

  opp.AddOperator(InfixOperator("**", after, 13, Assoc.Right, binary "**"))

  opp.AddOperator(InfixOperator("*", after, 12, Assoc.Left, binary "*"))

  opp.AddOperator(InfixOperator("/", after, 12, Assoc.Left, binary "/"))

  opp.AddOperator(InfixOperator("%", after, 12, Assoc.Left, binary "%"))

  opp.AddOperator(InfixOperator("+", after, 11, Assoc.Left, binary "+"))

  opp.AddOperator(InfixOperator("-", after, 11, Assoc.Left, binary "-"))

  opp.AddOperator(InfixOperator("<", after, 9, Assoc.Left, binary "<"))

  opp.AddOperator(InfixOperator("<=", after, 9, Assoc.Left, binary "<="))

  opp.AddOperator(InfixOperator(">", after, 9, Assoc.Left, binary ">"))

  opp.AddOperator(InfixOperator(">=", after, 9, Assoc.Left, binary ">="))

  opp.AddOperator(InfixOperator("==", after, 8, Assoc.Left, binary "=="))

  opp.AddOperator(InfixOperator("!=", after, 8, Assoc.Left, binary "!="))

  // bitwise and (7)
  // bitwise xor (6)
  // bitwise or (5)

  opp.AddOperator(InfixOperator("&&", after, 4, Assoc.Left, binary "&&"))

  opp.AddOperator(InfixOperator("||", after, 3, Assoc.Left, binary "||"))

  opp.AddOperator(
    InfixOperator(
      "=",
      after,
      2,
      Assoc.Right,
      (fun x y ->
        { Expr.Kind = Assign("=", x, y)
          Span = mergeSpans x.Span y.Span
          InferredType = None })
    )
  )

  exprRef.Value <- opp.ExpressionParser
