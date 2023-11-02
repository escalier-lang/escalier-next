namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax
open System.Text

exception ParseError of string

module ExprParser =
  let ws = spaces
  let str_ws s = pstring s .>> ws

  let mergeSpans (x: Span) (y: Span) = { start = x.start; stop = y.stop }

  let expr, exprRef = createParserForwardedToRef<Expr, unit> ()
  let stmt, stmtRef = createParserForwardedToRef<Stmt, unit> ()
  let pattern, patternRef = createParserForwardedToRef<Pattern, unit> ()
  let typeAnn, typeAnnRef = createParserForwardedToRef<TypeAnn, unit> ()

  let opp = new OperatorPrecedenceParser<Expr, list<Expr> * Position, unit>()

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

  let funcParam = ident //  .>> (str_ws ":" >>. ident)

  let func: Parser<Expr, unit> =
    pipe5
      getPosition
      (str_ws "fn")
      (between (str_ws "(") (str_ws ")") (sepBy funcParam (str_ws ",")))
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
      (fun (indices, pos) target ->
        { Expr.kind = ExprKind.Index(target, indices[0], false)
          span =
            { start = target.span.start
              stop = pos.Index |> int }
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
      (fun (args, pos) callee ->
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
              stop = pos.Index |> int } // TODO
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

  exprRef.Value <- opp.ExpressionParser

  let private exprStmt: Parser<Stmt, unit> =
    pipe3 getPosition expr getPosition
    <| fun p1 e p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { Stmt.kind = Expr(e)
        span = { start = start; stop = stop } }

  let private returnStmt: Parser<Stmt, unit> =
    pipe3 getPosition ((str_ws "return") >>. opt (expr)) getPosition
    <| fun p1 e p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { Stmt.kind = Return(e)
        span = { start = start; stop = stop } }

  // `let <expr> = <expr>`
  let private varDecl =
    pipe4
      getPosition
      (str_ws "let" >>. pattern)
      (str_ws "=" >>. expr)
      getPosition
    <| fun p1 pat init p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int
      let span = { start = start; stop = stop }

      { Stmt.kind =
          Decl(
            { kind = VarDecl(pat, Some(init), None, false)
              span = span }
          )
        span = span }

  // TODO: parse type params
  let private typeDecl =
    pipe4
      getPosition
      (str_ws "type" >>. ident)
      (str_ws "=" >>. typeAnn)
      getPosition
    <| fun p1 id typeAnn p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int
      let span = { start = start; stop = stop }

      { Stmt.kind =
          Decl(
            { kind = TypeDecl(id, typeAnn, None)
              span = span }
          )
        span = span }

  stmtRef.Value <- choice [ varDecl; typeDecl; returnStmt; exprStmt ]

  let private identPattern =
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

  let private literalPattern =
    pipe3 getPosition literal getPosition
    <| fun p1 lit p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int
      let span = { start = start; stop = stop }

      { Pattern.kind = PatternKind.Literal(span = span, value = lit)
        span = { start = start; stop = stop }
        inferred_type = None }

  let private tuplePattern =
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

  let private wildcardPattern =
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

  let private objectPattern =
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

  patternRef.Value <-
    choice
      [ identPattern
        literalPattern
        wildcardPattern
        objectPattern
        tuplePattern ]

  let private parenthesizedTypeAnn = (between (str_ws "(") (str_ws ")") typeAnn)

  let private keywordTypeAnn =

    let keyword =
      choice
        [ (str_ws "object" |>> fun _ -> KeywordType.Object)
          (str_ws "never" |>> fun _ -> Never)
          (str_ws "unknown" |>> fun _ -> Unknown)
          (str_ws "boolean" |>> fun _ -> Boolean)
          (str_ws "number" |>> fun _ -> Number)
          (str_ws "string" |>> fun _ -> String)
          (str_ws "symbol" |>> fun _ -> Symbol)
          (str_ws "null" |>> fun _ -> Null)
          (str_ws "undefined" |>> fun _ -> Undefined) ]

    pipe3 getPosition keyword getPosition
    <| fun p1 keyword p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { TypeAnn.kind = TypeAnnKind.Keyword(keyword)
        span = { start = start; stop = stop }
        inferred_type = None }

  let private tupleTypeAnn =
    pipe3
      getPosition
      (between (str_ws "[") (str_ws "]") (sepBy typeAnn (str_ws ",")))
      getPosition
    <| fun p1 typeAnns p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { TypeAnn.kind = TypeAnnKind.Tuple(typeAnns)
        span = { start = start; stop = stop }
        inferred_type = None }

  let private keyofTypeAnn =
    pipe3 getPosition (str_ws "keyof" >>. typeAnn) getPosition
    <| fun p1 target p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { TypeAnn.kind = TypeAnnKind.Keyof(target)
        span = { start = start; stop = stop }
        inferred_type = None }

  let private restTypeAnn =
    pipe3 getPosition (str_ws "..." >>. typeAnn) getPosition
    <| fun p1 target p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { TypeAnn.kind = TypeAnnKind.Rest(target)
        span = { start = start; stop = stop }
        inferred_type = None }

  let private typeofTypeAnn =
    pipe3 getPosition (str_ws "typeof" >>. expr) getPosition
    <| fun p1 target p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { TypeAnn.kind = TypeAnnKind.Typeof(target)
        span = { start = start; stop = stop }
        inferred_type = None }

  let private typeRef =
    pipe4
      getPosition
      ident
      (opt (between (str_ws "<") (str_ws ">") (sepBy typeAnn (str_ws ","))))
      getPosition
    <| fun p1 name typeArgs p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      { TypeAnn.kind = TypeAnnKind.TypeRef(name, typeArgs)
        span = { start = start; stop = stop }
        inferred_type = None }

  let opp' = new OperatorPrecedenceParser<TypeAnn, Position, unit>()

  let primaryType = opp'.ExpressionParser

  // NOTE(kevinb): We use an operator precedence parser to workaround the
  // fact that `[]` is left recursive.
  opp'.AddOperator(
    PostfixOperator(
      "[]",
      getPosition .>> ws,
      1,
      true,
      (),
      (fun pos target ->
        { TypeAnn.kind = Array(target)
          span =
            { start = target.span.start
              stop = pos.Index |> int }
          inferred_type = None })
    )
  )

  opp'.TermParser <-
    choice
      [ parenthesizedTypeAnn
        keywordTypeAnn // aka PredefinedType
        // objectTypeAnn
        tupleTypeAnn
        typeofTypeAnn // aka TypeQuery
        keyofTypeAnn
        restTypeAnn
        // thisTypeAnn
        // NOTE: should come last since any identifier can be a type reference
        typeRef ]

  // NOTE: We don't use InfixOperator here because that only supports
  // binary operators and intersection types are n-ary.
  let intersectionOrPrimaryType: Parser<TypeAnn, unit> =
    pipe3 getPosition (sepBy1 primaryType (str_ws "&")) getPosition
    <| fun p1 typeAnns p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | _ ->
        { TypeAnn.kind = TypeAnnKind.Intersection(typeAnns)
          span = { start = start; stop = stop }
          inferred_type = None }

  // NOTE: We don't use InfixOperator here because that only supports
  // binary operators and union types are n-ary.
  let unionOrIntersectionOrPrimaryType: Parser<TypeAnn, unit> =
    pipe3
      getPosition
      (sepBy1 intersectionOrPrimaryType (str_ws "|"))
      getPosition
    <| fun p1 typeAnns p2 ->
      let start = p1.Index |> int
      let stop = p2.Index |> int

      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | _ ->
        { TypeAnn.kind = TypeAnnKind.Union(typeAnns)
          span = { start = start; stop = stop }
          inferred_type = None }

  // TODO: handle function types
  typeAnnRef.Value <- unionOrIntersectionOrPrimaryType
