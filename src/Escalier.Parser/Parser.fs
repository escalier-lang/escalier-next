namespace Escalier.Parser

open FParsec
open System.Text

open Escalier.Data.Common
open Escalier.Data.Syntax

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

  let otherLiterals =
    (pstring "undefined" |>> fun _ -> Literal.Undefined)
    <|> (pstring "null" |>> fun _ -> Literal.Null)

  litRef.Value <- number <|> string <|> boolean <|> otherLiterals

  let mergeSpans (x: Span) (y: Span) = { Start = x.Start; Stop = y.Stop }

  let opp = OperatorPrecedenceParser<Expr, unit, unit>()

  let identExpr: Parser<Expr, unit> =
    withSpan ident
    |>> fun (sl, span) ->
      { Kind = ExprKind.Identifier(sl)
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
                ExprKind.TemplateLiteral(
                  { Parts = List.rev parts
                    Exprs = List.rev exprs }
                )
              Span = { Start = start; Stop = stop }
              InferredType = None }

          Reply(result)
        | ValueSome(value) -> value
      else
        Reply(Error, messageError "Expected '`'")

  let block: Parser<Block, unit> =
    withSpan (between (strWs "{") (strWs "}") (many stmt))
    |>> fun (stmts, span) -> { Span = span; Stmts = stmts }

  let func: Parser<Function, unit> =
    pipe2
      (funcSig opt)
      (block |>> BlockOrExpr.Block
       <|> (strWs "=>" >>. expr |>> BlockOrExpr.Expr))
    <| fun sig' body -> { Sig = sig'; Body = body }

  let funcExpr: Parser<Expr, unit> =
    withSpan func
    |>> fun (f, span) ->
      { Kind = ExprKind.Function f
        Span = span
        InferredType = None }

  let doExpr: Parser<Expr, unit> =
    withSpan (strWs "do" >>. block)
    |>> fun (body, span) ->
      { Kind = ExprKind.Do(body)
        Span = span
        InferredType = None }

  let tupleExpr: Parser<Expr, unit> =
    tuple expr |> withSpan
    |>> fun (exprs, span) ->
      { Kind = ExprKind.Tuple(exprs)
        Span = span
        InferredType = None }

  let objElemProperty: Parser<ObjElem, unit> =
    pipe4 getPosition ident (opt (strWs ":" >>. expr)) getPosition
    <| fun start key value stop ->
      let span = { Start = start; Stop = stop }

      match value with
      | Some(value) -> ObjElem.Property(span = span, key = key, value = value)
      | None -> ObjElem.Shorthand(span = span, key = key)

  let objElemSpread: Parser<ObjElem, unit> =
    withSpan (strWs "..." >>. expr)
    |>> fun (expr, span) -> ObjElem.Spread(span, expr)

  let objElem = choice [ objElemProperty; objElemSpread ]

  let objectExpr: Parser<Expr, unit> =
    withSpan (between (strWs "{") (strWs "}") (sepBy objElem (strWs ",")))
    |>> fun (objElems, span) ->
      { Kind = ExprKind.Object(objElems)
        Span = span
        InferredType = None }

  let ifElse, ifElseRef = createParserForwardedToRef<Expr, unit> ()

  ifElseRef.Value <-
    pipe5
      getPosition
      ((strWs "if") >>. expr)
      block
      (opt (
        strWs "else"
        >>. ((ifElse |>> (fun e -> BlockOrExpr.Expr(e)))
             <|> (block |>> BlockOrExpr.Block))
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
        doExpr
        ifElse
        tupleExpr
        objectExpr
        templateStringLiteral
        identExpr ]

  let term = (atom .>> ws) <|> between (strWs "(") (strWs ")") expr

  let arrayIndexSuffix: Parser<Expr -> Expr, unit> =
    pipe2 (strWs "[" >>. expr .>> strWs "]") getPosition
    <| fun index p2 target ->
      { Expr.Kind = ExprKind.Index(target, index, false)
        Span = { Start = target.Span.Start; Stop = p2 }
        InferredType = None }

  let callSuffix: Parser<Expr -> Expr, unit> =
    pipe2 (strWs "(" >>. sepBy expr (strWs ",") .>> strWs ")") getPosition
    <| fun args p2 callee ->
      { Expr.Kind =
          ExprKind.Call(
            { Callee = callee
              TypeArgs = None
              Args = args
              OptChain = false
              Throws = None }
          )
        Span = { Start = callee.Span.Start; Stop = p2 }
        InferredType = None }

  let memberOp =
    fun (optChain: bool) (obj: Expr) (prop: Expr) ->
      match prop.Kind with
      | ExprKind.Identifier ident ->
        { Expr.Kind = ExprKind.Member(obj, ident, optChain)
          Span = mergeSpans obj.Span prop.Span
          InferredType = None }
      | _ -> failwith "Expected identifier"

  let memberSuffix: Parser<Expr -> Expr, unit> =
    pipe3 (choice [ strWs "?."; strWs "." ]) identExpr getPosition
    <| fun op prop p2 obj ->
      match op with
      | "?." -> memberOp true obj prop
      | "." -> memberOp false obj prop

  let suffix = choice [ arrayIndexSuffix; callSuffix; memberSuffix ]

  let termWithSuffix: Parser<Expr, unit> =
    pipe2 term (many (suffix))
    <| fun expr suffixes -> List.fold (fun x suffix -> suffix x) expr suffixes

  opp.TermParser <- termWithSuffix

  type Assoc = Associativity

  let binary op x y =
    { Expr.Kind = ExprKind.Binary(op, x, y)
      Span = mergeSpans x.Span y.Span
      InferredType = None }

  // logical not (14)
  // bitwise not (14)

  opp.AddOperator(
    PrefixOperator(
      "+",
      ws,
      14,
      true,
      (fun x ->
        { Expr.Kind = ExprKind.Unary("+", x)
          Span = x.Span
          InferredType = None })
    )
  )

  opp.AddOperator(
    PrefixOperator(
      "-",
      ws,
      14,
      true,
      (fun x ->
        { Expr.Kind = ExprKind.Unary("-", x)
          Span = x.Span
          InferredType = None })
    )
  )

  // typeof (14)
  // delete (14)
  // await (14)

  opp.AddOperator(InfixOperator("**", ws, 13, Assoc.Right, binary "**"))
  opp.AddOperator(InfixOperator("*", ws, 12, Assoc.Left, binary "*"))
  opp.AddOperator(InfixOperator("/", ws, 12, Assoc.Left, binary "/"))
  opp.AddOperator(InfixOperator("%", ws, 12, Assoc.Left, binary "%"))
  opp.AddOperator(InfixOperator("+", ws, 11, Assoc.Left, binary "+"))
  opp.AddOperator(InfixOperator("-", ws, 11, Assoc.Left, binary "-"))
  opp.AddOperator(InfixOperator("<", ws, 9, Assoc.Left, binary "<"))
  opp.AddOperator(InfixOperator("<=", ws, 9, Assoc.Left, binary "<="))
  opp.AddOperator(InfixOperator(">", ws, 9, Assoc.Left, binary ">"))
  opp.AddOperator(InfixOperator(">=", ws, 9, Assoc.Left, binary ">="))
  opp.AddOperator(InfixOperator("==", ws, 8, Assoc.Left, binary "=="))
  opp.AddOperator(InfixOperator("!=", ws, 8, Assoc.Left, binary "!="))

  // bitwise and (7)
  // bitwise xor (6)
  // bitwise or (5)

  opp.AddOperator(InfixOperator("&&", ws, 4, Assoc.Left, binary "&&"))
  opp.AddOperator(InfixOperator("||", ws, 3, Assoc.Left, binary "||"))

  opp.AddOperator(
    InfixOperator(
      "=",
      ws,
      2,
      Assoc.Right,
      (fun x y ->
        { Expr.Kind = ExprKind.Assign("=", x, y)
          Span = mergeSpans x.Span y.Span
          InferredType = None })
    )
  )

  exprRef.Value <- opp.ExpressionParser

  let private exprStmt: Parser<Stmt, unit> =
    withSpan expr |>> fun (e, span) -> { Stmt.Kind = Expr(e); Span = span }

  let private returnStmt: Parser<Stmt, unit> =
    withSpan (strWs "return" >>. opt expr)
    |>> fun (e, span) -> { Stmt.Kind = Return(e); Span = span }

  // `let <expr> = <expr>`
  let private varDecl =
    pipe5
      getPosition
      (strWs "let" >>. pattern)
      (opt (strWs ":" >>. ws >>. typeAnn))
      (strWs "=" >>. expr)
      getPosition
    <| fun start pat typeAnn init stop ->
      let span = { Start = start; Stop = stop }

      { Stmt.Kind =
          Decl(
            { Kind = DeclKind.VarDecl(pat, init, typeAnn)
              Span = span }
          )
        Span = span }

  // TODO: parse type params
  let private typeDecl =
    pipe5
      getPosition
      (strWs "type" >>. ident)
      (opt (between (strWs "<") (strWs ">") (sepBy typeParam (strWs ","))))
      (strWs "=" >>. typeAnn)
      getPosition
    <| fun start id typeParams typeAnn stop ->
      let span = { Start = start; Stop = stop }

      { Stmt.Kind =
          Decl(
            { Kind = TypeDecl(id, typeAnn, typeParams)
              Span = span }
          )
        Span = span }

  // TODO: Parse for-loops
  stmtRef.Value <- ws >>. choice [ varDecl; typeDecl; returnStmt; exprStmt ]


  let private identPattern =
    withSpan ident
    |>> fun (id, span) ->
      { Pattern.Kind =
          PatternKind.Identifier(
            { Name = id
              Span = span
              IsMut = false }
          )
        Span = span
        InferredType = None }

  let private literalPattern =
    withSpan lit
    |>> fun (lit, span) ->
      { Pattern.Kind = PatternKind.Literal(span = span, value = lit)
        Span = span
        InferredType = None }

  let private tuplePattern =
    tuple pattern |> withSpan
    |>> fun (patterns, span) ->
      { Pattern.Kind = PatternKind.Tuple(patterns)
        Span = span
        InferredType = None }

  let private wildcardPattern =
    withSpan (strWs "_")
    |>> fun (_, span) ->
      { Pattern.Kind = PatternKind.Wildcard
        Span = span
        InferredType = None }

  let private objPatKeyValueOrShorthand =
    pipe4 getPosition ident (opt (strWs ":" >>. (ws >>. pattern))) getPosition
    <| fun start key value stop ->
      let span = { Start = start; Stop = stop }

      match value with
      | Some(value) ->
        KeyValuePat(span = span, key = key, value = value, init = None)
      | None ->
        ShorthandPat(span = span, name = key, init = None, isMut = false)

  let private objPatRestElem =
    withSpan (strWs "..." >>. pattern)
    |>> fun (pattern, span) ->
      RestPat(span = span, target = pattern, isMut = false)

  let private objPatElem = choice [ objPatKeyValueOrShorthand; objPatRestElem ]

  let private objectPattern =
    withSpan (between (strWs "{") (strWs "}") (sepBy objPatElem (strWs ",")))
    |>> fun (objElems, span) ->
      { Pattern.Kind = PatternKind.Object(objElems)
        Span = span
        InferredType = None }

  patternRef.Value <-
    choice
      [ identPattern
        literalPattern
        wildcardPattern
        objectPattern
        tuplePattern ]

  let private parenthesizedTypeAnn = (between (strWs "(") (strWs ")") typeAnn)

  let private litTypeAnn =
    withSpan lit
    |>> fun (lit, span) ->
      { TypeAnn.Kind = TypeAnnKind.Literal(lit)
        Span = span
        InferredType = None }

  let private keywordTypeAnn =

    let keyword =
      choice
        [ (strWs "object" |>> fun _ -> KeywordTypeAnn.Object)
          (strWs "never" |>> fun _ -> Never)
          (strWs "unknown" |>> fun _ -> Unknown)
          (strWs "boolean" |>> fun _ -> Boolean)
          (strWs "number" |>> fun _ -> Number)
          (strWs "string" |>> fun _ -> String)
          (strWs "symbol" |>> fun _ -> Symbol)
          (strWs "null" |>> fun _ -> Null)
          (strWs "undefined" |>> fun _ -> Undefined) ]

    withSpan keyword
    |>> fun (keyword, span) ->
      { TypeAnn.Kind = TypeAnnKind.Keyword(keyword)
        Span = span
        InferredType = None }

  let private tupleTypeAnn =
    tuple typeAnn |> withSpan
    |>> fun (typeAnns, span) ->
      { TypeAnn.Kind = TypeAnnKind.Tuple(typeAnns)
        Span = span
        InferredType = None }

  let private funcTypeAnn =
    funcSig id |> withSpan
    |>> fun (f, span) ->
      { TypeAnn.Kind = TypeAnnKind.Function(f)
        Span = span
        InferredType = None }

  let private objTypeAnnKeyValue: Parser<ObjTypeAnnElem, unit> =
    pipe5
      getPosition
      ident
      (opt (strWs "?"))
      (strWs ":" >>. typeAnn)
      getPosition
    <| fun p1 name optional typeAnn p2 ->
      // TODO: add location information
      let span = { Start = p1; Stop = p2 }

      ObjTypeAnnElem.Property
        { Name = name
          TypeAnn = typeAnn
          Optional = optional.IsSome
          Readonly = false }

  let private objTypeAnnElem = choice [ objTypeAnnKeyValue ]

  let private objectTypeAnn =
    withSpan (
      between (strWs "{") (strWs "}") (sepBy objTypeAnnElem (strWs ","))
    )
    |>> fun (objElems, span) ->
      { TypeAnn.Kind = TypeAnnKind.Object(objElems)
        Span = span
        InferredType = None }

  // TODO: don't include strWs in the span
  let private keyofTypeAnn =
    withSpan (strWs "keyof" >>. typeAnn)
    |>> fun (typeAnn, span) ->
      { TypeAnn.Kind = TypeAnnKind.Keyof(typeAnn)
        Span = span
        InferredType = None }

  // TODO: don't include strWs in the span
  let private restTypeAnn =
    withSpan (strWs "..." >>. typeAnn)
    |>> fun (typeAnn, span) ->
      { TypeAnn.Kind = TypeAnnKind.Rest(typeAnn)
        Span = span
        InferredType = None }

  // TODO: don't include strWs in the span
  let private typeofTypeAnn =
    withSpan (strWs "typeof" >>. expr)
    |>> fun (e, span) ->
      { TypeAnn.Kind = TypeAnnKind.Typeof(e)
        Span = span
        InferredType = None }

  let private typeRef =
    pipe4
      getPosition
      ident
      (opt (between (strWs "<") (strWs ">") (sepBy typeAnn (strWs ","))))
      getPosition
    <| fun start name typeArgs stop ->
      { TypeAnn.Kind = TypeAnnKind.TypeRef(name, typeArgs)
        Span = { Start = start; Stop = stop }
        InferredType = None }

  let primaryType =
    choice
      [ litTypeAnn
        parenthesizedTypeAnn
        keywordTypeAnn // aka PredefinedType
        tupleTypeAnn
        funcTypeAnn
        typeofTypeAnn // aka TypeQuery
        keyofTypeAnn
        restTypeAnn
        objectTypeAnn
        // TODO: thisTypeAnn
        // NOTE: should come last since any identifier can be a type reference
        typeRef ]
    .>> ws

  let arrayTypeSuffix: Parser<TypeAnn -> TypeAnn, unit> =
    pipe2 (strWs "[" >>. strWs "]") getPosition
    <| fun _ p2 target ->
      { TypeAnn.Kind = Array(target)
        Span = { Start = target.Span.Start; Stop = p2 }
        InferredType = None }

  let primaryTypeWithSuffix: Parser<TypeAnn, unit> =
    pipe2 primaryType (many (arrayTypeSuffix))
    <| fun typeAnn suffixes ->
      List.fold (fun x suffix -> suffix x) typeAnn suffixes

  // NOTE: We don't use InfixOperator here because that only supports
  // binary operators and intersection types are n-ary.
  let intersectionOrPrimaryType: Parser<TypeAnn, unit> =
    withSpan (sepBy1 primaryTypeWithSuffix (strWs "&"))
    |>> fun (typeAnns, span) ->
      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | _ ->
        { TypeAnn.Kind = TypeAnnKind.Intersection(typeAnns)
          Span = span
          InferredType = None }

  // NOTE: We don't use InfixOperator here because that only supports
  // binary operators and union types are n-ary.
  let unionOrIntersectionOrPrimaryType: Parser<TypeAnn, unit> =
    withSpan (sepBy1 intersectionOrPrimaryType (strWs "|"))
    |>> fun (typeAnns, span) ->
      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | _ ->
        { TypeAnn.Kind = TypeAnnKind.Union(typeAnns)
          Span = span
          InferredType = None }

  typeAnnRef.Value <- unionOrIntersectionOrPrimaryType

  // Public Exports
  let parseExpr (input: string) : Result<Expr, ParserError> =
    match run expr input with
    | ParserResult.Success(result, _, _) -> Result.Ok(result)
    | ParserResult.Failure(_, error, _) -> Result.Error(error)

  let parseScript (input: string) =
    match run (many stmt) input with
    | ParserResult.Success(result, _, _) ->
      let dummySpan =
        { Start = Position("", 0, 0, 0)
          Stop = Position("", 0, 0, 0) }

      let block = { Stmts = result; Span = dummySpan }

      Result.Ok(block)
    | ParserResult.Failure(_, error, _) -> Result.Error(error)
