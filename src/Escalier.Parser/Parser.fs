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

  // TODO: provide a way to control wehther default values for params are allowed
  // opt_or_id controls whether the type annotation is optional or not
  let funcSig<'A>
    (opt_or_id: Parser<TypeAnn, unit> -> Parser<'A, unit>)
    : Parser<FuncSig<'A>, unit> =
    pipe5
      (opt (strWs "async"))
      (strWs "fn" >>. (opt typeParams))
      (paramList opt_or_id)
      (opt_or_id (strWs "->" >>. typeAnn))
      (opt (ws .>> strWs "throws" >>. typeAnn))
    <| fun async type_params param_list return_type throws ->
      { TypeParams = type_params
        ParamList = param_list
        ReturnType = return_type
        Throws = throws
        IsAsync = async.IsSome }

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

  // let number: Parser<Literal, unit> = pfloat |>> Literal.Number

  let _string: Parser<string, unit> =
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

  let string: Parser<Literal, unit> = _string |>> fun sl -> Literal.String(sl)

  let boolean =
    (pstring "true" |>> fun _ -> Literal.Boolean true)
    <|> (pstring "false" |>> fun _ -> Literal.Boolean false)

  let otherLiterals =
    (pstring "undefined" |>> fun _ -> Literal.Undefined)
    <|> (pstring "null" |>> fun _ -> Literal.Null)

  litRef.Value <-
    choice [ number |>> Literal.Number; string; boolean; otherLiterals ]

  let mergeSpans (x: Span) (y: Span) = { Start = x.Start; Stop = y.Stop }

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
    <| fun start name value stop ->
      let span = { Start = start; Stop = stop }

      // TODO: handle parsing computed properties
      match value with
      | Some(value) ->
        ObjElem.Property(
          span = span,
          name = PropName.String name,
          value = value
        )
      | None -> ObjElem.Shorthand(span = span, name = name)

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

  let matchCase: Parser<MatchCase, unit> =
    pipe5
      getPosition
      (strWs "|" >>. pattern)
      (opt ((strWs "if") >>. expr))
      (strWs "=>"
       >>. ((block |>> BlockOrExpr.Block) <|> (expr |>> BlockOrExpr.Expr)))
      getPosition
    <| fun start pattern guard body stop ->

      { Pattern = pattern
        Guard = guard
        Body = body
        Span = { Start = start; Stop = stop } }

  let matchExpr: Parser<Expr, unit> =
    pipe4
      getPosition
      (strWs "match" >>. expr)
      (between (strWs "{") (strWs "}") (many matchCase))
      getPosition
    <| fun start expr cases stop ->
      { Kind = ExprKind.Match(expr, cases)
        Span = { Start = start; Stop = stop }
        InferredType = None }

  let catchClause: Parser<list<MatchCase>, unit> =
    pipe3
      getPosition
      (between (strWs "{") (strWs "}") (many matchCase))
      getPosition
    <| fun start cases stop -> cases

  let throwExpr: Parser<Expr, unit> =
    withSpan (strWs "throw" >>. expr)
    |>> fun (expr, span) ->
      { Kind = ExprKind.Throw(expr)
        Span = span
        InferredType = None }

  let tryExpr: Parser<Expr, unit> =
    pipe5
      getPosition
      (strWs "try" >>. block)
      (opt (strWs "catch" >>. catchClause))
      (opt (strWs "finally" >>. block))
      getPosition
    <| fun start body cases finally_ stop ->
      { Kind =
          ExprKind.Try
            { Body = body
              Catch = cases
              Finally = finally_
              Throws = None }
        Span = { Start = start; Stop = stop }
        InferredType = None }

  let atom =
    choice
      [ literalExpr
        attempt funcExpr // conflicts with identExpr
        attempt doExpr // conflicts with identExpr, e.g. 'double' starts with 'do'
        attempt ifElse // conflicts with identExpr
        attempt throwExpr // conflicts with identExpr
        attempt tryExpr // conflicts with identExpr
        attempt matchExpr // conflicts with identExpr
        tupleExpr
        objectExpr
        templateStringLiteral
        // TODO: have a combinator that wraps `identExpr` and checks if the
        // ident if `fn`, `do`, `if`, `throw`, `try`, match`, etc. and then
        // continue parsing the appropriate expression
        identExpr ]

  let term = atom .>> ws
  let exprParser = Pratt.PrattParser<Expr>(term)

  let binary op x y =
    { Expr.Kind = ExprKind.Binary(op, x, y)
      Span = mergeSpans x.Span y.Span
      InferredType = None }

  let prefixExprParlset
    (precedence: int)
    (callback: Expr -> Expr)
    : Pratt.PrefixParselet<Expr> =
    { Parse =
        fun (parser, stream, operator) ->
          let precedence = parser.GetPrecedence(operator)
          let operand = parser.Parse precedence stream

          match operand.Status with
          | Ok -> Reply(callback operand.Result)
          | _ -> operand
      Precedence = precedence }

  let unaryExprParslet
    (precedence: int)
    (operator: string)
    : Pratt.PrefixParselet<Expr> =
    prefixExprParlset precedence (fun operand ->
      { Expr.Kind = ExprKind.Unary(operator, operand)
        Span = operand.Span
        InferredType = None })

  let infixExprParselet
    (precedence: int)
    (callback: Expr -> Expr -> Expr)
    : Pratt.InfixParselet<Expr> =
    { Parse =
        fun (parser, stream, left, operator) ->
          let precedence = parser.GetPrecedence(operator)
          let right = parser.Parse precedence stream

          match right.Status with
          | Ok -> Reply(callback left right.Result)
          | _ -> right
      Precedence = precedence }

  let binaryExprParselet
    (precedence: int)
    (operator: string)
    : Pratt.InfixParselet<Expr> =
    infixExprParselet precedence (fun left right ->
      { Expr.Kind = ExprKind.Binary(operator, left, right)
        Span = mergeSpans left.Span right.Span
        InferredType = None })

  let groupingExprParselet (precedence: int) : Pratt.PrefixParselet<'T> =
    { Parse =
        fun (parser, stream, operator) -> (parser.Parse 0 .>> strWs ")") stream
      Precedence = precedence }

  exprParser.RegisterPrefix("(", groupingExprParselet 18)

  let indexParselet (precedence: int) : Pratt.InfixParselet<Expr> =
    { Parse =
        fun (parser, stream, left, operator) ->
          let index = parser.Parse(0)
          let reply = (index .>> (strWs "]")) stream

          match reply.Status with
          | Ok ->
            Reply(
              { Expr.Kind = ExprKind.Index(left, reply.Result, false)
                Span =
                  { Start = left.Span.Start
                    Stop = stream.Position }
                InferredType = None }
            )
          | _ -> Reply(reply.Status, reply.Error)
      Precedence = precedence }

  let memberOp =
    fun (optChain: bool) (obj: Expr) (prop: Expr) ->
      match prop.Kind with
      | ExprKind.Identifier ident ->
        { Expr.Kind = ExprKind.Member(obj, ident, optChain)
          Span = mergeSpans obj.Span prop.Span
          InferredType = None }
      | _ -> failwith "Expected identifier"

  exprParser.RegisterInfix(
    "?.",
    infixExprParselet 17 (fun left right -> memberOp true left right)
  )

  exprParser.RegisterInfix(
    ".",
    infixExprParselet 17 (fun left right -> memberOp false left right)
  )

  exprParser.RegisterInfix("[", indexParselet 17)

  let callParselet (precedence: int) : Pratt.InfixParselet<Expr> =
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
          | Ok ->
            Reply(
              { Expr.Kind =
                  ExprKind.Call
                    { Callee = left
                      TypeArgs = None
                      Args = args.Result
                      OptChain = false
                      Throws = None }
                Span =
                  { Start = left.Span.Start
                    Stop = stream.Position }
                InferredType = None }
            )

          | _ -> Reply(args.Status, args.Error)
      Precedence = precedence }

  exprParser.RegisterInfix("(", callParselet 17)

  // logical not (14)
  // bitwise not (14)

  exprParser.RegisterPrefix("+", unaryExprParslet 14 "+")
  exprParser.RegisterPrefix("-", unaryExprParslet 14 "-")

  exprParser.RegisterPrefix(
    "await",
    prefixExprParlset 14 (fun x ->
      { Expr.Kind = ExprKind.Await({ Value = x; Throws = None })
        Span = x.Span
        InferredType = None })
  )

  // typeof (14)
  // delete (14)
  // await (14)

  // TODO: add support for right associativity
  exprParser.RegisterInfix("**", binaryExprParselet 13 "**")
  exprParser.RegisterInfix("*", binaryExprParselet 12 "*")
  exprParser.RegisterInfix("/", binaryExprParselet 12 "/")
  exprParser.RegisterInfix("%", binaryExprParselet 12 "%")
  exprParser.RegisterInfix("+", binaryExprParselet 11 "+")
  exprParser.RegisterInfix("++", binaryExprParselet 11 "++")
  exprParser.RegisterInfix("-", binaryExprParselet 11 "-")
  exprParser.RegisterInfix("<", binaryExprParselet 9 "<")
  exprParser.RegisterInfix("<=", binaryExprParselet 9 "<=")
  exprParser.RegisterInfix(">", binaryExprParselet 9 ">")
  exprParser.RegisterInfix(">=", binaryExprParselet 9 ">=")
  exprParser.RegisterInfix("==", binaryExprParselet 8 "==")
  exprParser.RegisterInfix("!=", binaryExprParselet 8 "!=")

  // bitwise and (7)
  // bitwise xor (6)
  // bitwise or (5)

  exprParser.RegisterInfix("&&", binaryExprParselet 4 "&&")
  exprParser.RegisterInfix("||", binaryExprParselet 3 "||")

  exprRef.Value <- exprParser.Parse(0)

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

  let private forLoop =
    pipe5
      getPosition
      (strWs "for" >>. pattern)
      (strWs "in" >>. expr)
      (ws >>. block)
      getPosition
    <| fun start pattern expr body stop ->
      { Stmt.Kind = For(pattern, expr, body)
        Span = { Start = start; Stop = stop } }

  let _stmt =
    choice
      [ varDecl
        typeDecl
        returnStmt
        forLoop

        // exprStmt must come last because it can recognize any alphanumeric
        // sequence as an identifier which is a valid expression
        exprStmt ]

  stmtRef.Value <- ws >>. _stmt

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
    withSpan (lit .>> ws)
    |>> fun (lit, span) ->
      printfn "parsed literal: %A" lit

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
    <| fun start name value stop ->
      let span = { Start = start; Stop = stop }

      match value with
      | Some(value) ->
        KeyValuePat(span = span, key = name, value = value, init = None)
      | None ->
        ShorthandPat(span = span, name = name, init = None, isMut = false)

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

  let private litTypeAnn =
    withSpan lit
    |>> fun (lit, span) ->
      { TypeAnn.Kind = TypeAnnKind.Literal(lit)
        Span = span
        InferredType = None }

  let private keywordTypeAnn =
    let mutable unique = false

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

  // unique symbols are similar to schemes.  The type annotation is like
  // the scheme and when we infer the type annotation, we instantiate it
  // and create an id for it at that time.
  // in particular, when calling `new Symbol()` we need to create a new id
  // at that point in time.  maybe all `unique symbol`s that appear in a
  // function signature should get their own type variable (or whatever the
  // symbol equivalent of that is)
  let private uniqueSymbolTypeAnn =
    withSpan (strWs "unique" >>. strWs "symbol")
    |>> fun (_, span) ->
      { TypeAnn.Kind = TypeAnnKind.Keyword KeywordTypeAnn.UniqueSymbol
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

  let private propName =
    choice
      [ ident |>> PropName.String
        number .>> ws |>> PropName.Number
        _string .>> ws |>> PropName.String
        between (strWs "[") (strWs "]") expr |>> PropName.Computed ]

  let private propertyTypeAnn =
    pipe5
      getPosition
      propName
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

  let private readonlyModifier =
    pipe2 (opt (strWs "+" <|> strWs "-")) (strWs "readonly")
    <| fun pm _ ->
      match pm with
      | Some("+") -> MappedModifier.Add
      | Some("-") -> MappedModifier.Remove
      | _ -> MappedModifier.Add

  let private optionalModifier =
    pipe2 (opt (strWs "+" <|> strWs "-")) (strWs "?")
    <| fun pm _ ->
      match pm with
      | Some("+") -> MappedModifier.Add
      | Some("-") -> MappedModifier.Remove
      | _ -> MappedModifier.Add

  let private mappedTypeParam =
    pipe2 (strWs "for" >>. ident) (strWs "in" >>. typeAnn)
    <| fun name c -> { Name = name; Constraint = c }

  let private mappedTypeAnn =
    pipe5
      (opt readonlyModifier)
      (between (strWs "[") (strWs "]") typeAnn)
      (opt optionalModifier)
      (strWs ":" >>. typeAnn)
      mappedTypeParam
    <| fun readonly name optional typeAnn typeParam ->

      let name =
        match name.Kind with
        | TypeAnnKind.TypeRef(name, _) when name = typeParam.Name -> None
        | _ -> Some(name)

      ObjTypeAnnElem.Mapped
        { TypeParam = typeParam
          Name = name
          TypeAnn = typeAnn
          Readonly = readonly
          Optional = optional }

  let private callableSignature =
    pipe4 getPosition (opt (strWs "new")) (funcSig id) getPosition
    <| fun start newable funcSig stop ->
      match newable with
      | Some _ -> ObjTypeAnnElem.Constructor(funcSig)
      | None -> ObjTypeAnnElem.Callable(funcSig)

  let private objTypeAnnElem =
    choice
      [ attempt callableSignature
        // mappedTypeAnn must come before propertyTypeAnn because computed
        // properties conflicts with mapped types
        attempt mappedTypeAnn
        attempt propertyTypeAnn ]

  let private objectTypeAnn =
    withSpan (
      between (strWs "{") (strWs "}") (sepEndBy objTypeAnnElem (strWs ","))
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

  let condTypeAnn, condTypeAnnRef = createParserForwardedToRef<TypeAnn, unit> ()

  // TODO: add support for chaining conditional types
  condTypeAnnRef.Value <-
    pipe5
      getPosition
      (strWs "if"
       >>. (pipe2 typeAnn (strWs ":" >>. typeAnn)
            <| fun check extends -> (check, extends)))
      (strWs "{" >>. typeAnn .>> strWs "}")
      (strWs "else" >>. (condTypeAnn <|> (strWs "{" >>. typeAnn .>> strWs "}")))
      getPosition
    <| fun start (check, extends) trueType falseType stop ->
      { TypeAnn.Kind =
          TypeAnnKind.Condition
            { Check = check
              Extends = extends
              TrueType = trueType
              FalseType = falseType }
        Span = { Start = start; Stop = stop }
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
        keywordTypeAnn // aka PredefinedType
        uniqueSymbolTypeAnn
        tupleTypeAnn
        funcTypeAnn
        typeofTypeAnn // aka TypeQuery
        keyofTypeAnn
        restTypeAnn
        objectTypeAnn
        condTypeAnn
        // TODO: thisTypeAnn
        // NOTE: should come last since any identifier can be a type reference
        typeRef ]
    .>> ws

  let typeAnnParser = Pratt.PrattParser<TypeAnn>(primaryType)

  let groupingTypeAnnParselet
    (precedence: int)
    : Pratt.PrefixParselet<TypeAnn> =
    { Parse =
        fun (parser, stream, operator) ->
          (typeAnnParser.Parse 0 .>> strWs ")") stream
      Precedence = precedence }

  typeAnnParser.RegisterPrefix("(", groupingTypeAnnParselet 18)

  let indexTypeAnnParselet (precedence: int) : Pratt.InfixParselet<TypeAnn> =
    { Parse =
        fun (parser, stream, left, operator) ->
          let index = parser.Parse 0
          let reply = (index .>> (strWs "]")) stream

          match reply.Status with
          | Ok ->
            Reply(
              { TypeAnn.Kind = TypeAnnKind.Index(left, reply.Result)
                Span =
                  { Start = left.Span.Start
                    Stop = stream.Position }
                InferredType = None }
            )
          | _ -> Reply(reply.Status, reply.Error)
      Precedence = precedence }

  typeAnnParser.RegisterInfix("[", indexTypeAnnParselet 17)

  let postfixTypeAnnParselet
    (precedence: int)
    (callback: TypeAnn * Position -> TypeAnn)
    : Pratt.InfixParselet<TypeAnn> =
    { Parse =
        fun (parser, stream, left, operator) ->
          Reply(callback (left, stream.Position))
      Precedence = precedence }

  typeAnnParser.RegisterInfix(
    "[]",
    postfixTypeAnnParselet 17 (fun (target, position) ->
      { TypeAnn.Kind = Array(target)
        Span =
          { Start = target.Span.Start
            Stop = position }
        InferredType = None })
  )

  let naryTypeAnnParselet
    (precedence: int)
    (callback: list<TypeAnn> -> TypeAnn)
    : Pratt.InfixParselet<TypeAnn> =
    { Parse =
        fun (parser, stream, left, operator) ->
          let right = parser.Parse precedence stream

          let rec led (acc: list<TypeAnn>) (left: Reply<TypeAnn>) =
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

          Reply(callback (List.rev operands))
      Precedence = precedence }

  typeAnnParser.RegisterInfix(
    "&",
    naryTypeAnnParselet 4 (fun typeAnns ->
      let first = typeAnns[0]
      let last = typeAnns[typeAnns.Length - 1]

      { TypeAnn.Kind = TypeAnnKind.Intersection(typeAnns)
        Span = mergeSpans first.Span last.Span
        InferredType = None })
  )

  typeAnnParser.RegisterInfix(
    "|",
    naryTypeAnnParselet 3 (fun typeAnns ->
      let first = typeAnns[0]
      let last = typeAnns[typeAnns.Length - 1]

      { TypeAnn.Kind = TypeAnnKind.Union(typeAnns)
        Span = mergeSpans first.Span last.Span
        InferredType = None })
  )

  typeAnnRef.Value <- typeAnnParser.Parse(0)

  let namedSpecifier: Parser<ImportSpecifier, unit> =
    pipe4 getPosition ident (opt (strWs "as" >>. ident)) getPosition
    <| fun start name alias stop ->
      let span = { Start = start; Stop = stop }
      ImportSpecifier.Named(name, alias)

  let private namedSpecifiers: Parser<list<ImportSpecifier>, unit> =
    between (strWs "{") (strWs "}") (sepBy namedSpecifier (strWs ","))

  let private moduleAlias: Parser<list<ImportSpecifier>, unit> =
    pipe3 getPosition (strWs "as" >>. ident) getPosition
    <| fun start alias stop ->
      let span = { Start = start; Stop = stop }
      [ ImportSpecifier.ModuleAlias alias ]

  let private importSpecifiers = choice [ namedSpecifiers; moduleAlias ]

  let import: Parser<Import, unit> =
    pipe4
      getPosition
      (strWs "import" >>. _string .>> ws)
      (opt importSpecifiers)
      getPosition
    <| fun start source specifiers stop ->
      { Path = source
        Specifiers = Option.defaultValue [] specifiers }

  let declare: Parser<ModuleItem, unit> =
    pipe4
      getPosition
      (strWs "declare" >>. strWs "let" >>. pattern)
      (strWs ":" >>. ws >>. typeAnn)
      getPosition
    <| fun start pattern typeAnn stop -> ModuleItem.DeclareLet(pattern, typeAnn)

  let private moduleItem: Parser<ModuleItem, unit> =
    ws
    >>. choice
      [ import |>> ModuleItem.Import; declare; _stmt |>> ModuleItem.Stmt ]

  // Public Exports
  let parseExpr (input: string) : Result<Expr, ParserError> =
    match run expr input with
    | ParserResult.Success(result, _, _) -> Result.Ok(result)
    | ParserResult.Failure(_, error, _) -> Result.Error(error)

  let parseTypeAnn (input: string) : Result<TypeAnn, ParserError> =
    match run typeAnn input with
    | ParserResult.Success(result, _, _) -> Result.Ok(result)
    | ParserResult.Failure(_, error, _) -> Result.Error(error)

  let script: Parser<Module, unit> =
    (many moduleItem) .>> eof |>> fun items -> { Items = items }

  let parseScript (input: string) : Result<Module, ParserError> =
    match run script input with
    | ParserResult.Success(m, _, _) -> Result.Ok(m)
    | ParserResult.Failure(_, error, _) -> Result.Error(error)
