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

  let isIdentifierChar c = isLetter c || isDigit c || c = '_'

  let keyword s =
    pstring s .>> notFollowedBy (satisfy isIdentifierChar) .>> ws

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let qualifiedIdentPratt =
    Pratt.PrattParser<QualifiedIdent>(
      ident |>> fun ident -> QualifiedIdent.Ident ident
    )

  let infixQualifiedNameParselet
    (precedence: int)
    : Pratt.InfixParselet<QualifiedIdent> =
    { Parse =
        fun (parser, stream, left, operator) ->
          let right = ident stream

          match right.Status with
          | Ok -> Reply(QualifiedIdent.Member(left, right.Result))
          | _ -> Reply(left)
      Precedence = precedence }

  qualifiedIdentPratt.RegisterInfix(".", infixQualifiedNameParselet 17)

  let qualifiedIdent: Parser<QualifiedIdent, unit> =
    qualifiedIdentPratt.Parse(0)

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
    (self: bool)
    : Parser<FuncSig<'A>, unit> =
    pipe5
      (opt (keyword "async"))
      (keyword "fn" >>. (opt typeParams))
      (paramList opt_or_id)
      (opt_or_id (strWs "->" >>. typeAnn))
      (opt (ws .>> keyword "throws" >>. typeAnn))
    <| fun async type_params paramList return_type throws ->
      if self then
        // TODO: handle the case where `self` is true but `paramList` is []
        let self :: paramList = paramList

        { TypeParams = type_params
          Self = Some(self)
          ParamList = paramList
          ReturnType = return_type
          Throws = throws
          IsAsync = async.IsSome }
      else
        { TypeParams = type_params
          Self = None
          ParamList = paramList
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
    (keyword "true" |>> fun _ -> Literal.Boolean true)
    <|> (keyword "false" |>> fun _ -> Literal.Boolean false)

  let otherLiterals =
    (keyword "undefined" |>> fun _ -> Literal.Undefined)
    <|> (keyword "null" |>> fun _ -> Literal.Null)

  litRef.Value <-
    choice [ number |>> Literal.Number; string; boolean; otherLiterals ]

  let mergeSpans (x: Span) (y: Span) = { Start = x.Start; Stop = y.Stop }

  let identExpr: Parser<Expr, unit> =
    withSpan ident
    |>> fun (sl, span) ->
      { Kind = ExprKind.Identifier(sl)
        Span = span
        InferredType = None }

  // TODO: dedupe with tmplLitType
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
      (funcSig opt false)
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

  // TODO: spread/rest elements
  let tupleExpr: Parser<Expr, unit> =
    withSpan (between (strWs "[") (strWs "]") (sepBy expr (strWs ",")))
    |>> fun (exprs, span) ->
      { Kind = ExprKind.Tuple { Elems = exprs; Immutable = false }
        Span = span
        InferredType = None }

  // TODO: spread/rest elements
  let imTupleExpr: Parser<Expr, unit> =
    withSpan (between (strWs "#[") (strWs "]") (sepBy expr (strWs ",")))
    |>> fun (exprs, span) ->
      { Kind = ExprKind.Tuple { Elems = exprs; Immutable = true }
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
      { Kind = ExprKind.Object { Elems = objElems; Immutable = false }
        Span = span
        InferredType = None }

  let ifElse, ifElseRef = createParserForwardedToRef<Expr, unit> ()

  let elseClause =
    keyword "else"
    >>. ((ifElse |>> (fun e -> BlockOrExpr.Expr(e)))
         <|> (block |>> BlockOrExpr.Block))

  let ifLet: Parser<ExprKind, unit> =
    pipe3
      ((keyword "let") >>. pattern .>>. (strWs "=" >>. expr))
      block
      (opt elseClause)
    <| fun (pattern, expr) then_ else_ ->
      ExprKind.IfLet(pattern, expr, then_, else_)

  let ifCond: Parser<ExprKind, unit> =
    pipe3 expr block (opt elseClause)
    <| fun cond then_ else_ -> ExprKind.IfElse(cond, then_, else_)

  ifElseRef.Value <-
    pipe3 getPosition ((keyword "if") >>. choice [ ifLet; ifCond ]) getPosition
    <| fun start kind stop ->
      { Kind = kind
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
      (opt ((keyword "if") >>. expr))
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

  let structExpr: Parser<Expr, unit> =
    pipe5
      getPosition
      qualifiedIdent
      (opt (between (strWs "<") (strWs ">") (sepBy typeAnn (strWs ","))))
      (between (strWs "{") (strWs "}") (sepBy objElem (strWs ",")))
      getPosition
    <| fun start ident typeArgs elems stop ->
      let kind =
        ExprKind.Struct
          { TypeRef = { Ident = ident; TypeArgs = typeArgs }
            Elems = elems }

      { Kind = kind
        Span = { Start = start; Stop = stop }
        InferredType = None }

  let catchClause: Parser<list<MatchCase>, unit> =
    pipe3
      getPosition
      (between (strWs "{") (strWs "}") (many matchCase))
      getPosition
    <| fun start cases stop -> cases

  let throwExpr: Parser<Expr, unit> =
    withSpan (keyword "throw" >>. expr)
    |>> fun (expr, span) ->
      { Kind = ExprKind.Throw(expr)
        Span = span
        InferredType = None }

  let tryExpr: Parser<Expr, unit> =
    pipe5
      getPosition
      (keyword "try" >>. block)
      (opt (keyword "catch" >>. catchClause))
      (opt (keyword "finally" >>. block))
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

  let imRecordExpr: Parser<Expr, unit> =
    withSpan (between (strWs "#{") (strWs "}") (sepBy objElem (strWs ",")))
    |>> fun (objElems, span) ->
      { Kind = ExprKind.Object { Elems = objElems; Immutable = true }
        Span = span
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
        attempt structExpr // conflicts with identExpr
        tupleExpr
        objectExpr
        imTupleExpr
        imRecordExpr
        templateStringLiteral
        // TODO: have a combinator that wraps `identExpr` and checks if the
        // ident if `fn`, `do`, `if`, `throw`, `try`, match`, etc. and then
        // continue parsing the appropriate expression
        identExpr ]

  let exprParser = Pratt.PrattParser<Expr>(atom .>> ws)

  let binary op x y =
    { Expr.Kind = ExprKind.Binary(op, x, y)
      Span = mergeSpans x.Span y.Span
      InferredType = None }

  let prefixExprParslet
    (precedence: int)
    (callback: Expr -> Expr)
    : Pratt.PrefixParselet<Expr> =
    { Parse =
        fun (parser, stream, operator) ->
          let operand = parser.Parse precedence stream

          match operand.Status with
          | Ok -> Reply(callback operand.Result)
          | _ -> operand
      Precedence = precedence }

  let unaryExprParslet
    (precedence: int)
    (operator: string)
    : Pratt.PrefixParselet<Expr> =
    prefixExprParslet precedence (fun operand ->
      { Expr.Kind = ExprKind.Unary(operator, operand)
        Span = operand.Span
        InferredType = None })

  let infixExprParselet
    (precedence: int)
    (callback: Expr -> Expr -> Expr)
    : Pratt.InfixParselet<Expr> =
    { Parse =
        fun (parser, stream, left, operator) ->
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
            let kind =
              match left.Kind with
              | ExprKind.ExprWithTypeArgs(callee, typeArgs) ->
                ExprKind.Call
                  { Callee = callee
                    TypeArgs = Some(typeArgs)
                    Args = args.Result
                    OptChain = false
                    Throws = None }
              | _ ->
                ExprKind.Call
                  { Callee = left
                    TypeArgs = None
                    Args = args.Result
                    OptChain = false
                    Throws = None }

            Reply(
              { Expr.Kind = kind
                Span =
                  { Start = left.Span.Start
                    Stop = stream.Position }
                InferredType = None }
            )
          | _ -> Reply(args.Status, args.Error)
      Precedence = precedence }

  exprParser.RegisterInfix("(", callParselet 17)

  let typeArgsParselet (precedence: int) : Pratt.InfixParselet<Expr> =
    { Parse =
        fun (parser, stream, left, operator) ->

          let typeArgs =
            if stream.PeekString(1) = ">" then
              stream.Skip(1)
              ws stream |> ignore // always succeeds
              Reply([])
            else
              let parseTypeArgs = sepBy typeAnn (strWs ",")
              let reply = (parseTypeArgs .>> (strWs ">")) stream
              reply

          match typeArgs.Status with
          | Ok ->
            let kind = ExprKind.ExprWithTypeArgs(left, typeArgs.Result)

            Reply(
              { Expr.Kind = kind
                Span =
                  { Start = left.Span.Start
                    Stop = stream.Position }
                InferredType = None }
            )

          | _ -> Reply(typeArgs.Status, typeArgs.Error)
      Precedence = precedence }

  exprParser.RegisterInfix("<", typeArgsParselet 17)

  let newingParselet: Pratt.PrefixParselet<Expr> =
    prefixExprParslet 16 (fun operand ->
      match operand.Kind with
      | ExprKind.Call call ->
        { Expr.Kind =
            ExprKind.New
              { Callee = call.Callee
                TypeArgs = call.TypeArgs
                Args = Some call.Args
                Throws = None }
          Span = operand.Span
          InferredType = None }
      // TODO: return a Reply
      | _ -> failwith "Expected call expression")

  exprParser.RegisterPrefix("new", newingParselet)

  exprParser.RegisterPrefix("!", unaryExprParslet 14 "!")
  // bitwise not (14)
  exprParser.RegisterPrefix("+", unaryExprParslet 14 "+")
  exprParser.RegisterPrefix("-", unaryExprParslet 14 "-")

  exprParser.RegisterPrefix(
    "await",
    prefixExprParslet 14 (fun x ->
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

  exprParser.RegisterInfix(
    "..",
    infixExprParselet 2 (fun min max ->
      { Expr.Kind = ExprKind.Range { Min = min; Max = max }
        Span = mergeSpans min.Span max.Span
        InferredType = None })
  )

  // TODO: handle update expressions
  exprParser.RegisterInfix(
    "=",
    infixExprParselet 1 (fun left right ->
      { Expr.Kind = ExprKind.Assign("=", left, right)
        Span = mergeSpans left.Span right.Span
        InferredType = None })
  )

  exprRef.Value <- exprParser.Parse(0)

  // let private exprSemi:

  let private exprStmt: Parser<Stmt, unit> =
    fun stream ->
      let reply = expr stream

      if reply.Status <> Ok then
        Reply(reply.Status, reply.Error)
      else
        let e = reply.Result

        match e.Kind with
        // Don't allow ';' after these expressions
        | ExprKind.IfElse _
        | ExprKind.Try _
        | ExprKind.Match _
        | ExprKind.Do _ -> Reply({ Stmt.Kind = Expr(e); Span = e.Span })
        // Require ';' after all other expressions
        | _ ->
          let semi = choice [ strWs ";"; lookAhead (strWs "}") ]
          let semiReply = semi stream

          match semiReply.Status with
          | Ok -> Reply({ Stmt.Kind = Expr(e); Span = e.Span })
          | _ -> Reply(semiReply.Status, semiReply.Error)

  let private returnStmt: Parser<Stmt, unit> =
    withSpan (keyword "return" >>. opt expr .>> (strWs ";"))
    |>> fun (e, span) -> { Stmt.Kind = Return(e); Span = span }

  let private declStmt (decl: Parser<Decl, unit>) : Parser<Stmt, unit> =
    withSpan decl
    |>> fun (d, span) ->
      { Stmt.Kind = StmtKind.Decl d
        Span = span }

  let private varDecl: Parser<Decl, unit> =
    withSpan (
      tuple4
        (keyword "let" >>. pattern)
        (opt (strWs ":" >>. ws >>. typeAnn))
        (strWs "=" >>. expr)
        ((opt (keyword "else" >>. block)) .>> (strWs ";"))
    )
    |>> fun ((pat, typeAnn, init, elseClause), span) ->
      { Kind =
          DeclKind.VarDecl
            { Pattern = pat
              Init = init
              TypeAnn = typeAnn
              Else = elseClause }
        Span = span }

  let private typeDecl: Parser<Decl, unit> =
    pipe5
      getPosition
      (keyword "type" >>. ident)
      (opt (between (strWs "<") (strWs ">") (sepBy typeParam (strWs ","))))
      (strWs "=" >>. typeAnn .>> (strWs ";"))
      getPosition
    <| fun start id typeParams typeAnn stop ->
      let span = { Start = start; Stop = stop }

      { Kind =
          TypeDecl
            { Name = id
              TypeAnn = typeAnn
              TypeParams = typeParams }
        Span = span }

  let private enumVariant: Parser<EnumVariant, unit> =
    pipe4
      getPosition
      (strWs "|" >>. ident)
      (between (strWs "(") (strWs ")") (sepBy typeAnn (strWs ",")))
      getPosition
    <| fun start name typeAnns stop ->

      let span = { Start = start; Stop = stop }

      { Name = name; TypeAnns = typeAnns }

  let private enumDecl: Parser<Decl, unit> =
    pipe5
      getPosition
      (keyword "enum" >>. ident)
      (opt (between (strWs "<") (strWs ">") (sepBy typeParam (strWs ","))))
      (between (strWs "{") (strWs "}") (many enumVariant))
      getPosition
    <| fun start name typeParams variants stop ->

      let span = { Start = start; Stop = stop }

      { Kind =
          EnumDecl
            { Name = name
              TypeParams = typeParams
              Variants = variants }
        Span = span }

  let private forLoop =
    pipe5
      getPosition
      (keyword "for" >>. pattern)
      (keyword "in" >>. expr)
      (ws >>. block)
      getPosition
    <| fun start pattern expr body stop ->
      { Stmt.Kind = For(pattern, expr, body)
        Span = { Start = start; Stop = stop } }

  let private propName =
    choice
      [ ident |>> PropName.String
        number .>> ws |>> PropName.Number
        _string .>> ws |>> PropName.String
        between (strWs "[") (strWs "]") expr |>> PropName.Computed ]

  let private propertyTypeAnn: Parser<Property, unit> =
    pipe5
      getPosition
      propName
      (opt (strWs "?"))
      (strWs ":" >>. typeAnn)
      getPosition
    <| fun p1 name optional typeAnn p2 ->
      // TODO: add location information
      let span = { Start = p1; Stop = p2 }

      { Name = name
        TypeAnn = typeAnn
        Optional = optional.IsSome
        Readonly = false }

  let private structDecl: Parser<Decl, unit> =
    pipe5
      getPosition
      (keyword "struct" >>. ident)
      (opt (between (strWs "<") (strWs ">") (sepBy typeParam (strWs ","))))
      (between (strWs "{") (strWs "}") (sepEndBy propertyTypeAnn (strWs ",")))
      getPosition
    <| fun start name typeParams elems stop ->
      let span = { Start = start; Stop = stop }

      { Kind =
          StructDecl
            { Name = name
              TypeParams = typeParams
              Elems = elems }
        Span = span }

  let private method: Parser<ImplElem, unit> =
    pipe5
      (opt (keyword "async"))
      (keyword "fn" >>. ident)
      ((opt typeParams) .>>. (paramList opt))
      ((opt (strWs "->" >>. typeAnn))
       .>>. (opt (ws .>> keyword "throws" >>. typeAnn)))
      block
    <| fun async name (typeParams, paramList) (retType, throws) body ->
      let funcSig: FuncSig<option<TypeAnn>> =
        match paramList with
        | [] ->
          { TypeParams = typeParams
            Self = None
            ParamList = paramList
            ReturnType = retType
            Throws = throws
            IsAsync = async.IsSome }
        | { Pattern = { Kind = PatternKind.Ident { Name = "self" } } } as self :: paramList ->
          { TypeParams = typeParams
            Self = Some(self)
            ParamList = paramList
            ReturnType = retType
            Throws = throws
            IsAsync = async.IsSome }
        | paramList ->
          { TypeParams = typeParams
            Self = None
            ParamList = paramList
            ReturnType = retType
            Throws = throws
            IsAsync = async.IsSome }

      { Name = name
        Sig = funcSig
        Body = BlockOrExpr.Block body }
      |> ImplElem.Method

  let private getter: Parser<ImplElem, unit> =
    pipe5
      (opt (keyword "async"))
      (keyword "get" >>. ident)
      (between (strWs "(") (strWs ")") (funcParam opt))
      ((opt (strWs "->" >>. typeAnn))
       .>>. (opt (ws .>> keyword "throws" >>. typeAnn)))
      block
    <| fun async name self (retType, throws) body ->
      { Getter.Name = name
        Self = self
        Body = BlockOrExpr.Block body
        ReturnType = retType
        Throws = throws }
      |> ImplElem.Getter

  // TODO: generic setters
  let private setter: Parser<ImplElem, unit> =
    pipe5
      (opt (keyword "async"))
      (keyword "set" >>. ident)
      (between
        (strWs "(")
        (strWs ")")
        ((funcParam opt) .>>. (strWs "," >>. (funcParam opt))))
      (opt (ws .>> keyword "throws" >>. typeAnn))
      block
    <| fun async name (self, param) throws body ->
      { Setter.Name = name
        Self = self
        Param = param
        Body = BlockOrExpr.Block body
        Throws = throws }
      |> ImplElem.Setter

  let private implElem = choice [ method; getter; setter ]

  let private implStmt =
    pipe5
      getPosition
      (keyword "impl" >>. ident)
      (opt typeParams)
      (between (strWs "{") (strWs "}") (many implElem))
      getPosition
    <| fun start name typeParams elems stop ->
      { Stmt.Kind =
          Impl
            { TypeParams = typeParams
              Self = name
              Elems = elems }
        Span = { Start = start; Stop = stop } }

  let _stmt =
    choice
      [
        // TODO: these should all use `attempt` since you could have an
        // identifier like `lettuce` that conflicts with `let <ident> = <expr>`
        varDecl |> declStmt
        typeDecl |> declStmt
        structDecl |> declStmt
        enumDecl |> declStmt
        implStmt
        returnStmt
        forLoop

        // exprStmt must come last because it can recognize any alphanumeric
        // sequence as an identifier which is a valid expression
        exprStmt ]

  stmtRef.Value <- ws >>. _stmt

  let private isAssertion = (keyword "is" >>. qualifiedIdent)

  let private identPattern =
    withSpan (tuple3 (opt (keyword "mut")) ident (opt isAssertion))
    |>> fun ((mut, name, assertion), span) ->
      { Pattern.Kind =
          PatternKind.Ident
            { Name = name
              IsMut = mut.IsSome
              Assertion = assertion }
        Span = span
        InferredType = None }

  let private literalPattern =
    withSpan (lit .>> ws)
    |>> fun (lit, span) ->
      { Pattern.Kind = PatternKind.Literal lit
        Span = span
        InferredType = None }

  let private tuplePattern =
    between (strWs "[") (strWs "]") (sepBy pattern (strWs ",")) |> withSpan
    |>> fun (patterns, span) ->
      // TODO: handle immutable tuple patterns
      { Pattern.Kind = PatternKind.Tuple { Elems = patterns; Immutable = false }
        Span = span
        InferredType = None }

  let private imTuplePattern =
    between (strWs "#[") (strWs "]") (sepBy pattern (strWs ",")) |> withSpan
    |>> fun (patterns, span) ->
      // TODO: handle immutable tuple patterns
      { Pattern.Kind = PatternKind.Tuple { Elems = patterns; Immutable = true }
        Span = span
        InferredType = None }

  let private wildcardPattern =
    pipe4 getPosition (strWs "_") (opt isAssertion) getPosition
    <| fun start _ assertion stop ->
      { Pattern.Kind = PatternKind.Wildcard { Assertion = assertion }
        Span = { Start = start; Stop = stop }
        InferredType = None }

  let private keyValuePat =
    pipe5
      getPosition
      ident
      (strWs ":" >>. pattern)
      (opt (strWs "=" >>. expr))
      getPosition
    <| fun start name value def stop ->
      let span = { Start = start; Stop = stop }

      KeyValuePat
        { Span = span
          Key = name
          Value = value
          Default = def }

  let private shorthandPat =
    withSpan (
      tuple4
        (opt (keyword "mut"))
        ident
        (opt isAssertion)
        (opt (strWs "=" >>. expr))
    )
    |>> fun ((mut, name, assertion, def), span) ->
      ShorthandPat
        { Span = span
          Name = name
          Default = def
          IsMut = mut.IsSome
          Assertion = assertion }

  let private objPatRestElem =
    withSpan (strWs "..." >>. pattern)
    |>> fun (pattern, span) ->
      RestPat
        { Span = span
          Target = pattern
          IsMut = false }

  let private objPatElem =
    choice [ attempt keyValuePat; shorthandPat; objPatRestElem ]

  let private objectPattern =
    withSpan (between (strWs "{") (strWs "}") (sepBy objPatElem (strWs ",")))
    |>> fun (elems, span) ->
      // TODO: handle immutable object patterns
      { Pattern.Kind = PatternKind.Object { Elems = elems; Immutable = false }
        Span = span
        InferredType = None }

  let private structPattern =
    withSpan (
      tuple3
        qualifiedIdent
        (opt (between (strWs "<") (strWs ">") (sepBy typeAnn (strWs ","))))
        (between (strWs "{") (strWs "}") (sepBy objPatElem (strWs ",")))
    )
    |>> fun ((ident, typeArgs, elems), span) ->
      let kind =
        PatternKind.Struct
          { TypeRef = { Ident = ident; TypeArgs = typeArgs }
            Elems = elems }

      { Pattern.Kind = kind
        Span = span
        InferredType = None }

  let private imObjectPattern =
    withSpan (between (strWs "#{") (strWs "}") (sepBy objPatElem (strWs ",")))
    |>> fun (elems, span) ->
      // TODO: handle immutable object patterns
      { Pattern.Kind = PatternKind.Object { Elems = elems; Immutable = true }
        Span = span
        InferredType = None }

  let private enumVariantPattern: Parser<Pattern, unit> =
    withSpan (
      tuple3
        ident
        (strWs "." >>. ident)
        (opt (between (strWs "(") (strWs ")") (sepBy pattern (strWs ","))))
    )
    |>> fun ((qualifier, ident, args), span) ->
      let ident = QualifiedIdent.Member(QualifiedIdent.Ident qualifier, ident)

      { Pattern.Kind = PatternKind.Enum { Ident = ident; Args = args }
        Span = span
        InferredType = None }

  let private restPattern =
    withSpan (strWs "..." >>. pattern)
    |>> fun (pattern, span) ->
      { Pattern.Kind = PatternKind.Rest(pattern)
        Span = span
        InferredType = None }

  patternRef.Value <-
    choice
      [ attempt structPattern
        attempt enumVariantPattern
        identPattern
        literalPattern
        wildcardPattern
        objectPattern
        imObjectPattern
        tuplePattern
        imTuplePattern
        restPattern ]

  let private litTypeAnn =
    withSpan lit
    |>> fun (lit, span) ->
      { TypeAnn.Kind = TypeAnnKind.Literal(lit)
        Span = span
        InferredType = None }

  let private keywordTypeAnn =
    let keyword =
      choice
        [ (keyword "object" |>> fun _ -> KeywordTypeAnn.Object)
          (keyword "never" |>> fun _ -> Never)
          (keyword "unknown" |>> fun _ -> Unknown)
          (keyword "boolean" |>> fun _ -> Boolean)
          (keyword "number" |>> fun _ -> Number)
          (keyword "string" |>> fun _ -> String)
          (keyword "symbol" |>> fun _ -> Symbol)
          (keyword "null" |>> fun _ -> Null)
          (keyword "undefined" |>> fun _ -> Undefined) ]

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
    withSpan (keyword "unique" >>. keyword "symbol")
    |>> fun (_, span) ->
      { TypeAnn.Kind = TypeAnnKind.Keyword KeywordTypeAnn.UniqueSymbol
        Span = span
        InferredType = None }

  let private uniqueNumberTypeAnn =
    withSpan (keyword "unique" >>. keyword "number")
    |>> fun (_, span) ->
      { TypeAnn.Kind = TypeAnnKind.Keyword KeywordTypeAnn.UniqueNumber
        Span = span
        InferredType = None }

  let private tupleTypeAnn =
    between (strWs "[") (strWs "]") (sepBy typeAnn (strWs ",")) |> withSpan
    |>> fun (typeAnns, span) ->
      { TypeAnn.Kind = TypeAnnKind.Tuple { Elems = typeAnns; Immutable = false }
        Span = span
        InferredType = None }

  let private imTupleTypeAnn =
    between (strWs "#[") (strWs "]") (sepBy typeAnn (strWs ",")) |> withSpan
    |>> fun (typeAnns, span) ->
      { TypeAnn.Kind = TypeAnnKind.Tuple { Elems = typeAnns; Immutable = true }
        Span = span
        InferredType = None }

  let private funcTypeAnn =
    funcSig id false |> withSpan
    |>> fun (f, span) ->
      { TypeAnn.Kind = TypeAnnKind.Function(f)
        Span = span
        InferredType = None }

  let private readonlyModifier =
    pipe2 (opt (strWs "+" <|> strWs "-")) (keyword "readonly")
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
    pipe2 (keyword "for" >>. ident) (keyword "in" >>. typeAnn)
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
        | TypeAnnKind.TypeRef { Ident = QualifiedIdent.Ident name } when
          name = typeParam.Name
          ->
          None
        | _ -> Some(name)

      ObjTypeAnnElem.Mapped
        { TypeParam = typeParam
          Name = name
          TypeAnn = typeAnn
          Readonly = readonly
          Optional = optional }

  let private callableSignature =
    pipe4 getPosition (opt (keyword "new")) (funcSig id false) getPosition
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
        attempt (propertyTypeAnn |>> ObjTypeAnnElem.Property) ]

  let private objectTypeAnn =
    withSpan (
      between (strWs "{") (strWs "}") (sepEndBy objTypeAnnElem (strWs ","))
    )
    |>> fun (objElems, span) ->
      { TypeAnn.Kind =
          TypeAnnKind.Object { Elems = objElems; Immutable = false }
        Span = span
        InferredType = None }

  let private imObjectTypeAnn =
    withSpan (
      between (strWs "#{") (strWs "}") (sepEndBy objTypeAnnElem (strWs ","))
    )
    |>> fun (objElems, span) ->
      { TypeAnn.Kind = TypeAnnKind.Object { Elems = objElems; Immutable = true }
        Span = span
        InferredType = None }

  // TODO: don't include strWs in the span
  let private keyofTypeAnn =
    withSpan (keyword "keyof" >>. typeAnn)
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
    withSpan (keyword "typeof" >>. qualifiedIdent)
    |>> fun (e, span) ->
      { TypeAnn.Kind = TypeAnnKind.Typeof e
        Span = span
        InferredType = None }

  let condTypeAnn, condTypeAnnRef = createParserForwardedToRef<TypeAnn, unit> ()

  // TODO: add support for chaining conditional types
  condTypeAnnRef.Value <-
    pipe5
      getPosition
      (keyword "if"
       >>. (pipe2 typeAnn (strWs ":" >>. typeAnn)
            <| fun check extends -> (check, extends)))
      (strWs "{" >>. typeAnn .>> strWs "}")
      (keyword "else"
       >>. (condTypeAnn <|> (strWs "{" >>. typeAnn .>> strWs "}")))
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
    <| fun start ident typeArgs stop ->
      { TypeAnn.Kind =
          TypeAnnKind.TypeRef
            { Ident = QualifiedIdent.Ident ident
              TypeArgs = typeArgs }
        Span = { Start = start; Stop = stop }
        InferredType = None }

  // TODO: dedupe with templateStringLiteral
  let tmplLitType: Parser<TypeAnn, unit> =
    fun stream ->
      let sb = StringBuilder()
      let mutable parts: list<string> = []
      let mutable exprs: list<TypeAnn> = []
      let mutable reply: voption<Reply<TypeAnn>> = ValueNone

      if stream.Peek() = '`' then
        let start = stream.Position
        stream.Skip() // '`'

        while stream.Peek() <> '`' && reply = ValueNone do
          if stream.PeekString(2) = "${" then
            stream.Skip(2) // '${'
            parts <- sb.ToString() :: parts
            sb.Clear() |> ignore
            let expr = typeAnn stream

            if expr.Status = ReplyStatus.Ok then
              if stream.Peek() = '}' then
                stream.Skip()
                exprs <- expr.Result :: exprs
              else
                reply <- ValueSome(Reply(Error, messageError "Expected '}'"))
            else
              reply <- ValueSome(expr)
          else
            sb.Append(stream.Read()) |> ignore

        match reply with
        | ValueNone ->
          stream.Skip() // '`'
          let stop = stream.Position
          parts <- sb.ToString() :: parts

          let result: TypeAnn =
            { Kind =
                TypeAnnKind.TemplateLiteral(
                  { Parts = List.rev parts
                    Exprs = List.rev exprs }
                )
              Span = { Start = start; Stop = stop }
              InferredType = None }

          Reply(result)
        | ValueSome(value) -> value
      else
        Reply(Error, messageError "Expected '`'")

  // TODO: add support for type constraints on the inferred type
  let private inferType =
    withSpan (strWs "infer" >>. ident)
    |>> fun (name, span) ->
      { TypeAnn.Kind = TypeAnnKind.Infer(name)
        Span = span
        InferredType = None }

  let primaryType =
    choice
      [ litTypeAnn
        keywordTypeAnn // aka PredefinedType
        uniqueSymbolTypeAnn
        uniqueNumberTypeAnn
        tupleTypeAnn
        imTupleTypeAnn
        funcTypeAnn
        typeofTypeAnn // aka TypeQuery
        keyofTypeAnn
        inferType
        restTypeAnn
        objectTypeAnn
        imObjectTypeAnn
        condTypeAnn
        tmplLitType
        // TODO: thisTypeAnn
        // NOTE: should come last since any identifier can be a type reference
        typeRef ]
    .>> ws

  let infixTypeAnnParselet
    (precedence: int)
    // TODO: update callback to return a Reply<TypeAnn>
    (callback: TypeAnn -> TypeAnn -> TypeAnn)
    : Pratt.InfixParselet<TypeAnn> =
    { Parse =
        fun (parser, stream, left, operator) ->
          let right = parser.Parse precedence stream

          match right.Status with
          | Ok -> Reply(callback left right.Result)
          | _ -> right
      Precedence = precedence }

  let typeAnnParser = Pratt.PrattParser<TypeAnn>(primaryType)

  let groupingTypeAnnParselet
    (precedence: int)
    : Pratt.PrefixParselet<TypeAnn> =
    { Parse =
        fun (parser, stream, operator) ->
          (typeAnnParser.Parse 0 .>> strWs ")") stream
      Precedence = precedence }

  typeAnnParser.RegisterPrefix("(", groupingTypeAnnParselet 18)

  typeAnnParser.RegisterInfix(
    ".",
    infixTypeAnnParselet 17 (fun left right ->
      match left.Kind, right.Kind with
      | TypeAnnKind.TypeRef { Ident = qualifier },
        TypeAnnKind.TypeRef { Ident = QualifiedIdent.Ident name } ->
        let kind =
          TypeAnnKind.TypeRef
            { Ident = QualifiedIdent.Member(qualifier, name)
              TypeArgs = None }

        { TypeAnn.Kind = kind
          Span = mergeSpans left.Span right.Span
          InferredType = None }
      // TODO: return a Reply for this error case
      | _ -> failwith "TODO")
  )

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

  typeAnnParser.RegisterInfix(
    "..",
    infixTypeAnnParselet 2 (fun min max ->
      { TypeAnn.Kind = TypeAnnKind.Range { Min = min; Max = max }
        Span = mergeSpans min.Span max.Span
        InferredType = None })
  )

  typeAnnRef.Value <- typeAnnParser.Parse(0)

  let namedSpecifier: Parser<ImportSpecifier, unit> =
    pipe4 getPosition ident (opt (keyword "as" >>. ident)) getPosition
    <| fun start name alias stop ->
      let span = { Start = start; Stop = stop }
      ImportSpecifier.Named(name, alias)

  let private namedSpecifiers: Parser<list<ImportSpecifier>, unit> =
    between (strWs "{") (strWs "}") (sepBy namedSpecifier (strWs ","))

  let private moduleAlias: Parser<list<ImportSpecifier>, unit> =
    pipe3 getPosition (keyword "as" >>. ident) getPosition
    <| fun start alias stop ->
      let span = { Start = start; Stop = stop }
      [ ImportSpecifier.ModuleAlias alias ]

  let private importSpecifiers = choice [ namedSpecifiers; moduleAlias ]

  let import: Parser<Import, unit> =
    pipe4
      getPosition
      (keyword "import" >>. _string .>> ws)
      (opt importSpecifiers .>> (strWs ";"))
      getPosition
    <| fun start source specifiers stop ->
      { Path = source
        Specifiers = Option.defaultValue [] specifiers }

  let declare: Parser<ScriptItem, unit> =
    pipe4
      getPosition
      (keyword "declare" >>. keyword "let" >>. pattern)
      (strWs ":" >>. ws >>. typeAnn .>> (strWs ";"))
      getPosition
    <| fun start pattern typeAnn stop -> ScriptItem.DeclareLet(pattern, typeAnn)

  let private scriptItems: Parser<ScriptItem, unit> =
    ws
    >>. choice
      [ import |>> ScriptItem.Import; declare; _stmt |>> ScriptItem.Stmt ]

  let decl: Parser<Decl, unit> =
    choice [ varDecl; typeDecl; structDecl; enumDecl ]

  let private moduleItem: Parser<ModuleItem, unit> =
    ws >>. choice [ import |>> ModuleItem.Import; decl |>> ModuleItem.Decl ]

  // Public Exports
  let parseExpr (input: string) : Result<Expr, ParserError> =
    match run expr input with
    | ParserResult.Success(result, _, _) -> Result.Ok(result)
    | ParserResult.Failure(_, error, _) -> Result.Error(error)

  let parseTypeAnn (input: string) : Result<TypeAnn, ParserError> =
    match run typeAnn input with
    | ParserResult.Success(result, _, _) -> Result.Ok(result)
    | ParserResult.Failure(_, error, _) -> Result.Error(error)

  let script: Parser<Script, unit> =
    (many scriptItems) .>> eof |>> fun items -> { Items = items }

  let parseScript (input: string) : Result<Script, ParserError> =
    match run script input with
    | ParserResult.Success(m, _, _) -> Result.Ok(m)
    | ParserResult.Failure(_, error, _) -> Result.Error(error)

  let parseModule (input: string) : Result<Module, ParserError> =
    match
      run ((many moduleItem) .>> eof |>> fun items -> { Items = items }) input
    with
    | ParserResult.Success(m, _, _) -> Result.Ok(m)
    | ParserResult.Failure(_, error, _) -> Result.Error(error)
