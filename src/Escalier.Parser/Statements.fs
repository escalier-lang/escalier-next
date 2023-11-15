namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax

module private Statements =
  // re-export
  let stmt = ParserRefs.stmt

  let expr = ParserRefs.expr
  let pattern = ParserRefs.pattern
  let typeAnn = ParserRefs.typeAnn

  let ws = spaces
  let strWs s = pstring s .>> ws

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let withSpan p =
    pipe3 getPosition p getPosition
    <| fun start value stop -> (value, { Start = start; Stop = stop })

  let private exprStmt: Parser<Stmt, unit> =
    withSpan (expr) |>> fun (e, span) -> { Stmt.Kind = Expr(e); Span = span }

  let private returnStmt: Parser<Stmt, unit> =
    withSpan (strWs "return" >>. opt expr)
    |>> fun (e, span) -> { Stmt.Kind = Return(e); Span = span }

  // `let <expr> = <expr>`
  let private varDecl =
    pipe4 getPosition (strWs "let" >>. pattern) (strWs "=" >>. expr) getPosition
    <| fun start pat init stop ->
      let span = { Start = start; Stop = stop }

      { Stmt.Kind =
          Decl(
            { Kind = VarDecl(pat, Some(init), None, false)
              Span = span }
          )
        Span = span }

  // TODO: parse type params
  let private typeDecl =
    pipe4
      getPosition
      (strWs "type" >>. ident)
      (strWs "=" >>. typeAnn)
      getPosition
    <| fun start id typeAnn stop ->
      let span = { Start = start; Stop = stop }

      { Stmt.Kind =
          Decl(
            { Kind = TypeDecl(id, typeAnn, None)
              Span = span }
          )
        Span = span }

  // TODO: Parse for loops
  ParserRefs.stmtRef.Value <-
    ws >>. choice [ varDecl; typeDecl; returnStmt; exprStmt ]
