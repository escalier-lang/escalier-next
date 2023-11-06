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
  let str_ws s = pstring s .>> ws

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let withSpan p =
    pipe3 getPosition p getPosition
    <| fun start value stop -> (value, { start = start; stop = stop })

  let private exprStmt: Parser<Stmt, unit> =
    withSpan (ws >>. expr)
    |>> fun (e, span) -> { Stmt.kind = Expr(e); span = span }

  let private returnStmt: Parser<Stmt, unit> =
    withSpan (ws >>. str_ws "return" >>. opt expr)
    |>> fun (e, span) -> { Stmt.kind = Return(e); span = span }

  // `let <expr> = <expr>`
  let private varDecl =
    pipe4
      getPosition
      (ws >>. str_ws "let" >>. pattern)
      (str_ws "=" >>. expr)
      getPosition
    <| fun start pat init stop ->
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
      (ws >>. str_ws "type" >>. ident)
      (str_ws "=" >>. typeAnn)
      getPosition
    <| fun start id typeAnn stop ->
      let span = { start = start; stop = stop }

      { Stmt.kind =
          Decl(
            { kind = TypeDecl(id, typeAnn, None)
              span = span }
          )
        span = span }

  // TODO: Parse for loops
  ParserRefs.stmtRef.Value <- choice [ varDecl; typeDecl; returnStmt; exprStmt ]
