namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax

module Literals =
  let lit = ParserRefs.lit

  let number: Parser<Literal, unit> =
    pfloat |>> fun nl -> Literal.Number(nl |> string)

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

  ParserRefs.litRef.Value <- number <|> string <|> boolean
