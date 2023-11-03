namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax

module Literals =
  let number: Parser<Literal, unit> =
    pfloat |>> fun nl -> Literal.Number(nl |> string)

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

    _string |>> fun sl -> Literal.String(sl)

  let literal = number <|> stringLiteral
