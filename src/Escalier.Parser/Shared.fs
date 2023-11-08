namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax

module Shared =
  let ws = spaces
  let str_ws s = pstring s .>> ws

  let withSpan p =
    pipe3 getPosition p getPosition
    <| fun start value stop -> (value, { start = start; stop = stop })

  let tuple<'a> (parser: Parser<'a, unit>) =
    between (str_ws "[") (str_ws "]") (sepBy parser (str_ws ","))
