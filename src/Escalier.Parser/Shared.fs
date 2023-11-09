namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax

module Shared =
  let typeAnn = ParserRefs.typeAnn
  let pattern = ParserRefs.pattern

  let ws = spaces
  let str_ws s = pstring s .>> ws

  let withSpan p =
    pipe3 getPosition p getPosition
    <| fun start value stop -> (value, { start = start; stop = stop })

  let ident =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

  let tuple<'a> (parser: Parser<'a, unit>) =
    between (str_ws "[") (str_ws "]") (sepBy parser (str_ws ","))

  let typeParam: Parser<TypeParam, unit> =
    pipe5
      getPosition
      ident
      (opt (str_ws ":" >>. typeAnn))
      (opt (str_ws "=" >>. typeAnn))
      getPosition
    <| fun start name constraint_ default_ stop ->
      { name = name
        constraint_ = constraint_
        default_ = default_
        span = { start = start; stop = stop } }

  let typeParams: Parser<list<TypeParam>, unit> =
    between (str_ws "<") (str_ws ">") (sepBy typeParam (str_ws ","))

  let funcParam<'a>
    (opt_or_id: Parser<TypeAnn, unit> -> Parser<'a, unit>)
    : Parser<FuncParam<'a>, unit> =
    pipe3 pattern (opt (str_ws "?")) (opt_or_id (str_ws ":" >>. typeAnn))
    <| fun pattern optional typeAnn ->
      { pattern = pattern
        optional = optional.IsSome
        typeAnn = typeAnn }

  let param_list<'a>
    (opt_or_id: Parser<TypeAnn, unit> -> Parser<'a, unit>)
    : Parser<list<FuncParam<'a>>, unit> =
    between (str_ws "(") (str_ws ")") (sepBy (funcParam opt_or_id) (str_ws ","))

  let func_sig<'a>
    (opt_or_id: Parser<TypeAnn, unit> -> Parser<'a, unit>)
    : Parser<FuncSig<'a>, unit> =
    pipe4
      (opt typeParams)
      (str_ws "fn" >>. (param_list opt_or_id))
      (opt_or_id (str_ws "->" >>. typeAnn))
      (opt (ws .>> str_ws "throws" >>. typeAnn))
    <| fun type_params param_list return_type throws ->
      { param_list = param_list
        return_type = return_type
        type_params = type_params
        throws = throws }
