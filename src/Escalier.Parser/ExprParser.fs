namespace Escalier.Parser

open FParsec
open Escalier.Data.Syntax

module ExprParser =
    let ws = spaces

    let str_ws s = pstring s .>> ws

    let number: Parser<Expr, unit> =
        pipe3 getPosition pfloat getPosition
        <| fun p1 nl p2 ->
            let start = p1.Index |> int
            let stop = p2.Index |> int

            { kind = ExprKind.Literal(Literal.Number(nl |> string))
              span = { start = start; stop = stop }
              inferred_type = ref None }

    let opp = new OperatorPrecedenceParser<Expr, unit, unit>()
    let expr = opp.ExpressionParser
    let term = (number .>> ws) <|> between (str_ws "(") (str_ws ")") expr
    opp.TermParser <- term

    type Assoc = Associativity

    opp.AddOperator(
        InfixOperator(
            "+",
            ws,
            1,
            Assoc.Left,
            (fun x y ->
                { Expr.kind = ExprKind.Binary(x, BinaryOp.Add, y)
                  span =
                    { start = x.span.start
                      stop = y.span.stop }
                  inferred_type = ref None })
        )
    )

    opp.AddOperator(
        InfixOperator(
            "*",
            ws,
            2,
            Assoc.Left,
            (fun x y ->
                { Expr.kind = ExprKind.Binary(x, BinaryOp.Add, y)
                  span =
                    { start = x.span.start
                      stop = y.span.stop }
                  inferred_type = ref None })
        )
    )

    opp.AddOperator(
        PrefixOperator(
            "-",
            ws,
            3,
            true,
            (fun x ->
                { Expr.kind = ExprKind.Unary("-", x)
                  span =
                    { start = x.span.start
                      stop = x.span.stop }
                  inferred_type = ref None })
        )
    )
