namespace Escalier.Parser

open FParsec

module Say =
  let hello name = printfn $"Hello %s{name}"

  open Escalier.Data.Type

  let t =
    { kind =
        TypeKind.TypeVar(
          { id = 0
            instance = None
            bound = None }
        )
      provenance = None }

  open Escalier.Data.Syntax

  let start = Position("source.esc", 0, 0, 0)
  let stop = Position("source.esc", 5, 0, 5)

  let expr: Expr =
    { kind = Identifier("foo")
      span = { start = start; stop = stop }
      inferred_type = Some(t) }

  t.provenance <- Some(Provenance.Expr(expr))
