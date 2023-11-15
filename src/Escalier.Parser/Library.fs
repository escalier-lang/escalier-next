namespace Escalier.Parser

open FParsec

module Say =
  let hello name = printfn $"Hello %s{name}"

  open Escalier.Data.Type

  let t =
    { Kind =
        TypeKind.TypeVar(
          { Id = 0
            Instance = None
            Bound = None }
        )
      Provenance = None }

  open Escalier.Data.Syntax

  let start = Position("source.esc", 0, 0, 0)
  let stop = Position("source.esc", 5, 0, 5)

  let expr: Expr =
    { Kind = Identifier("foo")
      Span = { Start = start; Stop = stop }
      InferredType = Some(t) }

  t.Provenance <- Some(Provenance.Expr(expr))
