namespace Escalier.Parser

module Say =
  let hello name = printfn "Hello %s" name

  open Escalier.Data.Type

  let t =
    { kind = TypeKind.TypeVar(id = 0, instance = None, bound = None)
      provenance = ref None }

  open Escalier.Data.Syntax

  let expr: Expr =
    { kind = Identifer("foo")
      span = { start = 0; stop = 5 }
      inferred_type = Some(t) }

  t.provenance.Value <- Some(Provenance.Expr(expr))
