namespace Escalier.TypeChecker

open Escalier.Data.Type

module TypeVariable =
  let mutable next_id = 0

  let reset () = next_id <- 0

  // TODO: remove `env` as a param
  let new_type_var (bound: option<Type>) =
    let t =
      { Type.kind =
          TypeKind.TypeVar(
            { id = next_id
              bound = bound
              instance = None }
          )
        provenance = None }

    next_id <- next_id + 1

    t
