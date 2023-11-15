namespace Escalier.TypeChecker

open Escalier.Data.Type

module TypeVariable =
  let mutable nextId = 0

  let reset () = nextId <- 0

  // TODO: remove `env` as a param
  let newTypeVar (bound: option<Type>) =
    let t =
      { Type.Kind =
          TypeKind.TypeVar(
            { Id = nextId
              Bound = bound
              Instance = None }
          )
        Provenance = None }

    nextId <- nextId + 1

    t
