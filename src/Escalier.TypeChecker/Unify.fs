namespace Escalier.TypeChecker

open Escalier.Data.Type
open Escalier.TypeChecker.Errors

module Unify =
  let unify (t1: Type) (t2: Type) : Result<unit, TypeError> =

    Ok(())
