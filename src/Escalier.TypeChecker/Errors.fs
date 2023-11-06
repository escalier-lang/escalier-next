namespace Escalier.TypeChecker

open Escalier.Data.Type

module Errors =
  type TypeError =
    | NotImplemented
    | NotInferred
    | TypeMismatch of Type * Type
    | RecursiveUnification
