namespace Escalier.HindleyMilner

open Type

module Errors =
  type TypeError =
    | NotImplemented of string
    | SemanticError of string
    | NotInferred
    | TypeMismatch of Type * Type
    | RecursiveUnification
    | WrongNumberOfTypeArgs
