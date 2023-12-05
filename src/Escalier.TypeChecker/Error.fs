namespace Escalier.TypeChecker

open Escalier.Data.Type

module Error =
  type TypeError =
    | NotImplemented of string
    | SemanticError of string
    | NotInferred
    | TypeMismatch of Type * Type
    | RecursiveUnification
    | WrongNumberOfTypeArgs

  type Diagnostic = { Reasons: list<TypeError> }
