namespace Escalier.TypeChecker

open Escalier.Data.Type

module Error =
  type TypeError =
    | NotImplemented of string
    | SemanticError of string
    | NotInferred
    | TypeMismatch of Type * Type
    | RecursiveUnification of Type * Type
    | WrongNumberOfTypeArgs

  type Diagnostic =
    { Description: string
      Reasons: list<TypeError> }
