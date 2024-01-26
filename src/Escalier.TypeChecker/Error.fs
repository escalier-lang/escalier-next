namespace Escalier.TypeChecker

open Escalier.Data.Type

module Error =
  type TypeError =
    | NotImplemented of string
    | SemanticError of string
    | NotInferred
    | TypeMismatch of Type * Type
    | PropertyMissing of PropName
    | RecursiveUnification of Type * Type
    | WrongNumberOfTypeArgs

    override this.ToString() =
      match this with
      | NotImplemented s -> $"NotImplemented: {s}"
      | SemanticError s -> $"SemanticError: {s}"
      | NotInferred -> "NotInferred"
      | TypeMismatch(``type``, type1) -> $"TypeMismatch: {``type``} != {type1}"
      | PropertyMissing propName -> $"Object is missing property {propName}"
      | RecursiveUnification(``type``, type1) ->
        $"RecursiveUnification: {``type``} != {type1}"
      | WrongNumberOfTypeArgs -> "WrongNumberOfTypeArgs"

  type Diagnostic =
    { Description: string
      Reasons: list<TypeError> }
