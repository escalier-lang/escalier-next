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

  let printDiagnostic (d: Diagnostic) =
    let rec printReasons (rs: list<TypeError>) =
      match rs with
      | [] -> ()
      | r :: rs ->
        printReason r
        printReasons rs

    and printReason (r: TypeError) =
      match r with
      | NotImplemented s -> printf "- Not implemented: %s\n" s
      | SemanticError s -> printf "- Semantic error: %s\n" s
      | NotInferred -> printf "- Type could not be inferred\n"
      | TypeMismatch(t1, t2) -> printf $"- Type mismatch: {t1} and {t2}\n"
      | RecursiveUnification(t1, t2) ->
        printf "- Recursive unification: {t1} and {t2}\n"
      | WrongNumberOfTypeArgs -> printf "- Wrong number of type arguments\n"

    printf "ERROR: %s\n" d.Description
  // printReasons d.Reasons

  let rec printDiagnostics (ds: list<Diagnostic>) =
    match ds with
    | [] -> ()
    | d :: ds ->
      printDiagnostic d
      printDiagnostics ds
