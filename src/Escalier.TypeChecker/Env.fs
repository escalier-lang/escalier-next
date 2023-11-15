namespace Escalier.TypeChecker

open Escalier.Data.Type

module Env =
  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>
  type SchemeAssump = (string * Scheme)

  type Env =
    { Schemes: Map<string, Scheme>
      Values: Map<string, Binding>
      IsAsync: bool
      NonGeneric: Set<int> }
