namespace Escalier.TypeChecker

open Escalier.Data.Type

module Env =
  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>
  type SchemeAssump = (string * Scheme)

  type Env =
    { schemes: Map<string, Scheme>
      values: Map<string, Binding>
      isAsync: bool
      nonGeneric: Set<int> }
