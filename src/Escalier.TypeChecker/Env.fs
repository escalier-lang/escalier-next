namespace Escalier.TypeChecker

open Escalier.Data.Type

module Env =

  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>
  type SchemeAssump = string * Scheme

  type Env =
    { Values: Map<string, Binding>
      Schemes: Map<string, Scheme>
      IsAsync: bool }

    member this.AddValue (name: string) (binding: Binding) =
      { this with
          Values = Map.add name binding this.Values }

    member this.AddScheme (name: string) (s: Scheme) =
      { this with
          Schemes = Map.add name s this.Schemes }

  let makePrimitiveKind name =
    { Name = name
      TypeArgs = None
      Scheme = None }
    |> TypeKind.TypeRef

  let makeFunctionType typeParams paramList ret =
    let never =
      { Kind = makePrimitiveKind "never"
        Provenance = None }

    { Kind =
        Function
          { TypeParams = typeParams
            ParamList = paramList
            Return = ret
            Throws = never }
      Provenance = None }

  let numType =
    { Kind = makePrimitiveKind "number"
      Provenance = None }

  let boolType =
    { Kind = makePrimitiveKind "boolean"
      Provenance = None }

  let strType =
    { Kind = makePrimitiveKind "string"
      Provenance = None }
