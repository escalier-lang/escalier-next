namespace Escalier.TypeChecker

open Escalier.Data.Type

open Env

module Prelude =

  let makeParam (name: string) (ty: Type) : FuncParam =
    { Pattern = Pattern.Identifier name
      Type = ty
      Optional = false }

  let getEnv () : Env =
    let arithemtic =
      (makeFunctionType
        None
        [ makeParam "left" numType; makeParam "right" numType ]
        numType,
       false)

    let comparison =
      (makeFunctionType
        None
        [ makeParam "left" numType; makeParam "right" numType ]
        boolType,
       false)

    let logical =
      (makeFunctionType
        None
        [ makeParam "left" boolType; makeParam "right" boolType ]
        boolType,
       false)

    let typeRefA =
      { Kind = makePrimitiveKind "A"
        Provenance = None }

    let typeRefB =
      { Kind = makePrimitiveKind "B"
        Provenance = None }

    let typeParams: list<TypeParam> =
      [ { Name = "A"
          Constraint = None
          Default = None }
        { Name = "B"
          Constraint = None
          Default = None } ]

    // TODO: figure out how to make quality polymorphic
    let equality =
      (makeFunctionType
        (Some(typeParams))
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        boolType,
       false)

    { Env.Values =
        Map.ofList
          [ ("+", arithemtic)
            ("-", arithemtic)
            ("*", arithemtic)
            ("/", arithemtic)
            ("%", arithemtic)
            ("**", arithemtic)
            ("<", comparison)
            ("<=", comparison)
            (">", comparison)
            (">=", comparison)
            ("==", equality)
            ("!=", equality)
            ("||", logical)
            ("&&", logical) ]
      Env.Schemes = Map([])
      Env.IsAsync = false }
