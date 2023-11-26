namespace Escalier.TypeChecker

open Escalier.Data.Type

module Prelude =

  let makeParam (name: string) (ty: Type) : FuncParam =
    { Pattern = Pattern.Identifier name
      Type = ty
      Optional = false }

  let getEnv () : TypeChecker.Env =
    let arithemtic =
      (TypeChecker.makeFunctionType
        None
        [ makeParam "left" TypeChecker.numType
          makeParam "right" TypeChecker.numType ]
        TypeChecker.numType,
       false)

    let comparison =
      (TypeChecker.makeFunctionType
        None
        [ makeParam "left" TypeChecker.numType
          makeParam "right" TypeChecker.numType ]
        TypeChecker.boolType,
       false)

    let logical =
      (TypeChecker.makeFunctionType
        None
        [ makeParam "left" TypeChecker.boolType
          makeParam "right" TypeChecker.boolType ]
        TypeChecker.boolType,
       false)

    let typeRefA =
      { Kind = TypeChecker.makePrimitiveKind "A"
        Provenance = None }

    let typeRefB =
      { Kind = TypeChecker.makePrimitiveKind "B"
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
      (TypeChecker.makeFunctionType
        (Some(typeParams))
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        TypeChecker.boolType,
       false)

    { TypeChecker.Env.Values =
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
      TypeChecker.Env.Schemes = Map([])
      TypeChecker.Env.IsAsync = false }
