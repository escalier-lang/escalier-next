namespace Escalier.TypeChecker

open Escalier.Data.Type

open Env

module Prelude =

  let makeParam (name: string) (ty: Type) : FuncParam =
    { Pattern = Pattern.Identifier name
      Type = ty
      Optional = false }

  let getEnv () : Env =
    let tpA =
      { Name = "A"
        Constraint = Some(numType)
        Default = None }

    let tpB =
      { Name = "B"
        Constraint = Some(numType)
        Default = None }

    let typeRefA =
      { Kind =
          { Name = "A"
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let typeRefB =
      { Kind =
          { Name = "B"
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let arithemtic =
      (makeFunctionType
        (Some [ tpA; tpB ])
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
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

    let tpA =
      { Name = "A"
        Constraint = Some(strType)
        Default = None }

    let tpB =
      { Name = "B"
        Constraint = Some(strType)
        Default = None }

    let typeRefA =
      { Kind =
          { Name = "A"
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let typeRefB =
      { Kind =
          { Name = "B"
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef
        Provenance = None }

    let stringConcat =
      (makeFunctionType
        (Some [ tpA; tpB ])
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        strType,
       false)

    let t = intersection [ fst arithemtic; fst stringConcat ]

    { Env.Values =
        Map.ofList
          [ ("+", (t, false))
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
