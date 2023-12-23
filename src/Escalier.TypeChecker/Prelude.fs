namespace Escalier.TypeChecker

open Escalier.Data.Type

open Env

// TODO: move the prelude into its own file so that Provenance
// can be set to something
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

    let arithemtic (op: string) =
      (makeFunctionType
        (Some [ tpA; tpB ])
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        { Kind = TypeKind.Binary(typeRefA, op, typeRefB)
          Provenance = None },
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
      { Kind = makeTypeRefKind "A"
        Provenance = None }

    let typeRefB =
      { Kind = makeTypeRefKind "B"
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
        { Kind = TypeKind.Binary(typeRefA, "++", typeRefB)
          Provenance = None },
       false)

    let Promise =
      { Kind = makeTypeRefKind "Promise"
        Provenance = None }

    let promiseScheme: Scheme =
      { Type = Promise
        TypeParams = None
        IsTypeParam = false }

    { Env.Values =
        Map.ofList
          [ ("+", arithemtic "+")
            ("++", stringConcat)
            ("-", arithemtic "-")
            ("*", arithemtic "*")
            ("/", arithemtic "/")
            ("%", arithemtic "%")
            ("**", arithemtic "**")
            ("<", comparison)
            ("<=", comparison)
            (">", comparison)
            (">=", comparison)
            ("==", equality)
            ("!=", equality)
            ("||", logical)
            ("&&", logical) ]
      Env.Schemes = Map([ ("Promise", promiseScheme) ])
      Env.IsAsync = false }
