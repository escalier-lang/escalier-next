module Escalier.Compiler.Prelude

open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.TypeChecker.Env

let never =
  { Kind = TypeKind.Keyword Keyword.Never
    Provenance = None }

let makeParam (name: string) (ty: Type) : FuncParam =
  { Pattern = Pattern.Identifier { Name = name; IsMut = false }
    Type = ty
    Optional = false }

let getGlobalEnv () : Env =
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
        { Name = QualifiedIdent.Ident "A"
          TypeArgs = None
          Scheme = None }
        |> TypeKind.TypeRef
      Provenance = None }

  let typeRefB =
    { Kind =
        { Name = QualifiedIdent.Ident "B"
          TypeArgs = None
          Scheme = None }
        |> TypeKind.TypeRef
      Provenance = None }

  let arithemtic (op: string) : Binding =
    let t =
      makeFunctionType
        (Some [ tpA; tpB ])
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        { Kind =
            TypeKind.Binary
              { Op = op
                Left = typeRefA
                Right = typeRefB }
          Provenance = None }
        never

    { Type = t
      Mutable = false
      Export = false }

  let unaryArithmetic (op: string) : Binding =
    let t =
      makeFunctionType
        (Some [ tpA ])
        [ makeParam "arg" typeRefA ]
        { Kind = TypeKind.Unary { Op = op; Arg = typeRefA }
          Provenance = None }
        never

    { Type = t
      Mutable = false
      Export = false }

  let comparison (op: string) : Binding =
    let t =
      makeFunctionType
        (Some [ tpA; tpB ])
        [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
        { Kind =
            TypeKind.Binary
              { Op = op
                Left = typeRefA
                Right = typeRefB }
          Provenance = None }
        never

    { Type = t
      Mutable = false
      Export = false }

  let logical =
    { Type =
        makeFunctionType
          None
          [ makeParam "left" boolType; makeParam "right" boolType ]
          boolType
          never
      Mutable = false
      Export = false }

  let typeRefA =
    { Kind = makeTypeRefKind (QualifiedIdent.Ident "A")
      Provenance = None }

  let typeRefB =
    { Kind = makeTypeRefKind (QualifiedIdent.Ident "B")
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
    { Type =
        makeFunctionType
          (Some(typeParams))
          [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
          boolType
          never
      Mutable = false
      Export = false }

  let typeParams: list<TypeParam> =
    [ { Name = "A"
        Constraint = None
        Default = None } ]

  let unaryLogic (op: string) =
    { Type =
        makeFunctionType
          (Some(typeParams))
          [ makeParam "arg" typeRefA ]
          { Kind = TypeKind.Unary { Op = op; Arg = typeRefA }
            Provenance = None }
          never
      Mutable = false
      Export = false }

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
        { Name = (QualifiedIdent.Ident "A")
          TypeArgs = None
          Scheme = None }
        |> TypeKind.TypeRef
      Provenance = None }

  let typeRefB =
    { Kind =
        { Name = (QualifiedIdent.Ident "B")
          TypeArgs = None
          Scheme = None }
        |> TypeKind.TypeRef
      Provenance = None }

  let stringConcat =
    { Type =
        makeFunctionType
          (Some [ tpA; tpB ])
          [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
          { Kind =
              TypeKind.Binary
                { Op = "++"
                  Left = typeRefA
                  Right = typeRefB }
            Provenance = None }
          never
      Mutable = false
      Export = false }

  let binaryOps: Map<string, Binding> =
    Map.ofList
      [ ("+", arithemtic "+")
        ("++", stringConcat)
        ("-", arithemtic "-")
        ("*", arithemtic "*")
        ("/", arithemtic "/")
        ("%", arithemtic "%")
        ("**", arithemtic "**")
        ("<", comparison "<")
        ("<=", comparison "<=")
        (">", comparison ">")
        (">=", comparison ">=")
        ("==", equality)
        ("!=", equality)
        ("||", logical)
        ("&&", logical) ]

  let unaryOps =
    Map.ofList
      [ ("-", unaryArithmetic "-")
        ("+", unaryArithmetic "+")
        ("!", unaryLogic "!") ]

  let mutable ns = Namespace.empty

  let t =
    { Kind = TypeKind.Keyword Keyword.GlobalThis
      Provenance = None }

  let binding =
    { Type = t
      Mutable = false
      Export = false }

  ns <- ns.AddBinding "globalThis" binding

  // TODO: add a global `gql` function that returns a typed result
  // gql should return a TypedDocumentNode<TResult, TVariables> from
  // https://github.com/dotansimha/graphql-typed-document-node/blob/master/packages/core/src/index.ts
  // we don't actually care what the shape of the DocumentNode is that
  // TypeDocumentNode<TResult, TVariable> extends.  For our purposes, it's
  // okay if we treat DocumentNode as an opaque type.
  //
  // when dealing with fragments, we need to extract the TRsult from the
  // TypedDocumentNode of the fragment and merge it with the result of the
  // query.
  //
  // To start with we can manually construct TResult and TVariables and test
  // that TypedDocumentNode and types for `useQuery` are working as expected.

  let mutable env =
    { Filename = "<empty>"
      Namespace = ns
      BinaryOps = binaryOps
      UnaryOps = unaryOps
      IsAsync = false
      IsPatternMatching = false }

  env

let mutable envMemoized: Env option = None

let getGlobalEnvMemoized () =
  match envMemoized with
  | Some(e) -> e
  | None ->
    let env = getGlobalEnv ()
    envMemoized <- Some(env)
    env
