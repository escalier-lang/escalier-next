namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type

open Error
open Env
open Poly
open Unify

module InferPattern =
  let inferPattern
    (ctx: Ctx)
    (env: Env)
    (pat: Syntax.Pattern)
    : Result<BindingAssump * Type, TypeError> =
    let mutable assump = BindingAssump([])

    // TODO: update to return a result
    let rec infer_pattern_rec (pat: Syntax.Pattern) : Type =
      match pat.Kind with
      | PatternKind.Ident({ Name = name
                            IsMut = isMut
                            Assertion = assertion }) ->
        let t =
          match assertion with
          | Some qi ->
            let assertType =
              match qi with
              | QualifiedIdent.Ident "string" -> strType
              | QualifiedIdent.Ident "number" -> numType
              | QualifiedIdent.Ident "boolean" -> boolType
              | _ -> failwith $"TODO: lookup type of {qi}"

            assertType
          | None -> ctx.FreshTypeVar None None

        let binding =
          { Type = t
            Mutable = isMut
            Export = false }

        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(name, binding)
        pat.InferredType <- Some t
        t
      | PatternKind.Literal lit ->
        { Kind = TypeKind.Literal lit
          Provenance = None }
      | PatternKind.Object { Elems = elems; Immutable = immutable } ->
        let elems: list<ObjTypeElem> =
          List.choose
            (fun (elem: Syntax.ObjPatElem) ->
              match elem with
              | Syntax.ObjPatElem.KeyValuePat { Key = key
                                                Value = value
                                                Default = _ } ->
                let t = infer_pattern_rec value

                Some(
                  ObjTypeElem.Property
                    { Name = PropName.String key
                      Optional = false
                      Readonly = false
                      Type = t }
                )
              | Syntax.ObjPatElem.ShorthandPat({ Name = name
                                                 IsMut = isMut
                                                 Assertion = assertion } as pat) ->
                // TODO: lookup `assertion` in `env`

                let t =
                  match assertion with
                  | Some qi ->
                    let assertType =
                      match qi with
                      | QualifiedIdent.Ident "string" -> strType
                      | QualifiedIdent.Ident "number" -> numType
                      | QualifiedIdent.Ident "boolean" -> boolType
                      | _ -> failwith $"TODO: lookup type of {qi}"

                    assertType
                  | None -> ctx.FreshTypeVar None None

                let binding =
                  { Type = t
                    Mutable = isMut
                    Export = false }

                // TODO: check if `name` already exists in `assump`
                assump <- assump.Add(name, binding)
                pat.Inferred <- Some t

                Some(
                  ObjTypeElem.Property
                    { Name = PropName.String name
                      Optional = false
                      Readonly = false
                      Type = t }
                )

              | Syntax.ObjPatElem.RestPat { Target = target; IsMut = _ } ->
                Some(ObjTypeElem.RestSpread(infer_pattern_rec target)))
            elems

        let objType =
          { Kind =
              TypeKind.Object
                { Extends = None
                  Implements = None
                  Elems = elems
                  Exact = true // TODO: This should depend what the pattern is matching/destructuring
                  Immutable = immutable
                  Interface = false }
            Provenance = Some(Provenance.Pattern pat) }

        objType
      | PatternKind.Tuple { Elems = elems; Immutable = immutable } ->
        let elems = List.map infer_pattern_rec elems

        { Kind = TypeKind.Tuple { Elems = elems; Immutable = immutable }
          Provenance = None }
      | PatternKind.Enum variant ->
        printfn $"Looking up tag for {variant.Ident}"
        let tagType = Infer.getQualifiedIdentType ctx env variant.Ident

        let tagType =
          match tagType with
          | Result.Ok t ->
            let elems: list<ObjTypeElem> =
              [ ObjTypeElem.Property
                  { Name = PropName.String "__TAG__"
                    Optional = false
                    Readonly = true
                    Type = t } ]

            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = elems
                    Exact = true // TODO: This should depend what the pattern is matching/destructuring
                    Immutable = true
                    Interface = false }
              Provenance = None }
          | Result.Error _ -> failwith "Can't find enum type"

        let argType = variant.Arg |> Option.map infer_pattern_rec

        // This is the type inferred from the enum pattern
        let patternType =
          match argType with
          | Some argType ->
            { Kind = TypeKind.Intersection [ tagType; argType ]
              Provenance = None }
          | None -> tagType

        // TODO: stop using QualifiedIdent for Enum variants
        let enumName, variantName =
          match variant.Ident with
          | QualifiedIdent.Member(QualifiedIdent.Ident qualifier, name) ->
            (qualifier, name)
          | _ -> failwith "This should never happen"

        match env.GetScheme(QualifiedIdent.Ident enumName) with
        | Ok scheme ->
          let t = instantiateType ctx scheme.Type scheme.TypeParams None

          let t =
            match t with
            | Ok t -> t
            | Error _ -> failwith "Failed to instantiate enum scheme"

          let result = unify ctx env None patternType t

          match result with
          | Ok _ -> patternType
          | Error _ -> failwith $"Failed to unify {patternType} and {t}"

        | Error _ -> failwith $"Can't find scheme for {variant.Ident}"

      | PatternKind.Wildcard { Assertion = assertion } ->
        match assertion with
        | Some qi ->
          let assertType =
            match qi with
            | QualifiedIdent.Ident "string" -> strType
            | QualifiedIdent.Ident "number" -> numType
            | QualifiedIdent.Ident "boolean" -> boolType
            | _ -> failwith $"TODO: lookup type of {qi}"

          assertType
        | None ->
          { Kind = TypeKind.Wildcard
            Provenance = None }
      | PatternKind.Rest pat ->
        { Kind = TypeKind.RestSpread(infer_pattern_rec pat)
          Provenance = None }

    // | PatternKind.Is(span, binding, isName, isMut) ->
    //   match Map.tryFind isName env.Schemes with
    //   | Some(scheme) ->
    //     assump <- assump.Add(binding.Name, (scheme.Type, binding.IsMut))
    //     scheme.Type
    //   | None -> failwith "todo"

    let t = infer_pattern_rec pat

    pat.InferredType <- Some(t)
    t.Provenance <- Some(Provenance.Pattern pat)

    Result.Ok((assump, t))
