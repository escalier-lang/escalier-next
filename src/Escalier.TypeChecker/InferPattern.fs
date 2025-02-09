namespace Escalier.TypeChecker


open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type

open Error
open Env
open Unify
open InferExpr
open Helpers

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
                  Mutable = false
                  Interface = false }
            Provenance = Some(Provenance.Pattern pat) }

        objType
      | PatternKind.Tuple { Elems = elems; Immutable = immutable } ->
        let elems = List.map infer_pattern_rec elems

        { Kind =
            TypeKind.Tuple
              { Elems = elems
                Immutable = immutable
                Mutable = false }
          Provenance = None }
      | PatternKind.Extractor { Name = name; Args = args } ->
        let extractorType = getQualifiedIdentType ctx env name

        let symbolGlobal =
          match env.TryFindValue "Symbol" with
          | Some binding -> binding.Type
          | None -> failwith "Symbol not in scope"

        printfn $"symbolGlobal = {symbolGlobal}"

        let symbol =
          match
            getPropType
              ctx
              env
              symbolGlobal
              (PropName.String "customMatch")
              false
              ValueCategory.RValue
          with
          | Result.Ok sym -> sym
          | Result.Error _ -> failwith "Symbol.customMatch not found"

        let propName =
          match symbol.Kind with
          | TypeKind.UniqueSymbol id -> PropName.Symbol id
          | _ -> failwith "Symbol.customMatch is not a unique symbol"

        // TODO: check if `extractorType` is a class with a `[Symbol.customMatch]()` method
        let method =
          match extractorType with
          | Result.Ok t ->
            let mutable method: option<Function> = None

            match (prune t).Kind with
            | TypeKind.Object objElems ->
              for elem in objElems.Elems do
                match elem with
                | ObjTypeElem.Method { Name = name; Fn = f } when
                  name = propName
                  ->
                  method <- Some f
                | _ -> ()

              failwith "TODO: find [Symbol.customMatch]() method"
            | _ -> ()

            method
          | Result.Error _ -> failwith "Can't find enum type"

        match method with
        | Some method ->
          { Kind =
              TypeKind.Extractor
                { Name = name
                  Extractor = method
                  Args = List.map infer_pattern_rec args }
            Provenance = None }
        | None -> failwith "Can't find [Symbol.customMatch]() method"
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
