namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type

open Error
open Env
open Helpers
open InferExpr

module rec InferTypeAnn =
  let inferTypeAnn
    (ctx: Ctx)
    (env: Env)
    (typeAnn: TypeAnn)
    : Result<Type, TypeError> =
    let kind: Result<TypeKind, TypeError> =
      result {
        match typeAnn.Kind with
        | TypeAnnKind.Literal lit -> return TypeKind.Literal(lit)
        | TypeAnnKind.Keyword keyword ->
          match keyword with
          | KeywordTypeAnn.Boolean ->
            return TypeKind.Primitive Primitive.Boolean
          | KeywordTypeAnn.Number -> return TypeKind.Primitive Primitive.Number
          | KeywordTypeAnn.BigInt -> return TypeKind.Primitive Primitive.BigInt
          | KeywordTypeAnn.String -> return TypeKind.Primitive Primitive.String
          | KeywordTypeAnn.Symbol -> return TypeKind.Primitive Primitive.Symbol
          | KeywordTypeAnn.UniqueSymbol -> return ctx.FreshSymbol().Kind
          | KeywordTypeAnn.Null -> return TypeKind.Literal(Literal.Null)
          | KeywordTypeAnn.Undefined ->
            return TypeKind.Literal(Literal.Undefined)
          | KeywordTypeAnn.Unknown -> return TypeKind.Keyword Keyword.Unknown
          | KeywordTypeAnn.Never -> return TypeKind.Keyword Keyword.Never
          | KeywordTypeAnn.Object -> return TypeKind.Keyword Keyword.Object
          | KeywordTypeAnn.Any ->
            let t = ctx.FreshTypeVar None None
            return t.Kind
        | TypeAnnKind.Object { Elems = elems
                               Immutable = immutable
                               Exact = exact } ->
          let mutable newEnv = env

          match newEnv.Namespace.Schemes.TryFind "Self" with
          | Some _ -> ()
          | None ->
            let scheme =
              { TypeParams = None
                Type = ctx.FreshTypeVar None None }

            newEnv <- newEnv.AddScheme "Self" scheme

          let! elems =
            List.traverseResultM (inferObjTypeAnnElem ctx newEnv) elems

          return
            TypeKind.Object
              { Extends = None
                Implements = None
                Elems = elems
                Exact = exact
                Immutable = immutable
                Mutable = false // TODO
                Interface = false
                Nominal = false }
        | TypeAnnKind.Tuple { Elems = elems; Immutable = immutable } ->
          let! elems = List.traverseResultM (inferTypeAnn ctx env) elems

          return
            TypeKind.Tuple
              { Elems = elems
                Immutable = immutable
                Mutable = false }
        | TypeAnnKind.Union types ->
          let! types = List.traverseResultM (inferTypeAnn ctx env) types
          return (union types).Kind
        | TypeAnnKind.Intersection types ->
          let! types = List.traverseResultM (inferTypeAnn ctx env) types
          return TypeKind.Intersection types
        | TypeAnnKind.TypeRef typeRef -> return! inferTypeRef ctx env typeRef
        | TypeAnnKind.Function functionType ->
          let! f = inferFuncSig ctx env functionType None
          return TypeKind.Function(f)
        | TypeAnnKind.Keyof target ->
          return! inferTypeAnn ctx env target |> Result.map TypeKind.KeyOf
        | TypeAnnKind.Rest target ->
          return! inferTypeAnn ctx env target |> Result.map TypeKind.RestSpread
        | TypeAnnKind.Typeof target ->
          let! t = getQualifiedIdentType ctx env target
          return t.Kind
        | TypeAnnKind.Index { Target = target; Index = index } ->
          let! target = inferTypeAnn ctx env target
          let! index = inferTypeAnn ctx env index
          return TypeKind.Index { Target = target; Index = index }
        | TypeAnnKind.Condition conditionType ->
          let! check = inferTypeAnn ctx env conditionType.Check
          let! extends = inferTypeAnn ctx env conditionType.Extends
          let infers = findInfers extends

          // Adds placeholder types ot the environment so that we'll be able to
          // reference them in `trueType` and `falseType`.  They will be replaced
          // by `expandType` later.
          let mutable newEnv = env

          for infer in infers do
            let unknown =
              { Kind = TypeKind.Keyword Keyword.Unknown
                Provenance = None }

            let scheme = { TypeParams = None; Type = unknown }

            newEnv <- newEnv.AddScheme infer scheme

          let! trueType = inferTypeAnn ctx newEnv conditionType.TrueType
          let! falseType = inferTypeAnn ctx newEnv conditionType.FalseType

          return
            TypeKind.Condition
              { Check = check
                Extends = extends
                TrueType = trueType
                FalseType = falseType }
        | TypeAnnKind.Match _ ->
          return! Error(TypeError.NotImplemented "TODO: inferTypeAnn - Match") // TODO
        | TypeAnnKind.Infer name -> return TypeKind.Infer name
        | TypeAnnKind.Wildcard -> return TypeKind.Wildcard
        | TypeAnnKind.TemplateLiteral { Parts = parts; Exprs = exprs } ->
          let! exprs = List.traverseResultM (inferTypeAnn ctx env) exprs
          return TypeKind.TemplateLiteral { Parts = parts; Exprs = exprs }
        | TypeAnnKind.Intrinsic -> return TypeKind.Intrinsic
        | TypeAnnKind.ImportType _ ->
          return!
            Error(TypeError.NotImplemented "TODO: inferTypeAnn - ImportType")
      }

    let t: Result<Type, TypeError> =
      Result.map
        (fun kind ->
          let t =
            { Kind = kind
              Provenance = Some(Provenance.TypeAnn typeAnn) }

          typeAnn.InferredType <- Some(t)
          t)
        kind

    t


  let inferTypeRef
    (ctx: Ctx)
    (env: Env)
    ({ Ident = name; TypeArgs = typeArgs }: Syntax.TypeRef)
    : Result<TypeKind, TypeError> =
    result {
      match env.GetScheme name with
      | Ok scheme ->
        match typeArgs with
        | Some(typeArgs) ->
          let! typeArgs = List.traverseResultM (inferTypeAnn ctx env) typeArgs

          return
            { Name = name
              TypeArgs = Some(typeArgs)
              Mutable = false // TODO
              Scheme = Some scheme }
            |> TypeKind.TypeRef
        | None ->
          // TODO: check if scheme required type args
          return
            { Name = name
              TypeArgs = None
              Mutable = false // TODO
              Scheme = Some scheme }
            |> TypeKind.TypeRef
      | Error _ ->
        printfn "Can't find 'Self' in env"

        match name with
        | QualifiedIdent.Ident "_" ->
          printfn "inferring '_' as TypeKind.Wildcard"
          return TypeKind.Wildcard
        | _ -> return! Error(TypeError.SemanticError $"{name} is not in scope")
    }

  let inferObjTypeAnnElem
    (ctx: Ctx)
    (env: Env)
    (elem: ObjTypeAnnElem)
    : Result<ObjTypeElem, TypeError> =

    result {
      match elem with
      | ObjTypeAnnElem.Property { Name = name
                                  TypeAnn = typeAnn
                                  Value = value
                                  Optional = optional
                                  Readonly = readonly } ->
        let! t =
          match typeAnn, value with
          | Some typeAnn, Some value ->
            // TODO: infer `value`'s type and then unify with `typeAnn`'s type
            inferTypeAnn ctx env typeAnn
          | Some typeAnn, None -> inferTypeAnn ctx env typeAnn
          | None, Some value -> ctx.InferExpr ctx env None value
          | None, None -> Error(TypeError.SemanticError "Invalid property")

        let! name = inferPropName ctx env name

        return
          Property
            { Name = name
              Type = t
              Optional = optional
              Readonly = readonly }
      | ObjTypeAnnElem.Callable functionType ->
        let! f = inferFuncSig ctx env functionType None
        return Callable f
      | ObjTypeAnnElem.Constructor functionType ->
        let! f = inferFuncSig ctx env functionType None
        return Constructor f
      | ObjTypeAnnElem.Method { Name = name; Type = methodType } ->
        let! fn = inferFuncSig ctx env methodType None
        let! name = inferPropName ctx env name
        return Method { Name = name; Fn = fn }
      | ObjTypeAnnElem.Getter { Name = name
                                ReturnType = retType
                                Throws = throws } ->
        let f: FuncSig =
          { TypeParams = None
            Self = None
            ParamList = []
            ReturnType = Some retType
            Throws = throws
            IsAsync = false }

        let! fn = inferFuncSig ctx env f None
        let! name = inferPropName ctx env name
        return Getter { Name = name; Fn = fn }
      | ObjTypeAnnElem.Setter { Name = name
                                Param = param
                                Throws = throws } ->

        let undefined =
          { Kind = Keyword KeywordTypeAnn.Undefined
            Span = DUMMY_SPAN
            InferredType = None }

        let f: FuncSig =
          { TypeParams = None
            Self = None
            ParamList = [ param ]
            ReturnType = Some undefined
            Throws = throws
            IsAsync = false }

        let! fn = inferFuncSig ctx env f None
        let! name = inferPropName ctx env name
        return Setter { Name = name; Fn = fn }
      | ObjTypeAnnElem.Mapped mapped ->
        let! c = inferTypeAnn ctx env mapped.TypeParam.Constraint

        let param =
          { Name = mapped.TypeParam.Name
            Constraint = c }

        let newEnv = env.AddScheme param.Name { TypeParams = None; Type = c }

        let! typeAnn = inferTypeAnn ctx newEnv mapped.TypeAnn

        let! nameType =
          match mapped.Name with
          | Some(name) -> inferTypeAnn ctx newEnv name |> Result.map Some
          | None -> Ok None

        return
          Mapped
            { TypeParam = param
              NameType = nameType
              TypeAnn = typeAnn
              Optional = mapped.Optional
              Readonly = mapped.Readonly }

      | ObjTypeAnnElem.Spread spread ->
        let! typeAnn = inferTypeAnn ctx env spread.Arg

        return RestSpread typeAnn
    }
