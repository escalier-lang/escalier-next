namespace Escalier.Interop

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Type
open Escalier.Data.Common
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Unify
open Escalier.TypeChecker.Prelude
open Escalier.Interop.TypeScript

// NOTES:
// - we need to track global decls and module decls separately
// - this is kind of tricky because module decls might reference global decls
// - we also need to create placeholder decls for everything in the module as
//   opposed to how we're handling scripts where we infer top-level decls one at
//   a time

module rec Infer =
  let rec printTsEntityName (name: TsEntityName) : string =
    match name with
    | TsEntityName.Identifier id -> id.Name
    | TsEntityName.TsQualifiedName { Left = left; Right = right } ->
      sprintf "%s.%s" (printTsEntityName left) right.Name

  let inferFnParam (ctx: Ctx) (env: Env) (param: TsFnParam) : FuncParam =
    let typeAnn =
      match param.TypeAnn with
      | Some(t) -> inferTsTypeAnn ctx env t
      | None -> ctx.FreshTypeVar None

    let pat =
      match param.Pat with
      | TsFnParamPat.Ident binding -> Pat.Ident binding
      | TsFnParamPat.Array arrayPat -> Pat.Array arrayPat
      | TsFnParamPat.Rest restPat -> Pat.Rest restPat
      | TsFnParamPat.Object objectPat -> Pat.Object objectPat

    let pat = patToPattern env pat

    { Pattern = pat
      Type = typeAnn
      Optional = false }

  let inferTypeElement
    (ctx: Ctx)
    (env: Env)
    (elem: TsTypeElement)
    : ObjTypeElem =
    match elem with
    | TsCallSignatureDecl tsCallSignatureDecl ->
      let typeParams = None // TODO: handle type params

      let paramList =
        tsCallSignatureDecl.Params |> List.map (inferFnParam ctx env)

      let returnType =
        match tsCallSignatureDecl.TypeAnn with
        | Some(typeAnn) -> inferTsTypeAnn ctx env typeAnn
        | None -> ctx.FreshTypeVar None

      let throws =
        { Kind = TypeKind.Keyword Keyword.Never
          Provenance = None }

      let f: Type.Function =
        { TypeParams = typeParams
          ParamList = paramList
          Return = returnType
          Throws = throws }

      ObjTypeElem.Callable f
    | TsConstructSignatureDecl tsConstructSignatureDecl ->
      let typeParams = None // TODO: handle type params

      let paramList =
        tsConstructSignatureDecl.Params |> List.map (inferFnParam ctx env)

      let returnType =
        match tsConstructSignatureDecl.TypeAnn with
        | Some(typeAnn) -> inferTsTypeAnn ctx env typeAnn
        | None -> ctx.FreshTypeVar None

      let throws =
        { Kind = TypeKind.Keyword Keyword.Never
          Provenance = None }

      let f: Type.Function =
        { TypeParams = typeParams
          ParamList = paramList
          Return = returnType
          Throws = throws }

      ObjTypeElem.Constructor f
    | TsPropertySignature tsPropertySignature ->
      // TODO: handle computed keys
      let name =
        match tsPropertySignature.Key with
        | Expr.Ident id -> id.Name
        | Expr.Lit(Lit.Str str) -> str.Value
        | Expr.Lit(Lit.Num num) -> num.Value |> string
        | _ -> failwith "TODO: computed property name"

      let t = inferTsTypeAnn ctx env tsPropertySignature.TypeAnn

      let property =
        { Name = name
          Optional = tsPropertySignature.Optional
          Readonly = tsPropertySignature.Readonly
          Type = t }

      ObjTypeElem.Property(property)
    | TsGetterSignature tsGetterSignature ->
      // TODO: handle computed keys
      let name =
        match tsGetterSignature.Key with
        | Expr.Ident id -> id.Name
        | Expr.Lit(Lit.Str str) -> str.Value
        | Expr.Lit(Lit.Num num) -> num.Value |> string
        | _ -> failwith "TODO: computed property name"

      let returnType =
        match tsGetterSignature.TypeAnn with
        | Some(typeAnn) -> inferTsTypeAnn ctx env typeAnn
        | None -> ctx.FreshTypeVar None

      let throws =
        { Kind = TypeKind.Keyword Keyword.Never
          Provenance = None }

      ObjTypeElem.Getter(name, returnType, throws)
    | TsSetterSignature tsSetterSignature ->
      // TODO: handle computed keys
      let name =
        match tsSetterSignature.Key with
        | Expr.Ident id -> id.Name
        | Expr.Lit(Lit.Str str) -> str.Value
        | Expr.Lit(Lit.Num num) -> num.Value |> string
        | _ -> failwith "TODO: computed property name"

      let param = inferFnParam ctx env tsSetterSignature.Param

      let throws =
        { Kind = TypeKind.Keyword Keyword.Never
          Provenance = None }

      ObjTypeElem.Setter(name, param, throws)
    | TsMethodSignature tsMethodSignature ->
      let name =
        match tsMethodSignature.Key with
        | Expr.Ident id -> id.Name
        | Expr.Lit(Lit.Str str) -> str.Value
        | Expr.Lit(Lit.Num num) -> num.Value |> string
        | _ -> failwith "TODO: computed property name"

      let typeParams = None // TODO: handle type params

      let paramList =
        tsMethodSignature.Params |> List.map (inferFnParam ctx env)

      let returnType =
        match tsMethodSignature.TypeAnn with
        | Some(typeAnn) -> inferTsTypeAnn ctx env typeAnn
        | None -> ctx.FreshTypeVar None

      let throws =
        { Kind = TypeKind.Keyword Keyword.Never
          Provenance = None }

      let f: Type.Function =
        { TypeParams = typeParams
          ParamList = paramList
          Return = returnType
          Throws = throws }

      ObjTypeElem.Method(name, false, f)
    | TsIndexSignature tsIndexSignature ->
      let readonly =
        match tsIndexSignature.Readonly with
        | true -> Some(MappedModifier.Add)
        | false -> Some(MappedModifier.Remove)

      let optional = None

      let param: IndexParam =
        { Name = tsIndexSignature.Param.Name.Name
          Constraint = inferTsType ctx env tsIndexSignature.Param.Constraint }

      let mapped: Mapped =
        { TypeParam = param
          NameType = None // TODO
          TypeAnn = inferTsTypeAnn ctx env tsIndexSignature.TypeAnn
          Optional = optional
          Readonly = readonly }

      // TODO: handle tsIndexSignature.IsStatic
      ObjTypeElem.Mapped mapped

  let inferTsType (ctx: Ctx) (env: Env) (t: TsType) : Type =
    let kind =
      match t with
      | TsType.TsKeywordType keyword ->
        match keyword.Kind with
        | TsAnyKeyword ->
          let t = ctx.FreshTypeVar None
          t.Kind // TODO: find a better way to do this
        | TsUnknownKeyword -> TypeKind.Keyword Keyword.Unknown
        | TsNumberKeyword -> TypeKind.Primitive Primitive.Number
        | TsObjectKeyword -> TypeKind.Keyword Keyword.Object
        | TsBooleanKeyword -> TypeKind.Primitive Primitive.Boolean
        | TsBigIntKeyword -> failwith "TODO: TsBigIntKeyword"
        | TsStringKeyword -> TypeKind.Primitive Primitive.String
        | TsSymbolKeyword -> makeTypeRefKind "symbol"
        // TODO: figure out if Escalier needs its own `void` type
        | TsVoidKeyword -> TypeKind.Literal(Literal.Undefined)
        | TsUndefinedKeyword -> TypeKind.Literal(Literal.Undefined)
        | TsNullKeyword -> TypeKind.Literal(Literal.Null)
        | TsNeverKeyword -> TypeKind.Keyword Keyword.Never
        | TsIntrinsicKeyword -> failwith "TODO: TsIntrinsicKeyword"

      | TsType.TsThisType tsThisType ->
        // TODO: use a TypeRef for this, but we need to know what `this`
        // is reference so we can create a Scheme for it
        { Name = "Self"
          TypeArgs = None
          Scheme = None }
        |> TypeKind.TypeRef
      | TsType.TsFnOrConstructorType tsFnOrConstructorType ->
        match tsFnOrConstructorType with
        | TsFnOrConstructorType.TsFnType f ->
          let typeParams =
            f.TypeParams
            |> Option.map (fun typeParamDecl ->
              typeParamDecl.Params
              |> List.map (fun typeParam ->
                { Name = typeParam.Name.Name
                  Constraint =
                    Option.map (inferTsType ctx env) typeParam.Constraint
                  Default = Option.map (inferTsType ctx env) typeParam.Default }))

          let paramList: list<FuncParam> =
            f.Params |> List.map (inferFnParam ctx env)

          let retType = inferTsTypeAnn ctx env f.TypeAnn

          let t = makeFunctionType typeParams paramList retType never
          t.Kind // TODO: find a better way to do this
        | TsFnOrConstructorType.TsConstructorType f ->
          let typeParams =
            f.TypeParams
            |> Option.map (fun typeParamDecl ->
              typeParamDecl.Params
              |> List.map (fun typeParam ->
                { Name = typeParam.Name.Name
                  Constraint =
                    Option.map (inferTsType ctx env) typeParam.Constraint
                  Default = Option.map (inferTsType ctx env) typeParam.Default }))

          let paramList: list<FuncParam> =
            f.Params |> List.map (inferFnParam ctx env)

          let retType = inferTsTypeAnn ctx env f.TypeAnn

          let t = makeFunctionType typeParams paramList retType never
          t.Kind // TODO: find a better way to do this
      | TsType.TsTypeRef tsTypeRef ->
        let typeArgs =
          tsTypeRef.TypeParams
          |> Option.map (fun ({ Params = typeParams }) ->
            typeParams |> List.map (fun t -> inferTsType ctx env t))

        let kind =
          { Name = printTsEntityName tsTypeRef.TypeName
            TypeArgs = typeArgs
            Scheme = None }
          |> TypeKind.TypeRef

        kind
      | TsType.TsTypeQuery tsTypeQuery ->
        failwith "TODO: inferTsType - TsTypeQuery"
      | TsType.TsTypeLit tsTypeLit ->
        let elems = tsTypeLit.Members |> List.map (inferTypeElement ctx env)
        TypeKind.Object elems
      | TsType.TsArrayType tsArrayType ->
        TypeKind.Array(inferTsType ctx env tsArrayType.ElemType)
      | TsType.TsTupleType tsTupleType ->
        TypeKind.Tuple(
          List.map
            (fun elem -> inferTsType ctx env elem.Type)
            tsTupleType.ElemTypes
        )
      | TsType.TsOptionalType tsOptionalType ->
        failwith "TODO: inferTsType - TsOptionalType"
      | TsType.TsRestType tsRestType ->
        TypeKind.Rest(inferTsType ctx env tsRestType.TypeAnn)
      | TsType.TsUnionOrIntersectionType tsUnionOrIntersectionType ->
        match tsUnionOrIntersectionType with
        | TsIntersectionType { Types = types } ->
          let types = types |> List.map (fun t -> inferTsType ctx env t)
          TypeKind.Intersection types
        | TsUnionType { Types = types } ->
          let t = union (List.map (inferTsType ctx env) types)
          t.Kind // TODO: find a better way to do this
      | TsType.TsConditionalType tsConditionalType ->
        let checkType = inferTsType ctx env tsConditionalType.CheckType
        let extendsType = inferTsType ctx env tsConditionalType.ExtendsType
        let trueType = inferTsType ctx env tsConditionalType.TrueType
        let falseType = inferTsType ctx env tsConditionalType.FalseType
        TypeKind.Condition(checkType, extendsType, trueType, falseType)
      | TsType.TsInferType tsInferType ->
        TypeKind.Infer tsInferType.TypeParam.Name.Name
      | TsType.TsParenthesizedType tsParenthesizedType ->
        let t = inferTsType ctx env tsParenthesizedType.TypeAnn
        t.Kind // TODO: find a better way to do this
      | TsType.TsTypeOperator tsTypeOperator ->
        match tsTypeOperator.Op with
        | TsTypeOperatorOp.KeyOf ->
          TypeKind.KeyOf(inferTsType ctx env tsTypeOperator.TypeAnn)
        | TsTypeOperatorOp.Unique -> failwith "TODO: inferTsType - Unique"
        | TsTypeOperatorOp.Readonly ->
          // TODO: Add support for readonly types
          let t = inferTsType ctx env tsTypeOperator.TypeAnn
          t.Kind // TODO: find a better way to do this
      | TsType.TsIndexedAccessType tsIndexedAccessType ->
        let objType = inferTsType ctx env tsIndexedAccessType.ObjType
        let indexType = inferTsType ctx env tsIndexedAccessType.IndexType
        TypeKind.Index(objType, indexType)
      | TsType.TsMappedType tsMappedType ->

        let param: IndexParam =
          { Name = tsMappedType.TypeParam.Name.Name
            Constraint =
              match tsMappedType.TypeParam.Constraint with
              | Some(c) -> inferTsType ctx env c
              | None ->
                failwith "TODO: Mapped type's type param must have a constraint" }

        let mapped =
          { TypeParam = param
            NameType = None // TODO
            TypeAnn = inferTsType ctx env tsMappedType.TypeAnn
            Optional = None
            Readonly = None }

        let elem = ObjTypeElem.Mapped mapped

        TypeKind.Object [ elem ]
      | TsType.TsLitType tsLitType ->
        let lit =
          match tsLitType.Lit with
          | Number num -> Literal.Number(num.Value)
          | Str str -> Literal.String str.Value
          | Bool bool -> Literal.Boolean bool.Value
          | Tpl tsTplLitType -> failwith "TODO: inferTsType - TsTplLitType"

        TypeKind.Literal lit
      | TsType.TsTypePredicate tsTypePredicate ->
        // TODO: add proper support for type predicates
        boolType.Kind
      | TsType.TsImportType tsImportType ->
        failwith "TODO: inferTsType - TsImportType"

    { Kind = kind; Provenance = None }


  let inferTsTypeAnn (ctx: Ctx) (env: Env) (ta: TsTypeAnn) : Type =
    inferTsType ctx env ta.TypeAnn

  let inferPattern
    (ctx: Ctx)
    (env: Env)
    (p: Pat)
    : Result<BindingAssump * Type, TypeError> =
    result {
      let mutable assump = BindingAssump([])

      let rec infer_pattern_rec (pat: Pat) : Type =
        match pat with
        | Pat.Ident id ->
          let t = ctx.FreshTypeVar None
          // TODO:
          let isMut = false
          // TODO: check if `name` already exists in `assump`
          assump <- assump.Add(id.Id.Name, (t, isMut))
          t
        | Pat.Array arrayPat -> failwith "TODO: infer_pattern_rec - Array"
        | Pat.Rest restPat -> failwith "TODO: infer_pattern_rec - Rest"
        | Pat.Object objectPat -> failwith "TODO: infer_pattern_rec - Object"
        | Pat.Assign assignPat -> failwith "TODO: infer_pattern_rec - Assign"
        | Pat.Invalid invalid -> failwith "TODO: infer_pattern_rec - Invalid"
        // TODO: remove Pat.Expr from Pat
        | Pat.Expr expr -> failwith "TODO: infer_pattern_rec - Expr"

      let t = infer_pattern_rec p
      return (assump, t)
    }

  let patToPattern (env: Env) (p: Pat) : Pattern =
    match p with
    | Pat.Ident ident -> Pattern.Identifier ident.Id.Name
    | Pat.Array arrayPat ->
      let elems = arrayPat.Elems |> List.map (Option.map <| (patToPattern env))
      Pattern.Tuple elems
    | Pat.Rest rest -> patToPattern env rest.Arg |> Pattern.Rest
    | Pat.Object objectPat ->
      let elems: list<ObjPatElem> =
        objectPat.Props
        |> List.map (fun prop ->
          match prop with
          | ObjectPatProp.KeyValue { Key = key; Value = value } ->
            let key =
              match key with
              | PropName.Ident id -> id.Name
              | PropName.Str str -> str.Value
              | PropName.Num num -> num.Value |> string
              | PropName.Computed computedPropName ->
                failwith "TODO: computed property name"

            ObjPatElem.KeyValuePat(key, patToPattern env value, None)
          | ObjectPatProp.Assign { Key = key; Value = _ } ->
            ObjPatElem.ShorthandPat(key.Name, None)
          | ObjectPatProp.Rest { Arg = arg } ->
            patToPattern env arg |> ObjPatElem.RestPat)

      Pattern.Object elems
    // TODO: add assign patterns to Escalier's AST
    | Pat.Assign assignPat -> failwith "TODO: patToPattern - Assign"
    | Pat.Invalid invalid -> failwith "TODO: patToPattern - Invalid"
    // TODO: remove Pat.Expr from Pat
    | Pat.Expr expr -> failwith "TODO: remove Pat.Expr from Expr"

  let inferFunction (ctx: Ctx) (env: Env) (f: Function) : Type =
    let typeParams =
      f.TypeParams
      |> Option.map (fun typeParamDecl ->
        typeParamDecl.Params
        |> List.map (fun typeParam ->
          { Name = typeParam.Name.Name
            Constraint = Option.map (inferTsType ctx env) typeParam.Constraint
            Default = Option.map (inferTsType ctx env) typeParam.Default }))

    let paramList: list<FuncParam> =
      f.Params
      |> List.map (fun param ->
        let typeAnn =
          match param.TypeAnn with
          | Some(t) -> inferTsTypeAnn ctx env t
          | None -> ctx.FreshTypeVar None

        let pat = patToPattern env param.Pat

        { Pattern = pat
          Type = typeAnn
          Optional = false })

    let ret =
      match f.ReturnType with
      | Some(retType) -> inferTsTypeAnn ctx env retType
      | None -> failwith "Function decl has no return type"

    makeFunctionType typeParams paramList ret never

  // TODO: generalize inferred types
  // NOTE: some types with free variables will not be function types which
  // means the type checker will need to be able to instantiate generic types
  // that aren't function types
  let inferDecl (ctx: Ctx) (env: Env) (decl: Decl) : Result<Env, TypeError> =
    result {

      let mutable newEnv = env

      match decl with
      | Decl.Class classDecl -> failwith "TODO: classDecl"
      | Decl.Fn fnDecl ->
        let t = inferFunction ctx env fnDecl.Fn
        let name = fnDecl.Id.Name
        let isMut = false
        newEnv <- env.AddValue name (t, isMut)
      | Decl.Var varDecl ->
        for decl in varDecl.Decls do
          let! assumps, patType = inferPattern ctx env decl.Id

          for Operators.KeyValue(name, binding) in assumps do
            newEnv <- env.AddValue name binding

          match decl.TypeAnn with
          | Some(typeAnn) ->
            let typeAnnType = inferTsTypeAnn ctx env typeAnn
            do! unify ctx env typeAnnType patType
          | None -> ()

      | Decl.Using usingDecl -> failwith "TODO: usingDecl"
      | Decl.TsInterface tsInterfaceDecl ->
        // TODO: handle type params
        // TODO: handle extends
        let elems =
          tsInterfaceDecl.Body.Body |> List.map (inferTypeElement ctx env)

        let t =
          { Kind = TypeKind.Object elems
            Provenance = None }

        let scheme =
          { TypeParams = None
            Type = t
            IsTypeParam = false }
        // TODO: handle interface merging
        newEnv <- env.AddScheme tsInterfaceDecl.Id.Name scheme
      | Decl.TsTypeAlias decl ->
        let typeParams = None
        let t = inferTsType ctx env decl.TypeAnn

        let scheme =
          { TypeParams = typeParams
            Type = t
            IsTypeParam = false }

        // TODO: if decl.Global is true, add to the global env
        newEnv <- env.AddScheme decl.Id.Name scheme
      | Decl.TsEnum tsEnumDecl -> failwith "TODO: tsEnumDecl"
      | Decl.TsModule tsModuleDecl ->
        // TODO: handle namespaces
        ()

      return newEnv
    }

  let inferStmt (ctx: Ctx) (env: Env) (stmt: Stmt) : Result<Env, TypeError> =
    result {
      match stmt with
      | Stmt.Decl decl -> return! inferDecl ctx env decl
      | _ -> return env // .d.ts files shouldn't have any other statement kinds
    }

  let inferModuleDecl (env: Env) (decl: ModuleDecl) : Result<Env, TypeError> =
    result {
      match decl with
      | ModuleDecl.Import importDecl ->
        failwith "TODO: inferModuleDecl - importDecl"
      | ModuleDecl.ExportDecl exportDecl ->
        failwith "TODO: inferModuleDecl - exportDecl"
      | ModuleDecl.ExportNamed namedExport ->
        failwith "TODO: inferModuleDecl - namedExport"
      | ModuleDecl.ExportDefaultDecl exportDefaultDecl ->
        failwith "TODO: inferModuleDecl - exportDefaultDecl"
      | ModuleDecl.ExportDefaultExpr exportDefaultExpr ->
        failwith "TODO: inferModuleDecl - exportDefaultExpr"
      | ModuleDecl.ExportAll exportAll ->
        failwith "TODO: inferModuleDecl - exportAll"
      | ModuleDecl.TsImportEquals tsImportEqualsDecl ->
        failwith "TODO: inferModuleDecl - tsImportEqualsDecl"
      | ModuleDecl.TsExportAssignment tsExportAssignment ->
        failwith "TODO: inferModuleDecl - tsExportAssignment"
      | ModuleDecl.TsNamespaceExport tsNamespaceExportDecl ->
        failwith "TODO: inferModuleDecl - tsNamespaceExportDecl"

      return env
    }

  let inferModuleItem
    (ctx: Ctx)
    (env: Env)
    (item: ModuleItem)
    : Result<Env, TypeError> =
    result {
      match item with
      | ModuleItem.Stmt stmt -> return! inferStmt ctx env stmt
      | ModuleItem.ModuleDecl decl -> return env
    }

  let inferModule (ctx: Ctx) (env: Env) (m: Module) : Result<Env, TypeError> =
    result {
      let mutable newEnv = env

      for item in m.Body do
        let! env = inferModuleItem ctx newEnv item
        newEnv <- env

      return newEnv
    }
