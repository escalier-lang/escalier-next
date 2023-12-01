namespace Escalier.Interop

open FsToolkit.ErrorHandling

open Escalier.Data.Type
open Escalier.Data.Common
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Unify
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

  let inferTsType (env: Env) (t: TsType) : Type =
    let kind =
      match t with
      | TsType.TsKeywordType keyword ->
        match keyword.Kind with
        | TsAnyKeyword ->
          let t = TypeVariable.makeVariable None
          t.Kind // TODO: find a better way to do this
        | TsUnknownKeyword -> makePrimitiveKind "unknown"
        | TsNumberKeyword -> makePrimitiveKind "number"
        | TsObjectKeyword -> makePrimitiveKind "object"
        | TsBooleanKeyword -> makePrimitiveKind "boolean"
        | TsBigIntKeyword -> failwith "TODO: TsBigIntKeyword"
        | TsStringKeyword -> makePrimitiveKind "string"
        | TsSymbolKeyword -> makePrimitiveKind "symbol"
        | TsVoidKeyword -> failwith "TODO: TsVoidKeyword"
        | TsUndefinedKeyword -> TypeKind.Literal(Literal.Undefined)
        | TsNullKeyword -> TypeKind.Literal(Literal.Null)
        | TsNeverKeyword -> makePrimitiveKind "never"
        | TsIntrinsicKeyword -> failwith "TODO: TsIntrinsicKeyword"

      | TsType.TsThisType tsThisType ->
        // TODO: use a TypeRef for this, but we need to know what `this`
        // is reference so we can create a Scheme for it
        failwith "TODO: inferTsType - TsThisType"
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
                    Option.map (inferTsType env) typeParam.Constraint
                  Default = Option.map (inferTsType env) typeParam.Default }))

          let paramList: list<FuncParam> =
            f.Params
            |> List.map (fun param ->
              let typeAnn =
                match param.TypeAnn with
                | Some(t) -> inferTsTypeAnn env t
                | None -> TypeVariable.makeVariable None

              let pat =
                match param.Pat with
                | TsFnParamPat.Ident binding -> Pat.Ident binding
                | TsFnParamPat.Array arrayPat -> Pat.Array arrayPat
                | TsFnParamPat.Rest restPat -> Pat.Rest restPat
                | TsFnParamPat.Object objectPat -> Pat.Object objectPat

              let pat = patToPattern env pat

              { Pattern = pat
                Type = typeAnn
                Optional = false })

          let retType = inferTsTypeAnn env f.TypeAnn

          let t = makeFunctionType typeParams paramList retType
          t.Kind // TODO: find a better way to do this
        | TsFnOrConstructorType.TsConstructorType f ->
          let typeParams =
            f.TypeParams
            |> Option.map (fun typeParamDecl ->
              typeParamDecl.Params
              |> List.map (fun typeParam ->
                { Name = typeParam.Name.Name
                  Constraint =
                    Option.map (inferTsType env) typeParam.Constraint
                  Default = Option.map (inferTsType env) typeParam.Default }))

          let paramList: list<FuncParam> =
            f.Params
            |> List.map (fun param ->
              let typeAnn =
                match param.TypeAnn with
                | Some(t) -> inferTsTypeAnn env t
                | None -> TypeVariable.makeVariable None

              let pat =
                match param.Pat with
                | TsFnParamPat.Ident binding -> Pat.Ident binding
                | TsFnParamPat.Array arrayPat -> Pat.Array arrayPat
                | TsFnParamPat.Rest restPat -> Pat.Rest restPat
                | TsFnParamPat.Object objectPat -> Pat.Object objectPat

              let pat = patToPattern env pat

              { Pattern = pat
                Type = typeAnn
                Optional = false })

          let retType = inferTsTypeAnn env f.TypeAnn

          let t = makeFunctionType typeParams paramList retType
          t.Kind // TODO: find a better way to do this
      | TsType.TsTypeRef tsTypeRef ->
        let kind =
          { Name = printTsEntityName tsTypeRef.TypeName
            TypeArgs = None
            Scheme = None }
          |> TypeKind.TypeRef

        kind
      | TsType.TsTypeQuery tsTypeQuery ->
        failwith "TODO: inferTsType - TsTypeQuery"
      | TsType.TsTypeLit tsTypeLit ->
        // NOTE: this makes to TypeKind.Object
        failwith "TODO: inferTsType - TsTypeLit"
      | TsType.TsArrayType tsArrayType ->
        TypeKind.Array(inferTsType env tsArrayType.ElemType)
      | TsType.TsTupleType tsTupleType ->
        TypeKind.Tuple(
          List.map (fun elem -> inferTsType env elem.Type) tsTupleType.ElemTypes
        )
      | TsType.TsOptionalType tsOptionalType ->
        failwith "TODO: inferTsType - TsOptionalType"
      | TsType.TsRestType tsRestType ->
        TypeKind.Rest(inferTsType env tsRestType.TypeAnn)
      | TsType.TsUnionOrIntersectionType tsUnionOrIntersectionType ->
        match tsUnionOrIntersectionType with
        | TsIntersectionType { Types = types } ->
          let types = types |> List.map (fun t -> inferTsType env t)
          TypeKind.Intersection types
        | TsUnionType { Types = types } ->
          let t = union (List.map (inferTsType env) types)
          t.Kind // TODO: find a better way to do this
      | TsType.TsConditionalType tsConditionalType ->
        let checkType = inferTsType env tsConditionalType.CheckType
        let extendsType = inferTsType env tsConditionalType.ExtendsType
        let trueType = inferTsType env tsConditionalType.TrueType
        let falseType = inferTsType env tsConditionalType.FalseType
        TypeKind.Condition(checkType, extendsType, trueType, falseType)
      | TsType.TsInferType tsInferType ->
        TypeKind.Infer tsInferType.TypeParam.Name.Name
      | TsType.TsParenthesizedType tsParenthesizedType ->
        let t = inferTsType env tsParenthesizedType.TypeAnn
        t.Kind // TODO: find a better way to do this
      | TsType.TsTypeOperator tsTypeOperator ->
        match tsTypeOperator.Op with
        | TsTypeOperatorOp.KeyOf ->
          TypeKind.KeyOf(inferTsType env tsTypeOperator.TypeAnn)
        | TsTypeOperatorOp.Unique -> failwith "TODO: inferTsType - Unique"
        | TsTypeOperatorOp.Readonly -> failwith "TODO: inferTsType - Readonly"
      | TsType.TsIndexedAccessType tsIndexedAccessType ->
        let objType = inferTsType env tsIndexedAccessType.ObjType
        let indexType = inferTsType env tsIndexedAccessType.IndexType
        TypeKind.Index(objType, indexType)
      | TsType.TsMappedType tsMappedType ->
        failwith "TODO: inferTsType - TsMappedType"
      | TsType.TsLitType tsLitType ->
        let lit =
          match tsLitType.Lit with
          | Number num -> Literal.Number(num.Value |> string)
          | Str str -> Literal.String str.Value
          | Bool bool -> Literal.Boolean bool.Value
          | Tpl tsTplLitType -> failwith "TODO: inferTsType - TsTplLitType"

        TypeKind.Literal lit
      | TsType.TsTypePredicate tsTypePredicate ->
        failwith "TODO: inferTsType - TsTypePredicate"
      | TsType.TsImportType tsImportType ->
        failwith "TODO: inferTsType - TsImportType"

    { Kind = kind; Provenance = None }


  let inferTsTypeAnn (env: Env) (ta: TsTypeAnn) : Type =
    inferTsType env ta.TypeAnn

  let inferPattern
    (env: Env)
    (p: Pat)
    : Result<BindingAssump * Type, TypeError> =
    result {
      let mutable assump = BindingAssump([])

      let rec infer_pattern_rec (pat: Pat) : Type =
        match pat with
        | Pat.Ident id ->
          let t = TypeVariable.makeVariable None
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

  let inferFunction (env: Env) (f: Function) : Type =
    let typeParams =
      f.TypeParams
      |> Option.map (fun typeParamDecl ->
        typeParamDecl.Params
        |> List.map (fun typeParam ->
          { Name = typeParam.Name.Name
            Constraint = Option.map (inferTsType env) typeParam.Constraint
            Default = Option.map (inferTsType env) typeParam.Default }))

    let paramList: list<FuncParam> =
      f.Params
      |> List.map (fun param ->
        let typeAnn =
          match param.TypeAnn with
          | Some(t) -> inferTsTypeAnn env t
          | None -> TypeVariable.makeVariable None

        let pat = patToPattern env param.Pat

        { Pattern = pat
          Type = typeAnn
          Optional = false })

    let ret =
      match f.ReturnType with
      | Some(retType) -> inferTsTypeAnn env retType
      | None -> failwith "Function decl has no return type"

    makeFunctionType typeParams paramList ret

  // TODO: generalize inferred types
  // NOTE: some types with free variables will not be function types which
  // means the type checker will need to be able to instantiate generic types
  // that aren't function types
  let inferDecl (env: Env) (decl: Decl) : Result<Env, TypeError> =
    result {

      let mutable newEnv = env

      match decl with
      | Decl.Class classDecl -> failwith "TODO: classDecl"
      | Decl.Fn fnDecl ->
        let t = inferFunction env fnDecl.Fn
        let name = fnDecl.Id.Name
        let isMut = false
        newEnv <- env.AddValue name (t, isMut)
      | Decl.Var varDecl ->
        for decl in varDecl.Decls do
          let! assumps, patType = inferPattern env decl.Id

          for Operators.KeyValue(name, binding) in assumps do
            newEnv <- env.AddValue name binding

          match decl.TypeAnn with
          | Some(typeAnn) ->
            let typeAnnType = inferTsTypeAnn env typeAnn
            do! unify env typeAnnType patType
          | None -> ()

      | Decl.Using usingDecl -> failwith "TODO: usingDecl"
      | Decl.TsInterface tsInterfaceDecl ->
        // TODO: handle interface merging
        failwith "TODO: tsInterfaceDecl"
      | Decl.TsTypeAlias decl ->
        let typeParams = None
        let t = inferTsType env decl.TypeAnn
        let scheme = { TypeParams = typeParams; Type = t }

        // TODO: if decl.Global is true, add to the global env
        newEnv <- env.AddScheme decl.Id.Name scheme
      | Decl.TsEnum tsEnumDecl -> failwith "TODO: tsEnumDecl"
      | Decl.TsModule tsModuleDecl -> failwith "TODO: tsModuleDecl"

      return newEnv
    }

  let inferStmt (env: Env) (stmt: Stmt) : Result<Env, TypeError> =
    result {
      match stmt with
      | Stmt.Decl decl -> return! inferDecl env decl
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

  let inferModuleItem (env: Env) (item: ModuleItem) : Result<Env, TypeError> =
    result {
      match item with
      | ModuleItem.Stmt stmt -> return! inferStmt env stmt
      | ModuleItem.ModuleDecl decl -> return env
    }

  let inferModule (env: Env) (m: Module) : Result<Env, TypeError> =
    result {
      let mutable newEnv = env

      for item in m.Body do
        let! env = inferModuleItem newEnv item
        newEnv <- env

      return newEnv
    }
