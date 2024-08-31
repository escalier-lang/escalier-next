namespace Escalier.Interop

open Escalier.Data
open Escalier.Data.Syntax

open Escalier.Interop.TypeScript

module rec Migrate =
  type Ctx = { Declare: bool }

  let start = FParsec.Position("", 0, 1, 1)
  let stop = FParsec.Position("", 0, 1, 1)
  let DUMMY_SPAN: Syntax.Span = { Start = start; Stop = stop }

  let migrateExpr (expr: TypeScript.Expr) : Syntax.Expr =

    let kind =
      match expr with
      | Expr.This thisExpr -> failwith "todo"
      | Expr.Array arrayLit -> failwith "todo"
      | Expr.Object objectLit -> failwith "todo"
      | Expr.Fn fnExpr -> failwith "todo"
      | Expr.Unary unaryExpr -> failwith "todo"
      | Expr.Update updateExpr -> failwith "todo"
      | Expr.Bin binExpr -> failwith "todo"
      | Expr.Assign assignExpr -> failwith "todo"
      | Expr.Member memberExpr ->
        let obj = migrateExpr memberExpr.Object

        let prop =
          match memberExpr.Property with
          | Expr.Ident ident -> ident.Name
          | _ -> failwith "todo"

        ExprKind.Member
          { Target = obj
            Name = prop
            OptChain = false }
      | Expr.SuperProp superPropExpr -> failwith "todo"
      | Expr.Cond condExpr -> failwith "todo"
      | Expr.Call callExpr -> failwith "todo"
      | Expr.New newExpr -> failwith "todo"
      | Expr.Seq seqExpr -> failwith "todo"
      | Expr.Ident ident -> ExprKind.Identifier { Name = ident.Name }
      | Expr.Lit lit -> failwith "todo"
      | Expr.Tpl tpl -> failwith "todo"
      | Expr.TaggedTpl taggedTpl -> failwith "todo"
      | Expr.Arrow arrowExpr -> failwith "todo"
      | Expr.Class classExpr -> failwith "todo"
      | Expr.Yield yieldExpr -> failwith "todo"
      | Expr.MetaProp metaPropExpr -> failwith "todo"
      | Expr.Await awaitExpr -> failwith "todo"
      | Expr.Paren parenExpr -> failwith "todo"
      | Expr.JSXMember jsxMemberExpr -> failwith "todo"
      | Expr.JSXNamespacedName jsxNamespacedName -> failwith "todo"
      | Expr.JSXEmpty jsxEmptyExpr -> failwith "todo"
      | Expr.JSXElement jsxElement -> failwith "todo"
      | Expr.JSXFragment jsxFragment -> failwith "todo"
      | Expr.TsTypeAssertion tsTypeAssertion -> failwith "todo"
      | Expr.TsConstAssertion tsConstAssertion -> failwith "todo"
      | Expr.TsNonNull tsNonNullExpr -> failwith "todo"
      | Expr.TsAs tsAsExpr -> failwith "todo"
      | Expr.TsInstantiation tsInstantiation -> failwith "todo"
      | Expr.TsSatisfies tsSatisfiesExpr -> failwith "todo"
      | Expr.PrivateName privateName -> failwith "todo"
      | Expr.OptChain optChainExpr -> failwith "todo"
      | Expr.Invalid invalid -> failwith "todo"

    let expr: Syntax.Expr =
      { Kind = kind
        Span = DUMMY_SPAN
        InferredType = None }

    expr

  let makeSelfFuncParam (isMutable: bool) : FuncParam =
    let ident =
      { Name = "self"
        IsMut = isMutable
        Assertion = None }

    let pattern: Syntax.Pattern =
      { Kind = PatternKind.Ident ident
        Span = DUMMY_SPAN
        InferredType = None }

    let kind =
      { TypeRef.Ident = Common.QualifiedIdent.Ident "Self"
        TypeRef.TypeArgs = None }
      |> TypeRef

    let selfTypeAnn =
      { Kind = kind
        Span = DUMMY_SPAN
        InferredType = None }

    let self: Syntax.FuncParam =
      { Pattern = pattern
        TypeAnn = Some selfTypeAnn
        Optional = false }

    self

  let makeSelfFuncParamWithOptionalTypeann () : FuncParam =
    let ident =
      { Name = "self"
        IsMut = false
        Assertion = None }

    let pattern: Syntax.Pattern =
      { Kind = PatternKind.Ident ident
        Span = DUMMY_SPAN
        InferredType = None }

    let kind =
      { TypeRef.Ident = Common.QualifiedIdent.Ident "Self"
        TypeRef.TypeArgs = None }
      |> TypeRef

    let selfTypeAnn =
      { Kind = kind
        Span = DUMMY_SPAN
        InferredType = None }

    let self: Syntax.FuncParam =
      { Pattern = pattern
        TypeAnn = Some selfTypeAnn
        Optional = false }

    self

  let migrateTypeElement
    (isMutable: bool)
    (elem: TsTypeElement)
    : Syntax.ObjTypeAnnElem =
    match elem with
    | TsCallSignatureDecl { Params = fnParams
                            TypeAnn = typeAnn
                            TypeParams = typeParams } ->
      let typeParams =
        Option.map
          (fun (tpd: TsTypeParamDecl) -> List.map migrateTypeParam tpd.Params)
          typeParams

      let retType =
        match typeAnn with
        | Some t -> migrateTypeAnn t
        | None -> failwith "all callable signatures must have a return type"

      let f: FuncSig =
        { TypeParams = typeParams
          Self = Some(makeSelfFuncParam isMutable)
          ParamList = List.map (migrateFnParam isMutable) fnParams
          ReturnType = Some retType
          Throws = None
          IsAsync = false }

      ObjTypeAnnElem.Callable f
    | TsConstructSignatureDecl { Params = fnParams
                                 TypeAnn = typeAnn
                                 TypeParams = typeParams } ->
      let typeParams =
        Option.map
          (fun (tpd: TsTypeParamDecl) -> List.map migrateTypeParam tpd.Params)
          typeParams

      let retType =
        match typeAnn with
        | Some t -> migrateTypeAnn t
        | None -> failwith "all constructor signatures must have a return type"

      let f: FuncSig =
        { TypeParams = typeParams
          Self = Some(makeSelfFuncParam isMutable)
          ParamList = List.map (migrateFnParam isMutable) fnParams
          ReturnType = Some retType
          Throws = None
          IsAsync = false }

      ObjTypeAnnElem.Constructor f
    | TsPropertySignature { Key = key
                            TypeAnn = typeAnn
                            Optional = optional
                            Readonly = readonly
                            Computed = _computed } ->
      let name =
        match key with
        | Expr.Ident id -> PropName.String id.Name
        | Expr.Lit(Lit.Str str) -> PropName.String str.Value
        | Expr.Lit(Lit.Num num) -> PropName.Number(num.Value)
        | expr -> Syntax.PropName.Computed(migrateExpr expr)

      ObjTypeAnnElem.Property
        { Name = name
          TypeAnn = migrateTypeAnn typeAnn
          Optional = optional
          Readonly = readonly
          Static = false }
    | TsMethodSignature { Key = key
                          TypeParams = typeParams
                          Params = fnParams
                          TypeAnn = retType
                          Optional = _optional
                          Computed = _computed } ->

      let typeParams =
        Option.map
          (fun (tpd: TsTypeParamDecl) -> List.map migrateTypeParam tpd.Params)
          typeParams

      let retType =
        match retType with
        | Some t -> migrateTypeAnn t
        | None -> failwith "all method signatures must have a return type"

      let f: FuncSig =
        { TypeParams = typeParams
          Self = Some(makeSelfFuncParam isMutable)
          ParamList = List.map (migrateFnParam isMutable) fnParams
          ReturnType = Some retType
          Throws = None
          IsAsync = false }

      let name =
        match key with
        | Expr.Ident id -> PropName.String id.Name
        | Expr.Lit(Lit.Str str) -> PropName.String str.Value
        | Expr.Lit(Lit.Num num) -> PropName.Number(num.Value)
        | expr -> Syntax.PropName.Computed(migrateExpr expr)

      ObjTypeAnnElem.Method { Name = name; Type = f }
    | TsIndexSignature { Readonly = readonly
                         Param = typeParam
                         TypeAnn = typeAnn } ->
      let readonly =
        match readonly with
        | true -> Some Common.MappedModifier.Add
        | false -> None // Some(MappedModifier.Remove)

      let c = migrateType typeParam.Constraint

      let typeParam: IndexParam =
        { Name = typeParam.Name.Name
          Constraint = c }

      let mapped: Mapped =
        { TypeParam = typeParam
          Name = None // TODO: handle renaming
          Optional = Some Common.MappedModifier.Add
          Readonly = readonly
          TypeAnn = migrateTypeAnn typeAnn }

      ObjTypeAnnElem.Mapped mapped
    | TsGetterSignature { Key = key
                          TypeAnn = retType
                          Optional = _optional
                          Computed = _computed } ->
      let retType =
        match retType with
        | Some t -> migrateTypeAnn t
        | None -> failwith "all method signatures must have a return type"

      let name =
        match key with
        | Expr.Ident id -> PropName.String id.Name
        | Expr.Lit(Lit.Str str) -> PropName.String str.Value
        | Expr.Lit(Lit.Num num) -> PropName.Number(num.Value)
        | expr -> Syntax.PropName.Computed(migrateExpr expr)

      ObjTypeAnnElem.Getter
        { Name = name
          ReturnType = retType
          Throws = None }
    | TsSetterSignature { Key = key
                          Param = fnParam
                          Optional = _optional
                          Computed = _computed } ->
      // TODO: warn if there's a setter on a Readonly interface
      let fnParam = migrateFnParam isMutable fnParam

      let undefined: Syntax.TypeAnn =
        { Kind = Syntax.Keyword KeywordTypeAnn.Undefined
          Span = DUMMY_SPAN
          InferredType = None }

      let name =
        match key with
        | Expr.Ident id -> PropName.String id.Name
        | Expr.Lit(Lit.Str str) -> PropName.String str.Value
        | Expr.Lit(Lit.Num num) -> PropName.Number(num.Value)
        | expr -> Syntax.PropName.Computed(migrateExpr expr)

      ObjTypeAnnElem.Setter
        { Name = name
          Param = fnParam
          Throws = None }

  let migrateTypeAnn (typeAnn: TsTypeAnn) : TypeAnn =
    migrateType typeAnn.TypeAnn

  let migrateType (t: TypeScript.TsType) : TypeAnn =
    let kind: TypeAnnKind =
      match t with
      | TsType.TsKeywordType t ->
        match t.Kind with
        | TsAnyKeyword -> Wildcard
        | TsUnknownKeyword -> Keyword KeywordTypeAnn.Unknown
        | TsNumberKeyword -> Keyword KeywordTypeAnn.Number
        | TsObjectKeyword -> Keyword KeywordTypeAnn.Object
        | TsBooleanKeyword -> Keyword KeywordTypeAnn.Boolean
        | TsBigIntKeyword -> Keyword KeywordTypeAnn.BigInt
        | TsStringKeyword -> Keyword KeywordTypeAnn.String
        | TsSymbolKeyword -> Keyword KeywordTypeAnn.Symbol
        // Converting `void` to `undefined` is fine for .d.ts files but
        // won't work if we want to convert .ts files.
        | TsVoidKeyword -> Keyword KeywordTypeAnn.Undefined
        | TsUndefinedKeyword -> Keyword KeywordTypeAnn.Undefined
        | TsNullKeyword -> Keyword KeywordTypeAnn.Null
        | TsNeverKeyword -> Keyword KeywordTypeAnn.Never
        | TsIntrinsicKeyword -> TypeAnnKind.Intrinsic

      | TsType.TsThisType _ ->
        { TypeRef.Ident = Common.QualifiedIdent.Ident "Self"
          TypeRef.TypeArgs = None }
        |> TypeRef
      | TsType.TsFnOrConstructorType tsFnOrConstructorType ->
        // Assumes the entire object is mutable
        let isMutable = true

        match tsFnOrConstructorType with
        | TsFnType f ->
          // TsFnType is shorthand for an object with only a callable signature
          let typeParams =
            Option.map
              (fun (tpd: TsTypeParamDecl) ->
                List.map migrateTypeParam tpd.Params)
              f.TypeParams

          let paramList: list<FuncParam> =
            List.map (migrateFnParam isMutable) f.Params

          let fnType: FuncSig =
            { TypeParams = typeParams
              Self = None // TODO: check if first parameter has type `Self`
              ParamList = paramList
              ReturnType = Some(migrateTypeAnn f.TypeAnn)
              Throws = None
              IsAsync = false }

          TypeAnnKind.Function fnType
        | TsConstructorType f ->
          // TsConstructorType is shorthand for an object with only a newable signature
          let typeParams =
            Option.map
              (fun (tpd: TsTypeParamDecl) ->
                List.map migrateTypeParam tpd.Params)
              f.TypeParams

          let paramList = List.map (migrateFnParam isMutable) f.Params

          let fnType: FuncSig =
            { TypeParams = typeParams
              Self = None // TODO: check if first parameter has type `Self`
              ParamList = paramList
              ReturnType = Some(migrateTypeAnn f.TypeAnn)
              Throws = None
              IsAsync = false }

          TypeAnnKind.Function fnType
      | TsType.TsTypeRef { TypeName = typeName
                           TypeParams = typeParams } ->
        let typeArgs =
          Option.map
            (fun (tpi: TsTypeParamInstantiation) ->
              List.map migrateType tpi.Params)
            typeParams

        { TypeRef.Ident = entityNameToQualifiedIdent typeName
          TypeRef.TypeArgs = typeArgs }
        |> TypeRef
      | TsType.TsTypeQuery { ExprName = exprName
                             TypeArgs = _typeArgs } ->
        let name =
          match exprName with
          | TsEntityName entityName -> entityNameToQualifiedIdent entityName
          | Import _ -> failwith "TODO: handle typeof import"

        TypeAnnKind.Typeof name
      | TsType.TsTypeLit { Members = members } ->
        // Assumes that all methods on object types are mutable
        let isMutable = true

        let elems: list<ObjTypeAnnElem> =
          List.map (migrateTypeElement isMutable) members

        TypeAnnKind.Object
          { Elems = elems
            Immutable = false
            Exact = false }
      | TsType.TsArrayType { ElemType = elem } ->
        TypeAnnKind.Array(migrateType elem)
      | TsType.TsTupleType { ElemTypes = elems } ->
        let elemTypes = List.map (fun e -> migrateType e.Type) elems
        TypeAnnKind.Tuple { Elems = elemTypes; Immutable = false }
      | TsType.TsOptionalType _ -> failwith "TODO: migrate optional type"
      | TsType.TsRestType { TypeAnn = typeAnn } ->
        TypeAnnKind.Rest(migrateType typeAnn)
      | TsType.TsUnionOrIntersectionType unionOrIntersection ->
        match unionOrIntersection with
        | TsUnionType { Types = types } ->
          let typeAnnList = List.map migrateType types
          TypeAnnKind.Union typeAnnList
        | TsIntersectionType { Types = types } ->
          let typeAnnList = List.map migrateType types
          TypeAnnKind.Intersection typeAnnList
      | TsType.TsConditionalType { CheckType = check
                                   ExtendsType = extends
                                   TrueType = trueType
                                   FalseType = falseType } ->
        TypeAnnKind.Condition
          { Check = migrateType check
            Extends = migrateType extends
            TrueType = migrateType trueType
            FalseType = migrateType falseType }
      | TsType.TsInferType { TypeParam = typeParam } ->
        let name = typeParam.Name.Name
        TypeAnnKind.Infer name
      | TsType.TsParenthesizedType { TypeAnn = typeAnn } ->
        let t = migrateType typeAnn
        t.Kind
      | TsType.TsTypeOperator { Op = op; TypeAnn = typeAnn } ->
        match op with
        | TsTypeOperatorOp.KeyOf -> TypeAnnKind.Keyof(migrateType typeAnn)
        | TsTypeOperatorOp.Unique ->
          match typeAnn with
          | TsType.TsKeywordType { Kind = TsKeywordTypeKind.TsSymbolKeyword } ->
            TypeAnnKind.Keyword KeywordTypeAnn.UniqueSymbol
          | _ -> failwith "TODO: 'unique' is only valid with 'symbol' types"
        | TsTypeOperatorOp.Readonly ->
          // TODO: handle readonly types properly
          let t = migrateType typeAnn
          t.Kind
      | TsType.TsIndexedAccessType { ObjType = objType
                                     IndexType = indexType
                                     Readonly = _readonly } ->
        TypeAnnKind.Index
          { Target = migrateType objType
            Index = migrateType indexType }
      | TsType.TsMappedType { Readonly = readonly
                              TypeParam = typeParam
                              NameType = nameType
                              Optional = optional
                              TypeAnn = typeAnn } ->
        let optional =
          match optional with
          | Some TruePlusMinus.True -> Some Common.MappedModifier.Add
          | Some TruePlusMinus.Plus -> Some Common.MappedModifier.Add
          | Some TruePlusMinus.Minus -> Some Common.MappedModifier.Remove
          | None -> None

        let readonly =
          match readonly with
          | Some TruePlusMinus.True -> Some Common.MappedModifier.Add
          | Some TruePlusMinus.Plus -> Some Common.MappedModifier.Add
          | Some TruePlusMinus.Minus -> Some Common.MappedModifier.Remove
          | None -> None

        let c =
          match typeParam.Constraint with
          | Some c -> migrateType c
          | None ->
            failwith "mapped types must have a constraint on the key's type"

        let typeParam: IndexParam =
          { Name = typeParam.Name.Name
            Constraint = c }

        let mapped: Mapped =
          { TypeParam = typeParam
            Name = None // TODO: handle renaming
            Optional = optional
            Readonly = readonly
            TypeAnn = migrateType typeAnn }

        let elem: ObjTypeAnnElem = ObjTypeAnnElem.Mapped mapped

        TypeAnnKind.Object
          { Elems = [ elem ]
            Immutable = false
            Exact = false }
      | TsType.TsLitType { Lit = lit } ->
        match lit with
        | TsLit.Number { Value = value } ->
          Common.Literal.Number value |> TypeAnnKind.Literal
        | TsLit.Str { Value = value } ->
          Common.Literal.String value |> TypeAnnKind.Literal
        | TsLit.Bool { Value = value } ->
          Common.Literal.Boolean value |> TypeAnnKind.Literal
        | TsLit.Tpl { Types = types; Quasis = quasis } ->
          let parts: List<string> =
            List.map (fun (q: TplElement) -> q.Raw) quasis

          let exprs: List<TypeAnn> = List.map migrateType types

          TypeAnnKind.TemplateLiteral { Parts = parts; Exprs = exprs }
      | TsType.TsTypePredicate _ ->
        // TODO: add proper support for type predicates
        TypeAnnKind.Keyword KeywordTypeAnn.Boolean
      | TsType.TsImportType _ -> failwith "TODO: migrateType - TsImportType"

    { Kind = kind
      Span = DUMMY_SPAN
      InferredType = None }

  let entityNameToQualifiedIdent (name: TsEntityName) : Common.QualifiedIdent =
    match name with
    | TsEntityName.Identifier id -> Common.QualifiedIdent.Ident id.Name
    | TsEntityName.TsQualifiedName { Left = left; Right = right } ->
      Common.QualifiedIdent.Member(entityNameToQualifiedIdent left, right.Name)

  let migrateTypeParam (typeParam: TypeScript.TsTypeParam) : Syntax.TypeParam =
    { Name = typeParam.Name.Name
      Constraint = Option.map migrateType typeParam.Constraint
      Default = Option.map migrateType typeParam.Default
      Span = DUMMY_SPAN }

  let migrateFnParam
    (isMutable: bool)
    (fnParam: TypeScript.TsFnParam)
    : Syntax.FuncParam =
    let typeAnn =
      match fnParam.TypeAnn with
      | Some typeAnn -> Some(migrateTypeAnn typeAnn)
      | None -> failwith "all function parameters must have a type annotation"

    let isMutable =
      match typeAnn with
      | None -> isMutable
      | Some typeAnn ->
        match typeAnn.Kind with
        | TypeAnnKind.Literal _ -> false
        | TypeAnnKind.Keyword _ -> false
        | TypeAnnKind.Function _ -> false
        | TypeAnnKind.Keyof _ -> false
        | TypeAnnKind.TemplateLiteral _ -> false
        | _ -> isMutable

    { Pattern = migrateFnParamPattern isMutable fnParam.Pat
      TypeAnn =
        match fnParam.TypeAnn with
        | Some typeAnn -> Some(migrateTypeAnn typeAnn)
        | None -> failwith "all function parameters must have a type annotation"
      Optional = fnParam.Optional }

  let migrateFnParamPattern
    (isMutable: bool)
    (pat: TypeScript.TsFnParamPat)
    : Pattern =
    let kind =
      match pat with
      | TsFnParamPat.Ident { Id = ident } ->
        if ident.Name = "input" then
          printfn $"input is mutable: {isMutable}"

        PatternKind.Ident
          { Name = ident.Name
            IsMut = isMutable
            Assertion = None }
      | TsFnParamPat.Object { Props = props } ->
        let elems: list<ObjPatElem> =
          List.map
            (fun (prop: TypeScript.ObjectPatProp) ->
              match prop with
              | ObjectPatProp.KeyValue { Key = key; Value = value } ->
                let key =
                  match key with
                  | PropName.Ident id -> id.Name
                  | PropName.Str str -> str.Value
                  | PropName.Num num -> num.Value |> string
                  | PropName.Computed computedPropName ->
                    // TODO: update `key` to handle `unique symbol`s as well
                    failwith "TODO: computed property name"

                KeyValuePat
                  { Key = key
                    Value = migratePat value
                    Default = None // TODO
                    Span = DUMMY_SPAN }
              | ObjectPatProp.Assign { Key = key; Value = value } ->
                ShorthandPat
                  { Name = key.Name
                    IsMut = true
                    Assertion = None
                    Default = None // TODO
                    Span = DUMMY_SPAN
                    Inferred = None }
              | ObjectPatProp.Rest { Arg = arg } ->
                RestPat
                  { Target = migratePat arg
                    IsMut = true
                    Span = DUMMY_SPAN })
            props

        PatternKind.Object { Elems = elems; Immutable = false }
      | TsFnParamPat.Array { Elems = elems } ->
        let elems =
          List.map
            (fun (elem) ->
              match elem with
              | Some pat -> migratePat pat
              | None -> failwith "TODO: handle sparse array patterns")
            elems

        PatternKind.Tuple { Elems = elems; Immutable = false }
      | TsFnParamPat.Rest restPat -> PatternKind.Rest(migratePat restPat.Arg)

    { Kind = kind
      Span = DUMMY_SPAN
      InferredType = None }

  let migratePat (pat: TypeScript.Pat) : Pattern =
    let kind: PatternKind =
      match pat with
      | Pat.Ident { Id = ident } ->
        PatternKind.Ident
          { Name = ident.Name
            IsMut = true
            Assertion = None }
      | Pat.Array { Elems = elems } ->
        let elems =
          List.map
            (fun (elem) ->
              match elem with
              | Some pat -> migratePat pat
              | None -> failwith "TODO: handle sparse array patterns")
            elems

        PatternKind.Tuple { Elems = elems; Immutable = false }
      | Pat.Rest { Arg = arg } -> PatternKind.Rest(migratePat arg)
      | Pat.Object { Props = props } ->
        let elems: list<ObjPatElem> =
          List.map
            (fun (prop: TypeScript.ObjectPatProp) ->
              match prop with
              | ObjectPatProp.KeyValue { Key = key; Value = value } ->
                let key =
                  match key with
                  | PropName.Ident id -> id.Name
                  | PropName.Str str -> str.Value
                  | PropName.Num num -> num.Value |> string
                  | PropName.Computed computedPropName ->
                    // TODO: update `key` to handle `unique symbol`s as well
                    failwith "TODO: computed property name"

                KeyValuePat
                  { Key = key
                    Value = migratePat value
                    Default = None // TODO
                    Span = DUMMY_SPAN }
              | ObjectPatProp.Assign { Key = key; Value = value } ->
                ShorthandPat
                  { Name = key.Name
                    IsMut = true
                    Assertion = None
                    Default = None // TODO
                    Span = DUMMY_SPAN
                    Inferred = None }
              | ObjectPatProp.Rest { Arg = arg } ->
                RestPat
                  { Target = migratePat arg
                    IsMut = true
                    Span = DUMMY_SPAN })
            props

        PatternKind.Object { Elems = elems; Immutable = false }
      | Pat.Assign(_) -> failwith "TODO: handle assignment patterns"
      | Pat.Invalid(_) -> failwith "TODO: handle invalid patterns"

    { Kind = kind
      Span = DUMMY_SPAN
      InferredType = None }

  let rec migrateModuleDecl
    (ctx: Ctx)
    (decl: ModuleDecl)
    : list<Syntax.ModuleItem> =
    match decl with
    | ModuleDecl.Import { Specifiers = specifiers
                          Src = src
                          IsTypeOnly = _
                          With = _ } ->
      let specifiers =
        List.map
          (fun (specifier: ImportSpecifier) ->
            match specifier with
            | ImportSpecifier.Default { Local = local } ->
              Syntax.ImportSpecifier.Named
                { Name = "default"
                  Alias = Some local.Name }
            | ImportSpecifier.Named { Local = local; Imported = imported } ->
              match imported with
              | Some imported ->
                let name =
                  match imported with
                  | ModuleExportName.Ident id -> id.Name
                  | ModuleExportName.Str str -> str.Value

                Syntax.ImportSpecifier.Named
                  { Name = local.Name; Alias = Some name }
              | None ->
                Syntax.ImportSpecifier.Named
                  { Name = local.Name; Alias = None }
            | ImportSpecifier.Namespace { Local = local } ->
              Syntax.ImportSpecifier.ModuleAlias { Alias = local.Name })
          specifiers

      [ ModuleItem.Import
          { Path = src.Value
            Specifiers = specifiers } ]
    | ModuleDecl.ExportDecl { Decl = decl } ->
      (migrateStmt ctx (Stmt.Decl decl))
      |> List.map (fun decl ->
        { Kind = StmtKind.Decl decl
          Span = decl.Span })
      |> List.map Syntax.ModuleItem.Stmt
    | ModuleDecl.ExportNamed namedExport ->
      // TODO: handle named exports with specifiers
      // The only modules that use this so far do `export {}` so the specifiers
      // is empty.
      []
    | ModuleDecl.ExportDefaultDecl(_) ->
      failwith "TODO: migrateModuleDecl - exportDefaultDecl"
    | ModuleDecl.ExportDefaultExpr(_) ->
      failwith "TODO: migrateModuleDecl - exportDefaultExpr"
    | ModuleDecl.ExportAll(_) -> failwith "TODO: migrateModuleDecl - exportAll"
    | ModuleDecl.TsImportEquals(_) ->
      failwith "TODO: migrateModuleDecl - tsImportEquals"
    | ModuleDecl.TsExportAssignment(_) ->
      failwith "TODO: migrateModuleDecl - tsExportAssignment"
    | ModuleDecl.TsNamespaceExport(_) ->
      failwith "TODO: migrateModuleDecl - tsNamespaceExport"

  let migrateDeclarator (decl: VarDeclarator) : Syntax.Decl =
    let init =
      match decl.Init with
      | Some _expr -> failwith "TODO: variable declarator initializer"
      | None -> None

    let kind =
      DeclKind.VarDecl
        { Declare = true
          Pattern = migratePat decl.Id
          TypeAnn = Option.map migrateTypeAnn decl.TypeAnn
          Init = init
          Else = None }

    { Kind = kind; Span = DUMMY_SPAN }

  let migrateParam (param: Param) : Syntax.FuncParam =
    let pat = migratePat param.Pat

    let typeAnn =
      match param.TypeAnn with
      | Some typeAnn ->
        let typeAnn = migrateTypeAnn typeAnn

        match typeAnn.Kind with
        | TypeAnnKind.Keyword KeywordTypeAnn.Number
        | TypeAnnKind.Keyword KeywordTypeAnn.String
        | TypeAnnKind.Keyword KeywordTypeAnn.Boolean ->
          match pat.Kind with
          | PatternKind.Ident ident -> ident.IsMut <- false
          | _ -> ()
        | _ -> ()

        Some typeAnn
      | None -> None

    { Pattern = pat
      TypeAnn = typeAnn
      Optional = param.Optional }

  let migrateClassDecl (ctx: Ctx) (decl: ClassDecl) : Syntax.ClassDecl =
    let { Declare = declare
          Ident = { Name = name }
          Class = { TypeParams = typeParams
                    Super = extends
                    Body = body } } =
      decl

    let elems: list<Syntax.ClassElem> =
      List.map
        (fun (elem: ClassMember) ->
          match elem with
          | ClassMember.Constructor { Params = paramList
                                      Body = body
                                      Accessibility = accssibility
                                      IsOptional = optional
                                      Loc = loc } ->
            let selfTypeAnn: TypeAnn =
              { Kind =
                  TypeAnnKind.TypeRef
                    { Ident = Common.QualifiedIdent.Ident "Self"
                      TypeArgs = None }
                Span = DUMMY_SPAN
                InferredType = None }

            let paramList =
              paramList
              |> List.map (fun p ->
                match p with
                | Param param -> param
                | TsParamProp tsParamProp ->
                  let pat =
                    match tsParamProp.Param with
                    | TsParamPropParam.Ident bindingIdent ->
                      Pat.Ident bindingIdent
                    | TsParamPropParam.Assign assignPat ->
                      Pat.Assign assignPat

                  failwith "TODO: migrateClassDecl - TsParamProp")

            let fnSig: FuncSig =
              { TypeParams = None
                Self = Some(makeSelfFuncParamWithOptionalTypeann ())
                ParamList = List.map migrateParam paramList
                ReturnType = Some selfTypeAnn
                Throws = None
                IsAsync = false }

            ClassElem.Constructor { Sig = fnSig; Body = None }
          | ClassMember.Method { Key = key
                                 Function = f
                                 Kind = methodKind
                                 Accessibility = _
                                 IsAbstract = _
                                 IsOptional = _
                                 IsOverride = _
                                 IsStatic = isStatic } ->
            let name =
              match key with
              | PropName.Ident id -> Syntax.PropName.Ident id.Name
              | PropName.Str str -> Syntax.PropName.String str.Value
              | PropName.Num num -> Syntax.PropName.Number num.Value
              | PropName.Computed computedPropName ->
                // TODO: update `key` to handle `unique symbol`s as well
                failwith "TODO: computed property name"

            let typeParams =
              Option.map
                (fun (tpd: TsTypeParamDecl) ->
                  List.map migrateTypeParam tpd.Params)
                f.TypeParams

            let retType =
              match f.ReturnType with
              | Some t -> migrateTypeAnn t
              | None ->
                failwith "all callable signatures must have a return type"

            let self =
              match isStatic with
              | true -> None
              | false -> Some(makeSelfFuncParamWithOptionalTypeann ())

            let fnSig: FuncSig =
              { TypeParams = typeParams
                Self = self
                ParamList = List.map migrateParam f.Params
                ReturnType = Some retType
                Throws = None
                IsAsync = false }

            match methodKind with
            | Method ->
              ClassElem.Method
                { Name = name
                  Sig = fnSig
                  Body = None
                  Static = isStatic }
            | Getter -> failwith "TODO: migrateClassDecl - Getter"
            | Setter -> failwith "TODO: migrateClassDecl - Setter"
          | ClassMember.PrivateMethod(_) ->
            failwith "TODO: migrateClassDecl - PrivateMethod"
          | ClassMember.ClassProp { Key = key
                                    Value = _
                                    TypeAnn = typeAnn
                                    IsStatic = isStatic
                                    Accessibility = _
                                    IsAbstract = _
                                    IsOptional = optional
                                    IsOverride = _
                                    Readonly = readonly
                                    Declare = _
                                    Definite = _ } ->
            let name =
              match key with
              | PropName.Ident id -> Syntax.PropName.Ident id.Name
              | PropName.Str str -> Syntax.PropName.String str.Value
              | PropName.Num num -> Syntax.PropName.Number num.Value
              | PropName.Computed computedPropName ->
                // TODO: update `key` to handle `unique symbol`s as well
                failwith "TODO: computed property name"

            let typeAnn =
              match typeAnn with
              | Some t -> migrateTypeAnn t
              | None ->
                failwith "all class properties must have a type annotation"

            ClassElem.Property
              { Name = name
                TypeAnn = typeAnn
                Optional = optional
                Readonly = readonly
                Static = isStatic }
          | ClassMember.PrivateProp(_) ->
            failwith "TODO: migrateClassDecl - PrivateProp"
          | ClassMember.TsIndexSignature(_) ->
            failwith "TODO: migrateClassDecl - TsIndexSignature"
          | ClassMember.Empty(_) -> failwith "TODO: migrateClassDecl - Empty"
          | ClassMember.StaticBlock(_) ->
            failwith "TODO: migrateClassDecl - StaticBlock"
          | ClassMember.AutoAccessor(_) ->
            failwith "TODO: migrateClassDecl - AutoAccessor")
        body

    let typeParams =
      Option.map
        (fun (tpd: TsTypeParamDecl) -> List.map migrateTypeParam tpd.Params)
        typeParams

    let extends: option<TypeRef> =
      match extends with
      | Some extend ->
        let typeArgs =
          Option.map
            (fun (tpi: TsTypeParamInstantiation) ->
              List.map migrateType tpi.Params)
            extend.TypeParams

        { TypeRef.Ident = entityNameToQualifiedIdent extend.TypeName
          TypeRef.TypeArgs = typeArgs }
        |> Some
      | None -> None

    let cls: Syntax.Class =
      { Extends = extends
        Implements = None // TODO
        Name = Some name
        TypeParams = typeParams
        Elems = elems }

    { Declare = declare || ctx.Declare
      Name = name
      Class = cls }

  // This function returns a list of declarations because a single TypeScript
  // variable declaration can declare multiple variables.
  let migrateStmt (ctx: Ctx) (stmt: Stmt) : list<Syntax.Decl> =
    match stmt with
    | Stmt.Decl decl ->
      match decl with
      | Decl.Class decl ->
        let decl = migrateClassDecl ctx decl

        [ { Kind = DeclKind.ClassDecl decl
            Span = DUMMY_SPAN } ]
      | Decl.Fn { Declare = _; Id = ident; Fn = f } ->

        let typeParams =
          Option.map
            (fun (tpd: TsTypeParamDecl) -> List.map migrateTypeParam tpd.Params)
            f.TypeParams

        let paramList: list<FuncParam> = List.map migrateParam f.Params

        let retType =
          match f.ReturnType with
          | Some t -> migrateTypeAnn t
          | None -> failwith "all functions must have a return type"

        let fnSig: FuncSig =
          { TypeParams = typeParams
            Self = None
            ParamList = paramList
            ReturnType = Some retType
            Throws = None
            IsAsync = false }

        let kind =
          DeclKind.FnDecl
            { Declare = true // Some .d.ts files do `export function foo();`
              Name = ident.Name
              Sig = fnSig
              Body = None }

        [ { Kind = kind; Span = DUMMY_SPAN } ]
      | Decl.Var { Declare = declare; Decls = decls } ->
        List.map migrateDeclarator decls
      | Decl.Using _ -> failwith "TODO: migrate using"
      | Decl.TsInterface { Declare = _declare
                           Id = ident
                           TypeParams = typeParams
                           Extends = extends
                           Body = body } ->
        let typeParams =
          Option.map
            (fun (tpd: TsTypeParamDecl) -> List.map migrateTypeParam tpd.Params)
            typeParams

        let isReadonly =
          ident.Name.StartsWith "Readonly"
          || ident.Name.EndsWith "ReadOnly"
          || (List.contains
            ident.Name
            [ "Boolean"
              "Number"
              "String"
              "Symbol"
              "BigInt"
              // TODO: Track what namespace we're inside of if any
              // TODO: Track what node_module we're inside of it any
              // TODO: Maintain a full qualified list of interfaces that are
              // known to be "readonly" like `Intl.NumberFormatOptions`
              "NumberFormat" ])

        let isMutable = not isReadonly
        let elems = List.map (migrateTypeElement isMutable) body.Body

        let extends: option<list<TypeRef>> =
          match extends with
          | Some extends ->
            extends
            |> List.map (fun extend ->
              let typeArgs =
                Option.map
                  (fun (tpi: TsTypeParamInstantiation) ->
                    List.map migrateType tpi.Params)
                  extend.TypeParams

              { TypeRef.Ident = entityNameToQualifiedIdent extend.TypeName
                TypeRef.TypeArgs = typeArgs })
            |> Some
          | None -> None

        let decl: InterfaceDecl =
          { Name = ident.Name
            TypeParams = typeParams
            Extends = extends
            Elems = elems }

        let kind = DeclKind.InterfaceDecl decl
        [ { Kind = kind; Span = DUMMY_SPAN } ]
      | Decl.TsTypeAlias { Declare = _declare
                           Id = ident
                           TypeParams = typeParams
                           TypeAnn = typeAnn } ->

        let typeParams =
          Option.map
            (fun (tpd: TsTypeParamDecl) -> List.map migrateTypeParam tpd.Params)
            typeParams

        let decl: TypeDecl =
          { Name = ident.Name
            TypeParams = typeParams
            TypeAnn = migrateType typeAnn }

        let kind = DeclKind.TypeDecl decl
        [ { Kind = kind; Span = DUMMY_SPAN } ]
      | Decl.TsEnum _ -> failwith "TODO: migrate enum"
      | Decl.TsModule { Declare = declare
                        Global = _global
                        Id = name
                        Body = body } ->
        let ctx = if declare then { Declare = true } else ctx

        let name: string =
          match name with
          | TsModuleName.Ident ident -> ident.Name
          | TsModuleName.Str str -> str.Value

        let body: list<Syntax.Decl> =
          match body with
          | None -> []
          | Some body ->
            match body with
            | TsModuleBlock { Body = items } ->
              List.collect
                (fun (item: ModuleItem) ->
                  match item with
                  | ModuleItem.ModuleDecl decl ->
                    match decl with
                    | ModuleDecl.ExportDecl { Decl = decl } ->
                      migrateStmt ctx (Stmt.Decl decl)

                    | ModuleDecl.Import importDecl ->
                      failwith "TODO: TsModule - Import"
                    | ModuleDecl.ExportNamed namedExport ->
                      failwith "TODO: TsModule - ExportNamed"
                    | ModuleDecl.ExportDefaultDecl exportDefaultDecl ->
                      failwith "TODO: TsModule - ExportDefaultDecl"
                    | ModuleDecl.ExportDefaultExpr exportDefaultExpr ->
                      failwith "TODO: TsModule - ExportDefaultExpr"
                    | ModuleDecl.ExportAll exportAll ->
                      failwith "TODO: TsModule - ExportAll"
                    | ModuleDecl.TsImportEquals tsImportEqualsDecl ->
                      failwith "TODO: TsModule - TsImportEquals"
                    | ModuleDecl.TsExportAssignment tsExportAssignment ->
                      failwith "TODO: TsModule - TsExportAssignment"
                    | ModuleDecl.TsNamespaceExport tsNamespaceExportDecl ->
                      failwith "TODO: TsModule - TsNamespaceExport"

                  | ModuleItem.Stmt stmt -> migrateStmt ctx stmt)
                items
            | TsNamespaceDecl _tsNamespaceDecl ->
              failwith "TODO: inferModuleBlock- TsNamespaceDecl"

        let kind = DeclKind.NamespaceDecl { Name = name; Body = body }
        [ { Kind = kind; Span = DUMMY_SPAN } ]

    | _ -> failwith "only declarations are supported for now"

  let migrateModule (m: TypeScript.Module) : Syntax.Module =
    let ctx: Ctx = { Declare = false }

    let items: list<Syntax.ModuleItem> =
      List.collect
        (fun (item: ModuleItem) ->
          try
            match item with
            | ModuleItem.ModuleDecl decl -> migrateModuleDecl ctx decl
            | ModuleItem.Stmt stmt ->
              (migrateStmt ctx stmt)
              |> List.map (fun decl ->
                { Kind = StmtKind.Decl decl
                  Span = decl.Span })
              |> List.map Syntax.ModuleItem.Stmt
          with ex ->
            // TODO: fix all of the error messages
            printfn $"Error migrating module item: {ex.Message}"
            [])
        m.Body

    { Items = items }
