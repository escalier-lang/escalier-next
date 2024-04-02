namespace Escalier.Interop

open Escalier.Data
open Escalier.Data.Syntax

open TypeScript

module rec Migrate =
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

        Syntax.ExprKind.Member(obj, prop, false)
      | Expr.SuperProp superPropExpr -> failwith "todo"
      | Expr.Cond condExpr -> failwith "todo"
      | Expr.Call callExpr -> failwith "todo"
      | Expr.New newExpr -> failwith "todo"
      | Expr.Seq seqExpr -> failwith "todo"
      | Expr.Ident ident -> Syntax.ExprKind.Identifier ident.Name
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

  let makeSelfFuncParam () : FuncParam<TypeAnn> =
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

    let self: Syntax.FuncParam<TypeAnn> =
      { Pattern = pattern
        TypeAnn = selfTypeAnn
        Optional = false }

    self

  let migrateTypeElement (elem: TsTypeElement) : Syntax.ObjTypeAnnElem =
    match elem with
    | TsCallSignatureDecl _ -> failwith "TODO: call signature"
    | TsConstructSignatureDecl _ -> failwith "TODO: construct signature"
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
        | expr -> failwith "TODO: handle computed property name"

      ObjTypeAnnElem.Property
        { Name = name
          TypeAnn = migrateTypeAnn typeAnn
          Optional = optional
          Readonly = readonly }
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

      let f: FunctionType =
        { TypeParams = typeParams
          Self = Some(makeSelfFuncParam ())
          ParamList = List.map migrateFnParam fnParams
          ReturnType = retType
          Throws = None
          IsAsync = false }

      let name =
        match key with
        | Expr.Ident id -> PropName.String id.Name
        | Expr.Lit(Lit.Str str) -> PropName.String str.Value
        | Expr.Lit(Lit.Num num) -> PropName.Number(num.Value)
        | expr -> failwith "TODO: handle computed property name"

      ObjTypeAnnElem.Method { Name = name; Type = f }
    | TsIndexSignature _ -> failwith "TODO: index signature"
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
        | expr -> failwith "TODO: handle computed property name"

      ObjTypeAnnElem.Getter
        { Name = name
          Self = makeSelfFuncParam ()
          ReturnType = retType
          Throws = None }
    | TsSetterSignature { Key = key
                          Param = fnParam
                          Optional = _optional
                          Computed = _computed } ->
      let fnParam = migrateFnParam fnParam

      let undefined: Syntax.TypeAnn =
        { Kind = Syntax.Keyword KeywordTypeAnn.Undefined
          Span = DUMMY_SPAN
          InferredType = None }

      let name =
        match key with
        | Expr.Ident id -> PropName.String id.Name
        | Expr.Lit(Lit.Str str) -> PropName.String str.Value
        | Expr.Lit(Lit.Num num) -> PropName.Number(num.Value)
        | expr -> failwith "TODO: handle computed property name"

      ObjTypeAnnElem.Setter
        { Name = name
          Self = makeSelfFuncParam ()
          Param = fnParam
          Throws = None }

  let migrateTypeAnn (typeAnn: TypeScript.TsTypeAnn) : Syntax.TypeAnn =
    migrateType typeAnn.TypeAnn

  let migrateType (t: TypeScript.TsType) : TypeAnn =
    let kind: TypeAnnKind =
      match t with
      | TsType.TsKeywordType t ->
        match t.Kind with
        // TODO: introduce an `any` type kind that can only be used when
        // migrating .d.ts files.
        | TsAnyKeyword -> failwith "TODO: any"
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
        | TsIntrinsicKeyword -> failwith "TODO: handle intrinsic types"

      | TsType.TsThisType _ ->
        { TypeRef.Ident = Common.QualifiedIdent.Ident "Self"
          TypeRef.TypeArgs = None }
        |> TypeRef
      | TsType.TsFnOrConstructorType tsFnOrConstructorType ->
        match tsFnOrConstructorType with
        | TsFnType f ->
          let typeParams =
            Option.map
              (fun (tpd: TsTypeParamDecl) ->
                List.map migrateTypeParam tpd.Params)
              f.TypeParams

          let paramList: list<FuncParam<TypeAnn>> =
            List.map migrateFnParam f.Params

          let fnType: FunctionType =
            { TypeParams = typeParams
              Self = None // TODO: check if first parameter has type `Self`
              ParamList = paramList
              ReturnType = migrateTypeAnn f.TypeAnn
              Throws = None
              IsAsync = false }

          TypeAnnKind.Function fnType
        | TsConstructorType c -> failwith "TODO: migrate constructor type"
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
        let elems: list<ObjTypeAnnElem> = List.map migrateTypeElement members
        TypeAnnKind.Object { Elems = elems; Immutable = false }
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
      | TsType.TsConditionalType _ -> failwith "Not Implemented"
      | TsType.TsInferType _ -> failwith "Not Implemented"
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
        | TsTypeOperatorOp.Readonly -> failwith "TODO: handle readonly types"
      | TsType.TsIndexedAccessType { ObjType = objType
                                     IndexType = indexType
                                     Readonly = _readonly } ->
        TypeAnnKind.Index(migrateType objType, migrateType indexType)
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

        let typeParam: IndexParam = { Name = ""; Constraint = c }

        let mapped: Mapped =
          { TypeParam = typeParam
            Name = None // TODO: handle renaming
            Optional = optional
            Readonly = readonly
            TypeAnn = migrateType typeAnn }

        let elem: ObjTypeAnnElem = ObjTypeAnnElem.Mapped mapped
        TypeAnnKind.Object { Elems = [ elem ]; Immutable = false }
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
      | TsType.TsTypePredicate _ -> failwith "Not Implemented"
      | TsType.TsImportType _ -> failwith "Not Implemented"

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
    (fnParam: TypeScript.TsFnParam)
    : Syntax.FuncParam<TypeAnn> =
    { Pattern = migrateFnParamPattern fnParam.Pat
      TypeAnn =
        match fnParam.TypeAnn with
        | Some typeAnn -> migrateTypeAnn typeAnn
        | None -> failwith "all function parameters must have a type annotation"
      Optional = fnParam.Optional }

  let migrateFnParamPattern (pat: TypeScript.TsFnParamPat) : Pattern =
    let kind =
      match pat with
      | TsFnParamPat.Ident { Id = ident } ->
        PatternKind.Ident
          { Name = ident.Name
            IsMut = true
            Assertion = None }
      | TsFnParamPat.Object { Props = props } ->
        let elems: list<ObjPatElem> =
          List.map
            (fun (prop: TypeScript.ObjectPatProp) ->
              match prop with
              | KeyValue { Key = key; Value = value } ->
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
              | Assign { Key = key; Value = value } ->
                ShorthandPat
                  { Name = key.Name
                    IsMut = true
                    Assertion = None
                    Default = None // TODO
                    Span = DUMMY_SPAN }
              | Rest { Arg = arg } ->
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
              | KeyValue { Key = key; Value = value } ->
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
              | Assign { Key = key; Value = value } ->
                ShorthandPat
                  { Name = key.Name
                    IsMut = true
                    Assertion = None
                    Default = None // TODO
                    Span = DUMMY_SPAN }
              | Rest { Arg = arg } ->
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

  let migrateModuleDecl (decl: ModuleDecl) : Syntax.ModuleItem =
    failwith "TODO: migrateModuleDecl"

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

  // This function returns a list of declarations because a single TypeScript
  // variable declaration can declare multiple variables.
  let migrateStmt (stmt: Stmt) : list<Syntax.Decl> =
    match stmt with
    | Stmt.Decl decl ->
      match decl with
      | Decl.Class _ -> failwith "TODO: migrate class"
      | Decl.Fn _ -> failwith "TODO: migrate function"
      | Decl.Var { Declare = declare; Decls = decls } ->
        List.map migrateDeclarator decls
      | Decl.Using _ -> failwith "TODO: migrate using"
      | Decl.TsInterface { Declare = _declare
                           Id = ident
                           TypeParams = typeParams
                           Extends = _extends
                           Body = body } ->
        let typeParams =
          Option.map
            (fun (tpd: TsTypeParamDecl) -> List.map migrateTypeParam tpd.Params)
            typeParams

        let elems = List.map migrateTypeElement body.Body

        let decl: InterfaceDecl =
          { Name = ident.Name
            TypeParams = typeParams
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
      | Decl.TsModule _ -> failwith "TODO: migrate module"

    | _ -> failwith "only declarations are supported for now"

  let migrateModule (m: TypeScript.Module) : Syntax.Module =
    let items: list<Syntax.ModuleItem> =
      List.collect
        (fun (item: ModuleItem) ->
          match item with
          | ModuleItem.ModuleDecl decl -> [ migrateModuleDecl decl ]
          | ModuleItem.Stmt stmt ->
            List.map Syntax.ModuleItem.Decl (migrateStmt stmt))
        m.Body

    { Items = items }
