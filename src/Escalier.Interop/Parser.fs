namespace Escalier.Interop

open FParsec

open Escalier.Data
open Escalier.Interop.TypeScript

module Parser =
  let blockComment: Parser<unit, unit> =
    between
      (pstring "/*")
      (pstring "*/")
      (charsTillString "*/" false System.Int32.MaxValue)
    |>> ignore

  let lineComment: Parser<unit, unit> = (pstring "//") >>. (skipRestOfLine true)

  let ws =
    between spaces spaces (many ((blockComment <|> lineComment) >>. spaces))

  let strWs s = pstring s .>> ws

  let ident: Parser<Ident, unit> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace
    |>> fun name -> { Name = name; Loc = None }

  let expr, exprRef = createParserForwardedToRef<Expr, unit> ()
  let pat, patRef = createParserForwardedToRef<Pat, unit> ()
  let tsTypeAnn, tsTypeAnnRef = createParserForwardedToRef<TsTypeAnn, unit> ()
  let tsType, tsTypeRef = createParserForwardedToRef<TsType, unit> ()

  let moduleItem, moduleItemRef =
    createParserForwardedToRef<ModuleItem, unit> ()

  // Literals

  let num: Parser<Number, unit> =
    pfloat .>> ws
    |>> fun value ->
      { Number.Value = Common.Float value
        Raw = None
        Loc = None }

  let str: Parser<Str, unit> =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')

    let unescape c =
      match c with
      | 'n' -> "\n"
      | 'r' -> "\r"
      | 't' -> "\t"
      | c -> string c

    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)

    (between
      (pstring "\"")
      (pstring "\"")
      (stringsSepBy normalCharSnippet escapedChar))
    .>> ws
    |>> fun value ->
      { Str.Value = value
        Raw = None
        Loc = None }

  let bool: Parser<TsLit, unit> =
    choice
      [ (strWs "true") |>> fun _ -> TsLit.Bool { Value = true; Loc = None }
        (strWs "false") |>> fun _ -> TsLit.Bool { Value = false; Loc = None } ]

  // TODO
  // let tpl: Parser<TsLit, unit> = ...


  // Expressions

  exprRef.Value <- ident |>> Expr.Ident

  // Patterns

  let bindingIdent: Parser<BindingIdent, unit> =
    ident |>> fun ident -> { Id = ident; Loc = None }

  let arrayPat: Parser<ArrayPat, unit> =
    ((strWs "[") >>. (sepBy (opt pat) (strWs ",")) .>> (strWs "]"))
    |>> fun elems -> { Elems = elems; Loc = None }

  let restPat: Parser<RestPat, unit> =
    (strWs "..." >>. pat) |>> fun arg -> { Arg = arg; Loc = None }

  let objectPatProp: Parser<ObjectPatProp, unit> =
    pipe3 (strWs "readonly" >>. ident) (strWs ":" >>. tsTypeAnn) getPosition
    <| fun name typeAnn loc -> failwith "TODO: objectPatProp"

  let objectPat: Parser<ObjectPat, unit> =
    ((strWs "{") >>. (sepBy objectPatProp (strWs ",")) .>> (strWs "}"))
    |>> fun props -> { Props = props; Loc = None }

  // TODO flesh this out
  patRef.Value <-
    choice
      [ bindingIdent |>> Pat.Ident
        arrayPat |>> Pat.Array
        restPat |>> Pat.Rest
        objectPat |>> Pat.Object ]

  // Type Annotations

  let typeParam: Parser<TsTypeParam, unit> =
    pipe3 ident (opt (strWs "extends" >>. tsType)) (opt (strWs "=" >>. tsType))
    <| fun name c d ->
      { Name = name
        IsIn = false
        IsOut = false
        IsConst = false
        Constraint = c
        Default = d
        Loc = None }

  let typeParams: Parser<TsTypeParamDecl, unit> =
    between (strWs "<") (strWs ">") (sepBy typeParam (strWs ","))
    |>> fun typeParams -> { Params = typeParams; Loc = None }

  let funcParam: Parser<TsFnParam, unit> =
    let paramPat =
      choice
        [ bindingIdent |>> TsFnParamPat.Ident
          arrayPat |>> TsFnParamPat.Array
          restPat |>> TsFnParamPat.Rest
          objectPat |>> TsFnParamPat.Object ]

    pipe3 paramPat (opt (strWs "?")) (opt (strWs ":" >>. tsTypeAnn))
    <| fun pat optional typeAnn ->

      { Pat = pat
        TypeAnn = typeAnn
        Optional = optional.IsSome
        Loc = None }

  let tsFnParams: Parser<list<TsFnParam>, unit> =
    between (strWs "(") (strWs ")") (sepBy funcParam (strWs ","))

  let fnType: Parser<TsFnType, unit> =
    pipe3 (opt typeParams) tsFnParams (strWs "=>" >>. tsTypeAnn)
    <| fun type_params param_list return_type ->
      { TypeParams = type_params
        Params = param_list
        TypeAnn = return_type
        Loc = None }

  let constructorType: Parser<TsConstructorType, unit> =
    pipe4
      (opt (strWs "abstract"))
      (strWs "new" >>. (opt typeParams))
      tsFnParams
      (strWs "=>" >>. tsTypeAnn)
    <| fun abs type_params param_list return_type ->
      { TypeParams = type_params
        Params = param_list
        TypeAnn = return_type
        IsAbstract = abs.IsSome
        Loc = None }

  // TODO: split this up
  let fnOrConstructorType: Parser<TsFnOrConstructorType, unit> =
    choice
      [ fnType |>> TsFnOrConstructorType.TsFnType
        constructorType |>> TsFnOrConstructorType.TsConstructorType ]

  let keywordType: Parser<TsKeywordType, unit> =
    choice
      [ (strWs "any") |>> fun _ -> TsKeywordTypeKind.TsAnyKeyword
        (strWs "boolean") |>> fun _ -> TsKeywordTypeKind.TsBooleanKeyword
        (strWs "bigint") |>> fun _ -> TsKeywordTypeKind.TsBigIntKeyword
        (strWs "never") |>> fun _ -> TsKeywordTypeKind.TsNeverKeyword
        (strWs "null") |>> fun _ -> TsKeywordTypeKind.TsNullKeyword
        (strWs "number") |>> fun _ -> TsKeywordTypeKind.TsNumberKeyword
        (strWs "object") |>> fun _ -> TsKeywordTypeKind.TsObjectKeyword
        (strWs "string") |>> fun _ -> TsKeywordTypeKind.TsStringKeyword
        (strWs "symbol") |>> fun _ -> TsKeywordTypeKind.TsSymbolKeyword
        (strWs "undefined") |>> fun _ -> TsKeywordTypeKind.TsUndefinedKeyword
        (strWs "unknown") |>> fun _ -> TsKeywordTypeKind.TsUnknownKeyword
        (strWs "void") |>> fun _ -> TsKeywordTypeKind.TsVoidKeyword ]
    |>> fun kind -> { Kind = kind; Loc = None }

  let thisType: Parser<TsThisType, unit> =
    (strWs "this") >>. ws >>. preturn { Loc = None }

  let qualificationSuffix: Parser<TsEntityName -> TsEntityName, unit> =
    strWs "." >>. ident
    |>> fun id left -> TsEntityName.TsQualifiedName { Left = left; Right = id }

  let entityName =
    pipe2 ident (many qualificationSuffix)
    <| fun id suffixes ->
      suffixes
      |> List.fold (fun acc suffix -> suffix acc) (TsEntityName.Identifier id)

  let typeArgs: Parser<TsTypeParamInstantiation, unit> =
    between (strWs "<") (strWs ">") (sepBy tsType (strWs ","))
    |>> fun args -> { Params = args; Loc = None }

  let typeRef: Parser<TsTypeRef, unit> =
    pipe2 entityName (opt typeArgs)
    <| fun name typeArgs ->
      { TypeName = name
        TypeParams = typeArgs
        Loc = None }

  let typePredicate: Parser<TsTypePredicate, unit> =
    pipe2 ident (pstring "is" .>> spaces1 >>. ws >>. tsTypeAnn)
    <| fun id typeAnn ->
      let paramName =
        match id.Name with
        | "this" -> TsThisTypeOrIdent.TsThisType({ Loc = None })
        | _ -> TsThisTypeOrIdent.Ident id

      { Asserts = false // TODO
        ParamName = paramName
        Typeann = Some typeAnn
        Loc = None }

  // let typeQuery: Parser<TsTypeQuery, unit> = ...

  let callSignDecl: Parser<TsTypeElement, unit> =
    pipe3 (opt typeParams) tsFnParams (opt (strWs ":" >>. tsTypeAnn))
    <| fun typeParams ps typeAnn ->
      { TsCallSignatureDecl.TypeParams = typeParams
        Params = ps
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsCallSignatureDecl

  let constructorSignDecl: Parser<TsTypeElement, unit> =
    pipe3
      (strWs "new" >>. (opt typeParams))
      tsFnParams
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun typeParams ps typeAnn ->
      { TsConstructSignatureDecl.TypeParams = typeParams
        Params = ps
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsConstructSignatureDecl

  let propSig: Parser<TsTypeElement, unit> =
    pipe4
      (opt (strWs "readonly"))
      ((ident |>> Expr.Ident)
       <|> (num |>> fun value -> Expr.Lit(Lit.Num value))
       <|> (str |>> fun value -> Expr.Lit(Lit.Str value))) // TODO: support computed properties
      (opt (strWs "?"))
      (strWs ":" >>. tsTypeAnn)
    <| fun readonly id optional typeAnn ->
      { Readonly = readonly.IsSome
        Key = id
        Computed = false // TODO
        Optional = optional.IsSome
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsPropertySignature

  // TODO: require at least one whitepsace character after `set`
  let getterSig: Parser<TsTypeElement, unit> =
    pipe3
      (pstring "get" .>> spaces1 >>. ws >>. ident)
      (strWs "(" >>. strWs ")")
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun id _ typeAnn ->
      { Key = Expr.Ident id
        Computed = false
        Optional = false
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsGetterSignature

  // TODO: require at least one whitepsace character after `set`
  let setterSig: Parser<TsTypeElement, unit> =
    pipe2
      (pstring "set" .>> spaces1 >>. ws >>. ident)
      (strWs "(" >>. funcParam .>> strWs ")")
    <| fun id param ->
      { Key = Expr.Ident id
        Computed = false // TODO
        Optional = false // TODO
        Param = param
        Loc = None }
      |> TsTypeElement.TsSetterSignature

  let methodSig: Parser<TsTypeElement, unit> =
    pipe5
      ident
      (opt (strWs "?"))
      (opt typeParams)
      tsFnParams
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun key question typeParams ps typeAnn ->
      { Key = Expr.Ident key
        Computed = false // TODO
        Optional = question.IsSome
        Params = ps
        TypeAnn = typeAnn
        TypeParams = typeParams
        Loc = None }
      |> TsTypeElement.TsMethodSignature

  let indexSig: Parser<TsTypeElement, unit> =
    pipe4
      (opt (strWs "readonly"))
      (strWs "[" >>. ident)
      (strWs ":" >>. tsType .>> strWs "]")
      (strWs ":" >>. tsTypeAnn)
    <| fun readonly name c typeAnn ->
      let param: TsIndexParam = { Name = name; Constraint = c }

      { Param = param
        Readonly = readonly.IsSome
        TypeAnn = typeAnn
        IsStatic = false
        Loc = None }
      |> TsTypeElement.TsIndexSignature

  let typeMember: Parser<TsTypeElement, unit> =
    choice
      [ callSignDecl
        constructorSignDecl
        attempt propSig
        attempt getterSig
        attempt setterSig
        attempt methodSig
        indexSig ]

  let typeLit: Parser<TsTypeLit, unit> =
    between
      (strWs "{")
      (strWs "}")
      (sepEndBy typeMember (strWs "," <|> strWs ";"))
    |>> fun members -> { Members = members; Loc = None }

  // TODO: figure out how to parse tuple elements with a label
  let tupleElement: Parser<TsTupleElement, unit> =
    tsType |>> fun t -> { Label = None; Type = t; Loc = None }

  let tupleType: Parser<TsTupleType, unit> =
    between (strWs "[") (strWs "]") (sepBy tupleElement (strWs ","))
    |>> fun elemTypes -> { ElemTypes = elemTypes; Loc = None }

  let restType: Parser<TsRestType, unit> =
    strWs "..." >>. tsType |>> fun t -> { TypeAnn = t; Loc = None }

  // TODO: handle precedence
  let unionType: Parser<TsUnionType, unit> =
    sepBy tsType (strWs "|") |>> fun types -> { Types = types; Loc = None }

  let intersectionType: Parser<TsIntersectionType, unit> =
    sepBy tsType (strWs "&") |>> fun types -> { Types = types; Loc = None }

  let unionOrIntersectionType: Parser<TsUnionOrIntersectionType, unit> =
    choice [ unionType |>> TsUnionOrIntersectionType.TsUnionType ]
  // intersectionType |>> TsUnionOrIntersectionType.TsIntersectionType ]

  let conditionalType: Parser<TsConditionalType, unit> =
    pipe4
      tsType
      // TODO: require at least one whitepsace character after `extends`
      (strWs "extends" >>. tsType)
      (strWs "?" >>. tsType)
      (strWs ":" >>. tsType)
    <| fun check extends trueType falseType ->
      { CheckType = check
        ExtendsType = extends
        TrueType = trueType
        FalseType = falseType
        Loc = None }

  let inferType: Parser<TsInferType, unit> =
    // TODO: require at least one whitepsace character after `infer`
    strWs "infer" >>. typeParam
    |>> fun typeParam -> { TypeParam = typeParam; Loc = None }

  let parenthesizedType: Parser<TsParenthesizedType, unit> =
    between (strWs "(") (strWs ")") tsType
    |>> fun typeAnn -> { TypeAnn = typeAnn; Loc = None }

  let typeOperatorOp: Parser<TsTypeOperatorOp, unit> =
    choice
      [ (strWs "keyof") |>> fun _ -> TsTypeOperatorOp.KeyOf
        (strWs "unique") |>> fun _ -> TsTypeOperatorOp.Unique
        (strWs "readonly") |>> fun _ -> TsTypeOperatorOp.Readonly ]

  let typeOperator: Parser<TsTypeOperator, unit> =
    pipe2 typeOperatorOp tsType
    <| fun op typeAnn ->
      { Op = op
        TypeAnn = typeAnn
        Loc = None }

  let mappedTypeParam: Parser<TsTypeParam, unit> =
    pipe2 ident (opt (strWs "in" >>. tsType))
    <| fun name c ->
      { Name = name
        IsIn = true
        IsOut = false
        IsConst = false
        Constraint = c
        Default = None
        Loc = None }

  let readonlyTruePlusMinus: Parser<TruePlusMinus, unit> =
    choice
      [ strWs "+readonly" |>> fun _ -> TruePlusMinus.Plus
        strWs "-readonly" |>> fun _ -> TruePlusMinus.Minus
        strWs "readonly" |>> fun _ -> TruePlusMinus.True ]

  let optionalTruePlusMinus: Parser<TruePlusMinus, unit> =
    choice
      [ strWs "+?" |>> fun _ -> TruePlusMinus.Plus
        strWs "-?" |>> fun _ -> TruePlusMinus.Minus
        strWs "?" |>> fun _ -> TruePlusMinus.True ]

  let mappedType: Parser<TsMappedType, unit> =
    pipe5
      (strWs "{" >>. opt readonlyTruePlusMinus)
      (strWs "[" >>. mappedTypeParam)
      (opt (strWs "as" >>. spaces1 >>. tsType) .>> strWs "]")
      (opt optionalTruePlusMinus)
      (strWs ":" >>. tsType .>> opt (strWs ";") .>> strWs "}")
    <| fun readonly param name optional typeAnn ->
      { Readonly = readonly
        TypeParam = param
        NameType = name
        Optional = optional
        TypeAnn = typeAnn
        Loc = None }

  // between (strWs "{") (strWs "}") inner

  let litType: Parser<TsLitType, unit> =
    choice [ num |>> TsLit.Number; str |>> TsLit.Str; bool ]
    |>> fun lit -> { Lit = lit; Loc = None }

  let primaryType =
    choice
      [ keywordType |>> TsType.TsKeywordType
        tupleType |>> TsType.TsTupleType
        restType |>> TsType.TsRestType
        inferType |>> TsType.TsInferType
        typeOperator |>> TsType.TsTypeOperator
        litType |>> TsType.TsLitType

        // TODO: typeQuery |>> TsType.TsTypeQuery
        // TODO: optionalType |>> TsType.TsOptionalType
        // TODO: importType |>> TsType.TsImportType

        // These both start with '{'
        attempt mappedType |>> TsType.TsMappedType
        typeLit |>> TsType.TsTypeLit

        // These both start with a '('
        attempt parenthesizedType |>> TsType.TsParenthesizedType
        fnOrConstructorType |>> TsType.TsFnOrConstructorType

        // typePredicate conflicts with both typeRef and thisType
        attempt (typePredicate |>> TsType.TsTypePredicate)
        thisType |>> TsType.TsThisType
        typeRef |>> TsType.TsTypeRef ]

  let arrayTypeSuffix: Parser<TsType -> TsType, unit> =
    (strWs "[" >>. strWs "]")
    |>> fun _ target -> { ElemType = target; Loc = None } |> TsType.TsArrayType

  let indexedAccessTypeSuffix: Parser<TsType -> TsType, unit> =
    (between (strWs "[") (strWs "]") tsType)
    |>> fun indexType objType ->
      { Readonly = false
        ObjType = objType
        IndexType = indexType
        Loc = None }
      |> TsType.TsIndexedAccessType

  let conditionalTypeSuffix: Parser<TsType -> TsType, unit> =
    pipe3
      (strWs "extends" >>. tsType)
      (strWs "?" >>. tsType)
      (strWs ":" >>. tsType)
    <| fun extendsType trueType falseType checkType ->
      { CheckType = checkType
        ExtendsType = extendsType
        TrueType = trueType
        FalseType = falseType
        Loc = None }
      |> TsType.TsConditionalType

  let suffix: Parser<TsType -> TsType, unit> =
    choice
      [ attempt arrayTypeSuffix
        attempt indexedAccessTypeSuffix
        conditionalTypeSuffix ]

  let primaryTypeWithSuffix =
    pipe2 primaryType (many suffix)
    <| fun typeAnn suffixes ->
      List.fold (fun x suffix -> suffix x) typeAnn suffixes

  let intersectionOrPrimaryType: Parser<TsType, unit> =
    sepBy1 primaryTypeWithSuffix (strWs "&")
    |>> fun typeAnns ->
      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | types ->
        let intersection =
          TsUnionOrIntersectionType.TsIntersectionType
            { Types = types; Loc = None }

        TsType.TsUnionOrIntersectionType intersection

  let unionOrIntersectionOrPrimaryType: Parser<TsType, unit> =
    sepBy1 intersectionOrPrimaryType (strWs "|")
    |>> fun typeAnns ->
      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | types ->
        let union =
          TsUnionOrIntersectionType.TsUnionType { Types = types; Loc = None }

        TsType.TsUnionOrIntersectionType union

  tsTypeRef.Value <- unionOrIntersectionOrPrimaryType

  tsTypeAnnRef.Value <- tsType |>> fun t -> { TypeAnn = t; Loc = None }

  // Declarations

  let interfaceBody: Parser<TsInterfaceBody, unit> =
    (strWs "{" >>. (sepEndBy typeMember (strWs ";" <|> strWs ","))
     .>> (strWs "}"))
    |>> fun members -> { Body = members; Loc = None }

  let extend: Parser<TsExprWithTypeArgs, unit> =
    pipe2 ident (opt typeArgs)
    <| fun ident typeArgs ->
      { Expr = Expr.Ident ident
        TypeArgs = typeArgs }

  let interfaceDecl: Parser<bool -> Decl, unit> =
    pipe4
      ((strWs "interface") >>. ident)
      (opt typeParams)
      (opt ((strWs "extends") >>. (sepBy1 extend (strWs ","))))
      interfaceBody
    <| fun id typeParams extends body ->
      fun declare ->
        { Id = id
          Declare = declare
          TypeParams = typeParams
          Extends = extends
          Body = body
          Loc = None }
        |> Decl.TsInterface

  let param: Parser<Param, unit> =
    pipe3 pat (opt (strWs "?")) (opt (strWs ":" >>. tsTypeAnn))
    <| fun p optional typeAnn ->
      { Pat = p
        TypeAnn = typeAnn
        Loc = None }

  let fnParams: Parser<list<Param>, unit> =
    between (strWs "(") (strWs ")") (sepBy param (strWs ","))

  let fnDecl: Parser<bool -> Decl, unit> =
    pipe5
      (opt (strWs "async"))
      ((strWs "function") >>. ident)
      (opt typeParams)
      fnParams
      (opt ((strWs ":") >>. tsTypeAnn))
    <| fun async id typeParams ps typeAnn ->
      fun declare ->
        let fn: Function =
          { Params = ps
            Body = None
            IsGenerator = false // TODO
            IsAsync = async.IsSome
            TypeParams = typeParams
            ReturnType = typeAnn
            Loc = None }

        { FnDecl.Id = id
          Declare = declare
          Fn = fn }
        |> Decl.Fn

  let typeAliasDecl: Parser<bool -> Decl, unit> =
    pipe3 ((strWs "type") >>. ident) (opt typeParams) ((strWs "=") >>. tsType)
    <| fun id typeParams typeAnn ->
      fun declare ->
        { Declare = declare
          Id = id
          TypeParams = typeParams
          TypeAnn = typeAnn
          Loc = None }
        |> Decl.TsTypeAlias

  let varDeclKind: Parser<VariableDeclarationKind, unit> =
    choice
      [ strWs "var" |>> fun _ -> VariableDeclarationKind.Var
        strWs "let" |>> fun _ -> VariableDeclarationKind.Let
        strWs "const" |>> fun _ -> VariableDeclarationKind.Const ]

  let declarator: Parser<VarDeclarator, unit> =
    pipe3 pat (opt (strWs ":" >>. tsTypeAnn)) (opt (strWs "=" >>. expr))
    <| fun id typeAnn init ->
      { Id = id
        TypeAnn = typeAnn
        Init = init }

  let varDecl: Parser<bool -> Decl, unit> =
    pipe2 varDeclKind (sepBy1 declarator (strWs ","))
    <| fun kind declarators ->
      fun declare ->
        { Declare = declare
          Decls = declarators
          Kind = kind }
        |> Decl.Var

  let moduleBlock: Parser<TsNamespaceBody, unit> =
    (strWs "{" >>. many moduleItem .>> strWs "}")
    |>> fun body -> TsNamespaceBody.TsModuleBlock { Body = body; Loc = None }

  let namespaceBody = moduleBlock

  let moduleName = (ident |>> TsModuleName.Ident) <|> (str |>> TsModuleName.Str)

  let moduleDecl: Parser<bool -> Decl, unit> =
    pipe2 (strWs "namespace" >>. moduleName) moduleBlock
    <| fun id body declare ->
      { Declare = declare
        Global = false
        Id = id
        Body = Some(body)
        Loc = None }
      |> Decl.TsModule

  let declare: Parser<Decl, unit> =
    pipe2
      (opt (strWs "declare"))
      (choice [ typeAliasDecl; varDecl; fnDecl; interfaceDecl; moduleDecl ])
    <| fun declare decl -> decl declare.IsSome

  let decl = declare

  moduleItemRef.Value <-
    pipe2 (ws >>. opt (strWs "export")) (decl .>> (opt (strWs ";")))
    <| fun export decl ->
      match export with
      | Some _ ->
        ModuleItem.ModuleDecl(ModuleDecl.ExportDecl { Decl = decl; Loc = None })
      | None -> ModuleItem.Stmt(Stmt.Decl decl)

  let mod': Parser<Module, unit> =
    many moduleItem .>> eof
    |>> fun items ->
      { Body = items
        Shebang = None
        Loc = None }

  let parseModule (input: string) = run mod' input
