namespace Escalier.Interop

open Escalier.Interop.TypeScript
open FParsec

module Parser =
  let ws = spaces
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

  // Expressions

  exprRef.Value <- ident |>> Expr.Ident

  // Patterns

  let bindingIdent: Parser<BindingIdent, unit> =
    pipe2 ident (opt (strWs ":" >>. tsTypeAnn))
    <| fun ident typeAnn ->
      { Id = ident
        TypeAnn = typeAnn
        Loc = None }

  patRef.Value <- bindingIdent |>> Pat.Ident

  // Type Annotations

  let typeParam: Parser<TsTypeParam, unit> =
    pipe3 ident (opt (strWs ":" >>. tsType)) (opt (strWs "=" >>. tsType))
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

  let arrayPat: Parser<ArrayPat, unit> =
    pipe2
      ((strWs "[") >>. (sepBy (opt pat) (strWs ",")) .>> (strWs "]"))
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun elems typeAnn ->
      { Elems = elems
        Optional = false
        TypeAnn = typeAnn
        Loc = None }

  let restPat: Parser<RestPat, unit> =
    pipe2 (strWs "..." >>. pat) (opt (strWs ":" >>. tsTypeAnn))
    <| fun arg typeAnn ->
      { Arg = arg
        TypeAnn = typeAnn
        Loc = None }

  let objectPatProp: Parser<ObjectPatProp, unit> =
    pipe3 (strWs "readonly" >>. ident) (strWs ":" >>. tsTypeAnn) getPosition
    <| fun name typeAnn loc -> failwith "TODO: objectPatProp"

  let objectPat: Parser<ObjectPat, unit> =
    pipe2
      ((strWs "{") >>. (sepBy objectPatProp (strWs ",")) .>> (strWs "}"))
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun props typeAnn ->
      { Props = props
        Optional = false
        TypeAnn = typeAnn
        Loc = None }

  let funcParam: Parser<TsFnParam, unit> =
    choice
      [ bindingIdent |>> TsFnParam.Ident
        arrayPat |>> TsFnParam.Array
        restPat |>> TsFnParam.Rest
        objectPat |>> TsFnParam.Object ]

  let paramList: Parser<list<TsFnParam>, unit> =
    between (strWs "(") (strWs ")") (sepBy funcParam (strWs ","))

  let fnType: Parser<TsFnType, unit> =
    pipe3 (strWs "fn" >>. (opt typeParams)) paramList (strWs "=>" >>. tsTypeAnn)
    <| fun type_params param_list return_type ->
      { TypeParams = type_params
        Params = param_list
        TypeAnn = return_type
        Loc = None }

  let constructorType: Parser<TsConstructorType, unit> =
    pipe3
      (strWs "new" >>. (opt typeParams))
      paramList
      (strWs "=>" >>. tsTypeAnn)
    <| fun type_params param_list return_type ->
      { TypeParams = type_params
        Params = param_list
        TypeAnn = return_type
        IsAbstract = false // TODO: handle `abstract new`
        Loc = None }

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

  // let typeQuery: Parser<TsTypeQuery, unit> = ...

  let callSignDecl: Parser<TsTypeElement, unit> =
    pipe3 (opt typeParams) paramList (opt (strWs ":" >>. tsTypeAnn))
    <| fun typeParams ps typeAnn ->
      { TsCallSignatureDecl.TypeParams = typeParams
        Params = ps
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsCallSignatureDecl

  let constructorSignDecl: Parser<TsTypeElement, unit> =
    pipe3
      (strWs "new" >>. (opt typeParams))
      paramList
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun typeParams ps typeAnn ->
      { TsConstructSignatureDecl.TypeParams = typeParams
        Params = ps
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsConstructSignatureDecl

  let propSig: Parser<TsTypeElement, unit> =
    pipe3
      (opt (strWs "readonly"))
      (strWs "?" >>. ident) // TODO: support computed properties
      (strWs ":" >>. tsTypeAnn)
    <| fun readonly id typeAnn ->
      { Readonly = readonly.IsSome
        Key = Expr.Ident id
        Computed = false
        Optional = false // TODO
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsPropertySignature

  let getterSig: Parser<TsTypeElement, unit> =
    pipe3
      (strWs "get" >>. ident)
      (strWs "(" >>. strWs ")")
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun id _ typeAnn ->
      { Key = Expr.Ident id
        Computed = false
        Optional = false // TODO
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsGetterSignature

  let setterSig: Parser<TsTypeElement, unit> =
    pipe2 (strWs "set" >>. ident) (strWs "(" >>. funcParam .>> strWs ")")
    <| fun id param ->
      { Key = Expr.Ident id
        Computed = false // TODO
        Optional = false // TODO
        Param = param
        Loc = None }
      |> TsTypeElement.TsSetterSignature

  let methodSig: Parser<TsTypeElement, unit> =
    pipe4 ident (opt typeParams) paramList (opt (strWs ":" >>. tsTypeAnn))
    <| fun key typeParams ps typeAnn ->
      { Key = Expr.Ident key
        Computed = false // TODO
        Optional = false // TODO
        Params = ps
        TypeAnn = typeAnn
        TypeParams = typeParams
        Loc = None }
      |> TsTypeElement.TsMethodSignature

  let indexSig: Parser<TsTypeElement, unit> =
    pipe3
      (opt (strWs "readonly"))
      (strWs "[" >>. funcParam .>> strWs "]")
      (strWs ":" >>. tsTypeAnn)
    <| fun readonly key typeAnn ->
      { Params = [ key ]
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
        getterSig
        setterSig
        attempt methodSig
        indexSig ]

  let typeLit: Parser<TsTypeLit, unit> =
    between (strWs "{") (strWs "}") (sepBy typeMember (strWs ","))
    |>> fun members -> { Members = members; Loc = None }

  let arrayType: Parser<TsArrayType, unit> =
    between (strWs "[") (strWs "]") tsType
    |>> fun elemType -> { ElemType = elemType; Loc = None }

  let tupleElement: Parser<TsTupleElement, unit> =
    pipe2 (opt pat) (strWs ":" >>. tsType)
    <| fun label t -> { Label = label; Type = t; Loc = None }

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
    choice
      [ unionType |>> TsUnionOrIntersectionType.TsUnionType
        intersectionType |>> TsUnionOrIntersectionType.TsIntersectionType ]

  let conditionalType: Parser<TsConditionalType, unit> =
    pipe4 tsType (strWs "extends" >>. tsType) tsType (strWs "?" >>. tsType)
    <| fun check extends trueType falseType ->
      { CheckType = check
        ExtendsType = extends
        TrueType = trueType
        FalseType = falseType
        Loc = None }

  let inferType: Parser<TsInferType, unit> =
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

  let indexedAccessType: Parser<TsIndexedAccessType, unit> =
    pipe2 tsType (between (strWs "[") (strWs "]") tsType)
    <| fun objType indexType ->
      { Readonly = false
        ObjType = objType
        IndexType = indexType
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
      [ strWs "readonly+" |>> fun _ -> TruePlusMinus.Plus
        strWs "readonly-" |>> fun _ -> TruePlusMinus.Minus
        strWs "readonly" |>> fun _ -> TruePlusMinus.True ]

  let optionalTruePlusMinus: Parser<TruePlusMinus, unit> =
    choice
      [ strWs "?+" |>> fun _ -> TruePlusMinus.Plus
        strWs "?-" |>> fun _ -> TruePlusMinus.Minus
        strWs "?" |>> fun _ -> TruePlusMinus.True ]

  let mappedType: Parser<TsMappedType, unit> =
    pipe5
      (opt readonlyTruePlusMinus)
      ((strWs "[") >>. mappedTypeParam)
      ((opt tsType) .>> (strWs "]"))
      (opt optionalTruePlusMinus)
      (strWs ":" >>. tsType)
    <| fun readonly param name optional typeAnn ->
      { Readonly = readonly
        TypeParam = param
        NameType = name
        Optional = optional
        TypeAnn = typeAnn
        Loc = None }

  let number: Parser<TsLit, unit> =
    pfloat
    |>> fun value ->
      { Number.Value = value
        Raw = None
        Loc = None }
      |> TsLit.Number

  let string: Parser<TsLit, unit> =
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
    |>> fun value ->
      { Str.Value = value
        Raw = None
        Loc = None }
      |> TsLit.Str

  let bool: Parser<TsLit, unit> =
    choice
      [ (strWs "true") |>> fun _ -> TsLit.Bool { Value = true; Loc = None }
        (strWs "false") |>> fun _ -> TsLit.Bool { Value = false; Loc = None } ]

  // TODO
  // let tpl: Parser<TsLit, unit> = ...

  let litType: Parser<TsLitType, unit> =
    choice [ number; string; bool ] |>> fun lit -> { Lit = lit; Loc = None }

  tsTypeRef.Value <-
    choice
      [ keywordType |>> TsType.TsKeywordType
        thisType |>> TsType.TsThisType
        fnOrConstructorType |>> TsType.TsFnOrConstructorType
        typeRef |>> TsType.TsTypeRef
        // TODO: typeQuery |>> TsType.TsTypeQuery
        typeLit |>> TsType.TsTypeLit
        arrayType |>> TsType.TsArrayType
        tupleType |>> TsType.TsTupleType
        // TODO: optionalType |>> TsType.TsOptionalType
        restType |>> TsType.TsRestType
        unionOrIntersectionType |>> TsType.TsUnionOrIntersectionType
        conditionalType |>> TsType.TsConditionalType
        inferType |>> TsType.TsInferType
        parenthesizedType |>> TsType.TsParenthesizedType
        typeOperator |>> TsType.TsTypeOperator
        indexedAccessType |>> TsType.TsIndexedAccessType
        mappedType |>> TsType.TsMappedType
        litType |>> TsType.TsLitType
        // TODO: typePredicate |>> TsType.TsTypePredicate
        // TODO: importType |>> TsType.TsImportType
        ]

  tsTypeAnnRef.Value <- tsType |>> fun t -> { TypeAnn = t; Loc = None }

  // Declarations

  // let interfaceDecl: Parser<Decl, unit> = failwith "TODO"
  // let fnDecl: Parser<Decl, unit> = failwith "TODO"

  let typeAliasDecl: Parser<Decl, unit> =
    pipe4
      (opt (strWs "declare"))
      (strWs "type" >>. ident)
      (opt typeParams)
      (strWs "=" >>. tsType)
    <| fun declare id typeParam typeAnn ->
      { Declare = declare.IsSome
        Id = id
        TypeParams = typeParam
        TypeAnn = typeAnn
        Loc = None }
      |> Decl.TsTypeAlias

  let varDeclKind: Parser<VariableDeclarationKind, unit> =
    choice
      [ strWs "var" |>> fun _ -> VariableDeclarationKind.Var
        strWs "let" |>> fun _ -> VariableDeclarationKind.Let
        strWs "const" |>> fun _ -> VariableDeclarationKind.Const ]

  let declarator: Parser<VarDeclarator, unit> =
    pipe2 pat (opt (strWs "=" >>. expr))
    <| fun id init -> { Id = id; Init = init }

  let varDecl: Parser<Decl, unit> =
    pipe3 (opt (strWs "declare")) varDeclKind (sepBy1 declarator (strWs ","))
    <| fun declare kind declarators ->
      { Declare = declare.IsSome
        Decls = declarators
        Kind = kind }
      |> Decl.Var

  // let decl = choice [ (attempt varDecl); (attempt typeAliasDecl) ]
  let decl = varDecl

  let export =
    (strWs "export") >>. decl
    |>> fun decl -> ModuleDecl.ExportDecl { Decl = decl; Loc = None }

  let stmt: Parser<Stmt, unit> = decl |>> Stmt.Decl

  let moduleItem =
    (export |>> ModuleItem.ModuleDecl) <|> (stmt |>> ModuleItem.Stmt)

  let mod': Parser<Module, unit> =
    many moduleItem
    |>> fun items ->
      { Body = items
        Shebang = None
        Loc = None }

  let parseModule (input: string) = run mod' input
