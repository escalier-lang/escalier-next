namespace Escalier.Interop

open FParsec
open System.Text

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

  let isIdentifierChar c =
    isLetter c || isDigit c || c = '_' || c = '$'

  let keyword s =
    pstring s .>> notFollowedBy (satisfy isIdentifierChar) .>> ws

  let ident: Parser<Ident, unit> =
    let isIdentifierFirstChar c = isLetter c || c = '_' || c = '$'

    let identifier =
      many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

    let identifierWithKeywords =
      fun stream ->
        let reply = identifier stream

        match reply.Status with
        | ReplyStatus.Ok ->
          if reply.Result = "enum" then
            Reply(
              Error,
              messageError
                "The keyword 'enum' is reserved and cannot be used as an identifier."
            )
          else
            reply
        | _ -> reply

    identifierWithKeywords .>> ws // skips trailing whitespace
    |>> fun name -> { Name = name; Loc = None }

  let expr, exprRef = createParserForwardedToRef<Expr, unit> ()
  let pat, patRef = createParserForwardedToRef<Pat, unit> ()
  let tsTypeAnn, tsTypeAnnRef = createParserForwardedToRef<TsTypeAnn, unit> ()
  let tsType, tsTypeRef = createParserForwardedToRef<TsType, unit> ()

  let moduleItem, moduleItemRef =
    createParserForwardedToRef<ModuleItem, unit> ()

  // Literals
  // The default float format allows 'Inf' and 'NaN' to be used as literals.
  // We should allow `NaN` and `Infinity` as literals in TypeScript, but we
  // need to make sure that there's no trailing alphanumerics.
  let numberFormat =
    NumberLiteralOptions.DefaultFloat ^^^ NumberLiteralOptions.AllowInfinity

  let pfloat: Parser<float, 'u> =
    fun stream ->
      let reply = (numberLiteral numberFormat "number") stream

      if reply.Status = Ok then
        let nl = reply.Result

        try
          let d =
            if nl.IsDecimal then
              System.Double.Parse(
                nl.String,
                System.Globalization.CultureInfo.InvariantCulture
              )
            elif nl.IsHexadecimal then
              floatOfHexString nl.String
            elif nl.IsInfinity then
              if nl.HasMinusSign then
                System.Double.NegativeInfinity
              else
                System.Double.PositiveInfinity
            else
              System.Double.NaN

          Reply(d)
        with
        | :? System.OverflowException ->
          Reply(
            if nl.HasMinusSign then
              System.Double.NegativeInfinity
            else
              System.Double.PositiveInfinity
          )
        | :? System.FormatException ->
          stream.Skip(-nl.String.Length)

          Reply(
            FatalError,
            messageError
              "The floating-point number has an invalid format (this error is unexpected, please report this error message to fparsec@quanttec.com)."
          )
      else
        Reply(reply.Status, reply.Error)

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

  let atom =
    choice
      [ ident |>> Expr.Ident
        num |>> Lit.Num |>> Expr.Lit
        str |>> Lit.Str |>> Expr.Lit ]

  let exprParser = Pratt.PrattParser<Expr>(atom .>> ws)

  let infixExprParselet
    (precedence: int)
    (callback: Expr -> Expr -> Expr)
    : Pratt.InfixParselet<Expr> =
    { Parse =
        fun (parser, stream, left, operator) ->
          let right = parser.Parse precedence stream

          match right.Status with
          | Ok -> Reply(callback left right.Result)
          | _ -> right
      Precedence = precedence }

  let memberOp =
    fun (optChain: bool) (obj: Expr) (prop: Expr) ->
      match prop with
      | Expr.Ident _ ->
        Expr.Member
          { Object = obj
            Property = prop
            Computed = false
            OptChain = false
            Loc = None }
      | _ -> failwith "Expected identifier"

  exprParser.RegisterInfix(
    ".",
    infixExprParselet 17 (fun left right -> memberOp false left right)
  )

  exprRef.Value <- exprParser.Parse(0)

  // Patterns

  let bindingIdent: Parser<BindingIdent, unit> =
    ident |>> fun ident -> { Id = ident; Loc = None }

  let arrayPat: Parser<ArrayPat, unit> =
    ((strWs "[") >>. (sepBy (opt pat) (strWs ",")) .>> (strWs "]"))
    |>> fun elems -> { Elems = elems; Loc = None }

  let restPat: Parser<RestPat, unit> =
    (strWs "..." >>. pat) |>> fun arg -> { Arg = arg; Loc = None }

  let objectPatProp: Parser<ObjectPatProp, unit> =
    pipe3 ident (opt (strWs ":" >>. pat)) getPosition
    <| fun ident pattern loc ->
      match pattern with
      | Some pattern ->
        ObjectPatProp.KeyValue
          { Key = PropName.Ident ident
            Value = pattern
            Loc = None }
      | None ->
        ObjectPatProp.Assign
          { Key = ident // TODO: make this use BindingIdent instead
            Value = None // TODO
            Loc = None }

  let objectPatRest: Parser<ObjectPatProp, unit> =
    (strWs "..." >>. pat)
    |>> fun arg -> ObjectPatProp.Rest { Arg = arg; Loc = None }

  let objectPat: Parser<ObjectPat, unit> =
    ((strWs "{")
     >>. (sepEndBy (choice [ objectPatProp; objectPatRest ]) (strWs ","))
     .>> (strWs "}"))
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
    between (strWs "<") (strWs ">") (sepEndBy typeParam (strWs ","))
    |>> fun typeParams -> { Params = typeParams; Loc = None }

  let funcParam: Parser<TsFnParam, unit> =
    let paramPat =
      choice
        [ bindingIdent |>> Pat.Ident
          arrayPat |>> Pat.Array
          restPat |>> Pat.Rest
          objectPat |>> Pat.Object ]

    pipe3 paramPat (opt (strWs "?")) (opt (strWs ":" >>. tsTypeAnn))
    <| fun pat optional typeAnn ->

      { Pat = pat
        TypeAnn = typeAnn
        Optional = optional.IsSome
        Loc = None }

  let tsFnParams: Parser<list<TsFnParam>, unit> =
    between (strWs "(") (strWs ")") (sepEndBy funcParam (strWs ","))

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
      (keyword "new" >>. (opt typeParams))
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
        (strWs "intrinsic") |>> fun _ -> TsKeywordTypeKind.TsIntrinsicKeyword
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

  let assertsTypePredicate: Parser<TsTypePredicate, unit> =
    pipe2
      ((keyword "asserts") >>. ident)
      (opt (pstring "is" .>> spaces1 >>. ws >>. tsTypeAnn))
    <| fun id typeAnn ->
      let paramName =
        match id.Name with
        | "this" -> TsThisTypeOrIdent.TsThisType({ Loc = None })
        | _ -> TsThisTypeOrIdent.Ident id

      { Asserts = true
        ParamName = paramName
        Typeann = typeAnn
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

  let callSigDecl: Parser<TsTypeElement, unit> =
    pipe3 (opt typeParams) tsFnParams (opt (strWs ":" >>. tsTypeAnn))
    <| fun typeParams ps typeAnn ->
      { TsCallSignatureDecl.TypeParams = typeParams
        Params = ps
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsCallSignatureDecl

  let constructorSigDecl: Parser<TsTypeElement, unit> =
    pipe3
      (keyword "new" >>. (opt typeParams))
      tsFnParams
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun typeParams ps typeAnn ->
      { TsConstructSignatureDecl.TypeParams = typeParams
        Params = ps
        TypeAnn = typeAnn
        Loc = None }
      |> TsTypeElement.TsConstructSignatureDecl

  let propKey: Parser<PropName, unit> =
    choice
      [ strWs "[" >>. expr .>> strWs "]"
        |>> fun expr -> PropName.Computed { Expr = expr; Loc = None }
        ident |>> fun id -> PropName.Ident id
        num |>> fun value -> PropName.Num value
        str |>> fun value -> PropName.Str value ]

  let propSig: Parser<TsTypeElement, unit> =
    pipe4
      (opt (strWs "readonly"))
      propKey
      (opt (strWs "?"))
      (strWs ":" >>. tsTypeAnn)
    <| fun readonly key optional typeAnn ->
      { Readonly = readonly.IsSome
        Key = key
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
      { Key = PropName.Ident id
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
      { Key = PropName.Ident id
        Optional = false // TODO
        Param = param
        Loc = None }
      |> TsTypeElement.TsSetterSignature

  let methodSig: Parser<TsTypeElement, unit> =
    pipe5
      propKey
      (opt (strWs "?"))
      (opt typeParams)
      tsFnParams
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun key question typeParams ps typeAnn ->
      { Key = key
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

      // TODO: parse +/- modifiers on readonly
      { Param = param
        Readonly = readonly.IsSome
        TypeAnn = typeAnn
        IsStatic = false
        Loc = None }
      |> TsTypeElement.TsIndexSignature

  let typeMember: Parser<TsTypeElement, unit> =
    choice
      [ callSigDecl
        attempt constructorSigDecl
        attempt indexSig // can conflict with propSig when parsing computed properties
        attempt getterSig
        attempt setterSig
        attempt methodSig
        attempt propSig ]

  let typeLit: Parser<TsTypeLit, unit> =
    between
      (strWs "{")
      (strWs "}")
      (sepEndBy typeMember (strWs "," <|> strWs ";"))
    |>> fun members -> { Members = members; Loc = None }

  // TODO: figure out how to parse tuple elements with a label
  let tupleElement: Parser<TsTupleElement, unit> =
    pipe3 (opt (strWs "...")) tsType (opt (strWs "?"))
    <| fun rest t optional ->
      match optional with
      | Some _ ->
        { Label = None
          Type = TsType.TsOptionalType { TypeAnn = t; Loc = None }
          IsRest = rest.IsSome
          Loc = None }
      | None ->
        { Label = None
          Type = t
          IsRest = rest.IsSome
          Loc = None }

  let tupleElementWithLabel: Parser<TsTupleElement, unit> =
    pipe4 (opt (strWs "...")) ident (opt (strWs "?") .>> strWs ":") tsType
    <| fun rest label optional t ->
      match optional with
      | Some _ ->
        { Label = Some label
          Type = TsType.TsOptionalType { TypeAnn = t; Loc = None }
          IsRest = rest.IsSome
          Loc = None }
      | None ->
        { Label = None
          Type = t
          IsRest = rest.IsSome
          Loc = None }

  let tupleType: Parser<TsTupleType, unit> =
    let elem = choice [ attempt tupleElementWithLabel; tupleElement ]

    between (strWs "[") (strWs "]") (sepBy elem (strWs ","))
    |>> fun elemTypes -> { ElemTypes = elemTypes; Loc = None }

  let restType: Parser<TsRestType, unit> =
    strWs "..." >>. tsType |>> fun t -> { TypeAnn = t; Loc = None }

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
    keyword "infer" >>. typeParam
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
    pipe2 ident (strWs "in" >>. tsType)
    <| fun name c ->
      { Name = name
        IsIn = true
        IsOut = false
        IsConst = false
        Constraint = Some(c)
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
    pipe4
      (strWs "{" >>. opt readonlyTruePlusMinus)
      (between
        (strWs "[")
        (strWs "]")
        (mappedTypeParam .>>. (opt (keyword "as" >>. tsType))))
      (opt optionalTruePlusMinus)
      (strWs ":" >>. tsType .>> opt (strWs ";") .>> strWs "}")
    <| fun readonly (param, name) optional typeAnn ->
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

  let tmplLitType: Parser<TsType, unit> =
    fun stream ->
      let sb = StringBuilder()
      let mutable parts: list<string> = []
      let mutable exprs: list<TsType> = []
      let mutable reply: voption<Reply<TsType>> = ValueNone

      if stream.Peek() = '`' then
        let start = stream.Position
        stream.Skip() // '`'

        while stream.Peek() <> '`' && reply = ValueNone do
          if stream.PeekString(2) = "${" then
            stream.Skip(2) // '${'
            parts <- sb.ToString() :: parts
            sb.Clear() |> ignore
            let expr = tsType stream

            if expr.Status = ReplyStatus.Ok then
              if stream.Peek() = '}' then
                stream.Skip()
                exprs <- expr.Result :: exprs
              else
                reply <- ValueSome(Reply(Error, messageError "Expected '}'"))
            else
              reply <- ValueSome(expr)
          else
            sb.Append(stream.Read()) |> ignore

        match reply with
        | ValueNone ->
          stream.Skip() // '`'
          let stop = stream.Position
          parts <- sb.ToString() :: parts

          let quasis: list<TplElement> =
            parts
            |> List.map (fun value ->
              { Tail = false // TODO
                Cooked = Some value
                Raw = value
                Loc = None })

          let lit: TsLit =
            TsLit.Tpl
              { Types = List.rev exprs
                Quasis = List.rev quasis
                Loc = None }

          let result: TsType = TsType.TsLitType { Lit = lit; Loc = None }

          Reply(result)
        | ValueSome(value) -> value
      else
        Reply(Error, messageError "Expected '`'")

  let typeof: Parser<TsTypeQuery, unit> =
    pipe3 (keyword "typeof") entityName (opt typeArgs)
    <| fun _ name typeArgs ->
      { ExprName = TsTypeQueryExpr.TsEntityName name
        TypeArgs = typeArgs
        Loc = None }

  let importType: Parser<TsImportType, unit> =
    pipe3
      (keyword "import")
      (strWs "(" >>. str .>> strWs ")")
      (opt (strWs "." >>. entityName .>>. (opt typeArgs)))
    <| fun _ src qualifierAndTypeArgs ->
      match qualifierAndTypeArgs with
      | Some(qualifier, typeArgs) ->
        { Arg = src
          Qualifier = Some qualifier
          TypeArgs = typeArgs
          Loc = None }
      | None ->
        { Arg = src
          Qualifier = None
          TypeArgs = None
          Loc = None }

  let primaryType =
    choice
      [ // `typePredicate` goes first to handle `object` being used
        // as an identifier in a type predicate in React's index.d.ts
        attempt (typePredicate |>> TsType.TsTypePredicate)
        attempt (assertsTypePredicate |>> TsType.TsTypePredicate)

        keywordType |>> TsType.TsKeywordType
        tupleType |>> TsType.TsTupleType
        restType |>> TsType.TsRestType
        inferType |>> TsType.TsInferType
        typeOperator |>> TsType.TsTypeOperator
        litType |>> TsType.TsLitType

        // TODO: typeQuery |>> TsType.TsTypeQuery
        importType |>> TsType.TsImportType

        // These both start with '{'
        attempt mappedType |>> TsType.TsMappedType
        typeLit |>> TsType.TsTypeLit

        // These both start with a '('
        attempt parenthesizedType |>> TsType.TsParenthesizedType
        fnOrConstructorType |>> TsType.TsFnOrConstructorType

        thisType |>> TsType.TsThisType
        typeof |>> TsType.TsTypeQuery

        typeRef |>> TsType.TsTypeRef

        tmplLitType .>> ws ]

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
    (opt (strWs "&")) >>. sepBy1 primaryTypeWithSuffix (strWs "&")
    |>> fun typeAnns ->
      match typeAnns with
      | [ typeAnn ] -> typeAnn
      | types ->
        let intersection =
          TsUnionOrIntersectionType.TsIntersectionType
            { Types = types; Loc = None }

        TsType.TsUnionOrIntersectionType intersection

  let unionOrIntersectionOrPrimaryType: Parser<TsType, unit> =
    (opt (strWs "|")) >>. (sepBy1 intersectionOrPrimaryType (strWs "|"))
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

  let interfaceDecl: Parser<bool * bool -> Decl, unit> =
    pipe3
      ((strWs "interface") >>. ident .>>. (opt typeParams))
      (opt ((strWs "extends") >>. (sepBy1 typeRef (strWs ","))))
      interfaceBody
    <| fun (id, typeParams) extends body ->

      fun (export, declare) ->
        { Export = export
          Declare = declare
          Id = id
          TypeParams = typeParams
          Extends = extends
          Body = body
          Loc = None
          Comments = [] }
        |> Decl.TsInterface

  let param: Parser<Param, unit> =
    pipe3 pat (opt (strWs "?")) (opt (strWs ":" >>. tsTypeAnn))
    <| fun p optional typeAnn ->
      { Pat = p
        Optional = optional.IsSome
        TypeAnn = typeAnn
        Loc = None }

  let fnParams: Parser<list<Param>, unit> =
    between (strWs "(") (strWs ")") (sepEndBy param (strWs ","))

  let fnDecl: Parser<bool * bool -> Decl, unit> =
    pipe4
      (opt (strWs "async"))
      ((strWs "function") >>. ident .>>. (opt typeParams))
      fnParams
      (opt ((strWs ":") >>. tsTypeAnn))
    <| fun async (id, typeParams) ps typeAnn ->
      let fn: Function =
        { Params = ps
          Body = None
          IsGenerator = false // TODO
          IsAsync = async.IsSome
          TypeParams = typeParams
          ReturnType = typeAnn
          Loc = None }

      fun (export, declare) ->
        { Export = export
          Declare = declare
          Id = id
          Fn = fn
          Loc = None
          Comments = [] }
        |> Decl.Fn

  let enumIdent: Parser<TsEnumMemberId, unit> =
    choice [ ident |>> TsEnumMemberId.Ident; str |>> TsEnumMemberId.Str ]

  let enumMember: Parser<TsEnumMember, unit> =
    pipe2 enumIdent (opt (strWs "=" >>. expr))
    <| fun id value -> { Id = id; Init = value; Loc = None }

  let enumDecl: Parser<bool * bool -> Decl, unit> =
    pipe3
      (opt (keyword "const"))
      (keyword "enum" >>. ident)
      (between (strWs "{") (strWs "}") (sepEndBy enumMember (strWs ",")))
    <| fun isConst ident members ->
      fun (export, declare) ->
        { Export = export
          Declare = declare
          IsConst = isConst.IsSome
          Id = ident
          Members = members
          Loc = None
          Comments = [] }
        |> Decl.TsEnum

  let constructor: Parser<option<Accessibility> -> ClassMember, unit> =
    keyword "constructor" >>. fnParams
    |>> fun ps ->
      fun accessMod ->
        { Params = List.map ParamOrTsParamProp.Param ps
          Body = None
          Accessibility = accessMod
          IsOptional = false
          Loc = None }
        |> ClassMember.Constructor

  let classMethod: Parser<option<Accessibility> -> ClassMember, unit> =
    pipe5
      (tuple2
        (opt (keyword "static" <|> keyword "abstract"))
        (opt (keyword "async")))
      ident
      (opt typeParams)
      fnParams
      (opt ((strWs ":") >>. tsTypeAnn))
    <| fun (staticOrAbstract, isAsync) id typeParams ps typeAnn ->
      let fn: Function =
        { Params = ps
          Body = None
          IsGenerator = false // TODO
          IsAsync = isAsync.IsSome
          TypeParams = typeParams
          ReturnType = typeAnn
          Loc = None }

      let isStatic, isAbstract =
        match staticOrAbstract with
        | Some "static" -> true, false
        | Some "abstract" -> false, true
        | _ -> false, false

      fun accessMod ->
        let method: ClassMethod =
          { Key = PropName.Ident id
            Function = fn
            Kind = MethodKind.Method // TODO: handle getters and setters
            IsStatic = isStatic
            Accessibility = accessMod
            IsAbstract = isAbstract
            IsOptional = false
            IsOverride = false
            Loc = None }

        ClassMember.Method method

  let accessMod: Parser<Accessibility, unit> =
    choice
      [ keyword "private" |>> fun _ -> Accessibility.Private
        keyword "protected" |>> fun _ -> Accessibility.Protected
        keyword "public" |>> fun _ -> Accessibility.Public ]

  let classProp: Parser<option<Accessibility> -> ClassMember, unit> =
    pipe4
      ((opt (keyword "static")) .>>. (opt (keyword "readonly")))
      (ident .>>. (opt (pstring "?")))
      (opt (strWs ":" >>. tsTypeAnn))
      (opt (strWs "=" >>. expr))
    <| fun (isStatic, isReadonly) (name, optional) typeAnn value ->

      fun accessMod ->
        { Key = PropName.Ident name
          Value = value
          TypeAnn = typeAnn
          IsStatic = isStatic.IsSome
          // TODO=Decorators
          // decorators=list<Decorator>
          Accessibility = accessMod
          IsAbstract = false
          IsOptional = optional.IsSome
          IsOverride = false
          Readonly = isReadonly.IsSome
          Declare = false // what is this?
          Definite = false // what is this?
          Loc = None }
        |> ClassMember.ClassProp

  let classGetter: Parser<option<Accessibility> -> ClassMember, unit> =
    pipe3
      (keyword "get" >>. ident)
      (opt typeParams)
      (strWs "(" >>. strWs ")" >>. (opt (strWs ":" >>. tsTypeAnn)))
    <| fun id typeParams typeAnn ->
      let fn: Function =
        { Params = []
          Body = None
          IsGenerator = false // TODO
          IsAsync = false // TODO
          TypeParams = typeParams
          ReturnType = typeAnn
          Loc = None }

      fun accessMod ->
        { ClassMethod.Key = PropName.Ident id
          Function = fn
          Kind = MethodKind.Getter
          IsStatic = false // TODO
          Accessibility = accessMod
          IsAbstract = false // TODO
          IsOptional = false
          IsOverride = false
          Loc = None }
        |> ClassMember.Method

  let classSetter: Parser<option<Accessibility> -> ClassMember, unit> =
    pipe4
      (keyword "set" >>. ident)
      (opt typeParams)
      (between (strWs "(") (strWs ")") param)
      (opt (strWs ":" >>. tsTypeAnn))
    <| fun id typeParams param typeAnn ->
      let fn: Function =
        { Params = [ param ]
          Body = None
          IsGenerator = false // TODO
          IsAsync = false // TODO
          TypeParams = typeParams
          ReturnType = typeAnn
          Loc = None }

      fun accessMod ->
        { ClassMethod.Key = PropName.Ident id
          Function = fn
          Kind = MethodKind.Getter
          IsStatic = false // TODO
          Accessibility = accessMod
          IsAbstract = false // TODO
          IsOptional = false
          IsOverride = false
          Loc = None }
        |> ClassMember.Method

  let classMember: Parser<ClassMember, unit> =
    pipe2
      (opt accessMod)
      (choice
        [ constructor
          attempt classMethod
          attempt classGetter
          attempt classSetter
          attempt classProp ]
       .>> (opt (pstring ";"))
       .>> ws)
    <| fun accessMod mem -> mem accessMod

  let classDecl: Parser<bool * bool -> Decl, unit> =
    pipe5
      (opt (keyword "abstract"))
      ((keyword "class" >>. ident) .>>. (opt typeParams))
      (opt (keyword "extends" >>. typeRef)) // TODO: type params
      (opt (keyword "implements" >>. (sepBy typeRef (strWs ",")))) // TODO: type params
      (between (strWs "{") (strWs "}") (many classMember))
    <| fun abs (id, typeParams) extends implements members ->
      let cls: Class =
        { TypeParams = typeParams
          Super = extends
          IsAbstract = false
          Implements = implements
          Body = members
          Loc = None }

      fun (export, declare) ->
        { Export = export
          Declare = declare
          Ident = id
          Class = cls }
        |> Decl.Class

  let typeAliasDecl: Parser<bool * bool -> Decl, unit> =
    pipe3 ((strWs "type") >>. ident) (opt typeParams) ((strWs "=") >>. tsType)
    <| fun id typeParams typeAnn ->
      fun (export, declare) ->
        { Export = export
          Declare = declare
          Id = id
          TypeParams = typeParams
          TypeAnn = typeAnn
          Loc = None
          Comments = [] }
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

  let varDecl: Parser<bool * bool -> Decl, unit> =
    pipe2 varDeclKind (sepBy1 declarator (strWs ","))
    <| fun kind declarators ->
      fun (export, declare) ->
        { Export = export
          Declare = declare
          Decls = declarators
          Kind = kind
          Loc = None
          Comments = [] }
        |> Decl.Var

  let moduleBlock: Parser<TsNamespaceBody, unit> =
    (strWs "{" >>. many moduleItem .>> strWs "}")
    |>> fun body -> TsNamespaceBody.TsModuleBlock { Body = body; Loc = None }

  let namespaceBody = moduleBlock

  let namespaceDecl: Parser<bool * bool -> Decl, unit> =
    pipe2 (strWs "namespace" >>. ident) moduleBlock
    <| fun id body ->

      fun (export, declare) ->
        { Export = export
          Declare = declare
          Global = false
          Id = TsModuleName.Ident id
          Body = Some(body)
          Loc = None }
        |> Decl.TsModule

  let moduleDecl: Parser<bool * bool -> Decl, unit> =
    pipe2 (strWs "module" >>. str) moduleBlock
    <| fun id body ->

      fun (export, declare) ->
        { Export = export
          Declare = declare
          Global = false
          Id = TsModuleName.Str id
          Body = Some(body)
          Loc = None }
        |> Decl.TsModule

  let globalDecl: Parser<bool * bool -> Decl, unit> =
    keyword "global" >>. moduleBlock
    |>> fun body ->

      fun (export, declare) ->
        { Export = export
          Declare = declare
          Global = true
          Id =
            TsModuleName.Str
              { Value = "global"
                Raw = None
                Loc = None }
          Body = Some(body)
          Loc = None }
        |> Decl.TsModule

  let declareDecl (export: bool) : Parser<Decl, unit> =
    pipe2
      (opt (keyword "declare"))
      (choice
        [ attempt typeAliasDecl
          attempt varDecl
          attempt enumDecl
          attempt fnDecl
          attempt interfaceDecl
          attempt classDecl
          attempt namespaceDecl
          attempt moduleDecl
          attempt globalDecl ])
    <| fun declare decl -> decl (export, declare.IsSome)

  let importEquals (export: bool) : Parser<TsImportEqualsDecl, unit> =
    pipe3
      (keyword "import" >>. ident)
      (strWs "=" >>. entityName)
      (opt (strWs ";"))
    <| fun ident entityName _ ->
      { IsExport = export
        IsTypeOnly = false // TODO
        Id = ident
        ModuleRef = TsModuleRef.TsEntityName entityName
        Loc = None }

  let namedExportSpecifier: Parser<ExportSpecifier, unit> =
    pipe2 ident (opt (strWs "as" >>. ident))
    <| fun local exported ->
      ExportSpecifier.Named
        { Orig = ModuleExportName.Ident local
          Exported = Option.map ModuleExportName.Ident exported
          IsTypeOnly = false
          Loc = None }

  let namedExport: Parser<NamedExport, unit> =
    pipe3
      (opt (keyword "type"))
      (strWs "{" >>. sepEndBy namedExportSpecifier (strWs ",") .>> strWs "}")
      (opt (keyword "from" >>. str))
    <| fun isTypeOnly specifiers src ->
      { Specifiers = specifiers
        Src = src
        IsTypeOnly = isTypeOnly.IsSome
        With = None
        Loc = None }

  let exportAll: Parser<ExportAll, unit> =
    (strWs "*" >>. strWs "from" >>. str)
    |>> fun src ->
      { Src = src
        IsTypeOnly = false
        With = None
        Loc = None }

  let tsExportDefault: Parser<TsExportAssignment, unit> =
    (keyword "default" >>. expr) |>> fun expr -> { Expr = expr; Loc = None }

  let tsExportAssignment: Parser<TsExportAssignment, unit> =
    (strWs "=" >>. expr) |>> fun expr -> { Expr = expr; Loc = None }

  let tsNamespaceExport: Parser<TsNamespaceExportDecl, unit> =
    (keyword "as" >>. (keyword "namespace") >>. ident)
    |>> fun id -> { Id = id; Loc = None }

  let export: Parser<ModuleItem, unit> =
    pipe2
      (keyword "export")
      (choice
        [ importEquals true
          |>> ModuleDecl.TsImportEquals
          |>> ModuleItem.ModuleDecl
          declareDecl true |>> Stmt.Decl |>> ModuleItem.Stmt
          exportAll |>> ModuleDecl.ExportAll |>> ModuleItem.ModuleDecl
          namedExport |>> ModuleDecl.ExportNamed |>> ModuleItem.ModuleDecl
          tsExportDefault
          |>> ModuleDecl.TsExportAssignment
          |>> ModuleItem.ModuleDecl
          tsExportAssignment
          |>> ModuleDecl.TsExportAssignment
          |>> ModuleItem.ModuleDecl
          tsNamespaceExport
          |>> ModuleDecl.TsNamespaceExport
          |>> ModuleItem.ModuleDecl ])
    <| fun _ modDecl -> modDecl

  let namedImportSpecifier: Parser<ImportSpecifier, unit> =
    pipe2 ident (opt (strWs "as" >>. ident))
    <| fun local imported ->
      ImportSpecifier.Named
        { Local = local
          Imported = Option.map ModuleExportName.Ident imported
          IsTypeOnly = false // TODO
          Loc = None }

  let namedImports: Parser<list<ImportSpecifier>, unit> =
    (strWs "{" >>. sepEndBy namedImportSpecifier (strWs ",") .>> strWs "}")

  let namespaceImport: Parser<ImportSpecifier, unit> =
    pipe2 (strWs "*") (strWs "as" >>. ident)
    <| fun _ local -> ImportSpecifier.Namespace { Local = local; Loc = None }

  let defaultImport: Parser<ImportSpecifier, unit> =
    ident |>> fun local -> ImportSpecifier.Default { Local = local; Loc = None }

  let importSpecifiers: Parser<list<ImportSpecifier>, unit> =
    choice
      [ namespaceImport |>> fun ns -> [ ns ]
        (pipe2 defaultImport (opt (strWs "," >>. namedImports))
         <| fun def namedImport -> def :: Option.defaultValue [] namedImport)
        namedImports ]

  let import: Parser<ModuleDecl, unit> =
    pipe3
      (keyword "import" >>. (opt (keyword "type")))
      (opt (importSpecifiers .>> keyword "from"))
      str
    <| fun isTypeOnly specifiers src ->
      let specifiers =
        match specifiers with
        | Some specifiers -> specifiers
        | None -> []

      let decl: ImportDecl =
        { Specifiers = specifiers
          Src = src
          IsTypeOnly = isTypeOnly.IsSome
          With = None
          Loc = None }

      ModuleDecl.Import decl

  moduleItemRef.Value <-
    ws
    >>. choice
      [ declareDecl false |>> Stmt.Decl |>> ModuleItem.Stmt
        import |>> ModuleItem.ModuleDecl
        export ]
    .>> (opt (strWs ";"))

  let m: Parser<Module, unit> =
    ws >>. (opt (many moduleItem)) .>> eof
    |>> fun items ->
      let items =
        match items with
        | Some items -> items
        | None -> []

      { Body = items
        Shebang = None
        Loc = None }

  let parseModule (input: string) = run m input
