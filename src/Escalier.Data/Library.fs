namespace rec Escalier.Data

open FParsec
open System.Globalization

module Common =
  let ci = CultureInfo("en-US", true)

  type Number =
    | Float of float
    | Int of int

    override this.ToString() =
      match this with
      | Float value -> value.ToString(ci)
      | Int value -> value.ToString(ci)

    member this.Add(other: Number) =
      match this, other with
      | Float a, Float b -> Float(a + b)
      | Float a, Int b -> Float(a + float b)
      | Int a, Float b -> Float(float a + b)
      | Int a, Int b -> Int(a + b)

    member this.Sub(other: Number) =
      match this, other with
      | Float a, Float b -> Float(a - b)
      | Float a, Int b -> Float(a - float b)
      | Int a, Float b -> Float(float a - b)
      | Int a, Int b -> Int(a - b)

    member this.Mul(other: Number) =
      match this, other with
      | Float a, Float b -> Float(a * b)
      | Float a, Int b -> Float(a * float b)
      | Int a, Float b -> Float(float a * b)
      | Int a, Int b -> Int(a * b)

    member this.Div(other: Number) =
      match this, other with
      | Float a, Float b -> Float(a / b)
      | Float a, Int b -> Float(a / float b)
      | Int a, Float b -> Float(float a / b)
      // TODO: check if the division has a remainder and return a float if it does
      | Int a, Int b -> Float(float a / float b)

    member this.Mod(other: Number) =
      match this, other with
      | Float a, Float b -> Float(a % b)
      | Float a, Int b -> Float(a % float b)
      | Int a, Float b -> Float(float a % b)
      | Int a, Int b -> Int(a % b)

    member this.Exp(other: Number) =
      match this, other with
      | Float a, Float b -> Float(a ** b)
      | Float a, Int b -> Float(a ** float b)
      | Int a, Float b -> Float(float a ** b)
      | Int a, Int b -> Int(int ((float a) ** (float b)))

    member this.GreaterThan(other: Number) =
      match this, other with
      | Float a, Float b -> Boolean(a > b)
      | Float a, Int b -> Boolean(a > float b)
      | Int a, Float b -> Boolean(float a > b)
      | Int a, Int b -> Boolean(a > b)

    member this.GreaterThanOrEqual(other: Number) =
      match this, other with
      | Float a, Float b -> Boolean(a >= b)
      | Float a, Int b -> Boolean(a >= float b)
      | Int a, Float b -> Boolean(float a >= b)
      | Int a, Int b -> Boolean(a >= b)

    member this.LessThan(other: Number) =
      match this, other with
      | Float a, Float b -> Boolean(a < b)
      | Float a, Int b -> Boolean(a < float b)
      | Int a, Float b -> Boolean(float a < b)
      | Int a, Int b -> Boolean(a < b)

    member this.LessThanOrEqual(other: Number) =
      match this, other with
      | Float a, Float b -> Boolean(a <= b)
      | Float a, Int b -> Boolean(a <= float b)
      | Int a, Float b -> Boolean(float a <= b)
      | Int a, Int b -> Boolean(a <= b)

  type Literal =
    | Number of Number
    | String of string
    | Boolean of bool
    | Null
    | Undefined

    override this.ToString() =
      match this with
      | Number value -> value |> string
      | String value -> $"\"{value}\""
      | Boolean value -> if value then "true" else "false"
      | Null -> "null"
      | Undefined -> "undefined"

  type Object<'E> = { Elems: list<'E>; Immutable: bool }

  type Tuple<'T> = { Elems: list<'T>; Immutable: bool }

  type MappedModifier =
    | Add
    | Remove

  type TemplateLiteral<'T> =
    { Parts: list<string>; Exprs: list<'T> }

  type QualifiedIdent =
    | Ident of string
    | Member of left: QualifiedIdent * right: string

    override this.ToString() =
      match this with
      | Ident value -> value
      | Member(left, right) -> $"{left}.{right}"

module Syntax =
  type Span = { Start: Position; Stop: Position }

  type LineComment = { Span: Span; Text: string }

  type Block = { Span: Span; Stmts: list<Stmt> }

  [<RequireQualifiedAccess>]
  type BlockOrExpr =
    | Block of Block
    | Expr of Expr

  type TypeParam =
    { Span: Span
      Name: string
      Constraint: option<TypeAnn>
      Default: option<TypeAnn> }

  type FuncParam =
    { Pattern: Pattern
      Optional: bool
      TypeAnn: option<TypeAnn> }

    override this.ToString() = this.Pattern.ToString()

  type FuncSig =
    { TypeParams: option<list<TypeParam>>
      Self: option<FuncParam>
      ParamList: list<FuncParam>
      ReturnType: option<TypeAnn>
      Throws: option<TypeAnn>
      IsAsync: bool }

  type Identifier = { mutable Name: string }

  // TODO: include optional name
  type Function =
    { Sig: FuncSig
      Body: BlockOrExpr
      mutable Captures: option<list<string>> }

  type Constructor =
    { Sig: FuncSig
      Body: option<BlockOrExpr> }

  type Method =
    { Name: PropName
      Sig: FuncSig
      Body: option<BlockOrExpr>
      Static: bool }

  type Getter =
    { Name: PropName
      Self: option<FuncParam>
      Body: option<BlockOrExpr>
      ReturnType: option<TypeAnn>
      Throws: option<TypeAnn>
      Static: bool }

  type Setter =
    { Name: PropName
      Self: option<FuncParam>
      Param: FuncParam
      Body: option<BlockOrExpr>
      Throws: option<TypeAnn>
      Static: bool }

  type MatchCase =
    { Span: Span
      Pattern: Pattern
      Guard: option<Expr>
      Body: BlockOrExpr }

  type PropName =
    | Ident of string
    | String of string
    | Number of Common.Number
    | Computed of Expr

    override this.ToString() =
      match this with
      | Ident value -> $"{value}"
      | String value -> $"\"{value}\""
      | Number value -> $"[{value}]"
      | Computed expr -> $"[{expr}]"

  type ClassElem =
    | Property of Property
    | Constructor of Constructor
    | Method of Method
    | Getter of Getter
    | Setter of Setter

  type ObjProperty =
    { Span: Span
      Name: PropName
      Value: Expr }

  type ObjShorthand = { Span: Span; Name: string }
  type ObjSpread = { Span: Span; Value: Expr }

  // TODO: Add syntax for specifying callables
  // TODO: Add support for getters, setters, and methods
  type ObjElem =
    | Property of ObjProperty
    | Shorthand of ObjShorthand
    | Spread of ObjSpread

  type Call =
    { Callee: Expr
      TypeArgs: option<list<TypeAnn>>
      Args: list<Expr>
      OptChain: bool
      mutable Throws: option<Type.Type> }

  type ExprWithTypeArgs = { Expr: Expr; TypeArgs: list<TypeAnn> }

  type Class =
    { Extends: option<TypeRef>
      Implements: option<list<TypeRef>>
      Name: option<string>
      TypeParams: option<list<TypeParam>>
      Elems: list<ClassElem> }

  type Index =
    { Target: Expr
      Index: Expr
      OptChain: bool }

  type Member =
    { Target: Expr
      Name: string
      OptChain: bool }

  type IfElse =
    { Condition: Expr
      Then: Block
      Else: option<BlockOrExpr> } // Expr is only used when chaining if-else expressions

  type IfLet =
    { Pattern: Pattern
      Target: Expr
      Then: Block
      Else: option<BlockOrExpr> }

  type Match =
    { Target: Expr; Cases: list<MatchCase> }

  type Binary = { Op: string; Left: Expr; Right: Expr }

  type Unary = { Op: string; Value: Expr }

  type Try =
    { Body: Block
      Catch: option<list<MatchCase>>
      Finally: option<Block>
      mutable Throws: option<Type.Type> }

  type Await =
    { Value: Expr
      mutable Throws: option<Type.Type> }

  type TaggedTemplateLiteral =
    { Tag: Expr
      Template: Common.TemplateLiteral<Expr>
      mutable Throws: option<Type.Type> }

  type JSXElement =
    { Opening: JSXElementOpening
      Children: list<JSXElementChild>
      Closing: option<JSXElementClosing> }

  type JSXElementOpening =
    { Name: Common.QualifiedIdent
      Attrs: list<JSXAttr>
      Span: Span }

  type JSXElementClosing =
    { Name: Common.QualifiedIdent
      Span: Span }

  type JSXAttr =
    { Name: string
      Value: option<JSXAttrValue>
      Span: Span }

  type JSXAttrValue =
    | Str of Common.Literal
    | JSXExprContainer of JSXExprContainer
    | JSXElement of JSXElement
    | JSXFragment of JSXFragment

  type JSXExprContainer = { Expr: Expr; Span: Span }

  type JSXElementChild =
    | JSXText of JSXText
    | JSXExprContainer of JSXExprContainer
    | JSXElement of JSXElement
    | JSXFragment of JSXFragment

  type JSXText = { Text: string; Span: Span }

  type JSXFragment =
    { Opening: JSXFragmentOpening
      Children: list<JSXElementChild>
      Closing: JSXFragmentClosing }

  type JSXFragmentOpening = { Span: Span }
  type JSXFragmentClosing = { Span: Span }

  [<RequireQualifiedAccess>]
  type ExprKind =
    | Identifier of Identifier
    | Literal of Common.Literal
    | Function of Function
    | Call of Call
    | ExprWithTypeArgs of ExprWithTypeArgs
    | Object of Common.Object<ObjElem>
    | Class of Class
    | Tuple of Common.Tuple<Expr>
    | Index of Index
    | Member of Member
    | IfElse of IfElse
    | IfLet of IfLet
    | Match of Match
    | Assign of Binary
    | Binary of Binary
    | Unary of Unary
    | Try of Try
    | Do of Block
    | Await of Await
    | Throw of Expr
    | TemplateLiteral of Common.TemplateLiteral<Expr>
    | TaggedTemplateLiteral of TaggedTemplateLiteral
    | JSXElement of JSXElement
    | JSXFragment of JSXFragment

  [<CustomEquality; NoComparison>]
  type Expr =
    { Kind: ExprKind
      Span: Span
      mutable InferredType: option<Type.Type> }

    override this.ToString() = this.Kind.ToString()

    override this.Equals other =
      match other with
      | :? Expr as p -> p.Kind.Equals this.Kind
      | _ -> false

    override this.GetHashCode() = this.Kind.GetHashCode()

  type TypeRef =
    { Ident: Common.QualifiedIdent
      TypeArgs: option<list<TypeAnn>> }

  type KeyValuePat =
    { Span: Span
      Key: string
      Value: Pattern
      Default: option<Expr> }

  type ShorthandPat =
    { Span: Span
      Name: string
      IsMut: bool
      Default: option<Expr>
      Assertion: option<Common.QualifiedIdent>
      mutable Inferred: option<Type.Type> }

  type RestPat =
    { Span: Span
      Target: Pattern
      IsMut: bool }

  // | {mut msg is string, ...mut rest}

  type ObjPatElem =
    | KeyValuePat of KeyValuePat
    | ShorthandPat of ShorthandPat
    | RestPat of RestPat

  type IdentPat =
    { mutable Name: string
      mutable IsMut: bool
      Assertion: option<Common.QualifiedIdent> }

  type EnumVariantPattern =
    { Ident: Common.QualifiedIdent
      Arg: option<Pattern> }

  type WildcardPattern =
    { Assertion: option<Common.QualifiedIdent> }

  [<RequireQualifiedAccess>]
  type PatternKind =
    | Ident of IdentPat
    | Object of Common.Object<ObjPatElem> // TODO: rest patterns
    | Tuple of Common.Tuple<Pattern> // TODO: rest patterns
    | Enum of EnumVariantPattern
    | Wildcard of WildcardPattern
    | Literal of Common.Literal
    | Rest of Pattern

    override this.ToString() =
      match this with
      | Ident ident -> ident.Name // TODO: isMut
      | _ -> failwith "TODO"

  type Pattern =
    { Kind: PatternKind
      Span: Span
      mutable InferredType: option<Type.Type> }

    override this.ToString() = this.Kind.ToString()

  type VarDecl =
    { Declare: bool
      Export: bool
      Pattern: Pattern
      TypeAnn: option<TypeAnn>
      Init: option<Expr>
      Else: option<Block> }

  type FnDecl =
    { Declare: bool
      Export: bool
      Name: string
      Sig: FuncSig
      Body: option<BlockOrExpr>

      // This is to support overloaded the functions.  The alternative would be
      // to try to split up the inferred intersection type into multiple function
      // types, but that would likely be more work.  In the future we'll want to
      // support having docstrings on functions and this will make it easier to
      // do that.
      mutable InferredFunction: Option<Type.Function> }

  type ClassDeclInferredTypes =
    { Instance: Type.Scheme
      Statics: Type.Type }

  type ClassDecl =
    { Declare: bool
      Export: bool
      Name: string
      Class: Class }

  type TypeDecl =
    { Declare: bool
      Export: bool
      Name: string
      TypeAnn: TypeAnn
      TypeParams: option<list<TypeParam>> }

  type InterfaceDecl =
    { Declare: bool
      Export: bool
      Name: string
      TypeParams: option<list<TypeParam>>
      Extends: option<list<TypeRef>>
      Elems: list<ObjTypeAnnElem> }

  type EnumVariantKind =
    | Object of TypeAnn
    | Tuple of TypeAnn
    | Empty

  type EnumVariant =
    { Name: string
      TypeAnn: option<TypeAnn> // Must only be an object or tuple type
      Init: option<Expr> // Can only be a numer literal or string literal
      Span: Span }

  type EnumDecl =
    { Declare: bool
      Export: bool
      Name: string
      TypeParams: option<list<TypeParam>>
      Variants: list<EnumVariant> }

  type NamespaceDecl =
    { Declare: bool
      Export: bool
      Name: string
      Body: list<Decl> }

  type For =
    { Left: Pattern
      Right: Expr
      Body: Block }

  type DeclKind =
    | VarDecl of VarDecl
    | FnDecl of FnDecl
    | ClassDecl of ClassDecl
    | TypeDecl of TypeDecl
    | InterfaceDecl of InterfaceDecl
    | EnumDecl of EnumDecl
    | NamespaceDecl of NamespaceDecl

  type Decl = { Kind: DeclKind; Span: Span }

  type StmtKind =
    | Expr of Expr
    | For of For
    | Return of option<Expr>
    | Decl of Decl

  type Stmt = { Kind: StmtKind; Span: Span }

  type Property =
    { Name: PropName
      TypeAnn: option<TypeAnn>
      Value: option<Expr>
      Optional: bool
      Readonly: bool
      Static: bool }

  type IndexParam = { Name: string; Constraint: TypeAnn }

  type Mapped =
    { TypeParam: IndexParam
      Name: option<TypeAnn>
      TypeAnn: TypeAnn
      Optional: option<Common.MappedModifier>
      Readonly: option<Common.MappedModifier> }

  type Spread = { Arg: TypeAnn }

  // TODO: add location information
  type ObjTypeAnnElem =
    | Callable of FuncSig
    | Constructor of FuncSig
    | Method of MethodType
    | Getter of GetterType
    | Setter of SetterType
    | Property of Property
    | Mapped of Mapped
    | Spread of Spread

  type ObjTypeAnn =
    { Elems: list<ObjTypeAnnElem>
      Immutable: bool
      Exact: bool }

  type KeywordTypeAnn =
    | Boolean
    | Number
    | String
    | Symbol
    | UniqueSymbol
    | Null
    | Undefined
    | Unknown
    | Never
    | Object
    | BigInt
    | Any // only used by .d.ts files

  type MethodType = { Name: PropName; Type: FuncSig }

  type GetterType =
    { Name: PropName
      ReturnType: TypeAnn
      Throws: option<TypeAnn> }

  type SetterType =
    { Name: PropName
      Param: FuncParam
      Throws: option<TypeAnn> }

  type IndexType = { Target: TypeAnn; Index: TypeAnn }

  type ConditionType =
    { Check: TypeAnn
      Extends: TypeAnn
      TrueType: TypeAnn
      FalseType: TypeAnn }

  type MatchType =
    { Target: TypeAnn
      Cases: list<MatchTypeCase> }

  type MatchTypeCase = { Extends: TypeAnn; TrueType: TypeAnn }

  type ImportType =
    { Src: string
      Qualifier: option<Common.QualifiedIdent>
      TypeArgs: option<list<TypeAnn>> }

  type TypeAnnKind =
    | Literal of Common.Literal
    | Keyword of KeywordTypeAnn
    | Object of ObjTypeAnn
    | Tuple of Common.Tuple<TypeAnn>
    | Union of list<TypeAnn>
    | Intersection of list<TypeAnn>
    | TypeRef of TypeRef
    // TODO: parameterize the FuncSig from the Type module and use it here
    // we'll have to move FuncSig to Common to do this
    | Function of FuncSig
    | Keyof of TypeAnn
    | Rest of TypeAnn
    | Typeof of Common.QualifiedIdent
    | Index of IndexType
    | Condition of ConditionType
    | Match of MatchType
    | Infer of string
    | Wildcard
    | TemplateLiteral of Common.TemplateLiteral<TypeAnn>
    | Intrinsic
    | ImportType of ImportType

  [<CustomEquality; NoComparison>]
  type TypeAnn =
    { Kind: TypeAnnKind
      Span: Span
      mutable InferredType: option<Type.Type> }

    override this.Equals other =
      match other with
      | :? Expr as p -> p.Kind.Equals this.Kind
      | _ -> false

    override this.GetHashCode() = this.Kind.GetHashCode()

  type Named = { Name: string; Alias: option<string> }
  type ModuleAlias = { Alias: string }

  type ImportSpecifier =
    | Named of Named
    | ModuleAlias of ModuleAlias

  type Import =
    { Path: string
      Specifiers: list<ImportSpecifier> }

  type ExportSpecifier = Named of Named

  type NamedExport =
    { Src: option<string>
      Specifiers: list<ExportSpecifier> }

  type ExportAll = { Src: string }

  type Export =
    | NamespaceExport of Identifier
    | NamedExport of NamedExport
    | ExportAll of ExportAll
    | ExportDefault of Expr

  type ModuleItem =
    | Import of Import
    | Export of Export
    | Stmt of Stmt

  type Module = { Items: list<ModuleItem> }

module Type =
  type PropName =
    | String of string
    | Number of Common.Number
    | Symbol of int // symbol id

    override this.ToString() =
      match this with
      // TODO: check if `value` is a valid identifier or not and output a
      // computed property if it isn't.
      | String value -> value.ToString()
      | Number value -> value.ToString()
      | Symbol id -> $"[Symbol({id})]"

  [<RequireQualifiedAccess>]
  type Provenance =
    | Type of Type
    | Expr of Syntax.Expr
    | TypeAnn of Syntax.TypeAnn
    | Pattern of Syntax.Pattern

  // A type variable standing for an arbitrary type.
  // All type variables have a unique id, but names are only assigned lazily, when required.
  // TODO: add a `Default` field to allow type variables to have default types
  // This will be used by `unifyFuncCall`.
  type TypeVar =
    { Id: int
      mutable Bound: option<Type>
      mutable Default: option<Type>
      mutable Instance: option<Type> }

  // An n-ary type constructor which builds a new type from old
  [<CustomEquality; NoComparison>]
  type TypeRef =
    { mutable Name: Common.QualifiedIdent
      TypeArgs: option<list<Type>>
      Mutable: bool
      Scheme: option<Scheme> }

    // TODO: include `Scheme` in the equality check
    // First we need to figure out how to check the equality of
    // recursive types like `type Foo = number | Foo[]`
    override this.Equals other =
      match other with
      | :? TypeRef as p ->
        (p.Name.Equals this.Name)
        && match p.TypeArgs, this.TypeArgs with
           | Some typeArgs1, Some typeArgs2 -> typeArgs1.Equals typeArgs2
           | None, None -> true
           | _ -> false
      | _ -> false

    override this.GetHashCode() =
      (this.Name, this.TypeArgs).GetHashCode()

  type Primitive =
    | Boolean
    | Number
    | BigInt
    | String
    | Symbol

    override this.ToString() =
      match this with
      | Boolean -> "boolean"
      | Number -> "number"
      | BigInt -> "bigint"
      | String -> "string"
      | Symbol -> "symbol"

  type Keyword =
    | Object
    | Unknown
    | Never
    | GlobalThis

    override this.ToString() =
      match this with
      | Object -> "object"
      | Unknown -> "unknown"
      | Never -> "never"
      | GlobalThis -> "globalThis"

  type Class<'T> =
    { Name: option<string>
      Elems: list<'T> }

  type KeyValuePat =
    { Key: string // TODO: use PropName for this
      Value: Pattern
      Init: option<Syntax.Expr> }

  type ShorthandPat =
    { Name: string
      Init: option<Syntax.Expr>
      IsMut: bool }

  type ObjPatElem =
    | KeyValuePat of KeyValuePat
    | ShorthandPat of ShorthandPat
    | RestPat of Pattern

  type EnumVariantPat =
    { Tag: Type
      Ident: Common.QualifiedIdent // for human readable for
      Args: option<list<Pattern>> }

  type IdentPat = { Name: string; IsMut: bool }

  [<RequireQualifiedAccess>]
  type Pattern =
    | Identifier of IdentPat
    | Object of Common.Object<ObjPatElem>
    | Tuple of Common.Tuple<option<Pattern>>
    | Enum of EnumVariantPat // isn't being used and we may not need it
    | Literal of Common.Literal
    | Rest of Pattern
    | Wildcard

    override this.ToString() =
      match this with
      | Identifier { Name = name; IsMut = mut } ->
        match mut with
        | true -> $"mut {name}"
        | false -> name
      | Object { Elems = elems; Immutable = immutable } ->
        let elems =
          List.map
            (fun elem ->
              match elem with
              | KeyValuePat { Key = key
                              Value = value
                              Init = init } ->
                match init with
                | Some(init) -> $"{key}: {value} = {init}"
                | None -> $"{key}: {value}"
              | ShorthandPat { Name = name
                               Init = init
                               IsMut = mut } ->
                let name =
                  match mut with
                  | true -> $"mut {name}"
                  | false -> name

                match init with
                | Some(value) -> $"{name} = {value}"
                | None -> name
              | RestPat(target) -> $"...{target}")
            elems

        let elems = String.concat ", " elems

        match immutable with
        | true -> $"#{{{elems}}}"
        | false -> $"{{{elems}}}"
      | Tuple { Elems = elems; Immutable = immutable } ->
        let elems =
          List.map (fun item -> item.ToString()) elems |> String.concat ", "

        match immutable with
        | true -> $"#[{elems}]"
        | false -> $"[{elems}]"
      | Wildcard -> "_"
      | Literal lit -> lit.ToString()
      | Rest(target) -> $"...{target}"
      | Enum _ -> failwith "TODO: toString - Pattern.Enum"

  type FuncParam =
    { Pattern: Pattern
      Type: Type
      Optional: bool }

    override this.ToString() =
      match this.Optional with
      | true -> $"{this.Pattern}?: {this.Type}"
      | false -> $"{this.Pattern}: {this.Type}"

  type TypeParam =
    { Name: string
      Constraint: option<Type>
      Default: option<Type> }

    override this.ToString() =
      let c =
        match this.Constraint with
        | Some(c) -> $": {c}"
        | None -> ""

      let d =
        match this.Default with
        | Some(d) -> $" = {d}"
        | None -> ""

      $"{this.Name}{c}{d}"

  type Function =
    { TypeParams: option<list<TypeParam>>
      Self: option<FuncParam>
      ParamList: list<FuncParam>
      Return: Type
      // used to set throws to `never` on ambient function decls
      mutable Throws: Type }

    override this.ToString() = printFunction { Precedence = 0 } this

  type Method = { Name: PropName; Fn: Function }

  type IndexParam =
    { Name: string
      Constraint: Type }

    override this.ToString() = $"{this.Name}: {this.Constraint}"

  type Mapped =
    { TypeParam: IndexParam
      NameType: option<Type> // has to simplify to a valid key type
      TypeAnn: Type
      Optional: option<Common.MappedModifier>
      Readonly: option<Common.MappedModifier> }

    override this.ToString() = printMapped { Precedence = 0 } this

  type Property =
    { Name: PropName
      Optional: bool
      Readonly: bool
      Type: Type }

  type ObjTypeElem =
    | Callable of Function
    | Constructor of Function
    | Method of Method
    | Getter of Method
    | Setter of Method
    | Mapped of Mapped
    | Property of Property
    | RestSpread of Type

    override this.ToString() =
      match this with
      | Callable func -> func.ToString()
      | Constructor func -> sprintf "new %s" (func.ToString())
      | Method { Name = name; Fn = fn } -> $"{name} {fn}" // should be `fn name<...>(...)`
      | Getter { Name = name; Fn = fn } -> $"get {name} {fn}" // should be `get name(...)`
      | Setter { Name = name; Fn = fn } -> $"set {name} {fn}" // should be `set name(...)`
      | Mapped mapped -> mapped.ToString()
      | Property { Name = name
                   Optional = optional
                   Readonly = readonly
                   Type = type_ } ->
        let optional = if optional then "?" else ""
        let readonly = if readonly then "readonly " else ""

        $"{readonly}{name}{optional}: {type_}"
      | RestSpread t -> $"...{t}"

  type Object =
    { Extends: option<list<TypeRef>> // classes can only have one, interfaces can have many
      Implements: option<list<TypeRef>>
      mutable Elems: list<ObjTypeElem> // this is mutable to support interface merging
      Exact: bool // Can't be true if any of Interface, Implements, or Extends are true
      Immutable: bool // True for `#{...}`, False for `{...}`
      Mutable: bool // True for `mut {...}`, False for `{...}`
      Interface: bool }

  type Tuple =
    { Elems: list<Type>
      Mutable: bool
      Immutable: bool }

  type Index = { Target: Type; Index: Type }

  type Condition =
    { Check: Type
      Extends: Type
      TrueType: Type
      FalseType: Type }

  type Binary = { Op: string; Left: Type; Right: Type }

  type Unary = { Op: string; Arg: Type }

  type IntrinsicInstance =
    { Name: Common.QualifiedIdent
      TypeArgs: option<list<Type>> }

  [<RequireQualifiedAccess>]
  type TypeKind =
    | TypeVar of TypeVar
    | TypeRef of TypeRef
    | Namespace of Namespace
    | Primitive of Primitive
    | Keyword of Keyword
    | Function of Function
    | Object of Object
    | Tuple of Tuple
    | RestSpread of Type // whether it's rest or spread depends on how the type is being used
    | Literal of Common.Literal
    | UniqueSymbol of id: int
    | Union of list<Type> // TODO: use `Set<type>`
    | Intersection of list<Type> // TODO: use `Set<type>`
    | KeyOf of Type
    | Typeof of Common.QualifiedIdent
    | Index of Index
    | Condition of Condition
    | Infer of string
    | Wildcard
    | TemplateLiteral of Common.TemplateLiteral<Type>
    | Intrinsic
    | IntrinsicInstance of IntrinsicInstance

  [<CustomEquality; NoComparison>]
  type Type =
    { mutable Kind: TypeKind // Modified to rename AnonymousClass to the class name
      mutable Provenance: option<Provenance> }

    override this.Equals other =
      match other with
      | :? Type as p -> p.Kind.Equals this.Kind
      | _ -> false

    override this.GetHashCode() = this.Kind.GetHashCode()

    override this.ToString() = printType { Precedence = 0 } this

  type Binding =
    { Type: Type
      Mutable: bool
      Export: bool }

  type Scheme =
    { TypeParams: option<list<TypeParam>>
      mutable Type: Type } // Modified used when inferring type declarations

    override this.ToString() =
      match this.TypeParams with
      | Some(typeParams) ->
        let typeParams =
          typeParams |> List.map (_.ToString()) |> String.concat ", "

        $"<{typeParams}>({this.Type})"
      | None -> this.Type.ToString()

  type Namespace =
    { Name: string
      Values: Map<string, Binding>
      Schemes: Map<string, Scheme>
      Namespaces: Map<string, Namespace> }

    static member empty =
      { Name = "<root>"
        Values = Map.empty
        Schemes = Map.empty
        Namespaces = Map.empty }

  // TODO: Figure out how to share this code with TypeChecker.Prune
  let rec prune (t: Type) : Type =
    match t.Kind with
    | TypeKind.TypeVar({ Instance = Some(instance) } as v) ->
      let newInstance = prune instance
      v.Instance <- Some(newInstance)
      newInstance
    | _ -> t

  type PrintCtx = { Precedence: int }

  let getPrecedence (t: Type) : int =
    match t.Kind with
    | TypeKind.TypeVar _ -> 100
    | TypeKind.TypeRef _ -> 100
    | TypeKind.Namespace _ -> 100
    | TypeKind.Primitive _ -> 100
    | TypeKind.Keyword _ -> 100
    | TypeKind.Function _ -> 100
    | TypeKind.Object _ -> 100
    | TypeKind.Tuple _ -> 100
    | TypeKind.RestSpread _ -> 100
    | TypeKind.Literal _ -> 100
    | TypeKind.UniqueSymbol _ -> 15 // because `unique` is a keyword operator
    | TypeKind.Union _ -> 3
    | TypeKind.Intersection _ -> 4
    | TypeKind.KeyOf _ -> 15 // because `keyof` is a keyword operator
    | TypeKind.Index _ -> 18
    | TypeKind.Condition _ -> 100
    | TypeKind.Infer _ -> 15 // because `keyof` is a keyword operator
    | TypeKind.Wildcard -> 100
    | TypeKind.TemplateLiteral _ -> 100
    | TypeKind.Intrinsic -> 100
    | TypeKind.IntrinsicInstance _ -> 100
    | TypeKind.Typeof _ -> failwith "TODO: getPrecedence - TypeKind.Typeof"

  let rec printType (ctx: PrintCtx) (t: Type) : string =
    let outerPrec = ctx.Precedence
    let t = prune t
    let innerPrec = getPrecedence t

    let ctx = { Precedence = innerPrec }

    let result =
      match t.Kind with
      | TypeKind.TypeVar({ Instance = Some(instance) }) ->
        printType ctx instance
      | TypeKind.TypeVar({ Instance = None; Bound = bound } as v) ->
        let bound =
          match bound with
          | Some(bound) -> $":{printType ctx bound}"
          | None -> ""

        $"t{v.Id}{bound}"
      | TypeKind.TypeRef({ Name = name; TypeArgs = typeArgs }) ->
        let typeArgs =
          match typeArgs with
          | Some(typeArgs) ->
            let sep = ", "
            let ctx = { Precedence = 0 }
            $"<{typeArgs |> List.map (printType ctx) |> String.concat sep}>"
          | None -> ""

        $"{name}{typeArgs}"
      | TypeKind.Namespace ns -> ns.ToString()
      | TypeKind.Primitive primitive -> primitive.ToString()
      | TypeKind.Keyword keyword -> keyword.ToString()
      | TypeKind.Function f -> printFunction ctx f
      | TypeKind.Object obj -> printObject ctx obj
      | TypeKind.Tuple { Elems = elems; Immutable = immutable } ->
        let ctx = { Precedence = 0 }
        let elems = List.map (printType ctx) elems |> String.concat ", "

        match immutable with
        | true -> $"#[{elems}]"
        | false -> $"[{elems}]"
      | TypeKind.RestSpread t -> $"...{printType ctx t}"
      | TypeKind.Literal literal -> literal.ToString()
      | TypeKind.UniqueSymbol _ -> "unique symbol"
      | TypeKind.Union types ->
        List.map (printType ctx) types |> String.concat " | "
      | TypeKind.Intersection types ->
        List.map (printType ctx) types |> String.concat " & "
      | TypeKind.KeyOf t -> $"keyof {printType ctx t}"
      | TypeKind.Index { Target = target; Index = index } ->
        $"{printType ctx target}[{printType { Precedence = 0 } index}]"
      | TypeKind.Condition { Check = check
                             Extends = extends
                             TrueType = trueType
                             FalseType = falseType } ->
        $"{printType ctx check} extends {printType ctx extends} ? {printType ctx trueType} : {printType ctx falseType}"
      | TypeKind.Infer name -> $"infer {name}"
      | TypeKind.Wildcard -> "_"
      | TypeKind.TemplateLiteral { Parts = parts; Exprs = types } ->
        let mutable output = ""

        for part, t in List.zip (List.take types.Length parts) types do
          output <- output + part + $"${{{printType ctx t}}}"

        output <- output + List.last parts

        $"`{output}`"
      | TypeKind.Intrinsic -> "intrinsic"
      | TypeKind.IntrinsicInstance { Name = name; TypeArgs = typeArgs } ->
        let typeArgs =
          match typeArgs with
          | Some(typeArgs) ->
            let sep = ", "
            let ctx = { Precedence = 0 }
            $"<{typeArgs |> List.map (printType ctx) |> String.concat sep}>"
          | None -> ""

        $"**{name}{typeArgs}**"
      | TypeKind.Typeof(qualifiedIdent) -> $"typeof {qualifiedIdent}"

    if innerPrec < outerPrec then $"({result})" else result

  let printTypeParam (ctx: PrintCtx) (typeParam: TypeParam) : string =
    let c =
      match typeParam.Constraint with
      | Some(c) -> $": {printType ctx c}"
      | None -> ""

    let d =
      match typeParam.Default with
      | Some(d) -> $" = {printType ctx d}"
      | None -> ""

    $"{typeParam.Name}{c}{d}"

  let printObject (ctx: PrintCtx) (obj: Object) : string =
    let elems =
      List.map
        (fun (elem: ObjTypeElem) ->
          match elem with
          | Property { Name = name
                       Optional = optional
                       Readonly = readonly
                       Type = t } ->
            let ctx = { Precedence = 0 }
            let optional = if optional then "?" else ""
            let readonly = if readonly then "readonly " else ""
            $"{readonly}{name}{optional}: {printType ctx t}"
          | Mapped mapped -> printMapped ctx mapped
          | Method { Name = name; Fn = fn } -> $"{name} {fn}"
          | Getter { Name = name; Fn = fn } -> $"get {name} {fn}"
          | Setter { Name = name; Fn = fn } -> $"set {name} {fn}"
          | Callable func -> func.ToString()
          | Constructor func -> $"new {func}"
          | RestSpread t -> $"...{t}")
        obj.Elems

    let elems =
      match elems with
      | [] ->
        match obj.Exact with
        | true -> ""
        | false -> "..."
      | elems ->
        match obj.Exact with
        | true -> String.concat ", " elems
        | false -> (String.concat ", " elems) + ", ..."

    match obj.Immutable with
    | true -> $"#{{{elems}}}"
    | false -> $"{{{elems}}}"

  let printFunction (ctx: PrintCtx) (f: Function) : string =
    let paramList =
      match f.Self with
      | Some(self) -> self :: f.ParamList
      | None -> f.ParamList

    let paramList = List.map (_.ToString()) paramList |> String.concat ", "

    let typeParams =
      match f.TypeParams with
      | Some(typeParams) ->
        let sep = ", "
        let typeParams = List.map (printTypeParam ctx) typeParams
        $"<{String.concat sep typeParams}>"
      | None -> ""

    let ret = printType { Precedence = 0 } f.Return

    match (prune f.Throws).Kind with
    | TypeKind.Keyword Keyword.Never -> $"fn {typeParams}({paramList}) -> {ret}"
    | _ ->
      $"fn {typeParams}({paramList}) -> {ret} throws {printType ctx f.Throws}"

  let printMapped (_ctx: PrintCtx) (mapped: Mapped) : string =
    let name =
      match mapped.NameType with
      | Some(t) -> t.ToString()
      | None -> mapped.TypeParam.Name

    let optional =
      match mapped.Optional with
      | Some(optional) ->
        match optional with
        | Common.MappedModifier.Add -> "+?"
        | Common.MappedModifier.Remove -> "-?"
      | None -> ""

    let readonly =
      match mapped.Readonly with
      | Some(readonly) ->
        match readonly with
        | Common.MappedModifier.Add -> "+readonly"
        | Common.MappedModifier.Remove -> "-readonly"
      | None -> ""

    let typeAnn = printType { Precedence = 0 } mapped.TypeAnn
    let c = printType { Precedence = 0 } mapped.TypeParam.Constraint

    $"{readonly}[{name}]{optional}: {typeAnn} for {mapped.TypeParam.Name} in {c}"
