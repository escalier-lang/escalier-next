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

  type Range<'T> =
    { Min: 'T
      Max: 'T } // non-inclusive

    override this.ToString() = $"{this.Min}..{this.Max}"

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

  type Block = { Span: Span; Stmts: list<Stmt> }

  type BlockOrExpr =
    | Block of Block
    | Expr of Expr

  type TypeParam =
    { Span: Span
      Name: string
      Constraint: option<TypeAnn>
      Default: option<TypeAnn> }

  type FuncParam<'T> =
    { Pattern: Pattern
      TypeAnn: 'T
      Optional: bool }

    override this.ToString() = this.Pattern.ToString()

  type FuncSig<'T> =
    { TypeParams: option<list<TypeParam>>
      Self: option<FuncParam<'T>>
      ParamList: list<FuncParam<'T>>
      ReturnType: 'T
      Throws: option<TypeAnn>
      IsAsync: bool }

  // TODO: include optional name
  type Function =
    { Sig: FuncSig<option<TypeAnn>>
      Body: BlockOrExpr }

  type Constructor =
    { Sig: FuncSig<option<TypeAnn>>
      Body: BlockOrExpr }

  type Method =
    { Name: string
      Sig: FuncSig<option<TypeAnn>>
      Body: BlockOrExpr }

  type Getter =
    { Name: string
      Self: FuncParam<option<TypeAnn>>
      Body: BlockOrExpr
      ReturnType: option<TypeAnn>
      Throws: option<TypeAnn> }

  type Setter =
    { Name: string
      Self: FuncParam<option<TypeAnn>>
      Param: FuncParam<option<TypeAnn>>
      Body: BlockOrExpr
      Throws: option<TypeAnn> }

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

  type ObjElem =
    // TODO: Add syntax for specifying callables
    // TODO: Add support for getters, setters, and methods
    | Property of span: Span * name: PropName * value: Expr
    // NOTE: using PropName here doesn't make sense because numbers aren't
    // valid identifiers and symbols can only be reference by computed properties
    // TODO: handle computed properties
    | Shorthand of span: Span * name: string
    | Spread of span: Span * value: Expr

  type Call =
    { Callee: Expr
      TypeArgs: option<list<TypeAnn>>
      Args: list<Expr>
      OptChain: bool
      mutable Throws: option<Type.Type> }

  type New =
    { Callee: Expr
      TypeArgs: option<list<TypeAnn>>
      Args: option<list<Expr>>
      mutable Throws: option<Type.Type> }

  type Try =
    { Body: Block
      Catch: option<list<MatchCase>>
      Finally: option<Block>
      mutable Throws: option<Type.Type> }

  type Await =
    { Value: Expr
      mutable Throws: option<Type.Type> }

  type Class =
    { Name: option<string>
      TypeParams: option<list<TypeParam>>
      Elems: list<ClassElem> }

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
    | Identifier of name: string // TODO: Make an Ident struct
    | Literal of Common.Literal
    | Function of Function
    | Call of Call
    | New of New
    | ExprWithTypeArgs of target: Expr * typeArgs: list<TypeAnn>
    | Object of Common.Object<ObjElem>
    | Class of Class
    | Tuple of Common.Tuple<Expr>
    | Range of Common.Range<Expr>
    | Index of target: Expr * index: Expr * opt_chain: bool
    | Member of target: Expr * name: string * opt_chain: bool
    | IfElse of
      condition: Expr *
      thenBranch: Block *
      elseBranch: option<BlockOrExpr> // Expr is only used when chaining if-else expressions
    | IfLet of
      pattern: Pattern *
      target: Expr *
      thenBranch: Block *
      elseBranch: option<BlockOrExpr>
    | Match of target: Expr * cases: list<MatchCase>
    | Assign of op: string * left: Expr * right: Expr
    | Binary of op: string * left: Expr * right: Expr // TODO: BinaryOp
    | Unary of op: string * value: Expr
    | Try of Try
    | Do of body: Block
    | Await of Await
    | Throw of value: Expr
    | TemplateLiteral of Common.TemplateLiteral<Expr>
    | TaggedTemplateLiteral of
      tag: Expr *
      template: Common.TemplateLiteral<Expr> *
      throws: option<Type.Type>
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
      Assertion: option<Common.QualifiedIdent> }

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
    { Name: string
      IsMut: bool
      Assertion: option<Common.QualifiedIdent> }

  type EnumVariantPattern =
    { Ident: Common.QualifiedIdent
      Args: option<list<Pattern>> }

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
      Pattern: Pattern
      TypeAnn: option<TypeAnn>
      Init: option<Expr>
      Else: option<Block> }

  type FnDecl =
    { Declare: bool
      Name: string
      Sig: FuncSig<option<TypeAnn>>
      Body: option<BlockOrExpr> }

  type ClassDecl = { Name: string; Class: Class }

  type TypeDecl =
    { Name: string
      TypeAnn: TypeAnn
      TypeParams: option<list<TypeParam>> }

  type InterfaceDecl =
    { Name: string
      TypeParams: option<list<TypeParam>>
      Elems: list<ObjTypeAnnElem> }

  type EnumVariant =
    { Name: string
      TypeAnns: list<TypeAnn> }

  type EnumDecl =
    { Name: string
      TypeParams: option<list<TypeParam>>
      Variants: list<EnumVariant> }

  type NamespaceDecl = { Name: string; Body: list<Decl> }

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
    | For of left: Pattern * right: Expr * body: Block
    | Return of option<Expr>
    | Decl of Decl

  type Stmt = { Kind: StmtKind; Span: Span }

  type Property =
    { Name: PropName
      TypeAnn: TypeAnn
      Optional: bool
      Readonly: bool }

  type IndexParam = { Name: string; Constraint: TypeAnn }

  type Mapped =
    { TypeParam: IndexParam
      Name: option<TypeAnn>
      TypeAnn: TypeAnn
      Optional: option<Common.MappedModifier>
      Readonly: option<Common.MappedModifier> }

  // TODO: add location information
  type ObjTypeAnnElem =
    | Callable of FunctionType
    | Constructor of FunctionType
    | Method of MethodType
    | Getter of GetterType
    | Setter of SetterType
    | Property of Property
    | Mapped of Mapped

  type KeywordTypeAnn =
    | Boolean
    | Number
    | String
    | Symbol
    | UniqueSymbol
    | UniqueNumber
    | Null
    | Undefined
    | Unknown
    | Never
    | Object
    | BigInt
    | Any // only used by .d.ts files

  type FunctionType = FuncSig<TypeAnn>

  type MethodType = { Name: PropName; Type: FunctionType }

  type GetterType =
    { Name: PropName
      Self: FuncParam<TypeAnn>
      ReturnType: TypeAnn
      Throws: option<TypeAnn> }

  type SetterType =
    { Name: PropName
      Self: FuncParam<TypeAnn>
      Param: FuncParam<TypeAnn>
      Throws: option<TypeAnn> }

  type ConditionType =
    { Check: TypeAnn
      Extends: TypeAnn
      TrueType: TypeAnn
      FalseType: TypeAnn }

  type MatchType =
    { Target: TypeAnn
      Cases: list<MatchTypeCase> }

  type MatchTypeCase = { Extends: TypeAnn; TrueType: TypeAnn }

  type TypeAnnKind =
    | Literal of Common.Literal
    | Keyword of keyword: KeywordTypeAnn
    | Object of Common.Object<ObjTypeAnnElem>
    | Tuple of Common.Tuple<TypeAnn>
    | Array of elem: TypeAnn
    | Range of Common.Range<TypeAnn>
    | Union of types: list<TypeAnn>
    | Intersection of types: list<TypeAnn>
    | TypeRef of TypeRef
    | Function of FunctionType
    | Keyof of target: TypeAnn
    | Rest of target: TypeAnn
    | Typeof of target: Common.QualifiedIdent
    | Index of target: TypeAnn * index: TypeAnn
    | Condition of ConditionType
    | Match of MatchType
    | Infer of name: string
    | Wildcard
    | Binary of left: TypeAnn * op: string * right: TypeAnn // TODO: BinaryOp
    | TemplateLiteral of Common.TemplateLiteral<TypeAnn>

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

  type ImportSpecifier =
    | Named of name: string * alias: option<string>
    | ModuleAlias of alias: string

  type Import =
    { Path: string
      Specifiers: list<ImportSpecifier> }

  type ScriptItem =
    | Import of Import
    | Stmt of Stmt // contains decls along with other statements

  type Script = { Items: list<ScriptItem> }

  type ModuleItem =
    | Import of Import
    | Decl of Decl

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

  ///A type variable standing for an arbitrary type.
  ///All type variables have a unique id, but names are only assigned lazily, when required.
  type TypeVar =
    { Id: int
      mutable Bound: option<Type>
      mutable Instance: option<Type> }

  ///An n-ary type constructor which builds a new type from old
  [<CustomEquality; NoComparison>]
  type TypeRef =
    { mutable Name: Common.QualifiedIdent
      TypeArgs: option<list<Type>>
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

    override this.ToString() =
      match this with
      | Object -> "object"
      | Unknown -> "unknown"
      | Never -> "never"

  type Class<'T> =
    { Name: option<string>
      Elems: list<'T> }

  type KeyValuePat =
    { Key: string
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
      | Enum(_) -> failwith "TODO: toString - Pattern.Enum"

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
    | Method of name: PropName * fn: Function
    | Getter of name: PropName * fn: Function
    | Setter of name: PropName * fn: Function
    | Mapped of Mapped
    | Property of Property

    override this.ToString() =
      match this with
      | Callable(func) -> func.ToString()
      | Constructor(func) -> sprintf "new %s" (func.ToString())
      | Method(name, fn) -> $"{name} {fn}" // should be `fn name<...>(...)`
      | Getter(name, fn) -> $"get {name} {fn}" // should be `get name(...)`
      | Setter(name, fn) -> $"set {name} {fn}" // should be `set name(...)`
      | Mapped(mapped) -> mapped.ToString()
      | Property { Name = name
                   Optional = optional
                   Readonly = readonly
                   Type = type_ } ->
        let optional = if optional then "?" else ""
        let readonly = if readonly then "readonly " else ""

        $"{readonly}{name}{optional}: {type_}"

  type Object =
    { Elems: list<ObjTypeElem>
      Immutable: bool
      Interface: bool }

  type Array =
    { Elem: Type
      mutable Length: Type } // either `number` or `unique number`

    override this.ToString() = $"{this.Elem}[]"

  type EnumVariant =
    { Tag: Type
      Name: string
      Types: list<Type> } // TODO: consider making these named like function params

  type Enum = { Variants: Map<string, EnumVariant> }

  type Condition =
    { Check: Type
      Extends: Type
      TrueType: Type
      FalseType: Type }

  [<RequireQualifiedAccess>]
  type TypeKind =
    | TypeVar of TypeVar
    | TypeRef of TypeRef
    | Namespace of Namespace
    | Primitive of Primitive
    | Keyword of Keyword
    | Function of Function
    | Object of Object
    | Tuple of Common.Tuple<Type>
    | Array of Array
    | EnumVariant of EnumVariant
    | Rest of Type
    | Literal of Common.Literal
    | Range of Common.Range<Type>
    | UniqueSymbol of id: int
    | UniqueNumber of id: int
    | Union of list<Type> // TODO: use `Set<type>`
    | Intersection of list<Type> // TODO: use `Set<type>`
    | KeyOf of Type
    | Typeof of Common.QualifiedIdent
    | Index of target: Type * index: Type
    | Condition of Condition
    | Infer of name: string
    | Binary of left: Type * op: string * right: Type // use TypeRef? - const folding is probably a better approach
    | Unary of op: string * arg: Type
    | Wildcard
    | TemplateLiteral of Common.TemplateLiteral<Type>

  [<CustomEquality; NoComparison>]
  type Type =
    { Kind: TypeKind
      mutable Provenance: option<Provenance> }

    override this.Equals other =
      match other with
      | :? Type as p -> p.Kind.Equals this.Kind
      | _ -> false

    override this.GetHashCode() = this.Kind.GetHashCode()

    override this.ToString() = printType { Precedence = 0 } this

  type Binding = Type * bool

  type Scheme =
    // TODO: allow type params to have constraints and defaults
    { TypeParams: option<list<TypeParam>>
      mutable Type: Type // Only used when inferring type declarations
      IsTypeParam: bool }

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
    | TypeKind.Array _ -> 17
    | TypeKind.EnumVariant _ -> 100
    | TypeKind.Rest _ -> 100
    | TypeKind.Literal _ -> 100
    | TypeKind.Range _ -> 2
    | TypeKind.UniqueSymbol _ -> 15 // because `unique` is a keyword operator
    | TypeKind.UniqueNumber _ -> 15 // because `unique` is a keyword operator
    | TypeKind.Union _ -> 3
    | TypeKind.Intersection _ -> 4
    | TypeKind.KeyOf _ -> 15 // because `keyof` is a keyword operator
    | TypeKind.Index _ -> 18
    | TypeKind.Condition _ -> 100
    | TypeKind.Infer _ -> 15 // because `keyof` is a keyword operator
    | TypeKind.Binary(_, op, _) ->
      match op with
      | "**" -> 13
      | "*"
      | "/"
      | "%" -> 12
      | "+"
      | "-"
      | "++" -> 11
      | "<"
      | "<="
      | ">"
      | ">=" -> 10
      | "=="
      | "!="
      | "==="
      | "!==" -> 9
      | "||" -> 6
      | "&&" -> 5
      | _ -> failwith $"Invalid binary operator '{op}'"
    | TypeKind.Unary(op, arg) ->
      match op with
      | "+"
      | "-"
      | "!" -> 14
      | _ -> failwith $"Invalid unary operator '{op}'"
    | TypeKind.Wildcard -> 100
    | TypeKind.TemplateLiteral _ -> 100
    | TypeKind.Typeof(_) -> failwith "TODO: getPrecedence - TypeKind.Typeof"

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
      | TypeKind.Array { Elem = elem; Length = length } ->
        $"{printType ctx elem}[]"
      | TypeKind.EnumVariant variant ->
        match variant.Types with
        | [] -> variant.Name
        | types ->
          let types = types |> List.map (printType ctx) |> String.concat ", "

          $"{variant.Name}({types})"
      | TypeKind.Rest t -> $"...{printType ctx t}"
      | TypeKind.Literal literal -> literal.ToString()
      | TypeKind.Range { Min = min; Max = max } ->
        $"{printType ctx min}..{printType ctx max}"
      | TypeKind.UniqueSymbol id -> "symbol()"
      | TypeKind.UniqueNumber id -> "unique number"
      | TypeKind.Union types ->
        List.map (printType ctx) types |> String.concat " | "
      | TypeKind.Intersection types ->
        List.map (printType ctx) types |> String.concat " & "
      | TypeKind.KeyOf t -> $"keyof {printType ctx t}"
      | TypeKind.Index(target, index) ->
        $"{printType ctx target}[{printType { Precedence = 0 } index}]"
      | TypeKind.Condition { Check = check
                             Extends = extends
                             TrueType = trueType
                             FalseType = falseType } ->
        $"{printType ctx check} extends {printType ctx extends} ? {printType ctx trueType} : {printType ctx falseType}"
      | TypeKind.Infer name -> $"infer {name}"
      | TypeKind.Binary(left, op, right) ->
        $"{printType ctx left} {op} {printType ctx right}"
      | TypeKind.Unary(op, arg) -> $"{op}{printType ctx arg}"
      | TypeKind.Wildcard -> "_"
      | TypeKind.TemplateLiteral { Parts = parts; Exprs = types } ->
        failwith "TODO: printType - TypeKind.TemplateLiteral"
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
          | Method(name, fn) -> $"{name} {fn}"
          | Getter(name, fn) -> $"get {name} {fn}"
          | Setter(name, fn) -> $"set {name} {fn}"
          | Callable func -> func.ToString()
          | Constructor func -> $"new {func}")
        obj.Elems

    let elems = String.concat ", " elems

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

  let printMapped (ctx: PrintCtx) (mapped: Mapped) : string =
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
