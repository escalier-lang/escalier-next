namespace rec Escalier.Data

open FParsec
open System.Globalization
open System.Text

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

  type Object<'T> = { Elems: list<'T>; Immutable: bool }

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
      ParamList: list<FuncParam<'T>>
      ReturnType: 'T
      Throws: option<TypeAnn>
      IsAsync: bool }

  // TODO: include optional name
  type Function =
    { Sig: FuncSig<option<TypeAnn>>
      Body: BlockOrExpr }

  type Method =
    { Name: string
      Sig: FuncSig<option<TypeAnn>>
      Body: BlockOrExpr }

  type Getter =
    { Name: string
      ReturnType: TypeAnn
      Throws: TypeAnn }

  type Setter =
    { Name: string
      Param: FuncParam<TypeAnn>
      Throws: TypeAnn }

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

  type Struct =
    { Name: string
      TypeArgs: option<list<TypeAnn>>
      Elems: list<ObjElem> }

  type Try =
    { Body: Block
      Catch: option<list<MatchCase>>
      Finally: option<Block>
      mutable Throws: option<Type.Type> }

  type Await =
    { Value: Expr
      mutable Throws: option<Type.Type> }

  [<RequireQualifiedAccess>]
  type ExprKind =
    | Identifier of name: string // TODO: Make an Ident struct
    | Literal of Common.Literal
    | Function of Function
    | Call of Call
    | Object of Common.Object<ObjElem>
    | Struct of Struct
    | Tuple of Common.Tuple<Expr>
    | Range of Common.Range<Expr>
    | Index of target: Expr * index: Expr * opt_chain: bool
    | Member of target: Expr * name: string * opt_chain: bool
    | IfElse of
      condition: Expr *
      thenBranch: Block *
      elseBranch: option<BlockOrExpr> // Expr is only used when chaining if-else expressions
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

  type QualifiedIdent =
    | Ident of string
    | Member of left: QualifiedIdent * right: string

  type TypeRef =
    { Ident: string // TOOD: use QualifiedIdent
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
      Assertion: option<QualifiedIdent> }

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
      Assertion: option<QualifiedIdent> }

  type WildcardPattern = { Assertion: option<QualifiedIdent> }

  [<RequireQualifiedAccess>]
  type PatternKind =
    | Ident of IdentPat
    | Object of Common.Object<ObjPatElem> // TODO: rest patterns
    | Tuple of Common.Tuple<Pattern> // TODO: rest patterns
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

  type StructDecl =
    { Name: string
      TypeParams: option<list<TypeParam>>
      Elems: list<Property> }

  type Impl =
    { TypeParams: option<list<TypeParam>>
      SelfType: TypeRef
      Elems: list<Method> }

  type DeclKind =
    | VarDecl of name: Pattern * init: Expr * typeAnn: option<TypeAnn>
    | TypeDecl of
      name: string *
      typeAnn: TypeAnn *
      typeParams: option<list<TypeParam>>
    | StructDecl of StructDecl

  type Decl = { Kind: DeclKind; Span: Span }

  type StmtKind =
    | Expr of Expr
    | For of left: Pattern * right: Expr * body: Block
    | Return of option<Expr>
    | Decl of Decl
    | Impl of Impl

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
    | Getter of Getter
    | Setter of Setter
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

  type FunctionType = FuncSig<TypeAnn>

  type MethodType = { Name: PropName; Type: FunctionType }

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
    | Typeof of target: QualifiedIdent
    | Index of target: TypeAnn * index: TypeAnn
    | Condition of ConditionType
    | Match of MatchType
    | Infer of name: string
    | Wildcard
    | Binary of left: TypeAnn * op: string * right: TypeAnn // TODO: BinaryOp
    | TemplateLiteral of Common.TemplateLiteral<TypeAnn>

  type TypeAnn =
    { Kind: TypeAnnKind
      Span: Span
      mutable InferredType: option<Type.Type> }

  type ImportSpecifier =
    | Named of name: string * alias: option<string>
    | ModuleAlias of alias: string

  type Import =
    { Path: string
      Specifiers: list<ImportSpecifier> }

  type ModuleItem =
    | Import of Import
    | DeclareLet of name: Pattern * typeAnn: TypeAnn
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

  ///A type variable standing for an arbitrary type.
  ///All type variables have a unique id, but names are only assigned lazily, when required.
  type TypeVar =
    { Id: int
      mutable Bound: option<Type>
      mutable Instance: option<Type> }

  ///An n-ary type constructor which builds a new type from old
  type TypeRef =
    { Name: string
      TypeArgs: option<list<Type>>
      Scheme: option<Scheme> }

  type Primitive =
    | Boolean
    | Number
    | String
    | Symbol

    override this.ToString() =
      match this with
      | Boolean -> "boolean"
      | Number -> "number"
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

  type IdentPat = { Name: string; IsMut: bool }

  type Pattern =
    | Identifier of IdentPat
    | Object of Common.Object<ObjPatElem>
    | Tuple of Common.Tuple<option<Pattern>>
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

  type FuncParam =
    { Pattern: Pattern
      Type: Type
      Optional: bool }

    override this.ToString() = $"{this.Pattern}: {this.Type}"

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
      ParamList: list<FuncParam>
      Return: Type
      Throws: Type }

    override this.ToString() = printFunction { Precedence = 0 } this

  type Struct =
    { Name: string
      TypeArgs: option<list<Type>>
      Elems: list<ObjTypeElem> }

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
    | Method of name: PropName * is_mut: bool * fn: Function
    | Getter of name: PropName * return_type: Type * throws: Type
    | Setter of name: PropName * param: FuncParam * throws: Type
    | Mapped of Mapped
    | Property of Property

    override this.ToString() =
      match this with
      | Callable(func) -> func.ToString()
      | Constructor(func) -> sprintf "new %s" (func.ToString())
      | Method(name,
               is_mut,
               { ParamList = paramList
                 Return = return_type }) ->
        let sb = StringBuilder()

        let self = if is_mut then "mut self" else "self"
        let paramList' = self :: List.map (fun p -> p.ToString()) paramList

        sb
          .Append("fn ")
          .Append(name)
          .Append("(")
          // Do we need to include `self` in types?
          .Append(if is_mut then "mut self" else "self")
          .Append(String.concat ", " paramList')
          .Append(") -> ")
          .Append(return_type)
        |> ignore

        sb.ToString()
      | Getter(name, return_type, throws) ->
        let throws =
          if throws.Kind = TypeKind.Keyword Keyword.Never then
            ""
          else
            $" throws {throws}"

        $"get {name}() -> {return_type}{throws}"
      | Setter(name, param, throws) ->
        let throws =
          if throws.Kind = TypeKind.Keyword Keyword.Never then
            ""
          else
            $" throws {throws}"

        $"set {name}({param}){throws}"
      | Mapped(mapped) -> mapped.ToString()
      | Property { Name = name
                   Optional = optional
                   Readonly = readonly
                   Type = type_ } ->
        let optional = if optional then "?" else ""
        let readonly = if readonly then "readonly " else ""

        $"{readonly}{name}{optional}: {type_}"

  type Array =
    { Elem: Type
      mutable Length: Type } // either `number` or `unique number`

    override this.ToString() = $"{this.Elem}[]"

  type Condition =
    { Check: Type
      Extends: Type
      TrueType: Type
      FalseType: Type }

  [<RequireQualifiedAccess>]
  type TypeKind =
    | TypeVar of TypeVar
    | TypeRef of TypeRef
    | Primitive of Primitive
    | Keyword of Keyword
    | Function of Function
    | Object of Common.Object<ObjTypeElem>
    | Struct of Struct
    | Tuple of Common.Tuple<Type>
    | Array of Array
    | Rest of Type
    | Literal of Common.Literal
    | Range of Common.Range<Type>
    | UniqueSymbol of id: int
    | UniqueNumber of id: int
    | Union of list<Type> // TODO: use `Set<type>`
    | Intersection of list<Type> // TODO: use `Set<type>`
    | KeyOf of Type
    | Index of target: Type * index: Type
    | Condition of Condition
    | Infer of name: string
    | Binary of left: Type * op: string * right: Type // use TypeRef? - const folding is probably a better approach
    | Wildcard
    | TemplateLiteral of Common.TemplateLiteral<Type>

  [<CustomEquality; NoComparison>]
  type Type =
    { Kind: TypeKind
      // Used when performing mutability checks in variable declaration and
      // assignments as well arguments to to functions in function calls.
      Mutable: bool
      mutable Provenance: option<Provenance> }

    override this.Equals other =
      match other with
      | :? Type as p -> p.Kind.Equals this.Kind
      | _ -> false

    override this.GetHashCode() = this.Kind.GetHashCode()

    override this.ToString() = printType { Precedence = 0 } this

  type Scheme =
    // TODO: allow type params to have constraints and defaults
    { TypeParams: option<list<string>>
      Type: Type
      IsTypeParam: bool }

    override this.ToString() =
      match this.TypeParams with
      | Some(typeParams) ->
        let typeParams = String.concat ", " typeParams
        $"<{typeParams}>({this.Type})"
      | None -> this.Type.ToString()

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
    | TypeKind.TypeVar typeVar -> 100
    | TypeKind.TypeRef typeRef -> 100
    | TypeKind.Primitive primitive -> 100
    | TypeKind.Keyword keyword -> 100
    | TypeKind.Function f -> 100
    | TypeKind.Object o -> 100
    | TypeKind.Struct s -> 100
    | TypeKind.Tuple tuple -> 100
    | TypeKind.Array array -> 17
    | TypeKind.Rest t -> 100
    | TypeKind.Literal literal -> 100
    | TypeKind.Range range -> 2
    | TypeKind.UniqueSymbol id -> 15 // because `unique` is a keyword operator
    | TypeKind.UniqueNumber id -> 15 // because `unique` is a keyword operator
    | TypeKind.Union types -> 3
    | TypeKind.Intersection types -> 4
    | TypeKind.KeyOf t -> 15 // because `keyof` is a keyword operator
    | TypeKind.Index(target, index) -> 18
    | TypeKind.Condition condition -> 100
    | TypeKind.Infer name -> 15 // because `keyof` is a keyword operator
    | TypeKind.Binary(left, op, right) ->
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
    | TypeKind.Wildcard -> 100
    | TypeKind.TemplateLiteral templateLiteral -> 100

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
      | TypeKind.Primitive primitive -> primitive.ToString()
      | TypeKind.Keyword keyword -> keyword.ToString()
      | TypeKind.Function f -> printFunction ctx f
      | TypeKind.Object obj -> printObject ctx obj
      | TypeKind.Struct s -> printStruct ctx s
      | TypeKind.Tuple { Elems = elems; Immutable = immutable } ->
        let ctx = { Precedence = 0 }
        let elems = List.map (printType ctx) elems |> String.concat ", "

        match immutable with
        | true -> $"#[{elems}]"
        | false -> $"[{elems}]"
      | TypeKind.Array { Elem = elem; Length = length } ->
        $"{printType ctx elem}[]"
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
      | TypeKind.Wildcard -> "_"
      | TypeKind.TemplateLiteral { Parts = parts; Exprs = types } ->
        failwith "TODO: printType - TypeKind.TemplateLiteral"

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

  let printObject (ctx: PrintCtx) (obj: Common.Object<ObjTypeElem>) : string =
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
          | _ -> failwith "TODO: Type.ToString - Object - Elem"

        )
        obj.Elems

    let elems = String.concat ", " elems

    match obj.Immutable with
    | true -> $"#{{{elems}}}"
    | false -> $"{{{elems}}}"

  let printStruct (ctx: PrintCtx) (s: Struct) : string =
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
          | _ -> failwith "TODO: Type.ToString - Object - Elem"

        )
        s.Elems

    let elems = String.concat ", " elems

    match s.TypeArgs with
    | Some typeArgs ->
      let typeArgs = List.map (printType ctx) typeArgs |> String.concat ", "
      $"{s.Name}<{typeArgs}> {{{elems}}}"
    | None -> $"{s.Name} {{{elems}}}"

  let printFunction (ctx: PrintCtx) (f: Function) : string =
    let ps =
      List.map
        (fun p ->
          let ctx = { Precedence = 0 }
          $"{p.Pattern}: {printType ctx p.Type}")
        f.ParamList
      |> String.concat ", "

    let typeParams =
      match f.TypeParams with
      | Some(typeParams) ->
        let sep = ", "
        let typeParams = List.map (printTypeParam ctx) typeParams
        $"<{String.concat sep typeParams}>"
      | None -> ""

    let ret = printType { Precedence = 0 } f.Return

    match (prune f.Throws).Kind with
    | TypeKind.Keyword Keyword.Never -> $"fn {typeParams}({ps}) -> {ret}"
    | _ -> $"fn {typeParams}({ps}) -> {ret} throws {printType ctx f.Throws}"

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
