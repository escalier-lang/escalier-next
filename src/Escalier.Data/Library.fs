namespace rec Escalier.Data

module Syntax =
  type Span = { start: int; stop: int }

  type DeclKind =
    | TypeDecl of
      name: string *
      type_ann: TypeAnn *
      type_params: option<list<TypeParam>>
    | VarDecl of
      pattern: Pattern *
      init: option<Expr> *
      type_ann: option<TypeAnn> *
      is_declare: bool

  type Decl = { span: Span; kind: DeclKind }

  type StmtKind =
    | Expr of Expr
    | For of leff: Pattern * right: Expr * body: Block
    | Return of option<Expr>
    | Decl of Decl

  type Stmt = { span: Span; kind: StmtKind }

  type Literal =
    | Number of string
    | String of string
    | Boolean of bool
    | Null
    | Undefined

  type ObjPatElem =
    | KeyValue of span: Span * key: string * value: Pattern * init: option<Expr>
    | Shorthand of span: Span * name: string * init: option<Expr> * is_mut: bool

  [<RequireQualifiedAccess>]
  type PatternKind =
    | Identifier of span: Span * name: string * is_mut: bool
    | Object of elems: list<ObjPatElem>
    | Tuple of elems: list<Pattern>
    | Wildcard
    | Literal of span: Span * value: Literal
    | Is of span: Span * target: Pattern * id: string * is_mut: bool

  type Pattern =
    { kind: PatternKind
      span: Span
      inferred_type: option<Type.Type> }

  type Block = { span: Span; stmts: list<Stmt> }

  [<RequireQualifiedAccess>]
  type BlockOrExpr =
    | Block of Block
    | Expr of Expr

  type ObjElem =
    | Property of span: Span * key: string * value: Expr
    | Spread of span: Span * value: Expr

  type AssignOp =
    | Assign
    | AddAssign
    | SubAssign
    | MulAssign
    | DivAssign
    | ModAssign
    | ExpAssign

  type BinaryOp =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Exp
    | Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | And
    | Or

  type UnaryOp =
    | Pos
    | Neg
    | Not

  type MatchCase =
    { span: Span
      pattern: Pattern
      guard: option<Expr>
      body: Expr }

  type TemplateLiteral =
    { parts: list<string>
      exprs: list<Expr> }

  type ExprKind =
    | Identifer of string
    | Literal of Literal
    | Object of elems: list<ObjElem>
    | Tuple of elems: list<Expr>
    | Assign of left: Expr * op: AssignOp * right: Expr
    | Binary of left: Expr * op: BinaryOp * right: Expr
    | Unary of op: string * value: Expr
    | Function of param_list: list<string> * body: BlockOrExpr
    | Call of
      callee: Expr *
      type_args: option<list<TypeAnn>> *
      args: list<Expr> *
      opt_chain: bool *
      throws: option<Type.Type>
    | Index of target: Expr * index: Expr * opt_chain: bool
    | Member of target: Expr * name: string * opt_chain: bool
    | If of cond: Expr * then_branch: Expr * else_branch: Expr
    | Match of target: Expr * cases: list<MatchCase>
    | Try of body: Block * catch: option<Expr> * finally_: option<Expr>
    | Do of body: Block
    | Await of value: Expr
    | Throw of value: Expr
    | TemplateLiteral of TemplateLiteral
    | TaggedTemplateLiteral of
      tag: Expr *
      template: TemplateLiteral *
      throws: option<Type.Type>

  type Expr =
    { kind: ExprKind
      span: Span
      mutable inferred_type: option<Type.Type> }

  type ObjTypeAnnElem = int

  type KeywordType =
    | Boolean
    | Number
    | String
    | Symbol
    | Null
    | Undefined
    | Unknown
    | Never
    | Object

  type TypeParam =
    { span: Span
      name: string
      bound: option<Type.Type>
      default_: option<Type.Type> }

  type FunctionType =
    { type_params: option<list<TypeParam>>
      params_: list<TypeAnn>
      return_type: TypeAnn
      throws: option<Type.Type> }

  type ConditionType =
    { check: TypeAnn
      extends: TypeAnn
      true_type: TypeAnn
      false_type: TypeAnn }

  type MatchType =
    { target: TypeAnn
      cases: list<MatchTypeCase> }

  type MatchTypeCase =
    { extends: TypeAnn; true_type: TypeAnn }

  type TypeAnnKind =
    // TODO: collapse these into a single `Literal` type ann kind
    | BooleanLiteral of value: bool
    | NumberLiteral of value: string
    | StringLiteral of value: string
    | Keyword of keyword: KeywordType
    | Object of elems: list<ObjTypeAnnElem>
    | Tuple of elems: list<TypeAnn>
    | Array of elem: TypeAnn
    | Union of types: list<TypeAnn>
    | Intersection of types: list<TypeAnn>
    | TypeRef of name: string * type_args: option<list<TypeAnn>>
    | Function of FunctionType
    | Keyof of target: TypeAnn
    | Rest of target: TypeAnn
    | Typeof of target: Expr
    | Index of target: TypeAnn * index: TypeAnn
    | Condition of ConditionType
    | Match of MatchType
    | Infer of name: string
    | Wildcard
    | Binary of left: TypeAnn * op: BinaryOp * right: TypeAnn

  type TypeAnn =
    { kind: TypeAnnKind
      span: Span
      mutable inferred_type: option<Type.Type> }

module Type =
  type TypeParam =
    { name: string
      bound: option<Type>
      default_: option<Type> }

  type Scheme =
    { type_params: list<TypeParam>
      type_: Type
      is_type_param: bool }

  type Pattern =
    | Identifier of name: string
    | Object of elems: list<ObjPatElem>
    | Tuple of elems: list<Pattern>
    | Wildcard
    | Literal of Syntax.Literal
    | Is of target: Pattern * id: string
    | Rest of target: Pattern

  type ObjPatElem =
    | KeyValue of key: string * value: Pattern
    | Shorthand of name: string * value: option<Type>
    | Rest of target: Pattern

  type FuncParam =
    { pattern: Pattern
      type_: Type
      optional: bool }

  [<RequireQualifiedAccess>]
  type KeywordType =
    | Never
    | Object
    | Unknown

  type Primitive =
    | Boolean
    | Number
    | String
    | Symbol

  type Function =
    { param_list: list<FuncParam>
      return_type: Type
      type_params: option<list<TypeParam>>
      throws: Type }

  type Mapped =
    { key: Type
      value: Type
      target: string
      source: Type
      optional: option<MappedModifier>

      // First half of Conditional
      check: option<Type>
      extends: option<Type> }

  type MappedModifier =
    | Add
    | Remove

  type ObjKey = string // TODO

  type ObjTypeElem =
    | Call of Function
    | Constructor of Function
    | Method of name: ObjKey * is_mut: bool * type_: Function
    | Getter of name: ObjKey * return_type: Type * throws: Type
    | Setter of name: ObjKey * param: FuncParam * throws: Type
    | Mapped of Mapped
    | Property of name: ObjKey * optional: bool * readonly: bool * type_: Type

  [<RequireQualifiedAccess>]
  type TypeKind =
    | TypeVar of id: int * instance: option<Type> * bound: option<Type>
    | TypeRef of name: string * type_args: list<Type> * scheme: option<Scheme>
    | Literal of Syntax.Literal
    | Tuple of elems: list<Type>
    | Array of elem: Type
    | Union of types: list<Type>
    | Intersection of types: list<Type>
    | Keyword of keyword: KeywordType
    | Function of Function
    | Object of elems: list<ObjTypeElem>
    | Rest of target: Type
    | KeyOf of target: Type
    | Index of target: Type * index: Type
    | Condition of
      check: Type *
      extends: Type *
      true_type: Type *
      false_type: Type
    | Infer of name: string
    | Wildcard
    | Binary of left: Type * op: Syntax.BinaryOp * right: Type

  [<RequireQualifiedAccess>]
  type Provenance =
    | Type of Type
    | Expr of Syntax.Expr

  type Type =
    { kind: TypeKind
      provenance: option<Provenance> ref }
