namespace Escalier.Codegen

module rec TypeScript =

  type Position(line: int, column: int) =
    member this.Line = line
    member this.Column = column

  type SourceLocation(s: Position, e: Position, source: option<string>) =
    member this.Source = source
    member this.Start = s
    member this.End = e

  let loc: SourceLocation = SourceLocation(Position(0, 0), Position(0, 0), None)

  type Node =
    abstract member Loc: option<SourceLocation>

  type Ident =
    { Name: string
      Loc: option<SourceLocation> }

  type Regex = { Pattern: string; Flags: string }

  type LiteralValue =
    | String of string
    | Number of float
    | Boolean of bool
    | Regex of Regex
    | Null
    | Undefined

  type Literal =
    { Value: LiteralValue
      Loc: option<SourceLocation> }

  type Program(body: list<Stmt>, loc: option<SourceLocation>) =
    member this.Body = body

  // Statements
  [<RequireQualifiedAccess>]
  type Stmt =
    | Expression of ExpressionStatement
    | Block of BlockStatement
    | Empty of EmptyStatement
    | Debugger of DebuggerStatement
    | Return of ReturnStatement
    | Labeled of LabeledStatement
    | Break of BreakStatement
    | Continue of ContinueStatement
    | If of IfStatement
    | Switch of SwitchStatement
    | Throw of ThrowStatement
    | Try of TryStatement
    | While of WhileStatement
    | DoWhile of DoWhileStatement
    | For of ForStatement
    | ForIn of ForInStatement
    | Declaration of Declaration

  type ExpressionStatement =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type BlockStatement =
    { Body: list<Stmt>
      Loc: option<SourceLocation> }

  type EmptyStatement = { Loc: option<SourceLocation> }

  type DebuggerStatement = { Loc: option<SourceLocation> }

  type ReturnStatement =
    { Argument: option<Expr>
      Loc: option<SourceLocation> }

  type LabeledStatement =
    { Label: Ident
      Body: Stmt
      Loc: option<SourceLocation> }

  type BreakStatement =
    { Label: option<Ident>
      Loc: option<SourceLocation> }

  type ContinueStatement =
    { Label: option<Ident>
      Loc: option<SourceLocation> }

  type IfStatement =
    { Test: Expr
      Consequent: Stmt
      Alternate: option<Stmt>
      Loc: option<SourceLocation> }

  type SwitchStatement =
    { Discriminant: Expr
      Cases: list<SwitchCase>
      Loc: option<SourceLocation> }

  type SwitchCase =
    { Test: option<Expr>
      Consequent: list<Stmt>
      Loc: option<SourceLocation> }

  type ThrowStatement =
    { Argument: Expr
      Loc: option<SourceLocation> }

  type TryStatement =
    { Block: BlockStatement
      Handler: option<CatchClause>
      Finalizer: option<BlockStatement> }

  type CatchClause = { Param: Pat; Body: BlockStatement }

  type WhileStatement =
    { Test: Expr
      Body: Stmt
      Loc: option<SourceLocation> }

  type DoWhileStatement =
    { Body: Stmt
      Test: Expr
      Loc: option<SourceLocation> }

  type ForInit =
    | Variable of VariableDeclaration
    | Expression of Expr

  type ForStatement =
    { Init: option<ForInit>
      Test: option<Expr>
      Update: option<Expr>
      Body: Stmt }

  type ForInLeft =
    | Variable of VariableDeclaration
    | Pattern of Pat

  type ForInStatement =
    { Left: ForInLeft
      Right: Expr
      Body: Stmt }

  // Declarations

  type Declaration =
    | Function of FunctionDeclaration
    | Variable of VariableDeclaration

  // TODO: reuse with function expressions
  type FunctionDeclaration =
    { Id: Ident
      Params: list<Pat>
      Body: BlockStatement }

  type VariableDeclarationKind =
    | Var
    | Let
    | Const

  type VariableDeclaration =
    { Declarations: list<VariableDeclarator>
      Kind: VariableDeclarationKind }

  type VariableDeclarator = { Id: Pat; Init: option<Expr> }

  // Expressions
  [<RequireQualifiedAccess>]
  type Expr =
    | Literal of Literal
    | Identifier of Ident
    | This of ThisExpression
    | Array of ArrayExpression
    | Object of ObjectExpression
    | Function of FunctionExpression
    | ArrowFunction of ArrowFunctionExpression
    | Unary of UnaryExpression
    | Update of UpdateExpression
    | Binary of BinaryExpression
    | Assignment of AssignmentExpression
    | Logical of LogicalExpression
    | Member of MemberExpression
    | Conditional of ConditionalExpression
    | Call of CallExpression
    | New of NewExpression
    | Sequence of SequenceExpression

  type ThisExpression = { Loc: option<SourceLocation> }

  type ArrayExpression =
    { Elements: list<option<Expr>>
      Loc: option<SourceLocation> }

  type ObjectExpression =
    { Properties: list<Property>
      Loc: option<SourceLocation> }

  type PropertyKey =
    | Literal of Literal
    | Identifier of Ident

  type PropertyKind =
    | Init
    | Get
    | Set

  type Property =
    { Key: PropertyKey
      Value: Expr
      Kind: PropertyKind
      Loc: option<SourceLocation> }

  type FunctionExpression =
    { Id: option<Ident>
      Params: list<Pat>
      Body: BlockStatement }

  // TODO: also allow blocks bodies
  type ArrowFunctionExpression =
    { Params: list<Pat>
      Body: BlockStatement }

  type UnaryOperator =
    | Minus
    | Plus
    | Not
    | BitwiseNot
    | Typeof
    | Void
    | Delete
    | Await

  type UnaryExpression =
    { Operator: UnaryOperator
      Prefix: bool
      Argument: Expr
      Loc: option<SourceLocation> }

  type UpdateOperator =
    | Increment
    | Decrement

  type UpdateExpression =
    { Operator: UpdateOperator
      Argument: Expr
      Prefix: bool
      Loc: option<SourceLocation> }

  [<RequireQualifiedAccess>]
  type BinaryOperator =
    | Equal
    | NotEqual
    | StrictEqual
    | StrictNotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | LeftShift
    | RightShift
    | UnsignedRightShift
    | Plus
    | Minus
    | Multiply
    | Divide
    | Modulo
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor
    | In
    | InstanceOf

  type BinaryExpression =
    { Operator: BinaryOperator
      Left: Expr
      Right: Expr
      Loc: option<SourceLocation> }

  type AssignmentOperator =
    | Assign
    | PlusAssign
    | MinusAssign
    | MultiplyAssign
    | DivideAssign
    | ModuloAssign
    | LeftShiftAssign
    | RightShiftAssign
    | UnsignedRightShiftAssign
    | BitwiseAndAssign
    | BitwiseOrAssign
    | BitwiseXorAssign

  type AssignmentExpression =
    { Operator: AssignmentOperator
      Left: Expr
      Right: Expr
      Loc: option<SourceLocation> }

  type LogicalOperator =
    | And
    | Or

  type LogicalExpression =
    { Operator: LogicalOperator
      Left: Expr
      Right: Expr
      Loc: option<SourceLocation> }

  type MemberExpression =
    { Object: Expr
      Property: Expr
      Computed: bool
      Loc: option<SourceLocation> }

  type ConditionalExpression =
    { Test: Expr
      Alternate: Expr
      Consequent: Expr
      Loc: option<SourceLocation> }

  type CallExpression =
    { Callee: Expr
      Arguments: list<Expr>
      Loc: option<SourceLocation> }

  // TODO: combine with CallExpression
  type NewExpression =
    { Callee: Expr
      Arguments: list<Expr>
      Loc: option<SourceLocation> }

  type SequenceExpression =
    { Expressions: list<Expr>
      Loc: option<SourceLocation> }

  // Patterns
  [<RequireQualifiedAccess>]
  type Pat =
    | Ident of BindingIdent
    | Array of ArrayPat
    | Rest of RestPat
    | Object of ObjectPat
    | Assign of AssignPat
    | Invalid of Invalid
    | Expr of Expr // only valid in for-in/for-of loops

  type BindingIdent =
    { Id: Ident
      TypeAnn: option<TsTypeAnn>
      Loc: option<SourceLocation> }

  type ArrayPat =
    { Elems: list<option<Pat>>
      Optional: bool
      TypeAnn: option<TsTypeAnn>
      Loc: option<SourceLocation> }

  type RestPat =
    { Arg: Pat
      TypeAnn: option<TsTypeAnn>
      Loc: option<SourceLocation> }

  type ObjectPat =
    { Props: list<ObjectPatProp>
      Optional: bool
      TypeAnn: option<TsTypeAnn>
      Loc: option<SourceLocation> }

  type AssignPat =
    { Left: Pat
      Right: Expr
      Loc: option<SourceLocation> }

  type Invalid = { Loc: option<SourceLocation> }

  // Type Annotations
  // NOTE: we might not need this
  type TsTypeAnn =
    { TypeAnn: TsType
      Loc: option<SourceLocation> }

  type TsType =
    | TsKeywordType of TsKeywordType
    | TsThisType of TsThisType
    | TsFnOrConstructorType of TsFnOrConstructorType
    | TsTypeRef of TsTypeRef
    | TsTypeQuery of TsTypeQuery
    | TsTypeLit of TsTypeLit
    | TsArrayType of TsArrayType
    | TsTupleType of TsTupleType
    | TsOptionalType of TsOptionalType
    | TsRestType of TsRestType
    | TsUnionOrIntersectionType of TsUnionOrIntersectionType
    | TsConditionalType of TsConditionalType
    | TsInferType of TsInferType
    | TsParenthesizedType of TsParenthesizedType
    | TsTypeOperator of TsTypeOperator
    | TsIndexedAccessType of TsIndexedAccessType
    | TsMappedType of TsMappedType
    | TsLitType of TsLitType
    | TsTypePredicate of TsTypePredicate
    | TsImportType of TsImportType

  type TsKeywordType =
    { Kind: TsKeywordTypeKind
      Loc: option<SourceLocation> }

  type TsKeywordTypeKind =
    | TsAnyKeyword
    | TsUnknownKeyword
    | TsNumberKeyword
    | TsObjectKeyword
    | TsBooleanKeyword
    | TsBigIntKeyword
    | TsStringKeyword
    | TsSymbolKeyword
    | TsVoidKeyword
    | TsUndefinedKeyword
    | TsNullKeyword
    | TsNeverKeyword
    | TsIntrinsicKeyword

  type TsThisType = { Loc: option<SourceLocation> }

  type TsFnOrConstructorType =
    | TsFnType of TsFnType
    | TsConstructorType of TsConstructorType

  type TsFnType =
    { Params: list<TsFnParam>
      TypeParams: option<TsTypeParamDecl>
      TypeAnn: TsTypeAnn
      Loc: option<SourceLocation> }

  type TsConstructorType =
    { Params: list<TsFnParam>
      TypeParams: Option<TsTypeParamDecl>
      TypeAnn: TsTypeAnn
      IsAbstract: bool
      Loc: option<SourceLocation> }

  type TsTypeParamDecl =
    { Params: list<TsTypeParam>
      Loc: option<SourceLocation> }

  type TsTypeParam =
    { name: Ident
      IsIn: bool
      IsOut: bool
      IsConst: bool
      Constraint: option<TsType>
      Default: option<TsType>
      Loc: option<SourceLocation> }

  type TsFnParam =
    | Ident of BindingIdent
    | Array of ArrayPat
    | Rest of RestPat
    | Object of ObjectPat

  type ObjectPatProp =
    | KeyValue of KeyValuePatProp
    | Assign of AssignPatProp
    | Rest of RestPat

  type KeyValuePatProp =
    { Key: PropName
      Value: Pat
      Loc: option<SourceLocation> }

  type PropName =
    | Ident of Ident
    | Str of Str
    | Num of Number
    | Computed of ComputedPropName
  // TODO: BigInt of BigInt

  type ComputedPropName =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type AssignPatProp =
    { Key: Ident
      Value: option<Expr>
      Loc: option<SourceLocation> }

  type TsTypeRef =
    { Loc: option<SourceLocation>
      typeName: TsEntityName
      type_params: option<TsTypeParamInstantiation> }

  type TsQualifiedName = { Left: TsEntityName; Right: Ident }

  type TsEntityName =
    | TsQualifiedName of TsQualifiedName
    | Identifier of Ident

  type TsTypeParamInstantiation =
    { Params: list<TsType>
      Loc: option<SourceLocation> }

  type TsTypeQuery = { Loc: option<SourceLocation> }

  type TsTypeLit =
    { Members: list<TsTypeElement>
      Loc: option<SourceLocation> }

  type TsTypeElement =
    | TsCallSignatureDecl of TsCallSignatureDecl
    | TsConstructSignatureDecl of TsConstructSignatureDecl
    | TsPropertySignature of TsPropertySignature
    | TsGetterSignature of TsGetterSignature
    | TsSetterSignature of TsSetterSignature
    | TsMethodSignature of TsMethodSignature
    | TsIndexSignature of TsIndexSignature

  type TsCallSignatureDecl =
    { Params: list<TsFnParam>
      TypeAnn: option<TsTypeAnn>
      TypeParams: option<TsTypeParamDecl>
      Loc: option<SourceLocation> }

  type TsConstructSignatureDecl =
    { Params: list<TsFnParam>
      TypeAnn: option<TsTypeAnn>
      TypeParams: option<TsTypeParamDecl>
      Loc: option<SourceLocation> }

  type TsPropertySignature =
    { Readonly: bool
      Key: Expr
      Computed: bool
      Optional: bool
      Init: Option<Expr>
      Params: list<TsFnParam>
      TypeAnn: option<TsTypeAnn>
      TypeParams: option<TsTypeParamDecl>
      Loc: option<SourceLocation> }

  type TsGetterSignature =
    { Readonly: bool
      Key: Expr
      Computed: bool
      Optional: bool
      TypeAnn: option<TsTypeAnn>
      Loc: option<SourceLocation> }

  type TsSetterSignature =
    { Readonly: bool
      Key: Expr
      Computed: bool
      Optional: bool
      Param: TsFnParam
      Loc: option<SourceLocation> }

  type TsMethodSignature =
    { Readonly: bool
      Key: Expr
      Computed: bool
      Optional: bool
      Params: list<TsFnParam>
      TypeAnn: option<TsTypeAnn>
      TypeParams: option<TsTypeParamDecl>
      Loc: option<SourceLocation> }

  type TsIndexSignature =
    { Params: list<TsFnParam>
      TypeAnn: option<TsTypeAnn>
      Readonly: bool
      IsStatic: bool
      Loc: option<SourceLocation> }

  type TsArrayType =
    { ElemType: TsType
      Loc: option<SourceLocation> }

  type TsTupleType =
    { ElemTypes: list<TsTupleElement>
      Loc: option<SourceLocation> }

  type TsTupleElement =
    { label: option<Pat>
      Type: TsType
      Loc: option<SourceLocation> }

  type TsOptionalType =
    { TypeAnn: TsType
      Loc: option<SourceLocation> }

  type TsRestType =
    { TypeAnn: TsType
      Loc: option<SourceLocation> }

  type TsUnionOrIntersectionType =
    | TsUnionType of TsUnionType
    | TsIntersectionType of TsIntersectionType

  type TsUnionType =
    { Types: list<TsType>
      Loc: option<SourceLocation> }

  type TsIntersectionType =
    { Types: list<TsType>
      Loc: option<SourceLocation> }

  type TsConditionalType =
    { CheckType: TsType
      ExtendsType: TsType
      TrueType: TsType
      FalseType: TsType
      Loc: option<SourceLocation> }

  type TsInferType =
    { TypeParam: TsTypeParam
      Loc: option<SourceLocation> }

  type TsParenthesizedType =
    { TypeAnn: TsType
      Loc: option<SourceLocation> }

  type TsTypeOperator =
    { Op: TsTypeOperatorOp
      TypeAnn: TsType
      Loc: option<SourceLocation> }

  type TsTypeOperatorOp =
    | KeyOf
    | Unique
    | ReadOnly

  type TsIndexedAccessType =
    { Readonly: bool
      ObjType: TsType
      IndexType: TsType
      Loc: option<SourceLocation> }

  type TsMappedType =
    { Readonly: option<TruePlusMinus>
      TypeParam: TsTypeParam
      NameType: option<TsType>
      Optional: option<TruePlusMinus>
      Typeann: option<TsType>
      Loc: option<SourceLocation> }

  type TruePlusMinus =
    | True
    | Plus
    | Minus

  type TsLitType =
    { Lit: TsLit
      Loc: option<SourceLocation> }

  type TsLit =
    | Number of Number
    | Str of Str
    | Bool of Bool
    // TODO: BigInt of BigInt
    | Tpl of TsTplLitType

  type TsTplLitType =
    { Types: list<TsType>
      Quasis: list<TplElement>
      Loc: option<SourceLocation> }

  type TplElement =
    { Tail: bool
      Cooked: option<string>
      Raw: string
      Loc: option<SourceLocation> }

  type TsTypePredicate =
    { Asserts: bool
      ParamName: TsThisTypeOrIdent
      Typeann: option<TsTypeAnn>
      Loc: option<SourceLocation> }

  type TsThisTypeOrIdent =
    | TsThisType of TsThisType
    | Ident of Ident

  type TsImportType =
    { arg: Str
      qualifier: Option<TsEntityName>
      type_args: Option<TsTypeParamInstantiation>
      Loc: option<SourceLocation> }

  // Literals
  type Str =
    { Value: string
      Raw: Option<string>
      Loc: option<SourceLocation> }

  type Number =
    { Value: float
      Raw: Option<string>
      Loc: option<SourceLocation> }

  type Bool =
    { Value: bool
      Loc: option<SourceLocation> }
