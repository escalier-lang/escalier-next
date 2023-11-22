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

  type Identifier =
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

  type Program(body: list<Statement>, loc: option<SourceLocation>) =
    member this.Body = body

  // Statements

  [<RequireQualifiedAccess>]
  type Statement =
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
    { Expr: Expression
      Loc: option<SourceLocation> }

  type BlockStatement =
    { Body: list<Statement>
      Loc: option<SourceLocation> }

  type EmptyStatement = { Loc: option<SourceLocation> }

  type DebuggerStatement = { Loc: option<SourceLocation> }

  type ReturnStatement =
    { Argument: option<Expression>
      Loc: option<SourceLocation> }

  type LabeledStatement =
    { Label: Identifier
      Body: Statement
      Loc: option<SourceLocation> }

  type BreakStatement =
    { Label: option<Identifier>
      Loc: option<SourceLocation> }

  type ContinueStatement =
    { Label: option<Identifier>
      Loc: option<SourceLocation> }

  type IfStatement =
    { Test: Expression
      Consequent: Statement
      Alternate: option<Statement>
      Loc: option<SourceLocation> }

  type SwitchStatement =
    { Discriminant: Expression
      Cases: list<SwitchCase>
      Loc: option<SourceLocation> }

  type SwitchCase =
    { Test: option<Expression>
      Consequent: list<Statement>
      Loc: option<SourceLocation> }

  type ThrowStatement =
    { Argument: Expression
      Loc: option<SourceLocation> }

  type TryStatement =
    { Block: BlockStatement
      Handler: option<CatchClause>
      Finalizer: option<BlockStatement> }

  type CatchClause =
    { Param: Pattern; Body: BlockStatement }

  type WhileStatement =
    { Test: Expression
      Body: Statement
      Loc: option<SourceLocation> }

  type DoWhileStatement =
    { Body: Statement
      Test: Expression
      Loc: option<SourceLocation> }

  type ForInit =
    | Variable of VariableDeclaration
    | Expression of Expression

  type ForStatement =
    { Init: option<ForInit>
      Test: option<Expression>
      Update: option<Expression>
      Body: Statement }

  type ForInLeft =
    | Variable of VariableDeclaration
    | Pattern of Pattern

  type ForInStatement =
    { Left: ForInLeft
      Right: Expression
      Body: Statement }

  // Declarations

  type Declaration =
    | Function of FunctionDeclaration
    | Variable of VariableDeclaration

  // TODO: reuse with function expressions
  type FunctionDeclaration =
    { Id: Identifier
      Params: list<Pattern>
      Body: BlockStatement }

  type VariableDeclarationKind =
    | Var
    | Let
    | Const

  type VariableDeclaration =
    { Declarations: list<VariableDeclarator>
      Kind: VariableDeclarationKind }

  type VariableDeclarator =
    { Id: Pattern
      Init: option<Expression> }

  // Expressions
  [<RequireQualifiedAccess>]
  type Expression =
    | Literal of Literal
    | Identifier of Identifier
    | This of ThisExpression
    | Array of ArrayExpression
    | Object of ObjectExpression
    | Function of FunctionExpression
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
    { Elements: list<option<Expression>>
      Loc: option<SourceLocation> }

  type ObjectExpression =
    { Properties: list<Property>
      Loc: option<SourceLocation> }

  type PropertyKey =
    | Literal of Literal
    | Identifier of Identifier

  type PropertyKind =
    | Init
    | Get
    | Set

  type Property =
    { Key: PropertyKey
      Value: Expression
      Kind: PropertyKind
      Loc: option<SourceLocation> }

  type FunctionExpression =
    { Id: option<Identifier>
      Params: list<Pattern>
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
      Argument: Expression
      Loc: option<SourceLocation> }

  type UpdateOperator =
    | Increment
    | Decrement

  type UpdateExpression =
    { Operator: UpdateOperator
      Argument: Expression
      Prefix: bool
      Loc: option<SourceLocation> }

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
      Left: Expression
      Right: Expression
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
      Left: Expression
      Right: Expression
      Loc: option<SourceLocation> }

  type LogicalOperator =
    | And
    | Or

  type LogicalExpression =
    { Operator: LogicalOperator
      Left: Expression
      Right: Expression
      Loc: option<SourceLocation> }

  type MemberExpression =
    { Object: Expression
      Property: Expression
      Computed: bool
      Loc: option<SourceLocation> }

  type ConditionalExpression =
    { Test: Expression
      Alternate: Expression
      Consequent: Expression }

  type CallExpression =
    { Callee: Expression
      Arguments: list<Expression> }

  // TODO: combine with CallExpression
  type NewExpression =
    { Callee: Expression
      Arguments: list<Expression> }

  type SequenceExpression = { Expressions: list<Expression> }

  // Patterns
  // TODO: finish this off
  [<RequireQualifiedAccess>]
  type Pattern =
    | Identifier of Identifier
    | Member of MemberExpression
