namespace Escalier.Interop

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

  type Function =
    { Params: list<Param>
      // TODO: Decorators
      // Decorators: list<Decorator>
      Body: option<BlockStmt>
      IsGenerator: bool
      IsAsync: bool
      TypeParams: option<TsTypeParamDecl>
      ReturnType: option<TsTypeAnn>
      Loc: option<SourceLocation> }

  type Program =
    | Module of Module
    | Script of Script

  type Module =
    { Body: list<ModuleItem>
      Shebang: Option<string>
      Loc: option<SourceLocation> }

  type Script =
    { Body: list<Stmt>
      Shebang: Option<string>
      Loc: option<SourceLocation> }

  // Literals
  [<RequireQualifiedAccess>]
  type Lit =
    | Str of Str
    | Bool of Bool
    | Num of Number
    | Null of Null
    // | BigInt of BigInt
    | Regex of Regex
    | JSXText of JSXText

  type Str =
    { Value: string
      Raw: Option<string>
      Loc: option<SourceLocation> }

  type Bool =
    { Value: bool
      Loc: option<SourceLocation> }

  type Null = { Loc: option<SourceLocation> }

  type Number =
    { Value: float
      Raw: option<string>
      Loc: option<SourceLocation> }

  type Regex =
    { Exp: string
      Flags: string
      Loc: option<SourceLocation> }

  type JSXText =
    { Value: string
      Raw: string
      Loc: option<SourceLocation> }

  // Statements
  // TODO: add missing statements from swc_ecma_ast
  [<RequireQualifiedAccess>]
  type Stmt =
    | Block of BlockStmt
    | Empty of EmptyStmt
    | Debugger of DebuggerStmt
    // | With of WithStmt
    | Return of ReturnStmt
    | Labeled of LabeledStmt
    | Break of BreakStmt
    | Continue of ContinueStmt
    | If of IfStmt
    | Switch of SwitchStmt
    | Throw of ThrowStmt
    | Try of TryStmt
    | While of WhileStmt
    | DoWhile of DoWhileStmt
    | For of ForStmt
    | ForIn of ForInStmt
    | ForOf of ForOfStmt
    | Decl of Decl
    | Expr of ExprStmt

  type ExprStmt =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type BlockStmt =
    { Body: list<Stmt>
      Loc: option<SourceLocation> }

  type EmptyStmt = { Loc: option<SourceLocation> }

  type DebuggerStmt = { Loc: option<SourceLocation> }

  type ReturnStmt =
    { Argument: option<Expr>
      Loc: option<SourceLocation> }

  type LabeledStmt =
    { Label: Ident
      Body: Stmt
      Loc: option<SourceLocation> }

  type BreakStmt =
    { Label: option<Ident>
      Loc: option<SourceLocation> }

  type ContinueStmt =
    { Label: option<Ident>
      Loc: option<SourceLocation> }

  type IfStmt =
    { Test: Expr
      Consequent: Stmt
      Alternate: option<Stmt>
      Loc: option<SourceLocation> }

  type SwitchStmt =
    { Discriminant: Expr
      Cases: list<SwitchCase>
      Loc: option<SourceLocation> }

  type SwitchCase =
    { Test: option<Expr>
      Consequent: list<Stmt>
      Loc: option<SourceLocation> }

  type ThrowStmt =
    { Argument: Expr
      Loc: option<SourceLocation> }

  type TryStmt =
    { Block: BlockStmt
      Handler: option<CatchClause>
      Finalizer: option<BlockStmt> }

  type CatchClause = { Param: Pat; Body: BlockStmt }

  type WhileStmt =
    { Test: Expr
      Body: Stmt
      Loc: option<SourceLocation> }

  type DoWhileStmt =
    { Body: Stmt
      Test: Expr
      Loc: option<SourceLocation> }

  type ForInit =
    | Variable of VarDecl
    | Expr of Expr

  type ForStmt =
    { Init: option<ForInit>
      Test: option<Expr>
      Update: option<Expr>
      Body: Stmt
      Loc: option<SourceLocation> }

  type ForInLeft =
    | Variable of VarDecl
    | Pattern of Pat

  type ForInStmt =
    { Left: ForHead
      Right: Expr
      Body: Stmt
      Loc: option<SourceLocation> }

  type ForOfStmt =
    { IsAwait: bool
      Left: ForHead
      Right: Expr
      Body: Stmt
      Loc: option<SourceLocation> }

  type ForHead =
    | VarDecl of VarDecl
    | UsingDecl of UsingDecl
    | Pat of Pat

  // Declarations
  [<RequireQualifiedAccess>]
  type Decl =
    | Class of ClassDecl
    | Fn of FnDecl
    | Var of VarDecl
    | Using of UsingDecl
    | TsInterface of TsInterfaceDecl
    | TsTypeAlias of TsTypeAliasDecl
    | TsEnum of TsEnumDecl
    | TsModule of TsModuleDecl

  type ClassDecl =
    { Ident: Ident
      Declare: bool
      Class: Class }

  type FnDecl = { Id: Ident; Fn: Function }

  type VariableDeclarationKind =
    | Var
    | Let
    | Const

  type VarDecl =
    { Decls: list<VarDeclarator>
      Declare: bool
      Kind: VariableDeclarationKind }

  type VarDeclarator = { Id: Pat; Init: option<Expr> }

  type UsingDecl =
    { IsAwait: bool
      Decls: list<VarDeclarator>
      Loc: option<SourceLocation> }

  type TsInterfaceDecl =
    { id: Ident
      declare: bool
      type_params: Option<TsTypeParamDecl>
      extends: list<TsExprWithTypeArgs>
      body: TsInterfaceBody
      Loc: option<SourceLocation> }

  type TsInterfaceBody =
    { Body: list<TsTypeElement>
      Loc: option<SourceLocation> }

  type TsTypeAliasDecl =
    { Declare: bool
      Id: Ident
      TypeParams: option<TsTypeParamDecl>
      TypeAnn: TsType
      Loc: option<SourceLocation> }

  type TsEnumDecl =
    { Declare: bool
      IsConst: bool
      Id: Ident
      Members: list<TsEnumMember>
      Loc: option<SourceLocation> }

  type TsEnumMember =
    { Id: TsEnumMemberId
      Init: option<Expr>
      Loc: option<SourceLocation> }

  type TsEnumMemberId =
    | Ident of Ident
    | Str of Str

  type TsModuleDecl =
    { Declare: bool
      Global: bool
      Id: TsModuleName
      Body: Option<TsNamespaceBody>
      Loc: option<SourceLocation> }

  type TsModuleName =
    | Ident of Ident
    | Str of Str

  type TsNamespaceBody =
    | TsModuleBlock of TsModuleBlock
    | TsNamespaceDecl of TsNamespaceDecl

  type TsModuleBlock =
    { body: list<ModuleItem>
      Loc: option<SourceLocation> }

  type TsNamespaceDecl =
    { Declare: bool
      Global: bool
      Id: Ident
      Body: TsNamespaceBody }

  type ModuleItem =
    | ModuleDecl of ModuleDecl
    | Stmt of Stmt

  [<RequireQualifiedAccess>]
  type ModuleDecl =
    | Import of ImportDecl
    | ExportDecl of ExportDecl
    | ExportNamed of NamedExport
    | ExportDefaultDecl of ExportDefaultDecl
    | ExportDefaultExpr of ExportDefaultExpr
    | ExportAll of ExportAll
    | TsImportEquals of TsImportEqualsDecl
    | TsExportAssignment of TsExportAssignment
    | TsNamespaceExport of TsNamespaceExportDecl

  type ImportDecl =
    { Specifiers: list<ImportSpecifier>
      Src: Str
      IsTypeOnly: bool
      With: Option<ObjectLit>
      Loc: option<SourceLocation> }

  type ImportSpecifier =
    | Named of ImportNamedSpecifier
    | Default of ImportDefaultSpecifier
    | Namespace of ImportStarAsSpecifier

  type ImportNamedSpecifier =
    { Local: Ident
      Imported: Option<ModuleExportName>
      IsTypeOnly: bool
      Loc: option<SourceLocation> }

  type ModuleExportName =
    | Ident of Ident
    | Str of Str

  type ImportDefaultSpecifier =
    { Local: Ident
      Loc: option<SourceLocation> }

  type ImportStarAsSpecifier =
    { Local: Ident
      Loc: option<SourceLocation> }

  type ExportDecl =
    { Decl: Decl
      Loc: option<SourceLocation> }

  type NamedExport =
    { Specifiers: list<ExportSpecifier>
      Src: option<Str>
      IsTypeOnly: bool
      With: option<ObjectLit>
      Loc: option<SourceLocation> }

  type ExportSpecifier =
    | Namespace of ExportNamespaceSpecifier
    | Default of ExportDefaultSpecifier
    | Named of ExportNamedSpecifier

  type ExportNamespaceSpecifier =
    { Name: ModuleExportName
      Loc: option<SourceLocation> }

  type ExportDefaultSpecifier = { Exported: Ident }

  type ExportNamedSpecifier =
    { Orig: ModuleExportName
      Exported: option<ModuleExportName>
      IsTypeOnly: bool
      Loc: option<SourceLocation> }

  type ExportDefaultDecl =
    { Decl: DefaultDecl
      Loc: option<SourceLocation> }

  type DefaultDecl =
    | Class of ClassExpr
    | Fn of FnExpr
    | TsInterfaceDecl of TsInterfaceDecl

  type ExportDefaultExpr =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type ExportAll =
    { Src: Str
      IsTypeOnly: bool
      With: Option<ObjectLit>
      Loc: option<SourceLocation> }

  type TsImportEqualsDecl =
    { IsExport: bool
      IsTypeOnly: bool
      Id: Ident
      ModuleRef: TsModuleRef
      Loc: option<SourceLocation> }

  type TsModuleRef =
    | TsEntityName of TsEntityName
    | TsExternalModuleRef of TsExternalModuleRef

  type TsExternalModuleRef =
    { Expr: Str
      Loc: option<SourceLocation> }

  type TsExportAssignment =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type TsNamespaceExportDecl =
    { Id: Ident
      Loc: option<SourceLocation> }


  // Exprs
  [<RequireQualifiedAccess>]
  type Expr =
    | This of ThisExpr
    | Array of ArrayLit
    | Object of ObjectLit
    | Fn of FnExpr
    | Unary of UnaryExpr
    | Update of UpdateExpr
    | Bin of BinExpr
    | Assign of AssignExpr
    | Member of MemberExpr
    | SuperProp of SuperPropExpr
    | Cond of CondExpr
    | Call of CallExpr
    | New of NewExpr
    | Seq of SeqExpr
    | Ident of Ident
    | Lit of Lit
    | Tpl of Tpl
    | TaggedTpl of TaggedTpl
    | Arrow of ArrowExpr
    | Class of ClassExpr
    | Yield of YieldExpr
    | MetaProp of MetaPropExpr
    | Await of AwaitExpr
    | Paren of ParenExpr
    | JSXMember of JSXMemberExpr
    | JSXNamespacedName of JSXNamespacedName
    | JSXEmpty of JSXEmptyExpr
    | JSXElement of JSXElement
    | JSXFragment of JSXFragment
    | TsTypeAssertion of TsTypeAssertion
    | TsConstAssertion of TsConstAssertion
    | TsNonNull of TsNonNullExpr
    | TsAs of TsAsExpr
    | TsInstantiation of TsInstantiation
    | TsSatisfies of TsSatisfiesExpr
    | PrivateName of PrivateName
    | OptChain of OptChainExpr
    | Invalid of Invalid

  type ThisExpr = { Loc: option<SourceLocation> }

  type ArrayLit =
    { Elements: list<option<ExprOrSpread>>
      Loc: option<SourceLocation> }

  type ExprOrSpread =
    { Spread: bool // was a span
      Expr: Expr }

  type ObjectLit =
    { Properties: list<Property>
      Loc: option<SourceLocation> }

  type PropertyKey =
    | Lit of Lit
    | Ident of Ident

  type PropertyKind =
    | Init
    | Get
    | Set

  type Property =
    { Key: PropertyKey
      Value: Expr
      Kind: PropertyKind
      Loc: option<SourceLocation> }

  type FnExpr = { Id: option<Ident>; Fn: Function }

  // TODO: also allow blocks bodies
  type ArrowExpr = { Params: list<Pat>; Body: BlockStmt }

  type UnaryOperator =
    | Minus
    | Plus
    | Not
    | BitwiseNot
    | Typeof
    | Void
    | Delete
    | Await

  type UnaryExpr =
    { Operator: UnaryOperator
      Prefix: bool
      Argument: Expr
      Loc: option<SourceLocation> }

  type UpdateOperator =
    | Increment
    | Decrement

  type UpdateExpr =
    { Operator: UpdateOperator
      Argument: Expr
      Prefix: bool
      Loc: option<SourceLocation> }

  [<RequireQualifiedAccess>]
  type BinOp =
    | EqEq
    | NotEq
    | EqEqEq
    | NotEqEq
    | Lt
    | LtEq
    | Gt
    | GtEq
    | LShift
    | RShift
    | ZeroFillRShift
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | BitOr
    | BitXor
    | BitAnd
    | LogicalOr
    | LogicalAnd
    | In
    | InstanceOf
    | Exp
    | NullishCoalescing

  type BinExpr =
    { Operator: BinOp
      Left: Expr
      Right: Expr
      Loc: option<SourceLocation> }

  type AssignOp =
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

  type AssignExpr =
    { Operator: AssignOp
      Left: Expr
      Right: Expr
      Loc: option<SourceLocation> }

  type LogicalOperator =
    | And
    | Or

  type LogicalExpr =
    { Operator: LogicalOperator
      Left: Expr
      Right: Expr
      Loc: option<SourceLocation> }

  type MemberExpr =
    { Object: Expr
      Property: Expr
      Computed: bool
      Loc: option<SourceLocation> }

  // TODO: SuperPropExpr
  type SuperPropExpr = { Loc: option<SourceLocation> }

  type CondExpr =
    { Test: Expr
      Alternate: Expr
      Consequent: Expr
      Loc: option<SourceLocation> }

  type CallExpr =
    { Callee: Expr
      Arguments: list<Expr>
      Loc: option<SourceLocation> }

  // TODO: combine with CallExpr
  type NewExpr =
    { Callee: Expr
      Arguments: list<Expr>
      Loc: option<SourceLocation> }

  type SeqExpr =
    { Exprs: list<Expr>
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

  type Tpl =
    { Exprs: list<Expr>
      Quasis: list<TplElement>
      Loc: option<SourceLocation> }

  type TplElement =
    { Tail: bool
      Cooked: option<string>
      Raw: string
      Loc: option<SourceLocation> }

  type TaggedTpl =
    { Tag: Expr
      TypeParams: option<TsTypeParamInstantiation>
      Tpl: Tpl
      Loc: option<SourceLocation> }

  type ClassExpr =
    { Id: option<Ident>
      Class: Class
      Loc: option<SourceLocation> }

  type Class =
    {
      // TODO: Decorators
      // Decorators: list<Decorator>
      Body: list<ClassMember>
      SuperClass: Option<Expr>
      IsAbstract: bool
      TypeParams: Option<TsTypeParamDecl>
      SuperTypeParams: Option<TsTypeParamInstantiation>
      Implements: list<TsExprWithTypeArgs>
      Loc: option<SourceLocation> }

  type TsExprWithTypeArgs =
    { Expr: Expr
      TypeArgs: Option<TsTypeParamInstantiation> }

  type ClassMember =
    | Constructor of Constructor
    | Method of ClassMethod
    | PrivateMethod of PrivateMethod
    | ClassProp of ClassProp
    | PrivateProp of PrivateProp
    | TsIndexSignature of TsIndexSignature
    | Empty of EmptyStmt
    | StaticBlock of StaticBlock
    | AutoAccessor of AutoAccessor

  type Constructor =
    { Key: PropName
      Params: list<ParamOrTsParamProp>
      Body: Option<BlockStmt>
      Accessibility: Option<Accessibility>
      IsOptional: bool
      Loc: option<SourceLocation> }

  type ParamOrTsParamProp =
    | Param of Param
    | TsParamProp of TsParamProp

  type Param =
    {
      // TODO: Decorators
      // decorators: list<Decorator>
      Pat: Pat
      Loc: option<SourceLocation> }

  type TsParamProp =
    {
      // TODO: Decorators
      // decorators: list<Decorator>
      Accessibility: Option<Accessibility>
      IsOverride: bool
      Readonly: bool
      Param: TsParamPropParam
      Loc: option<SourceLocation> }

  type Accessibility =
    | Public
    | Protected
    | Private

  type TsParamPropParam =
    | Ident of BindingIdent
    | Assign of AssignPat

  type ClassMethod =
    { Key: PropName
      Function: Function
      Kind: MethodKind
      IsStatic: bool
      Accessibility: option<Accessibility>
      IsAbstract: bool
      IsOptional: bool
      IsOverride: bool
      Loc: option<SourceLocation> }

  type MethodKind =
    | Method
    | Getter
    | Setter

  type PrivateMethod =
    { Key: PrivateName
      Function: Function
      Kind: MethodKind
      IsStatic: bool
      Accessibility: option<Accessibility>
      IsAbstract: bool
      IsOptional: bool
      IsOverride: bool
      Loc: option<SourceLocation> }

  type PrivateName =
    { Id: Ident
      Loc: option<SourceLocation> }

  type ClassProp =
    { Key: PropName
      Value: option<Expr>
      TypeAnn: option<TsTypeAnn>
      IsStatic: bool
      // TODO: Decorators
      // decorators: list<Decorator>
      Accessibility: option<Accessibility>
      IsAbstract: bool
      IsOptional: bool
      IsOverride: bool
      Readonly: bool
      Declare: bool
      Definite: bool
      Loc: option<SourceLocation> }

  type PrivateProp =
    { Key: PrivateName
      Value: option<Expr>
      TypeAnn: option<TsTypeAnn>
      IsStatic: bool
      // TODO: Decorators
      // decorators: list<Decorator>
      Accessibility: option<Accessibility>
      IsOptional: bool
      IsOverride: bool
      Readonly: bool
      Definite: bool
      Loc: option<SourceLocation> }

  type StaticBlock =
    { Body: BlockStmt
      Loc: option<SourceLocation> }

  type AutoAccessor =
    { Key: Key
      Value: option<Expr>
      TypeAnn: option<TsTypeAnn>
      IsStatic: bool
      // TODO: Decorators
      // decorators: list<Decorator>
      Accessibility: option<Accessibility>
      Loc: option<SourceLocation> }

  type Key =
    | Private of PrivateName
    | Public of PropName

  type YieldExpr =
    { Arg: option<Expr>
      Delegate: bool
      Loc: option<SourceLocation> }

  type MetaPropExpr =
    { Kind: MetaPropKind
      Loc: option<SourceLocation> }

  type MetaPropKind =
    | NewTarget
    | ImportMeta

  type AwaitExpr =
    { Arg: Expr
      Loc: option<SourceLocation> }

  // NOTE: we don't actually use this
  type ParenExpr =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type JSXMemberExpr = { Obj: JSXObject; Prop: Ident }

  type JSXObject =
    | JSXMemberExpr of JSXMemberExpr
    | Ident of Ident

  type JSXNamespacedName = { NS: Ident; Name: Ident }

  type JSXEmptyExpr = { Loc: option<SourceLocation> }

  type JSXElement =
    { OpeningElement: JSXOpeningElement
      Children: list<JSXElementChild>
      ClosingElement: option<JSXClosingElement>
      Loc: option<SourceLocation> }

  type JSXOpeningElement =
    { Name: JSXElementName
      Attrs: list<JSXAttrOrSpread>
      SelfClosing: bool
      TypeArgs: Option<TsTypeParamInstantiation>
      Loc: option<SourceLocation> }

  type JSXAttrOrSpread =
    | JSXAttr of JSXAttr
    | SpreadElement of SpreadElement

  type SpreadElement =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type JSXAttr =
    { Name: JSXAttrName
      Value: option<JSXAttrValue>
      Loc: option<SourceLocation> }

  type JSXAttrName =
    | JSXIdent of Ident
    | JSXNamespacedName of JSXNamespacedName

  type JSXAttrValue =
    | Lit of Lit
    | JSXExprContainer of JSXExprContainer
    | JSXElement of JSXElement
    | JSXFragment of JSXFragment

  type JSXExprContainer =
    { Expr: JSXExpr
      Loc: option<SourceLocation> }

  type JSXExpr =
    | JSXEmptyExpr of JSXEmptyExpr
    | Expr of Expr

  type JSXElementName =
    | Ident of Ident
    | JSXMemberExpr of JSXMemberExpr
    | JSXNamespacedName of JSXNamespacedName

  type JSXElementChild =
    | JSXText of JSXText
    | JSXExprContainer of JSXExprContainer
    | JSXSpreadChild of JSXSpreadChild
    | JSXElement of JSXElement
    | JSXFragment of JSXFragment

  type JSXSpreadChild =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type JSXClosingElement =
    { Name: JSXElementName
      Loc: option<SourceLocation> }

  type JSXFragment =
    { Opening: JSXOpeningFragment
      Children: list<JSXElementChild>
      Closing: JSXClosingFragment
      Loc: option<SourceLocation> }

  type JSXOpeningFragment = { Loc: option<SourceLocation> }
  type JSXClosingFragment = { Loc: option<SourceLocation> }

  type TsTypeAssertion =
    { Expr: Expr
      TypeAnn: TsTypeAnn
      Loc: option<SourceLocation> }

  type TsConstAssertion =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type TsNonNullExpr =
    { Expr: Expr
      Loc: option<SourceLocation> }

  type TsAsExpr =
    { Expr: Expr
      TypeAnn: TsTypeAnn
      Loc: option<SourceLocation> }

  type TsInstantiation =
    { Expr: Expr
      TypeArgs: TsTypeParamInstantiation
      Loc: option<SourceLocation> }

  type TsSatisfiesExpr =
    { Expr: Expr
      TypeAnn: TsType
      Loc: option<SourceLocation> }

  type OptChainExpr =
    { Optional: bool
      Base: OptChainBase
      Loc: option<SourceLocation> }

  type OptChainBase =
    | Member of MemberExpr
    | Call of OptCall

  type OptCall =
    { Callee: Expr
      Args: list<ExprOrSpread>
      TypeArgs: Option<TsTypeParamInstantiation>
      Loc: option<SourceLocation> }

  type Invalid = { Loc: option<SourceLocation> }

  // Type Annotations
  type TsTypeAnn =
    { TypeAnn: TsType
      Loc: option<SourceLocation> }

  [<RequireQualifiedAccess>]
  type TsType =
    | TsKeywordType of TsKeywordType
    | TsThisType of TsThisType
    | TsFnOrConstructorType of TsFnOrConstructorType
    | TsTypeRef of TsTypeRef
    | TsTypeQuery of TsTypeQuery
    | TsTypeLit of TsTypeLit // TODO: rename this to TsObjType
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
    { Name: Ident
      IsIn: bool
      IsOut: bool
      IsConst: bool
      Constraint: option<TsType>
      Default: option<TsType>
      Loc: option<SourceLocation> }

  // TODO: Make BindingIdent, ArrayPat, RestPat, and ObjectPat
  // parametric so that we can make `.TypeAnn` required
  [<RequireQualifiedAccess>]
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
      TypeName: TsEntityName
      // TODO: consider renaming to TypeArgs
      TypeParams: option<TsTypeParamInstantiation> }

  type TsQualifiedName = { Left: TsEntityName; Right: Ident }

  type TsEntityName =
    | TsQualifiedName of TsQualifiedName
    | Identifier of Ident

  type TsTypeParamInstantiation =
    { Params: list<TsType>
      Loc: option<SourceLocation> }

  type TsTypeQuery =
    { ExprName: TsTypeQueryExpr
      TypeArgs: Option<TsTypeParamInstantiation>
      Loc: option<SourceLocation> }

  type TsTypeQueryExpr =
    | TsEntityName of TsEntityName
    | Import of TsImportType

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
      // Init: Option<Expr>
      // Params: list<TsFnParam>
      TypeAnn: TsTypeAnn
      // TypeParams: option<TsTypeParamDecl>
      Loc: option<SourceLocation> }

  type TsGetterSignature =
    {
      // Readonly: bool
      Key: Expr
      Computed: bool
      Optional: bool
      TypeAnn: option<TsTypeAnn>
      Loc: option<SourceLocation> }

  type TsSetterSignature =
    {
      // Readonly: bool
      Key: Expr
      Computed: bool
      Optional: bool
      Param: TsFnParam
      Loc: option<SourceLocation> }

  type TsMethodSignature =
    {
      // Methods are always readonly
      // Readonly: bool
      Key: Expr
      Computed: bool
      Optional: bool
      Params: list<TsFnParam>
      TypeAnn: option<TsTypeAnn>
      TypeParams: option<TsTypeParamDecl>
      Loc: option<SourceLocation> }

  type TsIndexSignature =
    { Params: list<TsFnParam>
      TypeAnn: TsTypeAnn
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
    { Label: option<Pat>
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

  [<RequireQualifiedAccess>]
  type TsTypeOperatorOp =
    | KeyOf
    | Unique
    | Readonly

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
      TypeAnn: TsType
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

  type TsTypePredicate =
    { Asserts: bool
      ParamName: TsThisTypeOrIdent
      Typeann: option<TsTypeAnn>
      Loc: option<SourceLocation> }

  type TsThisTypeOrIdent =
    | TsThisType of TsThisType
    | Ident of Ident

  type TsImportType =
    { Arg: Str
      Qualifier: Option<TsEntityName>
      Type_args: Option<TsTypeParamInstantiation>
      Loc: option<SourceLocation> }
