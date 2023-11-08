namespace rec Escalier.Data

open FParsec
open System.Text

module Syntax =
  type Span = { start: Position; stop: Position }

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

    override this.ToString() =
      match this with
      | Number(value) -> value
      | String(value) -> $"\"{value}\""
      | Boolean(true) -> "true"
      | Boolean(false) -> "false"
      | Null -> "null"
      | Undefined -> "undefined"

  type ObjPatElem =
    | KeyValuePat of
      span: Span *
      key: string *
      value: Pattern *
      init: option<Expr>
    | ShorthandPat of
      span: Span *
      name: string *
      init: option<Expr> *
      is_mut: bool

  type BindingIdent =
    { span: Span
      name: string
      isMut: bool }

  [<RequireQualifiedAccess>]
  type PatternKind =
    | Identifier of BindingIdent
    | Object of elems: list<ObjPatElem>
    | Tuple of elems: list<Pattern>
    | Wildcard
    | Literal of span: Span * value: Literal
    // TODO: get rid of `is_mut` since it's covered by `ident: BindingIdent`
    | Is of span: Span * ident: BindingIdent * is_name: string * is_mut: bool

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

  type FuncParam<'T> =
    { pattern: Pattern
      typeAnn: 'T
      optional: bool }

  type Function =
    { sig': FuncSig<option<TypeAnn>>
      body: BlockOrExpr }

  type ExprKind =
    | Identifer of string
    | Literal of Literal
    | Object of elems: list<ObjElem>
    | Tuple of elems: list<Expr>
    | Assign of left: Expr * op: AssignOp * right: Expr
    | Binary of left: Expr * op: BinaryOp * right: Expr
    | Unary of op: string * value: Expr
    | Function of Function
    | Call of
      callee: Expr *
      type_args: option<list<TypeAnn>> *
      args: list<Expr> *
      opt_chain: bool *
      throws: option<Type.Type>
    | Index of target: Expr * index: Expr * opt_chain: bool
    | Member of target: Expr * name: string * opt_chain: bool
    | IfElse of
      cond: Expr *
      then_branch: BlockOrExpr *
      else_branch: option<BlockOrExpr>
    | Match of target: Expr * cases: list<MatchCase>
    | Try of body: Block * catch: option<Expr> * finally_: option<Expr>
    | Do of body: Block
    | Await of value: Expr // TODO: convert rejects to throws
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

  type ObjTypeAnnElem =
    | Callable of Function
    | Constructor of Function
    | Method of name: string * is_mut: bool * type_: Function
    | Getter of name: string * return_type: TypeAnn * throws: TypeAnn
    | Setter of name: string * param: FuncParam<TypeAnn> * throws: TypeAnn

  type KeywordTypeAnn =
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
      bound: option<TypeAnn>
      default_: option<TypeAnn> }

  type FuncSig<'T> =
    { type_params: option<list<TypeParam>>
      param_list: list<FuncParam<'T>>
      return_type: 'T
      throws: option<TypeAnn> }

  type FunctionType = FuncSig<TypeAnn>

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
    | Literal of Literal
    | Keyword of keyword: KeywordTypeAnn
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

  // TODO: add support for imports
  type Script = list<Stmt>

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
    | Is of target: Syntax.BindingIdent * id: string
    | Rest of target: Pattern

    override this.ToString() =
      match this with
      | Identifier(name) -> name
      | Object(elems) ->
        sprintf
          "{%A}"
          (elems |> List.map (fun e -> e.ToString()) |> String.concat ", ")
      | Tuple(elems) ->
        let elems =
          elems |> List.map (fun e -> e.ToString()) |> String.concat ", "

        $"[{elems}]"
      | Wildcard -> "_"
      | Literal(lit) -> lit.ToString()
      | Is(target, id) -> $"{target} is {id}"
      | Rest(target) -> $"...{target}"

  type ObjPatElem =
    | KeyValuePat of key: string * value: Pattern
    | ShorthandPat of name: string * value: option<Type>
    | RestPat of target: Pattern

  type FuncParam =
    { pattern: Pattern
      type_: Type
      optional: bool }

    override this.ToString() =
      sprintf "%s: %s" (this.pattern.ToString()) (this.type_.ToString())

  [<RequireQualifiedAccess>]
  type KeywordType =
    | Never
    | Object
    | Unknown

    override this.ToString() =
      match this with
      | Never -> "never"
      | Object -> "object"
      | Unknown -> "unknown"

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

  type Function =
    { param_list: list<FuncParam>
      return_type: Type
      type_params: option<list<TypeParam>>
      throws: Type }

    override this.ToString() =
      sprintf
        "fn (%s) -> %s"
        (this.param_list
         |> List.map (fun p -> p.ToString())
         |> String.concat ", ")
        (this.return_type.ToString())

  type Mapped =
    { key: Type
      value: Type
      target: string
      source: Type
      optional: option<MappedModifier>

      // First half of Conditional
      check: option<Type>
      extends: option<Type> }

    override this.ToString() =
      let optional =
        match this.optional with
        | Some(modifier) -> modifier.ToString()
        | None -> ""

      $"[{this.key}]{optional}: {this.value} for {this.target} in {this.source}"

  type MappedModifier =
    | Add
    | Remove

    override this.ToString() =
      match this with
      | Add -> "+?"
      | Remove -> "-?"

  type ObjKey = string // TODO

  type ObjTypeElem =
    | Callable of Function
    | Constructor of Function
    | Method of name: ObjKey * is_mut: bool * type_: Function
    | Getter of name: ObjKey * return_type: Type * throws: Type
    | Setter of name: ObjKey * param: FuncParam * throws: Type
    | Mapped of Mapped
    | Property of name: ObjKey * optional: bool * readonly: bool * type_: Type

    override this.ToString() =
      match this with
      | Callable(func) -> func.ToString()
      | Constructor(func) -> sprintf "new %s" (func.ToString())
      | Method(name,
               is_mut,
               { param_list = param_list
                 return_type = return_type }) ->
        let sb = new StringBuilder()

        let self = if is_mut then "mut self" else "self"
        let param_list' = self :: List.map (fun p -> p.ToString()) param_list

        sb
          .Append("fn ")
          .Append(name)
          .Append("(")
          // Do we need to include `self` in types?
          .Append(if is_mut then "mut self" else "self")
          .Append(String.concat (", ") param_list')
          .Append(") -> ")
          .Append(return_type)
        |> ignore

        sb.ToString()
      | Getter(name, return_type, throws) ->
        sprintf
          "get %s() -> %s%s"
          name
          (return_type.ToString())
          (if throws.kind = TypeKind.Keyword(KeywordType.Never) then
             ""
           else
             " throws " + throws.ToString())
      | Setter(name, param, throws) ->
        sprintf
          "set %s(%s)%s"
          name
          (param.ToString())
          (if throws.kind = TypeKind.Keyword(KeywordType.Never) then
             ""
           else
             " throws " + throws.ToString())
      | Mapped(mapped) ->
        sprintf
          "%s%s%s%s%s"
          (if mapped.optional.IsSome then "optional " else "")
          (if mapped.check.IsSome then "check " else "")
          (if mapped.extends.IsSome then "extends " else "")
          (mapped.key.ToString())
          (mapped.value.ToString())
      | Property(name, optional, readonly, type_) ->
        sprintf
          "%s%s%s: %s"
          (if optional then "optional " else "")
          (if readonly then "readonly " else "")
          name
          (type_.ToString())

  type TypeVar =
    { id: int
      mutable instance: option<Type>
      bound: option<Type> }

  type TypeKind =
    | TypeVar of TypeVar
    | TypeRef of
      name: string *
      type_args: option<list<Type>> *
      scheme: option<Scheme> // used so that we can reference a type ref's scheme without importing it
    | Literal of Syntax.Literal
    | Primitive of Primitive
    | Tuple of list<Type>
    | Array of Type
    | Union of list<Type>
    | Intersection of list<Type>
    | Keyword of KeywordType
    | Function of Function
    | Object of list<ObjTypeElem>
    | Rest of Type
    | KeyOf of Type
    | Index of target: Type * index: Type
    | Condition of
      check: Type *
      extends: Type *
      true_type: Type *
      false_type: Type
    | Infer of name: string
    | Wildcard
    | Binary of left: Type * op: Syntax.BinaryOp * right: Type

    // TODO: add parenthesizes where necessary
    override this.ToString() =
      match this with
      | TypeVar({ id = id; instance = instance }) ->
        match instance with
        | Some(instance) -> instance.ToString()
        | None -> $"t{id}"
      | TypeRef(name, type_args, _) ->
        let sb = StringBuilder()
        sb.Append(name) |> ignore

        match type_args with
        | Some(type_args) ->
          sb
            .Append("<")
            .Append(
              String.concat
                (", ")
                (type_args |> List.map (fun t -> t.ToString()))
            )
            .Append(">")
          |> ignore
        | None -> ()

        sb.ToString()
      | Literal(lit) -> lit.ToString()
      | Primitive(prim) -> prim.ToString()
      | Tuple(elems) ->
        let elems =
          elems |> List.map (fun e -> e.ToString()) |> String.concat ", "

        $"[{elems}]"
      | Array(elem) -> $"{elem}[]"
      | Union(types) ->
        (types |> List.map (fun t -> t.ToString()) |> String.concat " | ")
      | Intersection(types) ->
        (types |> List.map (fun t -> t.ToString()) |> String.concat " & ")
      | Keyword(keyword) -> keyword.ToString()
      | Function(func) -> func.ToString()
      | Object(elems) ->
        sprintf
          "{%A}"
          (elems |> List.map (fun e -> e.ToString()) |> String.concat ", ")
      | Rest(target) -> $"...{target}"
      | KeyOf(target) -> $"keyof {target}"
      | Index(target, index) -> $"{target}[{index}]"
      | Condition(check, extends, true_type, false_type) ->
        $"{check} extends {extends} ? {true_type} : {false_type}"
      | Infer(name) -> $"infer {name}"
      | Wildcard -> "_"
      | Binary(left, op, right) -> $"{left} {op} {right}"

  [<RequireQualifiedAccess>]
  type Provenance =
    | Type of Type
    | Expr of Syntax.Expr

  type Type =
    { kind: TypeKind
      mutable provenance: option<Provenance> }

    override this.ToString() = this.kind.ToString()
