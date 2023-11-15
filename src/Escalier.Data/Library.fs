namespace rec Escalier.Data

open FParsec
open System.Text

module Syntax =
  type Span = { Start: Position; Stop: Position }

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

  type Decl = { Span: Span; Kind: DeclKind }

  type StmtKind =
    | Expr of Expr
    | For of leff: Pattern * right: Expr * body: Block
    | Return of option<Expr>
    | Decl of Decl

  type Stmt = { Span: Span; Kind: StmtKind }

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
    { Span: Span
      Name: string
      IsMut: bool }

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
    { Kind: PatternKind
      Span: Span
      InferredType: option<Type.Type> }

  type Block = { Span: Span; Stmts: list<Stmt> }

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
    { Span: Span
      Pattern: Pattern
      Guard: option<Expr>
      Body: Expr }

  type TemplateLiteral =
    { Parts: list<string>
      Exprs: list<Expr> }

  type FuncParam<'T> =
    { Pattern: Pattern
      TypeAnn: 'T
      Optional: bool }

  type Function =
    { Sig: FuncSig<option<TypeAnn>>
      Body: BlockOrExpr }

  type Call =
    { Callee: Expr
      TypeArgs: option<list<TypeAnn>>
      Args: list<Expr>
      OptChain: bool
      mutable Throws: option<Type.Type> }

  type ExprKind =
    | Identifier of string
    | Literal of Literal
    | Object of elems: list<ObjElem>
    | Tuple of elems: list<Expr>
    | Assign of left: Expr * op: AssignOp * right: Expr
    | Binary of left: Expr * op: BinaryOp * right: Expr
    | Unary of op: string * value: Expr
    | Function of Function
    | Call of Call
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
    { Kind: ExprKind
      Span: Span
      mutable InferredType: option<Type.Type> }

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
    { Span: Span
      Name: string
      Constraint: option<TypeAnn>
      Default: option<TypeAnn> }

    override this.ToString() =
      let sb = StringBuilder()

      sb
        .Append(this.Name)
        .Append(
          match this.Constraint with
          | Some(constraint_) -> $" : {constraint_}"
          | None -> ""
        )
        .Append(
          match this.Default with
          | Some(default_) -> $" = {default_}"
          | None -> ""
        )
      |> ignore

      sb.ToString()

  type FuncSig<'T> =
    { TypeParams: option<list<TypeParam>>
      ParamList: list<FuncParam<'T>>
      ReturnType: 'T
      Throws: option<TypeAnn> }

  type FunctionType = FuncSig<TypeAnn>

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
    { Kind: TypeAnnKind
      Span: Span
      mutable InferredType: option<Type.Type> }

  // TODO: add support for imports
  type Script = list<Stmt>

module Type =
  type TypeParam =
    { Name: string
      Constraint: option<Type>
      Default: option<Type> }

    override this.ToString() =
      let sb = StringBuilder()

      sb
        .Append(this.Name)
        .Append(
          match this.Constraint with
          | Some(constraint_) -> $" : {constraint_}"
          | None -> ""
        )
        .Append(
          match this.Default with
          | Some(default_) -> $" = {default_}"
          | None -> ""
        )
      |> ignore

      sb.ToString()

  type Scheme =
    { TypeParams: list<TypeParam>
      Type: Type
      IsTypeParam: bool }

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
    { Pattern: Pattern
      Type: Type
      Optional: bool }

    override this.ToString() =
      sprintf "%s: %s" (this.Pattern.ToString()) (this.Type.ToString())

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
    { ParamList: list<FuncParam>
      ReturnType: Type
      TypeParams: option<list<TypeParam>>
      Throws: Type }

    override this.ToString() =
      let sb = StringBuilder()

      // TODO: handle throws
      sb
        .Append("fn ")
        .Append(
          match this.TypeParams with
          | Some(typeParams) ->
            let typeParams =
              typeParams
              |> List.map (fun p -> p.ToString())
              |> String.concat ", "

            $"<{typeParams}>"
          | None -> ""
        )
        .Append("(")
        .Append(
          this.ParamList
          |> List.map (fun p -> p.ToString())
          |> String.concat ", "
        )
        .Append(") -> ")
        .Append(this.ReturnType.ToString())
      |> ignore

      sb.ToString()

  type Mapped =
    { Key: Type
      Value: Type
      Target: string
      Source: Type
      Optional: option<MappedModifier>

      // First half of Conditional
      Check: option<Type>
      Extends: option<Type> }

    override this.ToString() =
      let optional =
        match this.Optional with
        | Some(modifier) -> modifier.ToString()
        | None -> ""

      $"[{this.Key}]{optional}: {this.Value} for {this.Target} in {this.Source}"

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
               { ParamList = paramList
                 ReturnType = return_type }) ->
        let sb = StringBuilder()

        let self = if is_mut then "mut self" else "self"
        let paramList' = self :: List.map (fun p -> p.ToString()) paramList

        sb
          .Append("fn ")
          .Append(name)
          .Append("(")
          // Do we need to include `self` in types?
          .Append(if is_mut then "mut self" else "self")
          .Append(String.concat (", ") paramList')
          .Append(") -> ")
          .Append(return_type)
        |> ignore

        sb.ToString()
      | Getter(name, return_type, throws) ->
        sprintf
          "get %s() -> %s%s"
          name
          (return_type.ToString())
          (if throws.Kind = TypeKind.Keyword(KeywordType.Never) then
             ""
           else
             " throws " + throws.ToString())
      | Setter(name, param, throws) ->
        sprintf
          "set %s(%s)%s"
          name
          (param.ToString())
          (if throws.Kind = TypeKind.Keyword(KeywordType.Never) then
             ""
           else
             " throws " + throws.ToString())
      | Mapped(mapped) ->
        sprintf
          "%s%s%s%s%s"
          (if mapped.Optional.IsSome then "optional " else "")
          (if mapped.Check.IsSome then "check " else "")
          (if mapped.Extends.IsSome then "extends " else "")
          (mapped.Key.ToString())
          (mapped.Value.ToString())
      | Property(name, optional, readonly, type_) ->
        sprintf
          "%s%s%s: %s"
          (if optional then "optional " else "")
          (if readonly then "readonly " else "")
          name
          (type_.ToString())

  type TypeVar =
    { Id: int
      mutable Instance: option<Type>
      Bound: option<Type> }

  type TypeRef =
    { Name: string
      TypeArgs: option<list<Type>>
      // used so that we can reference a type ref's scheme without importing it
      Scheme: option<Scheme> }

  type TypeKind =
    | TypeVar of TypeVar
    | TypeRef of TypeRef
    | Literal of Syntax.Literal
    | Primitive of Primitive // use TypeRef
    | Tuple of list<Type> // use TypeRef
    | Array of Type // use TypeRef
    | Union of list<Type>
    | Intersection of list<Type>
    | Keyword of KeywordType // use TypeRef
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
    | Binary of left: Type * op: Syntax.BinaryOp * right: Type // use TypeRef? - const folding is probably a better approach

    // TODO: add parenthesizes where necessary
    override this.ToString() =
      match this with
      | TypeVar({ Id = id; Instance = instance }) ->
        match instance with
        | Some(instance) -> instance.ToString()
        | None -> $"t{id}"
      | TypeRef { Name = name; TypeArgs = typeArgs } ->
        let sb = StringBuilder()
        sb.Append(name) |> ignore

        match typeArgs with
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
    { Kind: TypeKind
      mutable Provenance: option<Provenance> }

    override this.ToString() = this.Kind.ToString()
