namespace rec Escalier.HindleyMilner

open FParsec

module Syntax =
  type Span = { start: Position; stop: Position }

  type Block = { span: Span; stmts: list<Stmt> }

  type TypeParam = string // TODO

  type FuncParam<'T> =
    { pattern: Pattern
      typeAnn: 'T
      optional: bool }

    override this.ToString() = this.pattern.ToString()

  type FuncSig<'T> =
    { typeParams: option<list<TypeParam>>
      paramList: list<FuncParam<'T>>
      ret: 'T
      throws: option<TypeAnn> }

  type Function =
    { sig': FuncSig<option<TypeAnn>>
      body: Block } // TODO: support fat arrow syntax

  type MatchCase =
    { span: Span
      pattern: Pattern
      guard: option<Expr>
      body: Expr }

  type TemplateLiteral =
    { parts: list<string>
      exprs: list<Expr> }

  type ObjElem =
    | Property of span: Span * key: string * value: Expr
    | Spread of span: Span * value: Expr

  type ExprKind =
    | Ident of name: string
    | Literal of Literal
    | Function of Function
    | Call of func: Expr * arguments: list<Expr>
    | Tuple of elements: list<Expr>
    // TODO: allow blocks for the branches
    // TODO: make the `elseBranch` optional
    | IfElse of condition: Expr * thenBranch: Expr * elseBranch: Expr
    | Match of target: Expr * cases: list<MatchCase>
    | Binary of op: string * left: Expr * right: Expr // TODO: BinaryOp
    | Unary of op: string * value: Expr
    | Object of elems: list<ObjElem>
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
      inferred_type: option<Type.Type> }

    override this.ToString() = this.kind.ToString()

  type Literal =
    | Number of string
    | String of string
    | Boolean of bool
    | Null
    | Undefined

    override this.ToString() =
      match this with
      | Number value -> value
      | String value -> $"\"{value}\""
      | Boolean value -> if value then "true" else "false"
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

    override this.ToString() =
      match this with
      | Identifier(bi) -> bi.name // TODO: isMut
      | _ -> failwith "TODO"

  type Pattern =
    { kind: PatternKind
      span: Span
      inferred_type: option<Type.Type> }

    override this.ToString() = this.kind.ToString()

  type Stmt =
    | Expr of Expr
    | For of leff: Pattern * right: Expr * body: Block
    | Let of name: string * definition: Expr
    | LetRec of name: string * definition: Expr

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
    | Binary of left: TypeAnn * op: string * right: TypeAnn // TODO: BinaryOp

  type TypeAnn =
    { kind: TypeAnnKind
      span: Span
      mutable inferred_type: option<Type.Type> }

module Type =
  [<RequireQualifiedAccess>]
  type Provenance =
    | Type of Type
    | Expr of Syntax.Expr
    | Pattern of Syntax.Pattern

  ///A type variable standing for an arbitrary type.
  ///All type variables have a unique id, but names are only assigned lazily, when required.
  type TypeVar =
    { id: int
      bound: option<Type>
      mutable instance: option<Type> }

  ///An n-ary type constructor which builds a new type from old
  type TypeRef =
    { name: string
      typeArgs: option<list<Type>> }

  type ObjPatElem =
    | KeyValuePat of key: string * value: Pattern
    | ShorthandPat of name: string * value: option<Type>
    | RestPat of target: Pattern

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
      | Identifier name -> name
      | _ -> failwith "TODO"

  type FuncParam =
    { pattern: Pattern
      type_: Type
      optional: bool }

    override this.ToString() = this.type_.ToString()

  type Function =
    { typeParams: option<list<string>>
      paramList: list<FuncParam>
      ret: Type }

    override this.ToString() =
      let args =
        List.map (fun item -> item.ToString()) this.paramList
        |> String.concat ", "

      let typeParams =
        match this.typeParams with
        | Some(typeParams) ->
          let sep = ", "
          $"<{String.concat sep typeParams}>"
        | None -> ""

      $"fn {typeParams}({args}) -> {this.ret}"

  type TypeKind =
    | TypeVar of TypeVar
    | TypeRef of TypeRef
    | Tuple of list<Type>
    | Function of Function
    | Literal of Syntax.Literal
    | Wildcard

  type Type =
    { kind: TypeKind
      provenance: option<Provenance> }

    override this.ToString() =
      match this.kind with
      | TypeVar({ instance = Some(instance) }) -> instance.ToString()
      | TypeVar({ instance = None } as v) -> $"t{v.id}"
      | Tuple elems ->
        let elems =
          List.map (fun item -> item.ToString()) elems |> String.concat ", "

        $"[{elems}]"
      | Function f -> f.ToString()
      | TypeRef({ name = name; typeArgs = typeArgs }) ->
        let typeArgs =
          match typeArgs with
          | Some(typeArgs) ->
            let sep = ", "
            $"<{typeArgs |> List.map (fun t -> t.ToString()) |> String.concat sep}>"
          | None -> ""

        $"{name}{typeArgs}"
      | Literal lit -> lit.ToString()
      | Wildcard -> "_"

  type Scheme =
    { typeParams: list<string>
      ty: Type }

    override this.ToString() =
      let typeParams = String.concat ", " this.typeParams
      $"<{typeParams}>{this.ty}"
