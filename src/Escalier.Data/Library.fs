﻿namespace rec Escalier.Data

open FParsec
open System.Text

module Common =
  type Literal =
    | Number of float
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

  type MappedModifier =
    | Add
    | Remove


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

  type Function =
    { Sig: FuncSig<option<TypeAnn>>
      Body: BlockOrExpr }

  type MatchCase =
    { Span: Span
      Pattern: Pattern
      Guard: option<Expr>
      Body: Expr }

  type TemplateLiteral =
    { Parts: list<string>
      Exprs: list<Expr> }

  type ObjElem =
    // TODO: Add syntax for specifying callables
    // TODO: Add support for getters, setters, and methods
    | Property of span: Span * key: string * value: Expr
    | Shorthand of span: Span * key: string
    | Spread of span: Span * value: Expr

  type Call =
    { Callee: Expr
      TypeArgs: option<list<TypeAnn>>
      Args: list<Expr>
      OptChain: bool
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
    | Tuple of elements: list<Expr>
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
    | Object of elems: list<ObjElem>
    | Try of
      body: Block *
      catch: option<string * Block> *
      finally_: option<Block>
    | Do of body: Block
    | Await of Await
    | Throw of value: Expr
    | TemplateLiteral of TemplateLiteral
    | TaggedTemplateLiteral of
      tag: Expr *
      template: TemplateLiteral *
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

  type ObjPatElem =
    // TODO: add isMut
    | KeyValuePat of
      span: Span *
      key: string *
      value: Pattern *
      init: option<Expr>
    | ShorthandPat of
      span: Span *
      name: string *
      init: option<Expr> *
      isMut: bool
    // TODO: rename to RestSpreadPat
    | RestPat of span: Span * target: Pattern * isMut: bool

  type BindingIdent =
    { Span: Span
      Name: string
      IsMut: bool }

  [<RequireQualifiedAccess>]
  type PatternKind =
    | Identifier of BindingIdent
    | Object of elems: list<ObjPatElem> // TODO: rest patterns
    | Tuple of elems: list<Pattern> // TODO: rest patterns
    | Wildcard
    | Literal of span: Span * value: Common.Literal
    // TODO: get rid of `is_mut` since it's covered by `ident: BindingIdent`
    | Is of span: Span * ident: BindingIdent * is_name: string * is_mut: bool

    override this.ToString() =
      match this with
      | Identifier(bi) -> bi.Name // TODO: isMut
      | _ -> failwith "TODO"

  type Pattern =
    { Kind: PatternKind
      Span: Span
      mutable InferredType: option<Type.Type> }

    override this.ToString() = this.Kind.ToString()

  type DeclKind =
    | VarDecl of name: Pattern * init: Expr * typeAnn: option<TypeAnn>
    | TypeDecl of
      name: string *
      typeAnn: TypeAnn *
      typeParams: option<list<TypeParam>>

  type Decl = { Kind: DeclKind; Span: Span }

  type StmtKind =
    | Expr of Expr
    | For of left: Pattern * right: Expr * body: Block
    | Return of option<Expr>
    | Decl of Decl

  type Stmt = { Kind: StmtKind; Span: Span }

  type Property =
    { Name: string
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
    | Callable of Function
    | Constructor of Function
    | Method of name: string * is_mut: bool * type_: Function
    | Getter of name: string * return_type: TypeAnn * throws: TypeAnn
    | Setter of name: string * param: FuncParam<TypeAnn> * throws: TypeAnn
    | Property of Property
    | Mapped of Mapped

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
    | Stmt of Stmt

  type Module = { Items: list<ModuleItem> }

module Type =
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

  type ObjPatElem =
    | KeyValuePat of key: string * value: Pattern * init: option<Syntax.Expr>
    | ShorthandPat of name: string * init: option<Syntax.Expr>
    | RestPat of target: Pattern

  type Pattern =
    | Identifier of name: string
    | Object of elems: list<ObjPatElem>
    // TODO: support sparse tuples
    // TODO: support rest patterns
    | Tuple of elems: list<option<Pattern>>
    | Wildcard
    | Literal of Common.Literal
    | Is of target: Syntax.BindingIdent * id: string
    | Rest of target: Pattern

    override this.ToString() =
      match this with
      | Identifier name -> name
      | Object elems ->
        let elems =
          List.map
            (fun elem ->
              match elem with
              | KeyValuePat(key, value, init) ->
                match init with
                | Some(init) -> $"{key}: {value} = {init}"
                | None -> $"{key}: {value}"
              | ShorthandPat(name, init) ->
                match init with
                | Some(value) -> $"{name} = {value}"
                | None -> name
              | RestPat(target) -> $"...{target}")
            elems

        let elems = String.concat ", " elems
        $"{{{elems}}}"
      | Tuple elems ->
        let elems =
          List.map (fun item -> item.ToString()) elems |> String.concat ", "

        $"[{elems}]"
      | Wildcard -> "_"
      | Literal lit -> lit.ToString()
      | Is({ Name = name }, id) -> $"{name} is {id}"
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

    override this.ToString() =
      let args =
        List.map (fun item -> item.ToString()) this.ParamList
        |> String.concat ", "

      let typeParams =
        match this.TypeParams with
        | Some(typeParams) ->
          let sep = ", "
          let typeParams = List.map (fun t -> t.ToString()) typeParams
          $"<{String.concat sep typeParams}>"
        | None -> ""

      // printfn "this.Throws = %A" this.Throws

      match (prune this.Throws).Kind with
      | TypeKind.Keyword Keyword.Never ->
        $"fn {typeParams}({args}) -> {this.Return}"
      | _ -> $"fn {typeParams}({args}) -> {this.Return} throws {this.Throws}"

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

    override this.ToString() =
      let name =
        match this.NameType with
        | Some(t) -> t.ToString()
        | None -> this.TypeParam.Name

      let optional =
        match this.Optional with
        | Some(optional) ->
          match optional with
          | Common.MappedModifier.Add -> "+?"
          | Common.MappedModifier.Remove -> "-?"
        | None -> ""

      let readonly =
        match this.Readonly with
        | Some(readonly) ->
          match readonly with
          | Common.MappedModifier.Add -> "+readonly"
          | Common.MappedModifier.Remove -> "-readonly"
        | None -> ""

      $"{readonly}[{name}]{optional}: {this.TypeAnn} for {this.TypeParam.Name} in {this.TypeParam.Constraint}"

  type ObjKey = string // TODO

  type Property =
    { Name: ObjKey
      Optional: bool
      Readonly: bool
      Type: Type }

  type ObjTypeElem =
    | Callable of Function
    | Constructor of Function
    | Method of name: ObjKey * is_mut: bool * fn: Function
    | Getter of name: ObjKey * return_type: Type * throws: Type
    | Setter of name: ObjKey * param: FuncParam * throws: Type
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
        sprintf
          "get %s() -> %s%s"
          name
          (return_type.ToString())
          (if throws.Kind = TypeKind.Keyword Keyword.Never then
             ""
           else
             " throws " + throws.ToString())
      | Setter(name, param, throws) ->
        sprintf
          "set %s(%s)%s"
          name
          (param.ToString())
          (if throws.Kind = TypeKind.Keyword Keyword.Never then
             ""
           else
             " throws " + throws.ToString())
      | Mapped(mapped) -> mapped.ToString()
      | Property { Name = name
                   Optional = optional
                   Readonly = readonly
                   Type = type_ } ->
        sprintf
          "%s%s%s: %s"
          (if optional then "optional " else "")
          (if readonly then "readonly " else "")
          name
          (type_.ToString())

  [<RequireQualifiedAccess>]
  type TypeKind =
    | TypeVar of TypeVar
    | TypeRef of TypeRef
    | Primitive of Primitive
    | Keyword of Keyword
    | Function of Function
    | Object of list<ObjTypeElem>
    | Rest of Type
    | Literal of Common.Literal
    | Union of list<Type> // TODO: use `Set<type>`
    | Intersection of list<Type> // TODO: use `Set<type>`
    | Tuple of list<Type>
    | Array of Type
    | KeyOf of Type
    | Index of target: Type * index: Type
    | Condition of
      check: Type *
      extends: Type *
      trueType: Type *
      falseType: Type
    | Infer of name: string
    | Binary of left: Type * op: string * right: Type // use TypeRef? - const folding is probably a better approach
    | Wildcard

  [<CustomEquality; NoComparison>]
  type Type =
    { Kind: TypeKind
      mutable Provenance: option<Provenance> }

    override this.Equals other =
      match other with
      | :? Type as p -> p.Kind.Equals this.Kind
      | _ -> false

    override this.GetHashCode() = this.Kind.GetHashCode()

    // TODO: handle operator precedence when converting types to strings
    override this.ToString() =
      match this.Kind with
      | TypeKind.TypeVar({ Instance = Some(instance) }) -> instance.ToString()
      | TypeKind.TypeVar({ Instance = None; Bound = bound } as v) ->
        let bound =
          match bound with
          | Some(bound) -> $":{bound}"
          | None -> ""

        $"t{v.Id}{bound}"
      | TypeKind.TypeRef({ Name = name; TypeArgs = typeArgs }) ->
        let typeArgs =
          match typeArgs with
          | Some(typeArgs) ->
            let sep = ", "
            $"<{typeArgs |> List.map (fun t -> t.ToString()) |> String.concat sep}>"
          | None -> ""

        $"{name}{typeArgs}"
      | TypeKind.Primitive prim -> prim.ToString()
      | TypeKind.Keyword keyword -> keyword.ToString()
      | TypeKind.Function f -> f.ToString()
      | TypeKind.Literal lit -> lit.ToString()
      | TypeKind.Union types ->
        List.map (fun item -> item.ToString()) types |> String.concat " | "
      | TypeKind.Intersection types ->
        List.map (fun item -> item.ToString()) types |> String.concat " & "
      | TypeKind.Tuple elems ->
        let elems =
          List.map (fun item -> item.ToString()) elems |> String.concat ", "

        $"[{elems}]"
      | TypeKind.Array t -> $"{t}[]"
      | TypeKind.Wildcard -> "_"
      | TypeKind.Object elems ->
        let elems =
          List.map
            (fun (elem: ObjTypeElem) ->
              match elem with
              | Property { Name = name
                           Optional = optional
                           Readonly = readonly
                           Type = type_ } ->
                let optional = if optional then "?" else ""
                let readonly = if readonly then "readonly " else ""
                $"{readonly}{name}{optional}: {type_}"
              | Mapped mapped -> mapped.ToString()
              | _ -> failwith "TODO: Type.ToString - Object - Elem"

            )
            elems

        let elems = String.concat ", " elems
        $"{{{elems}}}"
      | TypeKind.Rest t -> $"...{t}"
      | TypeKind.Index(target, index) -> $"{target}[{index}]"
      | TypeKind.Condition(check, extends, trueType, falseType) ->
        $"{check} extends {extends} ? {trueType} : {falseType}"
      | TypeKind.KeyOf t -> $"keyof {t}"
      // TODO: handle operator precedence
      | TypeKind.Binary(left, op, right) -> $"{left} {op} {right}"
      | _ ->
        printfn "this.Kind = %A" this.Kind
        failwith "TODO: finish implementing Type.ToString"

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
