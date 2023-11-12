namespace Escalier.HindleyMilner

module rec Syntax =
  type Expr =
    | Ident of name: string
    | Lambda of parameters: list<string> * body: list<Stmt> // last item is the return value
    | Apply of func: Expr * arguments: list<Expr>
    // | Let of name: string * definition: Expr * body: Expr
    // | LetRec of name: string * definition: Expr * body: Expr
    | Tuple of elements: list<Expr>
    | IfElse of condition: Expr * thenBranch: Expr * elseBranch: Expr
    | Binary of op: string * left: Expr * right: Expr

    override this.ToString() =
      match this with
      | Ident name -> name
      | Lambda(args, body) ->
        let args = String.concat ", " args
        $"fun ({args}) -> {body}"
      | Apply(fn, arg) -> $"{fn} {arg}"
      // | Let(v, def, body) -> $"let {v} = {def} in {body}"
      // | LetRec(v, def, body) -> $"let rec {v} = {def} in {body}"
      | Tuple elems ->
        let elems =
          List.map (fun item -> item.ToString()) elems |> String.concat ", "

        $"[{elems}]"
      | IfElse(condition, thenBranch, elseBranch) ->
        $"if {condition} then {thenBranch} else {elseBranch}"
      | Binary(op, left, right) -> $"({left} {op} {right})"

  type Stmt =
    | Expr of Expr
    | Let of name: string * definition: Expr
    | LetRec of name: string * definition: Expr

    override this.ToString() =
      match this with
      | Expr expr -> expr.ToString()
      | Let(v, def) -> $"let {v} = {def}"
      | LetRec(v, def) -> $"let rec {v} = {def}"

module rec Type =
  ///A type variable standing for an arbitrary type.
  ///All type variables have a unique id, but names are only assigned lazily, when required.
  type TypeVar =
    { id: int
      mutable instance: option<Type> }

  ///An n-ary type constructor which builds a new type from old
  type TypeOp = { name: string; types: list<Type> }

  type TypeKind =
    | TypeVar of TypeVar
    | TypeOp of TypeOp
    | Tuple of list<Type>
    | Function of (list<Type>) * Type // TODO: extend to support n-ary functions

  type Type =
    { kind: TypeKind } // TODO: add provenance later

    override this.ToString() =
      match this.kind with
      | TypeVar({ instance = Some(instance) }) -> instance.ToString()
      | TypeVar({ instance = None } as v) -> $"t{v.id}"
      | Tuple elems ->
        let elems =
          List.map (fun item -> item.ToString()) elems |> String.concat ", "

        $"[{elems}]"
      | Function(args, retType) ->
        let args =
          List.map (fun item -> item.ToString()) args |> String.concat ", "

        $"fn ({args}) -> {retType}"
      | TypeOp({ name = tyopName; types = tyopTypes }) ->
        match List.length tyopTypes with
        | 0 -> tyopName
        | 2 ->
          sprintf
            "(%s %s %s)"
            ((List.item 0 tyopTypes).ToString())
            tyopName
            ((List.item 1 tyopTypes).ToString())
        | _ ->
          sprintf
            "%s %s"
            tyopName
            (String.concat
              " "
              (List.map (fun item -> item.ToString()) tyopTypes))
