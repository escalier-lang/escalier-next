namespace Escalier.HindleyMilner

module rec Syntax =
  type Function =
    { typeParams: option<list<string>>
      args: list<string>
      body: list<Stmt> } // last item is the return value

  type Expr =
    | Ident of name: string
    | Lambda of Function
    | Apply of func: Expr * arguments: list<Expr>
    | Tuple of elements: list<Expr>
    | IfElse of condition: Expr * thenBranch: Expr * elseBranch: Expr
    | Binary of op: string * left: Expr * right: Expr

    override this.ToString() =
      match this with
      | Ident name -> name
      | Lambda f ->
        // TODO: add type params
        let args = String.concat ", " f.args
        $"fun ({args}) -> {f.body}"
      | Apply(fn, arg) -> $"{fn} {arg}"
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

  type Function =
    { typeParams: option<list<string>>
      args: list<Type>
      ret: Type }

  type TypeKind =
    | TypeVar of TypeVar
    | TypeOp of TypeOp
    | Tuple of list<Type>
    | Function of Function

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
      | Function f ->
        let args =
          List.map (fun item -> item.ToString()) f.args |> String.concat ", "

        let typeParams =
          match f.typeParams with
          | Some(typeParams) ->
            let sep = ", "
            $"<{String.concat sep typeParams}>"
          | None -> ""

        $"fn {typeParams}({args}) -> {f.ret}"
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

  type Scheme =
    { typeParams: list<string>
      ty: Type }

    override this.ToString() =
      let typeParams = String.concat ", " this.typeParams
      $"<{typeParams}>{this.ty}"
