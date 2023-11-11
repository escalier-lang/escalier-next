namespace Escalier.HindleyMilner

module Syntax =
  type Expr =
    | Ident of name: string
    | Lambda of name: string * body: Expr
    | Apply of func: Expr * argument: Expr
    | Let of name: string * definition: Expr * body: Expr
    | LetRec of name: string * definition: Expr * body: Expr

    override this.ToString() =
      match this with
      | Ident name -> name
      | Lambda(v, body) -> $"fun {v} -> {body}"
      | Apply(fn, arg) -> $"{fn} {arg}"
      | Let(v, def, body) -> $"let {v} = {def} in {body}"
      | LetRec(v, def, body) -> $"let rec {v} = {def} in {body}"

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

  type Type =
    { kind: TypeKind } // TODO: add provenance later

    override this.ToString() =
      match this.kind with
      | TypeVar({ instance = Some(instance) }) -> instance.ToString()
      | TypeVar({ instance = None } as v) -> $"t{v.id}"
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
