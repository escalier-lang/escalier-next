namespace Escalier.Data

open Escalier.Data.Syntax
open Escalier.Data.Type

module Visitor =
  let rec walk_type (v: Type -> unit) (t: Type) : unit =
    v t

    match t.kind with
    | Array elem -> walk_type v elem
    | TypeVar tv ->
      maybe_walk_type v tv.instance |> ignore
      maybe_walk_type v tv.bound |> ignore
    | TypeRef { type_args = typeArgs
                scheme = scheme } ->
      Option.map (List.iter (walk_type v)) typeArgs |> ignore

      Option.map
        (fun (scheme: Scheme) ->
          walk_type v scheme.type_
          walk_type_params v scheme.type_params)
        scheme
      |> ignore
    | Literal _ -> () // leaf node
    | Primitive _ -> () // leaf node
    | Tuple types -> List.iter (walk_type v) types
    | Union types -> List.iter (walk_type v) types
    | Intersection types -> List.iter (walk_type v) types
    | Keyword _ -> () // leaf node
    | Function f ->
      List.iter (walk_type v) (List.map (fun p -> p.type_) f.param_list)
    | Object objTypeElems ->
      for elem in objTypeElems do
        match elem with
        | Callable(callable) ->
          List.iter
            (walk_type v)
            (List.map (fun p -> p.type_) callable.param_list)

          walk_type v callable.return_type
        | _ -> ()
    | Rest t -> walk_type v t
    | KeyOf t -> walk_type v t
    | Index(target, index) ->
      walk_type v target
      walk_type v index
    | Condition(check, extends, trueType, falseType) ->
      walk_type v check
      walk_type v extends
      walk_type v trueType
      walk_type v falseType
    | Infer _ -> () // leaf node
    | Wildcard -> () // leaf node
    | Binary(left, _op, right) ->
      walk_type v left
      walk_type v right

  and maybe_walk_type (v: Type -> unit) (ot: option<Type>) : unit =
    Option.map (walk_type v) ot |> ignore

  and walk_func (v: Type -> unit) (f: Function) : unit =
    List.iter (walk_type v) (List.map (fun p -> p.type_) f.param_list)
    walk_type v f.return_type
    walk_type v f.throws
    Option.map (walk_type_params v) f.type_params |> ignore

  and walk_type_params (v: Type -> unit) (tp: list<TypeParam>) : unit =
    List.iter
      (fun (tp: TypeParam) ->
        maybe_walk_type v tp.constraint_
        maybe_walk_type v tp.default_)
      tp

  type SyntaxVisitor() =
    abstract member VisitExpr: Expr -> unit
    abstract member VisitStmt: Stmt -> unit
    abstract member VisitTypeAnn: TypeAnn -> unit
    abstract member VisitPattern: Syntax.Pattern -> unit
    abstract member VisitBlock: Block -> unit
    abstract member VisitScript: Script -> unit

    default this.VisitExpr(e: Expr) =
      match e.kind with
      | ExprKind.Assign(left, _op, right) ->
        this.VisitExpr left
        this.VisitExpr right
      | ExprKind.Identifier _ -> ()
      | ExprKind.Literal _ -> ()
      | ExprKind.Object elems ->
        List.iter
          (fun (elem: ObjElem) ->
            // TODO: add support for computed keys
            match elem with
            | ObjElem.Property(_span, _key, value) -> this.VisitExpr value
            | ObjElem.Spread(_span, value) -> this.VisitExpr value)
          elems
      | ExprKind.Tuple elems -> List.iter this.VisitExpr elems
      | ExprKind.Binary(left, _, right) ->
        this.VisitExpr left
        this.VisitExpr right
      | ExprKind.Unary(_op, value) -> this.VisitExpr value
      | ExprKind.Function f ->
        match f.body with
        | BlockOrExpr.Expr(e) -> this.VisitExpr e
        | BlockOrExpr.Block(b) -> this.VisitBlock b
      | ExprKind.Call call ->
        this.VisitExpr call.callee

        match call.typeArgs with
        | Some(typeArgs) -> List.iter this.VisitTypeAnn typeArgs
        | None -> ()

        List.iter this.VisitExpr call.args
      | ExprKind.Index(target, index, _optChain) ->
        this.VisitExpr target
        this.VisitExpr index
      | ExprKind.Member(target, _name, _optChain) -> this.VisitExpr target
      | ExprKind.IfElse(cond, thenBranch, elseBranch) ->
        this.VisitExpr cond

        match thenBranch with
        | BlockOrExpr.Expr(e) -> this.VisitExpr e
        | BlockOrExpr.Block(b) -> this.VisitBlock b

        match elseBranch with
        | Some(BlockOrExpr.Expr(e)) -> this.VisitExpr e
        | Some(BlockOrExpr.Block(b)) -> this.VisitBlock b
        | _ -> ()
      | ExprKind.Match(target, cases) ->
        this.VisitExpr target

        List.iter
          (fun (case: MatchCase) ->
            this.VisitPattern case.pattern
            this.MaybeWalkExpr case.guard
            this.VisitExpr case.body)
          cases
      | ExprKind.Try(body, catch, ``finally``) -> failwith "todo"
      | ExprKind.Do body -> failwith "todo"
      | ExprKind.Await value -> this.VisitExpr value
      | ExprKind.Throw value -> this.VisitExpr value
      | ExprKind.TemplateLiteral templateLiteral -> failwith "todo"
      | ExprKind.TaggedTemplateLiteral(tag, template, throws) -> failwith "todo"

    member this.MaybeWalkExpr(e: option<Expr>) =
      match e with
      | Some(e) -> this.VisitExpr e
      | None -> ()

    default this.VisitStmt(s: Stmt) =
      match s.kind with
      | Decl decl ->
        match decl.kind with
        | DeclKind.VarDecl(pattern, exprOption, typeAnnOption, isDeclare) ->
          this.VisitPattern pattern
          this.MaybeWalkExpr exprOption
          this.MaybeWalkTypeAnn typeAnnOption
        | DeclKind.TypeDecl(name, typeAnn, typeParamsOption) ->
          this.VisitTypeAnn typeAnn

          match typeParamsOption with
          | Some(typeParams) ->
            List.iter
              (fun (typeParam: Syntax.TypeParam) ->
                this.MaybeWalkTypeAnn typeParam.constraint_
                this.MaybeWalkTypeAnn typeParam.default_)
              typeParams
          | None -> ()
      | Expr expr -> this.VisitExpr expr
      | For(left, right, body) ->
        this.VisitPattern left
        this.VisitExpr right
        List.iter this.VisitStmt body.stmts
      | Return exprOption -> this.MaybeWalkExpr exprOption

    default this.VisitTypeAnn(ta: TypeAnn) = ()

    member this.MaybeWalkTypeAnn(ota: option<TypeAnn>) =
      match ota with
      | Some(ta) -> this.VisitTypeAnn ta
      | None -> ()

    default this.VisitPattern(p: Syntax.Pattern) = ()
    default this.VisitBlock(b: Block) = List.iter this.VisitStmt b.stmts
    default this.VisitScript(s: Script) = ()
