namespace Escalier.Data

open Escalier.Data.Syntax
open Escalier.Data.Type

module Visitor =
  let rec walkType (v: Type -> unit) (t: Type) : unit =
    v t

    match t.Kind with
    | Array elem -> walkType v elem
    | TypeVar tv ->
      maybeWalkType v tv.Instance |> ignore
      maybeWalkType v tv.Bound |> ignore
    | TypeRef { TypeArgs = typeArgs; Scheme = scheme } ->
      Option.map (List.iter (walkType v)) typeArgs |> ignore

      Option.map
        (fun (scheme: Scheme) ->
          walkType v scheme.Type
          walkTypeParams v scheme.TypeParams)
        scheme
      |> ignore
    | Literal _ -> () // leaf node
    | Primitive _ -> () // leaf node
    | Tuple types -> List.iter (walkType v) types
    | Union types -> List.iter (walkType v) types
    | Intersection types -> List.iter (walkType v) types
    | Keyword _ -> () // leaf node
    | Function f ->
      List.iter (walkType v) (List.map (fun p -> p.Type) f.ParamList)
    | Object objTypeElems ->
      for elem in objTypeElems do
        match elem with
        | Callable(callable) ->
          List.iter (walkType v) (List.map (fun p -> p.Type) callable.ParamList)

          walkType v callable.ReturnType
        | _ -> ()
    | Rest t -> walkType v t
    | KeyOf t -> walkType v t
    | Index(target, index) ->
      walkType v target
      walkType v index
    | Condition(check, extends, trueType, falseType) ->
      walkType v check
      walkType v extends
      walkType v trueType
      walkType v falseType
    | Infer _ -> () // leaf node
    | Wildcard -> () // leaf node
    | Binary(left, _op, right) ->
      walkType v left
      walkType v right

  and maybeWalkType (v: Type -> unit) (ot: option<Type>) : unit =
    Option.map (walkType v) ot |> ignore

  and walkFunc (v: Type -> unit) (f: Function) : unit =
    List.iter (walkType v) (List.map (fun p -> p.Type) f.ParamList)
    walkType v f.ReturnType
    walkType v f.Throws
    Option.map (walkTypeParams v) f.TypeParams |> ignore

  and walkTypeParams (v: Type -> unit) (tp: list<TypeParam>) : unit =
    List.iter
      (fun (tp: TypeParam) ->
        maybeWalkType v tp.Constraint
        maybeWalkType v tp.Default)
      tp

  type SyntaxVisitor() =
    abstract member VisitExpr: Expr -> unit
    abstract member VisitStmt: Stmt -> unit
    abstract member VisitTypeAnn: TypeAnn -> unit
    abstract member VisitPattern: Syntax.Pattern -> unit
    abstract member VisitBlock: Block -> unit
    abstract member VisitScript: Script -> unit

    default this.VisitExpr(e: Expr) =
      match e.Kind with
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
        match f.Body with
        | BlockOrExpr.Expr(e) -> this.VisitExpr e
        | BlockOrExpr.Block(b) -> this.VisitBlock b
      | ExprKind.Call call ->
        this.VisitExpr call.Callee

        match call.TypeArgs with
        | Some(typeArgs) -> List.iter this.VisitTypeAnn typeArgs
        | None -> ()

        List.iter this.VisitExpr call.Args
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
            this.VisitPattern case.Pattern
            this.MaybeWalkExpr case.Guard
            this.VisitExpr case.Body)
          cases
      | ExprKind.Try(body, catch, ``finally``) ->
        failwith "TODO: VisitExpr - Try"
      | ExprKind.Do body -> failwith "TODO: VisitExpr - Do"
      | ExprKind.Await value -> this.VisitExpr value
      | ExprKind.Throw value -> this.VisitExpr value
      | ExprKind.TemplateLiteral templateLiteral ->
        failwith "TODO: VisitExpr - TemplateLiteral"
      | ExprKind.TaggedTemplateLiteral(tag, template, throws) ->
        failwith "VisitExpr: VisitExpr - TaggedTemplateLiteral"

    member this.MaybeWalkExpr(e: option<Expr>) =
      match e with
      | Some(e) -> this.VisitExpr e
      | None -> ()

    default this.VisitStmt(s: Stmt) =
      match s.Kind with
      | Decl decl ->
        match decl.Kind with
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
                this.MaybeWalkTypeAnn typeParam.Constraint
                this.MaybeWalkTypeAnn typeParam.Default)
              typeParams
          | None -> ()
      | Expr expr -> this.VisitExpr expr
      | For(left, right, body) ->
        this.VisitPattern left
        this.VisitExpr right
        List.iter this.VisitStmt body.Stmts
      | Return exprOption -> this.MaybeWalkExpr exprOption

    default this.VisitTypeAnn(ta: TypeAnn) = ()

    member this.MaybeWalkTypeAnn(ota: option<TypeAnn>) =
      match ota with
      | Some(ta) -> this.VisitTypeAnn ta
      | None -> ()

    default this.VisitPattern(p: Syntax.Pattern) = ()
    default this.VisitBlock(b: Block) = List.iter this.VisitStmt b.Stmts
    default this.VisitScript(s: Script) = ()
