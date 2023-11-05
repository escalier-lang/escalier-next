namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.TypeChecker.Unify
open Escalier.TypeChecker.Errors

module rec Infer =
  type Env =
    { types: Map<string, Type>
      values: Map<string, Type> }

  let infer_expr (env: Env) (e: Expr) : Result<Type, TypeError> =
    let provenance = Some(Provenance.Expr(e))

    let kind =
      result {
        match e.kind with
        | ExprKind.Identifer name ->
          match Map.tryFind name env.values with
          | Some(t) -> return t.kind // How do we link back to the variable declaration?
          | None -> return! Error(NotImplemented)
        | ExprKind.Literal lit -> return TypeKind.Literal(lit)
        | ExprKind.Object elems -> return! Error(NotImplemented)
        | ExprKind.Tuple elems ->
          let! elems' = List.traverseResultM (infer_expr env) elems

          return TypeKind.Tuple elems'
        // TODO: make this a statement rather than an expression
        | ExprKind.Assign(left, op, right) -> return! Error(NotImplemented)
        | ExprKind.Binary(left, op, right) ->
          let! left = infer_expr env left
          let! right = infer_expr env right

          match op with
          | BinaryOp.Add
          | Sub
          | Mul
          | Div
          | Mod
          | Exp ->
            let number =
              { kind = TypeKind.Primitive(Primitive.Number)
                provenance = provenance }

            do! unify left number
            do! unify right number

            return TypeKind.Primitive(Primitive.Number)
          | LessThan
          | LessThanOrEqual
          | GreaterThan
          | GreaterThanOrEqual ->
            let number =
              { kind = TypeKind.Primitive(Primitive.Number)
                provenance = provenance }

            do! unify left number
            do! unify right number

            return TypeKind.Primitive(Primitive.Boolean)
          | Equal
          | NotEqual ->
            do! unify left right

            return TypeKind.Primitive(Primitive.Boolean)
          | And
          | Or ->
            let boolean =
              { kind = TypeKind.Primitive(Primitive.Boolean)
                provenance = provenance }

            do! unify left boolean
            do! unify right boolean

            return TypeKind.Primitive(Primitive.Boolean)
        | ExprKind.Unary(op, value) -> return! Error(NotImplemented)
        | ExprKind.Function(param_list, body) -> return! Error(NotImplemented)
        | ExprKind.Call(callee, type_args, args, opt_chain, throws) ->
          return! Error(NotImplemented)
        | ExprKind.Index(target, index, opt_chain) ->
          return! Error(NotImplemented)
        | ExprKind.Member(target, name, opt_chain) ->
          return! Error(NotImplemented)
        | ExprKind.If(cond, thenBranch, elseBranch) ->
          let! cond' = infer_expr env cond
          let! then' = infer_block env thenBranch
          let! else' = infer_block env elseBranch

          let bool =
            { kind = TypeKind.Primitive(Primitive.Boolean)
              provenance = Some(Provenance.Expr(cond)) }

          do! unify cond' bool

          return TypeKind.Union [ then'; else' ]
        | ExprKind.Match(target, cases) -> return! Error(NotImplemented)
        | ExprKind.Try(body, catch, finally_) -> return! Error(NotImplemented)
        | ExprKind.Do(body) -> return! Error(NotImplemented)
        | ExprKind.Await(value) -> return! Error(NotImplemented)
        | ExprKind.Throw(value) -> return! Error(NotImplemented)
        | ExprKind.TemplateLiteral(_) -> return! Error(NotImplemented)
        | ExprKind.TaggedTemplateLiteral(tag, template, throws) ->
          return! Error(NotImplemented)
      }

    let t =
      Result.map
        (fun kind ->
          let t = { kind = kind; provenance = provenance }
          e.inferred_type <- Some(t)
          t)
        kind

    t

  let infer_block (env: Env) (b: BlockOrExpr) : Result<Type, TypeError> =
    match b with
    | BlockOrExpr.Block block ->
      result {
        // TODO: make this recursive
        for stmt in block.stmts do
          do! infer_stmt env stmt

        let last = List.last block.stmts

        match last.kind with
        | StmtKind.Expr e ->
          match e.inferred_type with
          | Some(t) -> return t
          | None -> return! Error(NotInferred)
        | _ -> return! Error(NotImplemented)
      }
    | BlockOrExpr.Expr e -> infer_expr env e

  let infer_stmt (env: Env) (s: Stmt) : Result<unit, TypeError> =
    result {
      match s.kind with
      | StmtKind.Expr e ->
        infer_expr env e |> ignore
        return ()
      // TODO: implement the rest
      | _ -> return! Error(NotImplemented)
    }
