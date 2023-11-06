namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.TypeChecker.Unify
open Escalier.TypeChecker.Errors

module TypeVariable =
  let mutable next_id = 0

  let reset () = next_id <- 0

  let fresh () =
    let t =
      { Type.kind =
          TypeKind.TypeVar(
            { id = next_id
              instance = None
              bound = None }
          )
        provenance = None }

    next_id <- next_id + 1

    t

module rec Infer =
  type Binding = Type * bool
  type Assump = Map<string, Binding>

  type Env =
    { types: Map<string, Type>
      values: Map<string, Binding> }

  let infer_expr (env: Env) (e: Expr) : Result<Type, TypeError> =
    let provenance = Some(Provenance.Expr(e))

    let kind =
      result {
        match e.kind with
        | ExprKind.Identifer name ->
          match Map.tryFind name env.values with
          | Some((t, _)) -> return t.kind // How do we link back to the variable declaration?
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
          | NotEqual -> return TypeKind.Primitive(Primitive.Boolean)
          | And
          | Or ->
            let boolean =
              { kind = TypeKind.Primitive(Primitive.Boolean)
                provenance = provenance }

            do! unify left boolean
            do! unify right boolean

            return TypeKind.Primitive(Primitive.Boolean)
        | ExprKind.Unary(op, value) -> return! Error(NotImplemented)
        | ExprKind.Function(param_list, body) ->
          let mutable env = env

          let param_list =
            List.map
              (fun name ->
                let t = TypeVariable.fresh ()

                env <-
                  { env with
                      values = env.values.Add(name, (t, false)) }

                { pattern = Pattern.Identifier(name)
                  type_ = t
                  optional = false })
              param_list

          // TODO:
          // - find all return statements inside the body
          // - handle async/await
          // - handle throws
          let! return_type = infer_block env body

          let never =
            { kind = TypeKind.Keyword(KeywordType.Never)
              provenance = None }

          let func =
            { param_list = param_list
              return_type = return_type
              type_params = None
              throws = never }

          return TypeKind.Function(func)

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
    let mutable env' = env

    match b with
    | BlockOrExpr.Block block ->
      result {
        for stmt in block.stmts do
          match! infer_stmt env' stmt with
          | Some(assump) ->
            let mutable values = env'.values

            for KeyValue(name, binding) in assump do
              values <- values.Add(name, binding)

            env' <- { env' with values = values }
          | None -> ()

        let last = List.last block.stmts

        match last.kind with
        | StmtKind.Expr e ->
          match e.inferred_type with
          | Some(t) -> return t
          | None -> return! Error(NotInferred)
        | _ -> return! Error(NotImplemented)
      }
    | BlockOrExpr.Expr e -> infer_expr env e

  let infer_stmt (env: Env) (s: Stmt) : Result<option<Assump>, TypeError> =
    result {
      match s.kind with
      | StmtKind.Expr e ->
        infer_expr env e |> ignore
        return None
      | StmtKind.Decl(decl) ->
        match decl.kind with
        | DeclKind.TypeDecl(name, type_ann, type_params) -> return None
        | DeclKind.VarDecl(pattern, init, type_ann, is_declare) ->
          let! pat_bindings, pat_type = infer_pattern env pattern

          match init with
          | Some(init) ->
            let! init_type = infer_expr env init

            match type_ann with
            | Some(type_ann) -> return! Error(NotImplemented)
            | None ->
              do! unify init_type pat_type
              return Some(pat_bindings)
          | None -> return! Error(NotImplemented)
      // TODO: implement the rest
      | _ -> return! Error(NotImplemented)
    }

  let infer_pattern
    (env: Env)
    (pat: Escalier.Data.Syntax.Pattern)
    : Result<(Assump * Type), TypeError> =
    let mutable assump = Assump([])
    let rec foo x = x

    let rec infer_pattern_rec (pat: Escalier.Data.Syntax.Pattern) : Type =
      match pat.kind with
      | PatternKind.Identifier({ name = name; isMut = isMut }) ->
        let t =
          { Type.kind =
              TypeKind.TypeVar(
                { id = TypeVariable.next_id
                  instance = None
                  bound = None }
              )
            provenance = None }

        TypeVariable.next_id <- TypeVariable.next_id + 1

        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(name, (t, isMut))
        t
      | PatternKind.Literal(_, literal) ->
        { Type.kind = TypeKind.Literal(literal)
          provenance = None }
      | PatternKind.Object elems -> failwith "todo"
      | PatternKind.Tuple elems ->
        let elems' = List.map infer_pattern_rec elems

        { Type.kind = TypeKind.Tuple(elems')
          provenance = None }
      | PatternKind.Wildcard ->
        { Type.kind = TypeKind.Wildcard
          provenance = None }
      | PatternKind.Is(span, binding, isName, isMut) ->
        match Map.tryFind isName env.types with
        | Some(t) ->
          assump <- assump.Add(binding.name, (t, binding.isMut))
          t
        | None -> failwith "todo"

    let t = infer_pattern_rec pat
    Result.Ok((assump, t))

  let infer_script (env: Env) (script: Script) : Result<Env, TypeError> =
    let mutable env' = env

    result {
      for stmt in script do
        match! infer_stmt env' stmt with
        | Some(assump) ->
          let mutable values = env'.values

          for KeyValue(name, binding) in assump do
            values <- values.Add(name, binding)

          env' <- { env' with values = values }
        | None -> ()

      return env'
    }

// TODO: infer_script
// TODO: infer_module
// These will allow us to more thoroughly test infer_stmt and infer_pattern
// Both should return a modified Env so that we can check to see what
// values and types were defined
