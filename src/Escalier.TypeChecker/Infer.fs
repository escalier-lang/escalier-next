namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.Data
open Escalier.TypeChecker.Errors
open Escalier.TypeChecker.Unify

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
        | ExprKind.Function f -> return! infer_func env f
        | ExprKind.Call(callee, type_args, args, opt_chain, throws) ->
          return! Error(NotImplemented)
        | ExprKind.Index(target, index, opt_chain) ->
          return! Error(NotImplemented)
        | ExprKind.Member(target, name, opt_chain) ->
          return! Error(NotImplemented)
        | ExprKind.IfElse(cond, thenBranch, elseBranch) ->
          let! cond' = infer_expr env cond

          let bool =
            { kind = TypeKind.Primitive(Primitive.Boolean)
              provenance = Some(Provenance.Expr(cond)) }

          do! unify cond' bool

          let! then' = infer_block env thenBranch

          match elseBranch with
          | Some(elseBranch) ->
            let! else' = infer_block env elseBranch
            return TypeKind.Union [ then'; else' ]
          | None ->
            let undefined =
              { Type.kind = TypeKind.Literal(Literal.Undefined)
                provenance = None }

            do! unify then' undefined
            return TypeKind.Literal(Literal.Undefined)
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

  let infer_func
    (env: Env)
    (f: Escalier.Data.Syntax.Function)
    : Result<TypeKind, TypeError> =
    result {

      let mutable env = env

      let! param_list =
        List.traverseResultM
          (fun p ->
            result {
              let! type_ann_t =
                match p.typeAnn with
                | Some(typeAnn) -> infer_type_ann env typeAnn
                | None -> Result.Ok(TypeVariable.fresh ())

              let! assumps, param_t = infer_pattern env p.pattern

              do! unify param_t type_ann_t

              // TODO: add `non_generic` to env (or replace Env with Context)
              for KeyValue(name, binding) in assumps do
                env <-
                  { env with
                      values = env.values.Add(name, binding) }

              return
                { pattern = pattern_to_pattern p.pattern
                  type_ = type_ann_t
                  optional = false }
            })

          f.param_list

      // NOTE: infer_block returns a value to help with other uses
      // such as if-else and match expressions.
      let! _ = infer_block env f.body

      let returns = findReturns f.body

      let return_type =
        match returns with
        | [] ->
          { kind = TypeKind.Keyword(KeywordType.Never)
            provenance = None }
        | [ ret ] ->
          match ret.inferred_type with
          | Some(t) -> t
          | None -> failwith "todo"
        | many ->
          let types = many |> List.choose (fun (r: Expr) -> r.inferred_type)

          { kind = TypeKind.Union(types)
            provenance = None }

      let never =
        { kind = TypeKind.Keyword(KeywordType.Never)
          provenance = None }

      let func =
        { param_list = param_list
          return_type = return_type
          type_params = None
          throws = never }

      // TODO:
      // - handle async/await
      // - handle throws

      return TypeKind.Function(func)
    }

  let infer_type_ann (env: Env) (typeAnn: TypeAnn) : Result<Type, TypeError> =
    let kind: Result<TypeKind, TypeError> =
      result {
        match typeAnn.kind with
        | TypeAnnKind.Array elem ->
          let! elem = infer_type_ann env elem
          return TypeKind.Array(elem)
        | TypeAnnKind.BooleanLiteral value ->
          return TypeKind.Literal(Literal.Boolean value)
        | TypeAnnKind.NumberLiteral value ->
          return TypeKind.Literal(Literal.Number value)
        | TypeAnnKind.StringLiteral value ->
          return TypeKind.Literal(Literal.String value)
        | TypeAnnKind.Keyword keyword ->
          match keyword with
          | KeywordTypeAnn.Boolean ->
            return TypeKind.Primitive(Primitive.Boolean)
          | KeywordTypeAnn.Number -> return TypeKind.Primitive(Primitive.Number)
          | KeywordTypeAnn.String -> return TypeKind.Primitive(Primitive.String)
          | KeywordTypeAnn.Symbol -> return TypeKind.Primitive(Primitive.Symbol)
          | KeywordTypeAnn.Null -> return TypeKind.Literal(Literal.Null)
          | KeywordTypeAnn.Undefined ->
            return TypeKind.Literal(Literal.Undefined)
          | KeywordTypeAnn.Unknown ->
            return TypeKind.Keyword(KeywordType.Unknown)
          | KeywordTypeAnn.Never -> return TypeKind.Keyword(KeywordType.Never)
          | KeywordTypeAnn.Object -> return TypeKind.Keyword(KeywordType.Object)
        | TypeAnnKind.Object elems ->
          let! elems =
            List.traverseResultM
              (fun (elem: ObjTypeAnnElem) ->
                result {
                  // let! t = infer_type_ann env p.typeAnn
                  // let pattern = pattern_to_pattern p.pattern

                  // let elem: ObjTypeElem =
                  //   match elem with
                  //   | Callable f ->

                  // return
                  //   { pattern = pattern
                  //     type_ = t
                  //     optional = false }

                  return! Error(TypeError.NotImplemented)
                })
              elems

          return TypeKind.Object(elems)

        | TypeAnnKind.Tuple elems ->
          let! elems = List.traverseResultM (infer_type_ann env) elems
          return TypeKind.Tuple(elems)
        | TypeAnnKind.Union types ->
          let! types = List.traverseResultM (infer_type_ann env) types
          return TypeKind.Union(types)
        | TypeAnnKind.Intersection types ->
          let! types = List.traverseResultM (infer_type_ann env) types
          return TypeKind.Intersection types
        | TypeAnnKind.TypeRef(name, typeArgs) ->
          match typeArgs with
          | Some(typeArgs) ->
            let! typeArgs = List.traverseResultM (infer_type_ann env) typeArgs
            return TypeKind.TypeRef(name, Some(typeArgs), None)
          | None -> return TypeKind.TypeRef(name, None, None)
        | TypeAnnKind.Function functionType ->
          let! return_type = infer_type_ann env functionType.return_type

          let! throws =
            match functionType.throws with
            | Some(throws) -> infer_type_ann env throws
            | None ->
              Result.Ok(
                { Type.kind = TypeKind.Keyword(KeywordType.Never)
                  provenance = None }
              )

          let! param_list =
            List.traverseResultM
              (fun p ->
                result {
                  let! t = infer_type_ann env p.typeAnn
                  let pattern = pattern_to_pattern p.pattern

                  return
                    { pattern = pattern
                      type_ = t
                      optional = false }
                })
              functionType.params_

          let f =
            { param_list = param_list
              return_type = return_type
              type_params = None
              throws = throws }

          return TypeKind.Function(f)
        | TypeAnnKind.Keyof target ->
          return! infer_type_ann env target |> Result.map TypeKind.KeyOf
        | TypeAnnKind.Rest target ->
          return! infer_type_ann env target |> Result.map TypeKind.Rest
        | TypeAnnKind.Typeof target -> return! Error(TypeError.NotImplemented) // TODO: add Typeof to TypeKind
        | TypeAnnKind.Index(target, index) ->
          let! target = infer_type_ann env target
          let! index = infer_type_ann env index
          return TypeKind.Index(target, index)
        | TypeAnnKind.Condition conditionType ->
          let! check = infer_type_ann env conditionType.check
          let! extends = infer_type_ann env conditionType.extends
          let! trueType = infer_type_ann env conditionType.true_type
          let! falseType = infer_type_ann env conditionType.false_type
          return TypeKind.Condition(check, extends, trueType, falseType)
        | TypeAnnKind.Match matchType -> return! Error(TypeError.NotImplemented) // TODO
        | TypeAnnKind.Infer name -> return TypeKind.Infer name
        | TypeAnnKind.Wildcard -> return TypeKind.Wildcard
        | TypeAnnKind.Binary(left, op, right) ->
          let! left = infer_type_ann env left
          let! right = infer_type_ann env right
          return TypeKind.Binary(left, op, right)
      }

    let t: Result<Type, TypeError> =
      Result.map
        (fun kind ->
          let t = { kind = kind; provenance = None }
          typeAnn.inferred_type <- Some(t)
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
        | _ ->
          return
            { Type.kind = TypeKind.Literal(Literal.Undefined)
              provenance = None }
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
      | StmtKind.Return e ->
        match e with
        | Some(e) -> infer_expr env e |> ignore
        | None -> ()

        return None
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

  let rec pattern_to_pattern (pat: Escalier.Data.Syntax.Pattern) : Pattern =
    match pat.kind with
    | PatternKind.Identifier({ name = name; isMut = isMut }) ->
      Pattern.Identifier(name)
    | PatternKind.Is(span, bindingIdent, isName, isMut) ->
      Pattern.Is(bindingIdent, isName)
    | PatternKind.Object elems -> failwith "todo" // TODO
    | PatternKind.Tuple elems ->
      Pattern.Tuple(List.map pattern_to_pattern elems)
    | PatternKind.Wildcard -> Pattern.Wildcard
    | PatternKind.Literal(span, lit) -> Pattern.Literal(lit)

  type ReturnVisitor() =
    inherit Visitor.SyntaxVisitor()

    let mutable returnValues = []

    member this.ReturnValues = returnValues

    override this.VisitExpr(expr: Expr) =
      match expr.kind with
      | ExprKind.Function f -> () // Skips nested functions
      | _ -> base.VisitExpr(expr)

    override this.VisitStmt(stmt: Stmt) =
      match stmt.kind with
      | StmtKind.Return value ->
        match value with
        | Some(value) -> returnValues <- value :: returnValues
        | None -> ()
      | _ -> base.VisitStmt(stmt)

  let findReturns (block: BlockOrExpr) : Expr list =
    let visitor = ReturnVisitor()

    match block with
    | BlockOrExpr.Block b -> visitor.VisitBlock(b)
    | BlockOrExpr.Expr e -> visitor.VisitExpr(e)

    visitor.ReturnValues

// TODO: infer_module
// These will allow us to more thoroughly test infer_stmt and infer_pattern
// Both should return a modified Env so that we can check to see what
// values and types were defined
