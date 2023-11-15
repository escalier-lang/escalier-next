namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.Data

open Env
open Errors
open Unify

module rec Infer =
  let inferExpr (env: Env) (e: Expr) : Result<Type, TypeError> =
    let provenance = Some(Provenance.Expr(e))

    let kind =
      result {
        match e.Kind with
        | ExprKind.Identifier name ->
          match Map.tryFind name env.Values with
          | Some((t, _)) -> return (fresh env t).Kind // How do we link back to the variable declaration?
          | None -> return! Error(NotImplemented)
        | ExprKind.Literal lit -> return TypeKind.Literal(lit)
        | ExprKind.Object elems -> return! Error(NotImplemented)
        | ExprKind.Tuple elems ->
          let! elems' = List.traverseResultM (inferExpr env) elems

          return TypeKind.Tuple elems'
        // TODO: make this a statement rather than an expression
        | ExprKind.Assign(left, op, right) -> return! Error(NotImplemented)
        | ExprKind.Binary(left, op, right) ->
          let! left = inferExpr env left
          let! right = inferExpr env right

          match op with
          | BinaryOp.Add
          | Sub
          | Mul
          | Div
          | Mod
          | Exp ->
            let number =
              { Kind = TypeKind.Primitive(Primitive.Number)
                Provenance = provenance }

            do! unify left number
            do! unify right number

            return TypeKind.Primitive(Primitive.Number)
          | LessThan
          | LessThanOrEqual
          | GreaterThan
          | GreaterThanOrEqual ->
            let number =
              { Kind = TypeKind.Primitive(Primitive.Number)
                Provenance = provenance }

            do! unify left number
            do! unify right number

            return TypeKind.Primitive(Primitive.Boolean)
          | Equal
          | NotEqual -> return TypeKind.Primitive(Primitive.Boolean)
          | And
          | Or ->
            let boolean =
              { Kind = TypeKind.Primitive(Primitive.Boolean)
                Provenance = provenance }

            do! unify left boolean
            do! unify right boolean

            return TypeKind.Primitive(Primitive.Boolean)
        | ExprKind.Unary(op, value) -> return! Error(NotImplemented)
        | ExprKind.Function func -> return! inferFunc env func
        | ExprKind.Call call ->
          let! callee = inferExpr env call.Callee

          let! result, throws = unifyCall call.Args None callee (inferExpr env)

          // The call's `throw` field is initialized to None.  We update
          // if we've determine that the function being called can throw
          // something other than `never`.  We use this information to
          // determine what exceptions the caller throws.
          if throws.Kind <> TypeKind.Keyword(KeywordType.Never) then
            call.Throws <- Some(throws)

          return result.Kind
        | ExprKind.Index(target, index, opt_chain) ->
          return! Error(NotImplemented)
        | ExprKind.Member(target, name, opt_chain) ->
          return! Error(NotImplemented)
        | ExprKind.IfElse(cond, thenBranch, elseBranch) ->
          let! cond' = inferExpr env cond

          let bool =
            { Kind = TypeKind.Primitive(Primitive.Boolean)
              Provenance = Some(Provenance.Expr(cond)) }

          do! unify cond' bool

          let! then' = inferBlock env thenBranch

          match elseBranch with
          | Some(elseBranch) ->
            let! else' = inferBlock env elseBranch
            return TypeKind.Union [ then'; else' ]
          | None ->
            let undefined =
              { Type.Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }

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
          let t = { Kind = kind; Provenance = provenance }
          e.InferredType <- Some(t)
          t)
        kind

    t

  let inferFunc
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
                match p.TypeAnn with
                | Some(typeAnn) -> inferTypeAnn env typeAnn
                | None -> Result.Ok(TypeVariable.newTypeVar None)

              let! assumps, param_t = inferPattern env p.Pattern

              do! unify param_t type_ann_t

              for KeyValue(name, binding) in assumps do
                let nonGeneric =
                  match fst(binding).Kind with
                  | TypeVar { Id = id } -> env.NonGeneric.Add(id)
                  | _ -> env.NonGeneric

                env <-
                  { env with
                      NonGeneric = nonGeneric
                      Values = env.Values.Add(name, binding) }

              return
                { Pattern = patternToPattern p.Pattern
                  Type = type_ann_t
                  Optional = false }
            })

          f.Sig.ParamList

      // NOTE: infer_block returns a value to help with other uses
      // such as if-else and match expressions.
      let! _ = inferBlock env f.Body

      let returnType =
        match f.Body with
        | BlockOrExpr.Block(block) ->
          match findReturns block with
          | [] ->
            { Kind = TypeKind.Keyword(KeywordType.Never)
              Provenance = None }
          | [ ret ] ->
            match ret.InferredType with
            | Some(t) -> t
            | None -> failwith "inferFunc - return type not inferred"
          | many ->
            let types = many |> List.choose (fun (r: Expr) -> r.InferredType)

            { Kind = TypeKind.Union(types)
              Provenance = None }
        | BlockOrExpr.Expr(expr) ->
          match expr.InferredType with
          | Some(t) -> t
          | None -> failwith "inferFunc - expr type not inferred"


      let never =
        { Kind = TypeKind.Keyword(KeywordType.Never)
          Provenance = None }

      let! type_params =
        match f.Sig.TypeParams with
        | Some(typeParams) ->
          List.traverseResultM (inferTypeParam env) typeParams
          |> Result.map Some
        | None -> Ok None

      let func =
        { ParamList = param_list
          ReturnType = returnType
          TypeParams = type_params
          Throws = never }

      // TODO:
      // - handle async/await
      // - handle throws

      return TypeKind.Function(func)
    }

  let inferTypeParam
    (env: Env)
    (tp: Syntax.TypeParam)
    : Result<TypeParam, TypeError> =
    result {
      let! constraint_ =
        match tp.Constraint with
        | Some(c) -> inferTypeAnn env c |> Result.map Some
        | None -> Ok None

      let! default_ =
        match tp.Default with
        | Some(d) -> inferTypeAnn env d |> Result.map Some
        | None -> Ok None

      return
        { Name = tp.Name
          Constraint = constraint_
          Default = default_ }
    }

  let inferTypeAnn (env: Env) (typeAnn: TypeAnn) : Result<Type, TypeError> =
    let kind: Result<TypeKind, TypeError> =
      result {
        match typeAnn.Kind with
        | TypeAnnKind.Array elem ->
          let! elem = inferTypeAnn env elem
          return TypeKind.Array(elem)
        | TypeAnnKind.Literal lit -> return TypeKind.Literal(lit)
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
          let! elems = List.traverseResultM (inferTypeAnn env) elems
          return TypeKind.Tuple(elems)
        | TypeAnnKind.Union types ->
          let! types = List.traverseResultM (inferTypeAnn env) types
          return TypeKind.Union(types)
        | TypeAnnKind.Intersection types ->
          let! types = List.traverseResultM (inferTypeAnn env) types
          return TypeKind.Intersection types
        | TypeAnnKind.TypeRef(name, typeArgs) ->
          match typeArgs with
          | Some(typeArgs) ->
            let! typeArgs = List.traverseResultM (inferTypeAnn env) typeArgs

            return
              { Name = name
                TypeArgs = Some(typeArgs)
                Scheme = None }
              |> TypeKind.TypeRef
          | None ->
            return
              { Name = name
                TypeArgs = None
                Scheme = None }
              |> TypeKind.TypeRef
        | TypeAnnKind.Function functionType ->
          let! return_type = inferTypeAnn env functionType.ReturnType

          let! throws =
            match functionType.Throws with
            | Some(throws) -> inferTypeAnn env throws
            | None ->
              Result.Ok(
                { Type.Kind = TypeKind.Keyword(KeywordType.Never)
                  Provenance = None }
              )

          let! param_list =
            List.traverseResultM
              (fun p ->
                result {
                  let! t = inferTypeAnn env p.TypeAnn
                  let pattern = patternToPattern p.Pattern

                  return
                    { Pattern = pattern
                      Type = t
                      Optional = false }
                })
              functionType.ParamList

          let f =
            { ParamList = param_list
              ReturnType = return_type
              TypeParams = None
              Throws = throws }

          return TypeKind.Function(f)
        | TypeAnnKind.Keyof target ->
          return! inferTypeAnn env target |> Result.map TypeKind.KeyOf
        | TypeAnnKind.Rest target ->
          return! inferTypeAnn env target |> Result.map TypeKind.Rest
        | TypeAnnKind.Typeof target -> return! Error(TypeError.NotImplemented) // TODO: add Typeof to TypeKind
        | TypeAnnKind.Index(target, index) ->
          let! target = inferTypeAnn env target
          let! index = inferTypeAnn env index
          return TypeKind.Index(target, index)
        | TypeAnnKind.Condition conditionType ->
          let! check = inferTypeAnn env conditionType.Check
          let! extends = inferTypeAnn env conditionType.Extends
          let! trueType = inferTypeAnn env conditionType.TrueType
          let! falseType = inferTypeAnn env conditionType.FalseType
          return TypeKind.Condition(check, extends, trueType, falseType)
        | TypeAnnKind.Match matchType -> return! Error(TypeError.NotImplemented) // TODO
        | TypeAnnKind.Infer name -> return TypeKind.Infer name
        | TypeAnnKind.Wildcard -> return TypeKind.Wildcard
        | TypeAnnKind.Binary(left, op, right) ->
          let! left = inferTypeAnn env left
          let! right = inferTypeAnn env right
          return TypeKind.Binary(left, op, right)
      }

    let t: Result<Type, TypeError> =
      Result.map
        (fun kind ->
          let t = { Kind = kind; Provenance = None }
          typeAnn.InferredType <- Some(t)
          t)
        kind

    t

  let inferBlock (env: Env) (b: BlockOrExpr) : Result<Type, TypeError> =
    let mutable env' = env

    match b with
    | BlockOrExpr.Block block ->
      result {
        for stmt in block.Stmts do
          match! inferStmt env' stmt with
          | Some(StmtResult.Bindings(assump)) ->
            let mutable values = env'.Values

            for KeyValue(name, binding) in assump do
              values <- values.Add(name, binding)

            env' <- { env' with Values = values }
          | Some(StmtResult.Scheme(name, scheme)) ->
            env' <-
              { env' with
                  Schemes = env'.Schemes.Add(name, scheme) }
          | None -> ()

        let last = List.last block.Stmts

        match last.Kind with
        | StmtKind.Expr e ->
          match e.InferredType with
          | Some(t) -> return t
          | None -> return! Error(NotInferred)
        | _ ->
          return
            { Type.Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }
      }
    | BlockOrExpr.Expr e -> inferExpr env e

  type StmtResult =
    | Bindings of BindingAssump
    | Scheme of SchemeAssump

  let inferStmt (env: Env) (s: Stmt) : Result<option<StmtResult>, TypeError> =
    result {
      match s.Kind with
      | StmtKind.Expr e ->
        inferExpr env e |> ignore
        return None
      | StmtKind.Decl(decl) ->
        match decl.Kind with
        | DeclKind.TypeDecl(name, type_ann, type_params) ->
          let! t = inferTypeAnn env type_ann

          let scheme: Scheme =
            { TypeParams = []
              Type = t
              IsTypeParam = false }

          return Some(StmtResult.Scheme(name, scheme))
        | DeclKind.VarDecl(pattern, init, type_ann, is_declare) ->
          let! pat_bindings, pat_type = inferPattern env pattern

          match init with
          | Some(init) ->
            let! init_type = inferExpr env init

            match type_ann with
            | Some(type_ann) -> return! Error(NotImplemented)
            | None ->
              do! unify init_type pat_type
              return Some(StmtResult.Bindings(pat_bindings))
          | None -> return! Error(NotImplemented)
      | StmtKind.Return e ->
        match e with
        | Some(e) -> inferExpr env e |> ignore
        | None -> ()

        return None
      | _ -> return! Error(NotImplemented)
    }

  let inferPattern
    (env: Env)
    (pat: Syntax.Pattern)
    : Result<(BindingAssump * Type), TypeError> =
    let mutable assump = BindingAssump([])

    let rec inferPatternRec (pat: Syntax.Pattern) : Type =
      match pat.Kind with
      | PatternKind.Identifier({ Name = name; IsMut = isMut }) ->
        let t =
          { Type.Kind =
              TypeKind.TypeVar(
                { Id = TypeVariable.nextId
                  Instance = None
                  Bound = None }
              )
            Provenance = None }

        TypeVariable.nextId <- TypeVariable.nextId + 1

        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(name, (t, isMut))
        t
      | PatternKind.Literal(_, literal) ->
        { Type.Kind = TypeKind.Literal(literal)
          Provenance = None }
      | PatternKind.Object elems -> failwith "TODO: inferPattern - Object"
      | PatternKind.Tuple elems ->
        let elems' = List.map inferPatternRec elems

        { Type.Kind = TypeKind.Tuple(elems')
          Provenance = None }
      | PatternKind.Wildcard ->
        { Type.Kind = TypeKind.Wildcard
          Provenance = None }
      | PatternKind.Is(span, binding, isName, isMut) ->
        match Map.tryFind isName env.Schemes with
        | Some(scheme) ->
          assump <- assump.Add(binding.Name, (scheme.Type, binding.IsMut))
          scheme.Type
        | None -> failwith "TODO: inferPattern - Is, no scheme found"

    let t = inferPatternRec pat
    Result.Ok((assump, t))

  let inferScript (env: Env) (script: Script) : Result<Env, TypeError> =
    let mutable env' = env

    result {
      for stmt in script do
        match! inferStmt env' stmt with
        | Some(StmtResult.Bindings(assump)) ->
          let mutable values = env'.Values

          for KeyValue(name, binding) in assump do
            let (t, isMut) = binding
            let t = prune t

            match t.Kind with
            | TypeKind.Function f ->
              let t =
                { t with
                    Kind = generalizeFunc f |> TypeKind.Function }

              let binding = (t, isMut)
              values <- values.Add(name, binding)
            | _ -> values <- values.Add(name, binding)

          env' <- { env' with Values = values }
        | Some(StmtResult.Scheme(name, scheme)) ->
          env' <-
            { env' with
                Schemes = env'.Schemes.Add(name, scheme) }
        | None -> ()

      return env'
    }

  let rec patternToPattern (pat: Syntax.Pattern) : Pattern =
    match pat.Kind with
    | PatternKind.Identifier({ Name = name; IsMut = isMut }) ->
      Pattern.Identifier(name)
    | PatternKind.Is(span, bindingIdent, isName, isMut) ->
      Pattern.Is(bindingIdent, isName)
    | PatternKind.Object elems -> failwith "todo" // TODO
    | PatternKind.Tuple elems -> Pattern.Tuple(List.map patternToPattern elems)
    | PatternKind.Wildcard -> Pattern.Wildcard
    | PatternKind.Literal(span, lit) -> Pattern.Literal(lit)

  type ReturnVisitor() =
    inherit Visitor.SyntaxVisitor()

    let mutable returnValues = []

    member this.ReturnValues = returnValues

    override this.VisitExpr(expr: Expr) =
      match expr.Kind with
      | ExprKind.Function f -> () // Skips nested functions
      | _ -> base.VisitExpr(expr)

    override this.VisitStmt(stmt: Stmt) =
      match stmt.Kind with
      | StmtKind.Return value ->
        match value with
        | Some(value) -> returnValues <- value :: returnValues
        | None -> ()
      | _ -> base.VisitStmt(stmt)

  let findReturns (block: Block) : Expr list =
    let visitor = ReturnVisitor()

    visitor.VisitBlock(block)

    visitor.ReturnValues


  let generalizeFunc (func: Type.Function) : Type.Function =
    let mutable mapping: Map<int, string> = Map.empty

    let generalize (t: Type) : Type =
      let t = prune t

      match t.Kind with
      | TypeVar { Id = id } ->
        let name =
          match Map.tryFind id mapping with
          | Some(name) -> name
          | None ->
            let name = 65 + mapping.Count |> char |> string
            mapping <- Map.add id name mapping
            name

        { Type.Kind =
            { Name = name
              TypeArgs = None
              Scheme = None }
            |> TypeRef
          Provenance = None }
      | _ -> t

    let folder = Folder.TypeFolder(generalize)

    let paramList =
      List.map
        (fun (p: FuncParam) ->
          { p with
              Type = folder.FoldType(p.Type) })
        func.ParamList

    // TODO: FoldType(func.throws)?
    let returnType = folder.FoldType(func.ReturnType)

    let values = mapping.Values |> List.ofSeq

    let mutable typeParams: list<TypeParam> =
      List.map
        (fun name ->
          { Name = name
            Constraint = None
            Default = None })
        values

    Option.iter
      (fun tps ->
        for tp in tps do
          typeParams <- typeParams @ [ tp ])
      func.TypeParams

    { func with
        TypeParams = if typeParams.IsEmpty then None else Some(typeParams)
        ParamList = paramList
        ReturnType = returnType }

  let fresh (env: Env) (t: Type) =
    let mutable mapping: Map<int, Type> = Map.empty

    let foldFn (t: Type) : Type =
      match (prune t).Kind with
      | TypeVar { Id = id } ->
        if env.NonGeneric.Contains(id) then
          t
        else
          match Map.tryFind id mapping with
          | None ->
            let newTypeVar = TypeVariable.newTypeVar None
            mapping <- Map.add id newTypeVar mapping
            newTypeVar
          | Some(var) -> var
      | _ -> t

    // TODO: Fix TypeFolder so that we don't get new type variables
    // let folder = Folder.TypeFolder(foldFn)
    // folder.FoldType(t)

    t

// TODO: infer_module
// These will allow us to more thoroughly test infer_stmt and infer_pattern
// Both should return a modified Env so that we can check to see what
// values and types were defined
