namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type

open Env
open Error
open Poly
open Visitor
open Unify

module rec Infer =
  let rec patternToPattern (pat: Syntax.Pattern) : Pattern =
    match pat.Kind with
    | PatternKind.Identifier({ Name = name; IsMut = isMut }) ->
      Pattern.Identifier(name)
    | PatternKind.Is(span, bindingIdent, isName, isMut) ->
      Pattern.Is(bindingIdent, isName)
    | PatternKind.Object elems ->
      Pattern.Object(
        List.map
          (fun (elem: Syntax.ObjPatElem) ->
            match elem with
            | Syntax.ObjPatElem.KeyValuePat(span, key, value, init) ->
              ObjPatElem.KeyValuePat(key, patternToPattern value, init)
            | Syntax.ObjPatElem.ShorthandPat(span, name, init, isMut) ->
              // TODO: isMut
              ObjPatElem.ShorthandPat(name, init)
            | Syntax.ObjPatElem.RestPat(span, target, isMut) ->
              ObjPatElem.RestPat(patternToPattern target))
          elems
      )
    | PatternKind.Tuple elems ->
      Pattern.Tuple(List.map (patternToPattern >> Some) elems)
    | PatternKind.Wildcard -> Pattern.Wildcard
    | PatternKind.Literal(span, lit) -> Pattern.Literal(lit)

  ///Computes the type of the expression given by node.
  ///The type of the node is computed in the context of the
  ///supplied type environment env. Data types can be introduced into the
  ///language simply by having a predefined set of identifiers in the initial
  ///environment. environment; this way there is no need to change the syntax or, more
  ///importantly, the type-checking program when extending the language.
  let inferExpr (ctx: Ctx) (env: Env) (expr: Expr) : Result<Type, TypeError> =
    let r =
      result {
        match expr.Kind with
        | ExprKind.Identifier(name) -> return! env.GetType name
        | ExprKind.Literal(literal) ->
          return
            { Type.Kind = TypeKind.Literal(literal)
              Provenance = None }
        | ExprKind.Call call ->
          let! callee = inferExpr ctx env call.Callee

          let! result, throws =
            unifyCall ctx env inferExpr call.Args None callee

          // TODO: handle throws

          return result
        | ExprKind.Binary(op, left, right) ->
          let! funTy = env.GetType op

          let! result, throws =
            unifyCall ctx env inferExpr [ left; right ] None funTy

          // TODO: handle throws

          return result
        | ExprKind.Function f ->
          let mutable newEnv = env

          // TODO: move this up so that we can reference any type params in the
          // function params, body, return or throws
          let! typeParams =
            match f.Sig.TypeParams with
            | Some(typeParams) ->
              List.traverseResultM
                (fun typeParam ->
                  result {
                    let! typeParam = inferTypeParam newEnv typeParam

                    let unknown =
                      { Kind = TypeKind.Keyword Keyword.Unknown
                        Provenance = None }

                    let scheme =
                      { TypeParams = None
                        Type =
                          match typeParam.Constraint with
                          | Some c -> c
                          | None -> unknown
                        IsTypeParam = true }

                    newEnv <- newEnv.AddScheme typeParam.Name scheme

                    return typeParam
                  })
                typeParams
              |> Result.map Some
            | None -> Ok None

          let! paramList =
            List.traverseResultM
              (fun (param: Syntax.FuncParam<option<TypeAnn>>) ->
                result {
                  let! paramType =
                    match param.TypeAnn with
                    | Some(typeAnn) -> inferTypeAnn newEnv typeAnn
                    | None -> Result.Ok(ctx.FreshTypeVar None)

                  let! assumps, patternType =
                    inferPattern ctx newEnv param.Pattern

                  do! unify ctx newEnv patternType paramType

                  for KeyValue(name, binding) in assumps do
                    // TODO: update `Env.types` to store `Binding`s insetad of `Type`s
                    newEnv <- newEnv.AddValue name binding

                  return
                    { Pattern = patternToPattern param.Pattern
                      Type = paramType
                      Optional = false }
                })
              f.Sig.ParamList

          inferBlockOrExpr ctx newEnv f.Body |> ignore

          let retExprs = findReturns f

          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          // TODO: unify body return type with return type annotation
          // if it exists
          let! retType =
            result {
              match retExprs with
              | [] -> return undefined
              | [ expr ] ->
                match expr.InferredType with
                | Some(t) -> return t
                | None -> return! Error(TypeError.SemanticError "")
              | exprs ->
                let! types =
                  List.traverseResultM
                    (fun (expr: Expr) ->
                      match expr.InferredType with
                      | Some(t) -> Ok t
                      | None -> Error(TypeError.SemanticError ""))
                    exprs

                return
                  { Kind = TypeKind.Union types
                    Provenance = None }
            }

          let! retType =
            result {
              match f.Sig.ReturnType with
              | Some(sigRetType) ->
                let! sigRetType = inferTypeAnn newEnv sigRetType
                do! unify ctx newEnv retType sigRetType
                return sigRetType
              | None -> return retType
            }

          return makeFunctionType typeParams paramList retType
        | ExprKind.Tuple elems ->
          let! elems = List.traverseResultM (inferExpr ctx env) elems

          return
            { Type.Kind = TypeKind.Tuple(elems)
              Provenance = None }
        | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
          let! conditionTy = inferExpr ctx env condition

          let! thenBranchTy =
            inferBlockOrExpr ctx env (thenBranch |> BlockOrExpr.Block)

          let! elseBranchTy =
            Option.traverseResult (inferBlockOrExpr ctx env) elseBranch

          do! unify ctx env conditionTy boolType

          return
            match elseBranchTy with
            | Some(elseBranchTy) -> union [ thenBranchTy; elseBranchTy ]
            | None ->
              { Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }
        | ExprKind.Object elems ->
          let mutable spreadTypes = []

          let! elems =
            List.traverseResultM
              (fun (elem: ObjElem) ->
                result {
                  match elem with
                  | ObjElem.Property(_span, key, value) ->
                    let! t = inferExpr ctx env value

                    return
                      Some(
                        Property
                          { Name = key
                            Optional = false
                            Readonly = false
                            Type = t }
                      )
                  | ObjElem.Shorthand(_span, key) ->
                    let! value = env.GetType key

                    return
                      Some(
                        Property
                          { Name = key
                            Optional = false
                            Readonly = false
                            Type = value }
                      )
                  | ObjElem.Spread(span, value) ->
                    let! t = inferExpr ctx env value
                    spreadTypes <- t :: spreadTypes
                    return None
                })
              elems

          let elems = elems |> List.choose id

          let objType =
            { Kind = TypeKind.Object(elems)
              Provenance = None }

          match spreadTypes with
          | [] -> return objType
          | _ ->
            return
              { Kind = TypeKind.Intersection([ objType ] @ spreadTypes)
                Provenance = None }
        | ExprKind.Member(obj, prop, optChain) ->
          let! objType = inferExpr ctx env obj

          // TODO: handle optional chaining
          // TODO: lookup properties on object type
          return getPropType env objType prop optChain
        | _ ->
          printfn "expr.Kind = %A" expr.Kind

          return!
            Error(
              TypeError.NotImplemented "TODO: finish implementing infer_expr"
            )
      }

    Result.map
      (fun t ->
        expr.InferredType <- Some(t)
        t)
      r

  let getPropType (env: Env) (t: Type) (name: string) (optChain: bool) : Type =
    let t = prune t

    match t.Kind with
    | TypeKind.Object elems ->
      let elems =
        List.choose
          (fun (elem: ObjTypeElem) ->
            match elem with
            | Property p -> Some(p.Name, p)
            | _ -> None)
          elems
        |> Map.ofList

      match elems.TryFind name with
      | Some(p) ->
        match p.Optional with
        | true ->
          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          union [ p.Type; undefined ]
        | false -> p.Type
      | None -> failwithf $"Property {name} not found"
    | TypeKind.TypeRef { Name = typeRefName
                         Scheme = scheme
                         TypeArgs = typeArgs } ->
      match scheme with
      | Some scheme ->
        getPropType env (env.ExpandScheme scheme typeArgs) name optChain
      | None ->
        match env.Schemes.TryFind typeRefName with
        | Some scheme ->
          getPropType env (env.ExpandScheme scheme typeArgs) name optChain
        | None -> failwithf $"{name} not in scope"
    | TypeKind.Union types ->
      let undefinedTypes, definedTypes =
        List.partition
          (fun t -> t.Kind = TypeKind.Literal(Literal.Undefined))
          types

      if undefinedTypes.IsEmpty then
        failwith "TODO: lookup member on union type"
      else if not optChain then
        failwith "Can't lookup property on undefined"
      else
        match definedTypes with
        | [ t ] ->
          let t = getPropType env t name optChain

          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          union [ t; undefined ]
        | _ -> failwith "TODO: lookup member on union type"

    // TODO: intersection types
    // TODO: union types
    | _ -> failwith $"TODO: lookup member on type - {t}"

  let inferBlockOrExpr
    (ctx: Ctx)
    (env: Env)
    (blockOrExpr: BlockOrExpr)
    : Result<Type, TypeError> =
    result {
      let mutable newEnv = env

      match blockOrExpr with
      | BlockOrExpr.Block({ Stmts = stmts }) ->
        for stmt in stmts do
          let! stmtEnv = inferStmt ctx newEnv stmt false
          newEnv <- stmtEnv

        let undefined =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Provenance = None }

        match List.tryLast stmts with
        | Some(stmt) ->
          match stmt.Kind with
          | StmtKind.Expr expr ->
            match expr.InferredType with
            | Some(t) -> return t
            | None -> return undefined
          | _ -> return undefined
        | _ -> return undefined
      | BlockOrExpr.Expr expr -> return! inferExpr ctx newEnv expr
    }

  let inferTypeAnn (env: Env) (typeAnn: TypeAnn) : Result<Type, TypeError> =
    let kind: Result<TypeKind, TypeError> =
      result {
        match typeAnn.Kind with
        | TypeAnnKind.Array elem ->
          let! elem = inferTypeAnn env elem
          return TypeKind.Array elem
        | TypeAnnKind.Literal lit -> return TypeKind.Literal(lit)
        | TypeAnnKind.Keyword keyword ->
          match keyword with
          | KeywordTypeAnn.Boolean ->
            return TypeKind.Primitive Primitive.Boolean
          | KeywordTypeAnn.Number -> return TypeKind.Primitive Primitive.Number
          | KeywordTypeAnn.String -> return TypeKind.Primitive Primitive.String
          | KeywordTypeAnn.Symbol -> return TypeKind.Primitive Primitive.Symbol
          | KeywordTypeAnn.Null -> return TypeKind.Literal(Literal.Null)
          | KeywordTypeAnn.Undefined ->
            return TypeKind.Literal(Literal.Undefined)
          | KeywordTypeAnn.Unknown -> return TypeKind.Keyword Keyword.Unknown
          | KeywordTypeAnn.Never -> return TypeKind.Keyword Keyword.Never
          | KeywordTypeAnn.Object -> return TypeKind.Keyword Keyword.Object
        | TypeAnnKind.Object elems ->
          let! elems =
            List.traverseResultM
              (fun (elem: ObjTypeAnnElem) ->
                result {
                  match elem with
                  | ObjTypeAnnElem.Property { Name = name
                                              TypeAnn = typeAnn
                                              Optional = optional
                                              Readonly = readonly } ->
                    let! t = inferTypeAnn env typeAnn

                    return
                      Property
                        { Name = name
                          Type = t
                          Optional = optional
                          Readonly = readonly }
                  | ObjTypeAnnElem.Callable ``function`` ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Constructor ``function`` ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Method(name, isMut, ``type``) ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Getter(name, returnType, throws) ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Setter(name, param, throws) ->
                    return! Error(TypeError.NotImplemented "todo")
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
          // TODO: look up the scheme for this
          match env.Schemes.TryFind(name) with
          | Some(scheme) ->

            let scheme =
              match scheme.IsTypeParam with
              | true -> None
              | false -> Some(scheme)

            match typeArgs with
            | Some(typeArgs) ->
              let! typeArgs = List.traverseResultM (inferTypeAnn env) typeArgs

              return
                { Name = name
                  TypeArgs = Some(typeArgs)
                  Scheme = scheme }
                |> TypeKind.TypeRef
            | None ->
              return
                { Name = name
                  TypeArgs = None
                  Scheme = scheme }
                |> TypeKind.TypeRef
          | None ->
            return! Error(TypeError.SemanticError $"{name} is not in scope")
        | TypeAnnKind.Function functionType ->
          let! returnType = inferTypeAnn env functionType.ReturnType

          let! throws =
            match functionType.Throws with
            | Some(throws) -> inferTypeAnn env throws
            | None ->
              Result.Ok(
                { Type.Kind = TypeKind.Keyword Keyword.Never
                  Provenance = None }
              )

          let! paramList =
            List.traverseResultM
              (fun (p: FuncParam<TypeAnn>) ->
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
            { TypeParams = None // TODO: type params
              ParamList = paramList
              Return = returnType
              Throws = throws }

          return TypeKind.Function(f)
        | TypeAnnKind.Keyof target ->
          return! inferTypeAnn env target |> Result.map TypeKind.KeyOf
        | TypeAnnKind.Rest target ->
          return! inferTypeAnn env target |> Result.map TypeKind.Rest
        | TypeAnnKind.Typeof target ->
          return! Error(TypeError.NotImplemented "TODO: inferTypeAnn - Typeof") // TODO: add Typeof to TypeKind
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
        | TypeAnnKind.Match matchType ->
          return! Error(TypeError.NotImplemented "TODO: inferTypeAnn - Match") // TODO
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

  let inferPattern
    (ctx: Ctx)
    (env: Env)
    (pat: Syntax.Pattern)
    : Result<BindingAssump * Type, TypeError> =
    let mutable assump = BindingAssump([])

    let rec infer_pattern_rec (pat: Syntax.Pattern) : Type =
      match pat.Kind with
      | PatternKind.Identifier({ Name = name; IsMut = isMut }) ->
        let t = ctx.FreshTypeVar None

        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(name, (t, isMut))
        t
      | PatternKind.Literal(_, literal) ->
        { Type.Kind = TypeKind.Literal(literal)
          Provenance = None }
      | PatternKind.Object elems ->
        let mutable restType: option<Type> = None

        let elems: list<ObjTypeElem> =
          List.choose
            (fun (elem: Syntax.ObjPatElem) ->
              match elem with
              | Syntax.ObjPatElem.KeyValuePat(_span, key, value, _init) ->
                let t = infer_pattern_rec value

                Some(
                  ObjTypeElem.Property
                    { Name = key
                      Optional = false
                      Readonly = false
                      Type = t }
                )
              | Syntax.ObjPatElem.ShorthandPat(_span, name, _init, _is_mut) ->
                let t = ctx.FreshTypeVar None

                // TODO: check if `name` already exists in `assump`
                let isMut = false
                assump <- assump.Add(name, (t, isMut))

                Some(
                  ObjTypeElem.Property
                    { Name = name
                      Optional = false
                      Readonly = false
                      Type = t }
                )

              | Syntax.ObjPatElem.RestPat(span, pattern, isMut) ->
                restType <-
                  Some(
                    { Type.Kind = infer_pattern_rec pattern |> TypeKind.Rest
                      Provenance = None }
                  )

                None)
            elems

        let objType =
          { Type.Kind = TypeKind.Object(elems)
            Provenance = None }

        match restType with
        | Some(restType) ->
          { Type.Kind = TypeKind.Intersection([ objType; restType ])
            Provenance = None }
        | None -> objType
      | PatternKind.Tuple elems ->
        let elems' = List.map infer_pattern_rec elems

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
        | None -> failwith "todo"

    let t = infer_pattern_rec pat

    pat.InferredType <- Some(t)

    Result.Ok((assump, t))

  let inferTypeParam
    (env: Env)
    (tp: Syntax.TypeParam)
    : Result<TypeParam, TypeError> =
    result {
      let! c =
        match tp.Constraint with
        | Some(c) -> inferTypeAnn env c |> Result.map Some
        | None -> Ok None

      let! d =
        match tp.Default with
        | Some(d) -> inferTypeAnn env d |> Result.map Some
        | None -> Ok None

      return
        { Name = tp.Name
          Constraint = c
          Default = d }
    }

  // TODO: Return an updated `Env` instead of requiring `InferScript` to do the updates
  let inferStmt
    (ctx: Ctx)
    (env: Env)
    (stmt: Stmt)
    (generalize: bool)
    : Result<Env, TypeError> =
    result {
      match stmt.Kind with
      | StmtKind.Expr expr ->
        let! _ = inferExpr ctx env expr
        return env
      | StmtKind.For(pattern, right, block) ->
        return! Error(TypeError.NotImplemented "TODO: infer for")
      | StmtKind.Decl({ Kind = DeclKind.VarDecl(pattern, init, typeAnn) }) ->
        let! patBindings, patType = inferPattern ctx env pattern
        let mutable newEnv = env

        for KeyValue(name, binding) in patBindings do
          newEnv <- newEnv.AddValue name binding

        let! initType = inferExpr ctx newEnv init

        match typeAnn with
        | Some(typeAnn) ->
          let! typeAnnType = inferTypeAnn env typeAnn
          do! unify ctx env initType typeAnnType
          do! unify ctx env typeAnnType patType
        | None -> do! unify ctx env initType patType

        for KeyValue(name, binding) in patBindings do
          let binding =
            match generalize with
            | true ->
              let t, isMut = binding
              let t = prune t

              let t =
                match t.Kind with
                | TypeKind.Function f ->
                  { t with
                      Kind = generalizeFunc f |> TypeKind.Function }
                | _ -> t

              t, isMut
            | false -> binding

          newEnv <- newEnv.AddValue name binding

        return newEnv
      | StmtKind.Decl({ Kind = DeclKind.TypeDecl(name, typeAnn, typeParams) }) ->
        // Create a new environment to avoid polluting the current environment
        // with the type parameters.
        let mutable newEnv = env

        let typeParams =
          typeParams
          |> Option.map (fun typeParams ->
            typeParams
            |> List.map (fun typeParam ->
              let unknown =
                { Kind = TypeKind.Keyword Keyword.Unknown
                  Provenance = None }

              let scheme =
                { TypeParams = None
                  Type = unknown
                  IsTypeParam = false }

              newEnv <- newEnv.AddScheme typeParam.Name scheme
              typeParam.Name))

        let! t = inferTypeAnn newEnv typeAnn

        let scheme =
          { TypeParams = typeParams
            Type = t
            IsTypeParam = false }

        return env.AddScheme name scheme
      | StmtKind.Return expr ->
        match expr with
        | Some(expr) ->
          let! _ = inferExpr ctx env expr
          return env
        | None -> return env
    }

  // TODO: Create an `InferModule` that treats all decls as mutually recursive
  let inferScript
    (ctx: Ctx)
    (env: Env)
    (stmts: list<Stmt>)
    : Result<Env, TypeError> =
    result {
      let mutable newEnv = env

      let! _ =
        List.traverseResultM
          (fun stmt ->
            result {
              let! stmtEnv = inferStmt ctx newEnv stmt true
              newEnv <- stmtEnv
            })
          stmts

      return newEnv
    }

  let findReturns (f: Syntax.Function) : list<Expr> =
    let mutable returns: list<Expr> = []

    let visitor =
      { Visitor.VisitExpr =
          fun expr ->
            match expr.Kind with
            | ExprKind.Function _ -> false
            | _ -> true
        Visitor.VisitStmt =
          fun stmt ->
            match stmt.Kind with
            | StmtKind.Return expr ->
              match expr with
              | Some expr -> returns <- expr :: returns
              | None -> ()
            | _ -> ()

            true
        Visitor.VisitPattern = fun _ -> false
        Visitor.VisitTypeAnn = fun _ -> false }

    match f.Body with
    | BlockOrExpr.Block block -> List.iter (walkStmt visitor) block.Stmts
    | BlockOrExpr.Expr expr ->
      walkExpr visitor expr // There might be early returns in match expression
      returns <- expr :: returns // We treat the expression as a return in this case

    returns
