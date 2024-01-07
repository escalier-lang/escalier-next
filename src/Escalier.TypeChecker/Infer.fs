namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling
open System.IO

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type

open Error
open Prune
open ExprVisitor
open Env
open Poly
open Unify

module rec Infer =
  let rec patternToPattern (pat: Syntax.Pattern) : Pattern =
    match pat.Kind with
    | PatternKind.Ident { Name = name; IsMut = isMut } ->
      Pattern.Identifier(name)
    // | PatternKind.Is(span, bindingIdent, isName, isMut) ->
    //   Pattern.Is(bindingIdent, isName)
    | PatternKind.Object elems ->
      Pattern.Object(
        List.map
          (fun (elem: Syntax.ObjPatElem) ->
            match elem with
            | Syntax.ObjPatElem.KeyValuePat { Key = key
                                              Value = value
                                              Default = init } ->
              ObjPatElem.KeyValuePat(key, patternToPattern value, init)
            | Syntax.ObjPatElem.ShorthandPat { Name = name
                                               Default = init
                                               IsMut = isMut
                                               Assertion = assertion } ->
              // TODO: isMut
              ObjPatElem.ShorthandPat(name, init)
            | Syntax.ObjPatElem.RestPat { Target = target; IsMut = isMut } ->
              // TODO: isMut
              ObjPatElem.RestPat(patternToPattern target))
          elems
      )
    | PatternKind.Tuple elems ->
      Pattern.Tuple(List.map (patternToPattern >> Some) elems)
    | PatternKind.Wildcard { Assertion = assertion } -> Pattern.Wildcard
    | PatternKind.Literal lit -> Pattern.Literal lit

  let inferPropName
    (ctx: Ctx)
    (env: Env)
    (key: Syntax.PropName)
    : Result<PropName, TypeError> =
    result {
      match key with
      | Syntax.PropName.Ident value -> return PropName.String value
      | Syntax.PropName.String value -> return PropName.String value
      | Syntax.PropName.Number value -> return PropName.Number value
      | Syntax.PropName.Computed value ->
        let! t = inferExpr ctx env value

        match t.Kind with
        | TypeKind.Literal(Literal.String value) -> return PropName.String value
        | TypeKind.Literal(Literal.Number value) -> return PropName.Number value
        | TypeKind.UniqueSymbol id -> return PropName.Symbol id
        | _ -> return! Error(TypeError.SemanticError "Invalid key type")
    }

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
              Provenance = Some(Provenance.Expr expr) }
        | ExprKind.Call call ->
          let! callee = inferExpr ctx env call.Callee

          let! result, throws =
            unifyCall ctx env inferExpr call.Args None callee

          call.Throws <- Some(throws)

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
                    let! typeParam = inferTypeParam ctx newEnv typeParam

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
                    | Some(typeAnn) -> inferTypeAnn ctx newEnv typeAnn
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
          let throwTypes = findThrows f

          let bodyThrows = throwTypes |> union

          let! sigThrows =
            match f.Sig.Throws with
            | Some typeAnn -> inferTypeAnn ctx newEnv typeAnn
            | None -> Result.Ok(ctx.FreshTypeVar None)

          do! unify ctx newEnv bodyThrows sigThrows
          let mutable throwsType = sigThrows

          let! retType =
            result {
              match retExprs with
              | [] ->
                let undefined =
                  { Kind = TypeKind.Literal(Literal.Undefined)
                    Provenance = None }

                return undefined
              | [ expr ] ->
                match expr.InferredType with
                | Some(t) -> return t
                | None -> return! Error(TypeError.SemanticError "")
              | exprs ->
                let! types =
                  List.traverseResultM
                    (fun (expr: Expr) ->
                      match expr.InferredType with
                      | Some(t) -> Ok(t)
                      | None -> Error(TypeError.SemanticError ""))
                    exprs

                return union types
            }

          let retType =
            if f.Sig.IsAsync then
              let retType = maybeWrapInPromise retType throwsType

              let never =
                { Kind = TypeKind.Keyword Keyword.Never
                  Provenance = None }

              throwsType <- never

              retType
            else
              retType

          let! retType =
            result {
              match f.Sig.ReturnType with
              | Some(sigRetType) ->
                let! sigRetType = inferTypeAnn ctx newEnv sigRetType
                do! unify ctx newEnv retType sigRetType
                return sigRetType
              | None -> return retType
            }

          return makeFunctionType typeParams paramList retType throwsType
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
                    let! name = inferPropName ctx env key

                    return
                      Some(
                        Property
                          { Name = name
                            Optional = false
                            Readonly = false
                            Type = t }
                      )
                  | ObjElem.Shorthand(_span, key) ->
                    let! value = env.GetType key

                    return
                      Some(
                        Property
                          { Name = PropName.String key
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
          let propKey = PropName.String(prop)

          // TODO: handle optional chaining
          // TODO: lookup properties on object type
          return getPropType ctx env objType propKey optChain
        | ExprKind.Await(await) ->
          let! t = inferExpr ctx env await.Value

          match t.Kind with
          | TypeKind.TypeRef { Name = "Promise"
                               TypeArgs = Some([ t; e ]) } ->
            await.Throws <- Some e
            return t
          | _ -> return t
        | ExprKind.Throw expr ->
          // We throw the type away here because we don't need it, but
          // `expr` will still have its `InferredType` field set.
          let _ = inferExpr ctx env expr

          let never =
            { Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }

          return never
        | ExprKind.Try _try ->
          let! tryType = inferBlock ctx env _try.Body
          // NOTE: flatten expands any union types in the list
          let throwTypes = findThrowsInBlock _try.Body |> flatten

          let! maybeCatchType =
            match _try.Catch with
            | Some cases ->
              result {
                let! patternTypes, bodyTypes =
                  inferMatchCases ctx env (union throwTypes) cases

                let mutable caughtTypes = []

                // All of the patterns we're catching must be a sub-type of at
                // least one of the exceptions that's being thrown
                for patternType in patternTypes do
                  let mutable unified = false

                  for throwType in throwTypes do
                    if not unified then
                      match unify ctx env patternType throwType with
                      | Ok _ ->
                        caughtTypes <- throwType :: caughtTypes
                        unified <- true
                      | Error _ -> ()

                  if unified then
                    ()
                  else
                    return!
                      Error(
                        TypeError.TypeMismatch(patternType, union throwTypes)
                      )

                // If there are any exceptions that haven't been caught, we
                // rethrow them.
                let uncaughtTypes =
                  throwTypes
                  |> List.filter (fun t -> not (List.contains t caughtTypes))

                if not uncaughtTypes.IsEmpty then
                  let rethrowType = union uncaughtTypes
                  _try.Throws <- Some(rethrowType)

                return bodyTypes |> union
              }
              |> ResultOption.ofResult

            // inferBlock ctx newEnv block |> ResultOption.ofResult
            | None -> ResultOption.ofOption None

          let! _ =
            match _try.Finally with
            | Some(block) -> inferBlock ctx env block |> ResultOption.ofResult
            | None -> ResultOption.ofOption None

          match maybeCatchType with
          | Some catchType -> return union [ tryType; catchType ]
          | None -> return tryType
        | ExprKind.Match(expr, cases) ->
          let! exprType = inferExpr ctx env expr
          // TODO: pass the `exprType` to match against
          let! patternTypes, bodyTypes = inferMatchCases ctx env exprType cases
          // All of the patterns we're matching against `expr` must be sub-types
          // of its type.
          // do! unify ctx env (union patternTypes) exprType
          return (union bodyTypes)
        | ExprKind.Index(target, index, optChain) ->
          let! target = inferExpr ctx env target
          let! index = inferExpr ctx env index

          let target = prune target
          let index = prune index

          match index.Kind with
          | TypeKind.Range { Min = min; Max = max } ->
            match target.Kind with
            | TypeKind.Array { Elem = elem; Length = length } ->
              do! unify ctx env max length
              return elem
            | _ ->
              return!
                Error(
                  TypeError.NotImplemented "TODO: array indexing using a range"
                )
          | _ ->
            let key =
              match index.Kind with
              | TypeKind.Literal(Literal.Number i) -> PropName.Number i
              | TypeKind.Literal(Literal.String s) -> PropName.String s
              | TypeKind.UniqueSymbol id -> PropName.Symbol id
              | _ ->
                printfn "index = %A" index
                failwith $"TODO: index can't be a {index}"

            return getPropType ctx env target key optChain
        | ExprKind.Range { Min = min; Max = max } ->
          // TODO: add a constraint that `min` and `max` must be numbers
          // We can do this by creating type variables for them with the
          // proper constraint and then unifying them with the inferred types
          let! min = inferExpr ctx env min
          let! max = inferExpr ctx env max

          let scheme = env.Schemes.TryFind "RangeIterator"

          return
            { Kind =
                TypeKind.TypeRef
                  { Name = "RangeIterator"
                    TypeArgs = Some([ min; max ])
                    Scheme = scheme }
              Provenance = None }
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
        t.Provenance <- Some(Provenance.Expr expr)
        t)
      r

  let getPropType
    (ctx: Ctx)
    (env: Env)
    (t: Type)
    (key: PropName)
    (optChain: bool)
    : Type =
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

      match elems.TryFind key with
      | Some(p) ->
        match p.Optional with
        | true ->
          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          union [ p.Type; undefined ]
        | false -> p.Type
      | None -> failwith $"Property {key} not found"
    | TypeKind.TypeRef { Name = typeRefName
                         Scheme = scheme
                         TypeArgs = typeArgs } ->
      match scheme with
      | Some scheme ->
        getPropType
          ctx
          env
          (env.ExpandScheme (unify ctx) scheme typeArgs)
          key
          optChain
      | None ->
        match env.Schemes.TryFind typeRefName with
        | Some scheme ->
          getPropType
            ctx
            env
            (env.ExpandScheme (unify ctx) scheme typeArgs)
            key
            optChain
        | None -> failwithf $"{key} not in scope"
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
          let t = getPropType ctx env t key optChain

          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          union [ t; undefined ]
        | _ -> failwith "TODO: lookup member on union type"
    | TypeKind.Tuple elems ->
      match key with
      | PropName.String "length" ->
        { Kind = TypeKind.Literal(Literal.Number(Number.Int elems.Length))
          Provenance = None }
      | PropName.Number number ->
        match number with
        | Float f -> failwith "numeric indexes can't be floats"
        | Int i ->
          if i >= 0 && i < elems.Length then
            elems.[int i]
          else
            // TODO: report a diagnost about the index being out of range
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }
      | _ ->
        let arrayScheme =
          match env.Schemes.TryFind "Array" with
          | Some scheme -> scheme
          | None -> failwith "Array not in scope"
        // TODO: lookup keys in array prototype
        failwith "TODO: lookup member on tuple type"
    | TypeKind.Array { Elem = elem; Length = length } ->
      // TODO: update `TypeKind.Array` to also contain a `Length` type param
      // TODO: use getPropertyType to look up the type of the `.length` property
      // here (and when handling tuples) instead of fabricating it here.
      // TODO: make type param mutable so that we can update it if it's a mutable
      // array and the array size changes.
      match key with
      | PropName.String "length" -> length
      | PropName.Number _ ->
        let unknown =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Provenance = None }

        union [ elem; unknown ]
      | _ ->
        let arrayScheme =
          match env.Schemes.TryFind "Array" with
          | Some scheme -> scheme
          | None -> failwith "Array not in scope"
        // TODO: lookup keys in array prototype
        failwith "TODO: lookup member on array type"
    // TODO: intersection types
    | _ -> failwith $"TODO: lookup member on type - {t}"

  let inferBlockOrExpr
    (ctx: Ctx)
    (env: Env)
    (blockOrExpr: BlockOrExpr)
    : Result<Type, TypeError> =
    match blockOrExpr with
    | BlockOrExpr.Block block -> inferBlock ctx env block
    | BlockOrExpr.Expr expr -> inferExpr ctx env expr

  let inferBlock
    (ctx: Ctx)
    (env: Env)
    (block: Block)
    : Result<Type, TypeError> =
    result {
      let mutable newEnv = env

      for stmt in block.Stmts do
        let! stmtEnv = inferStmt ctx newEnv stmt false
        newEnv <- stmtEnv

      let undefined =
        { Kind = TypeKind.Literal(Literal.Undefined)
          Provenance = None }

      match List.tryLast block.Stmts with
      | Some(stmt) ->
        match stmt.Kind with
        | StmtKind.Expr expr ->
          match expr.InferredType with
          | Some(t) -> return t
          | None -> return undefined
        | _ -> return undefined
      | _ -> return undefined
    }

  let inferTypeAnn
    (ctx: Ctx)
    (env: Env)
    (typeAnn: TypeAnn)
    : Result<Type, TypeError> =
    let kind: Result<TypeKind, TypeError> =
      result {
        match typeAnn.Kind with
        | TypeAnnKind.Array elem ->
          let! elem = inferTypeAnn ctx env elem

          let length =
            { Kind = TypeKind.UniqueNumber(ctx.FreshUniqueId())
              Provenance = None }

          return TypeKind.Array { Elem = elem; Length = length }
        | TypeAnnKind.Literal lit -> return TypeKind.Literal(lit)
        | TypeAnnKind.Keyword keyword ->
          match keyword with
          | KeywordTypeAnn.Boolean ->
            return TypeKind.Primitive Primitive.Boolean
          | KeywordTypeAnn.Number -> return TypeKind.Primitive Primitive.Number
          | KeywordTypeAnn.String -> return TypeKind.Primitive Primitive.String
          | KeywordTypeAnn.Symbol -> return TypeKind.Primitive Primitive.Symbol
          | KeywordTypeAnn.UniqueSymbol -> return ctx.FreshSymbol().Kind
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
                    let! t = inferTypeAnn ctx env typeAnn
                    let! name = inferPropName ctx env name

                    return
                      Property
                        { Name = name
                          Type = t
                          Optional = optional
                          Readonly = readonly }
                  | ObjTypeAnnElem.Callable functionType ->
                    let! f = inferFunctionType ctx env functionType
                    return Callable f
                  | ObjTypeAnnElem.Constructor functionType ->
                    let! f = inferFunctionType ctx env functionType
                    return Constructor f
                  | ObjTypeAnnElem.Method(name, isMut, ``type``) ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Getter(name, returnType, throws) ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Setter(name, param, throws) ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Mapped mapped ->
                    let! c = inferTypeAnn ctx env mapped.TypeParam.Constraint

                    let param =
                      { Name = mapped.TypeParam.Name
                        Constraint = c }

                    let newEnv =
                      env.AddScheme
                        param.Name
                        { TypeParams = None
                          Type = c
                          IsTypeParam = true }

                    let! typeAnn = inferTypeAnn ctx newEnv mapped.TypeAnn

                    let! nameType =
                      match mapped.Name with
                      | Some(name) ->
                        inferTypeAnn ctx newEnv name |> Result.map Some
                      | None -> Ok None

                    return
                      Mapped
                        { TypeParam = param
                          NameType = nameType
                          TypeAnn = typeAnn
                          Optional = mapped.Optional
                          Readonly = mapped.Readonly }
                })
              elems

          return TypeKind.Object(elems)
        | TypeAnnKind.Tuple elems ->
          let! elems = List.traverseResultM (inferTypeAnn ctx env) elems
          return TypeKind.Tuple(elems)
        | TypeAnnKind.Union types ->
          let! types = List.traverseResultM (inferTypeAnn ctx env) types
          return (union types).Kind
        | TypeAnnKind.Intersection types ->
          let! types = List.traverseResultM (inferTypeAnn ctx env) types
          return TypeKind.Intersection types
        | TypeAnnKind.TypeRef(name, typeArgs) ->
          match env.Schemes.TryFind(name) with
          | Some(scheme) ->

            let scheme =
              match scheme.IsTypeParam with
              | true -> None
              | false -> Some(scheme)

            match typeArgs with
            | Some(typeArgs) ->
              let! typeArgs =
                List.traverseResultM (inferTypeAnn ctx env) typeArgs

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
          let! f = inferFunctionType ctx env functionType
          return TypeKind.Function(f)
        | TypeAnnKind.Keyof target ->
          return! inferTypeAnn ctx env target |> Result.map TypeKind.KeyOf
        | TypeAnnKind.Rest target ->
          return! inferTypeAnn ctx env target |> Result.map TypeKind.Rest
        | TypeAnnKind.Typeof target ->
          // TODO: limit what's allowed as a target of `typeof` so that we're
          // compatible with TypeScript
          let! t = inferExpr ctx env target
          return t.Kind
        | TypeAnnKind.Index(target, index) ->
          let! target = inferTypeAnn ctx env target
          let! index = inferTypeAnn ctx env index
          return TypeKind.Index(target, index)
        | TypeAnnKind.Condition conditionType ->
          let! check = inferTypeAnn ctx env conditionType.Check
          let! extends = inferTypeAnn ctx env conditionType.Extends

          let infers = findInfers extends
          let mutable newEnv = env

          // Add placeholder schemes for each `infer` type, these will be
          // replaced later when expanding the conditional in a type alias.
          for infer in infers do
            let scheme =
              { TypeParams = None
                Type = check
                IsTypeParam = true }

            newEnv <- newEnv.AddScheme infer scheme

          let! trueType = inferTypeAnn ctx newEnv conditionType.TrueType
          let! falseType = inferTypeAnn ctx newEnv conditionType.FalseType

          printfn $"trueType = {trueType}, falseType = {falseType}"

          return TypeKind.Condition(check, extends, trueType, falseType)
        | TypeAnnKind.Match matchType ->
          return! Error(TypeError.NotImplemented "TODO: inferTypeAnn - Match") // TODO
        | TypeAnnKind.Infer name -> return TypeKind.Infer name
        | TypeAnnKind.Wildcard -> return TypeKind.Wildcard
        | TypeAnnKind.Binary(left, op, right) ->
          let! left = inferTypeAnn ctx env left
          let! right = inferTypeAnn ctx env right
          return TypeKind.Binary(left, op, right)
        | TypeAnnKind.Range range ->
          let! min = inferTypeAnn ctx env range.Min
          let! max = inferTypeAnn ctx env range.Max
          return TypeKind.Range { Min = min; Max = max }
        | TypeAnnKind.TemplateLiteral { Parts = parts; Exprs = exprs } ->
          let! exprs = List.traverseResultM (inferTypeAnn ctx env) exprs
          return TypeKind.TemplateLiteral { Parts = parts; Exprs = exprs }
      }

    let t: Result<Type, TypeError> =
      Result.map
        (fun kind ->
          let t =
            { Kind = kind
              Provenance = Some(Provenance.TypeAnn typeAnn) }

          typeAnn.InferredType <- Some(t)
          t)
        kind

    t

  let inferFunctionType
    (ctx: Ctx)
    (env: Env)
    (functionType: Syntax.FunctionType)
    : Result<Function, TypeError> =
    result {
      let mutable newEnv = env

      let! typeParams =
        match functionType.TypeParams with
        | Some(typeParams) ->
          List.traverseResultM
            (fun typeParam ->
              result {
                let! typeParam = inferTypeParam ctx newEnv typeParam

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

      let! returnType = inferTypeAnn ctx newEnv functionType.ReturnType

      let! throws =
        match functionType.Throws with
        | Some(throws) -> inferTypeAnn ctx newEnv throws
        | None ->
          Result.Ok(
            { Type.Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }
          )

      let! paramList =
        List.traverseResultM
          (fun (p: FuncParam<TypeAnn>) ->
            result {
              let! t = inferTypeAnn ctx newEnv p.TypeAnn
              let pattern = patternToPattern p.Pattern

              return
                { Pattern = pattern
                  Type = t
                  Optional = false }
            })
          functionType.ParamList

      let f =
        { TypeParams = typeParams
          ParamList = paramList
          Return = returnType
          Throws = throws }

      return f
    }

  let inferPattern
    (ctx: Ctx)
    (env: Env)
    (pat: Syntax.Pattern)
    : Result<BindingAssump * Type, TypeError> =
    let mutable assump = BindingAssump([])

    let rec infer_pattern_rec (pat: Syntax.Pattern) : Type =
      match pat.Kind with
      | PatternKind.Ident({ Name = name
                            IsMut = isMut
                            Assertion = assertion }) ->
        let t =
          match assertion with
          | Some qi ->
            let assertType =
              match qi with
              | { Left = None; Right = "string" } -> strType
              | { Left = None; Right = "number" } -> numType
              | { Left = None; Right = "boolean" } -> boolType
              | _ -> failwith $"TODO: lookup type of {qi}"

            assertType
          | None -> ctx.FreshTypeVar None

        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(name, (t, isMut))
        t
      | PatternKind.Literal lit ->
        { Type.Kind = TypeKind.Literal lit
          Provenance = None }
      | PatternKind.Object elems ->
        let mutable restType: option<Type> = None

        let elems: list<ObjTypeElem> =
          List.choose
            (fun (elem: Syntax.ObjPatElem) ->
              match elem with
              | Syntax.ObjPatElem.KeyValuePat { Key = key
                                                Value = value
                                                Default = init } ->
                let t = infer_pattern_rec value

                Some(
                  ObjTypeElem.Property
                    { Name = PropName.String key
                      Optional = false
                      Readonly = false
                      Type = t }
                )
              | Syntax.ObjPatElem.ShorthandPat { Name = name
                                                 IsMut = isMut
                                                 Assertion = assertion } ->
                // TODO: lookup `assertion` in `env`

                let t = ctx.FreshTypeVar None

                // TODO: check if `name` already exists in `assump`
                assump <- assump.Add(name, (t, isMut))

                Some(
                  ObjTypeElem.Property
                    { Name = PropName.String name
                      Optional = false
                      Readonly = false
                      Type = t }
                )

              | Syntax.ObjPatElem.RestPat { Target = target; IsMut = isMut } ->
                restType <-
                  Some(
                    { Type.Kind = infer_pattern_rec target |> TypeKind.Rest
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
      | PatternKind.Wildcard { Assertion = assertion } ->
        match assertion with
        | Some qi ->
          let assertType =
            match qi with
            | { Left = None; Right = "string" } -> strType
            | { Left = None; Right = "number" } -> numType
            | { Left = None; Right = "boolean" } -> boolType
            | _ -> failwith $"TODO: lookup type of {qi}"

          assertType
        | None ->
          { Type.Kind = TypeKind.Wildcard
            Provenance = None }
      | PatternKind.Rest pat ->
        { Type.Kind = TypeKind.Rest(infer_pattern_rec pat)
          Provenance = None }

    // | PatternKind.Is(span, binding, isName, isMut) ->
    //   match Map.tryFind isName env.Schemes with
    //   | Some(scheme) ->
    //     assump <- assump.Add(binding.Name, (scheme.Type, binding.IsMut))
    //     scheme.Type
    //   | None -> failwith "todo"

    let t = infer_pattern_rec pat

    pat.InferredType <- Some(t)
    t.Provenance <- Some(Provenance.Pattern pat)

    Result.Ok((assump, t))

  let inferMatchCases
    (ctx: Ctx)
    (env: Env)
    (exprType: Type)
    (cases: list<MatchCase>)
    : Result<list<Type> * list<Type>, TypeError> =
    result {
      let mutable patternTypes = []

      // TODO: do two passes
      // 1. unify all of the patterns with `exprType`
      // 2. infer the types of all of the bodies

      // Infer all pattern types
      let! assumps =
        List.traverseResultM
          (fun (case: MatchCase) ->
            result {
              let! assump, patType = inferPattern ctx env case.Pattern
              patternTypes <- patType :: patternTypes
              return assump
            })
          cases

      // Unify all pattern types with `exprType`
      do! unify ctx env (union patternTypes) exprType

      // Infer body types
      let! bodyTypes =
        List.traverseResultM
          (fun (case: MatchCase, assump: BindingAssump) ->
            result {
              let mutable newEnv = env

              for binding in assump do
                newEnv <- newEnv.AddValue binding.Key binding.Value

              match case.Guard with
              | Some guard ->
                let! _ = inferExpr ctx newEnv guard
                ()
              | None -> ()

              return! inferBlockOrExpr ctx newEnv case.Body
            })
          (List.zip cases assumps)

      return patternTypes, bodyTypes
    }

  let inferTypeParam
    (ctx: Ctx)
    (env: Env)
    (tp: Syntax.TypeParam)
    : Result<TypeParam, TypeError> =
    result {
      let! c =
        match tp.Constraint with
        | Some(c) -> inferTypeAnn ctx env c |> Result.map Some
        | None -> Ok None

      let! d =
        match tp.Default with
        | Some(d) -> inferTypeAnn ctx env d |> Result.map Some
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
        let! patBindings, patType = inferPattern ctx env pattern
        let! rightType = inferExpr ctx env right

        let symbol =
          match env.Values.TryFind "Symbol" with
          | Some scheme -> fst scheme
          | None -> failwith "Symbol not in scope"

        let symbolIterator =
          getPropType ctx env symbol (PropName.String "iterator") false

        // TODO: only lookup Symbol.iterator on Array for arrays and tuples
        let arrayScheme =
          match env.Schemes.TryFind "Array" with
          | Some scheme -> scheme
          | None -> failwith "Array not in scope"

        let propName =
          match symbolIterator.Kind with
          | TypeKind.UniqueSymbol id -> PropName.Symbol id
          | _ -> failwith "Symbol.iterator is not a unique symbol"

        // TODO: Update `getPropType` to return a Result<Type, TypeError>
        getPropType ctx env arrayScheme.Type propName false |> ignore

        // TODO: add a variant of `ExpandType` that allows us to specify a
        // predicate that can stop the expansion early.
        let expandedRightType = env.ExpandType (unify ctx) rightType

        let elemType =
          match expandedRightType.Kind with
          | TypeKind.Array { Elem = elem; Length = length } -> elem
          | TypeKind.Tuple elemTypes -> union elemTypes
          | TypeKind.Range _ -> expandedRightType
          | TypeKind.Object _ ->
            // TODO: try using unify and/or an utility type to extract the
            // value type from an iterator

            // TODO: add a `tryGetPropType` function that returns an option
            let next =
              getPropType ctx env rightType (PropName.String "next") false

            match next.Kind with
            | TypeKind.Function f ->
              getPropType ctx env f.Return (PropName.String "value") false
            | _ -> failwith $"{rightType} is not an iterator"
          | _ -> failwith "TODO: for loop over non-iterable type"

        do! unify ctx env elemType patType

        let mutable newEnv = env

        for KeyValue(name, binding) in patBindings do
          newEnv <- newEnv.AddValue name binding

        let! _ = inferBlock ctx newEnv block
        return env
      | StmtKind.Decl({ Kind = DeclKind.VarDecl(pattern, init, typeAnn) }) ->
        let! patBindings, patType = inferPattern ctx env pattern
        let mutable newEnv = env

        for KeyValue(name, binding) in patBindings do
          newEnv <- newEnv.AddValue name binding

        let! initType = inferExpr ctx newEnv init

        match typeAnn with
        | Some(typeAnn) ->
          let! typeAnnType = inferTypeAnn ctx env typeAnn
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

        let! t = inferTypeAnn ctx newEnv typeAnn

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

  let resolvePath
    (baseDir: string)
    (currentPath: string)
    (importPath: string)
    : string =
    if importPath.StartsWith "~" then
      Path.GetFullPath(Path.Join(baseDir, importPath.Substring(1)))
    else if importPath.StartsWith "." then
      Path.GetFullPath(
        Path.Join(Path.GetDirectoryName(currentPath), importPath)
      )
    else
      importPath

  let inferImport
    (ctx: Ctx)
    (env: Env)
    (import: Import)
    : Result<Env, TypeError> =
    // TODO: read the file and infer the module
    // TODO: have a way of store
    failwith "TODO - inferImport"

  let inferModuleItem
    (ctx: Ctx)
    (env: Env)
    (filename: string)
    (item: ModuleItem)
    (generalize: bool)
    : Result<Env, TypeError> =

    result {
      match item with
      | Import import ->
        let exports = ctx.GetExports filename import

        let mutable imports = Env.empty

        for specifier in import.Specifiers do
          match specifier with
          | Named(name, alias) ->
            let source = name

            let target =
              match alias with
              | Some(alias) -> alias
              | None -> source

            let valueLookup =
              match Map.tryFind source exports.Values with
              | Some(binding) ->
                imports <- imports.AddValue target binding
                Ok(())
              | None -> Error("not found")

            let schemeLookup =
              match Map.tryFind source exports.Schemes with
              | Some(scheme) ->
                imports <- imports.AddScheme target scheme
                Ok(())
              | None -> Error("not found")

            match valueLookup, schemeLookup with
            // If we can't find the symbol in either the values or schemes
            // we report an error
            | Error _, Error _ ->
              let resolvedPath = ctx.ResolvePath filename import

              return!
                Error(
                  TypeError.SemanticError
                    $"{resolvedPath} doesn't export '{name}'"
                )
            | _, _ -> ()
          | ModuleAlias _ -> failwith "TODO"

        return
          { env with
              Values = FSharpPlus.Map.union env.Values imports.Values
              Schemes = FSharpPlus.Map.union env.Schemes imports.Schemes }
      | DeclareLet(name, typeAnn) ->
        let! typeAnnType = inferTypeAnn ctx env typeAnn
        let! assump, patType = inferPattern ctx env name

        do! unify ctx env typeAnnType patType

        let mutable newEnv = env

        for binding in assump do
          newEnv <- newEnv.AddValue binding.Key binding.Value

        return newEnv
      | Stmt stmt -> return! inferStmt ctx env stmt generalize
    }

  // TODO: Create an `InferModule` that treats all decls as mutually recursive
  let inferScript
    (ctx: Ctx)
    (env: Env)
    (filename: string)
    (m: Module)
    : Result<Env, TypeError> =
    result {
      let mutable newEnv = env

      let! _ =
        List.traverseResultM
          (fun item ->
            result {
              let! itemEnv = inferModuleItem ctx newEnv filename item true
              newEnv <- itemEnv
            })
          m.Items

      return newEnv
    }

  let findReturns (f: Syntax.Function) : list<Expr> =
    let mutable returns: list<Expr> = []

    let visitor =
      { ExprVisitor.VisitExpr =
          fun expr ->
            match expr.Kind with
            | ExprKind.Function _ -> false
            | _ -> true
        ExprVisitor.VisitStmt =
          fun stmt ->
            match stmt.Kind with
            | StmtKind.Return expr ->
              match expr with
              | Some expr -> returns <- expr :: returns
              | None -> ()
            | _ -> ()

            true
        ExprVisitor.VisitPattern = fun _ -> false
        ExprVisitor.VisitTypeAnn = fun _ -> false }

    match f.Body with
    | BlockOrExpr.Block block -> List.iter (walkStmt visitor) block.Stmts
    | BlockOrExpr.Expr expr ->
      walkExpr visitor expr // There might be early returns in match expression
      returns <- expr :: returns // We treat the expression as a return in this case

    returns

  let maybeWrapInPromise (t: Type) (e: Type) : Type =
    match t.Kind with
    | TypeKind.TypeRef { Name = "Promise" } -> t
    | _ ->
      let never =
        { Kind = TypeKind.Keyword Keyword.Never
          Provenance = None }

      { Kind =
          TypeKind.TypeRef
            { Name = "Promise"
              TypeArgs = Some([ t; e ])
              Scheme = None }
        Provenance = None }

  // TODO: dedupe with findThrowsInBlock
  let findThrows (f: Syntax.Function) : list<Type> =
    let mutable throws: list<Type> = []

    let visitor =
      { ExprVisitor.VisitExpr =
          fun expr ->
            match expr.Kind with
            | ExprKind.Function _ -> false
            | ExprKind.Try { Catch = catch
                             Throws = uncaughtThrows } ->
              match uncaughtThrows with
              | Some(uncaughtThrows) -> throws <- uncaughtThrows :: throws
              | None -> ()
              // If there is a catch clause, don't visit the children
              // TODO: we still need to visit the catch clause in that
              // cacse because there may be re-throws inside of it
              catch.IsNone
            | ExprKind.Throw expr ->
              match expr.InferredType with
              | Some t -> throws <- t :: throws
              | None -> failwith "Expected `expr` to have an `InferredType`"

              true // there might be other `throw` expressions inside
            | ExprKind.Call call ->
              match call.Throws with
              | Some t -> throws <- t :: throws
              | None -> ()

              true // there might be other `throw` expressions inside
            | ExprKind.Await await ->
              match await.Throws with
              | Some t -> throws <- t :: throws
              | None -> ()

              true // there might be other `throw` expressions inside
            | _ -> true
        ExprVisitor.VisitStmt = fun _ -> true
        ExprVisitor.VisitPattern = fun _ -> false
        ExprVisitor.VisitTypeAnn = fun _ -> false }

    match f.Body with
    | BlockOrExpr.Block block -> List.iter (walkStmt visitor) block.Stmts
    | BlockOrExpr.Expr expr -> walkExpr visitor expr

    throws

  // TODO: dedupe with findThrows
  let findThrowsInBlock (block: Block) : list<Type> =
    let mutable throws: list<Type> = []

    let visitor =
      { ExprVisitor.VisitExpr =
          fun expr ->
            match expr.Kind with
            | ExprKind.Function _ -> false
            | ExprKind.Try { Catch = catch
                             Throws = uncaughtThrows } ->
              match uncaughtThrows with
              | Some(uncaughtThrows) -> throws <- uncaughtThrows :: throws
              | None -> ()
              // If there is a catch clause, don't visit the children
              // TODO: we still need to visit the catch clause in that
              // cacse because there may be re-throws inside of it
              catch.IsNone
            | ExprKind.Throw expr ->
              match expr.InferredType with
              | Some t -> throws <- t :: throws
              | None -> failwith "Expected `expr` to have an `InferredType`"

              true // there might be other `throw` expressions inside
            | ExprKind.Call call ->
              match call.Throws with
              | Some t -> throws <- t :: throws
              | None -> ()

              true // there might be other `throw` expressions inside
            | ExprKind.Await await ->
              match await.Throws with
              | Some t -> throws <- t :: throws
              | None -> ()

              true // there might be other `throw` expressions inside
            | _ -> true
        ExprVisitor.VisitStmt = fun _ -> true
        ExprVisitor.VisitPattern = fun _ -> false
        ExprVisitor.VisitTypeAnn = fun _ -> false }

    List.iter (walkStmt visitor) block.Stmts

    throws

  let findBindingNames (p: Syntax.Pattern) : list<string> =
    let mutable names: list<string> = []

    let visitor =
      { ExprVisitor.VisitExpr =
          fun expr ->
            match expr.Kind with
            | ExprKind.Function _ -> false
            | _ -> true
        ExprVisitor.VisitStmt = fun _ -> false
        ExprVisitor.VisitPattern =
          fun pat ->
            match pat.Kind with
            | PatternKind.Ident { Name = name } ->
              names <- name :: names
              false
            | _ -> true
        ExprVisitor.VisitTypeAnn = fun _ -> false }

    walkPattern visitor p

    List.rev names

  let findModuleBindingNames (m: Module) : list<string> =
    let mutable names: list<string> = []

    for item in m.Items do
      match item with
      | Stmt stmt ->
        match stmt.Kind with
        | StmtKind.Decl({ Kind = DeclKind.VarDecl(pattern, _, _) }) ->
          names <- List.concat [ names; findBindingNames pattern ]
        | _ -> ()
      | _ -> ()

    names

  let findInfers (t: Type) : list<string> =
    // TODO: disallow multiple `infer`s with the same identifier
    let mutable infers: list<string> = []

    let visitor =
      fun (t: Type) ->
        match t.Kind with
        | TypeKind.Infer name -> infers <- name :: infers
        | _ -> ()

    TypeVisitor.walkType visitor t

    infers
