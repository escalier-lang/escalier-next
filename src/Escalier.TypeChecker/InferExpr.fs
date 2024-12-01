namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type

open Error
open Prune
open Env
open Mutability
open Poly
open Unify
open UnifyCall
open Helpers

module rec InferExpr =
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
        let! t = inferExpr ctx env None value

        match t.Kind with
        | TypeKind.Literal(Literal.String value) -> return PropName.String value
        | TypeKind.Literal(Literal.Number value) -> return PropName.Number value
        | TypeKind.UniqueSymbol id -> return PropName.Symbol id
        | _ -> return! Error(TypeError.SemanticError "Invalid key type")
    }

  /// Computes the type of the expression given by node.
  /// The type of the node is computed in the context of the
  /// supplied type environment env. Data types can be introduced into the
  /// language simply by having a predefined set of identifiers in the initial
  /// environment. environment; this way there is no need to change the syntax or, more
  /// importantly, the type-checking program when extending the language.
  /// TODO(#286): Update `inferExpr` to check `typeAnn` for all expression types
  let inferExpr
    (ctx: Ctx)
    (env: Env)
    (typeAnn: option<Type>)
    (expr: Expr)
    : Result<Type, TypeError> =
    ctx.PushReport()

    let r =
      result {
        match expr.Kind with
        | ExprKind.Identifier { Name = name } ->

          match env.Namespace.Namespaces.TryFind name with
          | None -> return! env.GetValue name
          | Some value ->
            let kind = TypeKind.Namespace value
            return { Kind = kind; Provenance = None }
        | ExprKind.Literal(literal) ->
          return
            { Kind = TypeKind.Literal(literal)
              Provenance = Some(Provenance.Expr expr) }
        | ExprKind.Call call ->
          let callee = call.Callee

          match maybeEnumForExpr ctx env callee with
          | Some variantType ->
            match variantType.Kind with
            | TypeKind.Intersection [ tagType
                                      { Kind = TypeKind.Tuple { Elems = elems } } ] ->
              for arg, elem in List.zip call.Args elems do
                let! argType = inferExpr ctx env None arg

                do! unify ctx env None argType elem

              return variantType
            | _ -> return! Error(TypeError.SemanticError "Invalid variant type")
          | None ->
            // If it's a QualifiedIdent, check if it's a enum.  If it is, call
            // the `inferEnumVariant` function instead.
            let! callee = inferExpr ctx env None call.Callee
            // TODO: handle typeArgs at the callsite, e.g. `foo<number>(1)`
            let! result, throws = unifyCall ctx env None call.Args None callee

            call.Throws <- Some(throws)

            return result
        | ExprKind.New call ->
          let! callee = inferExpr ctx env None call.Callee
          let! callee = expandType ctx env None Map.empty callee

          match callee.Kind with
          | TypeKind.Object objElems ->
            let constructors =
              objElems.Elems
              |> List.choose (function
                | Constructor c -> Some c
                | _ -> None)

            let callee: Type =
              constructors
              |> List.map (fun fn ->
                { Kind = TypeKind.Function fn
                  Provenance = None })
              |> intersection

            let args =
              match call.Args with
              | Some args -> args
              | None -> []

            let! typeArgs =
              match call.TypeArgs with
              | Some typeArgs ->
                List.traverseResultM
                  (fun typeArg ->
                    result {
                      let! typeArg = ctx.InferTypeAnn ctx env typeArg
                      return typeArg
                    })
                  typeArgs
                |> Result.map Some
              | _ -> Ok None

            // TODO: update unifyCall so that it can handle calling an object
            // type with constructor signatures directly
            let! returnType, throws =
              unifyCall ctx env None args typeArgs callee

            call.Throws <- Some(throws)

            return returnType
          | _ ->
            return!
              Error(TypeError.SemanticError "Callee is not a constructor type")
        | ExprKind.Binary { Op = op; Left = left; Right = right } ->
          let! funTy = env.GetBinaryOp op

          let! result, _throws =
            unifyCall ctx env None [ left; right ] None funTy

          // TODO: handle throws

          return result

        | ExprKind.Unary { Op = op; Value = arg } ->
          let! funTy = env.GetUnaryOp op

          let! result, _throws = unifyCall ctx env None [ arg ] None funTy

          // TODO: handle throws

          return result
        | ExprKind.Function { Sig = fnSig
                              Body = body
                              Captures = captures } ->
          match typeAnn with
          // Necessary for InferHandler, InferJsxWithCallback, and
          // InferPropsTypeObject tests to pass.
          | Some typeAnnType ->
            let! fn = inferFuncSig ctx env fnSig (Some typeAnnType)

            let exprType =
              { Kind = TypeKind.Function fn
                Provenance = None }

            // TODO: update `inferFuncSig` to do this instead
            do! unify ctx env None exprType typeAnnType

            // Typecheck the body, but don't use its type.  We want to use the
            // type from the type annotation.
            let! _ = inferFuncBody ctx env fnSig fn body

            return exprType
          | None ->
            let! f = inferFunction ctx env fnSig body

            return
              { Kind = TypeKind.Function f
                Provenance = None }

        | ExprKind.Tuple { Elems = elems; Immutable = immutable } ->
          let! elems = List.traverseResultM (inferExpr ctx env None) elems

          return
            { Kind = TypeKind.Tuple { Elems = elems; Immutable = immutable }
              Provenance = None }
        | ExprKind.IfElse { Condition = condition
                            Then = thenBranch
                            Else = elseBranch } ->
          let! conditionTy = inferExpr ctx env None condition

          let! thenBranchTy =
            inferBlockOrExpr ctx env (thenBranch |> BlockOrExpr.Block)

          let! elseBranchTy =
            Option.traverseResult (inferBlockOrExpr ctx env) elseBranch

          do! unify ctx env None conditionTy boolType

          return
            match elseBranchTy with
            | Some(elseBranchTy) -> union [ thenBranchTy; elseBranchTy ]
            | None ->
              { Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }
        | ExprKind.IfLet { Pattern = pattern
                           Target = init
                           Then = thenBranch
                           Else = elseBranch } ->
          // treat pattern/target the as a let binding
          let! invariantPaths =
            checkMutability
              (getPatBindingPaths pattern)
              (getExprBindingPaths env init)

          let! patBindings, patType = ctx.InferPattern ctx env pattern
          let mutable newEnv = env

          for KeyValue(name, binding) in patBindings do
            newEnv <- newEnv.AddValue name binding

          let! initType = inferExpr ctx newEnv None init

          // We expand the type here so that we can filter out any
          // `undefined` types from the union if the expanded type
          // is a union type.
          let! initType = expandType ctx env None Map.empty initType

          let initType =
            match (prune initType).Kind with
            | TypeKind.Union types ->
              let types =
                types
                |> List.filter (fun t ->
                  t.Kind <> TypeKind.Literal(Literal.Undefined))

              union types
            | _ -> initType

          // NOTE: the order is reversed here from what it normally is when
          // inferring a variable declaration. This is because the variable
          // being initialized only need to match one of the types in the union,
          // assuming initType is a union type.
          do! unify ctx newEnv invariantPaths patType initType

          let! thenBranchTy =
            inferBlockOrExpr ctx newEnv (thenBranch |> BlockOrExpr.Block)

          let! elseBranchTy =
            Option.traverseResult (inferBlockOrExpr ctx env) elseBranch

          return
            match elseBranchTy with
            | Some(elseBranchTy) -> union [ thenBranchTy; elseBranchTy ]
            | None ->
              { Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }
        | ExprKind.Object { Elems = elems; Immutable = immutable } ->
          let! map =
            result {
              match typeAnn with
              | Some typeAnn ->
                let! t = expandType ctx env None Map.empty typeAnn
                let! map = getPropertyMap t
                return Some map
              | None -> return None
            }

          let mutable exact = true

          // TODO: Update to use inferObjElem
          let! elems =
            List.traverseResultM
              (fun (elem: ObjElem) ->
                result {
                  match elem with
                  | ObjElem.Property { Name = key; Value = value } ->
                    let! name = inferPropName ctx env key

                    let typeAnn =
                      match map with
                      | None -> None
                      | Some map ->
                        match Map.tryFind name map with
                        | Some t -> Some t
                        | None ->
                          // TODO(#287): Excess property checking
                          // We need to change how getPropertyMap so that we know
                          // when the map represents an "open" or "closed" object type.
                          // ctx.Report.AddDiagnostic
                          //   { Description = $"Excess property '{name}' in object literal"
                          //     Reasons = [] }
                          None

                    // Necessary for the InferPropsTypeObject test to pass.
                    let! t = inferExpr ctx env typeAnn value

                    return
                      [ Property
                          { Name = name
                            Optional = false
                            Readonly = false
                            Type = t } ]
                  | ObjElem.Shorthand { Name = key } ->
                    let! value = env.GetValue key

                    return
                      [ Property
                          { Name = PropName.String key
                            Optional = false
                            Readonly = false
                            Type = value } ]
                  | ObjElem.Spread { Value = value } ->
                    let! t = inferExpr ctx env None value

                    match (prune t).Kind with
                    | TypeKind.Object { Elems = elems; Exact = e } ->
                      if not e then
                        exact <- false

                      return elems
                    | _ -> return [ RestSpread t ]
                })
              elems

          let elems = elems |> List.concat

          let objType =
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = elems
                    Exact = exact
                    Immutable = immutable
                    Interface = false }
              Provenance = None }

          return objType
        | ExprKind.Class cls ->
          let! t, _ = ctx.InferClass ctx env cls false
          return t
        | ExprKind.Member { Target = obj
                            Name = prop
                            OptChain = optChain } ->
          let! objType = inferExpr ctx env None obj
          let propKey = PropName.String(prop)

          let! t =
            getPropType ctx env objType propKey optChain ValueCategory.RValue

          let mutable t = t

          match t.Kind with
          | TypeKind.Function({ Self = Some(self) } as fn) ->
            match self.Pattern with
            | Pattern.Identifier identPat ->
              let! isObjMut = getIsMut ctx env obj

              if identPat.IsMut && not isObjMut then
                return!
                  Error(
                    TypeError.SemanticError
                      "Can't call a mutable method on a immutable object"
                  )
            | _ -> return! Error(TypeError.SemanticError "Invalid self pattern")

            // Replaces `Self` with `objType`
            let fold =
              fun t ->
                let result =
                  match t.Kind with
                  | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Self" } ->
                    objType
                  | _ -> t

                Some(result)

            let returnType = Folder.foldType fold fn.Return

            let paramList =
              fn.ParamList
              |> List.map (fun p ->
                { p with
                    Type = Folder.foldType fold p.Type })

            t <-
              { t with
                  Kind =
                    TypeKind.Function
                      { fn with
                          Return = returnType
                          ParamList = paramList } }

          | _ -> ()

          // TODO: remove `self` from the type of the function
          return t
        | ExprKind.Await(await) ->
          let! t = inferExpr ctx env None await.Value

          match (prune t).Kind with
          | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Promise"
                               TypeArgs = Some([ t ]) } -> return t
          | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Promise"
                               TypeArgs = Some([ t; e ]) } ->
            await.Throws <- Some e
            return t
          | _ -> return t
        | ExprKind.Throw expr ->
          // We throw the type away here because we don't need it, but
          // `expr` will still have its `InferredType` field set.
          let _ = inferExpr ctx env None expr

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
                      match unify ctx env None patternType throwType with
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
        | ExprKind.Match { Target = expr; Cases = cases } ->
          let! exprType = inferExpr ctx env None expr
          let! _, bodyTypes = inferMatchCases ctx env exprType cases
          return (union bodyTypes)
        | ExprKind.Index { Target = target
                           Index = index
                           OptChain = optChain } ->
          let! target = inferExpr ctx env None target
          let! index = inferExpr ctx env None index

          let target = prune target
          let index = prune index

          match index.Kind with
          | TypeKind.Range { Min = _; Max = max } ->
            match target.Kind with
            | TypeKind.Array { Elem = elem; Length = length } ->
              printfn $"max = {max}, length = {length}"
              do! unify ctx env None max length
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

            return! getPropType ctx env target key optChain ValueCategory.RValue
        | ExprKind.Range { Min = min; Max = max } ->
          // TODO: add a constraint that `min` and `max` must be numbers
          // We can do this by creating type variables for them with the
          // proper constraint and then unifying them with the inferred types
          let! min = inferExpr ctx env None min
          let! max = inferExpr ctx env None max

          let scheme = env.TryFindScheme "RangeIterator"

          return
            { Kind =
                TypeKind.TypeRef
                  { Name = QualifiedIdent.Ident "RangeIterator"
                    TypeArgs = Some([ min; max ])
                    Scheme = scheme }
              Provenance = None }
        | ExprKind.Assign { Left = left; Right = right } ->
          // TODO: handle update assign operations
          let! rightType = inferExpr ctx env None right

          let! binding = getLvalue ctx env left

          if binding.Mutable then
            do! unify ctx env None rightType binding.Type
          else
            return!
              Error(TypeError.SemanticError "Can't assign to immutable binding")

          return rightType
        | ExprKind.ExprWithTypeArgs { Expr = target; TypeArgs = typeArgs } ->
          let! t = inferExpr ctx env None target

          let! typeArgs =
            List.traverseResultM (ctx.InferTypeAnn ctx env) typeArgs

          let rec instantiate (t: Type) : Result<Type, TypeError> =
            result {
              match (prune t).Kind with
              | TypeKind.Function fn ->
                let! fn = instantiateFunc ctx fn (Some typeArgs)

                return
                  { Type.Kind = TypeKind.Function fn
                    Provenance = None }
              | TypeKind.TypeRef typeRef ->
                let! scheme =
                  match typeRef.Scheme with
                  | Some scheme -> Result.Ok scheme
                  | None -> env.GetScheme typeRef.Name

                return! instantiate scheme.Type
              | _ ->
                return!
                  Error(
                    TypeError.SemanticError
                      "Can't instantiate a non-function type"
                  )
            }

          return! instantiate t
        | ExprKind.JSXElement jsxElem -> return! inferJsxElement ctx env jsxElem
        | ExprKind.JSXFragment jsxFrag ->
          return! inferJsxFragment ctx env jsxFrag
        | ExprKind.Do body -> return! inferBlock ctx env body
        | ExprKind.TemplateLiteral templateLiteral ->
          // TODO(#353): if all of the expressions in the template literal are literals,
          // we can infer the type of the template literal as a string literal.
          let t =
            { Kind = TypeKind.Primitive Primitive.String
              Provenance = None }

          return t
        | ExprKind.TaggedTemplateLiteral taggedTemplate ->
          let! callee = inferExpr ctx env None taggedTemplate.Tag

          let stringExprs: list<Expr> =
            taggedTemplate.Template.Parts
            |> List.map (fun (part: string) ->
              { Kind = ExprKind.Literal(Literal.String part)
                Span = DUMMY_SPAN
                InferredType = None })

          let strings: Expr =
            { Kind =
                ExprKind.Tuple
                  { Elems = stringExprs
                    Immutable = true }
              Span = DUMMY_SPAN
              InferredType = None }

          let args: list<Expr> = strings :: taggedTemplate.Template.Exprs
          // TODO: handle typeArgs at the callsite, e.g. `foo<number>(1)`
          let! result, throws = unifyCall ctx env None args None callee

          taggedTemplate.Throws <- Some(throws)

          return result
      }

    ctx.MergeUpReport()

    Result.map
      (fun t ->
        expr.InferredType <- Some(t)
        t.Provenance <- Some(Provenance.Expr expr)
        t)
      r

  let inferJsxFragment (ctx: Ctx) (env: Env) (jsxFragment: JSXFragment) =
    result {
      let reactNode =
        { Kind =
            TypeKind.TypeRef
              { Name =
                  QualifiedIdent.Member(
                    QualifiedIdent.Ident "React",
                    "ReactNode"
                  )
                TypeArgs = None
                Scheme = None }
          Provenance = None }

      for child in jsxFragment.Children do
        match child with
        | JSXText jsxText -> () // nothing to infer
        | JSXExprContainer jsxExprContainer ->
          let! child = inferExpr ctx env None jsxExprContainer.Expr
          do! unify ctx env None child reactNode
        | JSXElement jsxElement ->
          let! child = inferJsxElement ctx env jsxElement
          do! unify ctx env None child reactNode
        | JSXFragment jsxFragment ->
          let! child = inferJsxFragment ctx env jsxFragment
          do! unify ctx env None child reactNode

      ctx.MergeUpReport()

      return reactNode
    }

  let inferJsxElement
    (ctx: Ctx)
    (env: Env)
    (jsxElem: JSXElement)
    : Result<Type, TypeError> =
    ctx.PushReport()

    let r =
      result {

        let { JSXElement.Opening = { Attrs = attrs }
              Children = children } =
          jsxElem

        let reactNode =
          { Kind =
              TypeKind.TypeRef
                { Name =
                    QualifiedIdent.Member(
                      QualifiedIdent.Ident "React",
                      "ReactNode"
                    )
                  TypeArgs = None
                  Scheme = None }
            Provenance = None }

        let intrinsics =
          { Kind =
              TypeKind.TypeRef
                { Name =
                    QualifiedIdent.Member(
                      QualifiedIdent.Member(QualifiedIdent.Ident "React", "JSX"),
                      "IntrinsicElements"
                    )
                  TypeArgs = None
                  Scheme = None }
            Provenance = None }

        let! componentProps =
          result {
            match jsxElem.Opening.Name with
            | QualifiedIdent.Ident s when System.Char.IsLower(s, 0) ->
              let key =
                { Kind = TypeKind.Literal(Literal.String s)
                  Provenance = None }

              let tag =
                { Kind = TypeKind.Index { Target = intrinsics; Index = key }
                  Provenance = None }

              return! expandType ctx env None Map.empty tag
            | ident ->
              let! t = getQualifiedIdentType ctx env ident

              match t.Kind with
              | TypeKind.Function { TypeParams = typeParams
                                    ParamList = paramsList
                                    Return = retType } ->
                do! unify ctx env None retType reactNode
                return! expandType ctx env None Map.empty paramsList[0].Type
              | TypeKind.Object _ ->
                // TODO: check that the object extends React.Component
                return!
                  Result.Error(
                    TypeError.NotImplemented
                      "TODO: inferJsxElement - handle class-based components"
                  )
              | _ ->
                return!
                  Result.Error(
                    TypeError.SemanticError
                      $"'{jsxElem.Opening.Name}' is not a component"
                  )
          }

        // NOTE: `componentProps` must be expanded before calling `getPropertyMap`
        let! componentPropsMap = getPropertyMap componentProps

        for attr in attrs do
          match Map.tryFind (PropName.String(attr.Name)) componentPropsMap with
          | None ->
            ctx.Report.AddDiagnostic
              { Description =
                  $"No prop named '{attr.Name}' exists in {jsxElem.Opening.Name}'s props"
                Reasons = [] }
          | Some t ->
            match attr.Value with
            | None ->
              failwith
                "TODO: inferJsxElement - attr.Value should not be optional"
            | Some value ->
              match value with
              | Str literal ->
                let prop =
                  { Kind = TypeKind.Literal literal
                    Provenance = None }

                do! unify ctx env None prop t
              | JSXAttrValue.JSXExprContainer jsxExprContainer ->
                let! propType = inferExpr ctx env (Some t) jsxExprContainer.Expr

                // The reason why we have to also call `unify` here is that `inferExpr`
                // only checks the `typeAnn` param for objects and functions.
                // TODO(#286): Update `inferExpr` to check `typeAnn` for all expression types
                match unify ctx env None propType t with
                | Ok _ -> ()
                | Error reason ->
                  ctx.Report.AddDiagnostic
                    { Description =
                        $"Wrong type provided for the '{attr.Name}' prop"
                      Reasons = [ reason ] }
              | JSXAttrValue.JSXElement jsxElement ->
                // We'll need to pass a type annotation to `inferJsxElement` similar
                // to what we do for `inferExpr` to handle cases where a prop expects
                // a certain type of JSX Element
                failwith "TODO: inferJsxElement - JSXElement"
              | JSXAttrValue.JSXFragment jsxFragment ->
                failwith "TODO: inferJsxElement - JSXFragment"

        for child in children do
          match child with
          | JSXText jsxText -> () // nothing to infer
          | JSXExprContainer jsxExprContainer ->
            let! child = inferExpr ctx env None jsxExprContainer.Expr
            do! unify ctx env None child reactNode
          | JSXElement jsxElement ->
            let! child = inferJsxElement ctx env jsxElement
            do! unify ctx env None child reactNode
          | JSXFragment jsxFragment ->
            let! child = inferJsxFragment ctx env jsxFragment
            do! unify ctx env None child reactNode

        return reactNode
      }

    ctx.MergeUpReport()

    r

  // TODO: update this to unify inferred function type with the typeAnnType if
  // one is provided
  let inferFuncSig
    (ctx: Ctx)
    (env: Env)
    (fnSig: FuncSig)
    (typeAnnType: option<Type>)
    : Result<Function, TypeError> =

    result {
      let mutable newEnv = env

      let! self =
        result {
          match fnSig.Self with
          | Some { Pattern = pattern } ->
            match pattern.Kind with
            | PatternKind.Ident identPat ->
              let scheme = env.TryFindScheme "Self"

              let t =
                { Kind =
                    TypeKind.TypeRef
                      { Name = QualifiedIdent.Ident "Self"
                        Scheme = scheme
                        TypeArgs = None }
                  Provenance = None }

              let param =
                { Pattern =
                    Pattern.Identifier
                      { Name = identPat.Name
                        IsMut = identPat.IsMut }
                  Type = t
                  Optional = false }

              return (Some param)
            | _ -> return! Error(TypeError.SemanticError "Invalid self pattern")
          | None -> return None
        }

      let! typeParams, newEnv = inferTypeParams ctx newEnv fnSig.TypeParams

      let! paramList =
        List.traverseResultM
          (fun (param: Syntax.FuncParam) ->
            result {
              let! paramType =
                match param.TypeAnn with
                | Some(typeAnn) -> ctx.InferTypeAnn ctx newEnv typeAnn
                | None ->
                  match typeAnnType with
                  | Some _ -> Result.Ok(ctx.FreshTypeVar None None)
                  | None ->
                    Result.Ok(
                      { Kind = TypeKind.Keyword Keyword.Unknown
                        Provenance = None }
                    )

              // TODO: figure out a way to avoid having to call inferPattern twice
              // per method (the other call is `inferFuncBody`)
              let! _assumps, patternType =
                ctx.InferPattern ctx newEnv param.Pattern

              // TODO: figure out how to handle unifying `...rest` and `infer _`
              // match patternType.Kind, paramType.Kind with
              // | TypeKind.RestSpread _, _ -> ()
              // | _, _ ->
              //   // Checks if paramType is assignable to the patternType.
              //   // It's okay paramType has extra properties.  Also, all bindings
              //   // in patternType are type variables.
              //   do! unify ctx newEnv None patternType paramType

              return
                { Pattern = patternToPattern param.Pattern
                  Type = paramType
                  Optional = param.Optional }
            })
          fnSig.ParamList

      let! sigThrows =
        match fnSig.Throws with
        | Some typeAnn -> ctx.InferTypeAnn ctx newEnv typeAnn
        | None -> Result.Ok(ctx.FreshTypeVar None None)

      let! sigRetType =
        match fnSig.ReturnType with
        | Some(sigRetType) -> ctx.InferTypeAnn ctx newEnv sigRetType
        | None -> Result.Ok(ctx.FreshTypeVar None None)

      let func = makeFunction typeParams self paramList sigRetType sigThrows

      return func
    }

  let inferFuncBody
    (ctx: Ctx)
    (newEnv: Env)
    (fnSig: FuncSig)
    (placeholderFn: Function)
    (body: BlockOrExpr)
    : Result<Function, TypeError> =

    result {
      let mutable newEnv = newEnv

      let { TypeParams = typeParams
            Self = self
            ParamList = paramList
            Return = sigRetType
            Throws = sigThrows } =
        placeholderFn

      match typeParams with
      | None -> ()
      | Some typeParams ->
        for typeParam in typeParams do

          // let scheme =
          //   { TypeParams = None
          //     Type =
          //       match typeParam.Constraint with
          //       | Some c -> c
          //       | None ->
          //         { Kind = TypeKind.Keyword Keyword.Unknown
          //           Provenance = None }
          //     IsTypeParam = true }

          let scheme =
            { TypeParams = None
              Type = ctx.FreshTypeVar typeParam.Constraint typeParam.Default }

          newEnv <- newEnv.AddScheme typeParam.Name scheme

      match self with
      | Some { Pattern = pattern; Type = t } ->
        match pattern with
        | Pattern.Identifier identPat ->
          let binding =
            { Type = t
              Mutable = identPat.IsMut
              Export = false }

          newEnv <- newEnv.AddValue identPat.Name binding
        | _ -> return! Error(TypeError.SemanticError "Invalid self pattern")
      | _ -> ()

      for { Type = paramType }, { Pattern = pattern } in
        List.zip paramList fnSig.ParamList do
        // TODO: figure out a way to avoid having to call inferPattern twice
        // per method (the other call is `inferFuncSig`)
        let! assumps, patternType = ctx.InferPattern ctx newEnv pattern

        do! unify ctx newEnv None patternType paramType

        for KeyValue(name, binding) in assumps do
          // TODO: update `Env.types` to store `Binding`s insetad of `Type`s
          newEnv <- newEnv.AddValue name binding

      let! _ = inferBlockOrExpr ctx newEnv body

      let retExprs = findReturns body
      let throwTypes = findThrows body

      let bodyThrows = throwTypes |> union

      do! unify ctx newEnv None bodyThrows sigThrows
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
        if fnSig.IsAsync then
          let retType = maybeWrapInPromise retType throwsType

          let never =
            { Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }

          throwsType <- never

          retType
        else
          retType

      do! unify ctx newEnv None retType sigRetType
      let retType = sigRetType

      // let self: option<FuncParam> =
      //   match fnSig.Self with
      //   | Some self ->
      //     let Self: Type =
      //       { Kind =
      //           TypeKind.TypeRef
      //             { Name = QualifiedIdent.Ident "Self"
      //               TypeArgs = None
      //               Scheme = None }
      //         Provenance = None }
      //
      //     Some
      //       { Pattern = patternToPattern self.Pattern
      //         Type = Self
      //         Optional = false }
      //   | None -> None

      return makeFunction typeParams self paramList retType throwsType

    }

  let inferFunction
    (ctx: Ctx)
    (env: Env)
    (fnSig: FuncSig)
    (body: BlockOrExpr)
    : Result<Function, TypeError> =

    result {
      let! placeholderFn = inferFuncSig ctx env fnSig None
      return! inferFuncBody ctx env fnSig placeholderFn body
    }

  let inferBlockOrExpr
    (ctx: Ctx)
    (env: Env)
    (blockOrExpr: BlockOrExpr)
    : Result<Type, TypeError> =
    match blockOrExpr with
    | BlockOrExpr.Block block -> inferBlock ctx env block
    | BlockOrExpr.Expr expr -> inferExpr ctx env None expr

  // TODO: create a version of this that uses similar logic to `inferModule`
  // we need to determine the dependencies between declarations within the
  // block.  We can also add a field to `Ctx` to determine whether or not
  // to use this new implementation of `inferBlock`.
  let inferBlock
    (ctx: Ctx)
    (env: Env)
    (block: Block)
    : Result<Type, TypeError> =
    result {
      let items = block.Stmts |> List.map ModuleItem.Stmt
      let! newEnv = ctx.InferModuleItems ctx env false items

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
        | StmtKind.Return _ ->
          // If there's a return in the block then the type of the block
          // is `never` because the code using the value of the block will
          // never be reached.
          let never =
            { Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }

          return never
        | _ -> return undefined
      | _ -> return undefined
    }

  let start = FParsec.Position("", 0, 1, 1)
  let stop = FParsec.Position("", 0, 1, 1)
  let DUMMY_SPAN: Span = { Start = start; Stop = stop }

  let inferMatchCases
    (ctx: Ctx)
    (env: Env)
    (exprType: Type)
    (cases: list<MatchCase>)
    : Result<list<Type> * list<Type>, TypeError> =
    result {
      let mutable patternTypes = []

      // Infer all pattern types
      let! assumps =
        List.traverseResultM
          (fun (case: MatchCase) ->
            result {
              let! assump, patType = ctx.InferPattern ctx env case.Pattern
              patternTypes <- patType :: patternTypes
              return assump
            })
          cases

      let mutable newExprTypes: list<Type> = []

      // TODO(#335): Check mutability when unifying by computing invariant paths
      // using checkMutability.

      // TODO(#332): Prevent pattern matching of unions of untagged inexact object
      // types. Untagged inexact object types can have extra properties that we
      // can't account for that could result in a different pattern being matched
      // than the intended one.

      // NOTE: the direction of assignability for patternType and exprType is
      // reversed.  This is because the type of the expression being assigned is
      // a union of types and the pattern we're assigning it to is only one of
      // those types.
      if hasTypeVars exprType then
        for patternType in patternTypes do
          let newExprType = fresh ctx exprType

          do!
            unify
              ctx
              { env with IsPatternMatching = true }
              None
              patternType
              newExprType

          newExprTypes <- newExprType :: newExprTypes
      else
        for patternType in patternTypes do
          do!
            unify
              ctx
              { env with IsPatternMatching = true }
              None
              patternType
              exprType

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
                let! _ = inferExpr ctx newEnv None guard
                ()
              | None -> ()

              return! inferBlockOrExpr ctx newEnv case.Body
            })
          (List.zip cases assumps)


      if newExprTypes.IsEmpty then
        return patternTypes, bodyTypes
      else
        let t = union newExprTypes
        do! unify ctx env None t exprType
        return newExprTypes, bodyTypes
    }

  let inferTypeParam
    (ctx: Ctx)
    (env: Env)
    (tp: Syntax.TypeParam)
    : Result<TypeParam, TypeError> =
    result {
      let! c =
        match tp.Constraint with
        | Some(c) -> ctx.InferTypeAnn ctx env c |> Result.map Some
        | None -> Ok None

      let! d =
        match tp.Default with
        | Some(d) -> ctx.InferTypeAnn ctx env d |> Result.map Some
        | None -> Ok None

      return
        { Name = tp.Name
          Constraint = c
          Default = d }
    }

  let inferTypeParams
    (ctx: Ctx)
    (env: Env)
    (typeParams: option<list<Syntax.TypeParam>>)
    : Result<option<list<TypeParam>> * Env, TypeError> =
    result {
      let mutable newEnv = env

      let! typeParams =
        match typeParams with
        | Some(typeParams) ->
          for { Name = name } in typeParams do
            let scheme =
              { TypeParams = None
                Type = ctx.FreshTypeVar None None } // placeholder

            newEnv <- newEnv.AddScheme name scheme

          List.traverseResultM
            (fun typeParam ->
              result {
                let! typeParam = inferTypeParam ctx newEnv typeParam

                let scheme =
                  { TypeParams = None
                    Type =
                      match typeParam.Constraint with
                      | Some c -> c
                      | None ->
                        { Kind = TypeKind.Keyword Keyword.Unknown
                          Provenance = None } }

                // QUESTION: Should we updating the existing schemes or should
                // we be updating their Type field instead?
                newEnv <- newEnv.AddScheme typeParam.Name scheme

                return typeParam
              })
            typeParams
          |> Result.map Some
        | None -> Ok None

      return typeParams, newEnv
    }

  let inferVarDecl
    (ctx: Ctx)
    (env: Env)
    (varDecl: VarDecl)
    : Result<Map<string, Binding> * Map<string, Scheme>, TypeError> =

    // TODO: handle case when `init` is None and `Declare` is `true`
    match varDecl with
    | { Declare = false
        Pattern = pattern
        Init = Some init
        TypeAnn = typeAnn
        Else = elseClause } ->
      result {
        let! invariantPaths =
          checkMutability
            (getPatBindingPaths pattern)
            (getExprBindingPaths env init)

        let! patBindings, patType = ctx.InferPattern ctx env pattern
        let mutable newEnv = env

        for KeyValue(name, binding) in patBindings do
          newEnv <- newEnv.AddValue name binding

        match elseClause with
        | None ->
          match typeAnn with
          | Some typeAnn ->
            let! typeAnnType = ctx.InferTypeAnn ctx newEnv typeAnn
            let! initType = inferExpr ctx newEnv (Some typeAnnType) init
            do! unify ctx newEnv invariantPaths initType typeAnnType
            do! unify ctx newEnv None typeAnnType patType
          | None ->
            let! initType = inferExpr ctx newEnv None init
            do! unify ctx newEnv invariantPaths initType patType
        | Some elseClause ->
          // TODO: udpate inferBlockOrExpr to return `never` if the block contains
          // a return statement
          let! initType = inferExpr ctx newEnv None init

          let initType =
            match (prune initType).Kind with
            | TypeKind.Union types ->
              let types =
                types
                |> List.filter (fun t ->
                  t.Kind <> TypeKind.Literal(Literal.Undefined))

              union types
            | _ -> initType

          match typeAnn with
          | Some typeAnn ->
            let! typeAnnType = ctx.InferTypeAnn ctx newEnv typeAnn
            // NOTE: the order is reversed here because the variable being
            // initialized only need to match one of the types in the union,
            // assuming initType is a union type.
            do! unify ctx newEnv invariantPaths typeAnnType initType
            do! unify ctx newEnv None typeAnnType patType
          | None ->
            // NOTE: the order is reverse here because the variable being
            // initialized only need to match one of the types in the union,
            // assuming initType is a union type.
            do! unify ctx newEnv invariantPaths patType initType

          // Ensure that the `else` clause matches the type of the variable
          // being initialized.
          let! elseTy =
            inferBlockOrExpr ctx env (elseClause |> BlockOrExpr.Block)

          do! unify ctx env invariantPaths elseTy patType

        let mutable schemes: Map<string, Scheme> = Map.empty

        for KeyValue(name, binding) in patBindings do
          match (prune binding.Type).Kind with
          | TypeKind.Object { Elems = elems } ->
            // TODO: modify the constructors so they return `Foo<T>` instead
            // of `Self`.  Right now unifyFuncCall is responsible for this,
            // but that doesn't seem like the best place for it.
            let fns =
              elems
              |> List.choose (fun elem ->
                match elem with
                | ObjTypeElem.Constructor fn -> Some fn
                | ObjTypeElem.Method { Fn = fn } -> Some fn
                | _ -> None)

            for fn in fns do
              // TODO: replace other references to AnonymousClass with the
              // actual class name
              let returnType = fn.Return

              match returnType.Kind with
              | TypeKind.TypeRef typeRef when
                typeRef.Name = QualifiedIdent.Ident "AnonymousClass"
                ->
                typeRef.Name <- QualifiedIdent.Ident name

                match typeRef.Scheme with
                | Some scheme -> schemes <- schemes.Add(name, scheme)
                | _ -> () // failwith "No scheme found"
              | _ -> ()
          | _ -> ()

        return (patBindings, schemes)
      }
    | { Declare = true
        Pattern = pattern
        Init = None
        TypeAnn = Some typeAnn
        Else = None } ->

      result {
        let! patBindings, patType = ctx.InferPattern ctx env pattern
        let mutable newEnv = env

        let! typeAnnType = ctx.InferTypeAnn ctx newEnv typeAnn
        do! unify ctx newEnv None typeAnnType patType

        return (patBindings, Map.empty)
      }
    | _ -> Error(TypeError.SemanticError "Invalid var decl")

  let inferTypeDeclDefn
    (ctx: Ctx)
    (env: Env)
    (placeholder: Scheme)
    (typeParams: option<list<Syntax.TypeParam>>)
    (getType: Env -> Result<Type, TypeError>)
    : Result<Scheme, TypeError> =

    result {
      let mutable newEnv = env

      // Some of the our code checks that a scheme exists in the environment
      // when inferring type references so we add dummy schemes here.
      // TODO: see if we can avoid this in the future.
      match placeholder.TypeParams with
      | None -> ()
      | Some typeParams ->
        for typeParam in typeParams do
          let unknown =
            { Kind = TypeKind.Keyword Keyword.Unknown
              Provenance = None }

          newEnv <-
            newEnv.AddScheme
              typeParam.Name
              { TypeParams = None; Type = unknown }

      newEnv <- newEnv.AddScheme "Self" placeholder

      let! t = getType newEnv

      // TODO: infer type param's constraints and defaults for real here
      let! typeParams, _ = inferTypeParams ctx newEnv typeParams

      return
        { TypeParams = typeParams
          Type = generalizeType t }
    }

  let rec getQualifiedIdentType (ctx: Ctx) (env: Env) (ident: QualifiedIdent) =
    result {
      match ident with
      | QualifiedIdent.Ident name ->
        match env.Namespace.Namespaces.TryFind name with
        | None -> return! env.GetValue name
        | Some value ->
          let kind = TypeKind.Namespace value
          return { Kind = kind; Provenance = None }
      | QualifiedIdent.Member(left, right) ->
        let! left = getQualifiedIdentType ctx env left

        return!
          getPropType
            ctx
            env
            left
            (PropName.String right)
            false
            ValueCategory.RValue
    }

  let rec getLvalue
    (ctx: Ctx)
    (env: Env)
    (expr: Expr)
    : Result<Binding, TypeError> =
    result {
      match expr.Kind with
      | ExprKind.Identifier { Name = name } -> return! env.GetBinding name
      | ExprKind.Index { Target = target; Index = index } ->
        // TODO: disallow optChain in lvalues
        let! binding = getLvalue ctx env target
        let! index = inferExpr ctx env None index

        let key =
          match index.Kind with
          | TypeKind.Literal(Literal.Number i) -> PropName.Number i
          | TypeKind.Literal(Literal.String s) -> PropName.String s
          | TypeKind.UniqueSymbol id -> PropName.Symbol id
          | _ ->
            printfn "index = %A" index
            failwith $"TODO: index can't be a {index}"

        let! t = getPropType ctx env binding.Type key false ValueCategory.LValue
        return { binding with Type = t }
      | ExprKind.Member { Target = target; Name = name } ->
        // TODO: check if `target` is a namespace
        // If the target is either an Identifier or another Member, we
        // can try to look look for a namespace for it.

        // TODO: disallow optChain in lvalues
        let! binding = getLvalue ctx env target

        let! t =
          getPropType
            ctx
            env
            binding.Type
            (PropName.String name)
            false
            ValueCategory.LValue

        return { binding with Type = t }
      | _ ->
        return! Error(TypeError.SemanticError $"{expr} is not a valid lvalue")
    }

  let maybeEnumForExpr (ctx: Ctx) (env: Env) (expr: Expr) : option<Type> =
    let res =
      result {
        match expr.Kind with
        | ExprKind.Member { Target = target; Name = variantName } ->
          match target.Kind with
          | ExprKind.Identifier { Name = targetName } ->
            let! t = env.GetValue targetName
            let! scheme = env.GetScheme(QualifiedIdent.Ident targetName)

            // TODO: add a property to Object types that indicates that the object is a enum
            // we could even link to the scheme for the enum so we don't have to look it up.
            match t.Kind with
            | TypeKind.Object { Elems = elems } ->
              let mutable tagType = None

              for elem in elems do
                match elem with
                | ObjTypeElem.Property { Name = name; Type = t } ->
                  if name = PropName.String variantName then
                    tagType <- Some t
                | _ -> ()

              match tagType with
              | Some tagType ->
                match scheme.Type.Kind with
                | TypeKind.Union types ->
                  let mutable variantType: option<Type> = None

                  for vt in types do
                    match vt.Kind with
                    | TypeKind.Intersection types ->
                      let first = List.head types

                      match first.Kind with
                      | TypeKind.Object { Elems = elems } ->
                        let firstElem = List.head elems

                        match firstElem with
                        | ObjTypeElem.Property { Name = name; Type = t } ->
                          if
                            name = PropName.String "__TAG__" && t = tagType
                          then
                            variantType <- Some vt
                        | _ -> ()
                      | _ -> ()
                    | TypeKind.Object { Elems = elems } ->
                      let firstElem = List.head elems

                      match firstElem with
                      | ObjTypeElem.Property { Name = name; Type = t } ->
                        if name = PropName.String "__TAG__" then
                          let never =
                            { Kind = TypeKind.Keyword Keyword.Never
                              Provenance = None }

                          variantType <- Some vt
                      | _ ->
                        return!
                          Error(TypeError.SemanticError "Invalid variant type")
                    | _ ->
                      return!
                        Error(TypeError.SemanticError "Invalid variant type")

                  match variantType with
                  | Some t ->
                    let! t = instantiateType ctx t scheme.TypeParams None
                    return Some t
                  | None -> return None
                | _ ->
                  return! Error(TypeError.SemanticError "Invalid enum type")
              | _ ->
                return! Error(TypeError.SemanticError "Invalid variant name")
            | _ -> return! Error(TypeError.SemanticError "Invalid enum name")
          | _ -> return! Error(TypeError.SemanticError "Invalid enum target")
        | _ -> return! Error(TypeError.SemanticError "expr is not an enum")
      }

    match res with
    | Result.Ok value -> value
    | Result.Error _ -> None
