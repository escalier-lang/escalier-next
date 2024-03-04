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
open Mutability
open Poly
open Unify

module rec Infer =
  let rec patternToPattern (pat: Syntax.Pattern) : Pattern =
    match pat.Kind with
    | PatternKind.Ident { Name = name; IsMut = mut } ->
      Pattern.Identifier { Name = name; IsMut = mut }
    // | PatternKind.Is(span, bindingIdent, isName, isMut) ->
    //   Pattern.Is(bindingIdent, isName)
    | PatternKind.Object { Elems = elems; Immutable = immutable } ->
      Pattern.Object
        { Elems =
            List.map
              (fun (elem: Syntax.ObjPatElem) ->
                match elem with
                | Syntax.ObjPatElem.KeyValuePat { Key = key
                                                  Value = value
                                                  Default = init } ->
                  ObjPatElem.KeyValuePat
                    { Key = key
                      Value = patternToPattern value
                      Init = init }
                | Syntax.ObjPatElem.ShorthandPat { Name = name
                                                   Default = init
                                                   IsMut = mut
                                                   Assertion = assertion } ->
                  ObjPatElem.ShorthandPat
                    { Name = name
                      Init = init
                      IsMut = mut }
                | Syntax.ObjPatElem.RestPat { Target = target; IsMut = isMut } ->
                  // TODO: isMut
                  ObjPatElem.RestPat(patternToPattern target))
              elems
          Immutable = immutable }
    | PatternKind.Tuple { Elems = elems; Immutable = immutable } ->
      Pattern.Tuple
        { Elems = List.map (patternToPattern >> Some) elems
          Immutable = immutable }
    | PatternKind.Wildcard { Assertion = assertion } -> Pattern.Wildcard
    | PatternKind.Literal lit -> Pattern.Literal lit
    | PatternKind.Rest rest -> Pattern.Rest(patternToPattern rest)

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
        | ExprKind.Identifier(name) ->

          match env.Namespaces.TryFind name with
          | None -> return! env.GetType name
          | Some value ->
            let kind = TypeKind.Namespace value
            return { Kind = kind; Provenance = None }
        | ExprKind.Literal(literal) ->
          return
            { Kind = TypeKind.Literal(literal)
              Provenance = Some(Provenance.Expr expr) }
        | ExprKind.Call call ->
          let! callee = inferExpr ctx env call.Callee

          let! result, throws = unifyCall ctx env None call.Args None callee

          call.Throws <- Some(throws)

          return result
        | ExprKind.New call ->
          let! callee = inferExpr ctx env call.Callee
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
                      let! typeArg = inferTypeAnn ctx env typeArg
                      return typeArg
                    })
                  typeArgs
                |> Result.map Some
              | _ -> Ok None

            // TODO: update unifyCall so that it can handle calling an object
            // type with constructor signatures directly
            let! result, throws = unifyCall ctx env None args typeArgs callee

            call.Throws <- Some(throws)

            return result
          | _ ->
            return!
              Error(TypeError.SemanticError "Callee is not a constructor type")
        | ExprKind.Binary(op, left, right) ->
          let! funTy = env.GetBinaryOp op

          let! result, throws =
            unifyCall ctx env None [ left; right ] None funTy

          // TODO: handle throws

          return result

        | ExprKind.Unary(op, arg) ->
          let! funTy = env.GetUnaryOp op

          let! result, throws = unifyCall ctx env None [ arg ] None funTy

          // TODO: handle throws

          return result
        | ExprKind.Function { Sig = fnSig; Body = body } ->
          let! f = inferFunction ctx env fnSig body

          return
            { Kind = TypeKind.Function f
              Provenance = None }
        | ExprKind.Tuple { Elems = elems; Immutable = immutable } ->
          let! elems = List.traverseResultM (inferExpr ctx env) elems

          return
            { Kind = TypeKind.Tuple { Elems = elems; Immutable = immutable }
              Provenance = None }
        | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
          let! conditionTy = inferExpr ctx env condition

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
        | ExprKind.Object { Elems = elems; Immutable = immutable } ->
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
            { Kind = TypeKind.Object { Elems = elems; Immutable = immutable }
              Provenance = None }

          match spreadTypes with
          | [] -> return objType
          | _ ->
            return
              { Kind = TypeKind.Intersection([ objType ] @ spreadTypes)
                Provenance = None }
        | ExprKind.Struct { TypeRef = { Ident = name; TypeArgs = typeArgs }
                            Elems = elems } ->

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

          let initObjType =
            { Kind = TypeKind.Object { Elems = elems; Immutable = false }
              Provenance = None }

          let! scheme = env.GetScheme name

          let! typeArgs =
            match typeArgs with
            | Some(typeArgs) ->
              List.traverseResultM (inferTypeAnn ctx env) typeArgs
              |> Result.map Some
            | None -> Ok None

          let! t = expandScheme ctx env None scheme Map.empty typeArgs

          match t.Kind with
          | TypeKind.Struct { Elems = elems; Impls = impls } ->
            let structObjType =
              { Kind = TypeKind.Object { Elems = elems; Immutable = false }
                Provenance = None }

            do! unify ctx env None initObjType structObjType

            let typeRef: TypeRef =
              { Name = name
                TypeArgs = typeArgs
                Scheme = None } // TODO: lookup Scheme

            let kind: TypeKind =
              TypeKind.Struct
                { TypeRef = typeRef
                  Elems = elems
                  Impls = impls }

            return { Type.Kind = kind; Provenance = None }
          | _ ->
            return!
              Error(TypeError.SemanticError $"Expected Struct type, got {t}")

        | ExprKind.Member(obj, prop, optChain) ->
          let! objType = inferExpr ctx env obj
          let propKey = PropName.String(prop)

          let! t = getPropType ctx env objType propKey optChain

          match t.Kind with
          | TypeKind.Function { Self = Some(self) } ->
            match self.Pattern with
            | Identifier identPat ->
              let! isObjMut = getIsMut ctx env obj

              if identPat.IsMut && not isObjMut then
                return!
                  Error(
                    TypeError.SemanticError
                      "Can't call a mutable method on a immutable object"
                  )
            | _ -> return! Error(TypeError.SemanticError "Invalid self pattern")
          | _ -> ()

          return t
        | ExprKind.Await(await) ->
          let! t = inferExpr ctx env await.Value

          match t.Kind with
          | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Promise"
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

            return! getPropType ctx env target key optChain
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
                  { Name = QualifiedIdent.Ident "RangeIterator"
                    TypeArgs = Some([ min; max ])
                    Scheme = scheme }
              Provenance = None }
        | ExprKind.Assign(operation, left, right) ->
          let! rightType = inferExpr ctx env right

          let! t, isMut = getLvalue ctx env left

          if isMut then
            do! unify ctx env None rightType t
          else
            return!
              Error(TypeError.SemanticError "Can't assign to immutable binding")

          return rightType
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

  let inferFuncSig
    (ctx: Ctx)
    (env: Env)
    (fnSig: FuncSig<option<TypeAnn>>)
    : Result<Function, TypeError> =

    result {
      let mutable newEnv = env

      let! self =
        result {
          match fnSig.Self with
          | Some { Pattern = pattern } ->
            match pattern.Kind with
            | PatternKind.Ident identPat ->
              let t =
                { Kind =
                    TypeKind.TypeRef
                      { Name = QualifiedIdent.Ident "Self"
                        Scheme = None
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
          (fun (param: Syntax.FuncParam<option<TypeAnn>>) ->
            result {
              let! paramType =
                match param.TypeAnn with
                | Some(typeAnn) -> inferTypeAnn ctx newEnv typeAnn
                | None -> Result.Ok(ctx.FreshTypeVar None)

              // TODO: figure out a way to avoid having to call inferPattern twice
              // per method (the other call is `inferFuncBody`)
              let! assumps, patternType = inferPattern ctx newEnv param.Pattern

              do! unify ctx newEnv None patternType paramType

              return
                { Pattern = patternToPattern param.Pattern
                  Type = paramType
                  Optional = param.Optional }
            })
          fnSig.ParamList

      let! sigThrows =
        match fnSig.Throws with
        | Some typeAnn -> inferTypeAnn ctx newEnv typeAnn
        | None -> Result.Ok(ctx.FreshTypeVar None)

      let! sigRetType =
        match fnSig.ReturnType with
        | Some(sigRetType) -> inferTypeAnn ctx newEnv sigRetType
        | None -> Result.Ok(ctx.FreshTypeVar None)

      let func = makeFunction typeParams self paramList sigRetType sigThrows

      return func
    }

  let inferFuncBody
    (ctx: Ctx)
    (newEnv: Env)
    (fnSig: FuncSig<option<TypeAnn>>)
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

          let scheme =
            { TypeParams = None
              Type =
                match typeParam.Constraint with
                | Some c -> c
                | None ->
                  { Kind = TypeKind.Keyword Keyword.Unknown
                    Provenance = None }
              IsTypeParam = true }

          newEnv <- newEnv.AddScheme typeParam.Name scheme

      match self with
      | Some { Pattern = pattern; Type = t } ->
        match pattern with
        | Identifier identPat ->
          newEnv <- newEnv.AddValue identPat.Name (t, identPat.IsMut)
        | _ -> return! Error(TypeError.SemanticError "Invalid self pattern")
      | _ -> ()

      for { Type = paramType }, { Pattern = pattern } in
        List.zip paramList fnSig.ParamList do
        // TODO: figure out a way to avoid having to call inferPattern twice
        // per method (the other call is `inferFuncSig`)
        let! assumps, patternType = inferPattern ctx newEnv pattern

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

      let self: option<FuncParam> =
        match fnSig.Self with
        | Some self ->
          let Self: Type =
            { Kind =
                TypeKind.TypeRef
                  { Name = QualifiedIdent.Ident "Self"
                    TypeArgs = None
                    Scheme = None }
              Provenance = None }

          Some
            { Pattern = patternToPattern self.Pattern
              Type = Self
              Optional = false }
        | None -> None

      return makeFunction typeParams self paramList retType throwsType

    }

  let inferFunction
    (ctx: Ctx)
    (env: Env)
    (fnSig: FuncSig<option<TypeAnn>>)
    (body: BlockOrExpr)
    : Result<Function, TypeError> =

    result {
      let! placeholderFn = inferFuncSig ctx env fnSig
      return! inferFuncBody ctx env fnSig placeholderFn body
    }

  let qualifyTypeRefs
    (t: Type)
    (nsName: string)
    (nsScheme: Map<string, Scheme>)
    : Type =

    let f =
      fun (t: Type) ->
        match t.Kind with
        | TypeKind.TypeRef { Name = QualifiedIdent.Ident name
                             Scheme = scheme
                             TypeArgs = typeArgs } ->
          match nsScheme.TryFind name with
          | Some _ ->
            let name = QualifiedIdent.Member(QualifiedIdent.Ident nsName, name)

            let kind =
              TypeKind.TypeRef
                { Name = name
                  TypeArgs = typeArgs
                  Scheme = scheme }

            Some { t with Kind = kind }
          | None -> Some t
        | _ -> Some t

    Folder.foldType f t

  let getPropType
    (ctx: Ctx)
    (env: Env)
    (t: Type)
    (key: PropName)
    (optChain: bool)
    : Result<Type, TypeError> =
    result {

      let t = prune t

      match t.Kind with
      | TypeKind.Object { Elems = elems } ->
        match inferMemberAccess key elems with
        | Some t -> return t
        | None ->
          return! Error(TypeError.SemanticError $"Property {key} not found")
      | TypeKind.Namespace { Name = nsName
                             Values = values
                             Schemes = schemes
                             Namespaces = namespaces } ->
        match key with
        | PropName.String s ->
          match values.TryFind s with
          | None ->
            match namespaces.TryFind s with
            | None ->
              return! Error(TypeError.SemanticError $"Property {key} not found")
            | Some ns ->
              return
                { Kind = TypeKind.Namespace ns
                  Provenance = None }
          // TODO: handle nested namespaces by adding a optional reference
          // to the parent namespace that we can follow
          | Some(t, _) -> return qualifyTypeRefs t nsName schemes
        | PropName.Number _ ->
          return!
            Error(
              TypeError.SemanticError
                "Can't use a number as a key with a namespace"
            )
        | PropName.Symbol _ ->
          return!
            Error(
              TypeError.SemanticError
                "Can't use a symbol as a key with a namespace"
            )
      | TypeKind.Struct { Elems = elems; Impls = impls } ->
        match inferMemberAccess key elems with
        | Some t -> return t
        | None ->
          let mutable t: option<Type> = None

          for impl in impls do
            if t.IsSome then
              ()
            else
              t <- inferMemberAccess key impl.Elems

          match t with
          | Some t -> return t
          | None ->
            return! Error(TypeError.SemanticError $"Property {key} not found")
      | TypeKind.TypeRef { Name = typeRefName
                           Scheme = scheme
                           TypeArgs = typeArgs } ->
        match scheme with
        | Some scheme ->
          let! objType = expandScheme ctx env None scheme Map.empty typeArgs
          return! getPropType ctx env objType key optChain
        | None ->
          let! scheme = env.GetScheme typeRefName
          let! objType = expandScheme ctx env None scheme Map.empty typeArgs
          return! getPropType ctx env objType key optChain
      | TypeKind.Union types ->
        let undefinedTypes, definedTypes =
          List.partition
            (fun t -> t.Kind = TypeKind.Literal(Literal.Undefined))
            types

        if undefinedTypes.IsEmpty then
          return!
            Error(TypeError.NotImplemented "TODO: lookup member on union type")
        else if not optChain then
          return!
            Error(TypeError.SemanticError "Can't lookup property on undefined")
        else
          match definedTypes with
          | [ t ] ->
            let! t = getPropType ctx env t key optChain

            let undefined =
              { Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }

            return union [ t; undefined ]
          | _ ->
            return!
              Error(
                TypeError.NotImplemented "TODO: lookup member on union type"
              )
      | TypeKind.Tuple { Elems = elems } ->
        match key with
        | PropName.String "length" ->
          return
            { Kind = TypeKind.Literal(Literal.Number(Number.Int elems.Length))
              Provenance = None }
        | PropName.Number number ->
          match number with
          | Float f ->
            return!
              Error(TypeError.SemanticError "numeric indexes can't be floats")
          | Int i ->
            if i >= 0 && i < elems.Length then
              return elems.[int i]
            else
              // TODO: report a diagnost about the index being out of range
              return
                { Kind = TypeKind.Literal(Literal.Undefined)
                  Provenance = None }
        | _ ->
          let arrayScheme =
            match env.Schemes.TryFind "Array" with
            | Some scheme -> scheme
            | None -> failwith "Array not in scope"
          // TODO: lookup keys in array prototype
          return!
            Error(TypeError.NotImplemented "TODO: lookup member on tuple type")
      | TypeKind.Array { Elem = elem; Length = length } ->
        // TODO: update `TypeKind.Array` to also contain a `Length` type param
        // TODO: use getPropertyType to look up the type of the `.length` property
        // here (and when handling tuples) instead of fabricating it here.
        // TODO: make type param mutable so that we can update it if it's a mutable
        // array and the array size changes.
        match key with
        | PropName.String "length" -> return length
        | PropName.Number _ ->
          let unknown =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          return union [ elem; unknown ]
        | _ ->
          // TODO: update Interop.Infer to combine Array and ReadonlyArray into
          // a single Array type where methods are marked appropriately with
          // `self` and `mut self`.
          let arrayScheme =
            match env.Schemes.TryFind "Array" with
            | Some scheme -> scheme
            | None -> failwith "Array not in scope"

          // Instead of expanding the whole scheme which could be quite expensive
          // we get the property from the type and then only instantiate it.
          let t = arrayScheme.Type
          let! prop = getPropType ctx env t key optChain

          let! prop =
            instantiateType ctx prop arrayScheme.TypeParams (Some [ elem ])

          return prop
      // TODO: intersection types
      | _ ->
        return!
          Error(TypeError.NotImplemented $"TODO: lookup member on type - {t}")
    }

  // TODO: differentiate between getting and setting
  // - getting: property, method, getter, mapped
  // - setting: non-readonly property, setter, non-readonly mapped
  let inferMemberAccess
    // TODO: do the search first and then return the appropriate ObjTypeElem
    (key: PropName)
    (elems: list<ObjTypeElem>)
    : option<Type> =

    let elem =
      List.tryFind
        (fun (elem: ObjTypeElem) ->
          match elem with
          | Property { Name = name } -> name = key
          | Method(name, _) -> name = key
          | Getter(name, _) -> name = key
          | Setter(name, _) -> name = key
          | Mapped _ ->
            failwith
              "TODO: inferMemberAccess - mapped (check if key is subtype of Mapped.TypeParam)"
          | _ -> false)
        elems

    match elem with
    | Some elem ->
      match elem with
      | Property p ->
        match p.Optional with
        | true ->
          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          Some(union [ p.Type; undefined ])
        | false -> Some p.Type
      | Method(_, fn) ->
        // TODO: check if the receiver is mutable or not
        let t =
          { Kind = TypeKind.Function fn
            Provenance = None }

        Some t
      | Getter(name, fn) ->
        // TODO: check if it's an lvalue
        // TODO: handle throws
        Some fn.Return
      | Setter(name, fn) ->
        // TODO: check if it's an rvalue
        // TODO: handle throws
        Some fn.ParamList[0].Type
      | Mapped mapped -> failwith "TODO: inferMemberAccess - mapped"
      | Callable _ -> failwith "Callable signatures don't have a name"
      | Constructor _ -> failwith "Constructor signatures don't have a name"
    | None -> None

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
        | TypeAnnKind.Object { Elems = elems; Immutable = immutable } ->
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
                  | ObjTypeAnnElem.Method { Name = name; Type = t } ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Getter { Name = name
                                            ReturnType = returnType
                                            Throws = throws } ->
                    return! Error(TypeError.NotImplemented "todo")
                  | ObjTypeAnnElem.Setter { Name = name
                                            Param = param
                                            Throws = throws } ->
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

          return TypeKind.Object { Elems = elems; Immutable = immutable }
        | TypeAnnKind.Tuple { Elems = elems; Immutable = immutable } ->
          let! elems = List.traverseResultM (inferTypeAnn ctx env) elems
          return TypeKind.Tuple { Elems = elems; Immutable = immutable }
        | TypeAnnKind.Union types ->
          let! types = List.traverseResultM (inferTypeAnn ctx env) types
          return (union types).Kind
        | TypeAnnKind.Intersection types ->
          let! types = List.traverseResultM (inferTypeAnn ctx env) types
          return TypeKind.Intersection types
        | TypeAnnKind.TypeRef { Ident = name; TypeArgs = typeArgs } ->
          match env.GetScheme name with
          | Ok scheme ->
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
              // TODO: check if scheme required type args
              return
                { Name = name
                  TypeArgs = None
                  Scheme = scheme }
                |> TypeKind.TypeRef
          | Error errorValue ->
            match name with
            | QualifiedIdent.Ident "_" -> return TypeKind.Wildcard
            | _ ->
              return! Error(TypeError.SemanticError $"{name} is not in scope")
        // match env.Schemes.TryFind(name) with
        // | Some(scheme) ->
        //
        //   let scheme =
        //     match scheme.IsTypeParam with
        //     | true -> None
        //     | false -> Some(scheme)
        //
        //   match typeArgs with
        //   | Some(typeArgs) ->
        //     let! typeArgs =
        //       List.traverseResultM (inferTypeAnn ctx env) typeArgs
        //
        //     return
        //       { Name = name
        //         TypeArgs = Some(typeArgs)
        //         Scheme = scheme }
        //       |> TypeKind.TypeRef
        //   | None ->
        //     // TODO: check if scheme required type args
        //     return
        //       { Name = name
        //         TypeArgs = None
        //         Scheme = scheme }
        //       |> TypeKind.TypeRef
        // | None ->
        //   if name = "_" then
        //     return TypeKind.Wildcard
        //   else
        //     return! Error(TypeError.SemanticError $"{name} is not in scope")
        | TypeAnnKind.Function functionType ->
          let! f = inferFunctionType ctx env functionType
          return TypeKind.Function(f)
        | TypeAnnKind.Keyof target ->
          return! inferTypeAnn ctx env target |> Result.map TypeKind.KeyOf
        | TypeAnnKind.Rest target ->
          return! inferTypeAnn ctx env target |> Result.map TypeKind.Rest
        | TypeAnnKind.Typeof target ->
          let! t = getQualifiedIdentType ctx env target
          return t.Kind
        | TypeAnnKind.Index(target, index) ->
          let! target = inferTypeAnn ctx env target
          let! index = inferTypeAnn ctx env index
          return TypeKind.Index(target, index)
        | TypeAnnKind.Condition conditionType ->
          let! check = inferTypeAnn ctx env conditionType.Check
          let! extends = inferTypeAnn ctx env conditionType.Extends
          let infers = findInfers extends

          // Adds placeholder types ot the environment so that we'll be able to
          // reference them in `trueType` and `falseType`.  They will be replaced
          // by `expandType` later.
          let mutable newEnv = env

          for infer in infers do
            let unknown =
              { Kind = TypeKind.Keyword Keyword.Unknown
                Provenance = None }

            let scheme =
              { TypeParams = None
                Type = unknown
                IsTypeParam = true }

            newEnv <- newEnv.AddScheme infer scheme

          let! trueType = inferTypeAnn ctx newEnv conditionType.TrueType
          let! falseType = inferTypeAnn ctx newEnv conditionType.FalseType

          return
            TypeKind.Condition
              { Check = check
                Extends = extends
                TrueType = trueType
                FalseType = falseType }
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
      let! typeParams, newEnv = inferTypeParams ctx env functionType.TypeParams
      let! returnType = inferTypeAnn ctx newEnv functionType.ReturnType

      let! throws =
        match functionType.Throws with
        | Some(throws) -> inferTypeAnn ctx newEnv throws
        | None ->
          Result.Ok(
            { Kind = TypeKind.Keyword Keyword.Never
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
          Self = None
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

    // TODO: update to return a result
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
              | QualifiedIdent.Ident "string" -> strType
              | QualifiedIdent.Ident "number" -> numType
              | QualifiedIdent.Ident "boolean" -> boolType
              | _ -> failwith $"TODO: lookup type of {qi}"

            assertType
          | None -> ctx.FreshTypeVar None

        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(name, (t, isMut))
        t
      | PatternKind.Literal lit ->
        { Kind = TypeKind.Literal lit
          Provenance = None }
      | PatternKind.Object { Elems = elems; Immutable = immutable } ->
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

                // let t = ctx.FreshTypeVar None
                let t =
                  match assertion with
                  | Some qi ->
                    let assertType =
                      match qi with
                      | QualifiedIdent.Ident "string" -> strType
                      | QualifiedIdent.Ident "number" -> numType
                      | QualifiedIdent.Ident "boolean" -> boolType
                      | _ -> failwith $"TODO: lookup type of {qi}"

                    assertType
                  | None -> ctx.FreshTypeVar None

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
                    { Kind = infer_pattern_rec target |> TypeKind.Rest
                      Provenance = None }
                  )

                None)
            elems

        let objType =
          { Kind = TypeKind.Object { Elems = elems; Immutable = immutable }
            Provenance = None }

        match restType with
        | Some(restType) ->
          { Kind = TypeKind.Intersection([ objType; restType ])
            Provenance = None }
        | None -> objType
      | PatternKind.Struct { TypeRef = { Ident = name; TypeArgs = typeArgs }
                             Elems = elems } ->
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

                // let t = ctx.FreshTypeVar None
                let t =
                  match assertion with
                  | Some qi ->
                    let assertType =
                      match qi with
                      | QualifiedIdent.Ident "string" -> strType
                      | QualifiedIdent.Ident "number" -> numType
                      | QualifiedIdent.Ident "boolean" -> boolType
                      | _ -> failwith $"TODO: lookup type of {qi}"

                    assertType
                  | None -> ctx.FreshTypeVar None

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
                    { Kind = infer_pattern_rec target |> TypeKind.Rest
                      Provenance = None }
                  )

                None)
            elems

        let typeArgs =
          match typeArgs with
          | Some typeArgs ->
            List.traverseResultM
              (fun typeArg ->
                result {
                  let! typeArg = inferTypeAnn ctx env typeArg
                  return typeArg
                })
              typeArgs
            |> Result.map Some
          | _ -> Ok None

        let typeArgs =
          match typeArgs with
          | Ok typeArgs -> typeArgs
          | Error e -> failwith "inferPattern - Struct typeArgs"

        let scheme =
          match env.GetScheme name with
          | Ok scheme -> scheme
          | Error e -> failwith $"inferPattnern - Can't find scheme {name}"

        let typeRef: TypeRef =
          { Name = name
            TypeArgs = typeArgs
            Scheme = Some scheme }

        let objType =
          { Kind =
              TypeKind.Struct
                { TypeRef = typeRef
                  Elems = elems
                  Impls = [] }
            Provenance = None }

        match restType with
        | Some(restType) ->
          { Kind = TypeKind.Intersection([ objType; restType ])
            Provenance = None }
        | None -> objType

      | PatternKind.Tuple { Elems = elems; Immutable = immutable } ->
        let elems = List.map infer_pattern_rec elems

        { Kind = TypeKind.Tuple { Elems = elems; Immutable = immutable }
          Provenance = None }
      | PatternKind.Enum variant ->
        let argTypes =
          variant.Args |> Option.defaultValue [] |> List.map infer_pattern_rec

        // TODO: stop using QualifiedIdent for Enum variants
        let enumName, variantName =
          match variant.Ident with
          | QualifiedIdent.Member(QualifiedIdent.Ident qualifier, name) ->
            (qualifier, name)
          | _ -> failwith "This should never happen"

        match env.GetScheme(QualifiedIdent.Ident enumName) with
        | Ok scheme ->
          // TODO: handle type args
          match scheme.Type.Kind with
          | TypeKind.Union types ->
            let variantType =
              types
              |> List.tryFind (fun t ->
                match t.Kind with
                | TypeKind.EnumVariant v -> v.Name = variantName
                | _ -> false)

            match variantType with
            | Some t ->
              match t.Kind with
              | TypeKind.EnumVariant variant ->
                let paramTypes = variant.Types

                for t1, t2 in List.zip argTypes paramTypes do
                  let result = unify ctx env None t1 t2

                  match result with
                  | Ok resultValue -> ()
                  | Error errorValue ->
                    failwith $"Failed to unify {t1} and {t2}"
              | _ -> failwith "Can't find variant type in enum"

              t
            | None -> failwith "Can't find variant type in enum"
          | _ -> failwith "enum scheme type is not a union type"
        | Error errorValue -> failwith $"Can't find scheme for {variant.Ident}"

      | PatternKind.Wildcard { Assertion = assertion } ->
        match assertion with
        | Some qi ->
          let assertType =
            match qi with
            | QualifiedIdent.Ident "string" -> strType
            | QualifiedIdent.Ident "number" -> numType
            | QualifiedIdent.Ident "boolean" -> boolType
            | _ -> failwith $"TODO: lookup type of {qi}"

          assertType
        | None ->
          { Kind = TypeKind.Wildcard
            Provenance = None }
      | PatternKind.Rest pat ->
        { Kind = TypeKind.Rest(infer_pattern_rec pat)
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

      // TODO: We have to unify one pattern at a time, but in order to do
      // that we need to replace the type params in `exprType` with fresh types
      // and then union the results together afterwards.  We'll need to have
      // some sort of mapping to keep track of all of these type variables.

      // TODO: write a function that checks if something has type variables in it

      let mutable newExprTypes: list<Type> = []

      // Unify all pattern types with `exprType`
      if hasTypeVars exprType then
        for patternType in patternTypes do
          let newExprType = fresh ctx exprType
          do! unify ctx env None patternType newExprType
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
                let! _ = inferExpr ctx newEnv guard
                ()
              | None -> ()

              return! inferBlockOrExpr ctx newEnv case.Body
            })
          (List.zip cases assumps)


      if newExprTypes.IsEmpty then
        return patternTypes, bodyTypes
      else
        // TODO: simplify the union before unifying with `exprType`
        let t = union newExprTypes
        let t = simplifyUnion t
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
                          Provenance = None }
                    IsTypeParam = true }

                newEnv <- newEnv.AddScheme typeParam.Name scheme

                return typeParam
              })
            typeParams
          |> Result.map Some
        | None -> Ok None

      return typeParams, newEnv
    }

  let inferDecl
    (ctx: Ctx)
    (env: Env)
    (decl: Decl)
    (generalize: bool)
    : Result<Env, TypeError> =

    result {
      match decl.Kind with
      | DeclKind.VarDecl varDecl ->
        let! bindings = inferVarDecl ctx env varDecl

        let bindings =
          if generalize then generalizeBindings bindings else bindings

        return env.AddBindings bindings
      | DeclKind.EnumDecl { Name = name
                            TypeParams = _typeParams
                            Variants = variants } ->
        // TODO: handle type params in enums

        let never =
          { Kind = TypeKind.Keyword Keyword.Never
            Provenance = None }

        let! variants =
          List.traverseResultM
            (fun (variant: Syntax.EnumVariant) ->
              result {
                let name = variant.Name

                let! types =
                  List.traverseResultM
                    (fun typeAnn ->
                      result {
                        let! t = inferTypeAnn ctx env typeAnn
                        return t
                      })
                    variant.TypeAnns

                let variant =
                  { SymbolId = ctx.FreshUniqueId()
                    Name = name
                    Types = types }

                return variant
              })
            variants

        let elems =
          variants
          |> List.map (fun variant ->
            // TODO: make each function param generic
            let paramList: list<FuncParam> =
              variant.Types
              |> List.mapi (fun i t ->
                { Pattern =
                    Pattern.Identifier { Name = $"arg{i}"; IsMut = false }
                  Type = t
                  Optional = false })

            let retType =
              { Type.Kind = TypeKind.EnumVariant variant
                Provenance = None }

            ObjTypeElem.Method(
              PropName.String variant.Name,
              makeFunction None None paramList retType never
            ))

        let types =
          variants
          |> List.map (fun variant ->
            { Type.Kind = TypeKind.EnumVariant variant
              Provenance = None })

        let t = union types

        let scheme =
          { Type = t
            TypeParams = None
            IsTypeParam = false }

        printfn $"Adding enum {name} to env"

        let value =
          { Type.Kind = TypeKind.Object { Elems = elems; Immutable = false }
            Provenance = None }

        let mutable newEnv = env

        newEnv <- newEnv.AddScheme name scheme
        newEnv <- newEnv.AddValue name (value, false)

        return newEnv
      | DeclKind.TypeDecl typeDecl ->
        let! placeholder = inferTypeDeclScheme ctx env typeDecl
        let mutable newEnv = env

        // Handles self-recursive types
        newEnv <- newEnv.AddScheme typeDecl.Name placeholder

        let! scheme = inferTypeDeclDefn ctx newEnv placeholder typeDecl.TypeAnn

        // Replace the placeholder's type with the actual type.
        // NOTE: This is a bit hacky and we may want to change this later to use
        // `foldType` to replace any uses of the placeholder with the actual type.
        placeholder.Type <- scheme.Type

        return newEnv.AddScheme typeDecl.Name scheme
      | DeclKind.StructDecl { Name = name
                              TypeParams = typeParams
                              Elems = elems } ->
        let mutable newEnv = env

        let! placeholderTypeParams =
          match typeParams with
          | None -> ResultOption.ofOption None
          | Some typeParams ->
            List.traverseResultM (inferTypeParam ctx newEnv) typeParams
            |> ResultOption.ofResult

        // TODO: add support for constraints on type params to aliases
        let placeholder =
          { TypeParams = placeholderTypeParams
            Type = ctx.FreshTypeVar None
            IsTypeParam = false }

        // Handles self-recursive types
        newEnv <- newEnv.AddScheme name placeholder

        let! typeParams =
          match typeParams with
          | None -> ResultOption.ofOption None
          | Some typeParams ->
            List.traverseResultM
              (fun (typeParam: Syntax.TypeParam) ->
                result {
                  let unknown =
                    { Kind = TypeKind.Keyword Keyword.Unknown
                      Provenance = None }

                  let scheme =
                    { TypeParams = None
                      Type = unknown
                      IsTypeParam = false }

                  newEnv <- newEnv.AddScheme typeParam.Name scheme
                  return! inferTypeParam ctx env typeParam
                })
              typeParams
            |> ResultOption.ofResult

        let! elems =
          elems
          |> List.traverseResultM
            (fun
                 { Name = name
                   Optional = optional
                   Readonly = readonly
                   TypeAnn = typeAnn } ->
              result {

                let name =
                  match name with
                  | Syntax.Ident s -> PropName.String s
                  | Syntax.String s -> PropName.String s
                  | Syntax.Number n -> PropName.Number n
                  | Computed expr ->
                    let t = inferExpr ctx newEnv expr
                    // TODO: check if `t` is a valid type for a PropName
                    failwith "TODO: inferStmt - Computed prop name"

                let! t = inferTypeAnn ctx newEnv typeAnn

                return
                  ObjTypeElem.Property
                    { Name = name
                      Optional = optional
                      Readonly = readonly
                      Type = t }
              })

        let typeRef: TypeRef =
          { Name = QualifiedIdent.Ident name
            TypeArgs = None // Should we be setting this to `Some(typeParams)`?
            Scheme = None }

        let t =
          { Kind =
              TypeKind.Struct
                { TypeRef = typeRef
                  Elems = elems
                  Impls = [] }
            Provenance = None }

        let scheme =
          { TypeParams = typeParams
            Type = t
            IsTypeParam = false }

        // Replace the placeholder's type with the actual type.
        // NOTE: This is a bit hacky and we may want to change this later to use
        // `foldType` to replace any uses of the placeholder with the actual type.
        placeholder.Type <- scheme.Type

        // TODO: add something to env.Values for the constructor so that
        // we can construct structs in JS/TS
        let newEnv = env.AddScheme name scheme

        // Add object with statics to env.Values
        let t =
          { Kind = TypeKind.Object { Elems = []; Immutable = false }
            Provenance = None }

        let newEnv = newEnv.AddValue name (t, false)

        return newEnv

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

        let! symbolIterator =
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

        let! _ = getPropType ctx env arrayScheme.Type propName false

        // TODO: add a variant of `ExpandType` that allows us to specify a
        // predicate that can stop the expansion early.
        let! expandedRightType = expandType ctx env None Map.empty rightType

        let! elemType =
          result {
            match expandedRightType.Kind with
            | TypeKind.Array { Elem = elem; Length = length } -> return elem
            | TypeKind.Tuple { Elems = elems } -> return union elems
            | TypeKind.Range _ -> return expandedRightType
            | TypeKind.Object _ ->
              // TODO: try using unify and/or an utility type to extract the
              // value type from an iterator

              // TODO: add a `tryGetPropType` function that returns an option
              let! next =
                getPropType ctx env rightType (PropName.String "next") false

              match next.Kind with
              | TypeKind.Function f ->
                return!
                  getPropType ctx env f.Return (PropName.String "value") false
              | _ ->
                return!
                  Error(
                    TypeError.SemanticError $"{rightType} is not an iterator"
                  )
            | _ ->
              return!
                Error(
                  TypeError.NotImplemented
                    "TODO: for loop over non-iterable type"
                )
          }

        do! unify ctx env None elemType patType

        let mutable newEnv = env

        for KeyValue(name, binding) in patBindings do
          newEnv <- newEnv.AddValue name binding

        let! _ = inferBlock ctx newEnv block
        return env
      | StmtKind.Decl decl -> return! inferDecl ctx env decl generalize
      | StmtKind.Impl { TypeParams = typeParams
                        Self = structName
                        Elems = elems } ->

        // TODO: instantiate the scheme (apply the type args)
        let scheme =
          match env.Schemes.TryFind structName with
          | Some scheme -> scheme
          | None -> failwith $"Struct {structName} not in scope"

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

        let typeArgs =
          typeParams
          |> Option.map (fun typeParams ->
            typeParams
            |> List.map (fun name ->
              { Kind =
                  TypeKind.TypeRef
                    { Name = QualifiedIdent.Ident name
                      TypeArgs = None
                      Scheme = None }
                Provenance = None }))

        newEnv <- newEnv.AddScheme structName scheme

        let selfType =
          { Kind =
              TypeKind.TypeRef
                { Name = QualifiedIdent.Ident structName
                  TypeArgs = typeArgs
                  Scheme = Some scheme }
            Provenance = None }

        let selfScheme =
          { TypeParams = None
            Type = selfType
            IsTypeParam = false }

        newEnv <- newEnv.AddScheme "Self" selfScheme

        let mutable instanceTuples
          : list<ObjTypeElem * FuncSig<TypeAnn option> * BlockOrExpr> =
          []

        let mutable staticTuples
          : list<ObjTypeElem * FuncSig<TypeAnn option> * BlockOrExpr> =
          []

        for elem in elems do
          match elem with
          | ImplElem.Method { Name = name
                              Sig = fnSig
                              Body = body } ->
            let! placeholderFn = inferFuncSig ctx newEnv fnSig

            match fnSig.Self with
            | None ->
              staticTuples <-
                (ObjTypeElem.Method(PropName.String name, placeholderFn),
                 fnSig,
                 body)
                :: staticTuples
            | Some _ ->
              instanceTuples <-
                (ObjTypeElem.Method(PropName.String name, placeholderFn),
                 fnSig,
                 body)
                :: instanceTuples
          | ImplElem.Getter { Name = name
                              Self = self
                              ReturnType = retType
                              Body = body } ->
            // TODO: handle static getters
            let fnSig: FuncSig<option<TypeAnn>> =
              { TypeParams = None
                Self = Some self
                ParamList = []
                ReturnType = retType
                Throws = None
                IsAsync = false }

            let! placeholderFn = inferFuncSig ctx newEnv fnSig

            instanceTuples <-
              (ObjTypeElem.Getter(PropName.String name, placeholderFn),
               fnSig,
               body)
              :: instanceTuples
          | ImplElem.Setter { Name = name
                              Self = self
                              Param = param
                              Body = body } ->
            // TODO: handle static setters
            let fnSig: FuncSig<option<TypeAnn>> =
              { TypeParams = None
                Self = Some self
                ParamList = [ param ]
                ReturnType = None
                Throws = None
                IsAsync = false }

            let! placeholderFn = inferFuncSig ctx newEnv fnSig

            instanceTuples <-
              (ObjTypeElem.Setter(PropName.String name, placeholderFn),
               fnSig,
               body)
              :: instanceTuples

        match scheme.Type.Kind with
        | TypeKind.Struct strct ->
          // Build temporary impl from placeholder method signatures
          let mutable instanceElems: list<ObjTypeElem> = []
          let mutable staticElems: list<ObjTypeElem> = []

          for elem, _, _ in instanceTuples do
            match elem with
            | Method(name, placeholderFn) ->
              instanceElems <-
                ObjTypeElem.Method(name, placeholderFn) :: instanceElems
            | Getter(name, placeholderFn) ->
              instanceElems <-
                ObjTypeElem.Getter(name, placeholderFn) :: instanceElems
            | Setter(name, placeholderFn) ->
              instanceElems <-
                ObjTypeElem.Setter(name, placeholderFn) :: instanceElems

          for elem, _, _ in staticTuples do
            match elem with
            | Method(name, placeholderFn) ->
              staticElems <-
                ObjTypeElem.Method(name, placeholderFn) :: staticElems
            | Getter(name, placeholderFn) ->
              staticElems <-
                ObjTypeElem.Getter(name, placeholderFn) :: staticElems
            | Setter(name, placeholderFn) ->
              staticElems <-
                ObjTypeElem.Setter(name, placeholderFn) :: staticElems

          let impl: Impl =
            { Elems = instanceElems
              Immutable = false }

          let strct =
            { strct with
                Impls = impl :: strct.Impls }

          // Update the `Self` scheme to include the new impl
          scheme.Type <-
            { scheme.Type with
                Kind = TypeKind.Struct strct }

          // Infer the bodies of each method body
          for elem, fnSig, body in instanceTuples do
            let placeholderFn =
              match elem with
              | Method(_, placeholderFn) -> placeholderFn
              | Getter(_, placeholderFn) -> placeholderFn
              | Setter(_, placeholderFn) -> placeholderFn

            let! _ = inferFuncBody ctx newEnv fnSig placeholderFn body
            ()

          for elem, fnSig, body in staticTuples do
            let placeholderFn =
              match elem with
              | Method(_, placeholderFn) -> placeholderFn
              | Getter(_, placeholderFn) -> placeholderFn
              | Setter(_, placeholderFn) -> placeholderFn

            let! _ = inferFuncBody ctx newEnv fnSig placeholderFn body
            ()

          let! oldStaticObjElems =
            match newEnv.GetBinding structName with
            | Ok(t, _) ->
              match t.Kind with
              | TypeKind.Object { Elems = elems } -> Result.Ok elems
              | _ ->
                Result.Error(
                  TypeError.SemanticError $"{structName} is not an object"
                )
            | Error _ ->
              Result.Error(TypeError.SemanticError $"{structName} not found")

          let staticObjType =
            { Kind =
                TypeKind.Object
                  { Elems = staticElems @ oldStaticObjElems
                    Immutable = false }
              Provenance = None }

          // Update the static object type for the struct with the new static methods
          let newEnv = newEnv.AddValue structName (staticObjType, false)

          // Update the scheme for the original named struct
          let newEnv = newEnv.AddScheme structName scheme

          return newEnv
        | _ ->
          return! Error(TypeError.SemanticError $"{structName} is not a struct")
      | StmtKind.Return expr ->
        match expr with
        | Some(expr) ->
          let! _ = inferExpr ctx env expr
          return env
        | None -> return env
    }

  let inferVarDecl
    (ctx: Ctx)
    (env: Env)
    (varDecl: VarDecl)
    : Result<Map<string, Binding>, TypeError> =

    let { Pattern = pattern
          Init = init
          TypeAnn = typeAnn } =
      varDecl

    result {
      let! invariantPaths =
        checkMutability
          (getPatBindingPaths pattern)
          (getExprBindingPaths env init)

      let! patBindings, patType = inferPattern ctx env pattern
      let mutable newEnv = env

      for KeyValue(name, binding) in patBindings do
        newEnv <- newEnv.AddValue name binding

      let! initType = inferExpr ctx newEnv init

      match typeAnn with
      | Some(typeAnn) ->
        let! typeAnnType = inferTypeAnn ctx newEnv typeAnn
        do! unify ctx newEnv invariantPaths initType typeAnnType
        do! unify ctx newEnv None typeAnnType patType
      | None -> do! unify ctx newEnv invariantPaths initType patType

      return patBindings

    // let bindings =
    //   if generalize then
    //     generalizeBindings patBindings
    //   else
    //     patBindings
    //
    // return env.AddBindings bindings
    }

  let generalizeBindings
    (bindings: Map<string, Binding>)
    : Map<string, Binding> =
    let mutable newBindings = Map.empty

    for KeyValue(name, (t, isMut)) in bindings do
      let t =
        match (prune t).Kind with
        | TypeKind.Function f ->
          { t with
              Kind = generalizeFunc f |> TypeKind.Function }
        | _ -> t

      newBindings <- newBindings.Add(name, (t, isMut))

    newBindings

  let inferTypeDeclScheme
    (ctx: Ctx)
    (env: Env)
    (typeDecl: TypeDecl)
    : Result<Scheme, TypeError> =

    let { Name = name
          TypeAnn = typeAnn
          TypeParams = typeParams } =
      typeDecl

    result {
      let! typeParams, _ = inferTypeParams ctx env typeParams

      let scheme =
        { TypeParams = typeParams
          Type = ctx.FreshTypeVar None
          IsTypeParam = false }

      return scheme
    }

  let inferTypeDeclDefn
    (ctx: Ctx)
    (env: Env)
    (scheme: Scheme)
    (typeAnn: TypeAnn)
    : Result<Scheme, TypeError> =

    result {
      let mutable newEnv = env

      match scheme.TypeParams with
      | None -> ()
      | Some typeParams ->
        for typeParam in typeParams do
          let unknown =
            { Kind = TypeKind.Keyword Keyword.Unknown
              Provenance = None }

          newEnv <-
            newEnv.AddScheme
              typeParam.Name
              { TypeParams = None
                Type = unknown
                IsTypeParam = true }

      let! t = inferTypeAnn ctx newEnv typeAnn
      return { scheme with Type = t }
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
    (filename: string)
    (import: Import)
    : Result<Env, TypeError> =

    result {
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
        | ModuleAlias name ->
          let ns: Namespace =
            { Name = name
              Values = exports.Values
              Schemes = exports.Schemes
              Namespaces = exports.Namespaces }

          imports <- imports.AddNamespace name ns

      return
        { env with
            Values = FSharpPlus.Map.union env.Values imports.Values
            Schemes = FSharpPlus.Map.union env.Schemes imports.Schemes
            Namespaces = FSharpPlus.Map.union env.Namespaces imports.Namespaces }
    }

  let inferScriptItem
    (ctx: Ctx)
    (env: Env)
    (filename: string)
    (item: ScriptItem)
    (generalize: bool)
    : Result<Env, TypeError> =

    result {
      match item with
      | ScriptItem.Import import -> return! inferImport ctx env filename import
      | ScriptItem.DeclareLet(name, typeAnn) ->
        let! typeAnnType = inferTypeAnn ctx env typeAnn
        let! assump, patType = inferPattern ctx env name

        do! unify ctx env None typeAnnType patType

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
    (m: Script)
    : Result<Env, TypeError> =
    result {
      let mutable newEnv = env

      let! _ =
        List.traverseResultM
          (fun item ->
            result {
              let! itemEnv = inferScriptItem ctx newEnv filename item true
              newEnv <- itemEnv
            })
          m.Items

      return newEnv
    }

  let inferModule
    (ctx: Ctx)
    (env: Env)
    (filename: string)
    (m: Module)
    : Result<Env, TypeError> =
    result {
      let mutable newEnv = env
      let mutable prebindings: Map<string, Binding> = Map.empty
      let mutable typeDecls: Map<string, Scheme> = Map.empty

      for item in m.Items do
        match item with
        | Import import ->
          let! importEnv = inferImport ctx newEnv filename import
          newEnv <- importEnv
        | DeclareLet(name, typeAnn) -> failwith "todo"
        | Decl { Kind = kind } ->
          match kind with
          | VarDecl { Pattern = pattern
                      Init = init
                      TypeAnn = typeAnn } ->
            let! bindings, _ = inferPattern ctx env pattern

            for KeyValue(name, binding) in bindings do
              prebindings <- prebindings.Add(name, binding)
              newEnv <- newEnv.AddValue name binding
          | TypeDecl typeDecl ->
            // TODO: replace placeholders, with a reference the actual definition
            // once we've inferred the definition
            let! placeholder = inferTypeDeclScheme ctx env typeDecl
            typeDecls <- typeDecls.Add(typeDecl.Name, placeholder)

            // TODO: update AddScheme to return a Result<Env, TypeError> and
            // return an error if the name already exists since we can't redefine
            // types.
            newEnv <- newEnv.AddScheme typeDecl.Name placeholder
          | StructDecl structDecl -> failwith "todo"

      // TODO: add pre-bindings to the environment before inferring the initializers

      let mutable bindings: Map<string, Binding> = Map.empty

      for item in m.Items do
        match item with
        | Import _ -> () // handled in the first pass
        | DeclareLet(name, typeAnn) -> failwith "todo"
        | Decl { Kind = kind } ->
          match kind with
          | VarDecl varDecl ->
            // NOTE: We explicitly don't generalize here because we want other
            // declarations to be able to unify with any free type variables
            // from this declaration.  We generalize things below.
            let! newBindings = inferVarDecl ctx newEnv varDecl
            bindings <- FSharpPlus.Map.union bindings newBindings
          | TypeDecl { Name = name; TypeAnn = typeAnn } ->
            let scheme =
              match typeDecls.TryFind name with
              | None -> failwith "todo"
              | Some value -> value

            // Handles self-recursive types
            newEnv <- newEnv.AddScheme name scheme

            let! scheme = inferTypeDeclDefn ctx newEnv scheme typeAnn
            newEnv <- newEnv.AddScheme name scheme
          | StructDecl structDecl -> failwith "todo"

      // Unify each binding with its prebinding
      for KeyValue(name, binding) in bindings do

        match prebindings.TryFind(name) with
        | Some prebinding ->
          // QUESTION: Which direction should we unify in?
          let (t1, _) = prebinding
          let (t2, _) = binding
          do! unify ctx newEnv None t1 t2
        | None -> ()

      // Prune any functions before generalizing, this avoids
      // issues with mutually recursive functions being generalized
      // prematurely.
      // for binding in bindings.values() {
      //     let pruned_index = self.prune(binding.index);
      //     self.bind(ctx, binding.index, pruned_index)?;
      // }

      // Generalize any functions.
      let bindings = generalizeBindings bindings
      return newEnv.AddBindings bindings
    }

  let findReturns (body: BlockOrExpr) : list<Expr> =
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

    match body with
    | BlockOrExpr.Block block -> List.iter (walkStmt visitor) block.Stmts
    | BlockOrExpr.Expr expr ->
      walkExpr visitor expr // There might be early returns in match expression
      returns <- expr :: returns // We treat the expression as a return in this case

    returns

  let maybeWrapInPromise (t: Type) (e: Type) : Type =
    match t.Kind with
    | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Promise" } -> t
    | _ ->
      let never =
        { Kind = TypeKind.Keyword Keyword.Never
          Provenance = None }

      { Kind =
          TypeKind.TypeRef
            { Name = QualifiedIdent.Ident "Promise"
              TypeArgs = Some([ t; e ])
              Scheme = None }
        Provenance = None }

  // TODO: dedupe with findThrowsInBlock
  let findThrows (body: BlockOrExpr) : list<Type> =
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

    match body with
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

  let findModuleBindingNames (m: Script) : list<string> =
    let mutable names: list<string> = []

    for item in m.Items do
      match item with
      | Stmt stmt ->
        match stmt.Kind with
        | StmtKind.Decl({ Kind = DeclKind.VarDecl { Pattern = pattern } }) ->
          names <- List.concat [ names; findBindingNames pattern ]
        | _ -> ()
      | _ -> ()

    names

  // TODO: dedupe with findInfers in Env.fs
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

  let hasTypeVars (t: Type) : bool =
    let mutable hasTypeVars = false

    let visitor =
      fun (t: Type) ->
        match t.Kind with
        | TypeKind.TypeVar _ -> hasTypeVars <- true
        | _ -> ()

    TypeVisitor.walkType visitor (prune t)

    hasTypeVars

  let fresh (ctx: Ctx) (t: Type) : Type =

    let folder: Type -> option<Type> =
      fun t ->
        match t.Kind with
        | TypeKind.TypeVar _ -> Some(ctx.FreshTypeVar None)
        | _ -> None

    Folder.foldType folder t

  let simplifyUnion (t: Type) : Type =
    match (prune t).Kind with
    | TypeKind.Union types ->
      let objTypes, otherTypes =
        List.partition
          (fun t ->
            match t.Kind with
            | TypeKind.Object _ -> true
            | _ -> false)
          types

      if objTypes.IsEmpty then
        t
      else
        // Collect the types of each named property into a map of lists
        let mutable namedTypes: Map<string, list<Type>> = Map.empty

        // TODO: handle other object element types
        for objType in objTypes do
          match objType.Kind with
          | TypeKind.Object { Elems = elems; Immutable = immutable } ->
            for elem in elems do
              match elem with
              | ObjTypeElem.Property { Name = PropName.String name
                                       Optional = optional
                                       Readonly = readonly
                                       Type = t } ->

                let types =
                  match Map.tryFind name namedTypes with
                  | Some(types) -> types
                  | None -> []

                namedTypes <- Map.add name (t :: types) namedTypes
              | _ -> ()
          | _ -> ()

        // Simplify each list of types
        let namedTypes = namedTypes |> Map.map (fun name types -> union types)

        // Create a new object type from the simplified named properties
        let objTypeElems =
          namedTypes
          |> Map.map (fun name t ->
            ObjTypeElem.Property
              { Name = PropName.String name
                Optional = false
                Readonly = false
                Type = t })
          |> Map.values
          |> Seq.toList

        let objType =
          { Kind =
              TypeKind.Object
                { Elems = objTypeElems
                  Immutable = false }
            Provenance = None }

        union (objType :: otherTypes)
    | _ -> t


  let rec getQualifiedIdentType (ctx: Ctx) (env: Env) (ident: QualifiedIdent) =
    match ident with
    | QualifiedIdent.Ident name -> env.GetType name
    | QualifiedIdent.Member(left, right) ->
      result {
        let! left = getQualifiedIdentType ctx env left
        return! getPropType ctx env left (PropName.String right) false
      }

  let rec getLvalue
    (ctx: Ctx)
    (env: Env)
    (expr: Expr)
    : Result<Type * bool, TypeError> =
    result {
      match expr.Kind with
      | ExprKind.Identifier name -> return! env.GetBinding name
      | ExprKind.Index(target, index, optChain) ->
        // TODO: disallow optChain in lvalues
        let! target, isMut = getLvalue ctx env target
        let! index = inferExpr ctx env index

        let key =
          match index.Kind with
          | TypeKind.Literal(Literal.Number i) -> PropName.Number i
          | TypeKind.Literal(Literal.String s) -> PropName.String s
          | TypeKind.UniqueSymbol id -> PropName.Symbol id
          | _ ->
            printfn "index = %A" index
            failwith $"TODO: index can't be a {index}"

        let! t = getPropType ctx env target key false
        return t, isMut
      | ExprKind.Member(target, name, optChain) ->
        // TODO: check if `target` is a namespace
        // If the target is either an Identifier or another Member, we
        // can try to look look for a namespace for it.

        // TODO: disallow optChain in lvalues
        let! target, isMut = getLvalue ctx env target
        let! t = getPropType ctx env target (PropName.String name) false
        return t, isMut
      | _ ->
        return! Error(TypeError.SemanticError $"{expr} is not a valid lvalue")
    }

  let rec getIsMut
    (ctx: Ctx)
    (env: Env)
    (expr: Expr)
    : Result<bool, TypeError> =
    result {
      match expr.Kind with
      | ExprKind.Identifier name ->
        let! _, isMut = env.GetBinding name
        return isMut
      | ExprKind.Index(target, index, optChain) ->
        return! getIsMut ctx env target
      | ExprKind.Member(target, name, optChain) ->
        return! getIsMut ctx env target
      | _ ->
        return! Error(TypeError.SemanticError $"{expr} is not a valid lvalue")
    }


  let unifyCall
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (args: list<Syntax.Expr>)
    (typeArgs: option<list<Type>>)
    (callee: Type)
    : Result<(Type * Type), TypeError> =

    result {
      let callee = prune callee

      match callee.Kind with
      | TypeKind.Function func ->
        return! unifyFuncCall ctx env ips args typeArgs func
      | TypeKind.Intersection types ->
        let mutable result = None

        for t in types do
          match t.Kind with
          | TypeKind.Function func ->
            match unifyCall ctx env ips args typeArgs t with
            | Result.Ok value ->
              match result with
              | Some _ -> ()
              | None -> result <- Some(value)
            | Result.Error _ -> ()
          | _ -> ()

        match result with
        | Some(value) -> return value
        | None ->
          return! Error(TypeError.NotImplemented $"kind = {callee.Kind}")
      | TypeKind.TypeVar _ ->

        // TODO: use a `result {}` CE here
        let! argTypes = List.traverseResultM (inferExpr ctx env) args

        let paramList =
          List.mapi
            (fun i t ->
              let name = $"arg{i}"

              let p: Pattern =
                Pattern.Identifier { Name = name; IsMut = false }

              { Pattern = p
                Type = t
                Optional = false })
            argTypes

        let retType = ctx.FreshTypeVar None
        let throwsType = ctx.FreshTypeVar None

        let callType =
          { Type.Kind =
              TypeKind.Function
                { ParamList = paramList
                  Self = None // TODO: pass in the receiver if this is a method call
                  Return = retType
                  Throws = throwsType
                  TypeParams = None } // TODO
            Provenance = None }

        match bind ctx env ips callee callType with
        | Ok _ -> return (prune retType, prune throwsType)
        | Error e -> return! Error e
      | kind -> return! Error(TypeError.NotImplemented $"kind = {kind}")
    }

  let unifyFuncCall
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (args: list<Syntax.Expr>)
    (typeArgs: option<list<Type>>)
    (callee: Function)
    : Result<Type * Type, TypeError> =

    result {
      let! callee =
        result {
          if callee.TypeParams.IsSome then
            return! instantiateFunc ctx callee typeArgs
          else
            return callee
        }

      let! args =
        List.traverseResultM
          (fun arg ->
            result {
              let! argType = inferExpr ctx env arg
              return arg, argType
            })
          args

      // TODO: require the optional params come after the required params
      // TODO: require that if there is a rest param, it comes last
      let optionalParams, requiredParams =
        callee.ParamList
        |> List.partition (fun p ->
          match p.Pattern with
          | Rest _ -> true
          | _ -> p.Optional)

      if args.Length < requiredParams.Length then
        // TODO: make this into a diagnostic instead of an error
        return!
          Error(
            TypeError.SemanticError "function called with too few arguments"
          )

      let requiredArgs, optionalArgs = List.splitAt requiredParams.Length args

      for (arg, argType), param in List.zip requiredArgs requiredParams do
        if
          param.Optional && argType.Kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          let! invariantPaths =
            checkMutability
              (getTypePatBindingPaths param.Pattern)
              (getExprBindingPaths env arg)

          match unify ctx env invariantPaths argType param.Type with
          | Ok _ -> ()
          | Error(reason) ->
            let never =
              { Kind = TypeKind.Keyword Keyword.Never
                Provenance = None }

            do! unify ctx env ips never param.Type

            ctx.AddDiagnostic(
              { Description =
                  $"arg type '{argType}' doesn't satisfy param '{param.Pattern}' type '{param.Type}' in function call"
                Reasons = [ reason ] }
            )

      let optionalParams, restParams =
        optionalParams
        |> List.partition (fun p ->
          match p.Pattern with
          | Rest _ -> false
          | _ -> true)

      let! restParam =
        match restParams with
        | [] -> Result.Ok None
        | [ restParam ] -> Result.Ok(Some(restParam))
        | _ -> Error(TypeError.SemanticError "Too many rest params!")

      let restArgs =
        match restParam with
        | None -> None
        | Some _ ->
          if optionalArgs.Length > optionalParams.Length then
            Some(List.skip optionalParams.Length optionalArgs)
          else
            Some []

      // Functions can be passed more args than parameters as well as
      // fewer args that the number optional params.  We handle both
      // cases here.
      let minLength = min optionalArgs.Length optionalParams.Length
      let optionalParams = List.take minLength optionalParams
      let optionalArgs = List.take minLength optionalArgs

      for (arg, argType), param in List.zip optionalArgs optionalParams do
        if
          param.Optional && argType.Kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          let! invariantPaths =
            checkMutability
              (getTypePatBindingPaths param.Pattern)
              (getExprBindingPaths env arg)

          match unify ctx env invariantPaths argType param.Type with
          | Ok _ -> ()
          | Error(reason) ->
            let never =
              { Kind = TypeKind.Keyword Keyword.Never
                Provenance = None }

            do! unify ctx env ips never param.Type

            ctx.AddDiagnostic(
              { Description =
                  $"arg type '{argType}' doesn't satisfy param '{param.Pattern}' type '{param.Type}' in function call"
                Reasons = [ reason ] }
            )

      match restArgs, restParam with
      | Some args, Some param ->
        let args = List.map (fun (arg, argType) -> argType) args

        let tuple =
          { Kind = TypeKind.Tuple { Elems = args; Immutable = false }
            Provenance = None }

        do! unify ctx env ips tuple param.Type
      | _ -> ()

      return (callee.Return, callee.Throws)
    }
