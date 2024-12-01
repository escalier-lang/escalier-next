namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.Data.Visitor

open Error
open Prune
open Env
open Mutability
open Poly
open Unify
open Helpers

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
                                                   Assertion = _ } ->
                  ObjPatElem.ShorthandPat
                    { Name = name
                      Init = init
                      IsMut = mut }
                | Syntax.ObjPatElem.RestPat { Target = target; IsMut = _ } ->
                  // TODO: isMut
                  ObjPatElem.RestPat(patternToPattern target))
              elems
          Immutable = immutable }
    | PatternKind.Tuple { Elems = elems; Immutable = immutable } ->
      Pattern.Tuple
        { Elems = List.map (patternToPattern >> Some) elems
          Immutable = immutable }
    | PatternKind.Wildcard { Assertion = _ } -> Pattern.Wildcard
    | PatternKind.Literal lit -> Pattern.Literal lit
    | PatternKind.Rest rest -> Pattern.Rest(patternToPattern rest)
    | PatternKind.Enum _ -> failwith "TODO: patternToPattern - PatternKind.Enum"

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

  // TODO: support inferring class declarations without method bodies as long
  // as the methods are fully typed.
  let inferClass
    (ctx: Ctx)
    (env: Env)
    (cls: Class)
    (declare: bool)
    : Result<Type * Scheme, TypeError> =
    result {
      let className =
        match cls.Name with
        | Some name -> name
        | None -> "AnonymousClass" // TODO: make this unique

      let mutable newEnv = env

      let! placeholderTypeParams =
        match cls.TypeParams with
        | None -> ResultOption.ofOption None
        | Some typeParams ->
          List.traverseResultM (inferTypeParam ctx newEnv) typeParams
          |> ResultOption.ofResult

      // TODO: add support for constraints on type params to aliases
      let placeholder =
        { TypeParams = placeholderTypeParams
          Type = ctx.FreshTypeVar None None }

      newEnv <- newEnv.AddScheme className placeholder

      let typeArgs =
        cls.TypeParams
        |> Option.map (fun typeParams ->
          typeParams
          |> List.map (fun typeParam ->
            { Kind =
                TypeKind.TypeRef
                  { Name = QualifiedIdent.Ident typeParam.Name
                    TypeArgs = None
                    Scheme = None }
              Provenance = None }))

      let selfType =
        { Kind =
            TypeKind.TypeRef
              { Name = QualifiedIdent.Ident className
                TypeArgs = typeArgs
                Scheme = Some placeholder }
          Provenance = None }

      let selfScheme = { TypeParams = None; Type = selfType }

      // Handles self-recursive types
      newEnv <- newEnv.AddScheme "Self" selfScheme

      let! typeParams =
        match cls.TypeParams with
        | None -> ResultOption.ofOption None
        | Some typeParams ->
          List.traverseResultM
            (fun (typeParam: Syntax.TypeParam) ->
              result {
                // TODO: support constraints on type params
                let unknown =
                  { Kind = TypeKind.Keyword Keyword.Unknown
                    Provenance = None }

                let scheme = { TypeParams = None; Type = unknown }

                newEnv <- newEnv.AddScheme typeParam.Name scheme
                return! inferTypeParam ctx env typeParam
              })
            typeParams
          |> ResultOption.ofResult

      let mutable instanceMethods
        : list<ObjTypeElem * FuncSig * option<BlockOrExpr>> =
        []

      let mutable staticMethods
        : list<ObjTypeElem * FuncSig * option<BlockOrExpr>> =
        []

      let mutable instanceElems: list<ObjTypeElem> = []
      let mutable staticElems: list<ObjTypeElem> = []

      for elem in cls.Elems do
        match elem with
        | ClassElem.Property { Name = name
                               TypeAnn = typeAnn
                               Value = value
                               Optional = optional
                               Readonly = readonly
                               Static = isStatic } ->
          let! name = inferPropName ctx env name

          let! t =
            match typeAnn, value with
            | Some typeAnn, Some value ->
              // TODO: infer `value`'s type and then unify with `typeAnn`'s type
              inferTypeAnn ctx newEnv typeAnn
            | Some typeAnn, None -> inferTypeAnn ctx newEnv typeAnn
            | None, Some value -> inferExpr ctx newEnv None value
            | None, None -> Error(TypeError.SemanticError "Invalid property")

          let prop =
            ObjTypeElem.Property
              { Name = name
                Optional = optional
                Readonly = readonly
                Type = t }

          if isStatic then
            staticElems <- prop :: staticElems
          else
            instanceElems <- prop :: instanceElems
        | ClassElem.Constructor { Sig = fnSig; Body = body } ->
          let! placeholderFn = inferFuncSig ctx newEnv fnSig None

          let placeholderFn =
            { placeholderFn with
                Return = selfType
                TypeParams = typeParams }

          staticMethods <-
            (ObjTypeElem.Constructor placeholderFn, fnSig, body)
            :: staticMethods
        | ClassElem.Method { Name = name
                             Sig = fnSig
                             Body = body } ->
          let! placeholderFn = inferFuncSig ctx newEnv fnSig None
          let! name = inferPropName ctx env name

          // .d.ts files don't track whether a method throws or not so we default
          // to `never` for now.  In the future we may override the types for some
          // APIs that we know throw.
          if declare then
            placeholderFn.Throws <-
              { Kind = TypeKind.Keyword Keyword.Never
                Provenance = None }

          match fnSig.Self with
          | None ->
            staticMethods <-
              (ObjTypeElem.Method { Name = name; Fn = placeholderFn },
               fnSig,
               body)
              :: staticMethods
          | Some _ ->
            instanceMethods <-
              (ObjTypeElem.Method { Name = name; Fn = placeholderFn },
               fnSig,
               body)
              :: instanceMethods
        | ClassElem.Getter { Name = name
                             Self = self
                             ReturnType = retType
                             Body = body
                             Static = isStatic } ->
          let fnSig: FuncSig =
            { TypeParams = None
              Self = self
              ParamList = []
              ReturnType = retType
              Throws = None
              IsAsync = false }

          let! placeholderFn = inferFuncSig ctx newEnv fnSig None
          let! name = inferPropName ctx env name

          match self, isStatic with
          | None, true ->
            staticMethods <-
              (ObjTypeElem.Getter { Name = name; Fn = placeholderFn },
               fnSig,
               body)
              :: staticMethods
          | Some _, false ->
            instanceMethods <-
              (ObjTypeElem.Getter { Name = name; Fn = placeholderFn },
               fnSig,
               body)
              :: instanceMethods
          | _, _ -> failwith "Invalid getter"
        | ClassElem.Setter { Name = name
                             Self = self
                             Param = param
                             Body = body
                             Static = isStatic } ->
          let fnSig: FuncSig =
            { TypeParams = None
              Self = self
              ParamList = [ param ]
              ReturnType = None
              Throws = None
              IsAsync = false }

          let! placeholderFn = inferFuncSig ctx newEnv fnSig None
          let! name = inferPropName ctx env name

          match self, isStatic with
          | None, true ->
            staticMethods <-
              (ObjTypeElem.Setter { Name = name; Fn = placeholderFn },
               fnSig,
               body)
              :: staticMethods
          | Some _, false ->
            instanceMethods <-
              (ObjTypeElem.Setter { Name = name; Fn = placeholderFn },
               fnSig,
               body)
              :: instanceMethods
          | _, _ -> failwith "Invalid setter"

      for elem, _, _ in instanceMethods do
        match elem with
        | Method { Name = name; Fn = placeholderFn } ->
          instanceElems <-
            ObjTypeElem.Method { Name = name; Fn = placeholderFn }
            :: instanceElems
        | Getter { Name = name; Fn = placeholderFn } ->
          instanceElems <-
            ObjTypeElem.Getter { Name = name; Fn = placeholderFn }
            :: instanceElems
        | Setter { Name = name; Fn = placeholderFn } ->
          instanceElems <-
            ObjTypeElem.Setter { Name = name; Fn = placeholderFn }
            :: instanceElems
        | _ -> ()

      let mutable hasConstructor = false

      for elem, _, _ in staticMethods do
        match elem with
        | Constructor(placeholderFn) ->
          hasConstructor <- true

          staticElems <- ObjTypeElem.Constructor(placeholderFn) :: staticElems
        | Method { Name = name; Fn = placeholderFn } ->
          staticElems <-
            ObjTypeElem.Method { Name = name; Fn = placeholderFn }
            :: staticElems
        | Getter { Name = name; Fn = placeholderFn } ->
          staticElems <-
            ObjTypeElem.Getter { Name = name; Fn = placeholderFn }
            :: staticElems
        | Setter { Name = name; Fn = placeholderFn } ->
          staticElems <-
            ObjTypeElem.Setter { Name = name; Fn = placeholderFn }
            :: staticElems
        | _ -> ()

      let objType =
        { Kind =
            TypeKind.Object
              { Extends = None // QUESTION: Do we need this for the placeholder?
                Implements = None // TODO
                Elems = instanceElems
                Exact = false
                Immutable = false
                Interface = false }
          Provenance = None }

      placeholder.Type <- objType

      if not hasConstructor then
        let never =
          { Kind = TypeKind.Keyword Keyword.Never
            Provenance = None }

        let constructor =
          { TypeParams = typeParams
            Self = None
            ParamList = []
            Return = selfType
            Throws = never }

        staticElems <- ObjTypeElem.Constructor(constructor) :: staticElems

      // TODO: This static object should be added to the environment
      // sooner so that methods can construct new objects of this type.
      let staticObjType =
        { Kind =
            TypeKind.Object
              { Extends = None
                Implements = None
                Elems = staticElems
                Exact = true
                Immutable = false
                Interface = false }
          Provenance = None }

      // TODO: Make Type.Kind mutable so that we can modify the type after its
      // been created.
      newEnv <-
        newEnv.AddValue
          "Self"
          { Type = staticObjType
            Mutable = false
            Export = false }

      // Infer the bodies of each instance method body
      for elem, fnSig, body in instanceMethods do
        let placeholderFn =
          match elem with
          | Method { Fn = placeholderFn } -> placeholderFn
          | Getter { Fn = placeholderFn } -> placeholderFn
          | Setter { Fn = placeholderFn } -> placeholderFn
          | _ -> failwith "instanceMethods should only contain methods"

        match body, declare with
        | Some body, false ->
          // TODO: Generalize methods but only after inferring them all
          let! _ = inferFuncBody ctx newEnv fnSig placeholderFn body
          ()
        | Some _, true ->
          failwith
            "methods should not have a body when using declare with a class"
        | None, true -> ()
        | None, false ->
          failwith
            "methods should have a body when not using declare with a class"

      // Infer the bodies of each static method body
      for elem, fnSig, body in staticMethods do
        match elem with
        | Method { Fn = placeholderFn } ->
          match body, declare with
          | Some body, false ->
            let! _ = inferFuncBody ctx newEnv fnSig placeholderFn body
            ()
          | _ -> () // TODO: handle other cases correctly
        | Getter { Fn = placeholderFn } ->
          match body, declare with
          | Some body, false ->
            let! _ = inferFuncBody ctx newEnv fnSig placeholderFn body
            ()
          | _ -> () // TODO: handle other cases correctly
        | Setter { Fn = placeholderFn } ->
          match body, declare with
          | Some body, false ->
            let! _ = inferFuncBody ctx newEnv fnSig placeholderFn body
            ()
          | _ -> () // TODO: handle other cases correctly
        | Constructor placeholderFn ->
          match body, declare with
          | Some body, false ->
            // Constructors are special. When calling a constructor, an
            // instance of the class is created and returned. This is
            // done automatically by the language, so we don't need an
            // explicit `return` statement in the constructor body. We
            // set the return type of `placeholderFn` to be `undefined`
            // before inferring the body so to avoid needing the `return`
            // statement.
            let placeholderFn =
              { placeholderFn with
                  Return =
                    { Kind = TypeKind.Literal(Literal.Undefined)
                      Provenance = None } }

            // TODO: find all assignment expressions in the body and
            // ensure that they're all assignments to `self` properties

            let mutable assignedProps: list<string> = []
            let mutable methodsCalled: list<string> = []

            let visitor: ExprVisitor.SyntaxVisitor<unit> =
              { ExprVisitor.VisitExpr =
                  fun (expr: Expr, state) ->
                    match expr.Kind with
                    | ExprKind.Assign { Left = left } ->
                      match left.Kind with
                      | ExprKind.Member { Target = obj; Name = prop } ->
                        match obj.Kind with
                        | ExprKind.Identifier { Name = "self" } ->
                          assignedProps <- prop :: assignedProps
                          (true, state)
                        | _ -> (true, state)
                      | _ -> (true, state)
                    | ExprKind.Call { Callee = callee } ->
                      match callee.Kind with
                      | ExprKind.Member { Target = obj; Name = prop } ->
                        match obj.Kind with
                        | ExprKind.Identifier { Name = "self" } ->
                          methodsCalled <- prop :: methodsCalled
                          (true, state)
                        | _ -> (true, state)
                      | _ -> (true, state)
                    | _ -> (true, state)

                ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
                ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
                ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
                ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
                ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
                ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
                ExprVisitor.VisitTypeAnnObjElem =
                  fun (_, state) -> (false, state) }

            match body with
            | BlockOrExpr.Block block ->
              List.iter (ExprVisitor.walkStmt visitor ()) block.Stmts
            | BlockOrExpr.Expr _expr -> failwith "TODO"

            if not methodsCalled.IsEmpty then
              ctx.Report.AddDiagnostic
                { Description =
                    $"Methods called in constructor: {methodsCalled}"
                  Reasons = [] }

            let instanceProps =
              instanceElems
              |> List.choose (function
                | Property { Name = PropName.String name } -> Some name
                | _ -> None)

            let unassignedProps =
              instanceProps
              |> List.filter (fun p -> not (List.contains p assignedProps))

            if not unassignedProps.IsEmpty then
              ctx.Report.AddDiagnostic(
                { Description =
                    $"Unassigned properties in constructor: {unassignedProps}"
                  Reasons = [] }
              )

            // TODO: Check that we aren't using any properties before
            // they've been assigned.

            let! _ = inferFuncBody ctx newEnv fnSig placeholderFn body

            ()
          | _ -> () // TODO: handle other cases correctly
        | _ -> printfn "elem = %A" elem

      let instanceElems =
        List.map
          (fun elem ->
            match elem with
            | Method { Name = name; Fn = fn } ->
              ObjTypeElem.Method { Name = name; Fn = generalizeFunc fn }
            | _ -> elem)
          instanceElems

      // TODO: do the same thing we do in inferTypeDeclDefn in order to add
      // schemes for each type param before inferring the extends clause that
      // consumes those schemes.

      let getType (env: Env) : Result<Type, TypeError> =
        result {

          let! extends =
            match cls.Extends with
            | Some typeRef ->
              result {

                let! extends = Infer.inferTypeRef ctx env typeRef

                let extends =
                  match extends with
                  | TypeKind.TypeRef typeRef -> typeRef
                  | _ -> failwith "Invalid type for extends"

                return Some [ extends ]
              }
            | None -> Result.Ok None

          let objType =
            { Kind =
                TypeKind.Object
                  { Extends = extends
                    Implements = None // TODO
                    Elems = instanceElems
                    Exact = false
                    Immutable = false
                    Interface = false }
              Provenance = None }

          return objType
        }

      let! newScheme =
        Infer.inferTypeDeclDefn ctx newEnv placeholder cls.TypeParams getType

      placeholder.Type <- newScheme.Type

      let staticElems =
        List.map
          (fun elem ->
            match elem with
            | Method { Name = name; Fn = fn } ->
              ObjTypeElem.Method { Name = name; Fn = generalizeFunc fn }
            | _ -> elem)
          staticElems

      staticObjType.Kind <-
        TypeKind.Object
          { Extends = None
            Implements = None
            Elems = staticElems
            Exact = true
            Immutable = false
            Interface = false }

      return staticObjType, placeholder
    }

  ///Computes the type of the expression given by node.
  ///The type of the node is computed in the context of the
  ///supplied type environment env. Data types can be introduced into the
  ///language simply by having a predefined set of identifiers in the initial
  ///environment. environment; this way there is no need to change the syntax or, more
  ///importantly, the type-checking program when extending the language.
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
                      let! typeArg = inferTypeAnn ctx env typeArg
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
                              Captures = captures
                              InferredType = it } ->
          match it with
          | Some t -> return t
          | None ->
            match typeAnn with
            | Some typeAnnType ->
              let! fn = inferFuncSig ctx env fnSig (Some typeAnnType)

              let exprType =
                { Kind = TypeKind.Function fn
                  Provenance = None }

              let invariantPaths = None // TODO
              do! unify ctx env invariantPaths exprType typeAnnType

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
          let! t, _ = inferClass ctx env cls false
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

          let! typeArgs = List.traverseResultM (inferTypeAnn ctx env) typeArgs

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
                | Some(typeAnn) -> inferTypeAnn ctx newEnv typeAnn
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
        | Some typeAnn -> inferTypeAnn ctx newEnv typeAnn
        | None -> Result.Ok(ctx.FreshTypeVar None None)

      let! sigRetType =
        match fnSig.ReturnType with
        | Some(sigRetType) -> inferTypeAnn ctx newEnv sigRetType
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
    (valueCategory: ValueCategory)
    : Result<Type, TypeError> =
    result {
      let t = prune t

      match t.Kind with
      | TypeKind.Object { Elems = elems } ->
        match inferMemberAccess ctx key valueCategory elems with
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
          | Some(binding) -> return qualifyTypeRefs binding.Type nsName schemes
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
      | TypeKind.TypeRef { Name = typeRefName
                           Scheme = scheme
                           TypeArgs = typeArgs } ->
        match scheme with
        | Some scheme ->
          let! objType = expandScheme ctx env None scheme Map.empty typeArgs
          return! getPropType ctx env objType key optChain valueCategory
        | None ->
          let! scheme = env.GetScheme typeRefName
          let! objType = expandScheme ctx env None scheme Map.empty typeArgs
          return! getPropType ctx env objType key optChain valueCategory
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
            let! t = getPropType ctx env t key optChain valueCategory

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
          | Float _ ->
            return!
              Error(TypeError.SemanticError "numeric indexes can't be floats")
          | Int i ->
            if i >= 0 && i < elems.Length then
              return elems[int i]
            else
              // TODO: report a diagnost about the index being out of range
              return
                { Kind = TypeKind.Literal(Literal.Undefined)
                  Provenance = None }
        | _ ->
          let _arrayScheme =
            match env.TryFindScheme "Array" with
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
            match env.TryFindScheme "Array" with
            | Some scheme -> scheme
            | None -> failwith "Array not in scope"

          // Instead of expanding the whole scheme which could be quite expensive
          // we get the property from the type and then only instantiate it.
          let t = arrayScheme.Type
          let! prop = getPropType ctx env t key optChain valueCategory

          let! prop =
            instantiateType ctx prop arrayScheme.TypeParams (Some [ elem ])

          return prop
      | TypeKind.Literal(Literal.String _)
      | TypeKind.Primitive Primitive.String ->
        let scheme =
          match env.TryFindScheme "String" with
          | Some scheme -> scheme
          | None -> failwith "String not in scope"

        return! getPropType ctx env scheme.Type key optChain valueCategory
      | TypeKind.Literal(Literal.Number _)
      | TypeKind.Primitive Primitive.Number ->
        let scheme =
          match env.TryFindScheme "Number" with
          | Some scheme -> scheme
          | None -> failwith "Number not in scope"

        return! getPropType ctx env scheme.Type key optChain valueCategory
      | TypeKind.Literal(Literal.Boolean _)
      | TypeKind.Primitive Primitive.Boolean ->
        let scheme =
          match env.TryFindScheme "Boolean" with
          | Some scheme -> scheme
          | None -> failwith "Boolean not in scope"

        return! getPropType ctx env scheme.Type key optChain valueCategory
      | TypeKind.Primitive Primitive.Symbol
      | TypeKind.UniqueSymbol _ ->
        let scheme =
          match env.TryFindScheme "Symbol" with
          | Some scheme -> scheme
          | None -> failwith "Symbol not in scope"

        return! getPropType ctx env scheme.Type key optChain valueCategory
      | _ ->
        // TODO: intersection types
        return!
          Error(TypeError.NotImplemented $"TODO: lookup member on type - {t}")
    }

  let inferMemberAccess
    // TODO: do the search first and then return the appropriate ObjTypeElem
    (ctx: Ctx)
    (key: PropName)
    (valueCategory: ValueCategory)
    (elems: list<ObjTypeElem>)
    : option<Type> =

    // TODO: instead of using tryFind, use a for-loop with an early return
    // we can use this to provide better error messages when trying to assign
    // something that has a getter by no setter and similar situations.
    let elem =
      List.tryFind
        (fun (elem: ObjTypeElem) ->
          match elem with
          | Property { Name = name } -> name = key
          | Method { Name = name } ->
            name = key && valueCategory = ValueCategory.RValue
          | Getter { Name = name } ->
            name = key && valueCategory = ValueCategory.RValue
          | Setter { Name = name } ->
            name = key && valueCategory = ValueCategory.LValue
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
      | Method { Fn = fn } ->
        // TODO: replace `Self` with the object type
        // TODO: check if the receiver is mutable or not
        let t =
          { Kind = TypeKind.Function fn
            Provenance = None }

        Some t
      | Getter { Fn = fn } -> Some fn.Return // TODO: handle throws
      | Setter { Fn = fn } -> Some fn.ParamList[0].Type // TODO: handle throws
      | Mapped _mapped -> failwith "TODO: inferMemberAccess - mapped"
      | RestSpread _ -> failwith "TODO: inferMemberAccess - rest"
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

  let inferTypeRef
    (ctx: Ctx)
    (env: Env)
    ({ Ident = name; TypeArgs = typeArgs }: Syntax.TypeRef)
    : Result<TypeKind, TypeError> =
    result {
      match env.GetScheme name with
      | Ok scheme ->
        match typeArgs with
        | Some(typeArgs) ->
          let! typeArgs = List.traverseResultM (inferTypeAnn ctx env) typeArgs

          return
            { Name = name
              TypeArgs = Some(typeArgs)
              Scheme = Some scheme }
            |> TypeKind.TypeRef
        | None ->
          // TODO: check if scheme required type args
          return
            { Name = name
              TypeArgs = None
              Scheme = Some scheme }
            |> TypeKind.TypeRef
      | Error _ ->
        printfn "Can't find 'Self' in env"

        match name with
        | QualifiedIdent.Ident "_" ->
          printfn "inferring '_' as TypeKind.Wildcard"
          return TypeKind.Wildcard
        | _ -> return! Error(TypeError.SemanticError $"{name} is not in scope")
    }

  let inferObjElem
    (ctx: Ctx)
    (env: Env)
    (elem: ObjTypeAnnElem)
    : Result<ObjTypeElem, TypeError> =

    result {
      match elem with
      | ObjTypeAnnElem.Property { Name = name
                                  TypeAnn = typeAnn
                                  Value = value
                                  Optional = optional
                                  Readonly = readonly } ->
        let! t =
          match typeAnn, value with
          | Some typeAnn, Some value ->
            // TODO: infer `value`'s type and then unify with `typeAnn`'s type
            inferTypeAnn ctx env typeAnn
          | Some typeAnn, None -> inferTypeAnn ctx env typeAnn
          | None, Some value -> inferExpr ctx env None value
          | None, None -> Error(TypeError.SemanticError "Invalid property")

        let! name = inferPropName ctx env name

        return
          Property
            { Name = name
              Type = t
              Optional = optional
              Readonly = readonly }
      | ObjTypeAnnElem.Callable functionType ->
        let! f = inferFuncSig ctx env functionType None
        return Callable f
      | ObjTypeAnnElem.Constructor functionType ->
        let! f = inferFuncSig ctx env functionType None
        return Constructor f
      | ObjTypeAnnElem.Method { Name = name; Type = methodType } ->
        let! fn = inferFuncSig ctx env methodType None
        let! name = inferPropName ctx env name
        return Method { Name = name; Fn = fn }
      | ObjTypeAnnElem.Getter { Name = name
                                ReturnType = retType
                                Throws = throws } ->
        let f: FuncSig =
          { TypeParams = None
            Self = None
            ParamList = []
            ReturnType = Some retType
            Throws = throws
            IsAsync = false }

        let! fn = inferFuncSig ctx env f None
        let! name = inferPropName ctx env name
        return Getter { Name = name; Fn = fn }
      | ObjTypeAnnElem.Setter { Name = name
                                Param = param
                                Throws = throws } ->

        let undefined =
          { Kind = Keyword KeywordTypeAnn.Undefined
            Span = DUMMY_SPAN
            InferredType = None }

        let f: FuncSig =
          { TypeParams = None
            Self = None
            ParamList = [ param ]
            ReturnType = Some undefined
            Throws = throws
            IsAsync = false }

        let! fn = inferFuncSig ctx env f None
        let! name = inferPropName ctx env name
        return Setter { Name = name; Fn = fn }
      | ObjTypeAnnElem.Mapped mapped ->
        let! c = inferTypeAnn ctx env mapped.TypeParam.Constraint

        let param =
          { Name = mapped.TypeParam.Name
            Constraint = c }

        let newEnv = env.AddScheme param.Name { TypeParams = None; Type = c }

        let! typeAnn = inferTypeAnn ctx newEnv mapped.TypeAnn

        let! nameType =
          match mapped.Name with
          | Some(name) -> inferTypeAnn ctx newEnv name |> Result.map Some
          | None -> Ok None

        return
          Mapped
            { TypeParam = param
              NameType = nameType
              TypeAnn = typeAnn
              Optional = mapped.Optional
              Readonly = mapped.Readonly }

      | ObjTypeAnnElem.Spread spread ->
        let! typeAnn = inferTypeAnn ctx env spread.Arg

        return RestSpread typeAnn
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
          | KeywordTypeAnn.BigInt -> return TypeKind.Primitive Primitive.BigInt
          | KeywordTypeAnn.String -> return TypeKind.Primitive Primitive.String
          | KeywordTypeAnn.Symbol -> return TypeKind.Primitive Primitive.Symbol
          | KeywordTypeAnn.UniqueSymbol -> return ctx.FreshSymbol().Kind
          | KeywordTypeAnn.Null -> return TypeKind.Literal(Literal.Null)
          | KeywordTypeAnn.Undefined ->
            return TypeKind.Literal(Literal.Undefined)
          | KeywordTypeAnn.Unknown -> return TypeKind.Keyword Keyword.Unknown
          | KeywordTypeAnn.Never -> return TypeKind.Keyword Keyword.Never
          | KeywordTypeAnn.Object -> return TypeKind.Keyword Keyword.Object
          | KeywordTypeAnn.Any ->
            let t = ctx.FreshTypeVar None None
            return t.Kind
          | _ ->
            return!
              Error(
                TypeError.NotImplemented
                  $"TODO: unhandled keyword type - {keyword}"
              )
        | TypeAnnKind.Object { Elems = elems
                               Immutable = immutable
                               Exact = exact } ->
          let mutable newEnv = env

          match newEnv.Namespace.Schemes.TryFind "Self" with
          | Some _ -> ()
          | None ->
            let scheme =
              { TypeParams = None
                Type = ctx.FreshTypeVar None None }

            newEnv <- newEnv.AddScheme "Self" scheme

          let! elems = List.traverseResultM (inferObjElem ctx newEnv) elems

          return
            TypeKind.Object
              { Extends = None
                Implements = None
                Elems = elems
                Exact = exact
                Immutable = immutable
                Interface = false }
        | TypeAnnKind.Tuple { Elems = elems; Immutable = immutable } ->
          let! elems = List.traverseResultM (inferTypeAnn ctx env) elems
          return TypeKind.Tuple { Elems = elems; Immutable = immutable }
        | TypeAnnKind.Union types ->
          let! types = List.traverseResultM (inferTypeAnn ctx env) types
          return (union types).Kind
        | TypeAnnKind.Intersection types ->
          let! types = List.traverseResultM (inferTypeAnn ctx env) types
          return TypeKind.Intersection types
        | TypeAnnKind.TypeRef typeRef -> return! inferTypeRef ctx env typeRef
        | TypeAnnKind.Function functionType ->
          let! f = inferFuncSig ctx env functionType None
          return TypeKind.Function(f)
        | TypeAnnKind.Keyof target ->
          return! inferTypeAnn ctx env target |> Result.map TypeKind.KeyOf
        | TypeAnnKind.Rest target ->
          return! inferTypeAnn ctx env target |> Result.map TypeKind.RestSpread
        | TypeAnnKind.Typeof target ->
          let! t = getQualifiedIdentType ctx env target
          return t.Kind
        | TypeAnnKind.Index { Target = target; Index = index } ->
          let! target = inferTypeAnn ctx env target
          let! index = inferTypeAnn ctx env index
          return TypeKind.Index { Target = target; Index = index }
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

            let scheme = { TypeParams = None; Type = unknown }

            newEnv <- newEnv.AddScheme infer scheme

          let! trueType = inferTypeAnn ctx newEnv conditionType.TrueType
          let! falseType = inferTypeAnn ctx newEnv conditionType.FalseType

          return
            TypeKind.Condition
              { Check = check
                Extends = extends
                TrueType = trueType
                FalseType = falseType }
        | TypeAnnKind.Match _ ->
          return! Error(TypeError.NotImplemented "TODO: inferTypeAnn - Match") // TODO
        | TypeAnnKind.Infer name -> return TypeKind.Infer name
        | TypeAnnKind.Wildcard -> return TypeKind.Wildcard
        | TypeAnnKind.Binary { Op = op; Left = left; Right = right } ->
          let! left = inferTypeAnn ctx env left
          let! right = inferTypeAnn ctx env right
          return TypeKind.Binary { Op = op; Left = left; Right = right }
        | TypeAnnKind.Range range ->
          let! min = inferTypeAnn ctx env range.Min
          let! max = inferTypeAnn ctx env range.Max
          return TypeKind.Range { Min = min; Max = max }
        | TypeAnnKind.TemplateLiteral { Parts = parts; Exprs = exprs } ->
          let! exprs = List.traverseResultM (inferTypeAnn ctx env) exprs
          return TypeKind.TemplateLiteral { Parts = parts; Exprs = exprs }
        | TypeAnnKind.Intrinsic -> return TypeKind.Intrinsic
        | TypeAnnKind.ImportType _ ->
          return!
            Error(TypeError.NotImplemented "TODO: inferTypeAnn - ImportType")
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
            let! typeAnnType = inferTypeAnn ctx newEnv typeAnn
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
            let! typeAnnType = inferTypeAnn ctx newEnv typeAnn
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

        let! typeAnnType = inferTypeAnn ctx newEnv typeAnn
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

  let inferImport
    (ctx: Ctx)
    (env: Env)
    (import: Import)
    : Result<Env, TypeError> =

    result {
      let exports = ctx.GetExports env.Filename import

      let mutable imports = Namespace.empty

      for specifier in import.Specifiers do
        match specifier with
        | ImportSpecifier.Named { Name = name; Alias = alias } ->
          let source = name

          let target =
            match alias with
            | Some(alias) -> alias
            | None -> source

          let valueLookup =
            match exports.Values.TryFind source with
            | Some(binding) ->
              imports <- imports.AddBinding target binding
              Ok(())
            | None -> Error("not found")

          let schemeLookup =
            match exports.Schemes.TryFind source with
            | Some(scheme) ->
              imports <- imports.AddScheme target scheme
              Ok(())
            | None -> Error("not found")

          let namespaceLookup =
            match exports.Namespaces.TryFind source with
            | Some(ns) ->
              imports <- imports.AddNamespace target ns
              Ok(())
            | None -> Error("not found")

          match valueLookup, schemeLookup, namespaceLookup with
          // If we can't find the symbol in either the values or schemes
          // we report an error
          | Error _, Error _, Error _ ->
            let resolvedPath = ctx.ResolvePath env.Filename import

            return!
              Error(
                TypeError.SemanticError
                  $"{resolvedPath} doesn't export '{name}'"
              )
          | _, _, _ -> ()
        | ModuleAlias { Alias = name } ->
          let ns: Namespace = { exports with Name = name }

          imports <- imports.AddNamespace name ns

      return
        { env with
            Namespace = env.Namespace.Merge imports }
    }

  let unifyCall
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (args: list<Syntax.Expr>)
    (typeArgs: option<list<Type>>)
    (callee: Type)
    : Result<Type * Type, TypeError> =

    result {
      let callee = prune callee

      match callee.Kind with
      | TypeKind.Function func ->
        return! unifyFuncCall ctx env ips args typeArgs func
      | TypeKind.TypeRef { Name = name; Scheme = scheme } ->
        let callee =
          match scheme with
          | Some scheme ->
            // TODO: handle typeArgs
            scheme.Type
          | None ->
            match env.GetScheme(name) with
            | Result.Ok scheme ->
              // TODO: handle typeArgs
              scheme.Type
            | Result.Error _ -> failwith "'{name}' is not in scope"

        return! unifyCall ctx env ips args typeArgs callee
      | TypeKind.Object { Elems = elems } ->
        let mutable callable = None

        for elem in elems do
          match elem with
          | Callable c ->
            callable <-
              Some
                { Kind = TypeKind.Function c
                  Provenance = None }
          | _ -> ()

        match callable with
        | Some c -> return! unifyCall ctx env ips args typeArgs c
        | None ->
          return!
            Error(TypeError.SemanticError $"No callable signature in {callee}")
      | TypeKind.Intersection types ->
        let mutable result = None

        // TODO: handle an intersection of intersections

        let mutable reports = []
        let mutable retTypes = []
        let mutable throwTypes = []

        for t in types do
          if result.IsNone then
            ctx.PushReport()

            match unifyCall ctx env ips args typeArgs t with
            | Result.Ok(retType, throwType) ->
              retTypes <- retType :: retTypes
              throwTypes <- throwType :: throwTypes

              if ctx.Report.Diagnostics.IsEmpty then
                result <- Some(retType, throwType)
            | Result.Error _ -> ()

            reports <- ctx.Report :: reports
            ctx.PopReport()

        match result with
        | Some(value) -> return value
        | None ->
          let retType =
            { Kind = TypeKind.Intersection(List.rev retTypes)
              Provenance = None }

          let throwType =
            { Kind = TypeKind.Intersection(List.rev throwTypes)
              Provenance = None }

          // TODO: come up with a better way of merging diagnostics
          for report in reports do
            ctx.Report.Diagnostics <-
              ctx.Report.Diagnostics @ report.Diagnostics

          return retType, throwType
      | TypeKind.TypeVar _ ->

        // TODO: use a `result {}` CE here
        let! argTypes = List.traverseResultM (inferExpr ctx env None) args

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

        let retType = ctx.FreshTypeVar None None
        let throwsType = ctx.FreshTypeVar None None

        let fn =
          { ParamList = paramList
            Self = None // TODO: pass in the receiver if this is a method call
            Return = retType
            Throws = throwsType
            TypeParams = None } // TODO

        let callType =
          { Type.Kind = TypeKind.Function fn
            Provenance = None }

        match bind ctx env ips callee callType with
        | Ok _ -> return (prune retType, prune throwsType)
        | Error e -> return! Error e
      | kind ->
        printfn $"callee = {callee}"
        return! Error(TypeError.NotImplemented $"kind = {kind}")
    }

  // Returns a Result with 2-tuple containing the return and throws types.
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

      // TODO: require the optional params come after the required params
      // TODO: require that if there is a rest param, it comes last
      let optionalParams, requiredParams =
        callee.ParamList
        |> List.partition (fun p ->
          match p.Pattern with
          | Pattern.Rest _ -> true
          | _ -> p.Optional)

      if args.Length < requiredParams.Length then
        // TODO: make this into a diagnostic instead of an error
        return!
          Error(
            TypeError.SemanticError "function called with too few arguments"
          )

      let requiredArgs, optionalArgs = List.splitAt requiredParams.Length args

      for arg, param in List.zip requiredArgs requiredParams do
        let! invariantPaths =
          checkMutability
            (getTypePatBindingPaths param.Pattern)
            (getExprBindingPaths env arg)

        let! argType = inferExpr ctx env (Some param.Type) arg

        if
          param.Optional && argType.Kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          match unify ctx env invariantPaths argType param.Type with
          | Ok _ -> ()
          | Error reason ->
            // QUESTION: Does unifying the param with `never` actually do
            // anything or could we skip it?  Does this have to do with
            // params whose type annotations are or include type params?
            let never =
              { Kind = TypeKind.Keyword Keyword.Never
                Provenance = None }

            do! unify ctx env ips never param.Type

            ctx.Report.AddDiagnostic(
              { Description =
                  $"arg type '{argType}' doesn't satisfy param '{param.Pattern}' type '{param.Type}' in function call"
                Reasons = [ reason ] }
            )

      let optionalParams, restParams =
        optionalParams
        |> List.partition (fun p ->
          match p.Pattern with
          | Pattern.Rest _ -> false
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

      let mutable reasons: list<TypeError> = []

      for arg, param in List.zip optionalArgs optionalParams do
        let! argType = inferExpr ctx env None arg

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
          | Error(reason) -> reasons <- reason :: reasons

      match restArgs, restParam with
      | Some args, Some param ->
        let! args = List.traverseResultM (inferExpr ctx env None) args

        let tuple =
          { Kind = TypeKind.Tuple { Elems = args; Immutable = false }
            Provenance = None }

        // TODO: check the result type and add a `reason` to `reasons` if there's
        // a type error
        do! unify ctx env ips tuple param.Type
      | _ -> ()

      if not reasons.IsEmpty then
        let diagnostic =
          { Description = "Calling function with incorrect args"
            Reasons = List.rev reasons }

        ctx.Report.AddDiagnostic diagnostic

      return (callee.Return, callee.Throws)
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
