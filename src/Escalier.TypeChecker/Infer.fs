namespace Escalier.TypeChecker

open Escalier.TypeChecker.QualifiedGraph
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
open Helpers
open BuildGraph
open ExprVisitor

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
                               Optional = optional
                               Readonly = readonly
                               Static = isStatic } ->
          let! name = inferPropName ctx env name
          let! t = inferTypeAnn ctx newEnv typeAnn

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
          let! placeholderFn = inferFuncSig ctx newEnv fnSig

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
          let! placeholderFn = inferFuncSig ctx newEnv fnSig
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
              (ObjTypeElem.Method(name, placeholderFn), fnSig, body)
              :: staticMethods
          | Some _ ->
            instanceMethods <-
              (ObjTypeElem.Method(name, placeholderFn), fnSig, body)
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

          let! placeholderFn = inferFuncSig ctx newEnv fnSig
          let! name = inferPropName ctx env name

          match self, isStatic with
          | None, true ->
            staticMethods <-
              (ObjTypeElem.Getter(name, placeholderFn), fnSig, body)
              :: staticMethods
          | Some _, false ->
            instanceMethods <-
              (ObjTypeElem.Getter(name, placeholderFn), fnSig, body)
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

          let! placeholderFn = inferFuncSig ctx newEnv fnSig
          let! name = inferPropName ctx env name

          match self, isStatic with
          | None, true ->
            staticMethods <-
              (ObjTypeElem.Setter(name, placeholderFn), fnSig, body)
              :: staticMethods
          | Some _, false ->
            instanceMethods <-
              (ObjTypeElem.Setter(name, placeholderFn), fnSig, body)
              :: instanceMethods
          | _, _ -> failwith "Invalid setter"

      for elem, _, _ in instanceMethods do
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
        | _ -> ()

      let mutable hasConstructor = false

      for elem, _, _ in staticMethods do
        match elem with
        | Constructor(placeholderFn) ->
          hasConstructor <- true

          staticElems <- ObjTypeElem.Constructor(placeholderFn) :: staticElems
        | Method(name, placeholderFn) ->
          staticElems <- ObjTypeElem.Method(name, placeholderFn) :: staticElems
        | Getter(name, placeholderFn) ->
          staticElems <- ObjTypeElem.Getter(name, placeholderFn) :: staticElems
        | Setter(name, placeholderFn) ->
          staticElems <- ObjTypeElem.Setter(name, placeholderFn) :: staticElems
        | _ -> ()

      let objType =
        { Kind =
            TypeKind.Object
              { Extends = None // QUESTION: Do we need this for the placeholder?
                Implements = None // TODO
                Elems = instanceElems
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
                Immutable = false
                Interface = false }
          Provenance = None }

      // TODO: Make Type.Kind mutable so that we can modify the type after its
      // been created.
      newEnv <- newEnv.AddValue "Self" (staticObjType, false)

      // Infer the bodies of each instance method body
      for elem, fnSig, body in instanceMethods do
        let placeholderFn =
          match elem with
          | Method(_, placeholderFn) -> placeholderFn
          | Getter(_, placeholderFn) -> placeholderFn
          | Setter(_, placeholderFn) -> placeholderFn
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
        | Method(_, placeholderFn) ->
          match body, declare with
          | Some body, false ->
            let! _ = inferFuncBody ctx newEnv fnSig placeholderFn body
            ()
          | _ -> () // TODO: handle other cases correctly
        | Getter(_, placeholderFn) ->
          match body, declare with
          | Some body, false ->
            let! _ = inferFuncBody ctx newEnv fnSig placeholderFn body
            ()
          | _ -> () // TODO: handle other cases correctly
        | Setter(_, placeholderFn) ->
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

            let visitor: SyntaxVisitor<unit> =
              { VisitExpr =
                  fun (expr: Expr, state) ->
                    match expr.Kind with
                    | ExprKind.Assign(_, left, _) ->
                      match left.Kind with
                      | ExprKind.Member(obj, prop, _) ->
                        match obj.Kind with
                        | ExprKind.Identifier "self" ->
                          assignedProps <- prop :: assignedProps
                          (true, state)
                        | _ -> (true, state)
                      | _ -> (true, state)
                    | ExprKind.Call { Callee = callee } ->
                      match callee.Kind with
                      | ExprKind.Member(obj, prop, _) ->
                        match obj.Kind with
                        | ExprKind.Identifier "self" ->
                          methodsCalled <- prop :: methodsCalled
                          (true, state)
                        | _ -> (true, state)
                      | _ -> (true, state)
                    | _ -> (true, state)

                ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
                ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
                ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
                VisitStmt = fun (_, state) -> (true, state)
                VisitPattern = fun (_, state) -> (false, state)
                VisitTypeAnn = fun (_, state) -> (false, state)
                VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

            match body with
            | BlockOrExpr.Block block ->
              List.iter (walkStmt visitor ()) block.Stmts
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
            | Method(name, f) -> ObjTypeElem.Method(name, generalizeFunc f)
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
            | Method(name, f) -> ObjTypeElem.Method(name, generalizeFunc f)
            | _ -> elem)
          staticElems

      staticObjType.Kind <-
        TypeKind.Object
          { Extends = None
            Implements = None
            Elems = staticElems
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
        | ExprKind.Identifier(name) ->

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
        | ExprKind.Binary(op, left, right) ->
          let! funTy = env.GetBinaryOp op

          let! result, _throws =
            unifyCall ctx env None [ left; right ] None funTy

          // TODO: handle throws

          return result

        | ExprKind.Unary(op, arg) ->
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
              let! fn = inferFuncSig ctx env fnSig

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
        | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
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
        | ExprKind.IfLet(pattern, init, thenBranch, elseBranch) ->
          // treat pattern/target the as a let binding
          let! invariantPaths =
            checkMutability
              (getPatBindingPaths pattern)
              (getExprBindingPaths env init)

          let! patBindings, patType = inferPattern ctx env pattern
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
          let mutable spreadTypes = []

          let! map =
            result {
              match typeAnn with
              | Some typeAnn ->
                let! t = expandType ctx env None Map.empty typeAnn
                let! map = getPropertyMap t
                return Some map
              | None -> return None
            }

          let! elems =
            List.traverseResultM
              (fun (elem: ObjElem) ->
                result {
                  match elem with
                  | ObjElem.Property(_span, key, value) ->
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
                      Some(
                        Property
                          { Name = name
                            Optional = false
                            Readonly = false
                            Type = t }
                      )
                  | ObjElem.Shorthand(_span, key) ->
                    let! value = env.GetValue key

                    return
                      Some(
                        Property
                          { Name = PropName.String key
                            Optional = false
                            Readonly = false
                            Type = value }
                      )
                  | ObjElem.Spread(_span, value) ->
                    let! t = inferExpr ctx env None value
                    spreadTypes <- t :: spreadTypes
                    return None
                })
              elems

          let elems = elems |> List.choose id

          let objType =
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = elems
                    Immutable = immutable
                    Interface = false }
              Provenance = None }

          match spreadTypes with
          | [] -> return objType
          | _ ->
            return
              { Kind = TypeKind.Intersection([ objType ] @ spreadTypes)
                Provenance = None }
        | ExprKind.Class cls ->
          let! t, _ = inferClass ctx env cls false
          return t
        | ExprKind.Member(obj, prop, optChain) ->
          let! objType = inferExpr ctx env None obj
          let propKey = PropName.String(prop)

          let! t =
            getPropType ctx env objType propKey optChain ValueCategory.RValue

          let mutable t = t

          match t.Kind with
          | TypeKind.Function({ Self = Some(self) } as fn) ->
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

          match t.Kind with
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
        | ExprKind.Match(expr, cases) ->
          let! exprType = inferExpr ctx env None expr
          let! _, bodyTypes = inferMatchCases ctx env exprType cases
          return (union bodyTypes)
        | ExprKind.Index(target, index, optChain) ->
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
        | ExprKind.Assign(_operation, left, right) ->
          // TODO: handle update assign operations
          let! rightType = inferExpr ctx env None right

          let! t, isMut = getLvalue ctx env left

          if isMut then
            do! unify ctx env None rightType t
          else
            return!
              Error(TypeError.SemanticError "Can't assign to immutable binding")

          return rightType
        | ExprKind.ExprWithTypeArgs(target, typeArgs) ->
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
        | ExprKind.JSXFragment _ ->
          return!
            Error(TypeError.NotImplemented "TODO: inferExpr - JSX fragments")
        | ExprKind.Do body ->
          return!
            Error(TypeError.NotImplemented "TODO: inferExpr - Do expression")
        | ExprKind.TemplateLiteral templateLiteral ->
          return!
            Error(TypeError.NotImplemented "TODO: inferExpr - TemplateLiteral")
        | ExprKind.TaggedTemplateLiteral(tag, template, throws) ->
          return!
            Error(
              TypeError.NotImplemented "TODO: inferExpr - TaggedTemplateLiteral"
            )
      }

    ctx.MergeUpReport()

    Result.map
      (fun t ->
        expr.InferredType <- Some(t)
        t.Provenance <- Some(Provenance.Expr expr)
        t)
      r

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
                { Kind = TypeKind.Index(intrinsics, key)
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
          | JSXFragment jsxFragment -> failwith "todo"

        return reactNode
      }

    ctx.MergeUpReport()

    r

  let inferFuncSig
    (ctx: Ctx)
    (env: Env)
    (fnSig: FuncSig)
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
                | None -> Result.Ok(ctx.FreshTypeVar None None)

              // TODO: figure out a way to avoid having to call inferPattern twice
              // per method (the other call is `inferFuncBody`)
              let! _assumps, patternType =
                inferPattern ctx newEnv param.Pattern

              // TODO: figure out how to handle unifying `...rest` and `infer _`
              // do! unify ctx newEnv None patternType paramType

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
      // TODO: intersection types
      | _ ->
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
          | Method(name, _) ->
            name = key && valueCategory = ValueCategory.RValue
          | Getter(name, _) ->
            name = key && valueCategory = ValueCategory.RValue
          | Setter(name, _) ->
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
      | Method(_, fn) ->
        // TODO: replace `Self` with the object type
        // TODO: check if the receiver is mutable or not
        let t =
          { Kind = TypeKind.Function fn
            Provenance = None }

        Some t
      | Getter(_, fn) -> Some fn.Return // TODO: handle throws
      | Setter(_, fn) -> Some fn.ParamList[0].Type // TODO: handle throws
      | Mapped _mapped -> failwith "TODO: inferMemberAccess - mapped"
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
      let! newEnv = inferStmts ctx env false block.Stmts

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
        let! f = inferFuncSig ctx env functionType
        return Callable f
      | ObjTypeAnnElem.Constructor functionType ->
        let! f = inferFuncSig ctx env functionType
        return Constructor f
      | ObjTypeAnnElem.Method { Name = name; Type = methodType } ->
        let! f = inferFuncSig ctx env methodType
        let! name = inferPropName ctx env name
        return Method(name, f)
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

        let! f = inferFuncSig ctx env f
        let! name = inferPropName ctx env name
        return Getter(name, f)
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

        let! f = inferFuncSig ctx env f
        let! name = inferPropName ctx env name
        return Setter(name, f)
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
        | TypeAnnKind.Object { Elems = elems; Immutable = immutable } ->
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
          let! f = inferFuncSig ctx env functionType
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
          | None -> ctx.FreshTypeVar None None

        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(name, (t, isMut))
        pat.InferredType <- Some t
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
                                                Default = _ } ->
                let t = infer_pattern_rec value

                Some(
                  ObjTypeElem.Property
                    { Name = PropName.String key
                      Optional = false
                      Readonly = false
                      Type = t }
                )
              | Syntax.ObjPatElem.ShorthandPat({ Name = name
                                                 IsMut = isMut
                                                 Assertion = assertion } as pat) ->
                // TODO: lookup `assertion` in `env`

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
                  | None -> ctx.FreshTypeVar None None

                // TODO: check if `name` already exists in `assump`
                assump <- assump.Add(name, (t, isMut))
                pat.Inferred <- Some t

                Some(
                  ObjTypeElem.Property
                    { Name = PropName.String name
                      Optional = false
                      Readonly = false
                      Type = t }
                )

              | Syntax.ObjPatElem.RestPat { Target = target; IsMut = _ } ->
                restType <-
                  Some(
                    { Kind = infer_pattern_rec target |> TypeKind.Rest
                      Provenance = None }
                  )

                None)
            elems

        let objType =
          { Kind =
              TypeKind.Object
                { Extends = None
                  Implements = None
                  Elems = elems
                  Immutable = immutable
                  Interface = false }
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
        printfn $"Looking up tag for {variant.Ident}"
        let tagType = getQualifiedIdentType ctx env variant.Ident

        let tagType =
          match tagType with
          | Result.Ok t ->
            let elems: list<ObjTypeElem> =
              [ ObjTypeElem.Property
                  { Name = PropName.String "__TAG__"
                    Optional = false
                    Readonly = true
                    Type = t } ]

            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = elems
                    Immutable = true
                    Interface = false }
              Provenance = None }
          | Result.Error _ -> failwith "Can't find enum type"

        let argType = variant.Arg |> Option.map infer_pattern_rec

        // This is the type inferred from the enum pattern
        let patternType =
          match argType with
          | Some argType ->
            { Kind = TypeKind.Intersection [ tagType; argType ]
              Provenance = None }
          | None -> tagType

        // TODO: stop using QualifiedIdent for Enum variants
        let enumName, variantName =
          match variant.Ident with
          | QualifiedIdent.Member(QualifiedIdent.Ident qualifier, name) ->
            (qualifier, name)
          | _ -> failwith "This should never happen"

        match env.GetScheme(QualifiedIdent.Ident enumName) with
        | Ok scheme ->
          let t = instantiateType ctx scheme.Type scheme.TypeParams None

          let t =
            match t with
            | Ok t -> t
            | Error _ -> failwith "Failed to instantiate enum scheme"

          let result = unify ctx env None patternType t

          match result with
          | Ok _ -> patternType
          | Error _ -> failwith $"Failed to unify {patternType} and {t}"

        | Error _ -> failwith $"Can't find scheme for {variant.Ident}"

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

      // TODO: check mutability when unifying by computing invariant paths
      // using checkMutability

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
                let! _ = inferExpr ctx newEnv None guard
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

        let! patBindings, patType = inferPattern ctx env pattern
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
          let t, _ = binding

          match (prune t).Kind with
          | TypeKind.Object { Elems = elems } ->
            // TODO: modify the constructors so they return `Foo<T>` instead
            // of `Self`.  Right now unifyFuncCall is responsible for this,
            // but that doesn't seem like the best place for it.
            let fns =
              elems
              |> List.choose (fun elem ->
                match elem with
                | ObjTypeElem.Constructor fn -> Some fn
                | ObjTypeElem.Method(_, fn) -> Some fn
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
        let! patBindings, patType = inferPattern ctx env pattern
        let mutable newEnv = env

        let! typeAnnType = inferTypeAnn ctx newEnv typeAnn
        do! unify ctx newEnv None typeAnnType patType

        return (patBindings, Map.empty)
      }
    | _ -> Error(TypeError.SemanticError "Invalid var decl")

  // Infers a placeholder scheme from a type declaration
  // It has the proper type params but the type definition itself is a
  // fresh type variable.
  let inferTypeDeclPlaceholderScheme
    (ctx: Ctx)
    (env: Env)
    (typeParams: option<list<Syntax.TypeParam>>)
    : Result<Scheme, TypeError> =

    result {
      let typeParams =
        typeParams
        |> Option.map (fun typeParams ->
          List.map
            (fun (typeParam: Syntax.TypeParam) ->
              // The fresh type variables here eventually get unified
              // with the real types for the Constraint and Default when
              // we unify them later.
              let c =
                match typeParam.Constraint with
                | Some(c) -> Some(ctx.FreshTypeVar None None)
                | None -> None

              let d =
                match typeParam.Default with
                | Some(d) -> Some(ctx.FreshTypeVar None None)
                | None -> None

              { Name = typeParam.Name
                Constraint = c
                Default = d })
            typeParams)

      let scheme =
        { TypeParams = typeParams
          Type = ctx.FreshTypeVar None None }

      return scheme
    }

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
        | Named(name, alias) ->
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
        | ModuleAlias name ->
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
          | Rest _ -> true
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

  let rec getQualifiedIdentType
    (ctx: Ctx)
    (env: Env)
    (ident: Common.QualifiedIdent)
    =
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
    : Result<Type * bool, TypeError> =
    result {
      match expr.Kind with
      | ExprKind.Identifier name -> return! env.GetBinding name
      | ExprKind.Index(target, index, _optChain) ->
        // TODO: disallow optChain in lvalues
        let! target, isMut = getLvalue ctx env target
        let! index = inferExpr ctx env None index

        let key =
          match index.Kind with
          | TypeKind.Literal(Literal.Number i) -> PropName.Number i
          | TypeKind.Literal(Literal.String s) -> PropName.String s
          | TypeKind.UniqueSymbol id -> PropName.Symbol id
          | _ ->
            printfn "index = %A" index
            failwith $"TODO: index can't be a {index}"

        let! t = getPropType ctx env target key false ValueCategory.LValue
        return t, isMut
      | ExprKind.Member(target, name, _optChain) ->
        // TODO: check if `target` is a namespace
        // If the target is either an Identifier or another Member, we
        // can try to look look for a namespace for it.

        // TODO: disallow optChain in lvalues
        let! target, isMut = getLvalue ctx env target

        let! t =
          getPropType
            ctx
            env
            target
            (PropName.String name)
            false
            ValueCategory.LValue

        return t, isMut
      | _ ->
        return! Error(TypeError.SemanticError $"{expr} is not a valid lvalue")
    }

  let getAllBindingPatterns (pattern: Syntax.Pattern) : Map<string, Type> =
    let mutable result = Map.empty

    let visitor =
      { ExprVisitor.VisitExpr =
          fun (expr, state) ->
            match expr.Kind with
            | ExprKind.Function _ -> (false, state)
            | _ -> (true, state)
        ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
        ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
        ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
        ExprVisitor.VisitStmt = fun (_, state) -> (false, state)
        ExprVisitor.VisitPattern =
          fun (pat, state) ->
            match pat.Kind with
            | PatternKind.Ident { Name = name } ->
              match pat.InferredType with
              | Some t -> result <- Map.add name t result
              | None -> ()

              (false, state)
            | PatternKind.Object { Elems = elems } ->
              for elem in elems do
                match elem with
                | Syntax.ObjPatElem.ShorthandPat { Name = name
                                                   Inferred = inferred } ->
                  match inferred with
                  | Some t -> result <- Map.add name t result
                  | None -> ()
                | _ -> ()

              (true, state) // visit rest RestPats and KeyValuePats
            | _ -> (true, state)
        ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
        ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

    walkPattern visitor () pattern

    result

  let rec inferExprStructuralPlacholder
    (ctx: Ctx)
    (env: Env)
    (expr: Expr)
    : Result<Type, TypeError> =
    result {
      let mutable elemTypes: list<ObjTypeElem> = []
      let mutable spreadTypes: list<Type> = []

      match expr.Kind with
      | ExprKind.Object { Elems = elems } ->
        for elem in elems do
          match elem with
          | ObjElem.Property(span, name, value) ->
            let! name = Infer.inferPropName ctx env name
            let! t = inferExprStructuralPlacholder ctx env value

            elemTypes <-
              ObjTypeElem.Property
                { Name = name
                  Optional = false
                  Readonly = false
                  Type = t }
              :: elemTypes
          | ObjElem.Shorthand(span, name) ->
            match env.TryFindValue name with
            | Some(t, _) ->
              elemTypes <-
                ObjTypeElem.Property
                  { Name = PropName.String name
                    Optional = false
                    Readonly = false
                    Type = t }
                :: elemTypes
            | None -> return! Error(TypeError.SemanticError $"{name} not found")
          | ObjElem.Spread(span, value) ->
            match value.Kind with
            | ExprKind.Identifier name ->
              match env.TryFindValue name with
              | Some(t, _) -> spreadTypes <- t :: spreadTypes
              | None ->
                return! Error(TypeError.SemanticError $"{name} not found")
            | _ ->
              return!
                Error(
                  TypeError.NotImplemented
                    $"TODO: inferExprStructuralPlacholder - handle spread of {value}"
                )

        let kind =
          TypeKind.Object
            { Extends = None
              Implements = None
              Elems = List.rev elemTypes
              Immutable = false
              Interface = false }

        let objType = { Kind = kind; Provenance = None }

        match spreadTypes with
        | [] -> return objType
        | _ ->
          let kind = TypeKind.Intersection(objType :: spreadTypes)
          return { Kind = kind; Provenance = None }
      | ExprKind.Tuple { Elems = elems } ->
        let! elems =
          elems
          |> List.traverseResultM (fun elem ->
            inferExprStructuralPlacholder ctx env elem)

        let kind = TypeKind.Tuple { Elems = elems; Immutable = false }

        return { Kind = kind; Provenance = None }
      | _ -> return ctx.FreshTypeVar None None
    }

  let getKey
    (ident: QDeclIdent)
    (name: string)
    : QualifiedGraph.QualifiedIdent =
    let key: QualifiedGraph.QualifiedIdent =
      match ident with
      | QDeclIdent.Type { Parts = parts } ->
        { Parts = List.take (parts.Length - 1) parts @ [ name ] }
      | QDeclIdent.Value { Parts = parts } ->
        { Parts = List.take (parts.Length - 1) parts @ [ name ] }

    key

  let inferDeclPlaceholders
    (ctx: Ctx)
    (env: Env)
    (qns: QualifiedNamespace)
    (idents: list<QDeclIdent>)
    (graph: QGraph<Decl>)
    : Result<QualifiedNamespace, TypeError> =

    result {
      let mutable qns = qns

      for ident in idents do
        if not (graph.Nodes.ContainsKey ident) then
          // TODO: make sure that the name space exists when determine if a decl
          // is qualified with a namespace or not.
          // printfn "ident = %A" ident
          return! Error(TypeError.SemanticError "Missing node in graph")

        let decls = graph.Nodes[ident]

        for decl in decls do
          match decl.Kind with
          | VarDecl { Pattern = pattern
                      Init = init
                      TypeAnn = typeAnn } ->
            // QUESTION: Should we check the type annotation as we're generating
            // the placeholder type?
            let! bindings, patternType = Infer.inferPattern ctx env pattern

            match typeAnn, init with
            | None, Some init ->
              // TODO: Think about whether inferExprStructuralPlacholder should
              // be used here. In particular, do we want to support objects without
              // type annotations, reference the object methods on the object.
              let placeholderType = ctx.FreshTypeVar None None
              do! unify ctx env None patternType placeholderType
            | _, _ -> ()

            for KeyValue(name, binding) in bindings do
              qns <- qns.AddValue (getKey ident name) binding
          | FnDecl({ Declare = declare
                     Sig = fnSig
                     Name = name }) ->
            if declare then
              // TODO: capture these errors as diagnostics and infer the missing
              // types as `never`
              for p in fnSig.ParamList do
                if p.TypeAnn.IsNone then
                  failwith "Ambient function declarations must be fully typed"

              if fnSig.ReturnType.IsNone then
                failwith "Ambient function declarations must be fully typed"

            let t = ctx.FreshTypeVar None None
            qns <- qns.AddValue (getKey ident name) (t, false)
          | ClassDecl({ Name = name
                        Class = { TypeParams = typeParams } } as decl) ->
            let key = getKey ident name

            if not (qns.Schemes.ContainsKey key) then
              // TODO: treat ClassDecl similar to object types where we create a
              // structural placeholder type instead of an opaque type variable.
              // We should do this for both instance members and statics.
              let! instance =
                Infer.inferTypeDeclPlaceholderScheme ctx env typeParams

              let statics: Type = ctx.FreshTypeVar None None

              qns <- qns.AddScheme key instance
              qns <- qns.AddValue key (statics, false)
          | EnumDecl { Variants = variants
                       Name = name
                       TypeParams = typeParams } ->

            let key = getKey ident name

            if not (qns.Schemes.ContainsKey key) then

              let! scheme =
                Infer.inferTypeDeclPlaceholderScheme ctx env typeParams

              qns <- qns.AddScheme key scheme
              qns <- qns.AddValue key (ctx.FreshTypeVar None None, false)
          | TypeDecl { TypeParams = typeParams; Name = name } ->
            // TODO: check to make sure we aren't redefining an existing type
            // TODO: replace placeholders, with a reference the actual definition
            // once we've inferred the definition
            let! placeholder =
              Infer.inferTypeDeclPlaceholderScheme ctx env typeParams

            qns <- qns.AddScheme (getKey ident name) placeholder
          | InterfaceDecl({ Name = name
                            TypeParams = typeParams
                            Extends = extends } as decl) ->
            let key = getKey ident name

            let parts =
              match ident with
              | QDeclIdent.Type { Parts = parts } -> parts
              | QDeclIdent.Value { Parts = parts } -> parts

            // Instead of looking things up in the environment, we need some way to
            // find the existing type on other declarations.
            let! placeholder =
              // NOTE: looking up the scheme using `name` works here because we
              // called `openNamespaces` at the top of this function.
              match parts, env.TryFindScheme name with
              // TODO: handle the case where the qualifier is `global` to handle
              // declare global { ... } statements.
              | [ _ ], Some scheme -> Result.Ok scheme
              | _, _ ->
                match qns.Schemes.TryFind(key) with
                | Some scheme -> Result.Ok scheme
                | None ->
                  Infer.inferTypeDeclPlaceholderScheme ctx env typeParams

            qns <- qns.AddScheme key placeholder
          | NamespaceDecl nsDecl ->
            return!
              Error(
                TypeError.NotImplemented
                  "TODO: inferDeclPlaceholders - NamespaceDecl"
              )

      return qns
    }

  // Copies symbols from the nested namespaces listed in `namespaces` into `env`.
  // For example if `namespaces` contains `["Foo", "Bar"]` then this function will
  // copy symbols from the `Foo` and `Foo.Bar` namespaces into `env`.
  let rec openNamespaces (env: Env) (namespaces: list<string>) : Env =
    let mutable newEnv = env

    let rec openNamespace (parentNS: Namespace) (namespaces: list<string>) =
      match namespaces with
      | [] -> ()
      | head :: rest ->
        match parentNS.Namespaces.TryFind(head) with
        | None -> printfn $"namespace {head} not found"
        | Some _ -> ()

        let nextNS = parentNS.Namespaces[head]

        for KeyValue(name, scheme) in nextNS.Schemes do
          newEnv <- newEnv.AddScheme name scheme

        for KeyValue(name, binding) in nextNS.Values do
          newEnv <- newEnv.AddValue name binding

        for KeyValue(name, ns) in nextNS.Namespaces do
          newEnv <- newEnv.AddNamespace name ns

        openNamespace nextNS rest

    match namespaces with
    | "global" :: rest -> openNamespace env.Namespace rest
    | namespaces -> openNamespace env.Namespace namespaces

    newEnv

  let inferDeclDefinitions
    (ctx: Ctx)
    (env: Env)
    (qns: QualifiedNamespace)
    (idents: list<QDeclIdent>)
    (graph: QGraph<Decl>)
    : Result<QualifiedNamespace, TypeError> =

    result {
      let mutable qns = qns

      // There are separate identifiers for the instance (type) and statics
      // (value).  This set is used to avoid inferring classes more than once.
      let mutable inferredClasses = Set.empty
      let mutable inferredEnums = Set.empty

      for ident in idents do
        let decls = graph.Nodes[ident]

        // TODO: check if we're inside a namespace and update the env accordingly
        let parts =
          match ident with
          | QDeclIdent.Type { Parts = parts } -> parts
          | QDeclIdent.Value { Parts = parts } -> parts

        let namespaces = List.take (List.length parts - 1) parts
        let name = List.last parts

        let mutable newEnv = env
        newEnv <- openNamespaces newEnv namespaces

        // Strategy:
        // - separate decls into groups based on their kind
        // - if there are multiple non-empty groups, error
        // - if any of the groups other than FnDecl or InterfaceDecl have more
        //   than one decl, error
        // - infer each group
        let mutable varDecls = []
        let mutable fnDecls = []
        let mutable classDecls = []
        let mutable typeDecls = []
        let mutable interfaceDecls = []
        let mutable enumDecls = []
        let mutable namespaceDecls = []

        for decl in decls do
          match decl.Kind with
          | VarDecl varDecl -> varDecls <- varDecl :: varDecls
          | FnDecl fnDecl -> fnDecls <- fnDecl :: fnDecls
          | ClassDecl classDecl -> classDecls <- classDecl :: classDecls
          | TypeDecl typeDecl -> typeDecls <- typeDecl :: typeDecls
          | InterfaceDecl interfaceDecl ->
            interfaceDecls <- interfaceDecl :: interfaceDecls
          | EnumDecl enumDecl -> enumDecls <- enumDecl :: enumDecls
          | NamespaceDecl namespaceDecl ->
            namespaceDecls <- namespaceDecl :: namespaceDecls

        fnDecls <- List.rev fnDecls
        interfaceDecls <- List.rev interfaceDecls

        let mutable count = 0

        if varDecls.Length > 0 then
          count <- count + 1

        if fnDecls.Length > 0 then
          count <- count + 1

        if classDecls.Length > 0 then
          count <- count + 1

        if typeDecls.Length > 0 then
          count <- count + 1

        if interfaceDecls.Length > 0 then
          count <- count + 1

        if enumDecls.Length > 0 then
          count <- count + 1

        if namespaceDecls.Length > 0 then
          count <- count + 1

        if count > 1 then
          return!
            Error(
              TypeError.SemanticError
                "more than one kind of decl found for ident"
            )

        match varDecls with
        | [] -> ()
        | [ varDecl ] ->
          let placeholderTypes = getAllBindingPatterns varDecl.Pattern

          // NOTE: We explicitly don't generalize here because we want other
          // declarations to be able to unify with any free type variables
          // from this declaration.  We generalize things in `inferModule` and
          // `inferTreeRec`.
          let! newBindings, newSchemes = inferVarDecl ctx newEnv varDecl

          let inferredTypes = getAllBindingPatterns varDecl.Pattern

          for KeyValue(name, inferredType) in inferredTypes do
            let placeholderType =
              match Map.tryFind name placeholderTypes with
              | Some t -> t
              | None -> failwith "Missing placeholder type"

            do! unify ctx newEnv None placeholderType inferredType

          // Schemes can be generated for things like class expressions, e.g.
          // let Foo = class { ... }
          for KeyValue(name, scheme) in newSchemes do
            qns <- qns.AddScheme (getKey ident name) scheme
        | _ ->
          return!
            Error(TypeError.SemanticError "multiple var decls found for ident")

        if fnDecls.Length > 0 then
          let! fns =
            List.traverseResultM
              (fun (fnDecl: FnDecl) ->
                result {
                  let! f =
                    match fnDecl.Declare, fnDecl.Body with
                    | false, Some body ->
                      // NOTE: `inferFunction` also calls unify
                      Infer.inferFunction ctx newEnv fnDecl.Sig body
                    | true, None -> Infer.inferFuncSig ctx newEnv fnDecl.Sig
                    | _, _ ->
                      Result.Error(
                        TypeError.SemanticError "Invalid function declaration"
                      )

                  return f
                })
              fnDecls

          let types =
            fns
            |> List.map (fun f ->
              { Kind = TypeKind.Function f
                Provenance = None })

          let inferredType =
            match types with
            | [] -> failwith "No types found"
            | [ t ] -> t
            | _ ->
              { Kind = TypeKind.Intersection types
                Provenance = None }

          let placeholderType, _ =
            match newEnv.TryFindValue name with
            | Some t -> t
            | None -> failwith "Missing placeholder type"

          // NOTE: We explicitly don't generalize here because we want other
          // declarations to be able to unify with any free type variables
          // from this declaration.  We generalize things in `inferModule` and
          // `inferTreeRec`.
          do! unify ctx newEnv None placeholderType inferredType


        // TODO: handle remaining decl kind lists
        match classDecls with
        | [] -> ()
        | [ classDecl ] ->
          let { Declare = declare
                Name = name
                Class = cls } =
            classDecl

          let key = getKey ident name

          if not (inferredClasses.Contains key) then
            let! inferredType, inferredScheme =
              Infer.inferClass ctx newEnv cls declare

            let placeholderScheme = qns.Schemes[key]
            let placeholderType, _ = qns.Values[key]

            do! unify ctx newEnv None placeholderScheme.Type inferredScheme.Type
            do! unify ctx newEnv None placeholderType inferredType

            inferredClasses <- inferredClasses.Add key
        | _ ->
          return!
            Error(TypeError.SemanticError "More than one class decl for ident")

        match enumDecls with
        | [] -> ()
        | [ enumDecl ] ->
          let { Name = name
                Variants = variants
                TypeParams = typeParams } =
            enumDecl

          let key = getKey ident name

          // Prevents inferring the enum more than once
          if not (inferredEnums.Contains key) then
            let placeholderScheme = qns.Schemes[key]

            let { Name = name
                  TypeParams = typeParams
                  Variants = variants } =
              enumDecl

            // TODO: do the same thing we do for type decls
            match placeholderScheme.TypeParams with
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

            let mutable variantTypes = []
            let mutable variantTags = []

            for variant in variants do
              let tag = ctx.FreshSymbol()

              variantTags <-
                ObjTypeElem.Property
                  { Name = PropName.String variant.Name
                    Optional = false
                    Readonly = true
                    Type = tag }
                :: variantTags

              let elems =
                [ ObjTypeElem.Property
                    { Name = PropName.String "__TAG__"
                      Optional = false
                      Readonly = true
                      Type = tag } ]

              let tagType =
                { Kind =
                    TypeKind.Object
                      { Elems = elems
                        Immutable = false
                        Extends = None
                        Implements = None
                        Interface = false }
                  Provenance = None }

              let! variantType =
                result {
                  match variant.TypeAnn with
                  | Some typeAnn ->
                    let! inferredType = Infer.inferTypeAnn ctx newEnv typeAnn

                    return
                      { Kind = TypeKind.Intersection [ tagType; inferredType ]
                        Provenance = None }
                  | None -> return tagType
                }

              variantTypes <- variantType :: variantTypes

            let unionType = List.rev variantTypes |> union

            let inferredType =
              { Kind =
                  TypeKind.Object
                    { Elems = variantTags
                      Immutable = false
                      Extends = None
                      Implements = None
                      Interface = false }
                Provenance = None }

            let placeholderScheme = qns.Schemes[key]
            let placeholderType, _ = qns.Values[key]

            placeholderScheme.Type <- unionType
            do! unify ctx newEnv None placeholderType inferredType

            // avoid inferring the enum more than once
            inferredEnums <- inferredEnums.Add key
        | _ -> return! Error(TypeError.SemanticError "More than one enum decl")

        match typeDecls with
        | [] -> ()
        | [ typeDecl ] ->
          let { Name = name
                TypeAnn = typeAnn
                TypeParams = typeParams } =
            typeDecl

          let key = getKey ident name
          let placeholder = qns.Schemes[key]

          // TODO: when computing the decl graph, include self-recursive types in
          // the deps set so that we don't have to special case this here.
          // Handles self-recursive types
          newEnv <- newEnv.AddScheme name placeholder
          let getType = fun env -> Infer.inferTypeAnn ctx env typeAnn

          let! scheme =
            Infer.inferTypeDeclDefn ctx newEnv placeholder typeParams getType

          match placeholder.TypeParams, scheme.TypeParams with
          | Some typeParams1, Some typeParams2 ->
            for typeParam1, typeParam2 in List.zip typeParams1 typeParams2 do
              match typeParam1.Constraint, typeParam2.Constraint with
              | Some c1, Some c2 -> do! unify ctx newEnv None c1 c2
              | None, None -> ()
              | _, _ ->
                return!
                  Error(
                    TypeError.SemanticError
                      "One scheme has a constraint type while the other doesn't"
                  )

              match typeParam1.Default, typeParam2.Default with
              | Some d1, Some d2 -> do! unify ctx newEnv None d1 d2
              | None, None -> ()
              | _, _ ->
                return!
                  Error(
                    TypeError.SemanticError
                      "One scheme has a default type while the other doesn't"
                  )
          | None, None -> ()
          | _, _ ->
            return!
              Error(
                TypeError.SemanticError
                  "One scheme has type params while the other doesn't"
              )

          // Replace the placeholder's type with the actual type.
          // NOTE: This is a bit hacky and we may want to change this later to use
          // `foldType` to replace any uses of the placeholder with the actual type.
          // Required for the following test cases:
          // - InferRecursiveGenericObjectTypeInModule
          // - InferNamespaceInModule
          // placeholder.Value.Type <- scheme.Type
          qns.Schemes[key].Type <- scheme.Type

        | _ ->
          return!
            Error(TypeError.SemanticError "More than one type decl for ident")

        match interfaceDecls with
        | [] -> ()
        | interfaceDecls ->

          for interfaceDecl in interfaceDecls do
            let { Name = name
                  TypeParams = typeParams
                  Extends = extends
                  Elems = elems } =
              interfaceDecl

            let key = getKey ident name
            let placeholder = qns.Schemes[key]

            // TODO: when computing the decl graph, include self-recursive types in
            // the deps set so that we don't have to special case this here.
            // Handles self-recursive types
            let newEnv = newEnv.AddScheme name placeholder

            let getType (env: Env) : Result<Type, TypeError> =
              result {
                let! elems =
                  List.traverseResultM (Infer.inferObjElem ctx env) elems

                let! extends =
                  match extends with
                  | Some typeRefs ->
                    result {

                      let! extends =
                        List.traverseResultM
                          (Infer.inferTypeRef ctx env)
                          typeRefs

                      let extends =
                        List.map
                          (fun kind ->
                            match kind with
                            | TypeKind.TypeRef typeRef -> typeRef
                            | _ -> failwith "Invalid type for extends")
                          extends

                      return Some extends
                    }
                  | None -> Result.Ok None

                let kind =
                  TypeKind.Object
                    { Extends = extends
                      Implements = None // TODO
                      Elems = elems
                      Immutable = false
                      Interface = true }

                return { Kind = kind; Provenance = None }
              }

            let! newScheme =
              Infer.inferTypeDeclDefn ctx newEnv placeholder typeParams getType

            match placeholder.Type.Kind, newScheme.Type.Kind with
            | TypeKind.Object { Elems = existingElems },
              TypeKind.Object { Elems = newElems } ->
              // TODO: remove duplicates
              let mergedElems = existingElems @ newElems

              let kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None // TODO
                    Elems = mergedElems
                    Immutable = false
                    Interface = false }

              // NOTE: We explicitly don't generalize here because we want other
              // declarations to be able to unify with any free type variables
              // from this declaration.  We generalize things in `inferModule` and
              // `inferTreeRec`.
              // TODO: suport multiple provenances
              let t = { Kind = kind; Provenance = None }

              // We modify the existing scheme in place so that existing values
              // with this type are updated.
              placeholder.Type <- t
              qns.Schemes[key].Type <- t
            | _ ->
              // Replace the placeholder's type with the actual type.
              // NOTE: This is a bit hacky and we may want to change this later to use
              // `foldType` to replace any uses of the placeholder with the actual type.
              placeholder.Type <- newScheme.Type
              qns.Schemes[key].Type <- newScheme.Type

        match namespaceDecls with
        | [] -> ()
        | [ namespaceDecl ] ->
          return! Error(TypeError.NotImplemented "NamespaceDecl")
        | _ ->
          return!
            Error(
              TypeError.SemanticError "More than one namespace decl for ident"
            )

      return qns
    }

  let maybeEnumForExpr (ctx: Ctx) (env: Env) (expr: Expr) : option<Type> =
    let res =
      result {
        match expr.Kind with
        | ExprKind.Member(target, variantName, _) ->
          match target.Kind with
          | ExprKind.Identifier targetName ->
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

  type QDeclTree =
    { Edges: Map<Set<QDeclIdent>, Set<Set<QDeclIdent>>>
      CycleMap: Map<QDeclIdent, Set<QDeclIdent>> }

  // Based on the algorithm from https://en.wikipedia.org/wiki/Path-based_strong_component_algorithm
  let findStronglyConnectedComponents<'T>
    (graph: QGraph<'T>)
    : list<list<QDeclIdent>> =

    let mutable S: list<QDeclIdent> = [] // not yet assigned to a SCC
    let mutable P: list<QDeclIdent> = [] // not yet in different SCCs
    let mutable preorder: Map<QDeclIdent, int> = Map.empty
    let mutable C: int = 0
    let mutable components: list<list<QDeclIdent>> = []

    let rec visit (v: QDeclIdent) : unit =
      // 1. Set the preorder number of v to C, and increment C.
      preorder <- Map.add v C preorder
      C <- C + 1

      // 2. Push v onto S and also onto P.
      S <- v :: S
      P <- v :: P

      let deps =
        match graph.Edges.TryFind v with
        | None -> Set.empty
        | Some deps -> deps

      // 3. For each edge from v to a neighboring vertex w:
      for dep in deps do
        let w = dep

        match preorder.TryFind w with
        | None ->
          // If the preorder number of w has not yet been assigned (the edge is a
          // tree edge), recursively search w;
          visit w
        | Some _ ->
          // Otherwise, if w has not yet been assigned to a strongly connected
          // component (the edge is a forward/back/cross edge):
          if List.contains w S then
            // Repeatedly pop vertices from P until the top element of P has a
            // preorder number less than or equal to the preorder number of w
            while preorder[List.head P] > preorder[w] do
              P <- List.tail P // pop from P

      let mutable comp: list<QDeclIdent> = []

      // 4. If v is the top element of P:
      if v = List.head P then
        // Pop vertices from S until v has been popped, and assign the popped
        // vertices to a new component.
        while v <> List.head S do
          comp <- List.head S :: comp
          S <- List.tail S

        comp <- List.head S :: comp
        S <- List.tail S

        // Pop v from P.
        P <- List.tail P

        components <- comp :: components

    for v in graph.Nodes.Keys do
      if not (preorder.ContainsKey v) then
        visit v

    components


  type QCompTree = Map<Set<QDeclIdent>, Set<Set<QDeclIdent>>>

  let buildComponentTree<'T>
    (graph: QGraph<'T>)
    (components: list<list<QDeclIdent>>)
    : QCompTree =

    let comps = List.map (fun comp -> Set.ofList comp) components
    let mutable compMap: Map<QDeclIdent, Set<QDeclIdent>> = Map.empty

    for comp in comps do
      for v in comp do
        compMap <- Map.add v comp compMap

    let mutable tree: QCompTree = Map.empty

    for comp in comps do
      let mutable targets = Set.empty

      let mutable compDepNodes = Set.empty

      for node in comp do
        let nodeDeps =
          match graph.Edges.TryFind node with
          | None -> Set.empty
          | Some deps -> deps

        compDepNodes <- Set.union (Set.difference nodeDeps comp) compDepNodes

      let compDeps = Set.map (fun dep -> Map.find dep compMap) compDepNodes
      tree <- Map.add comp compDeps tree

    tree

  let findEntryPoints (tree: QCompTree) : Set<Set<QDeclIdent>> =
    let mutable allDeps = Set.empty

    for KeyValue(_, deps) in tree do
      allDeps <- Set.union allDeps deps

    Set.difference (Set.ofSeq tree.Keys) allDeps

  let addBinding
    (env: Env)
    (ident: QualifiedGraph.QualifiedIdent)
    (binding: Binding)
    : Env =

    let rec addValueRec (ns: Namespace) (parts: list<string>) : Namespace =
      match parts with
      | [] -> failwith "Invalid qualified ident"
      | [ name ] -> ns.AddBinding name binding
      | headNS :: restNS ->
        match ns.Namespaces.TryFind(headNS) with
        | None ->
          let newNS = { Namespace.empty with Name = headNS }
          ns.AddNamespace headNS (addValueRec newNS restNS)
        | Some existingNS ->
          ns.AddNamespace headNS (addValueRec existingNS restNS)

    let parts =
      match ident.Parts with
      | "global" :: rest -> rest
      | parts -> parts

    { env with
        Namespace = addValueRec env.Namespace parts }

  let addScheme
    (env: Env)
    (ident: QualifiedGraph.QualifiedIdent)
    (scheme: Scheme)
    : Env =

    let rec addSchemeRec (ns: Namespace) (parts: list<string>) : Namespace =
      match parts with
      | [] -> failwith "Invalid qualified ident"
      | [ name ] -> ns.AddScheme name scheme
      | headNS :: restNS ->
        match ns.Namespaces.TryFind(headNS) with
        | None ->
          let newNS = { Namespace.empty with Name = headNS }
          ns.AddNamespace headNS (addSchemeRec newNS restNS)
        | Some existingNS ->
          ns.AddNamespace headNS (addSchemeRec existingNS restNS)

    let parts =
      match ident.Parts with
      | "global" :: rest -> rest
      | parts -> parts

    { env with
        Namespace = addSchemeRec env.Namespace parts }

  let updateEnvWithQualifiedNamespace
    (env: Env)
    (inferredLocals: QualifiedNamespace)
    : Env =
    let mutable newEnv = env

    for KeyValue(ident, binding) in inferredLocals.Values do
      newEnv <- addBinding newEnv ident binding

    for KeyValue(ident, scheme) in inferredLocals.Schemes do
      newEnv <- addScheme newEnv ident scheme

    newEnv

  let updateQualifiedNamespace
    (src: QualifiedNamespace)
    (dst: QualifiedNamespace)
    : QualifiedNamespace =
    let mutable dst = dst

    for KeyValue(key, binding) in src.Values do
      dst <- dst.AddValue key binding

    for KeyValue(key, scheme) in src.Schemes do
      dst <- dst.AddScheme key scheme

    dst

  let generalizeBindings
    (bindings: Map<QualifiedGraph.QualifiedIdent, Binding>)
    : Map<QualifiedGraph.QualifiedIdent, Binding> =
    let mutable newBindings = Map.empty

    for KeyValue(name, (t, isMut)) in bindings do
      let t = generalizeType t
      newBindings <- newBindings.Add(name, (t, isMut))

    newBindings

  let inferTree
    (ctx: Ctx)
    (env: Env)
    (shouldGeneralize: bool)
    (graph: QGraph<Decl>)
    (tree: QCompTree)
    : Result<Env, TypeError> =

    result {
      let mutable newEnv = env
      let entryPoints = findEntryPoints tree

      let mutable processed: Map<Set<QDeclIdent>, QualifiedNamespace> =
        Map.empty

      let rec inferTreeRec
        (ctx: Ctx)
        (env: Env)
        (root: Set<QDeclIdent>)
        (graph: QGraph<Decl>)
        (tree: QCompTree)
        (fullQns: QualifiedNamespace)
        : Result<QualifiedNamespace * QualifiedNamespace, TypeError> =

        result {
          if Map.containsKey root processed then
            return fullQns, processed[root]
          else
            let mutable fullQns = fullQns
            let mutable partialQns = QualifiedNamespace.Empty

            // Infer dependencies
            match tree.TryFind root with
            | Some deps ->
              for dep in deps do
                // TODO: avoid re-inferring types if they've already been inferred
                let! newFullQns, newPartialQns =
                  inferTreeRec ctx env dep graph tree fullQns

                // Update partialQns with new values and types
                partialQns <- updateQualifiedNamespace newPartialQns partialQns

            | None -> ()

            // Update environment to include dependencies
            let mutable newEnv = updateEnvWithQualifiedNamespace env partialQns

            let names = Set.toList root

            // Infer declarations
            let! newPartialQns =
              inferDeclPlaceholders ctx newEnv partialQns names graph

            newEnv <- updateEnvWithQualifiedNamespace newEnv newPartialQns // mutually recursive functions

            let! newPartialQns =
              inferDeclDefinitions ctx newEnv newPartialQns names graph

            // Generalize bindings
            let newPartialQns =
              if shouldGeneralize then
                { newPartialQns with
                    Values = generalizeBindings newPartialQns.Values }
              else
                newPartialQns

            // Update fullQns with new values and types
            fullQns <- updateQualifiedNamespace newPartialQns fullQns

            processed <- Map.add root newPartialQns processed

            return fullQns, newPartialQns
        }

      let mutable qns = QualifiedNamespace.Empty

      // Infer entry points and all their dependencies
      for entryPoint in entryPoints do
        try
          let! fullQns, _ = inferTreeRec ctx env entryPoint graph tree qns
          qns <- fullQns
        with e ->
          printfn $"Error: {e}"
          return! Error(TypeError.SemanticError(e.ToString()))

      // NOTE: We could also return inferredLocals instead of adding them to the
      // environment.  The only time we actually need to add things to the
      // environment is when we're dealing with globals.

      // Update environment with all new values and types
      return updateEnvWithQualifiedNamespace env qns
    }

  let inferGraph
    (ctx: Ctx)
    (env: Env)
    (graph: QGraph<Decl>)
    : Result<Env, TypeError> =
    result {
      // TODO: handle imports

      let components = findStronglyConnectedComponents graph
      let tree = buildComponentTree graph components
      let! newEnv = inferTree ctx env true graph tree

      return newEnv
    }

  let inferStmts
    (ctx: Ctx)
    (env: Env)
    (shouldGeneralize: bool)
    (stmts: List<Stmt>)
    =
    result {
      let decls =
        stmts
        |> List.choose (fun (stmt: Stmt) ->
          match stmt.Kind with
          | StmtKind.Decl decl -> Some decl
          | _ -> None)

      let graph = buildGraph env decls
      let components = findStronglyConnectedComponents graph
      let tree = buildComponentTree graph components

      let! newEnv = inferTree ctx env shouldGeneralize graph tree

      for stmt in stmts do
        match stmt.Kind with
        | StmtKind.Expr expr ->
          let! _ = inferExpr ctx newEnv None expr
          ()
        | StmtKind.For(pattern, right, body) ->
          let mutable blockEnv = newEnv

          let! patBindings, patType = inferPattern ctx blockEnv pattern
          let! rightType = inferExpr ctx blockEnv None right

          let symbol =
            match env.TryFindValue "Symbol" with
            | Some scheme -> fst scheme
            | None -> failwith "Symbol not in scope"

          let! symbolIterator =
            getPropType
              ctx
              blockEnv
              symbol
              (PropName.String "iterator")
              false
              ValueCategory.RValue

          // TODO: only lookup Symbol.iterator on Array for arrays and tuples
          let arrayScheme =
            match env.TryFindScheme "Array" with
            | Some scheme -> scheme
            | None -> failwith "Array not in scope"

          let propName =
            match symbolIterator.Kind with
            | TypeKind.UniqueSymbol id -> PropName.Symbol id
            | _ -> failwith "Symbol.iterator is not a unique symbol"

          let! _ =
            getPropType
              ctx
              blockEnv
              arrayScheme.Type
              propName
              false
              ValueCategory.RValue

          // TODO: add a variant of `ExpandType` that allows us to specify a
          // predicate that can stop the expansion early.
          let! expandedRightType =
            expandType ctx blockEnv None Map.empty rightType

          let! elemType =
            result {
              match expandedRightType.Kind with
              | TypeKind.Array { Elem = elem; Length = _ } -> return elem
              | TypeKind.Tuple { Elems = elems } -> return union elems
              | TypeKind.Range _ -> return expandedRightType
              | TypeKind.Object _ ->
                // TODO: try using unify and/or an utility type to extract the
                // value type from an iterator

                // TODO: add a `tryGetPropType` function that returns an option
                let! next =
                  getPropType
                    ctx
                    blockEnv
                    rightType
                    (PropName.String "next")
                    false
                    ValueCategory.RValue

                match next.Kind with
                | TypeKind.Function f ->
                  return!
                    getPropType
                      ctx
                      blockEnv
                      f.Return
                      (PropName.String "value")
                      false
                      ValueCategory.RValue
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

          do! unify ctx blockEnv None elemType patType

          for KeyValue(name, binding) in patBindings do
            blockEnv <- newEnv.AddValue name binding

          let! _ = inferStmts ctx blockEnv false body.Stmts
          ()
        | StmtKind.Return expr ->
          match expr with
          | Some(expr) ->
            let! _ = inferExpr ctx newEnv None expr
            ()
          | None -> ()
        | StmtKind.Decl _ -> () // Already inferred

      return newEnv
    }

  let inferModule (ctx: Ctx) (env: Env) (ast: Module) : Result<Env, TypeError> =
    result {
      // TODO: update this function to accept a filename
      let mutable newEnv = env // { env with Filename = "input.esc" }

      let imports =
        List.choose
          (fun item ->
            match item with
            | Import import -> Some import
            | _ -> None)
          ast.Items

      for import in imports do
        let! importEnv = Infer.inferImport ctx newEnv import
        newEnv <- importEnv

      let stmts =
        ast.Items
        |> List.choose (fun (item: ModuleItem) ->
          match item with
          | ModuleItem.Stmt stmt -> Some stmt
          | _ -> None)

      return! inferStmts ctx newEnv true stmts
    }
