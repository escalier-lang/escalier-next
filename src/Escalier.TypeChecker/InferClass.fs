namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.Data.Visitor

open Error
open Env
open Poly
open InferExpr
open InferTypeAnn

module InferClass =
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
                    Mutable = false
                    Scheme = None }
              Provenance = None }))

      let selfType =
        { Kind =
            TypeKind.TypeRef
              { Name = QualifiedIdent.Ident className
                TypeArgs = typeArgs
                Mutable = false
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
              ctx.InferTypeAnn ctx newEnv typeAnn
            | Some typeAnn, None -> ctx.InferTypeAnn ctx newEnv typeAnn
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
                Mutable = false
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
                Mutable = false
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

                let! extends = inferTypeRef ctx env typeRef

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
                    Mutable = false
                    Interface = false }
              Provenance = None }

          return objType
        }

      let! newScheme =
        InferExpr.inferTypeDeclDefn
          ctx
          newEnv
          placeholder
          cls.TypeParams
          getType

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
            Mutable = false
            Interface = false }

      return staticObjType, placeholder
    }
