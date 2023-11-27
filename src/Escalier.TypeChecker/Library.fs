namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type

open Env
open Error
open Poly
open TypeVariable
open Visitor

module rec TypeChecker =

  let occursInType (v: Type) (t2: Type) : bool =
    match (prune t2).Kind with
    | pruned when pruned = v.Kind -> true
    | TypeKind.TypeRef({ TypeArgs = typeArgs }) ->
      match typeArgs with
      | Some(typeArgs) -> occursIn v typeArgs
      | None -> false
    | _ -> false

  let occursIn (t: Type) (types: list<Type>) : bool =
    List.exists (occursInType t) types

  let bind (t1: Type) (t2: Type) =
    let t1 = prune t1
    let t2 = prune t2

    result {
      if t1.Kind <> t2.Kind then
        if occursInType t1 t2 then
          return! Error(TypeError.RecursiveUnification)

        match t1.Kind with
        | TypeKind.TypeVar(v) ->
          v.Instance <- Some(t2)
          return ()
        | _ -> return! Error(TypeError.NotImplemented "bind error")
    }

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
    | PatternKind.Tuple elems -> Pattern.Tuple(List.map patternToPattern elems)
    | PatternKind.Wildcard -> Pattern.Wildcard
    | PatternKind.Literal(span, lit) -> Pattern.Literal(lit)

  type TypeChecker() =

    ///Unify the two types t1 and t2. Makes the types t1 and t2 the same.
    member this.Unify
      (env: Env)
      (t1: Type)
      (t2: Type)
      : Result<unit, TypeError> =
      // printfn "unify %A %A" t1 t2

      result {
        match (prune t1).Kind, (prune t2).Kind with
        | TypeKind.TypeVar _, _ -> do! bind t1 t2
        | _, TypeKind.TypeVar _ -> do! this.Unify env t2 t1
        | TypeKind.Tuple(elems1), TypeKind.Tuple(elems2) ->
          if List.length elems1 <> List.length elems2 then
            return! Error(TypeError.TypeMismatch(t1, t2))

          ignore (List.map2 (this.Unify env) elems1 elems2)
        | TypeKind.Function(f1), TypeKind.Function(f2) ->
          // TODO: check if `f1` and `f2` have the same type params
          let! f1 = instantiateFunc f1 None
          let! f2 = instantiateFunc f2 None

          let paramList1 =
            List.map (fun (param: FuncParam) -> param.Type) f1.ParamList

          let paramList2 =
            List.map (fun (param: FuncParam) -> param.Type) f2.ParamList

          if paramList1.Length > paramList2.Length then
            // t1 needs to have at least as many params as t2
            return! Error(TypeError.TypeMismatch(t1, t2))

          for i in 0 .. paramList1.Length - 1 do
            let param1 = paramList1[i]
            let param2 = paramList2[i]
            do! this.Unify env param2 param1 // params are contravariant

          do! this.Unify env f1.Return f2.Return // returns are covariant
        | TypeKind.TypeRef({ Name = name1; TypeArgs = types1 }),
          TypeKind.TypeRef({ Name = name2; TypeArgs = types2 }) ->

          if (name1 <> name2) then
            return! Error(TypeError.TypeMismatch(t1, t2))

          match (types1, types2) with
          | None, None -> ()
          | Some(types1), Some(types2) ->
            if List.length types1 <> List.length types2 then
              return! Error(TypeError.TypeMismatch(t1, t2))

            ignore (List.map2 (this.Unify env) types1 types2)
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
        | TypeKind.Literal lit,
          TypeKind.TypeRef({ Name = name; TypeArgs = typeArgs }) ->
          // TODO: check that `typeArgs` is `None`
          match lit, name with
          | Literal.Number _, "number" -> ()
          | Literal.String _, "string" -> ()
          | Literal.Boolean _, "boolean" -> ()
          | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
        | TypeKind.Literal l1, TypeKind.Literal l2 ->
          match l1, l2 with
          | Literal.Number n1, Literal.Number n2 when n1 = n2 -> ()
          | Literal.String s1, Literal.String s2 when s1 = s2 -> ()
          | Literal.Boolean b1, Literal.Boolean b2 when b1 = b2 -> ()
          | Literal.Null, Literal.Null -> ()
          | Literal.Undefined, Literal.Undefined -> ()
          | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
        | TypeKind.Object elems1, TypeKind.Object elems2 ->

          let namedProps1 =
            List.choose
              (fun (elem: ObjTypeElem) ->
                match elem with
                // TODO: handle methods, setters, and getters
                | Property p -> Some(p.Name, p)
                | _ -> None)
              elems1
            |> Map.ofList

          let namedProps2 =
            List.choose
              (fun (elem: ObjTypeElem) ->
                match elem with
                // TODO: handle methods, setters, and getters
                | Property p -> Some(p.Name, p)
                | _ -> None)
              elems2
            |> Map.ofList

          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          for KeyValue(name, prop2) in namedProps2 do
            match namedProps1.TryFind name with
            | Some(prop1) ->
              let p1Type =
                match prop1.Optional with
                | true -> union [ prop1.Type; undefined ]
                | false -> prop1.Type

              let p2Type =
                match prop2.Optional with
                | true -> union [ prop2.Type; undefined ]
                | false -> prop2.Type

              do! this.Unify env p1Type p2Type
            | None ->
              if not prop2.Optional then
                return! Error(TypeError.TypeMismatch(t1, t2))

        | TypeKind.Object allElems, TypeKind.Intersection types ->
          let mutable combinedElems = []
          let mutable restTypes = []

          for t in types do
            match t.Kind with
            | TypeKind.Object elems -> combinedElems <- combinedElems @ elems
            | TypeKind.Rest t -> restTypes <- t :: restTypes
            | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

          let objType =
            { Kind = TypeKind.Object(combinedElems)
              Provenance = None }

          match restTypes with
          | [] -> do! this.Unify env t1 objType
          | [ restType ] ->
            let objElems, restElems =
              List.partition
                (fun (ae: ObjTypeElem) ->
                  List.exists
                    (fun ce ->
                      match ae, ce with
                      | Property ap, Property cp -> ap.Name = cp.Name
                      | _ -> false)
                    combinedElems)
                allElems

            let newObjType =
              { Kind = TypeKind.Object(objElems)
                Provenance = None }

            do! this.Unify env newObjType objType

            let newRestType =
              { Kind = TypeKind.Object(restElems)
                Provenance = None }

            do! this.Unify env newRestType restType
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        | TypeKind.Intersection types, TypeKind.Object allElems ->
          let mutable combinedElems = []
          let mutable restTypes = []

          for t in types do
            match t.Kind with
            | TypeKind.Object elems -> combinedElems <- combinedElems @ elems
            | TypeKind.Rest t -> restTypes <- t :: restTypes
            | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

          let objType =
            { Kind = TypeKind.Object(combinedElems)
              Provenance = None }

          match restTypes with
          | [] -> do! this.Unify env objType t2
          | [ restType ] ->
            let objElems, restElems =
              List.partition
                (fun (ae: ObjTypeElem) ->
                  List.exists
                    (fun ce ->
                      match ae, ce with
                      | Property ap, Property cp -> ap.Name = cp.Name
                      | _ -> false)
                    combinedElems)
                allElems

            let newObjType =
              { Kind = TypeKind.Object(objElems)
                Provenance = None }

            do! this.Unify env objType newObjType

            let newRestType =
              { Kind = TypeKind.Object(restElems)
                Provenance = None }

            do! this.Unify env restType newRestType
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        | _, TypeKind.Union(types) ->

          let unifier =
            List.tryFind (fun t -> this.Unify env t1 t |> Result.isOk) types

          match unifier with
          | Some _ -> return ()
          | _ -> return! Error(TypeError.TypeMismatch(t1, t2))

        | _, _ ->

          let t1' = env.ExpandType t1
          let t2' = env.ExpandType t2

          if t1' <> t1 || t2' <> t2 then
            return! this.Unify env t1' t2'
          else
            return! Error(TypeError.TypeMismatch(t1, t2))
      }

    member this.UnifyCall
      (args: list<Expr>)
      (typeArgs: option<list<Type>>)
      (callee: Type)
      (env: Env)
      : Result<(Type * Type), TypeError> =

      result {
        let callee = prune callee

        let retType = TypeVariable.makeVariable None
        let throwsType = TypeVariable.makeVariable None

        match callee.Kind with
        | TypeKind.Function func ->
          return! this.UnifyFuncCall args typeArgs retType throwsType func env
        | TypeKind.TypeVar _ ->

          // TODO: use a `result {}` CE here
          let! argTypes =
            List.traverseResultM (fun arg -> this.InferExpr arg env) args

          let paramList =
            List.mapi
              (fun i t ->
                let p: Pattern = Pattern.Identifier $"arg{i}"

                { Pattern = p
                  Type = t
                  Optional = false })
              argTypes

          let callType =
            { Type.Kind =
                TypeKind.Function
                  { ParamList = paramList
                    Return = retType
                    Throws = throwsType
                    TypeParams = None } // TODO
              Provenance = None }

          match bind callee callType with
          | Ok _ -> return (prune retType, prune throwsType)
          | Error e -> return! Error e
        | kind -> return! Error(TypeError.NotImplemented $"kind = {kind}")
      }

    member this.UnifyFuncCall
      (args: list<Expr>)
      (typeArgs: option<list<Type>>)
      (retType: Type)
      (throwsType: Type)
      (callee: Function)
      (env: Env)
      : Result<(Type * Type), TypeError> =

      result {
        let! callee =
          result {
            if callee.TypeParams.IsSome then
              return! instantiateFunc callee typeArgs
            else
              return callee
          }

        let! args =
          List.traverseResultM
            (fun (arg: Expr) ->
              result {
                let! argType = this.InferExpr arg env
                return arg, argType
              })
            args

        for ((arg, argType), param) in List.zip args callee.ParamList do
          if
            param.Optional && argType.Kind = TypeKind.Literal(Literal.Undefined)
          then
            ()
          else
            // TODO: check_mutability of `arg`
            do! this.Unify env argType param.Type // contravariant

        do! this.Unify env retType callee.Return // covariant
        do! this.Unify env throwsType callee.Throws // covariant

        return (retType, throwsType)
      }

    ///Computes the type of the expression given by node.
    ///The type of the node is computed in the context of the
    ///supplied type environment env. Data types can be introduced into the
    ///language simply by having a predefined set of identifiers in the initial
    ///environment. environment; this way there is no need to change the syntax or, more
    ///importantly, the type-checking program when extending the language.
    member this.InferExpr (expr: Expr) (env: Env) : Result<Type, TypeError> =
      let r =
        result {
          match expr.Kind with
          | ExprKind.Identifier(name) -> return! env.GetType name
          | ExprKind.Literal(literal) ->
            return
              { Type.Kind = TypeKind.Literal(literal)
                Provenance = None }
          | ExprKind.Call call ->
            let! callee = this.InferExpr call.Callee env

            let! result, throws = this.UnifyCall call.Args None callee env

            // TODO: handle throws

            return result
          | ExprKind.Binary(op, left, right) ->
            let! funTy = env.GetType op
            let! result, throws = this.UnifyCall [ left; right ] None funTy env

            // TODO: handle throws

            return result
          | ExprKind.Function f ->
            let mutable newEnv = env

            let! paramList =
              List.traverseResultM
                (fun (param: Syntax.FuncParam<option<TypeAnn>>) ->
                  result {
                    let! paramType =
                      match param.TypeAnn with
                      | Some(typeAnn) -> this.InferTypeAnn env typeAnn
                      | None -> Result.Ok(makeVariable None)

                    let! assumps, patternType =
                      this.InferPattern env param.Pattern

                    do! this.Unify env patternType paramType

                    for KeyValue(name, binding) in assumps do
                      // TODO: update `Env.types` to store `Binding`s insetad of `Type`s
                      newEnv <- newEnv.AddValue name binding

                    return
                      { Pattern = patternToPattern param.Pattern
                        Type = paramType
                        Optional = false }
                  })
                f.Sig.ParamList

            this.InferBlockOrExpr newEnv f.Body |> ignore

            let retExprs = findReturns f

            let undefined =
              { Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }

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

            // TODO: move this up so that we can reference any type params in the
            // function params, body, return or throws
            let! typeParams =
              match f.Sig.TypeParams with
              | Some(typeParams) ->
                List.traverseResultM (this.InferTypeParam env) typeParams
                |> Result.map Some
              | None -> Ok None

            return makeFunctionType typeParams paramList retType
          | ExprKind.Tuple elems ->
            let! elems =
              List.traverseResultM (fun elem -> this.InferExpr elem env) elems

            return
              { Type.Kind = TypeKind.Tuple(elems)
                Provenance = None }
          | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
            let! conditionTy = this.InferExpr condition env

            let! thenBranchTy =
              this.InferBlockOrExpr env (thenBranch |> BlockOrExpr.Block)

            let! elseBranchTy =
              Option.traverseResult (this.InferBlockOrExpr env) elseBranch

            do! this.Unify env conditionTy boolType

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
                      let! t = this.InferExpr value env

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
                      let! t = this.InferExpr value env
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
            let! objType = this.InferExpr obj env

            // TODO: handle optional chaining
            // TODO: lookup properties on object type
            return this.GetPropType env objType prop optChain
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

    member this.GetPropType
      (env: Env)
      (t: Type)
      (name: string)
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
          this.GetPropType env (env.ExpandScheme scheme typeArgs) name optChain
        | None ->
          match env.Schemes.TryFind typeRefName with
          | Some scheme ->
            this.GetPropType
              env
              (env.ExpandScheme scheme typeArgs)
              name
              optChain
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
            let t = this.GetPropType env t name optChain

            let undefined =
              { Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }

            union [ t; undefined ]
          | _ -> failwith "TODO: lookup member on union type"

      // TODO: intersection types
      // TODO: union types
      | _ -> failwith $"TODO: lookup member on type - {t}"

    member this.InferBlockOrExpr
      (env: Env)
      (blockOrExpr: BlockOrExpr)
      : Result<Type, TypeError> =
      result {
        let mutable newEnv = env

        match blockOrExpr with
        | BlockOrExpr.Block({ Stmts = stmts }) ->
          for stmt in stmts do
            let! stmtEnv = this.InferStmt stmt newEnv false
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
        | BlockOrExpr.Expr expr -> return! this.InferExpr expr newEnv
      }

    member this.InferTypeParam
      (env: Env)
      (tp: Syntax.TypeParam)
      : Result<TypeParam, TypeError> =
      result {
        let! c =
          match tp.Constraint with
          | Some(c) -> this.InferTypeAnn env c |> Result.map Some
          | None -> Ok None

        let! d =
          match tp.Default with
          | Some(d) -> this.InferTypeAnn env d |> Result.map Some
          | None -> Ok None

        return
          { Name = tp.Name
            Constraint = c
            Default = d }
      }

    member this.InferTypeAnn
      (env: Env)
      (typeAnn: TypeAnn)
      : Result<Type, TypeError> =
      let kind: Result<TypeKind, TypeError> =
        result {
          match typeAnn.Kind with
          | TypeAnnKind.Array elem ->
            let! elem = this.InferTypeAnn env elem
            return TypeKind.Array elem
          | TypeAnnKind.Literal lit -> return TypeKind.Literal(lit)
          | TypeAnnKind.Keyword keyword ->
            match keyword with
            | KeywordTypeAnn.Boolean -> return makePrimitiveKind "boolean"
            | KeywordTypeAnn.Number -> return makePrimitiveKind "number"
            | KeywordTypeAnn.String -> return makePrimitiveKind "string"
            | KeywordTypeAnn.Symbol -> return makePrimitiveKind "symbol"
            | KeywordTypeAnn.Null -> return TypeKind.Literal(Literal.Null)
            | KeywordTypeAnn.Undefined ->
              return TypeKind.Literal(Literal.Undefined)
            | KeywordTypeAnn.Unknown -> return makePrimitiveKind "unknown"
            | KeywordTypeAnn.Never -> return makePrimitiveKind "never"
            | KeywordTypeAnn.Object -> return makePrimitiveKind "object"
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
                      let! t = this.InferTypeAnn env typeAnn

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
            let! elems = List.traverseResultM (this.InferTypeAnn env) elems
            return TypeKind.Tuple(elems)
          | TypeAnnKind.Union types ->
            let! types = List.traverseResultM (this.InferTypeAnn env) types
            return TypeKind.Union(types)
          | TypeAnnKind.Intersection types ->
            let! types = List.traverseResultM (this.InferTypeAnn env) types
            return TypeKind.Intersection types
          | TypeAnnKind.TypeRef(name, typeArgs) ->
            match typeArgs with
            | Some(typeArgs) ->
              let! typeArgs =
                List.traverseResultM (this.InferTypeAnn env) typeArgs

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
            let! returnType = this.InferTypeAnn env functionType.ReturnType

            let! throws =
              match functionType.Throws with
              | Some(throws) -> this.InferTypeAnn env throws
              | None ->
                Result.Ok(
                  { Type.Kind = makePrimitiveKind "never"
                    Provenance = None }
                )

            let! paramList =
              List.traverseResultM
                (fun (p: FuncParam<TypeAnn>) ->
                  result {
                    let! t = this.InferTypeAnn env p.TypeAnn
                    let pattern = patternToPattern p.Pattern

                    return
                      { Pattern = pattern
                        Type = t
                        Optional = false }
                  })
                functionType.ParamList

            let f =
              { TypeParams = None
                ParamList = paramList
                Return = returnType
                Throws = throws }

            return TypeKind.Function(f)
          | TypeAnnKind.Keyof target ->
            return! this.InferTypeAnn env target |> Result.map TypeKind.KeyOf
          | TypeAnnKind.Rest target ->
            return! this.InferTypeAnn env target |> Result.map TypeKind.Rest
          | TypeAnnKind.Typeof target ->
            return!
              Error(TypeError.NotImplemented "TODO: this.InferTypeAnn - Typeof") // TODO: add Typeof to TypeKind
          | TypeAnnKind.Index(target, index) ->
            let! target = this.InferTypeAnn env target
            let! index = this.InferTypeAnn env index
            return TypeKind.Index(target, index)
          | TypeAnnKind.Condition conditionType ->
            let! check = this.InferTypeAnn env conditionType.Check
            let! extends = this.InferTypeAnn env conditionType.Extends
            let! trueType = this.InferTypeAnn env conditionType.TrueType
            let! falseType = this.InferTypeAnn env conditionType.FalseType
            return TypeKind.Condition(check, extends, trueType, falseType)
          | TypeAnnKind.Match matchType ->
            return!
              Error(TypeError.NotImplemented "TODO: this.InferTypeAnn - Match") // TODO
          | TypeAnnKind.Infer name -> return TypeKind.Infer name
          | TypeAnnKind.Wildcard -> return TypeKind.Wildcard
          | TypeAnnKind.Binary(left, op, right) ->
            let! left = this.InferTypeAnn env left
            let! right = this.InferTypeAnn env right
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

    // TODO: Return an updated `Env` instead of requiring `InferScript` to do the updates
    member this.InferStmt
      (stmt: Stmt)
      (env: Env)
      (generalize: bool)
      : Result<Env, TypeError> =
      result {
        match stmt.Kind with
        | StmtKind.Expr expr ->
          let! _ = this.InferExpr expr env
          return env
        | StmtKind.For(pattern, right, block) ->
          return! Error(TypeError.NotImplemented "TODO: infer for")
        | StmtKind.Decl({ Kind = DeclKind.VarDecl(pattern, init, typeAnn) }) ->
          let! patBindings, patType = this.InferPattern env pattern
          let mutable newEnv = env

          for KeyValue(name, binding) in patBindings do
            newEnv <- newEnv.AddValue name binding

          let! initType = this.InferExpr init newEnv

          match typeAnn with
          | Some(typeAnn) ->
            let! typeAnnType = this.InferTypeAnn env typeAnn
            do! this.Unify env initType typeAnnType
            do! this.Unify env typeAnnType patType
          | None -> do! this.Unify env initType patType

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

          let! t = this.InferTypeAnn env typeAnn

          let typeParams =
            Option.map
              (List.map (fun (param: Syntax.TypeParam) -> param.Name))
              typeParams

          let scheme = { TypeParams = typeParams; Type = t }

          return env.AddScheme name scheme
        | StmtKind.Return expr ->
          match expr with
          | Some(expr) ->
            let! _ = this.InferExpr expr env
            return env
          | None -> return env
      }

    member this.InferPattern
      (env: Env)
      (pat: Syntax.Pattern)
      : Result<BindingAssump * Type, TypeError> =
      let mutable assump = BindingAssump([])

      let rec infer_pattern_rec (pat: Syntax.Pattern) : Type =
        match pat.Kind with
        | PatternKind.Identifier({ Name = name; IsMut = isMut }) ->
          let t = TypeVariable.makeVariable None

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
                  let t = TypeVariable.makeVariable None

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

    // TODO: Create an `InferModule` that treats all decls as mutually recursive
    member this.InferScript
      (stmts: list<Stmt>)
      (env: Env)
      : Result<Env, TypeError> =
      result {
        let mutable newEnv = env

        let! _ =
          List.traverseResultM
            (fun stmt ->
              result {
                let! stmtEnv = this.InferStmt stmt newEnv true
                newEnv <- stmtEnv
              })
            stmts

        return newEnv
      }

  let findReturns (f: Syntax.Function) : list<Expr> =
    let mutable returns: list<Expr> = []

    let visitor =
      { VisitExpr =
          fun expr ->
            match expr.Kind with
            | ExprKind.Function _ -> false
            | _ -> true
        VisitStmt =
          fun stmt ->
            match stmt.Kind with
            | StmtKind.Return expr ->
              match expr with
              | Some expr -> returns <- expr :: returns
              | None -> ()
            | _ -> ()

            true
        VisitPattern = fun _ -> false
        VisitTypeAnn = fun _ -> false }

    match f.Body with
    | BlockOrExpr.Block block -> List.iter (walkStmt visitor) block.Stmts
    | BlockOrExpr.Expr expr ->
      walkExpr visitor expr // There might be early returns in match expression
      returns <- expr :: returns // We treat the expression as a return in this case

    returns
