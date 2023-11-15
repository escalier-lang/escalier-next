namespace Escalier.HindleyMilner

open Escalier.HindleyMilner.Syntax
open FsToolkit.ErrorHandling
open System.Collections.Generic

open Type
open Errors

module TypeVariable =
  let mutable nextVariableId = 0

  let makeVariable bound =
    let newVar =
      { Id = nextVariableId
        Bound = bound
        Instance = None }

    nextVariableId <- nextVariableId + 1

    { Kind = TypeVar(newVar)
      Provenance = None }

module rec TypeChecker =
  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>
  type SchemeAssump = string * Scheme

  type Env =
    { Values: Map<string, Binding>
      Schemes: Map<string, Scheme>
      IsAsync: bool }

    member this.AddValue (name: string) (binding: Binding) =
      { this with
          Values = Map.add name binding this.Values }

    member this.AddScheme (name: string) (s: Scheme) =
      { this with
          Schemes = Map.add name s this.Schemes }

  type Assump = string * Binding

  let makePrimitiveKind name =
    { Name = name
      TypeArgs = None
      Scheme = None }
    |> TypeKind.TypeRef

  let makeFunctionType typeParams paramList ret =
    let never =
      { Kind = makePrimitiveKind "never"
        Provenance = None }

    { Kind =
        Function
          { TypeParams = typeParams
            ParamList = paramList
            Return = ret
            Throws = never }
      Provenance = None }

  let numType =
    { Kind = makePrimitiveKind "number"
      Provenance = None }

  let boolType =
    { Kind = makePrimitiveKind "boolean"
      Provenance = None }

  let strType =
    { Kind = makePrimitiveKind "string"
      Provenance = None }

  /// Returns the currently defining instance of t.
  /// As a side effect, collapses the list of type instances. The function Prune
  /// is used whenever a type expression has to be inspected: it will always
  /// return a type expression which is either an uninstantiated type variable or
  /// a type operator; i.e. it will skip instantiated variables, and will
  /// prune them from expressions to remove long chains of instantiated variables.
  let prune (t: Type) : Type =
    match t.Kind with
    | TypeVar({ Instance = Some(instance) } as v) ->
      let newInstance = prune instance
      v.Instance <- Some(newInstance)
      newInstance
    | _ -> t

  let foldType (f: Type -> option<Type>) (t: Type) : Type =
    let rec fold (t: Type) : Type =
      let t = prune t

      let t =
        match t.Kind with
        | TypeVar _ -> t
        | Function f ->
          { Kind =
              TypeKind.Function
                { f with
                    ParamList =
                      List.map
                        (fun param -> { param with Type = fold param.Type })
                        f.ParamList
                    // TODO: fold TypeParams
                    Return = fold f.Return }
            Provenance = None }
        | Tuple(elems) ->
          let elems = List.map fold elems

          { Kind = Tuple(elems)
            Provenance = None }
        | TypeRef({ Name = name
                    TypeArgs = typeArgs
                    Scheme = scheme }) ->
          let typeArgs = Option.map (List.map fold) typeArgs

          let scheme =
            Option.map
              (fun (scheme: Scheme) -> { scheme with Type = fold scheme.Type })
              scheme

          { Kind =
              TypeRef(
                { Name = name
                  TypeArgs = typeArgs
                  Scheme = scheme }
              )
            Provenance = None }
        | Literal _ -> t
        | Wildcard -> t
        | Object _ -> failwith "TODO: foldType - Object"
        | Rest t ->
          { Kind = Rest(fold t)
            Provenance = None }
        | Union types ->
          { Kind = Union(List.map fold types)
            Provenance = None }
        | Intersection types ->
          { Kind = Intersection(List.map fold types)
            Provenance = None }
        | Array t ->
          { Kind = Array(fold t)
            Provenance = None }
        | KeyOf t ->
          { Kind = KeyOf(fold t)
            Provenance = None }
        | Index(target, index) ->
          { Kind = Index(fold target, fold index)
            Provenance = None }
        | Condition(check, extends, trueType, falseType) ->
          { Kind =
              Condition(fold check, fold extends, fold trueType, fold falseType)
            Provenance = None }
        | Infer _ -> t
        | Binary(left, op, right) ->
          { Kind = Binary(fold left, op, fold right)
            Provenance = None }

      match f t with
      | Some(t) -> t
      | None -> t

    fold t

  let generalizeFunc (f: Function) : Function =
    let mutable mapping: Map<int, string> = Map.empty
    let mutable nextId = 0

    // QUESTION: should we call `prune` inside the folder as well?
    let folder t =
      match t.Kind with
      | TypeVar { Id = id } ->
        match Map.tryFind id mapping with
        | Some(name) ->
          Some(
            { Kind =
                TypeRef
                  { Name = name
                    TypeArgs = None
                    Scheme = None }
              Provenance = None }
          )
        | None ->
          let tpName = 65 + nextId |> char |> string
          nextId <- nextId + 1
          mapping <- mapping |> Map.add id tpName

          Some(
            { Kind =
                TypeRef
                  { Name = tpName
                    TypeArgs = None
                    Scheme = None }
              Provenance = None }
          )
      | _ -> None

    let paramList =
      List.map
        (fun (p: FuncParam) -> { p with Type = foldType folder p.Type })
        f.ParamList

    let ret = foldType folder f.Return
    let throws = foldType folder f.Throws

    let values = mapping.Values |> List.ofSeq

    let mutable newTypeParams: list<TypeParam> =
      List.map
        (fun name ->
          { Name = name
            Constraint = None
            Default = None })
        values

    Option.iter
      (fun typeParams ->
        for param in typeParams do
          newTypeParams <- newTypeParams @ [ param ])
      f.TypeParams

    { TypeParams = if newTypeParams.IsEmpty then None else Some(newTypeParams)
      ParamList = paramList
      Return = ret
      Throws = throws }

  let instantiateFunc
    (f: Function)
    (typeArgs: option<list<Type>>)
    : Result<Function, TypeError> =

    result {
      let mutable mapping: Map<string, Type> = Map.empty

      let folder t =
        match t.Kind with
        | TypeRef({ Name = name }) ->
          match Map.tryFind name mapping with
          | Some(tv) -> Some(tv)
          | None -> None
        | _ -> None

      match f.TypeParams with
      | Some(typeParams) ->
        match typeArgs with
        | Some(typeArgs) ->
          if typeArgs.Length <> typeParams.Length then
            return! Error(TypeError.WrongNumberOfTypeArgs)

          for tp, ta in List.zip typeParams typeArgs do
            mapping <- mapping.Add(tp.Name, ta)
        | None ->
          for tp in typeParams do
            mapping <-
              mapping.Add(tp.Name, TypeVariable.makeVariable tp.Constraint)
      | None -> ()

      return
        { TypeParams = None
          ParamList =
            List.map
              (fun param ->
                { param with
                    Type = foldType folder param.Type })
              f.ParamList
          Return = foldType folder f.Return
          Throws = foldType folder f.Throws }
    }

  let occursInType (v: Type) (t2: Type) : bool =
    match (prune t2).Kind with
    | pruned when pruned = v.Kind -> true
    | TypeRef({ TypeArgs = typeArgs }) ->
      match typeArgs with
      | Some(typeArgs) -> occursIn v typeArgs
      | None -> false
    | _ -> false

  let occursIn (t: Type) (types: list<Type>) : bool =
    List.exists (occursInType t) types

  let bind (t1: Type) (t2: Type) =
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

  let isIntegerLiteral (name: string) =
    match
      (try
        Some(int name)
       with _ex ->
         None)
    with
    | None -> false
    | Some _ -> true

  let isGeneric v nonGeneric = not (nonGeneric |> Set.contains v)

  ///Makes a copy of a type expression.
  ///The type t is copied. The the generic variables are duplicated and the
  ///nonGeneric variables are shared.
  let fresh (t: Type) (nonGeneric: Set<int>) : Type =
    let table = Dictionary<int, Type>()

    let rec loop tp =
      let t = prune tp

      match t.Kind with
      | TypeVar p ->
        if isGeneric p.Id nonGeneric then
          match table.ContainsKey p.Id with
          | false ->
            let newVar = TypeVariable.makeVariable None
            table.Add(p.Id, newVar)
            newVar
          | true -> table[p.Id]
        else
          t
      | Tuple elems ->
        { Kind = Tuple(List.map loop elems)
          Provenance = None }
      | Function f ->
        makeFunctionType
          f.TypeParams
          (List.map
            (fun param -> { param with Type = loop param.Type })
            f.ParamList)
          (loop f.Return)
      | TypeRef({ TypeArgs = typeArgs } as op) ->
        let kind =
          TypeRef(
            { op with
                TypeArgs = Option.map (List.map loop) typeArgs }
          )

        { Kind = kind; Provenance = None }
      | Literal _ -> t
      | _ -> t

    loop t

  ///Get the type of identifier name from the type environment env
  let getType (name: string) (env: Env) (nonGeneric: Set<int>) : Type =
    match env.Values |> Map.tryFind name with
    | Some(var) ->
      // TODO: check `isMut` and return an immutable type if necessary
      let (t, isMut) = var
      fresh t nonGeneric
    | None ->
      if isIntegerLiteral name then
        numType
      else
        failwithf $"Undefined symbol {name}"

  ///Unify the two types t1 and t2. Makes the types t1 and t2 the same.
  let unify (t1: Type) (t2: Type) : Result<unit, TypeError> =
    result {
      match (prune t1).Kind, (prune t2).Kind with
      | TypeVar _, _ -> do! bind t1 t2
      | _, TypeVar _ -> do! unify t2 t1
      | Tuple(elems1), Tuple(elems2) ->
        if List.length elems1 <> List.length elems2 then
          return! Error(TypeError.TypeMismatch(t1, t2))

        ignore (List.map2 unify elems1 elems2)
      | Function(f1), Function(f2) ->
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
          do! unify param2 param1 // params are contravariant

        do! unify f1.Return f2.Return // returns are covariant
      | TypeRef({ Name = name1; TypeArgs = types1 }),
        TypeRef({ Name = name2; TypeArgs = types2 }) ->

        if (name1 <> name2) then
          return! Error(TypeError.TypeMismatch(t1, t2))

        match (types1, types2) with
        | None, None -> ()
        | Some(types1), Some(types2) ->
          if List.length types1 <> List.length types2 then
            return! Error(TypeError.TypeMismatch(t1, t2))

          ignore (List.map2 unify types1 types2)
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | Literal lit, TypeRef({ Name = name; TypeArgs = typeArgs }) ->
        // TODO: check that `typeArgs` is `None`
        match lit, name with
        | Literal.Number _, "number" -> ()
        | Literal.String _, "string" -> ()
        | Literal.Boolean _, "boolean" -> ()
        | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
    }

  let unifyCall
    (args: list<Expr>)
    (typeArgs: option<list<Type>>)
    (callee: Type)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<(Type * Type), TypeError> =

    result {
      let callee = prune callee

      let retType = TypeVariable.makeVariable None
      let throwsType = TypeVariable.makeVariable None

      match callee.Kind with
      | TypeKind.Function func ->
        return!
          unifyFuncCall args typeArgs retType throwsType func env nonGeneric
      | TypeKind.TypeVar _ ->

        // TODO: use a `result {}` CE here
        let! argTypes =
          List.traverseResultM (fun arg -> inferExpr arg env nonGeneric) args

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

  let unifyFuncCall
    (args: list<Expr>)
    (typeArgs: option<list<Type>>)
    (retType: Type)
    (throwsType: Type)
    (callee: Function)
    (env: Env)
    (nonGeneric: Set<int>)
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
              let! argType = inferExpr arg env nonGeneric
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
          do! unify argType param.Type // contravariant

      do! unify retType callee.Return // covariant
      do! unify throwsType callee.Throws // covariant

      return (retType, throwsType)
    }

  ///Computes the type of the expression given by node.
  ///The type of the node is computed in the context of the
  ///supplied type environment env. Data types can be introduced into the
  ///language simply by having a predefined set of identifiers in the initial
  ///environment. environment; this way there is no need to change the syntax or, more
  ///importantly, the type-checking program when extending the language.
  let inferExpr
    (expr: Expr)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<Type, TypeError> =
    result {
      match expr.Kind with
      | ExprKind.Ident(name) -> return getType name env nonGeneric
      | ExprKind.Literal(literal) ->
        return
          { Type.Kind = Literal(literal)
            Provenance = None }
      | ExprKind.Call(callee, args) ->
        let! callee = inferExpr callee env nonGeneric

        let! result, throws = unifyCall args None callee env nonGeneric

        // TODO: handle throws

        return result
      | ExprKind.Binary(op, left, right) ->
        let funTy = getType op env nonGeneric

        let! result, throws =
          unifyCall [ left; right ] None funTy env nonGeneric

        // TODO: handle throws

        return result
      | ExprKind.Function f ->
        let mutable newEnv = env
        let mutable newNonGeneric = nonGeneric

        let! paramList =
          List.traverseResultM
            (fun (param: Syntax.FuncParam<option<TypeAnn>>) ->
              result {
                let paramType = TypeVariable.makeVariable None in
                let! assumps, patternType = inferPattern env param.Pattern
                do! unify patternType paramType

                for KeyValue(name, binding) in assumps do
                  // TODO: update `Env.types` to store `Binding`s insetad of `Type`s
                  newEnv <- newEnv.AddValue name binding

                match paramType.Kind with
                | TypeVar { Id = id } ->
                  newNonGeneric <- newNonGeneric |> Set.add id
                | _ -> ()

                return
                  { Pattern = patternToPattern param.Pattern
                    Type = paramType
                    Optional = false }
              })
            f.Sig.ParamList

        // TODO: search for return statements in blocks
        let! retTy =
          match f.Body with
          | BlockOrExpr.Block({ Stmts = stmts }) ->
            result {
              let! stmtTypes =
                List.traverseResultM
                  (fun stmt ->
                    result {
                      let! t, assump = inferStmt stmt newEnv newNonGeneric

                      match assump with
                      | Some(name, t) -> newEnv <- newEnv.AddValue name t
                      | None -> ()

                      return t
                    })
                  stmts

              match List.tryLast stmtTypes with
              | Some(t) ->
                match t with
                | Some(t) -> return t
                | None ->
                  return!
                    Error(
                      TypeError.SemanticError
                        "Last statement must be an expression"
                    )
              | None ->
                return! Error(TypeError.SemanticError "Empty lambda body")
            }
          | BlockOrExpr.Expr expr -> inferExpr expr newEnv newNonGeneric

        let! typeParams =
          match f.Sig.TypeParams with
          | Some(typeParams) ->
            List.traverseResultM (inferTypeParam env) typeParams
            |> Result.map Some
          | None -> Ok None

        return makeFunctionType typeParams paramList retTy // TODO: handle explicit type params
      | ExprKind.Tuple elems ->
        let! elems =
          List.traverseResultM (fun elem -> inferExpr elem env nonGeneric) elems

        return
          { Type.Kind = TypeKind.Tuple(elems)
            Provenance = None }
      | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
        let retTy = TypeVariable.makeVariable None

        let! conditionTy = inferExpr condition env nonGeneric
        let! thenBranchTy = inferExpr thenBranch env nonGeneric
        let! elseBranchTy = inferExpr elseBranch env nonGeneric

        do! unify conditionTy boolType
        do! unify thenBranchTy retTy
        do! unify elseBranchTy retTy

        return retTy
      | _ ->
        return!
          Error(TypeError.NotImplemented "TODO: finish implementing infer_expr")
    }

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
                  // let! t = infer_type_ann env p.typeAnn
                  // let pattern = pattern_to_pattern p.pattern

                  // let elem: ObjTypeElem =
                  //   match elem with
                  //   | Callable f ->

                  // return
                  //   { pattern = pattern
                  //     type_ = t
                  //     optional = false }

                  return!
                    Error(
                      TypeError.NotImplemented "TODO: inferTypeAnn - Object"
                    )
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
          let! returnType = inferTypeAnn env functionType.Ret

          let! throws =
            match functionType.Throws with
            | Some(throws) -> inferTypeAnn env throws
            | None ->
              Result.Ok(
                { Type.Kind = makePrimitiveKind "never"
                  Provenance = None }
              )

          let! paramList =
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
            { TypeParams = None
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

  let inferStmt
    (stmt: Stmt)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<option<Type> * option<Assump>, TypeError> =
    result {
      match stmt with
      | Stmt.Expr expr ->
        let! t = inferExpr expr env nonGeneric
        return (Some(t), None)
      | For(pattern, right, block) ->
        return! Error(TypeError.NotImplemented "TODO: infer for")
      | Let(name, definition) ->
        let! defnTy = inferExpr definition env nonGeneric
        let assump = (name, (defnTy, false)) // TODO: isMut
        return (None, Some(assump))
      | LetRec(name, defn) ->
        let newTy = TypeVariable.makeVariable None
        let newEnv = env.AddValue name (newTy, false) // TODO: isMut

        let newNonGeneric =
          match newTy.Kind with
          | TypeVar { Id = id } -> nonGeneric |> Set.add id
          | _ -> nonGeneric

        let! defnTy = inferExpr defn newEnv newNonGeneric
        do! unify newTy defnTy

        let binding = (newTy, false) // TODO: isMut
        return (None, Some(name, binding))
    }

  let inferPattern
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
      | PatternKind.Object elems -> failwith "todo"
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
    Result.Ok((assump, t))

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

  let inferScript (stmts: list<Stmt>) (env: Env) : Result<Env, TypeError> =
    result {

      let nonGeneric = Set.empty
      let mutable newEnv = env

      let! _ =
        List.traverseResultM
          (fun stmt ->
            result {
              let! _, assump = inferStmt stmt newEnv nonGeneric

              match assump with
              | Some(assump) ->
                let name, binding = assump
                let t = prune (fst binding)

                match t.Kind with
                | TypeKind.Function f ->
                  let t =
                    { t with
                        Kind = generalizeFunc f |> TypeKind.Function }

                  newEnv <- newEnv.AddValue name (t, snd binding)
                | _ -> newEnv <- newEnv.AddValue name (t, snd binding)
              | None -> ()
            })
          stmts

      return newEnv
    }
