namespace Escalier.HindleyMilner

open FsToolkit.ErrorHandling
open System.Collections.Generic

open Syntax
open Type
open Errors

module TypeVariable =
  let mutable nextVariableId = 0

  let makeVariable bound =
    let newVar =
      { id = nextVariableId
        bound = bound
        instance = None }

    nextVariableId <- nextVariableId + 1

    { kind = TypeVar(newVar)
      provenance = None }

module rec TypeChecker =
  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>
  type SchemeAssump = (string * Scheme)

  type Env =
    { values: Map<string, Binding>
      schemes: Map<string, Scheme>
      isAsync: bool }

    member this.addValue (name: string) (binding: Binding) =
      { this with
          values = Map.add name binding this.values }

    member this.addSchem (name: string) (s: Scheme) =
      { this with
          schemes = Map.add name s this.schemes }

  type Assump = string * Binding

  let makeFunctionType typeParams paramList ret =
    { kind =
        Function
          { typeParams = typeParams
            paramList = paramList
            ret = ret }
      provenance = None }

  let numType =
    { kind = TypeRef({ name = "number"; typeArgs = None })
      provenance = None }

  let boolType =
    { kind = TypeRef({ name = "boolean"; typeArgs = None })
      provenance = None }

  let strType =
    { kind = TypeRef({ name = "string"; typeArgs = None })
      provenance = None }

  /// Returns the currently defining instance of t.
  /// As a side effect, collapses the list of type instances. The function Prune
  /// is used whenever a type expression has to be inspected: it will always
  /// return a type expression which is either an uninstantiated type variable or
  /// a type operator; i.e. it will skip instantiated variables, and will
  /// prune them from expressions to remove long chains of instantiated variables.
  let prune (t: Type) : Type =
    match t.kind with
    | TypeVar({ instance = Some(instance) } as v) ->
      let newInstance = prune instance
      v.instance <- Some(newInstance)
      newInstance
    | _ -> t

  let fold_type (f: Type -> option<Type>) (t: Type) : Type =
    let rec fold (t: Type) : Type =
      let t = prune t

      let t =
        match t.kind with
        | TypeVar _ -> t
        | Function f ->
          { kind =
              TypeKind.Function
                { f with
                    paramList =
                      List.map
                        (fun param -> { param with type_ = fold param.type_ })
                        f.paramList
                    ret = fold f.ret }
            provenance = None }
        | Tuple(elems) ->
          let elems = List.map fold elems

          { kind = Tuple(elems)
            provenance = None }
        | TypeRef({ name = name; typeArgs = typeArgs }) ->
          let typeArgs = Option.map (List.map fold) typeArgs

          { kind = TypeRef({ name = name; typeArgs = typeArgs })
            provenance = None }
        | Literal _ -> t
        | Wildcard -> t

      match f t with
      | Some(t) -> t
      | None -> t

    fold t

  let generalize_func (f: Function) : Function =
    let mutable mapping: Map<int, string> = Map.empty
    let mutable nextId = 0

    // QUESTION: should we call `prune` inside the folder as well?
    let folder t =
      match t.kind with
      | TypeVar { id = id } ->
        match Map.tryFind id mapping with
        | Some(name) ->
          Some(
            { kind = TypeRef { name = name; typeArgs = None }
              provenance = None }
          )
        | None ->
          let tpName = 65 + nextId |> char |> string
          nextId <- nextId + 1
          mapping <- mapping |> Map.add id tpName

          Some(
            { kind = TypeRef { name = tpName; typeArgs = None }
              provenance = None }
          )
      | _ -> None

    let paramList =
      List.map
        (fun (p: FuncParam) ->
          { p with
              type_ = fold_type folder p.type_ })
        f.paramList

    // TODO: f.throws
    let ret = fold_type folder f.ret

    let values = mapping.Values |> List.ofSeq

    let mutable newTypeParams: list<TypeParam> =
      List.map
        (fun name ->
          { name = name
            constraint_ = None
            default_ = None })
        values

    Option.iter
      (fun typeParams ->
        for param in typeParams do
          newTypeParams <- newTypeParams @ [ param ])
      f.typeParams

    { f with
        typeParams = if newTypeParams.IsEmpty then None else Some(newTypeParams)
        paramList = paramList
        ret = ret }

  let instantiate_func
    (f: Function)
    (typeArgs: option<list<Type>>)
    : Result<Function, TypeError> =

    result {
      let mutable mapping: Map<string, Type> = Map.empty

      let folder t =
        match t.kind with
        | TypeRef({ name = name }) ->
          match Map.tryFind name mapping with
          | Some(tv) -> Some(tv)
          | None -> None
        | _ -> None

      match f.typeParams with
      | Some(typeParams) ->
        match typeArgs with
        | Some(typeArgs) ->
          if typeArgs.Length <> typeParams.Length then
            return! Error(TypeError.WrongNumberOfTypeArgs)

          for tp, ta in List.zip typeParams typeArgs do
            mapping <- mapping.Add(tp.name, ta)
        | None ->
          for tp in typeParams do
            mapping <-
              mapping.Add(tp.name, TypeVariable.makeVariable tp.constraint_)
      | None -> ()

      return
        { f with
            typeParams = None
            paramList =
              List.map
                (fun param ->
                  { param with
                      type_ = fold_type folder param.type_ })
                f.paramList
            ret = fold_type folder f.ret }
    }

  let occursInType (v: Type) (t2: Type) : bool =
    match (prune t2).kind with
    | pruned when pruned = v.kind -> true
    | TypeRef({ typeArgs = typeArgs }) ->
      match typeArgs with
      | Some(typeArgs) -> occursIn v typeArgs
      | None -> false
    | _ -> false

  let occursIn (t: Type) (types: list<Type>) : bool =
    List.exists (occursInType t) types

  let bind (t1: Type) (t2: Type) =
    result {
      if t1.kind <> t2.kind then
        if occursInType t1 t2 then
          return! Error(TypeError.RecursiveUnification)

        match t1.kind with
        | TypeKind.TypeVar(v) ->
          v.instance <- Some(t2)
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

      match t.kind with
      | TypeVar p ->
        if isGeneric p.id nonGeneric then
          match table.ContainsKey p.id with
          | false ->
            let newVar = TypeVariable.makeVariable None
            table.Add(p.id, newVar)
            newVar
          | true -> table[p.id]
        else
          t
      | Tuple elems ->
        { kind = Tuple(List.map loop elems)
          provenance = None }
      | Function f ->
        makeFunctionType
          f.typeParams
          (List.map
            (fun param -> { param with type_ = loop param.type_ })
            f.paramList)
          (loop f.ret)
      | TypeRef({ typeArgs = typeArgs } as op) ->
        let kind =
          TypeRef(
            { op with
                typeArgs = Option.map (List.map loop) typeArgs }
          )

        { kind = kind; provenance = None }
      | Literal _ -> t
      | _ -> t

    loop t

  ///Get the type of identifier name from the type environment env
  let getType (name: string) (env: Env) (nonGeneric: Set<int>) : Type =
    match env.values |> Map.tryFind name with
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
      match (prune t1).kind, (prune t2).kind with
      | TypeVar _, _ -> do! bind t1 t2
      | _, TypeVar _ -> do! unify t2 t1
      | Tuple(elems1), Tuple(elems2) ->
        if List.length elems1 <> List.length elems2 then
          return! Error(TypeError.TypeMismatch(t1, t2))

        ignore (List.map2 unify elems1 elems2)
      | Function(f1), Function(f2) ->
        // TODO: check if `f1` and `f2` have the same type params
        let! f1 = instantiate_func f1 None
        let! f2 = instantiate_func f2 None

        let paramList1 = List.map (fun param -> param.type_) f1.paramList
        let paramList2 = List.map (fun param -> param.type_) f2.paramList

        if paramList1.Length > paramList2.Length then
          // t1 needs to have at least as many params as t2
          return! Error(TypeError.TypeMismatch(t1, t2))

        for i in 0 .. paramList1.Length - 1 do
          let param1 = paramList1[i]
          let param2 = paramList2[i]
          do! unify param2 param1 // params are contravariant

        do! unify f1.ret f2.ret // returns are covariant
      | TypeRef({ name = name1; typeArgs = types1 }),
        TypeRef({ name = name2; typeArgs = types2 }) ->

        if (name1 <> name2) then
          return! Error(TypeError.TypeMismatch(t1, t2))

        match (types1, types2) with
        | None, None -> ()
        | Some(types1), Some(types2) ->
          if List.length types1 <> List.length types2 then
            return! Error(TypeError.TypeMismatch(t1, t2))

          ignore (List.map2 unify types1 types2)
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | Literal lit, TypeRef({ name = name; typeArgs = typeArgs }) ->
        // TODO: check that `typeArgs` is `None`
        match lit, name with
        | Literal.Number _, "number" -> ()
        | Literal.String _, "string" -> ()
        | Literal.Boolean _, "boolean" -> ()
        | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | _, _ -> return! Error(TypeError.TypeMismatch(t1, t2))
    }

  let unify_call
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

      match callee.kind with
      | TypeKind.Function func ->
        return!
          unify_func_call args typeArgs retType throwsType func env nonGeneric
      | TypeKind.TypeVar _ ->

        // TODO: use a `result {}` CE here
        let! argTypes =
          List.traverseResultM (fun arg -> infer_expr arg env nonGeneric) args

        let paramList =
          List.mapi
            (fun i t ->
              let p: Pattern = Pattern.Identifier $"arg{i}"

              { pattern = p
                type_ = t
                optional = false })
            argTypes

        let callType =
          { Type.kind =
              TypeKind.Function
                { paramList = paramList
                  ret = retType
                  // throws = throwsType
                  typeParams = None } // TODO
            provenance = None }

        match bind callee callType with
        | Ok _ -> return (prune retType, prune throwsType)
        | Error e -> return! Error e
      | kind -> return! Error(TypeError.NotImplemented $"kind = {kind}")
    }

  let unify_func_call
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
          if callee.typeParams.IsSome then
            return! instantiate_func callee typeArgs
          else
            return callee
        }

      let! args =
        List.traverseResultM
          (fun (arg: Expr) ->
            result {
              let! argType = infer_expr arg env nonGeneric
              return arg, argType
            })
          args

      for ((arg, argType), param) in List.zip args callee.paramList do
        if
          param.optional && argType.kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          // TODO: check_mutability of `arg`
          do! unify argType param.type_ // contravariant

      do! unify retType callee.ret // covariant
      // do! unify throwsType callee.throws // TODO

      return (retType, throwsType)
    }

  ///Computes the type of the expression given by node.
  ///The type of the node is computed in the context of the
  ///supplied type environment env. Data types can be introduced into the
  ///language simply by having a predefined set of identifiers in the initial
  ///environment. environment; this way there is no need to change the syntax or, more
  ///importantly, the type-checking program when extending the language.
  let infer_expr
    (expr: Expr)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<Type, TypeError> =
    result {
      match expr.kind with
      | ExprKind.Ident(name) -> return getType name env nonGeneric
      | ExprKind.Literal(literal) ->
        return
          { Type.kind = Literal(literal)
            provenance = None }
      | ExprKind.Call(callee, args) ->
        let! callee = infer_expr callee env nonGeneric

        let! result, throws = unify_call args None callee env nonGeneric

        // TODO: handle throws

        return result
      | ExprKind.Binary(op, left, right) ->
        let funTy = getType op env nonGeneric

        let! result, throws =
          unify_call [ left; right ] None funTy env nonGeneric

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
                let! assumps, patternType = infer_pattern env param.pattern
                do! unify patternType paramType

                for KeyValue(name, binding) in assumps do
                  // TODO: update `Env.types` to store `Binding`s insetad of `Type`s
                  newEnv <- newEnv.addValue name binding

                match paramType.kind with
                | TypeVar { id = id } ->
                  newNonGeneric <- newNonGeneric |> Set.add id
                | _ -> ()

                return
                  { pattern = Pattern.Identifier "arg"
                    type_ = paramType
                    optional = false }
              })
            f.sig'.paramList

        let! stmtTypes =
          List.traverseResultM
            (fun stmt ->
              result {
                let! t, assump = infer_stmt stmt newEnv newNonGeneric

                match assump with
                | Some(name, t) -> newEnv <- newEnv.addValue name t
                | None -> ()

                return t
              })
            f.body.stmts

        let retTy =
          match List.tryLast stmtTypes with
          | Some(t) ->
            match t with
            | Some(t) -> t
            | None -> failwith "Last statement must be an expression"
          | None -> failwith "Empty lambda body"

        return makeFunctionType None paramList retTy // TODO: handle explicit type params
      | ExprKind.Tuple elems ->
        let! elems =
          List.traverseResultM
            (fun elem -> infer_expr elem env nonGeneric)
            elems

        return
          { Type.kind = TypeKind.Tuple(elems)
            provenance = None }
      | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
        let retTy = TypeVariable.makeVariable None

        let! conditionTy = infer_expr condition env nonGeneric
        let! thenBranchTy = infer_expr thenBranch env nonGeneric
        let! elseBranchTy = infer_expr elseBranch env nonGeneric

        do! unify conditionTy boolType
        do! unify thenBranchTy retTy
        do! unify elseBranchTy retTy

        return retTy
      | _ ->
        return!
          Error(TypeError.NotImplemented "TODO: finish implementing infer_expr")
    }

  let infer_stmt
    (stmt: Stmt)
    (env: Env)
    (nonGeneric: Set<int>)
    : Result<option<Type> * option<Assump>, TypeError> =
    result {
      match stmt with
      | Stmt.Expr expr ->
        let! t = infer_expr expr env nonGeneric
        return (Some(t), None)
      | For(pattern, right, block) ->
        return! Error(TypeError.NotImplemented "TODO: infer for")
      | Let(name, definition) ->
        let! defnTy = infer_expr definition env nonGeneric
        let assump = (name, (defnTy, false)) // TODO: isMut
        return (None, Some(assump))
      | LetRec(name, defn) ->
        let newTy = TypeVariable.makeVariable None
        let newEnv = env.addValue name (newTy, false) // TODO: isMut

        let newNonGeneric =
          match newTy.kind with
          | TypeVar { id = id } -> nonGeneric |> Set.add id
          | _ -> nonGeneric

        let! defnTy = infer_expr defn newEnv newNonGeneric
        do! unify newTy defnTy

        let binding = (newTy, false) // TODO: isMut
        return (None, Some(name, binding))
    }

  let infer_pattern
    (env: Env)
    (pat: Syntax.Pattern)
    : Result<BindingAssump * Type, TypeError> =
    let mutable assump = BindingAssump([])

    let rec infer_pattern_rec (pat: Syntax.Pattern) : Type =
      match pat.kind with
      | PatternKind.Identifier({ name = name; isMut = isMut }) ->
        let t = TypeVariable.makeVariable None

        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(name, (t, isMut))
        t
      | PatternKind.Literal(_, literal) ->
        { Type.kind = TypeKind.Literal(literal)
          provenance = None }
      | PatternKind.Object elems -> failwith "todo"
      | PatternKind.Tuple elems ->
        let elems' = List.map infer_pattern_rec elems

        { Type.kind = TypeKind.Tuple(elems')
          provenance = None }
      | PatternKind.Wildcard ->
        { Type.kind = TypeKind.Wildcard
          provenance = None }
      | PatternKind.Is(span, binding, isName, isMut) ->
        match Map.tryFind isName env.schemes with
        | Some(scheme) ->
          assump <- assump.Add(binding.name, (scheme.ty, binding.isMut))
          scheme.ty
        | None -> failwith "todo"

    let t = infer_pattern_rec pat
    Result.Ok((assump, t))


  let infer_script (stmts: list<Stmt>) (env: Env) : Result<Env, TypeError> =
    result {

      let nonGeneric = Set.empty
      let mutable newEnv = env

      let! _ =
        List.traverseResultM
          (fun stmt ->
            result {
              let! _, assump = infer_stmt stmt newEnv nonGeneric

              match assump with
              | Some(assump) ->
                let name, binding = assump
                let t = prune (fst binding)

                match t.kind with
                | TypeKind.Function f ->
                  let t =
                    { t with
                        kind = generalize_func f |> TypeKind.Function }

                  newEnv <- newEnv.addValue name (t, snd binding)
                | _ -> newEnv <- newEnv.addValue name (t, snd binding)
              | None -> ()
            })
          stmts

      return newEnv
    }
