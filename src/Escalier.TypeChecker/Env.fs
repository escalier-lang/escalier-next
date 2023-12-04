namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data.Common
open Escalier.Data.Type

open TypeVariable
open Error

module rec Env =

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
        TypeKind.Function
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

  let isIntegerLiteral (name: string) =
    match
      (try
        Some(int name)
       with _ex ->
         None)
    with
    | None -> false
    | Some _ -> true


  let rec flatten (types: list<Type>) : list<Type> =
    List.collect
      (fun t ->
        match t.Kind with
        | TypeKind.Union ts -> flatten ts
        | _ -> [ t ])
      types

  let intersection (types: list<Type>) : Type =
    let types = flatten types

    // Removes duplicates
    let types =
      types |> List.map prune |> Seq.distinctBy (fun t -> t.Kind) |> Seq.toList

    match types with
    | [] ->
      { Kind = makePrimitiveKind "never"
        Provenance = None }
    | [ t ] -> t
    | types ->
      { Kind = TypeKind.Intersection(types)
        Provenance = None }

  let union (types: list<Type>) : Type =
    let types = flatten types

    // Removes duplicates
    let types =
      types |> List.map prune |> Seq.distinctBy (fun t -> t.Kind) |> Seq.toList

    let mutable hasNum = false
    let mutable hasStr = false
    let mutable hasBool = false

    for t in types do
      match (prune t).Kind with
      | TypeKind.TypeRef { Name = "number" } -> hasNum <- true
      | TypeKind.TypeRef { Name = "string" } -> hasStr <- true
      | TypeKind.TypeRef { Name = "boolean" } -> hasBool <- true
      | _ -> ()

    // Removes literals if the corresponding primitive is already in
    // the union, e.g. 1 | number -> number
    let types =
      List.filter
        (fun t ->
          match t.Kind with
          | TypeKind.Literal(Literal.Number _) -> not hasNum
          | TypeKind.Literal(Literal.String _) -> not hasStr
          | TypeKind.Literal(Literal.Boolean _) -> not hasBool
          | _ -> true)
        types

    match types with
    | [] ->
      { Kind = makePrimitiveKind "never"
        Provenance = None }
    | [ t ] -> t
    | types ->
      { Kind = TypeKind.Union(types)
        Provenance = None }


  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>
  type SchemeAssump = string * Scheme


  type Env =
    { Values: Map<string, Binding>
      Schemes: Map<string, Scheme>
      IsAsync: bool }

    // TODO: Rename to AddBinding
    member this.AddValue (name: string) (binding: Binding) =
      { this with
          Values = Map.add name binding this.Values }

    member this.AddScheme (name: string) (s: Scheme) =
      { this with
          Schemes = Map.add name s this.Schemes }

    // TODO: Rename to `GetBinding`
    // Get the type of identifier name from the type environment env
    member this.GetType(name: string) : Result<Type, TypeError> =
      match this.Values |> Map.tryFind name with
      | Some(var) ->
        // TODO: check `isMut` and return an immutable type if necessary
        let (t, isMut) = var
        Ok(t)
      | None ->
        if isIntegerLiteral name then
          Ok(numType)
        else
          Error(TypeError.SemanticError $"Undefined symbol {name}")

    member this.ExpandScheme
      (scheme: Scheme)
      (typeArgs: option<list<Type>>)
      : Type =

      match scheme.TypeParams, typeArgs with
      | None, None -> this.ExpandType scheme.Type
      | _ -> failwith "TODO: expandScheme with type params/args"

    member this.ExpandType(t: Type) : Type =
      let rec expand t =
        let t = prune t

        match t.Kind with
        | TypeKind.KeyOf t -> failwith "TODO: expand keyof"
        | TypeKind.Index(target, index) -> failwith "TODO: expand index"
        | TypeKind.Condition _ -> failwith "TODO: expand condition"
        | TypeKind.Binary _ -> simplify t
        // TODO: instead of expanding object types, we should try to
        // look up properties on the object type without expanding it
        // since expansion can be quite expensive
        | TypeKind.Object _ -> t
        | TypeKind.TypeRef { Name = name
                             TypeArgs = typeArgs
                             Scheme = scheme } ->
          // TODO: do a better job of handling this
          if name = "string" || name = "number" || name = "boolean" then
            t
          else
            let t =
              match scheme with
              | Some scheme -> this.ExpandScheme scheme typeArgs
              | None ->
                match this.Schemes.TryFind name with
                | Some scheme -> this.ExpandScheme scheme typeArgs
                | None -> failwith $"{name} is not in scope"

            expand t
        | _ -> t

      expand t

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

  let makeVariable bound =
    let newVar =
      { Id = TypeVariable.nextVariableId
        Bound = bound
        Instance = None }

    nextVariableId <- nextVariableId + 1

    { Kind = TypeKind.TypeVar(newVar)
      Provenance = None }

  let simplify (t: Type) : Type =
    match t.Kind with
    | TypeKind.Binary(left, op, right) ->
      match (prune left).Kind, (prune right).Kind with
      | TypeKind.Literal(Literal.Number n1), TypeKind.Literal(Literal.Number n2) ->
        let n1 = float n1
        let n2 = float n2

        let result =
          match op with
          | "+" -> n1 + n2
          | "-" -> n1 - n2
          | "*" -> n1 * n2
          | "/" -> n1 / n2
          | "%" -> n1 % n2
          | "**" -> n1 ** n2
          | _ -> failwith "TODO: simplify binary"

        { Kind = TypeKind.Literal(Literal.Number(string result))
          Provenance = None }
      // TODO: Check `op` when collapsing binary expressions involving numbers
      | _, TypeKind.TypeRef { Name = "number" } -> right
      | TypeKind.TypeRef { Name = "number" }, _ -> left
      | TypeKind.Literal(Literal.String s1), TypeKind.Literal(Literal.String s2) ->

        let result =
          match op with
          | "++" -> s1 + s2
          | _ -> failwith "TODO: simplify binary"

        { Kind = TypeKind.Literal(Literal.Number(string result))
          Provenance = None }
      // TODO: Check `op` when collapsing binary expressions involving strings
      | _, TypeKind.TypeRef { Name = "string" } -> right
      | TypeKind.TypeRef { Name = "string" }, _ -> left
      | _ -> t
    | _ -> t

  /// Returns the currently defining instance of t.
  /// As a side effect, collapses the list of type instances. The function Prune
  /// is used whenever a type expression has to be inspected: it will always
  /// return a type expression which is either an uninstantiated type variable or
  /// a type operator; i.e. it will skip instantiated variables, and will
  /// prune them from expressions to remove long chains of instantiated variables.
  let rec prune (t: Type) : Type =
    match t.Kind with
    | TypeKind.TypeVar({ Instance = Some(instance) } as v) ->
      let newInstance = prune instance
      v.Instance <- Some(newInstance)
      newInstance
    | _ -> simplify t

  let rec bind
    (env: Env)
    (unify: Env -> Type -> Type -> Result<unit, TypeError>)
    (t1: Type)
    (t2: Type)
    =
    let t1 = prune t1
    let t2 = prune t2

    result {
      if t1.Kind <> t2.Kind then
        if occursInType t1 t2 then
          return! Error(TypeError.RecursiveUnification)

        match t1.Kind with
        | TypeKind.TypeVar(v) ->
          // printfn "Binding %A to %A" t1 t2
          match v.Bound with
          | Some(bound) -> do! unify env t2 bound
          | None -> ()
          // return! Error(TypeError.TypeBoundMismatch(t1, t2))
          v.Instance <- Some(t2)
          return ()
        | _ -> return! Error(TypeError.NotImplemented "bind error")
    }

  and occursInType (v: Type) (t2: Type) : bool =
    match (prune t2).Kind with
    | pruned when pruned = v.Kind -> true
    | TypeKind.TypeRef({ TypeArgs = typeArgs }) ->
      match typeArgs with
      | Some(typeArgs) -> occursIn v typeArgs
      | None -> false
    | _ -> false

  and occursIn (t: Type) (types: list<Type>) : bool =
    List.exists (occursInType t) types
