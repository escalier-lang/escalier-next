namespace Escalier.TypeChecker

open Escalier.TypeChecker.Error
open FsToolkit.ErrorHandling

open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.Data

open Folder
open Prune

module rec Env =
  type Ctx
    (
      getExports: Ctx -> string -> Syntax.Import -> Env,
      resolvePath: Ctx -> string -> Syntax.Import -> string
    ) =
    // let baseDir = baseDir
    // let filesystem = filesystem
    let mutable nextVariableId = 0
    let mutable diagnostics: list<Diagnostic> = []

    member this.FreshTypeVar(bound: option<Type>) =
      let newVar =
        { Id = nextVariableId
          Bound = bound
          Instance = None }

      nextVariableId <- nextVariableId + 1

      { Kind = TypeKind.TypeVar newVar
        Provenance = None }

    member this.AddDiagnostic(diagnostic: Diagnostic) =
      diagnostics <- diagnostic :: diagnostics

    member this.Diagnostics = diagnostics
    member this.GetExports = getExports this
    member this.ResolvePath = resolvePath this

  let makeTypeRefKind name =
    { Name = name
      TypeArgs = None
      Scheme = None }
    |> TypeKind.TypeRef

  let makeFunctionType typeParams paramList ret =
    let never =
      { Kind = TypeKind.Keyword Keyword.Never
        Provenance = None }

    { Kind =
        TypeKind.Function
          { TypeParams = typeParams
            ParamList = paramList
            Return = ret
            Throws = never }
      Provenance = None }

  let numType =
    { Kind = TypeKind.Primitive Primitive.Number
      Provenance = None }

  let boolType =
    { Kind = TypeKind.Primitive Primitive.Boolean
      Provenance = None }

  let strType =
    { Kind = TypeKind.Primitive Primitive.String
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

  let rec cartesian (lstlst: list<list<Type>>) =
    match lstlst with
    | [] -> [ [] ]
    | lst :: lstlst ->
      let rest = cartesian lstlst
      List.collect (fun x -> List.map (fun y -> x :: y) rest) lst

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
      { Kind = makeTypeRefKind "never"
        Provenance = None }
    | [ t ] -> t
    | types ->
      { Kind = TypeKind.Intersection(types)
        Provenance = None }

  let union (types: list<Type>) : Type =
    let types = flatten types

    let types =
      types
      |> List.map prune
      // Removes duplicates
      |> List.distinctBy (fun t -> t.Kind)
      // Removes 'never's
      |> List.filter (fun t -> t.Kind <> TypeKind.Keyword Keyword.Never)

    let mutable hasNum = false
    let mutable hasStr = false
    let mutable hasBool = false

    for t in types do
      match (prune t).Kind with
      | TypeKind.Primitive Primitive.Number -> hasNum <- true
      | TypeKind.Primitive Primitive.String -> hasStr <- true
      | TypeKind.Primitive Primitive.Boolean -> hasBool <- true
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
      { Kind = makeTypeRefKind "never"
        Provenance = None }
    | [ t ] -> t
    | types ->
      { Kind = TypeKind.Union(types)
        Provenance = None }

  let instantiateScheme (scheme: Scheme) (mapping: Map<string, Type>) =
    let fold =
      fun t ->
        let result =
          match t.Kind with
          | TypeKind.TypeRef { Name = name
                               TypeArgs = typeArgs
                               Scheme = scheme } ->
            match Map.tryFind name mapping with
            | Some typeArg -> typeArg
            | None -> t
          | _ -> t

        Some(result)

    foldType fold scheme.Type


  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>
  type SchemeAssump = string * Scheme


  type Env =
    { Values: Map<string, Binding>
      Schemes: Map<string, Scheme>
      IsAsync: bool }

    static member empty =
      { Values = Map.empty
        Schemes = Map.empty
        IsAsync = false }

    // TODO: Rename to AddBinding
    // TODO: don't curry this function
    member this.AddValue (name: string) (binding: Binding) =
      { this with
          Values = Map.add name binding this.Values }

    // TODO: don't curry this function
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
      (unify: Env -> Type -> Type -> Result<unit, TypeError>)
      (scheme: Scheme)
      (typeArgs: option<list<Type>>)
      : Type =

      match scheme.TypeParams, typeArgs with
      | None, None -> this.ExpandType unify scheme.Type
      | Some(typeParams), Some(typeArgs) ->
        let mapping = Map.ofList (List.zip typeParams typeArgs)

        let mutable checkTypes: List<Type> = []

        let findCond =
          fun t ->
            match t.Kind with
            | TypeKind.Condition(check, _, _, _) ->
              checkTypes <- check :: checkTypes
            | _ -> ()

        TypeVisitor.walkType findCond scheme.Type

        printfn "checkTypes = %A" checkTypes

        // TODO: filter checkTypes to only include union types
        let checkTypeNames =
          checkTypes
          |> List.choose (fun t ->
            match t.Kind with
            | TypeKind.TypeRef { Name = name } ->
              match Map.containsKey name mapping with
              | true -> Some(name)
              | false -> None
            | _ -> None)

        if checkTypeNames.IsEmpty then
          instantiateScheme scheme mapping
        else
          // TODO: extract all of the check types that are union types
          // TODO: create a cartesian product of all of the union types

          let name = checkTypeNames[0]
          let unionType = Map.find name mapping

          match unionType.Kind with
          | TypeKind.Union types ->
            types
            |> List.map (fun t ->
              let mapping = Map.add name t mapping
              let t = instantiateScheme scheme mapping
              this.ExpandType unify t)
            |> union
          | _ -> instantiateScheme scheme mapping
      | _ -> failwith "TODO: expandScheme with type params/args"

    member this.ExpandType
      (unify: Env -> Type -> Type -> Result<unit, TypeError>)
      (t: Type)
      : Type =
      let rec expand t =
        let t = prune t

        match t.Kind with
        | TypeKind.KeyOf t -> failwith "TODO: expand keyof"
        | TypeKind.Index(target, index) -> failwith "TODO: expand index"
        | TypeKind.Condition(check, extends, trueType, falseType) ->
          match check.Kind with
          | TypeKind.Union types -> failwith "TODO: distribute conditional"
          | _ ->
            match unify this check extends with
            | Ok _ -> expand trueType
            | Error _ -> expand falseType
        | TypeKind.Binary _ -> simplify t
        // TODO: instead of expanding object types, we should try to
        // look up properties on the object type without expanding it
        // since expansion can be quite expensive
        | TypeKind.Object _ -> t
        | TypeKind.TypeRef { Name = name
                             TypeArgs = typeArgs
                             Scheme = scheme } ->
          let t =
            match scheme with
            | Some scheme -> this.ExpandScheme unify scheme typeArgs
            | None ->
              match this.Schemes.TryFind name with
              | Some scheme -> this.ExpandScheme unify scheme typeArgs
              | None -> failwith $"{name} is not in scope"

          expand t
        | _ -> t

      expand t

    member this.GetPropType
      (unify: Env -> Type -> Type -> Result<unit, TypeError>)
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
          this.GetPropType
            unify
            env
            (env.ExpandScheme unify scheme typeArgs)
            name
            optChain
        | None ->
          match env.Schemes.TryFind typeRefName with
          | Some scheme ->
            this.GetPropType
              unify
              env
              (env.ExpandScheme unify scheme typeArgs)
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
            let t = this.GetPropType unify env t name optChain

            let undefined =
              { Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }

            union [ t; undefined ]
          | _ -> failwith "TODO: lookup member on union type"

      // TODO: intersection types
      // TODO: union types
      | _ -> failwith $"TODO: lookup member on type - {t}"

  let rec bind
    (ctx: Ctx)
    (env: Env)
    (unify: Ctx -> Env -> Type -> Type -> Result<unit, TypeError>)
    (t1: Type)
    (t2: Type)
    : Result<unit, TypeError> =
    let t1 = prune t1
    let t2 = prune t2

    result {
      if t1.Kind <> t2.Kind then
        if occursInType t1 t2 then
          return! Error(TypeError.RecursiveUnification(t1, t2))

        match t1.Kind with
        | TypeKind.TypeVar(v) ->
          match v.Bound with
          | Some(bound) ->
            // Type params are contravariant for similar reasons to
            // why function params are contravariant
            do! unify ctx env t2 bound

            match t2.Kind with
            | TypeKind.Keyword Keyword.Never -> v.Instance <- Some(bound)
            | _ -> v.Instance <- Some(t2)
          | None -> v.Instance <- Some(t2)

          return ()
        | _ -> return! Error(TypeError.NotImplemented "bind error")
    }

  // TODO: finish implementing this function
  and occursInType (v: Type) (t2: Type) : bool =
    match (prune t2).Kind with
    | pruned when pruned = v.Kind -> true
    | TypeKind.TypeRef({ TypeArgs = typeArgs }) ->
      match typeArgs with
      | Some(typeArgs) -> occursIn v typeArgs
      | None -> false
    | TypeKind.Binary(left, op, right) ->
      occursInType v left || occursInType v right
    | _ -> false

  and occursIn (t: Type) (types: list<Type>) : bool =
    List.exists (occursInType t) types
