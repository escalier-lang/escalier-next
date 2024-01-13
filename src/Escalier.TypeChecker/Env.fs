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
    let mutable nextTypeVarId = 0
    let mutable nextUniqueId = 0
    let mutable diagnostics: list<Diagnostic> = []

    member this.FreshTypeVar(bound: option<Type>) =
      let newVar =
        { Id = nextTypeVarId
          Bound = bound
          Instance = None }

      nextTypeVarId <- nextTypeVarId + 1

      { Kind = TypeKind.TypeVar newVar
        Provenance = None }

    member this.FreshUniqueId() =
      let id = nextUniqueId
      nextUniqueId <- nextUniqueId + 1
      id

    member this.FreshSymbol() =
      let id = this.FreshUniqueId()

      { Kind = TypeKind.UniqueSymbol id
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

  let makeFunctionType typeParams paramList ret throws =
    { Kind =
        TypeKind.Function
          { TypeParams = typeParams
            ParamList = paramList
            Return = ret
            Throws = throws }
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
        match (prune t).Kind with
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
      { Kind = TypeKind.Keyword Keyword.Never
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
          match (prune t).Kind with
          | TypeKind.Literal(Literal.Number _) -> not hasNum
          | TypeKind.Literal(Literal.String _) -> not hasStr
          | TypeKind.Literal(Literal.Boolean _) -> not hasBool
          | TypeKind.Keyword Keyword.Never -> false
          | _ -> true)
        types

    match types with
    | [] ->
      { Kind = TypeKind.Keyword Keyword.Never
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
      IsAsync: bool
      IsPatternMatching: bool }

    static member empty =
      { Values = Map.empty
        Schemes = Map.empty
        IsAsync = false
        IsPatternMatching = false }

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


  let expandScheme
    (ctx: Ctx)
    (env: Env)
    (unify: Env -> Type -> Type -> Result<unit, TypeError>)
    (scheme: Scheme)
    (mapping: Map<string, Type>)
    (typeArgs: option<list<Type>>)
    : Type =

    // We eagerly expand type args so that any union types can
    // be distributed properly across conditionals so that types
    // like `Exclude<T, U> = T extends U ? never : T` work properly.
    let typeArgs =
      typeArgs
      |> Option.map (fun typeArgs ->
        typeArgs |> List.map (fun t -> expandType ctx env unify mapping t))

    match scheme.TypeParams, typeArgs with
    | None, None -> expandType ctx env unify mapping scheme.Type
    | Some(typeParams), Some(typeArgs) ->
      let mapping = Map.ofList (List.zip typeParams typeArgs)
      expandType ctx env unify mapping scheme.Type
    | _ -> failwith "TODO: expandScheme with type params/args"

  // `mapping` must be distict from `env` because type params that are union
  // types distribute across conditional types.
  let expandType
    (ctx: Ctx)
    (env: Env)
    (unify: Env -> Type -> Type -> Result<unit, TypeError>)
    (mapping: Map<string, Type>) // type param names -> type args
    (t: Type)
    : Type =

    let rec expand mapping t =
      // TODO: only define `fold` when we actually need to use it
      let fold =
        fun t ->
          let result =
            match t.Kind with
            | TypeKind.TypeRef { Name = name } ->
              match Map.tryFind name mapping with
              | Some typeArg -> typeArg
              | None -> t
            | _ -> t

          Some(result)

      match t.Kind with
      | TypeKind.TypeRef { Name = "Promise" } -> printfn $"t = {t}"
      | _ -> ()

      let t = prune t

      match t.Kind with
      | TypeKind.KeyOf t ->
        let t = expandType ctx env unify mapping t

        match t.Kind with
        | TypeKind.Object { Elems = elems } ->
          let keys =
            elems
            |> List.choose (fun elem ->
              // TODO: handle mapped types
              match elem with
              | Property p -> Some(p.Name)
              | _ -> None)

          let keys =
            keys
            |> List.map (fun key ->
              match key with
              | PropName.String s ->
                { Kind = TypeKind.Literal(Literal.String s)
                  Provenance = None }
              | PropName.Number n ->
                { Kind = TypeKind.Literal(Literal.Number n)
                  Provenance = None }
              | PropName.Symbol id ->
                { Kind = TypeKind.UniqueSymbol id
                  Provenance = None })

          union keys
        | _ -> failwith "TODO: expand keyof"
      | TypeKind.Index(target, index) ->
        let target = expandType ctx env unify mapping target
        let index = expandType ctx env unify mapping index

        let key =
          match index.Kind with
          | TypeKind.Literal(Literal.String s) -> PropName.String s
          | TypeKind.Literal(Literal.Number n) -> PropName.Number n
          | TypeKind.UniqueSymbol id -> PropName.Symbol id
          | _ -> failwith "TODO: expand index - key type"

        match target.Kind with
        | TypeKind.Object { Elems = elems } ->
          let mutable t = None

          for elem in elems do
            match elem with
            | Property p when p.Name = key -> t <- Some(p.Type)
            | _ -> ()

          match t with
          | Some t -> t
          | None -> failwithf $"Property {key} not found"
        | _ ->
          // TODO: Handle the case where the type is a primitive and use a
          // special function to expand the type
          // TODO: Handle different kinds of index types, e.g. number, symbol
          failwith "TODO: expand index"
      | TypeKind.Condition { Check = check
                             Extends = extends
                             TrueType = trueType
                             FalseType = falseType } ->

        let infers = findInfers extends
        let mutable newMapping = mapping

        for name in infers do
          let t = ctx.FreshTypeVar None
          newMapping <- Map.add name t newMapping

        let extends = replaceInfers extends newMapping

        match check.Kind with
        | TypeKind.TypeRef { Name = name } ->
          match Map.tryFind name newMapping with
          | Some { Kind = TypeKind.Union types } ->
            let extends = expand newMapping extends

            let types =
              types
              |> List.map (fun check ->
                let newMapping = Map.add name check newMapping

                match unify env check extends with
                | Ok _ -> expand newMapping trueType
                | Error _ -> expand newMapping falseType)

            union types
          | Some check ->
            let newMapping = Map.add name check newMapping

            match unify env check extends with
            | Ok _ -> expand newMapping trueType
            | Error _ -> expand newMapping falseType

          // failwith "TODO: replace check type with the type argument"
          | _ ->
            failwith "TODO: check if the TypeRef's scheme is defined and use it"

            match unify env check extends with
            | Ok _ -> expand newMapping trueType
            | Error _ -> expand newMapping falseType
        | _ ->
          match unify env check extends with
          | Ok _ -> expand newMapping trueType
          | Error _ -> expand newMapping falseType

      | TypeKind.Binary _ -> simplify t
      // TODO: instead of expanding object types, we should try to
      // look up properties on the object type without expanding it
      // since expansion can be quite expensive
      | TypeKind.Object { Elems = elems; Immutable = immutable } ->
        let elems =
          elems
          |> List.collect (fun elem ->
            match elem with
            | Mapped m ->
              let c = expandType ctx env unify mapping m.TypeParam.Constraint

              match c.Kind with
              | TypeKind.Union types ->
                let elems =
                  types
                  |> List.map (fun keyType ->
                    // let key =
                    //   match keyType.Kind with
                    //   | TypeKind.Literal(Literal.String s) ->
                    //     PropKey.String s
                    //   | TypeKind.Literal(Literal.Number n) ->
                    //     PropKey.Number n
                    //   | TypeKind.UniqueSymbol id -> PropKey.Symbol id
                    //   | _ -> failwith "TODO: expand mapped type - key type"

                    match keyType.Kind with
                    | TypeKind.Literal(Literal.String name) ->
                      let typeAnn = m.TypeAnn

                      let folder t =
                        match t.Kind with
                        | TypeKind.TypeRef({ Name = name }) when
                          name = m.TypeParam.Name
                          ->
                          Some(keyType)
                        | _ -> None

                      let typeAnn = foldType folder typeAnn

                      Property
                        { Name = PropName.String name
                          Type = expandType ctx env unify mapping typeAnn
                          Optional = false // TODO
                          Readonly = false // TODO
                        }
                    // TODO: handle other valid key types, e.g. number, symbol
                    | _ -> failwith "TODO: expand mapped type - key type")

                elems
              | TypeKind.Literal(Literal.String key) ->
                let typeAnn = m.TypeAnn

                let folder t =
                  match t.Kind with
                  | TypeKind.TypeRef({ Name = name }) when
                    name = m.TypeParam.Name
                    ->
                    Some(m.TypeParam.Constraint)
                  | _ -> None

                let typeAnn = foldType folder typeAnn

                [ Property
                    { Name = PropName.String key
                      Type = expandType ctx env unify mapping typeAnn
                      Optional = false // TODO
                      Readonly = false // TODO
                    } ]
              | _ -> failwith "TODO: expand mapped type - constraint type"
            | _ -> [ elem ])

        let t =
          { Kind = TypeKind.Object { Elems = elems; Immutable = immutable }
            Provenance = None // TODO: set provenance
          }

        // Replaces type parameters with their corresponding type arguments
        // TODO: do this more consistently
        if mapping = Map.empty then t else foldType fold t
      | TypeKind.TypeRef { Name = name
                           TypeArgs = typeArgs
                           Scheme = scheme } ->


        // TODO: Take this a setep further and update ExpandType and ExpandScheme
        // to be functions that accept an `env: Env` param.  We can then augment
        // the `env` instead of using the `mapping` param.
        let t =
          match Map.tryFind name mapping with
          | Some t -> t
          | None ->
            match scheme with
            | Some scheme -> expandScheme ctx env unify scheme mapping typeArgs
            | None ->
              match env.Schemes.TryFind name with
              | Some scheme ->
                expandScheme ctx env unify scheme mapping typeArgs
              | None -> failwith $"{name} is not in scope"

        expand mapping t
      | _ ->
        // Replaces type parameters with their corresponding type arguments
        // TODO: do this more consistently
        if mapping = Map.empty then t else foldType fold t

    expand mapping t

  // TODO: dedupe with findInfers in Infer.fs
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

  let replaceInfers (t: Type) (mapping: Map<string, Type>) : Type =
    let fold =
      fun t ->
        let result =
          match t.Kind with
          | TypeKind.Infer name ->
            match Map.tryFind name mapping with
            | Some t -> t
            | None -> t
          | _ -> t

        Some(result)

    foldType fold t

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
