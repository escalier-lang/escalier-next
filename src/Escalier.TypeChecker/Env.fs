namespace Escalier.TypeChecker

open Escalier.Data.Common
open Escalier.Data.Type

open TypeVariable
open Error

module Env =

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

  let union (types: list<Type>) : Type =

    let types = flatten types

    // Removes duplicates
    let types =
      types |> List.map prune |> Seq.distinctBy (fun t -> t.Kind) |> Seq.toList

    let mutable hasNum = false

    for t in types do
      match (prune t).Kind with
      | TypeKind.TypeRef { Name = "number" } -> hasNum <- true
      | _ -> ()

    // Removes literals if the corresponding primitive is already in
    // the union, e.g. 1 | number -> number
    let types =
      List.filter
        (fun t ->
          match t.Kind with
          | TypeKind.Literal(Literal.Number _) -> not hasNum
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
        | TypeKind.Binary(left, op, right) -> failwith "TODO: expand binary"
        // TODO: instead of expanding object types, we should try to
        // look up properties on the object type without expanding it
        // since expansion can be quite expensive
        | TypeKind.Object _ -> t
        | TypeKind.TypeRef { Name = name
                             TypeArgs = typeArgs
                             Scheme = scheme } ->
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
