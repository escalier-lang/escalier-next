namespace Escalier.TypeChecker

open Escalier.TypeChecker.Error

open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.Data

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
        Mutable = false
        Provenance = None }

    member this.FreshUniqueId() =
      let id = nextUniqueId
      nextUniqueId <- nextUniqueId + 1
      id

    member this.FreshSymbol() =
      let id = this.FreshUniqueId()

      { Kind = TypeKind.UniqueSymbol id
        Mutable = false
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
      Mutable = false
      Provenance = None }

  let numType =
    { Kind = TypeKind.Primitive Primitive.Number
      Mutable = false
      Provenance = None }

  let boolType =
    { Kind = TypeKind.Primitive Primitive.Boolean
      Mutable = false
      Provenance = None }

  let strType =
    { Kind = TypeKind.Primitive Primitive.String
      Mutable = false
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
        Mutable = false
        Provenance = None }
    | [ t ] -> t
    | types ->
      { Kind = TypeKind.Intersection(types)
        Mutable = false
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
        Mutable = false
        Provenance = None }
    | [ t ] -> t
    | types ->
      { Kind = TypeKind.Union(types)
        Mutable = false
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
        // TODO: why do we need to check if it's an integer literal?
        if isIntegerLiteral name then
          Ok(numType)
        else
          Error(TypeError.SemanticError $"Undefined symbol {name}")

    member this.GetBinding(name: string) : Result<Type * bool, TypeError> =
      match this.Values |> Map.tryFind name with
      | Some(var) -> Ok var
      | None -> Error(TypeError.SemanticError $"Undefined symbol {name}")
