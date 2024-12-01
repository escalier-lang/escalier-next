namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.TypeChecker.Error

open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.Data

open Prune

module rec Env =
  type DeclIdent =
    | Value of string
    | Type of string

  type DeclGraph<'T> =
    { Edges: Map<DeclIdent, list<DeclIdent>>
      Nodes: Map<DeclIdent, list<'T>>
      Namespaces: Map<string, DeclGraph<'T>> }

    member this.Add(name: DeclIdent, decl: 'T, deps: list<DeclIdent>) =
      let decls =
        match this.Nodes.TryFind name with
        | Some nodes -> nodes @ [ decl ]
        | None -> [ decl ]

      { this with
          Edges = this.Edges.Add(name, deps)
          Nodes = this.Nodes.Add(name, decls) }

    member this.AddNamespace(name: string, graph: DeclGraph<'T>) =
      { this with
          Namespaces = this.Namespaces.Add(name, graph) }

    static member Empty: DeclGraph<'T> =
      { Edges = Map.empty
        Nodes = Map.empty
        Namespaces = Map.empty }

  type Ctx
    (
      getExports: Ctx -> string -> Syntax.Import -> Namespace,
      resolvePath: Ctx -> string -> Syntax.Import -> string,
      inferExpr:
        Ctx -> Env -> option<Type> -> Syntax.Expr -> Result<Type, TypeError>,
      inferModuleItems:
        Ctx -> Env -> bool -> list<Syntax.ModuleItem> -> Result<Env, TypeError>,
      inferPattern:
        Ctx
          -> Env
          -> Syntax.Pattern
          -> Result<(BindingAssump * Type), TypeError>,
      inferTypeAnn: Ctx -> Env -> Syntax.TypeAnn -> Result<Type, TypeError>,
      inferClass:
        Ctx -> Env -> Syntax.Class -> bool -> Result<(Type * Scheme), TypeError>
    ) =

    let mutable nextTypeVarId = 0
    let mutable nextUniqueId = 0
    let mutable parentReports: list<Report> = []
    let mutable currentReport: Report = { Diagnostics = [] }

    member val NextTypeVarId = nextTypeVarId with get, set
    member val NextUniqueId = nextUniqueId with get, set

    member val InferExpr = inferExpr
    member val InferModuleItems = inferModuleItems
    member val InferPattern = inferPattern
    member val InferTypeAnn = inferTypeAnn
    member val InferClass = inferClass

    member this.FreshTypeVar (bound: option<Type>) (def: option<Type>) =
      let newVar =
        { Id = nextTypeVarId
          Bound = bound
          Default = def
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

    member this.Report = currentReport

    member this.PushReport() =
      parentReports <- currentReport :: parentReports
      currentReport <- { Diagnostics = [] }

    member this.PopReport() =
      match parentReports with
      | parent :: ancestors ->
        currentReport <- parent
        parentReports <- ancestors
      | [] -> ()

    member this.MergeUpReport() =
      match parentReports with
      | parent :: ancestors ->
        currentReport.Diagnostics <-
          parent.Diagnostics @ currentReport.Diagnostics

        parentReports <- ancestors
      | [] -> ()

    member this.GetExports = getExports this
    member this.ResolvePath = resolvePath this

    member this.Clone =
      let clone =
        Ctx(
          getExports,
          resolvePath,
          inferExpr,
          inferModuleItems,
          inferPattern,
          inferTypeAnn,
          inferClass
        )

      clone.NextTypeVarId <- this.NextTypeVarId
      clone.NextUniqueId <- this.NextUniqueId
      clone

  let makeTypeRefKind name =
    { Name = name
      TypeArgs = None
      Scheme = None }
    |> TypeKind.TypeRef

  let makeFunction typeParams self paramList ret throws =
    { TypeParams = typeParams
      Self = self
      ParamList = paramList
      Return = ret
      Throws = throws }

  let makeFunctionType typeParams paramList ret throws =
    { Kind =
        TypeKind.Function(makeFunction typeParams None paramList ret throws)
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

  type BindingAssump = Map<string, Binding>
  type SchemeAssump = string * Scheme

  type Namespace with

    member this.AddNamespace (name: string) (ns: Namespace) =
      { this with
          Namespaces = Map.add name ns this.Namespaces }

    member this.AddScheme (name: string) (scheme: Scheme) =
      { this with
          Schemes = Map.add name scheme this.Schemes }

    member this.GetScheme(name: string) : Result<Scheme, TypeError> =
      match this.Schemes |> Map.tryFind name with
      | Some(s) -> Ok(s)
      | None ->
        Error(
          TypeError.SemanticError
            $"Namespace.GetScheme - Undefined symbol {name}"
        )

    member this.AddBinding (name: string) (binding: Binding) =
      { this with
          Values = Map.add name binding this.Values }

    member this.GetBinding(name: string) : Result<Binding, TypeError> =
      match this.Values |> Map.tryFind name with
      | Some(var) -> Ok var
      | None ->
        Error(
          TypeError.SemanticError
            $"Namespace.GetBinding - Undefined symbol {name}"
        )

    member this.GetNamspace
      (ident: QualifiedIdent)
      : Result<Namespace, TypeError> =
      result {
        match ident with
        | Ident name ->
          match this.Namespaces |> Map.tryFind name with
          | Some(ns) -> return ns
          | None ->
            return! Error(TypeError.SemanticError $"Undefined namespace {name}")
        | Member(qualifier, name) ->
          let! ns = this.GetNamspace qualifier

          match ns.Namespaces |> Map.tryFind name with
          | Some(ns) -> return ns
          | None ->
            return! Error(TypeError.SemanticError $"Undefined namespace {name}")
      }

    member this.GetQualifiedScheme
      (ident: QualifiedIdent)
      : Result<Scheme, TypeError> =

      printfn "ident = %A" ident

      result {
        match ident with
        | QualifiedIdent.Ident name -> return! this.GetScheme name
        | QualifiedIdent.Member(qualifier, name) ->
          let! ns = this.GetNamspace qualifier
          return! ns.GetScheme name
      }

    member this.GetQualifiedBinding
      (ident: QualifiedIdent)
      : Result<Binding, TypeError> =

      result {
        match ident with
        | QualifiedIdent.Ident name -> return! this.GetBinding name
        | QualifiedIdent.Member(qualifier, name) ->
          let! ns = this.GetNamspace qualifier
          return! ns.GetBinding name
      }

    member this.Merge(other: Namespace) =
      { this with
          Values = FSharpPlus.Map.union other.Values this.Values
          // TODO: call `merge` on each namespace?
          Namespaces = FSharpPlus.Map.union other.Namespaces this.Namespaces
          Schemes = FSharpPlus.Map.union other.Schemes this.Schemes }

  type Env =
    { Filename: string
      Namespace: Namespace
      BinaryOps: Map<string, Binding>
      UnaryOps: Map<string, Binding>
      IsAsync: bool
      IsPatternMatching: bool }

    static member empty(filename: string) =
      { Filename = filename
        Namespace = Namespace.empty
        BinaryOps = Map.empty
        UnaryOps = Map.empty
        IsAsync = false
        IsPatternMatching = false }

    // TODO: Rename to AddBinding
    // TODO: don't curry this function
    member this.AddValue (name: string) (binding: Binding) =
      { this with
          Namespace = this.Namespace.AddBinding name binding }

    member this.AddBindings(bindings: Map<string, Binding>) =
      let mutable newEnv = this

      for KeyValue(name, binding) in bindings do
        let binding =
          { binding with
              Type = prune binding.Type }

        newEnv <- newEnv.AddValue name binding

      newEnv

    // TODO: don't curry this function
    member this.AddScheme (name: string) (s: Scheme) =
      { this with
          Namespace = this.Namespace.AddScheme name s }

    member this.AddSchemes(schemes: Map<string, Scheme>) =
      let mutable newEnv = this

      for KeyValue(name, scheme) in schemes do
        newEnv <- newEnv.AddScheme name scheme

      newEnv

    member this.AddNamespace (name: string) (ns: Namespace) =
      { this with
          Namespace = this.Namespace.AddNamespace name ns }

    member this.GetBinaryOp(name: string) : Result<Type, TypeError> =
      match this.BinaryOps |> Map.tryFind name with
      | Some(var) -> Ok(var.Type)
      | None ->
        Error(TypeError.SemanticError $"Undefined binary operator {name}")

    member this.GetUnaryOp(name: string) : Result<Type, TypeError> =
      match this.UnaryOps |> Map.tryFind name with
      | Some(var) -> Ok(var.Type)
      | None ->
        Error(TypeError.SemanticError $"Undefined unary operator {name}")

    // Get the type of identifier name from the type environment env
    member this.GetValue(name: string) : Result<Type, TypeError> =
      match this.TryFindValue name with
      | Some(var) ->
        // TODO: check `isMut` and return an immutable type if necessary
        Ok(var.Type)
      | None ->
        // TODO: why do we need to check if it's an integer literal?
        if isIntegerLiteral name then
          Ok(numType)
        else
          Error(
            TypeError.SemanticError $"Env.GetValue - Undefined symbol {name}"
          )

    member this.GetScheme(ident: QualifiedIdent) : Result<Scheme, TypeError> =
      result {
        match ident with
        | Ident name ->
          match this.TryFindScheme name with
          | Some(s) -> return s
          | None ->
            return!
              Error(
                TypeError.SemanticError
                  $"Env.GetScheme - Undefined symbol {ident}"
              )
        | Member(qualifier, ident) ->
          let! ns = this.GetNamspace qualifier
          return! ns.GetScheme ident
      }

    member this.GetBinding(name: string) : Result<Binding, TypeError> =
      match this.Namespace.Values |> Map.tryFind name with
      | Some(var) -> Ok var
      | None ->
        Error(
          TypeError.SemanticError $"Env.GetBinding - Undefined symbol {name}"
        )

    member this.GetNamspace
      (ident: QualifiedIdent)
      : Result<Namespace, TypeError> =
      result {
        match ident with
        | Ident name ->
          match this.Namespace.Namespaces |> Map.tryFind name with
          | Some(ns) -> return ns
          | None ->
            return! Error(TypeError.SemanticError $"Undefined namespace {name}")
        | Member(qualifier, name) ->
          let! ns = this.GetNamspace qualifier

          match ns.Namespaces |> Map.tryFind name with
          | Some(ns) -> return ns
          | None ->
            return! Error(TypeError.SemanticError $"Undefined namespace {name}")
      }

    member this.GetQualifiedBinding
      (ident: QualifiedIdent)
      : Result<Binding, TypeError> =

      result {
        match ident with
        | QualifiedIdent.Ident name -> return! this.GetBinding name
        | QualifiedIdent.Member(qualifier, name) ->
          let! ns = this.GetNamspace qualifier
          return! ns.GetBinding name
      }

    member this.GetNamespace
      (expr: Syntax.Expr)
      : Result<option<Namespace * option<Syntax.Expr>>, TypeError> =

      result {
        match expr.Kind with
        | Syntax.ExprKind.Identifier ident ->
          match this.Namespace.Namespaces.TryFind ident.Name with
          | None -> return None
          | Some ns -> return Some(ns, None)
        | _ -> return None
      }

    member this.FindValue(name: string) : Binding =
      Map.find name this.Namespace.Values

    member this.TryFindValue(name: string) : option<Binding> =
      Map.tryFind name this.Namespace.Values

    member this.FindScheme(name: string) : Scheme =
      Map.find name this.Namespace.Schemes

    member this.TryFindScheme(name: string) : option<Scheme> =
      Map.tryFind name this.Namespace.Schemes
