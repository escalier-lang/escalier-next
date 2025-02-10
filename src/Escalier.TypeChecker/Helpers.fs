module rec Escalier.TypeChecker.Helpers

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Syntax
open Escalier.Data.Common
open Escalier.Data.Type
open Escalier.Data.Visitor

open Error
open Env
open Poly
open Folder

type ValueCategory =
  | LValue
  | RValue

// TODO: update this function to use the type visitor/folder
let rec generalizeType (t: Type) : Type =
  match (prune t).Kind with
  | TypeKind.TypeVar { Instance = None; Default = Some d } -> d
  | TypeKind.TypeVar { Instance = None; Bound = Some b } -> b
  | TypeKind.TypeVar { Instance = None
                       Bound = None
                       Default = None } ->
    { Kind = TypeKind.Keyword Keyword.Unknown
      Provenance = None }
  | TypeKind.Function f ->
    { t with
        Kind = generalizeFunc f |> TypeKind.Function }
  | TypeKind.Object({ Elems = elems } as objectKind) ->
    let elems =
      elems
      |> List.map (fun elem ->
        match elem with
        | ObjTypeElem.Callable fn -> ObjTypeElem.Callable(generalizeFunc fn)
        | ObjTypeElem.Constructor fn ->
          ObjTypeElem.Constructor(generalizeFunc fn)
        | ObjTypeElem.Method { Name = name; Fn = fn } ->
          ObjTypeElem.Method
            { Name = name
              Fn = (generalizeFunc fn) }
        | ObjTypeElem.Getter { Name = name; Fn = fn } ->
          ObjTypeElem.Getter
            { Name = name
              Fn = (generalizeFunc fn) }
        | ObjTypeElem.Setter { Name = name; Fn = fn } ->
          ObjTypeElem.Setter
            { Name = name
              Fn = (generalizeFunc fn) }
        | ObjTypeElem.Property { Name = name
                                 Optional = optional
                                 Readonly = readonly
                                 Type = t } ->
          ObjTypeElem.Property
            { Name = name
              Optional = optional
              Readonly = readonly
              Type = generalizeType t }
        | _ -> elem)

    { t with
        Kind = TypeKind.Object { objectKind with Elems = elems } }
  | TypeKind.Tuple({ Elems = elems } as tupleKind) ->
    let elems = elems |> List.map generalizeType

    { t with
        Kind = TypeKind.Tuple { tupleKind with Elems = elems } }
  | TypeKind.Intersection types ->
    let types = types |> List.map generalizeType

    { t with
        Kind = TypeKind.Intersection types }
  | TypeKind.Union types ->
    let types = types |> List.map generalizeType
    { t with Kind = TypeKind.Union types }
  | _ -> t

let generalizeBindings (bindings: Map<string, Binding>) : Map<string, Binding> =
  let mutable newBindings = Map.empty

  for KeyValue(name, binding) in bindings do
    let binding =
      { binding with
          Type = generalizeType binding.Type }

    newBindings <- newBindings.Add(name, binding)

  newBindings

let findBindingNames (p: Syntax.Pattern) : Set<string> =
  let mutable names: list<string> = []

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | ExprKind.Function _ -> (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitJsxElement = fun (_, state) -> (false, state)
      ExprVisitor.VisitJsxFragment = fun (_, state) -> (false, state)
      ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (false, state)
      ExprVisitor.VisitPattern =
        fun (pat, state) ->
          match pat.Kind with
          | PatternKind.Ident { Name = name } ->
            names <- name :: names
            (false, state)
          | PatternKind.Object { Elems = elems } ->
            for elem in elems do
              match elem with
              | Syntax.ShorthandPat { Name = name } -> names <- name :: names
              | _ -> ()

            (true, state)
          | _ -> (true, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  ExprVisitor.walkPattern visitor () p

  Set.ofList names

let findReturns (body: BlockOrExpr) : list<Expr> =
  let mutable returns: list<Expr> = []

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | ExprKind.Function _ -> (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitJsxElement = fun (_, state) -> (false, state)
      ExprVisitor.VisitJsxFragment = fun (_, state) -> (false, state)
      ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
      ExprVisitor.VisitStmt =
        fun (stmt, state) ->
          match stmt.Kind with
          | StmtKind.Return expr ->
            match expr with
            | Some expr -> returns <- expr :: returns
            | None -> ()
          | _ -> ()

          (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  match body with
  | BlockOrExpr.Block block ->
    List.iter (ExprVisitor.walkStmt visitor ()) block.Stmts
  | BlockOrExpr.Expr expr ->
    ExprVisitor.walkExpr visitor () expr // There might be early returns in match expression
    returns <- expr :: returns // We treat the expression as a return in this case

  returns

let maybeWrapInPromise (t: Type) (e: Type) : Type =
  match t.Kind with
  | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Promise" } -> t
  | _ ->
    { Kind =
        TypeKind.TypeRef
          { Name = QualifiedIdent.Ident "Promise"
            TypeArgs = Some([ t; e ])
            Mutable = false
            Scheme = None }
      Provenance = None }

// TODO: dedupe with findThrowsInBlock
let findThrows (body: BlockOrExpr) : list<Type> =
  let mutable throws: list<Type> = []

  let visitor: ExprVisitor.SyntaxVisitor<unit> =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | ExprKind.Function _ -> (false, state)
          | ExprKind.Try { Catch = catch
                           Throws = uncaughtThrows } ->
            match uncaughtThrows with
            | Some(uncaughtThrows) -> throws <- uncaughtThrows :: throws
            | None -> ()
            // If there is a catch clause, don't visit the children
            // TODO: we still need to visit the catch clause in that
            // cacse because there may be re-throws inside of it
            (catch.IsNone, state)
          | ExprKind.Throw expr ->
            match expr.InferredType with
            | Some t -> throws <- t :: throws
            | None -> failwith "Expected `expr` to have an `InferredType`"

            (true, state) // there might be other `throw` expressions inside
          | ExprKind.Call call ->
            match call.Throws with
            | Some t -> throws <- t :: throws
            | None -> ()

            (true, state) // there might be other `throw` expressions inside
          | ExprKind.Await await ->
            match await.Throws with
            | Some t -> throws <- t :: throws
            | None -> ()

            (true, state) // there might be other `throw` expressions inside
          | _ -> (true, state)
      ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  match body with
  | BlockOrExpr.Block block ->
    List.iter (ExprVisitor.walkStmt visitor ()) block.Stmts
  | BlockOrExpr.Expr expr -> ExprVisitor.walkExpr visitor () expr

  throws

// TODO: dedupe with findThrows
let findThrowsInBlock (block: Block) : list<Type> =
  let mutable throws: list<Type> = []

  let visitor: ExprVisitor.SyntaxVisitor<unit> =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | ExprKind.Function _ -> (false, state)
          | ExprKind.Try { Catch = catch
                           Throws = uncaughtThrows } ->
            match uncaughtThrows with
            | Some(uncaughtThrows) -> throws <- uncaughtThrows :: throws
            | None -> ()
            // If there is a catch clause, don't visit the children
            // TODO: we still need to visit the catch clause in that
            // cacse because there may be re-throws inside of it
            (catch.IsNone, state)
          | ExprKind.Throw expr ->
            match expr.InferredType with
            | Some t -> throws <- t :: throws
            | None -> failwith "Expected `expr` to have an `InferredType`"

            (true, state) // there might be other `throw` expressions inside
          | ExprKind.Call call ->
            match call.Throws with
            | Some t -> throws <- t :: throws
            | None -> ()

            (true, state) // there might be other `throw` expressions inside
          | ExprKind.Await await ->
            match await.Throws with
            | Some t -> throws <- t :: throws
            | None -> ()

            (true, state) // there might be other `throw` expressions inside
          | _ -> (true, state)
      ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  List.iter (ExprVisitor.walkStmt visitor ()) block.Stmts

  throws

// TODO: dedupe with findInfers in Env.fs
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

let hasTypeVars (t: Type) : bool =
  let mutable hasTypeVars = false

  let visitor =
    fun (t: Type) ->
      match t.Kind with
      | TypeKind.TypeVar _ -> hasTypeVars <- true
      | _ -> ()

  TypeVisitor.walkType visitor (prune t)

  hasTypeVars

let fresh (ctx: Ctx) (t: Type) : Type =

  let folder: Type -> option<Type> =
    fun t ->
      match t.Kind with
      | TypeKind.TypeVar _ -> Some(ctx.FreshTypeVar None None)
      | _ -> None

  Folder.foldType folder t

let rec getIsMut (ctx: Ctx) (env: Env) (expr: Expr) : Result<bool, TypeError> =
  result {
    match expr.Kind with
    | ExprKind.Identifier { Name = name } ->
      let! binding = env.GetBinding name
      return binding.Mutable
    | ExprKind.Literal _ -> return false
    | ExprKind.Object _ -> return true
    | ExprKind.Tuple _ -> return true
    | ExprKind.Index { Target = target } -> return! getIsMut ctx env target
    | ExprKind.Member { Target = target } -> return! getIsMut ctx env target
    | _ ->
      return!
        Error(TypeError.NotImplemented $"determine the mutability of {expr}")
  }

// Return a structure which indicates whether the object represented by the property
// map is open or closed.  A type like `{foo: string}` is closed but something like
// `{foo: string} & T` is open as long as `T` is open.
let rec getPropertyMap (t: Type) : Result<Map<PropName, Type>, TypeError> =
  result {
    let mutable map = Map.empty

    match (prune t).Kind with
    | TypeKind.Object { Elems = elems } ->
      for elem in elems do
        match elem with
        | Callable ``function`` ->
          return!
            Error(
              TypeError.SemanticError
                "Callable signatures cannot appear in object literals"
            )
        | Constructor ``function`` ->
          return!
            Error(
              TypeError.SemanticError
                "Constructor signatures cannot appear in object literals"
            )
        | Method { Name = name; Fn = fn } ->
          let t =
            { Kind = TypeKind.Function fn
              Provenance = None }

          map <- Map.add name t map
        | Getter { Name = name; Fn = fn } ->
          failwith "TODO: getPropertyMap - ObjElemType.Getter"
        | Setter { Name = name; Fn = fn } ->
          failwith "TODO: getPropertyMap - ObjElemType.Setter"
        // We'll need a different solution for looking up properties in mapped
        // types because their key could have a type like `string` or `number`
        // which is unbounded.
        | Mapped mapped -> failwith "TODO: getPropertyMap - ObjElemType.Mapped"
        | RestSpread t -> failwith "TODO: getPropertyMap - ObjElemType.Rest"
        | Property { Name = name; Type = t } -> map <- Map.add name t map
    | TypeKind.Intersection types ->
      for t in types do
        let! subMap = getPropertyMap t
        map <- FSharpPlus.Map.union subMap map
    | _ -> ()

    return map
  }

let rec patternToPattern (pat: Syntax.Pattern) : Pattern =
  match pat.Kind with
  | PatternKind.Ident { Name = name; IsMut = mut } ->
    Pattern.Identifier { Name = name; IsMut = mut }
  // | PatternKind.Is(span, bindingIdent, isName, isMut) ->
  //   Pattern.Is(bindingIdent, isName)
  | PatternKind.Object { Elems = elems; Immutable = immutable } ->
    Pattern.Object
      { Elems =
          List.map
            (fun (elem: Syntax.ObjPatElem) ->
              match elem with
              | Syntax.ObjPatElem.KeyValuePat { Key = key
                                                Value = value
                                                Default = init } ->
                ObjPatElem.KeyValuePat
                  { Key = key
                    Value = patternToPattern value
                    Init = init }
              | Syntax.ObjPatElem.ShorthandPat { Name = name
                                                 Default = init
                                                 IsMut = mut
                                                 Assertion = _ } ->
                ObjPatElem.ShorthandPat
                  { Name = name
                    Init = init
                    IsMut = mut }
              | Syntax.ObjPatElem.RestPat { Target = target; IsMut = _ } ->
                // TODO: isMut
                ObjPatElem.RestPat(patternToPattern target))
            elems
        Immutable = immutable }
  | PatternKind.Tuple { Elems = elems; Immutable = immutable } ->
    Pattern.Tuple
      { Elems = List.map (patternToPattern >> Some) elems
        Immutable = immutable }
  | PatternKind.Wildcard { Assertion = _ } -> Pattern.Wildcard
  | PatternKind.Literal lit -> Pattern.Literal lit
  | PatternKind.Rest rest -> Pattern.Rest(patternToPattern rest)
  | PatternKind.Extractor _ ->
    failwith "TODO: patternToPattern - PatternKind.Extractor"

let qualifyTypeRefs
  (t: Type)
  (nsName: string)
  (nsScheme: Map<string, Scheme>)
  : Type =

  let f =
    fun (t: Type) ->
      match t.Kind with
      | TypeKind.TypeRef { Name = QualifiedIdent.Ident name
                           Scheme = scheme
                           Mutable = mut
                           TypeArgs = typeArgs } ->
        match nsScheme.TryFind name with
        | Some _ ->
          let name = QualifiedIdent.Member(QualifiedIdent.Ident nsName, name)

          let kind =
            TypeKind.TypeRef
              { Name = name
                TypeArgs = typeArgs
                Mutable = mut
                Scheme = scheme }

          Some { t with Kind = kind }
        | None -> Some t
      | _ -> Some t

  Folder.foldType f t

let inferMemberAccess
  // TODO: do the search first and then return the appropriate ObjTypeElem
  (ctx: Ctx)
  (key: PropName)
  (valueCategory: ValueCategory)
  (elems: list<ObjTypeElem>)
  : option<Type> =

  // TODO: instead of using tryFind, use a for-loop with an early return
  // we can use this to provide better error messages when trying to assign
  // something that has a getter by no setter and similar situations.
  let elem =
    List.tryFind
      (fun (elem: ObjTypeElem) ->
        match elem with
        | Property { Name = name } -> name = key
        | Method { Name = name } ->
          name = key && valueCategory = ValueCategory.RValue
        | Getter { Name = name } ->
          name = key && valueCategory = ValueCategory.RValue
        | Setter { Name = name } ->
          name = key && valueCategory = ValueCategory.LValue
        | Mapped _ ->
          failwith
            "TODO: inferMemberAccess - mapped (check if key is subtype of Mapped.TypeParam)"
        | _ -> false)
      elems

  match elem with
  | Some elem ->
    match elem with
    | Property p ->
      match p.Optional with
      | true ->
        let undefined =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Provenance = None }

        Some(union [ p.Type; undefined ])
      | false -> Some p.Type
    | Method { Fn = fn } ->
      // TODO: replace `Self` with the object type
      // TODO: check if the receiver is mutable or not
      let t =
        { Kind = TypeKind.Function fn
          Provenance = None }

      Some t
    | Getter { Fn = fn } -> Some fn.Return // TODO: handle throws
    | Setter { Fn = fn } -> Some fn.ParamList[0].Type // TODO: handle throws
    | Mapped _mapped -> failwith "TODO: inferMemberAccess - mapped"
    | RestSpread _ -> failwith "TODO: inferMemberAccess - rest"
    | Callable _ -> failwith "Callable signatures don't have a name"
    | Constructor _ -> failwith "Constructor signatures don't have a name"
  | None -> None

let getPropType
  (ctx: Ctx)
  (env: Env)
  (t: Type)
  (key: PropName)
  (optChain: bool)
  (valueCategory: ValueCategory)
  : Result<Type, TypeError> =
  result {
    let t = prune t

    match t.Kind with
    | TypeKind.Object { Elems = elems } ->
      match inferMemberAccess ctx key valueCategory elems with
      | Some t -> return t
      | None ->
        return! Error(TypeError.SemanticError $"Property {key} not found")
    | TypeKind.Namespace { Name = nsName
                           Values = values
                           Schemes = schemes
                           Namespaces = namespaces } ->
      match key with
      | PropName.String s ->
        match values.TryFind s with
        | None ->
          match namespaces.TryFind s with
          | None ->
            return! Error(TypeError.SemanticError $"Property {key} not found")
          | Some ns ->
            return
              { Kind = TypeKind.Namespace ns
                Provenance = None }
        // TODO: handle nested namespaces by adding a optional reference
        // to the parent namespace that we can follow
        | Some(binding) -> return qualifyTypeRefs binding.Type nsName schemes
      | PropName.Number _ ->
        return!
          Error(
            TypeError.SemanticError
              "Can't use a number as a key with a namespace"
          )
      | PropName.Symbol _ ->
        return!
          Error(
            TypeError.SemanticError
              "Can't use a symbol as a key with a namespace"
          )
    | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                         TypeArgs = Some [ arrayElem ] } ->
      // TODO: update `TypeKind.Array` to also contain a `Length` type param
      // TODO: use getPropertyType to look up the type of the `.length` property
      // here (and when handling tuples) instead of fabricating it here.
      // TODO: make type param mutable so that we can update it if it's a mutable
      // array and the array size changes.
      match key with
      | PropName.Number _ ->
        let unknown =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Provenance = None }

        return union [ arrayElem; unknown ]
      | _ ->
        // TODO: update Interop.Infer to combine Array and ReadonlyArray into
        // a single Array type where methods are marked appropriately with
        // `self` and `mut self`.
        let arrayScheme =
          match env.TryFindScheme "Array" with
          | Some scheme -> scheme
          | None -> failwith "Array not in scope"

        // Instead of expanding the whole scheme which could be quite expensive
        // we get the property from the type and then only instantiate it.
        let t = arrayScheme.Type
        let! prop = getPropType ctx env t key optChain valueCategory

        let! prop =
          instantiateType ctx prop arrayScheme.TypeParams (Some [ arrayElem ])

        return prop
    | TypeKind.TypeRef { Name = typeRefName
                         Scheme = scheme
                         TypeArgs = typeArgs } ->
      match scheme with
      | Some scheme ->
        let! objType = expandScheme ctx env None scheme Map.empty typeArgs
        return! getPropType ctx env objType key optChain valueCategory
      | None ->
        let! scheme = env.GetScheme typeRefName
        let! objType = expandScheme ctx env None scheme Map.empty typeArgs
        return! getPropType ctx env objType key optChain valueCategory
    | TypeKind.Union types ->
      let undefinedTypes, definedTypes =
        List.partition
          (fun t -> t.Kind = TypeKind.Literal(Literal.Undefined))
          types

      if undefinedTypes.IsEmpty then
        return!
          Error(TypeError.NotImplemented "TODO: lookup member on union type")
      else if not optChain then
        return!
          Error(TypeError.SemanticError "Can't lookup property on undefined")
      else
        match definedTypes with
        | [ t ] ->
          let! t = getPropType ctx env t key optChain valueCategory

          let undefined =
            { Kind = TypeKind.Literal(Literal.Undefined)
              Provenance = None }

          return union [ t; undefined ]
        | _ ->
          return!
            Error(TypeError.NotImplemented "TODO: lookup member on union type")
    | TypeKind.Tuple { Elems = elems } ->
      match key with
      | PropName.String "length" ->
        return
          { Kind = TypeKind.Literal(Literal.Number(Number.Int elems.Length))
            Provenance = None }
      | PropName.Number number ->
        match number with
        | Float _ ->
          return!
            Error(TypeError.SemanticError "numeric indexes can't be floats")
        | Int i ->
          if i >= 0 && i < elems.Length then
            return elems[int i]
          else
            // TODO: report a diagnost about the index being out of range
            return
              { Kind = TypeKind.Literal(Literal.Undefined)
                Provenance = None }
      | _ ->
        let _arrayScheme =
          match env.TryFindScheme "Array" with
          | Some scheme -> scheme
          | None -> failwith "Array not in scope"
        // TODO: lookup keys in array prototype
        return!
          Error(TypeError.NotImplemented "TODO: lookup member on tuple type")
    | TypeKind.Literal(Literal.String _)
    | TypeKind.Primitive Primitive.String ->
      let scheme =
        match env.TryFindScheme "String" with
        | Some scheme -> scheme
        | None -> failwith "String not in scope"

      return! getPropType ctx env scheme.Type key optChain valueCategory
    | TypeKind.Literal(Literal.Number _)
    | TypeKind.Primitive Primitive.Number ->
      let scheme =
        match env.TryFindScheme "Number" with
        | Some scheme -> scheme
        | None -> failwith "Number not in scope"

      return! getPropType ctx env scheme.Type key optChain valueCategory
    | TypeKind.Literal(Literal.Boolean _)
    | TypeKind.Primitive Primitive.Boolean ->
      let scheme =
        match env.TryFindScheme "Boolean" with
        | Some scheme -> scheme
        | None -> failwith "Boolean not in scope"

      return! getPropType ctx env scheme.Type key optChain valueCategory
    | TypeKind.Primitive Primitive.Symbol
    | TypeKind.UniqueSymbol _ ->
      let scheme =
        match env.TryFindScheme "Symbol" with
        | Some scheme -> scheme
        | None -> failwith "Symbol not in scope"

      return! getPropType ctx env scheme.Type key optChain valueCategory
    | _ ->
      // TODO: intersection types
      return!
        Error(
          TypeError.NotImplemented $"TODO: lookup member '{key}' on type - {t}"
        )
  }

let expandScheme
  (ctx: Ctx)
  (env: Env)
  (ips: option<list<list<string>>>)
  (scheme: Scheme)
  (mapping: Map<string, Type>)
  (typeArgs: option<list<Type>>)
  : Result<Type, TypeError> =

  result {
    match scheme.TypeParams, typeArgs with
    | None, None -> return! expandType ctx env ips mapping scheme.Type
    | Some(typeParams), Some(typeArgs) ->
      let mutable newEnv = env
      let mutable typeArgs = typeArgs

      // Fill in any missing type args with defaults from the type params
      if typeParams.Length > typeArgs.Length then
        let defaults =
          typeParams
          |> List.skip typeArgs.Length
          |> List.choose (fun tp ->
            match tp.Default with
            | Some t -> Some(t)
            | None -> None)

        if defaults.Length = (typeParams.Length - typeArgs.Length) then
          typeArgs <- typeArgs @ defaults

      let! mapping = buildTypeArgMapping ctx typeParams (Some typeArgs)

      for KeyValue(name, t) in mapping do
        newEnv <- newEnv.AddScheme name { Type = t; TypeParams = None }

      return! expandType ctx newEnv ips mapping scheme.Type
    | Some(typeParams), None ->
      let mutable newEnv = env

      let defaults =
        typeParams
        |> List.choose (fun tp ->
          match tp.Default with
          | Some t -> Some(t)
          | None -> None)

      if defaults.Length = typeParams.Length then
        let! mapping = buildTypeArgMapping ctx typeParams (Some defaults)

        for KeyValue(name, t) in mapping do
          newEnv <- newEnv.AddScheme name { Type = t; TypeParams = None }

        return! expandType ctx newEnv ips mapping scheme.Type
      else
        return!
          Error(
            TypeError.NotImplemented "TODO: expandScheme with type params/args"
          )
    | None, Some(typeArgs) ->
      return!
        Error(
          TypeError.SemanticError
            "Scheme has no type params but type args were provided"
        )
  }

// `mapping` must be distict from `env` because type params that are union
// types distribute across conditional types.
let expandType
  (ctx: Ctx)
  (env: Env)
  (ips: option<list<list<string>>>)
  (mapping: Map<string, Type>) // type param names -> type args
  (t: Type)
  : Result<Type, TypeError> =

  let rec expand
    (mapping: Map<string, Type>)
    (t: Type)
    : Result<Type, TypeError> =

    // TODO: only define `fold` when we actually need to use it
    let fold =
      fun t ->
        let result =
          match t.Kind with
          | TypeKind.TypeRef { Name = QualifiedIdent.Ident name } ->
            match Map.tryFind name mapping with
            | Some typeArg -> typeArg
            | None -> t
          | _ -> t

        Some(result)

    result {

      match t.Kind with
      | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Promise" } ->
        printfn $"t = {t}"
      | _ -> ()

      let t = prune t

      match t.Kind with
      | TypeKind.KeyOf t ->
        let! t = expandType ctx env ips mapping t

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

          return union keys
        | TypeKind.Tuple { Elems = elems } ->
          let keys =
            elems
            |> List.mapi (fun i _ ->
              { Kind = TypeKind.Literal(Literal.Number(Number.Int i))
                Provenance = None })

          return union keys
        | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                             TypeArgs = Some [ _ ] } ->
          return
            { Kind = TypeKind.Primitive Primitive.Number
              Provenance = None }
        | _ ->
          printfn "t = %A" t
          return! Error(TypeError.NotImplemented $"TODO: expand keyof {t}")
      | TypeKind.Index { Target = target; Index = index } ->
        let! target = expandType ctx env ips mapping target
        let! index = expandType ctx env ips mapping index

        match index.Kind with
        | TypeKind.Keyword Keyword.Never -> return index
        | _ ->
          // TODO: dedupe this with the getPropType and inferMemberAccess in Infer.fs
          match target.Kind with
          | TypeKind.Object { Elems = elems } ->
            let key =
              match index.Kind with
              | TypeKind.Literal(Literal.String s) -> PropName.String s
              | TypeKind.Literal(Literal.Number n) -> PropName.Number n
              | TypeKind.UniqueSymbol id -> PropName.Symbol id
              | TypeKind.Primitive Primitive.String ->
                failwith "TODO: expand string index"
              | TypeKind.Primitive Primitive.Number ->
                failwith "TODO: expand number index"
              | _ -> failwith $"Invalid index type {index} for Object"

            let mutable t = None

            for elem in elems do
              match elem with
              | Property p when p.Name = key -> t <- Some(p.Type)
              | Method { Name = name; Fn = fn } when name = key ->
                // TODO: replace `Self` with the object type
                // TODO: check if the receiver is mutable or not
                t <-
                  Some(
                    { Kind = TypeKind.Function fn
                      Provenance = None }
                  )
              | _ -> ()

            match t with
            | Some t -> return! expand mapping t
            | None ->
              printfn $"target = {target}"

              return! Error(TypeError.SemanticError $"Property {key} not found")
          | TypeKind.Tuple { Elems = elems } ->
            match index.Kind with
            | TypeKind.Literal(Literal.Number(Number.Int n)) ->
              if n >= 0 && n < elems.Length then
                return! expand mapping elems[n]
              else
                return!
                  Error(TypeError.SemanticError $"Index {n} out of bounds")
            | _ ->
              return!
                Error(
                  TypeError.NotImplemented $"Invalid index for extanding {t}"
                )
          | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                               TypeArgs = Some [ arrayElem ] } ->
            match index.Kind with
            | TypeKind.Primitive Primitive.Number ->
              return! expand mapping arrayElem
            | _ ->
              return!
                Error(
                  TypeError.NotImplemented $"Invalid index for extanding {t}"
                )
          | _ ->
            // TODO: Handle the case where the type is a primitive and use a
            // special function to expand the type
            // TODO: Handle different kinds of index types, e.g. number, symbol
            return!
              Error(
                TypeError.NotImplemented
                  $"TODO: expand index - target type = {target}"
              )
      | TypeKind.Condition { Check = check
                             Extends = extends
                             TrueType = trueType
                             FalseType = falseType } ->

        let infers = findInfers extends
        let mutable newMapping = mapping

        for name in infers do
          let t = ctx.FreshTypeVar None None
          newMapping <- Map.add name t newMapping

        let extends = replaceInfers extends newMapping

        match check.Kind with
        | TypeKind.TypeRef { Name = QualifiedIdent.Ident name } ->
          let! check = expand newMapping check

          match check.Kind with
          | TypeKind.Union types ->
            let! extends = expand newMapping extends

            let! types =
              types
              |> List.traverseResultM (fun check ->
                let newMapping = Map.add name check newMapping

                match ctx.Unify ctx env ips check extends with
                | Ok _ -> expand newMapping trueType
                | Error _ -> expand newMapping falseType)

            return union types
          | _ ->
            match ctx.Unify ctx env ips check extends with
            | Ok _ -> return! expand newMapping trueType
            | Error _ -> return! expand newMapping falseType
        | _ ->
          match ctx.Unify ctx env ips check extends with
          | Ok _ -> return! expand newMapping trueType
          | Error _ -> return! expand newMapping falseType

      // TODO: instead of expanding object types, we should try to
      // look up properties on the object type without expanding it
      // since expansion can be quite expensive
      | TypeKind.Object { Elems = elems
                          Exact = exact
                          Extends = extends
                          Immutable = immutable } ->

        let rec processExtends
          (extends: option<list<TypeRef>>)
          : Result<list<ObjTypeElem>, TypeError> =

          result {
            match extends with
            | Some typeRefs ->
              let mutable allElems = []

              for typeRef in typeRefs do
                let { Name = typeRefName
                      Scheme = scheme
                      TypeArgs = typeArgs } =
                  typeRef

                let! objType =
                  result {
                    match scheme with
                    | Some scheme ->
                      return! expandScheme ctx env None scheme mapping typeArgs
                    | None ->
                      let! scheme = env.GetScheme typeRefName

                      return! expandScheme ctx env None scheme mapping typeArgs
                  }

                match objType.Kind with
                | TypeKind.Object { Elems = elems; Extends = extends } ->
                  allElems <- allElems @ elems
                  let! moreElems = processExtends extends
                  allElems <- allElems @ moreElems
                | _ -> failwith $"expected ${objType} to be an object type"

              return allElems
            | None -> return []
          }

        let! elemsFromExtends = processExtends extends
        let allElems = elems @ elemsFromExtends
        let mutable exact = exact

        let! elems =
          allElems
          |> List.traverseResultM (fun elem ->
            result {
              match elem with
              | Mapped m ->
                match m.TypeParam.Constraint.Kind with
                | TypeKind.KeyOf t ->
                  match t.Kind with
                  | TypeKind.TypeRef { Name = QualifiedIdent.Ident ident } ->
                    match Map.tryFind ident mapping with
                    | Some t ->
                      let! t = expandType ctx env ips Map.empty t

                      match t.Kind with
                      | TypeKind.Object { Exact = e } -> exact <- e
                      | _ -> ()
                    | None -> ()
                  | _ -> ()
                | _ -> ()

                let! c = expandType ctx env ips mapping m.TypeParam.Constraint

                // TODO: Document this because I don't remember why we need to
                // do this.
                match c.Kind with
                | TypeKind.Union types ->
                  let mutable elems = []

                  for keyType in types do
                    let typeAnn = m.TypeAnn

                    let folder t =
                      match t.Kind with
                      | TypeKind.TypeRef({ Name = QualifiedIdent.Ident name }) when
                        name = m.TypeParam.Name
                        ->
                        Some(keyType)
                      | _ -> None

                    let typeAnn = foldType folder typeAnn
                    let! t = expandType ctx env ips mapping typeAnn

                    let optional =
                      match m.Optional with
                      | None -> false // TODO: copy value from typeAnn if it's an index access type
                      | Some MappedModifier.Add -> true
                      | Some MappedModifier.Remove -> false

                    let readonly =
                      match m.Readonly with
                      | None -> false // TODO: copy value from typeAnn if it's an index access type
                      | Some MappedModifier.Add -> true
                      | Some MappedModifier.Remove -> false

                    let mutable nameMapping: Map<string, Type> = Map.empty

                    nameMapping <- Map.add m.TypeParam.Name keyType nameMapping

                    let! keyType =
                      match m.NameType with
                      | Some nameType ->
                        expandType ctx env ips nameMapping nameType
                      | None -> Result.Ok keyType

                    let propName =
                      match keyType.Kind with
                      | TypeKind.Literal(Literal.String name) ->
                        Some(PropName.String name)
                      | TypeKind.Literal(Literal.Number name) ->
                        Some(PropName.Number name)
                      | TypeKind.UniqueSymbol id -> Some(PropName.Symbol id)
                      | _ -> None

                    match propName with
                    | Some propName ->
                      elems <-
                        elems
                        @ [ Property
                              { Name = propName
                                Type = t
                                Optional = optional
                                Readonly = readonly } ]
                    | _ -> () // Omits entries with other key types

                  return elems
                | TypeKind.Literal(Literal.String key) ->
                  let typeAnn = m.TypeAnn

                  let folder t =
                    match t.Kind with
                    | TypeKind.TypeRef({ Name = QualifiedIdent.Ident name }) when
                      name = m.TypeParam.Name
                      ->
                      Some(m.TypeParam.Constraint)
                    | _ -> None

                  let typeAnn = foldType folder typeAnn
                  let! t = expandType ctx env ips mapping typeAnn

                  return
                    [ Property
                        { Name = PropName.String key
                          Type = t
                          Optional = false // TODO
                          Readonly = false // TODO
                        } ]
                | TypeKind.Primitive Primitive.Number ->
                  let typeAnn = m.TypeAnn

                  let folder t =
                    match t.Kind with
                    | TypeKind.TypeRef({ Name = QualifiedIdent.Ident name }) when
                      name = m.TypeParam.Name
                      ->
                      Some(m.TypeParam.Constraint)
                    | _ -> None

                  let typeAnn = foldType folder typeAnn
                  let! t = expandType ctx env ips mapping typeAnn

                  let c =
                    { Kind = TypeKind.Primitive Primitive.Number
                      Provenance = None }

                  let typeParam =
                    { Name = m.TypeParam.Name
                      Constraint = c }

                  return
                    [ Mapped
                        { m with
                            TypeAnn = t
                            TypeParam = typeParam } ]
                | _ -> return [ elem ]
              | RestSpread t ->
                let! t = expand mapping t

                match (prune t).Kind with
                | TypeKind.Object { Elems = elems } ->
                  return
                    elems
                    |> List.filter (fun elem ->
                      match elem with
                      | Callable _ -> false
                      | Constructor _ -> false
                      | _ -> true)
                | _ ->
                  return!
                    Error(
                      TypeError.SemanticError "Can't spread non-object type"
                    )
              | _ -> return [ elem ]
            })

        let elems = List.collect id elems

        let callableElems, namedElems =
          elems
          |> List.partition (fun elem ->
            match elem with
            | Callable _ -> true
            | Constructor _ -> true
            | _ -> false)

        // TODO: build this map while iterating over allElems
        let mutable namedElemsMap = Map.empty
        let mutable mappedElems = []

        for elem in namedElems do
          match elem with
          | Property p ->
            // match Map.tryFind p.Name namedElemsMap with
            // | Some otherElem -> printfn $"duplicate elems: {p.Name}"
            // | None -> ()
            //
            // namedElemsMap <- Map.add p.Name elem namedElemsMap
            match Map.tryFind p.Name namedElemsMap with
            | Some otherElem ->
              if p.Optional then
                let otherType =
                  match otherElem with
                  | Property { Type = t } -> Some t
                  | Method { Fn = fn } ->
                    Some
                      { Kind = TypeKind.Function fn
                        Provenance = None }
                  | Getter { Fn = fn } -> Some fn.Return
                  | Setter { Fn = fn } -> None // can't spread what can't be read
                  | _ -> None

                match otherType with
                | None -> ()
                | Some t ->
                  let newP =
                    { Name = p.Name
                      Optional = false // TODO
                      Readonly = false // TODO
                      Type = union [ t; p.Type ] }

                  namedElemsMap <- Map.add p.Name (Property newP) namedElemsMap
              else
                namedElemsMap <- Map.add p.Name elem namedElemsMap
            | None -> namedElemsMap <- Map.add p.Name elem namedElemsMap
          | Getter { Name = name } ->
            namedElemsMap <- Map.add name elem namedElemsMap
          | Setter _ -> () // can't spread what can't be read
          | Method { Name = name } ->
            namedElemsMap <- Map.add name elem namedElemsMap
          | Mapped m -> mappedElems <- elem :: mappedElems
          | _ -> ()

        let elems =
          callableElems @ (Map.values namedElemsMap |> List.ofSeq) @ mappedElems

        let t =
          { Kind =
              TypeKind.Object
                { Extends = None // because we've expanded the `extends` above
                  Implements = None // TODO
                  Elems = elems
                  Exact = exact
                  Immutable = immutable
                  Mutable = false // TODO
                  Interface = false }
            Provenance = None // TODO: set provenance
          }

        // Replaces type parameters with their corresponding type arguments
        // TODO: do this more consistently
        if mapping = Map.empty then
          return t
        else
          return foldType fold t

      | TypeKind.TypeRef { Name = QualifiedIdent.Ident "Array"
                           TypeArgs = Some [ arrayElem ]
                           Scheme = scheme } ->
        let! arrayElem = expandType ctx env ips mapping arrayElem

        return
          { Kind =
              TypeKind.TypeRef
                { Name = QualifiedIdent.Ident "Array"
                  TypeArgs = Some [ arrayElem ]
                  Mutable = false // TODO
                  Scheme = scheme }
            Provenance = None }
      | TypeKind.Tuple({ Elems = elems } as tuple) ->
        let! elems = elems |> List.traverseResultM (expand mapping)

        return
          { Kind = TypeKind.Tuple { tuple with Elems = elems }
            Provenance = None }
      | TypeKind.TypeRef { Name = name
                           TypeArgs = typeArgs
                           Scheme = scheme } ->

        // Replaces type refs appearing in type args with their definitions
        // from `mapping`.
        let typeArgs =
          match typeArgs with
          | Some typeArgs -> typeArgs |> List.map (foldType fold) |> Some
          | None -> None

        // TODO: Take this a setep further and update ExpandType and ExpandScheme
        // to be functions that accept an `env: Env` param.  We can then augment
        // the `env` instead of using the `mapping` param.
        // TODO: check if `name` is a qualified ident first
        // only unqualified idents can appear in the mapping
        let! t =
          match name with
          | Ident name ->
            match Map.tryFind name mapping with
            | Some t -> Result.Ok t
            | None ->
              match scheme with
              | Some scheme -> expandScheme ctx env ips scheme mapping typeArgs
              | None ->
                match env.TryFindScheme name with
                | Some scheme ->
                  expandScheme ctx env ips scheme mapping typeArgs
                | None -> failwith $"{name} is not in scope"
          | Member _ ->
            match scheme with
            | Some scheme -> expandScheme ctx env ips scheme mapping typeArgs
            | None ->
              match env.GetScheme name with
              | Ok scheme -> expandScheme ctx env ips scheme mapping typeArgs
              | Error errorValue -> failwith $"{name} is not in scope"

        match t.Kind with
        | TypeKind.Intrinsic ->
          // If the TypeRef is an intrinsic type, we expand it to an intrinsic
          // instance which includes the name and type args from the TypeRef.
          // These are used by `unify` to determine if a given string literal
          // conforms to the given intrinsic.
          return
            { Kind =
                TypeKind.IntrinsicInstance { Name = name; TypeArgs = typeArgs }
              Provenance = None }
        | _ -> return! expand mapping t
      | TypeKind.Intersection types ->
        let! types = types |> List.traverseResultM (expand mapping)
        let mutable allElems = []
        let mutable containsNonObjectType = false
        let mutable hasExactObjects = false

        for t in types do
          match (prune t).Kind with
          | TypeKind.Object { Elems = elems; Exact = exact } ->
            allElems <- allElems @ elems
            hasExactObjects <- hasExactObjects || exact
          | _ -> containsNonObjectType <- true

        if containsNonObjectType then
          // Needed for test case: InferTypeParamInIntersection
          return
            { Kind = TypeKind.Intersection types
              Provenance = None }
        else if hasExactObjects then
          // The intersection of exact types is never unless the types are
          // exactly the same.  Checking if two object types are exactly the
          // same is expensive so we just return `never` for now.  Spread
          // types should be used instead of intersection types when working
          // with object types since it actually corresponds to a real value-
          // level operation.
          return
            { Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }
        else
          return
            { Kind =
                TypeKind.Object
                  { Extends = None
                    Implements = None
                    Elems = allElems
                    Exact = false // TODO
                    Immutable = false // TODO
                    Mutable = false // TODO
                    Interface = false }
              Provenance = None }
      | TypeKind.TemplateLiteral { Exprs = elems; Parts = quasis } ->
        let! elems = elems |> List.traverseResultM (expand mapping)

        // TODO: check for other types that can't appear within template literal types
        let isSymbol t =
          match t.Kind with
          | TypeKind.UniqueSymbol _ -> true
          | _ -> false

        if List.exists isSymbol elems then
          // We don't bother reporting an error because template literal types
          // are only expanded when renaming properties in mapped types.
          // Returning `never` is fine because the mapped type expansion will
          // filter out any keys whose type is `never`.
          return
            { Kind = TypeKind.Keyword Keyword.Never
              Provenance = None }
        else
          let isLiteralOrIntrinsic t =
            match t.Kind with
            | TypeKind.Literal _ -> true
            | TypeKind.IntrinsicInstance { TypeArgs = typeArgs } ->
              match typeArgs with
              | Some [ { Kind = TypeKind.Literal _ } ] -> true
              | _ -> false
            | _ -> false

          // If all `elems` are literals, we can expand the type to a string.
          // This is used for property renaming in mapped types.
          if List.forall isLiteralOrIntrinsic elems then
            let mutable str = ""

            for elem, quasi in (List.zip elems (List.take elems.Length quasis)) do
              match elem.Kind with
              | TypeKind.Literal(Literal.String s) -> str <- str + quasi + s
              | TypeKind.Literal(Literal.Number n) ->
                str <- str + quasi + string (n)
              | TypeKind.Literal(Literal.Boolean b) ->
                str <- str + quasi + string (b)
              | TypeKind.IntrinsicInstance { Name = name
                                             TypeArgs = Some [ { Kind = TypeKind.Literal(Literal.String s) } ] } ->
                match name with
                | QualifiedIdent.Ident "Uppercase" ->
                  str <- str + quasi + s.ToUpper()
                | QualifiedIdent.Ident "Lowercase" ->
                  str <- str + quasi + s.ToLower()
                | QualifiedIdent.Ident "Capitalize" ->
                  str <-
                    str + quasi + s.[0].ToString().ToUpper() + s.[1..].ToLower()
                | QualifiedIdent.Ident "Uncapitalize" ->
                  str <-
                    str + quasi + s.[0].ToString().ToLower() + s.[1..].ToUpper()
                | _ ->
                  failwith $"Unsupported intrinsic {name} in template literal"
              | _ -> () // should never happen because we pre-filtered the list

            str <- str + List.last quasis

            return
              { Kind = TypeKind.Literal(Literal.String str)
                Provenance = None }
          else
            return t
      | _ ->
        // Replaces type parameters with their corresponding type arguments
        // TODO: do this more consistently
        if mapping = Map.empty then
          return t
        else
          return foldType fold t
    }

  expand mapping t

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
