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
open Unify

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
  | PatternKind.Enum _ -> failwith "TODO: patternToPattern - PatternKind.Enum"

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
                           TypeArgs = typeArgs } ->
        match nsScheme.TryFind name with
        | Some _ ->
          let name = QualifiedIdent.Member(QualifiedIdent.Ident nsName, name)

          let kind =
            TypeKind.TypeRef
              { Name = name
                TypeArgs = typeArgs
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
            Error(
              TypeError.NotImplemented "TODO: lookup member on union type"
            )
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
    | TypeKind.Array { Elem = elem; Length = length } ->
      // TODO: update `TypeKind.Array` to also contain a `Length` type param
      // TODO: use getPropertyType to look up the type of the `.length` property
      // here (and when handling tuples) instead of fabricating it here.
      // TODO: make type param mutable so that we can update it if it's a mutable
      // array and the array size changes.
      match key with
      | PropName.String "length" -> return length
      | PropName.Number _ ->
        let unknown =
          { Kind = TypeKind.Literal(Literal.Undefined)
            Provenance = None }

        return union [ elem; unknown ]
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
          instantiateType ctx prop arrayScheme.TypeParams (Some [ elem ])

        return prop
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
        Error(TypeError.NotImplemented $"TODO: lookup member on type - {t}")
  }
