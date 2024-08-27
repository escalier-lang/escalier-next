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
        | ObjTypeElem.Method(name, fn) ->
          ObjTypeElem.Method(name, (generalizeFunc fn))
        | ObjTypeElem.Getter(name, fn) ->
          ObjTypeElem.Getter(name, (generalizeFunc fn))
        | ObjTypeElem.Setter(name, fn) ->
          ObjTypeElem.Setter(name, (generalizeFunc fn))
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

  for KeyValue(name, (t, isMut)) in bindings do
    let t = generalizeType t
    newBindings <- newBindings.Add(name, (t, isMut))

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

let findModuleBindingNames (m: Script) : Set<string> =
  let mutable names: Set<string> = Set.empty

  for item in m.Items do
    match item with
    | ScriptItem.Stmt stmt ->
      match stmt.Kind with
      | StmtKind.Decl({ Kind = DeclKind.VarDecl { Pattern = pattern } }) ->
        names <- Set.union names (findBindingNames pattern)
      | _ -> ()
    | _ -> ()

  names

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
      let! _, isMut = env.GetBinding name
      return isMut
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
        | Method(name, fn) ->
          let t =
            { Kind = TypeKind.Function fn
              Provenance = None }

          map <- Map.add name t map
        | Getter(name, fn) ->
          failwith "TODO: getPropertyMap - ObjElemType.Getter"
        | Setter(name, fn) ->
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
