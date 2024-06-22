module rec Escalier.TypeChecker.Helpers

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Syntax
open Escalier.Data.Common
open Escalier.Data.Type

open Error
open Env
open ExprVisitor
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

let findBindingNames (p: Syntax.Pattern) : list<string> =
  let mutable names: list<string> = []

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | ExprKind.Function _ -> (false, state)
          | _ -> (true, state)
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

            (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  walkPattern visitor () p

  List.rev names

let findReturns (body: BlockOrExpr) : list<Expr> =
  let mutable returns: list<Expr> = []

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | ExprKind.Function _ -> (false, state)
          | _ -> (true, state)
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
  | BlockOrExpr.Block block -> List.iter (walkStmt visitor ()) block.Stmts
  | BlockOrExpr.Expr expr ->
    walkExpr visitor () expr // There might be early returns in match expression
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

  let visitor: SyntaxVisitor<unit> =
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
      ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  match body with
  | BlockOrExpr.Block block -> List.iter (walkStmt visitor ()) block.Stmts
  | BlockOrExpr.Expr expr -> walkExpr visitor () expr

  throws

// TODO: dedupe with findThrows
let findThrowsInBlock (block: Block) : list<Type> =
  let mutable throws: list<Type> = []

  let visitor: SyntaxVisitor<unit> =
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
      ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  List.iter (walkStmt visitor ()) block.Stmts

  throws

let findModuleBindingNames (m: Script) : list<string> =
  let mutable names: list<string> = []

  for item in m.Items do
    match item with
    | Stmt stmt ->
      match stmt.Kind with
      | StmtKind.Decl({ Kind = DeclKind.VarDecl { Pattern = pattern } }) ->
        names <- List.concat [ names; findBindingNames pattern ]
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

let simplifyUnion (t: Type) : Type =
  match (prune t).Kind with
  | TypeKind.Union types ->
    let objTypes, otherTypes =
      List.partition
        (fun t ->
          match t.Kind with
          | TypeKind.Object _ -> true
          | _ -> false)
        types

    if objTypes.IsEmpty then
      t
    else
      // Collect the types of each named property into a map of lists
      let mutable namedTypes: Map<string, list<Type>> = Map.empty

      // TODO: handle other object element types
      for objType in objTypes do
        match objType.Kind with
        | TypeKind.Object { Elems = elems; Immutable = _ } ->
          for elem in elems do
            match elem with
            | ObjTypeElem.Property { Name = PropName.String name
                                     Optional = _
                                     Readonly = _
                                     Type = t } ->

              let types =
                match Map.tryFind name namedTypes with
                | Some(types) -> types
                | None -> []

              namedTypes <- Map.add name (t :: types) namedTypes
            | _ -> ()
        | _ -> ()

      // Simplify each list of types
      let namedTypes = namedTypes |> Map.map (fun _name types -> union types)

      // Create a new object type from the simplified named properties
      let objTypeElems =
        namedTypes
        |> Map.map (fun name t ->
          ObjTypeElem.Property
            { Name = PropName.String name
              Optional = false
              Readonly = false
              Type = t })
        |> Map.values
        |> Seq.toList

      let objType =
        { Kind =
            // QUESTION: what when to properties from the super type when simplifying?
            TypeKind.Object
              { Extends = None
                Implements = None
                Elems = objTypeElems
                Immutable = false
                Interface = false }
          Provenance = None }

      union (objType :: otherTypes)
  | _ -> t

let rec getIsMut (ctx: Ctx) (env: Env) (expr: Expr) : Result<bool, TypeError> =
  result {
    match expr.Kind with
    | ExprKind.Identifier name ->
      let! _, isMut = env.GetBinding name
      return isMut
    | ExprKind.Index(target, _index, _optChain) ->
      return! getIsMut ctx env target
    | ExprKind.Member(target, _name, _optChain) ->
      return! getIsMut ctx env target
    | _ ->
      return! Error(TypeError.SemanticError $"{expr} is not a valid lvalue")
  }
