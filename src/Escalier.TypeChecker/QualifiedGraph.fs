module Escalier.TypeChecker.QualifiedGraph

open Escalier.Data.Syntax

open Escalier.Data
open Escalier.Data.Type
open Escalier.Data.Visitor


type QualifiedIdent =
  { Filename: string
    Parts: list<string> }

  override this.ToString() = String.concat "." this.Parts

  static member FromString (filename: string) (name: string) =
    { Filename = filename
      Parts = [ name ] }

  static member FromCommonQualifiedIdent
    (filename: string)
    (qid: Common.QualifiedIdent)
    : QualifiedIdent =
    match qid with
    | Common.QualifiedIdent.Ident name ->
      { Filename = filename
        Parts = [ name ] }
    | Common.QualifiedIdent.Member(left, right) ->
      let left = QualifiedIdent.FromCommonQualifiedIdent filename left

      { Filename = filename
        Parts = left.Parts @ [ right ] }

// TODO:
// - infer types for all the declarations in each namespace
// - determine the dependences between declarations in each namespace
// - infer them in the correct order

// TODO: use a list instead of QualifiedIdent which is recursive
[<RequireQualifiedAccess>]
type QDeclIdent =
  | Type of QualifiedIdent
  | Value of QualifiedIdent

  override this.ToString() =
    match this with
    | Type qid -> $"Type {qid}"
    | Value qid -> $"Value {qid}"

  static member MakeValue (filename: string) (parts: list<string>) =
    QDeclIdent.Value { Filename = filename; Parts = parts }

  static member MakeType (filename: string) (parts: list<string>) =
    QDeclIdent.Type { Filename = filename; Parts = parts }

  member this.GetParts() =
    match this with
    | Type { Parts = parts } -> parts
    | Value { Parts = parts } -> parts

type DeclOrImport =
  | Decl of Decl
  | Import of Import

type QGraph =
  // A type can depend on multiple interface declarations
  { Nodes: Map<QDeclIdent, list<DeclOrImport>>
    Edges: Map<QDeclIdent, Set<QDeclIdent>> }

type QualifiedNamespace =
  { Values: Map<QualifiedIdent, Binding>
    Schemes: Map<QualifiedIdent, Scheme> }

  static member Empty =
    { Values = Map.empty
      Schemes = Map.empty }

  member this.AddValue (ident: QualifiedIdent) (binding: Binding) =
    { this with
        Values = Map.add ident binding this.Values }

  member this.AddScheme (ident: QualifiedIdent) (scheme: Scheme) =
    { this with
        Schemes = Map.add ident scheme this.Schemes }

type SyntaxNode =
  | TypeAnn of Syntax.TypeAnn
  | TypeRef of Syntax.TypeRef
  | Expr of Syntax.Expr

let findFunctions (expr: Syntax.Expr) : list<Syntax.Function> =
  let mutable fns: list<Syntax.Function> = []

  let visitor =
    { ExprVisitor.VisitExpr =
        fun (expr, state) ->
          match expr.Kind with
          | Syntax.ExprKind.Function f ->
            fns <- f :: fns
            (false, state)
          | _ -> (true, state)
      ExprVisitor.VisitJsxElement = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxFragment = fun (_, state) -> (true, state)
      ExprVisitor.VisitJsxText = fun (_, state) -> (false, state)
      ExprVisitor.VisitStmt = fun (_, state) -> (true, state)
      ExprVisitor.VisitPattern = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnn = fun (_, state) -> (false, state)
      ExprVisitor.VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

  ExprVisitor.walkExpr visitor () expr

  List.rev fns

// TODO: Merge ReadonlyFoo and Foo as part Escalier.Interop.Migrate
let mergeType (imutType: Type) (mutType: Type) : Type =
  // If a method exists on both `imutType` and `mutType` then it's a mutable method
  // If it only exists on `imutType` then it's an immutable method
  // There should never be a method that only exists on `mutType`

  match imutType.Kind, mutType.Kind with
  | TypeKind.Object imutElems, TypeKind.Object mutElems ->
    // TODO: figure out how to handle overloaded methods
    let mutable imutNamedElems: Map<PropName, ObjTypeElem> =
      imutElems.Elems
      |> List.choose (fun elem ->
        match elem with
        | ObjTypeElem.Property p ->
          match p.Name with
          | PropName.String s -> Some(PropName.String s, elem)
          | PropName.Number n -> Some(PropName.Number n, elem)
          | PropName.Symbol i -> Some(PropName.Symbol i, elem)
        | ObjTypeElem.Method { Name = name } ->
          match name with
          | PropName.String s -> Some(PropName.String s, elem)
          | PropName.Number n -> Some(PropName.Number n, elem)
          | PropName.Symbol i -> Some(PropName.Symbol i, elem)
        | ObjTypeElem.Getter { Name = name } ->
          match name with
          | PropName.String s -> Some(PropName.String s, elem)
          | PropName.Number n -> Some(PropName.Number n, elem)
          | PropName.Symbol i -> Some(PropName.Symbol i, elem)
        | ObjTypeElem.Setter { Name = name } ->
          match name with
          | PropName.String s -> Some(PropName.String s, elem)
          | PropName.Number n -> Some(PropName.Number n, elem)
          | PropName.Symbol i -> Some(PropName.Symbol i, elem)
        | _ -> None)
      |> Map.ofSeq

    let mutable unnamedElems: list<ObjTypeElem> = []

    let mutable mutNamedElems: Map<PropName, ObjTypeElem> =
      mutElems.Elems
      |> List.choose (fun elem ->
        match elem with
        | ObjTypeElem.Property p ->
          match p.Name with
          | PropName.String s -> Some(PropName.String s, elem)
          | PropName.Number n -> Some(PropName.Number n, elem)
          | PropName.Symbol i -> Some(PropName.Symbol i, elem)
        | ObjTypeElem.Method { Name = name } ->
          match name with
          | PropName.String s -> Some(PropName.String s, elem)
          | PropName.Number n -> Some(PropName.Number n, elem)
          | PropName.Symbol i -> Some(PropName.Symbol i, elem)
        | ObjTypeElem.Getter { Name = name } ->
          match name with
          | PropName.String s -> Some(PropName.String s, elem)
          | PropName.Number n -> Some(PropName.Number n, elem)
          | PropName.Symbol i -> Some(PropName.Symbol i, elem)
        | ObjTypeElem.Setter { Name = name } ->
          match name with
          | PropName.String s -> Some(PropName.String s, elem)
          | PropName.Number n -> Some(PropName.Number n, elem)
          | PropName.Symbol i -> Some(PropName.Symbol i, elem)
        | elem ->
          // This assumes that indexed/mapped signatures are on both the
          // readonly and non-readonly interfaces.  We ignore the readonly
          // one because the signature isn't responsible for preventing
          // mutation of the object in this way.  Instead we have a special
          // check for this.
          unnamedElems <- elem :: unnamedElems
          None)
      |> Map.ofSeq

    let elems =
      mutNamedElems
      |> Map.toSeq
      |> Seq.map (fun (key, value) ->
        match imutNamedElems.TryFind key with
        | Some(imutValue) -> imutValue
        | None ->
          match value with
          | ObjTypeElem.Method { Name = name; Fn = fn } ->
            let self =
              match fn.Self with
              | None -> None
              | Some self ->
                Some
                  { self with
                      Pattern =
                        Pattern.Identifier { Name = "self"; IsMut = true } }

            ObjTypeElem.Method
              { Name = name
                Fn = { fn with Self = self } }
          | elem -> elem)

    let kind =
      TypeKind.Object
        { Extends = None
          Implements = None // TODO
          Elems = List.ofSeq elems @ unnamedElems
          Exact = false // MergeType should only be used with interfaces which can't be exact
          Immutable = false
          Mutable = false
          Interface = false }

    { Kind = kind; Provenance = None }
  | _ -> failwith "both types must be objects to merge them"
