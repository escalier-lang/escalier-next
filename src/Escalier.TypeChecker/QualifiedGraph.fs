module Escalier.TypeChecker.QualifiedGraph

open Escalier.Data.Type

open Escalier.Data


type QualifiedIdent =
  { Namespaces: list<string>
    Name: string }

  override this.ToString() =
    match this.Namespaces with
    | [] -> this.Name
    | namespaces ->
      let namespaces = String.concat "." namespaces
      $"{namespaces}.{this.Name}"

  static member FromString(name: string) = { Namespaces = []; Name = name }

  static member FromCommonQualifiedIdent
    (qid: Common.QualifiedIdent)
    : QualifiedIdent =
    match qid with
    | Common.QualifiedIdent.Ident name -> { Namespaces = []; Name = name }
    | Common.QualifiedIdent.Member(left, right) ->
      let left = QualifiedIdent.FromCommonQualifiedIdent left
      let namespaces = left.Namespaces @ [ left.Name ]

      { Namespaces = namespaces
        Name = right }

// TODO:
// - infer types for all the declarations in each namespace
// - determine the dependences between declarations in each namespace
// - infer them in the correct order

// TODO: use a list instead of QualifiedIdent which is recursive
type QDeclIdent =
  | Type of QualifiedIdent
  | Value of QualifiedIdent

  override this.ToString() =
    match this with
    | Type qid -> $"Type {qid}"
    | Value qid -> $"Value {qid}"

type QGraph<'T> =
  // A type can depend on multiple interface declarations
  { Nodes: Map<QDeclIdent, list<'T>>
    Edges: Map<QDeclIdent, list<QDeclIdent>> }

  member this.Add(name: QDeclIdent, decl: 'T, deps: list<QDeclIdent>) =
    let decls =
      match this.Nodes.TryFind name with
      | Some nodes -> nodes @ [ decl ]
      | None -> [ decl ]

    { Edges = this.Edges.Add(name, deps)
      Nodes = this.Nodes.Add(name, decls) }

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

type SyntaxNode = Graph.SyntaxNode
