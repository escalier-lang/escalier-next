module Escalier.TypeChecker.StronglyConnectedComponents

open Escalier.TypeChecker.Env

type Graph<'T> =
  { Edges: Map<DeclIdent, list<DeclIdent>>
    Nodes: Map<DeclIdent, 'T> }

// Based on the algorithm from https://en.wikipedia.org/wiki/Path-based_strong_component_algorithm
let findStronglyConnectedComponents<'T>
  (graph: Graph<'T>)
  : list<list<DeclIdent>> =

  let mutable S: list<DeclIdent> = [] // not yet assigned to a SCC
  let mutable P: list<DeclIdent> = [] // not yet in different SCCs
  let mutable preorder: Map<DeclIdent, int> = Map.empty
  let mutable C: int = 0
  let mutable components: list<list<DeclIdent>> = []

  let rec visit (v: DeclIdent) : unit =
    // 1. Set the preorder number of v to C, and increment C.
    preorder <- Map.add v C preorder
    C <- C + 1

    // 2. Push v onto S and also onto P.
    S <- v :: S
    P <- v :: P

    let deps =
      match graph.Edges.TryFind v with
      | None -> []
      | Some deps -> deps

    // 3. For each edge from v to a neighboring vertex w:
    for dep in deps do
      let w = dep

      match preorder.TryFind w with
      | None ->
        // If the preorder number of w has not yet been assigned (the edge is a
        // tree edge), recursively search w;
        visit w
      | Some _ ->
        // Otherwise, if w has not yet been assigned to a strongly connected
        // component (the edge is a forward/back/cross edge):
        if List.contains w S then
          // Repeatedly pop vertices from P until the top element of P has a
          // preorder number less than or equal to the preorder number of w
          while preorder[List.head P] > preorder[w] do
            P <- List.tail P // pop from P

    let mutable comp: list<DeclIdent> = []

    // 4. If v is the top element of P:
    if v = List.head P then
      // Pop vertices from S until v has been popped, and assign the popped
      // vertices to a new component.
      while v <> List.head S do
        comp <- List.head S :: comp
        S <- List.tail S

      comp <- List.head S :: comp
      S <- List.tail S

      // Pop v from P.
      P <- List.tail P

      components <- comp :: components

  for v in graph.Nodes.Keys do
    if not (preorder.ContainsKey v) then
      visit v

  components

type CompTree = Map<Set<DeclIdent>, Set<Set<DeclIdent>>>

let buildComponentTree<'T>
  (graph: Graph<'T>)
  (components: list<list<DeclIdent>>)
  : CompTree =

  let comps = List.map (fun comp -> Set.ofList comp) components
  let mutable compMap: Map<DeclIdent, Set<DeclIdent>> = Map.empty

  for comp in comps do
    for v in comp do
      compMap <- Map.add v comp compMap

  let mutable tree: CompTree = Map.empty

  for comp in comps do
    let mutable targets = Set.empty

    let mutable compDepNodes = Set.empty

    for node in comp do
      let nodeDeps =
        match graph.Edges.TryFind node with
        | None -> Set.empty
        | Some deps -> Set.ofList deps

      compDepNodes <- Set.union (Set.difference nodeDeps comp) compDepNodes

    let compDeps = Set.map (fun dep -> Map.find dep compMap) compDepNodes
    tree <- Map.add comp compDeps tree

  tree

let findEntryPoints (tree: CompTree) : Set<Set<DeclIdent>> =
  let mutable allDeps = Set.empty

  for KeyValue(_, deps) in tree do
    allDeps <- Set.union allDeps deps

  Set.difference (Set.ofSeq tree.Keys) allDeps
