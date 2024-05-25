module Escalier.TypeChecker.Tests.SCCTests

open Escalier.TypeChecker.Env
open Xunit

open Escalier.TypeChecker.StronglyConnectedComponents

[<Fact>]
let EmptyGraph () =
  let graph: Graph<unit> = { Nodes = Map.empty; Edges = Map.empty }

  let components = findStronglyConnectedComponents graph

  Assert.Equal(0, components.Length)

[<Fact>]
let GraphWithSingleCycleThatIsTheEntireGraph () =
  let nodes: Map<DeclIdent, unit> =
    Map.ofList
      [ DeclIdent.Value "A", ()
        DeclIdent.Value "B", ()
        DeclIdent.Value "C", () ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A", [ DeclIdent.Value "B" ]
        DeclIdent.Value "B", [ DeclIdent.Value "C" ]
        DeclIdent.Value "C", [ DeclIdent.Value "A" ] ]

  let graph = { Nodes = nodes; Edges = edges }

  let components = findStronglyConnectedComponents graph

  Assert.Equal(1, components.Length)
  Assert.Equal(3, components.[0].Length)

[<Fact>]
let GraphWithSingleCycleThatIsntTheEntireGraph () =
  let nodes: Map<DeclIdent, unit> =
    Map.ofList
      [ DeclIdent.Value "A", ()
        DeclIdent.Value "B", ()
        DeclIdent.Value "C", () ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A", [ DeclIdent.Value "B" ]
        DeclIdent.Value "B", [ DeclIdent.Value "A" ] ]

  let graph = { Nodes = nodes; Edges = edges }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ DeclIdent.Value "C" ]; [ DeclIdent.Value "A"; DeclIdent.Value "B" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)

[<Fact>]
let GraphWithComponentsWithMultipleDependencies () =
  let nodes: Map<DeclIdent, unit> =
    Map.ofList
      [ DeclIdent.Value "A", ()
        DeclIdent.Value "B", ()
        DeclIdent.Value "C", ()
        DeclIdent.Value "D", () ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A",
        [ DeclIdent.Value "B"; DeclIdent.Value "C"; DeclIdent.Value "D" ]
        // one of the dependencies of "A" contains a cycle
        DeclIdent.Value "C", [ DeclIdent.Value "D" ]
        DeclIdent.Value "D", [ DeclIdent.Value "C" ] ]

  let graph = { Nodes = nodes; Edges = edges }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ Value "A" ]; [ Value "C"; Value "D" ]; [ Value "B" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)

  let tree = buildComponentTree graph actual

  Assert.Equal(
    tree.ToString(),
    """map [(set [Value "A"], set [set [Value "B"]; set [Value "C"; Value "D"]]); (set [Value "B"], set []); (set [Value "C"; Value "D"], set [])]"""
  )

  let entryPoints = findEntryPoints tree

  Assert.Equal(entryPoints.ToString(), """set [set [Value "A"]]""")

[<Fact>]
let GraphWithMultipleComponents () =
  let nodes: Map<DeclIdent, unit> =
    Map.ofList
      [ DeclIdent.Value "A", ()
        DeclIdent.Value "B", ()
        DeclIdent.Value "C", ()
        DeclIdent.Value "D", ()
        DeclIdent.Value "E", () ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A", [ DeclIdent.Value "B" ]
        DeclIdent.Value "B", [ DeclIdent.Value "A"; DeclIdent.Value "C" ]
        DeclIdent.Value "C", [ DeclIdent.Value "D" ]
        DeclIdent.Value "D", [ DeclIdent.Value "E" ]
        DeclIdent.Value "E", [ DeclIdent.Value "D" ] ]

  let graph = { Nodes = nodes; Edges = edges }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ Value "A"; Value "B" ]; [ Value "C" ]; [ Value "D"; Value "E" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)

  let tree = buildComponentTree graph actual

  Assert.Equal(
    tree.ToString(),
    """map [(set [Value "A"; Value "B"], set [set [Value "C"]]); (set [Value "C"], set [set [Value "D"; Value "E"]]); (set [Value "D"; Value "E"], set [])]"""
  )

  let entryPoints = findEntryPoints tree

  Assert.Equal(entryPoints.ToString(), """set [set [Value "A"; Value "B"]]""")

[<Fact>]
let GraphWithMultipleEntryPoints () =
  let nodes: Map<DeclIdent, unit> =
    Map.ofList
      [ DeclIdent.Value "A", ()
        DeclIdent.Value "B", ()
        DeclIdent.Value "C", ()
        DeclIdent.Value "D", ()
        DeclIdent.Value "E", () ]

  let edges: Map<DeclIdent, list<DeclIdent>> =
    Map.ofList
      [ DeclIdent.Value "A", [ DeclIdent.Value "B" ]
        DeclIdent.Value "B", [ DeclIdent.Value "A"; DeclIdent.Value "C" ]
        DeclIdent.Value "C", []
        DeclIdent.Value "D", [ DeclIdent.Value "E" ]
        DeclIdent.Value "E", [ DeclIdent.Value "D" ] ]

  let graph = { Nodes = nodes; Edges = edges }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ Value "D"; Value "E" ]; [ Value "A"; Value "B" ]; [ Value "C" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)

  let tree = buildComponentTree graph actual

  Assert.Equal(
    tree.ToString(),
    """map [(set [Value "A"; Value "B"], set [set [Value "C"]]); (set [Value "C"], set []); (set [Value "D"; Value "E"], set [])]"""
  )

  let entryPoints = findEntryPoints tree

  Assert.Equal(
    entryPoints.ToString(),
    """set [set [Value "A"; Value "B"]; set [Value "D"; Value "E"]]"""
  )

[<Fact>]
let GraphWithNoEdges () =
  let nodes: Map<DeclIdent, unit> =
    Map.ofList
      [ DeclIdent.Value "A", ()
        DeclIdent.Value "B", ()
        DeclIdent.Value "C", () ]

  let edges: Map<DeclIdent, list<DeclIdent>> = Map.empty

  let graph: Graph<unit> = { Nodes = nodes; Edges = edges }

  let actual = findStronglyConnectedComponents graph

  let expected: list<list<DeclIdent>> =
    [ [ Value "C" ]; [ Value "B" ]; [ Value "A" ] ]

  Assert.Equal<DeclIdent list list>(expected, actual)
