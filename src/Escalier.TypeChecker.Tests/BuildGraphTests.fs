module Escalier.TypeChecker.Tests.BuildGraphTests

open Escalier.Data.Type
open Xunit

open Escalier.TypeChecker.QualifiedGraph
open Escalier.TypeChecker.BuildGraph

[<Fact>]
let PlaceholderTest () =
  let ns = Namespace.empty
  let ident = QDeclIdent.MakeValue [ "x" ]
  let localsTree = localsToDeclTree Set.empty
  let actual = postProcessDeps ns Set.empty localsTree ident []

  let expected = Set.empty
  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let SimpleDeps () =
  let ns = Namespace.empty
  let locals = Set.singleton (QDeclIdent.MakeValue [ "foo" ])
  let localsTree = localsToDeclTree locals
  let ident = QDeclIdent.MakeValue [ "x" ]
  let deps = [ QDeclIdent.MakeValue [ "foo" ]; QDeclIdent.MakeValue [ "bar" ] ]

  let actual = postProcessDeps ns locals localsTree ident deps

  let expected = Set.singleton (QDeclIdent.MakeValue [ "foo" ])
  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let DepsWithMemberAccess () =
  let ns = Namespace.empty
  let locals = Set.singleton (QDeclIdent.MakeValue [ "foo" ])
  let localsTree = localsToDeclTree locals
  let ident = QDeclIdent.MakeValue [ "x" ]
  let deps = [ QDeclIdent.MakeValue [ "foo"; "bar" ] ]

  let actual = postProcessDeps ns locals localsTree ident deps

  let expected = Set.singleton (QDeclIdent.MakeValue [ "foo" ])
  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let QualifiedDeps () =
  let ns = Namespace.empty
  let locals = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar"; "baz" ])
  let localsTree = localsToDeclTree locals
  let ident = QDeclIdent.MakeValue [ "x" ]
  let deps = [ QDeclIdent.MakeValue [ "foo"; "bar"; "baz" ] ]

  let actual = postProcessDeps ns locals localsTree ident deps

  let expected = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar"; "baz" ])
  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let QualifiedDepsWithMemberAccess () =
  let ns = Namespace.empty
  let locals = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar" ])
  let localsTree = localsToDeclTree locals
  let ident = QDeclIdent.MakeValue [ "x" ]
  let deps = [ QDeclIdent.MakeValue [ "foo"; "bar"; "baz" ] ]

  let actual = postProcessDeps ns locals localsTree ident deps

  let expected = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar" ])
  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let SimpleDepsNeedingNamespaces () =
  let ns = Namespace.empty
  let locals = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar" ])
  let localsTree = localsToDeclTree locals
  let ident = QDeclIdent.MakeValue [ "foo"; "x" ]
  let deps = [ QDeclIdent.MakeValue [ "bar" ]; QDeclIdent.MakeValue [ "baz" ] ]

  let result = postProcessDeps ns locals localsTree ident deps

  let expected = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar" ])

  Assert.Equal<QDeclIdent Set>(expected, result)

[<Fact>]
let FullnamespacedDepsInsideNamespace () =
  let ns = Namespace.empty
  let locals = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar" ])
  let localsTree = localsToDeclTree locals
  let ident = QDeclIdent.MakeValue [ "foo"; "x" ]

  let deps =
    [ QDeclIdent.MakeValue [ "foo"; "bar" ]
      QDeclIdent.MakeValue [ "foo"; "baz" ] ]

  let result = postProcessDeps ns locals localsTree ident deps

  let expected = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar" ])

  Assert.Equal<QDeclIdent Set>(expected, result)

[<Fact>]
let PartialShadowing () =
  let ns = Namespace.empty
  let locals = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar"; "baz" ])
  let localsTree = localsToDeclTree locals
  let ident = QDeclIdent.MakeValue [ "foo"; "bar"; "x" ]
  let deps = [ QDeclIdent.MakeValue [ "bar"; "baz" ] ]

  let result = postProcessDeps ns locals localsTree ident deps

  let expected = Set.singleton (QDeclIdent.MakeValue [ "foo"; "bar"; "baz" ])

  Assert.Equal<QDeclIdent Set>(expected, result)
