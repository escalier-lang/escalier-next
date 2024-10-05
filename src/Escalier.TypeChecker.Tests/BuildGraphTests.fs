module Escalier.TypeChecker.Tests.BuildGraphTests

open Escalier.Data.Type
open Escalier.TypeChecker.Env
open Xunit

open Escalier.TypeChecker.QualifiedGraph
open Escalier.TypeChecker.BuildGraph

let env = Env.empty "hello.esc"

[<Fact>]
let PlaceholderTest () =
  let ns = Namespace.empty
  let ident = QDeclIdent.MakeValue env.Filename [ "x" ]
  let localsTree = localsToDeclTree env Set.empty
  let actual = postProcessDeps env ns Set.empty localsTree ident []

  let expected = Set.empty
  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let SimpleDeps () =
  let ns = Namespace.empty
  let locals = Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo" ])
  let localsTree = localsToDeclTree env locals
  let ident = QDeclIdent.MakeValue env.Filename [ "x" ]

  let deps =
    [ QDeclIdent.MakeValue env.Filename [ "foo" ]
      QDeclIdent.MakeValue env.Filename [ "bar" ] ]

  let actual = postProcessDeps env ns locals localsTree ident deps

  let expected = Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo" ])
  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let DepsWithMemberAccess () =
  let ns = Namespace.empty
  let locals = Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo" ])
  let localsTree = localsToDeclTree env locals
  let ident = QDeclIdent.MakeValue env.Filename [ "x" ]
  let deps = [ QDeclIdent.MakeValue env.Filename [ "foo"; "bar" ] ]

  let actual = postProcessDeps env ns locals localsTree ident deps

  let expected = Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo" ])
  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let QualifiedDeps () =
  let ns = Namespace.empty

  let locals =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar"; "baz" ])

  let localsTree = localsToDeclTree env locals
  let ident = QDeclIdent.MakeValue env.Filename [ "x" ]
  let deps = [ QDeclIdent.MakeValue env.Filename [ "foo"; "bar"; "baz" ] ]

  let actual = postProcessDeps env ns locals localsTree ident deps

  let expected =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar"; "baz" ])

  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let QualifiedDepsWithMemberAccess () =
  let ns = Namespace.empty

  let locals =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar" ])

  let localsTree = localsToDeclTree env locals
  let ident = QDeclIdent.MakeValue env.Filename [ "x" ]
  let deps = [ QDeclIdent.MakeValue env.Filename [ "foo"; "bar"; "baz" ] ]

  let actual = postProcessDeps env ns locals localsTree ident deps

  let expected =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar" ])

  Assert.Equal<QDeclIdent Set>(expected, actual)

[<Fact>]
let SimpleDepsNeedingNamespaces () =
  let ns = Namespace.empty

  let locals =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar" ])

  let localsTree = localsToDeclTree env locals
  let ident = QDeclIdent.MakeValue env.Filename [ "foo"; "x" ]

  let deps =
    [ QDeclIdent.MakeValue env.Filename [ "bar" ]
      QDeclIdent.MakeValue env.Filename [ "baz" ] ]

  let result = postProcessDeps env ns locals localsTree ident deps

  let expected =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar" ])

  Assert.Equal<QDeclIdent Set>(expected, result)

[<Fact>]
let FullnamespacedDepsInsideNamespace () =
  let ns = Namespace.empty

  let locals =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar" ])

  let localsTree = localsToDeclTree env locals
  let ident = QDeclIdent.MakeValue env.Filename [ "foo"; "x" ]

  let deps =
    [ QDeclIdent.MakeValue env.Filename [ "foo"; "bar" ]
      QDeclIdent.MakeValue env.Filename [ "foo"; "baz" ] ]

  let result = postProcessDeps env ns locals localsTree ident deps

  let expected =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar" ])

  Assert.Equal<QDeclIdent Set>(expected, result)

[<Fact>]
let PartialShadowing () =
  let ns = Namespace.empty

  let locals =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar"; "baz" ])

  let localsTree = localsToDeclTree env locals
  let ident = QDeclIdent.MakeValue env.Filename [ "foo"; "bar"; "x" ]
  let deps = [ QDeclIdent.MakeValue env.Filename [ "bar"; "baz" ] ]

  let result = postProcessDeps env ns locals localsTree ident deps

  let expected =
    Set.singleton (QDeclIdent.MakeValue env.Filename [ "foo"; "bar"; "baz" ])

  Assert.Equal<QDeclIdent Set>(expected, result)
