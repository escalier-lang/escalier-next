module Escalier.TypeChecker.Tests.BuildGraphTests

open Xunit

open Escalier.TypeChecker.QualifiedGraph
open Escalier.TypeChecker.BuildGraph

[<Fact>]
let PlaceholderTest () =
  let ident = QDeclIdent.MakeValue [] "x"
  let actual = postProcessValueDeps [] ident []

  let expected = []
  Assert.Equal<QDeclIdent list>(expected, actual)

[<Fact>]
let SimpleDeps () =
  let ident = QDeclIdent.MakeValue [] "x"

  let deps = [ QDeclIdent.MakeValue [] "foo"; QDeclIdent.MakeValue [] "bar" ]
  let actual = postProcessValueDeps [] ident deps

  let expected = deps
  Assert.Equal<QDeclIdent list>(expected, actual)

[<Fact>]
let QualifiedDeps () =
  let ident = QDeclIdent.MakeValue [] "x"

  let deps = [ QDeclIdent.MakeValue [ "foo"; "bar" ] "baz" ]
  let actual = postProcessValueDeps [] ident deps

  let expected = deps
  Assert.Equal<QDeclIdent list>(expected, actual)

[<Fact>]
let SimpleDepsNeedingNamespaces () =
  let ident = QDeclIdent.MakeValue [ "foo" ] "x"

  let deps = [ QDeclIdent.MakeValue [] "bar"; QDeclIdent.MakeValue [] "baz" ]
  let result = postProcessValueDeps [] ident deps

  let expected =
    [ QDeclIdent.MakeValue [ "foo" ] "bar"
      QDeclIdent.MakeValue [ "foo" ] "baz" ]

  Assert.Equal<QDeclIdent list>(expected, result)

[<Fact>]
let FullnamespacedDepsInsideNamespace () =
  let ident = QDeclIdent.MakeValue [ "foo" ] "x"

  let deps =
    [ QDeclIdent.MakeValue [ "foo" ] "bar"
      QDeclIdent.MakeValue [ "foo" ] "baz" ]

  let result = postProcessValueDeps [] ident deps

  let expected =
    [ QDeclIdent.MakeValue [ "foo" ] "bar"
      QDeclIdent.MakeValue [ "foo" ] "baz" ]

  Assert.Equal<QDeclIdent list>(expected, result)

// TODO: write tests for shadowing of identifiers in nested namespaces

[<Fact>]
let PartialShadowing () =
  let ident = QDeclIdent.MakeValue [ "foo"; "bar" ] "x"

  let deps = [ QDeclIdent.MakeValue [ "bar" ] "baz" ]
  let result = postProcessValueDeps [] ident deps

  let expected = [ QDeclIdent.MakeValue [ "foo"; "bar" ] "baz" ]

  Assert.Equal<QDeclIdent list>(expected, result)


[<Fact(Skip = "TODO")>]
let PartialShadowingWithLocals () =
  let ident = QDeclIdent.MakeValue [ "foo"; "bar" ] "x"

  let deps = [ QDeclIdent.MakeValue [ "bar" ] "baz" ]

  let result =
    postProcessValueDeps
      [ QDeclIdent.MakeValue [ "foo"; "bar" ] "baz" ]
      ident
      deps

  let expected = [ QDeclIdent.MakeValue [ "foo"; "bar" ] "baz" ]

  Assert.Equal<QDeclIdent list>(expected, result)
