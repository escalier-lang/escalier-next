module Escalier.Interop.Tests.MergeLib

open FsToolkit.ErrorHandling
open Xunit
open System.IO

open Escalier.Interop.MergeLib

[<Fact>]
let testResolvePath () =
  let projectRoot = __SOURCE_DIRECTORY__

  let resolvedPath = resolvePath projectRoot projectRoot "@apollo/client"

  let combinedPath = Path.Combine("/foo/bar", "../baz")
  let di = DirectoryInfo(combinedPath)
  printfn $"di.FullName = {di.FullName}"

  Assert.Equal(
    "/Users/kevinbarabash/projects/escalier-next/node_modules/@apollo/client/index.d.ts",
    resolvedPath
  )

[<Fact>]
let testGetDependencies () =
  let res =
    result {
      let projectRoot = __SOURCE_DIRECTORY__

      let! deps = getDependencies projectRoot "@apollo/client"

      printfn $"deps = {deps}"
    }

  printfn $"res = %A{res}"
  Assert.True(Result.isOk res)
