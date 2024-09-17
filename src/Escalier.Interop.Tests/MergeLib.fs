module Escalier.Interop.Tests.MergeLib

open FsToolkit.ErrorHandling
open Xunit

open Escalier.Interop.MergeLib

[<Fact>]
let testGetDependencies () =
  let res =
    result {
      let projectRoot = __SOURCE_DIRECTORY__

      let! deps = getDependencies projectRoot "@apollo/client"

      printfn $"deps.Length = {deps.Length}"
    }

  printfn $"res = %A{res}"
  Assert.True(Result.isOk res)
