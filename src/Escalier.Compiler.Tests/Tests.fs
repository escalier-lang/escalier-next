module Tests

open Xunit

open Escalier.Compiler

let baseDir = __SOURCE_DIRECTORY__

[<Fact>]
let BasicsTest1 () =
  Compiler.compile baseDir "fixtures/basics/test1/test1.esc"
