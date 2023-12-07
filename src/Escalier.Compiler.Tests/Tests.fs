module Tests

open Xunit
open System.IO
open Escalier.Compiler

let baseDir = __SOURCE_DIRECTORY__
let fixturePath = Path.Combine(baseDir, "fixtures", "basics")

let fixturePaths: obj[] seq =
  Directory.GetDirectories(fixturePath)
  |> Seq.collect (fun dir ->
    Directory.GetFiles(dir, "*.esc")
    |> Array.map (fun file ->
      file.Substring(baseDir.Length) |> fun file -> [| box file |]))

[<Theory>]
[<MemberData(nameof (fixturePaths))>]
let BasicsTests (srcPath: string) =
  printfn "Running test %s" srcPath
  Compiler.compile baseDir srcPath
