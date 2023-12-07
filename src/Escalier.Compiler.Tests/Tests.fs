module Tests

open FsToolkit.ErrorHandling
open Xunit
open System.IO
open System.IO.Abstractions.TestingHelpers

open Escalier.Compiler

let baseDir = __SOURCE_DIRECTORY__
// TODO: iterate over all subdirectories of `fixtures` instead of hardcoding basics
let fixtureDir = Path.Combine("fixtures", "basics")

let fixturePaths: obj[] seq =
  Directory.GetDirectories(Path.Join(baseDir, fixtureDir))
  |> Seq.map (fun dir -> [| box (dir.Substring baseDir.Length) |])

[<Theory>]
[<MemberData(nameof fixturePaths)>]
let BasicsTests (fixtureDir: string) =
  result {
    let testName = (Path.GetFileName fixtureDir)

    let srcPath = Path.Join(fixtureDir, $"{testName}.esc")
    let jsPath = Path.Join(fixtureDir, $"{testName}.js")
    let dtsPath = Path.Join(fixtureDir, $"{testName}.d.ts")

    let srcContents = File.ReadAllText(Path.Join(baseDir, srcPath))

    // TODO: use the real filesystem when update fixtures
    let filesystem = MockFileSystem()
    filesystem.AddFile(srcPath, srcContents)

    do! Compiler.compile filesystem "" srcPath

    let jsOutputExpected =
      match File.Exists(Path.Join(baseDir, jsPath)) with
      | true -> Some(File.ReadAllText(Path.Join(baseDir, jsPath)))
      | false -> None

    let jsOutputActual =
      match filesystem.File.Exists(jsPath) with
      | true -> Some(filesystem.File.ReadAllText(jsPath))
      | false -> None

    let envVars = System.Environment.GetEnvironmentVariables()
    let shouldUpdate = envVars.Contains("ESCALIER_UPDATE_FIXTURES")

    if envVars.Contains("ESCALIER_UPDATE_FIXTURES") then
      match jsOutputActual with
      | Some(output) ->
        let path = Path.Join(baseDir, jsPath)
        File.WriteAllText(path, output)
        printfn "Updated %s" path
      | None ->
        // TODO: delete the file
        ()
    else
      Assert.Equal(jsOutputExpected, jsOutputActual)

    let dtsOutputExpected =
      match File.Exists(Path.Join(baseDir, dtsPath)) with
      | true -> Some(File.ReadAllText(Path.Join(baseDir, dtsPath)))
      | false -> None

    let dtsOutputActual =
      match filesystem.File.Exists(dtsPath) with
      | true -> Some(filesystem.File.ReadAllText(dtsPath))
      | false -> None

    if shouldUpdate then
      match dtsOutputActual with
      | Some(output) ->
        let path = Path.Join(baseDir, dtsPath)
        File.WriteAllText(path, output)
        printfn "Updated %s" path
      | None ->
        // TODO: delete the file
        ()
    else
      Assert.Equal(dtsOutputExpected, dtsOutputActual)

    return ()
  }
