module Tests

open FsToolkit.ErrorHandling
open Xunit
open System.IO
open System.IO.Abstractions.TestingHelpers

open Escalier.Compiler

let baseDir = __SOURCE_DIRECTORY__
// TODO: iterate over all subdirectories of `fixtures` instead of hardcoding basics
let fixturePath = Path.Combine(baseDir, "fixtures", "basics")

let fixturePaths: obj[] seq =
  Directory.GetDirectories(fixturePath) |> Seq.map (fun dir -> [| box dir |])

[<Theory>]
[<MemberData(nameof (fixturePaths))>]
let BasicsTests (dirname: string) =
  result {
    let envVars = System.Environment.GetEnvironmentVariables()
    let shouldUpdate = envVars.Contains("ESCALIER_UPDATE_FIXTURES")

    let filenameBase = (Path.GetFileName dirname)

    // relative to the baseDir
    let srcPath =
      Path.Join(dirname, $"{filenameBase}.esc").Substring(baseDir.Length)

    // relative to the baseDir
    let jsPath =
      if File.Exists(Path.Join(dirname, $"{filenameBase}.js")) then
        Some(Path.ChangeExtension(srcPath, ".js"))
      else
        None

    // relative to the baseDir
    let dtsPath =
      if File.Exists(Path.Join(dirname, $"{filenameBase}.d.ts")) then
        Some(Path.ChangeExtension(srcPath, ".d.ts"))
      else
        None

    let fullSrcPath = Path.Join(baseDir, srcPath)
    let contents = File.ReadAllText(fullSrcPath)

    // TODO: use the real filesystem when update fixtures
    let filesystem = MockFileSystem()
    filesystem.AddFile(srcPath, contents)

    do! Compiler.compile filesystem "" srcPath

    let jsOutputExpected =
      jsPath
      |> Option.map (fun jsPath -> File.ReadAllText(Path.Join(baseDir, jsPath)))

    let jsOutputActual =
      jsPath |> Option.map (fun jsPath -> filesystem.File.ReadAllText(jsPath))

    if shouldUpdate then
      match jsOutputActual with
      | Some(output) ->
        let path = Path.Join(dirname, $"{filenameBase}.js")
        File.WriteAllText(path, output)
        printfn "Updated %s" path
      | None ->
        // TODO: delete the file
        ()
    else
      Assert.Equal(jsOutputExpected, jsOutputActual)

    let dtsOutputExpected =
      dtsPath
      |> Option.map (fun dtsPath ->
        File.ReadAllText(Path.Join(baseDir, dtsPath)))

    let dtsOutputActual =
      dtsPath
      |> Option.map (fun dtsPath -> filesystem.File.ReadAllText(dtsPath))

    if shouldUpdate then
      match dtsOutputActual with
      | Some(output) ->
        let path = Path.Join(dirname, $"{filenameBase}.d.ts")
        File.WriteAllText(path, output)
        printfn "Updated %s" path
      | None ->
        // TODO: delete the file
        ()
    else
      Assert.Equal(dtsOutputExpected, dtsOutputActual)

    return ()
  }
