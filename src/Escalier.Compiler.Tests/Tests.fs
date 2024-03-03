module Tests

open FsToolkit.ErrorHandling
open Xunit
open System.IO
open System.IO.Abstractions.TestingHelpers

open Escalier.Compiler
open Escalier.TypeChecker.Env

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())


let baseDir = __SOURCE_DIRECTORY__
// TODO: iterate over all subdirectories of `fixtures` instead of hardcoding basics
let fixtureDir = Path.Combine("fixtures", "basics")

let fixturePaths: obj[] seq =
  Directory.GetDirectories(Path.Join(baseDir, fixtureDir))
  |> Seq.map (fun dir -> [| box (dir.Substring baseDir.Length) |])

[<Theory>]
[<MemberData(nameof fixturePaths)>]
let BasicsTests (fixtureDir: string) =
  let testResult =
    result {
      let testName = (Path.GetFileName fixtureDir)

      let srcPath = Path.Join(fixtureDir, $"{testName}.esc")
      let jsPath = Path.Join(fixtureDir, $"{testName}.js")
      let dtsPath = Path.Join(fixtureDir, $"{testName}.d.ts")
      let errorsPath = Path.Join(fixtureDir, $"{testName}.errors")

      let srcContents = File.ReadAllText(Path.Join(baseDir, srcPath))

      // TODO: use the real filesystem when update fixtures
      let mockFileSystem = MockFileSystem()
      mockFileSystem.AddFile(srcPath, srcContents)

      let mockWriter = new StringWriter()

      do! Compiler.compileFile mockFileSystem mockWriter "" srcPath

      let jsOutputExpected =
        match File.Exists(Path.Join(baseDir, jsPath)) with
        | true -> Some(File.ReadAllText(Path.Join(baseDir, jsPath)))
        | false -> None

      let jsOutputActual =
        match mockFileSystem.File.Exists(jsPath) with
        | true -> Some(mockFileSystem.File.ReadAllText(jsPath))
        | false -> None

      let envVars = System.Environment.GetEnvironmentVariables()
      let shouldUpdate = envVars.Contains("ESCALIER_UPDATE_FIXTURES")

      if envVars.Contains("ESCALIER_UPDATE_FIXTURES") then
        match jsOutputActual with
        | Some(output) ->
          let path = Path.Join(baseDir, jsPath)
          File.WriteAllText(path, output)
          printfn "Wrote %s" path
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
        match mockFileSystem.File.Exists(dtsPath) with
        | true -> Some(mockFileSystem.File.ReadAllText(dtsPath))
        | false -> None

      if shouldUpdate then
        match dtsOutputActual with
        | Some(output) ->
          let path = Path.Join(baseDir, dtsPath)
          File.WriteAllText(path, output)
          printfn "Wrote %s" path
        | None ->
          // TODO: delete the file
          ()
      else
        Assert.Equal(dtsOutputExpected, dtsOutputActual)

      let errorOutputExpected =
        match File.Exists(Path.Join(baseDir, errorsPath)) with
        | true -> Some(File.ReadAllText(Path.Join(baseDir, errorsPath)))
        | false -> None

      let errorOutputActual =
        match mockWriter.ToString() with
        | "" -> None
        | output -> Some(output)

      if shouldUpdate then
        match errorOutputActual with
        | Some(output) ->
          let path = Path.Join(baseDir, errorsPath)
          File.WriteAllText(path, output)
          printf "Wrote %s" path
        | None ->
          // TODO: delete the file
          ()
      else
        Assert.Equal(errorOutputExpected, errorOutputActual)

      return ()
    }

  Assert.True(Result.isOk testResult)

[<Fact>]
let SimpleNamedImports () =
  let result =
    result {

      let mockFileSystem = MockFileSystem()

      let files =
        [ "/imports1/entry.esc"; "/imports1/math.esc"; "/imports1/point.esc" ]

      for file in files do
        let srcPath = Path.Join("/fixtures/imports", file)
        let srcContents = File.ReadAllText(Path.Join(baseDir, srcPath))
        mockFileSystem.AddFile(srcPath, srcContents)

      let mockWriter = new StringWriter()

      let! ctx, env =
        Compiler.compileFiles
          mockFileSystem
          mockWriter
          "/fixtures/imports"
          "/fixtures/imports/imports1/entry.esc"

      if ctx.Diagnostics.Length > 0 then
        Compiler.printDiagnostics mockWriter ctx.Diagnostics
        printfn "DIAGNOSTICS:\n%s" (mockWriter.ToString())

      Assert.Value(env, "p", "Point")
      Assert.Equal(ctx.Diagnostics.Length, 0)

      return ()
    }

  printfn "result = %A" result
  Assert.True(Result.isOk result)

[<Fact>]
let ModuleAliasImport () =
  let result =
    result {

      let mockFileSystem = MockFileSystem()

      let files =
        [ "/imports2/entry.esc"; "/imports2/math.esc"; "/imports2/point.esc" ]

      for file in files do
        let srcPath = Path.Join("/fixtures/imports", file)
        let srcContents = File.ReadAllText(Path.Join(baseDir, srcPath))
        mockFileSystem.AddFile(srcPath, srcContents)

      let mockWriter = new StringWriter()

      let! ctx, env =
        Compiler.compileFiles
          mockFileSystem
          mockWriter
          "/fixtures/imports"
          "/fixtures/imports/imports2/entry.esc"

      if ctx.Diagnostics.Length > 0 then
        Compiler.printDiagnostics mockWriter ctx.Diagnostics
        printfn "DIAGNOSTICS:\n%s" (mockWriter.ToString())

      Assert.Value(env, "p", "point.Point")
      Assert.Equal(ctx.Diagnostics.Length, 0)

      return ()
    }

  printfn "result = %A" result
  Assert.True(Result.isOk result)
