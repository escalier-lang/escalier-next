module Tests

open FsToolkit.ErrorHandling
open Xunit
open System.IO
open System.IO.Abstractions
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


let projectRoot = __SOURCE_DIRECTORY__
// TODO: iterate over all subdirectories of `fixtures` instead of hardcoding basics
let fixtureDir = Path.Combine("fixtures", "basics")

let fixturePaths: obj[] seq =
  Directory.GetDirectories(Path.Join(projectRoot, fixtureDir))
  |> Seq.map (fun dir -> [| box (dir.Substring projectRoot.Length) |])

[<Theory>]
[<MemberData(nameof fixturePaths)>]
let BasicsTests (fixtureDir: string) =
  let testResult =
    result {
      let fixtureDir = Path.Join(projectRoot, fixtureDir)
      printfn $"Running test for {fixtureDir}"
      let testName = (Path.GetFileName fixtureDir)

      // input file
      let srcPath = $"{testName}.esc"

      // output files
      let jsPath = $"{testName}.js"
      let dtsPath = $"{testName}.d.ts"
      let errorsPath = $"{testName}.errors"

      let jsExpected =
        match File.Exists(Path.Join(fixtureDir, jsPath)) with
        | true ->
          let content = File.ReadAllText(Path.Join(fixtureDir, jsPath))
          File.Delete(Path.Join(fixtureDir, jsPath))
          Some(content)
        | false -> None

      let dtsExpected =
        match File.Exists(Path.Join(fixtureDir, dtsPath)) with
        | true ->
          let content = File.ReadAllText(Path.Join(fixtureDir, dtsPath))
          File.Delete(Path.Join(fixtureDir, dtsPath))
          Some(content)
        | false -> None

      let errorExpected =
        match File.Exists(Path.Join(fixtureDir, errorsPath)) with
        | true ->
          let content = File.ReadAllText(Path.Join(fixtureDir, errorsPath))
          File.Delete(Path.Join(fixtureDir, errorsPath))
          Some(content)
        | false -> None

      let filesystem = FileSystem()
      let mockWriter = new StringWriter()
      do! Compiler.compileFile filesystem mockWriter fixtureDir srcPath

      let jsActual =
        match File.Exists(Path.Join(fixtureDir, jsPath)) with
        | true -> Some(File.ReadAllText(Path.Join(fixtureDir, jsPath)))
        | false -> None

      let dtsActual =
        match File.Exists(Path.Join(fixtureDir, dtsPath)) with
        | true -> Some(File.ReadAllText(Path.Join(fixtureDir, dtsPath)))
        | false -> None

      // Error messages are written to the console so we need to cpature
      // them.
      let errorActual =
        match mockWriter.ToString() with
        | "" -> None
        | output -> Some(output)

      let envVars = System.Environment.GetEnvironmentVariables()
      let shouldUpdate = envVars.Contains("ESCALIER_UPDATE_FIXTURES")

      if envVars.Contains("ESCALIER_UPDATE_FIXTURES") then
        // Write the new output files to disk
        if jsActual.IsSome then
          let path = Path.Join(fixtureDir, jsPath)
          File.WriteAllText(path, jsActual.Value)

        if dtsActual.IsSome then
          let path = Path.Join(fixtureDir, dtsPath)
          File.WriteAllText(path, dtsActual.Value)

        if errorActual.IsSome then
          let path = Path.Join(fixtureDir, errorsPath)
          File.WriteAllText(path, errorActual.Value)
      else
        // Write the original output files to disk
        if jsExpected.IsSome then
          let path = Path.Join(fixtureDir, jsPath)
          File.WriteAllText(path, jsExpected.Value)

        if dtsExpected.IsSome then
          let path = Path.Join(fixtureDir, dtsPath)
          File.WriteAllText(path, dtsExpected.Value)

        if errorExpected.IsSome then
          let path = Path.Join(fixtureDir, errorsPath)
          File.WriteAllText(path, errorExpected.Value)

        // Assert that the actual output matches the expected output
        Assert.Equal(jsExpected, jsActual)
        Assert.Equal(dtsExpected, dtsActual)
        Assert.Equal(errorExpected, errorActual)

      let dtsOutputExpected =
        match File.Exists(Path.Join(projectRoot, dtsPath)) with
        | true -> Some(File.ReadAllText(Path.Join(projectRoot, dtsPath)))
        | false -> None

      let dtsOutputActual =
        match filesystem.File.Exists(dtsPath) with
        | true -> Some(filesystem.File.ReadAllText(dtsPath))
        | false -> None

      if shouldUpdate then
        match dtsOutputActual with
        | Some(output) ->
          let path = Path.Join(projectRoot, dtsPath)
          File.WriteAllText(path, output)
        | None ->
          // TODO: delete the file
          ()
      else
        Assert.Equal(dtsOutputExpected, dtsOutputActual)

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
        let srcContents = File.ReadAllText(Path.Join(projectRoot, srcPath))
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
        let srcContents = File.ReadAllText(Path.Join(projectRoot, srcPath))
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
