module Tests

open FsToolkit.ErrorHandling
open Xunit
open System.IO

open Escalier.Compiler
open Escalier.TypeChecker.Env

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let binding = env.FindValue name
    Assert.Equal(expected, binding.Type.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = env.FindScheme name
    Assert.Equal(expected, scheme.ToString())

type Output =
  { js: string option
    dts: string option }

let projectRoot = __SOURCE_DIRECTORY__

let fixturePaths: obj[] seq =
  Directory.GetDirectories(Path.Join(projectRoot, "fixtures"))
  |> Seq.collect Directory.GetDirectories
  |> Seq.filter (fun dir -> not (dir.Contains("cyclic")))
  |> Seq.map (fun dir -> [| box (dir.Substring projectRoot.Length) |])

[<Theory>]
[<MemberData(nameof fixturePaths)>]
let BasicsTests (fixtureDir: string) =
  let testResult =
    result {
      let fixtureDir = Path.Join(projectRoot, fixtureDir)
      let testName = (Path.GetFileName fixtureDir)

      let entryPath =
        if File.Exists(Path.Join(fixtureDir, $"{testName}.esc")) then
          Path.Join(fixtureDir, $"{testName}.esc")
        else if File.Exists(Path.Join(fixtureDir, "entry.esc")) then
          Path.Join(fixtureDir, "entry.esc")
        else
          failwith "no valid entry file found"

      let errorsPath = "errors.txt"

      let files =
        Directory.GetFiles(fixtureDir, "*.esc")
        |> Array.map Path.GetFileNameWithoutExtension

      let mutable expected: Map<string, Output> = Map.empty

      for file in files do
        let jsPath = $"{file}.js"
        let dtsPath = $"{file}.d.ts"

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

        expected <- expected.Add(file, { js = jsExpected; dts = dtsExpected })

      let errorExpected =
        match File.Exists(Path.Join(fixtureDir, errorsPath)) with
        | true ->
          let content = File.ReadAllText(Path.Join(fixtureDir, errorsPath))
          File.Delete(Path.Join(fixtureDir, errorsPath))
          Some(content)
        | false -> None

      let mockWriter = new StringWriter()

      // TODO:
      // - Find all files that can be reached from the entry file (excluding
      //   imports from other packages)
      // - Update qualified declarations to include the module name
      // - Create a list of all declarations in the files

      // Questions:
      // - How do we handle renamed imports?
      //   We need to be careful to only add the dependencies that are needed
      //   when inferring a particular declaration.
      //   The tricky bit is decls involved in recursive definitions.  How do
      //   do we deal with renaming in that case?
      //   We just need to pick unique names for each declaration.  Once we've
      //   inferred all of their values we can change the names back to what
      //   they were in each file.
      //   Maybe we should give all declarations a unique name and then rename
      //   them back to their original names after we've inferred all of their
      //   values.

      let paths = Compiler.findFiles fixtureDir entryPath

      printfn "Filenames:"

      for path in paths do
        printfn "%s" path

      do! Compiler.compileFile mockWriter fixtureDir entryPath

      let mutable actual: Map<string, Output> = Map.empty

      for file in files do
        let jsPath = $"{file}.js"
        let dtsPath = $"{file}.d.ts"

        let jsActual =
          match File.Exists(Path.Join(fixtureDir, jsPath)) with
          | true -> Some(File.ReadAllText(Path.Join(fixtureDir, jsPath)))
          | false -> None

        let dtsActual =
          match File.Exists(Path.Join(fixtureDir, dtsPath)) with
          | true -> Some(File.ReadAllText(Path.Join(fixtureDir, dtsPath)))
          | false -> None

        actual <- actual.Add(file, { js = jsActual; dts = dtsActual })

      // Error messages are written to the console so we need to cpature
      // them.
      let errorActual =
        match mockWriter.ToString() with
        | "" -> None
        | output -> Some(output)

      let envVars = System.Environment.GetEnvironmentVariables()
      let shouldUpdate = envVars.Contains("ESCALIER_UPDATE_FIXTURES")

      if shouldUpdate then
        // Write the new output files to disk
        for KeyValue(file, { js = jsActual; dts = dtsActual }) in actual do
          let jsPath = $"{file}.js"
          let dtsPath = $"{file}.d.ts"

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
        for KeyValue(file, { js = jsExpected; dts = dtsExpected }) in expected do
          let jsPath = $"{file}.js"
          let dtsPath = $"{file}.d.ts"

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
        for KeyValue(file, { js = jsExpected; dts = dtsExpected }) in expected do
          let { js = jsActual; dts = dtsActual } = actual.[file]
          Assert.Equal(jsExpected, jsActual)
          Assert.Equal(dtsExpected, dtsActual)

        Assert.Equal(errorExpected, errorActual)

      return ()
    }

  Assert.True(Result.isOk testResult)
