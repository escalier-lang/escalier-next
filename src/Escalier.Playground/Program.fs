open FSharp.Data
open System.IO
open System.Runtime.InteropServices
open Extism

open Escalier.Compiler

type Data = { greeting: string; name: string }

[<UnmanagedCallersOnly(EntryPoint = "greet")>]
let Greet () : int32 =
  printfn "Greet called"

  let contents = File.ReadAllText("./input.txt")
  printfn "contents = %s" contents

  let input = Pdk.GetInputString()
  printfn $"input = {input}"
  let data = JsonValue.Parse input
  let greeting = data.GetProperty "greeting" |> _.AsString()
  let name = data.GetProperty "name" |> _.AsString()

  printfn $"greeting = {greeting}, name = {name}"

  let result = $"{greeting}, {name}!"
  Pdk.SetOutput(result)
  0

[<UnmanagedCallersOnly(EntryPoint = "compile")>]
let Compile () : int32 =
  printfn "Compile called"

  let srcCode = Pdk.GetInputString() // filename
  let textWriter = new StringWriter()

  match Compiler.compileString textWriter "/" srcCode with
  | Ok(js, dts) ->
    let js = System.Web.HttpUtility.JavaScriptStringEncode js
    let dts = System.Web.HttpUtility.JavaScriptStringEncode dts
    Pdk.SetOutput($"{{\"js\": \"{js}\", \"dts\": \"{dts}\"}}")
  | Error errorValue ->
    // TODO: update CompileOutput to include an error field
    printfn $"errorValue = {errorValue}"

  0

[<EntryPoint>]
let Main args =
  // Note: an `EntryPoint` function is required for the app to compile
  0
