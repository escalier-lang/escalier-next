open System.IO
open System.Text.Json
open System.Runtime.InteropServices
open Extism

type Data = { greeting: string; name: string }

[<UnmanagedCallersOnly(EntryPoint = "greet")>]
let Greet () : int32 =
  printfn "Greet called"

  let contents = File.ReadAllText("./input.txt")
  printfn "contents = %s" contents

  let input = Pdk.GetInputString()
  printfn $"input = {input}"
  let data = JsonSerializer.Deserialize<Data> input

  printfn $"greeting = {data.greeting}, name = {data.name}"

  let result = $"{data.greeting}, {data.name}!"
  Pdk.SetOutput(result)
  0

[<EntryPoint>]
let Main args =
  // Note: an `EntryPoint` function is required for the app to compile
  0
