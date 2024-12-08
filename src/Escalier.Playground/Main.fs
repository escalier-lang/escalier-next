module Escalier.Playground.Main

open System
open System.IO
open System.Net.Http
open System.Net.Http.Json
open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Elmish
open Bolero
open Bolero.Html

open Escalier.Compiler.FileSystem
open Escalier.Compiler.Compiler

/// Routing endpoints definition.
type Page =
  | [<EndPoint "/">] Home
  | [<EndPoint "/counter">] Counter
  | [<EndPoint "/data">] Data

/// The Elmish application's model.
type Model =
  { page: Page
    counter: int
    books: Book[] option
    error: string option
    input: string
    output: string
    compiler: Compiler }

and Book =
  { title: string
    author: string
    publishDate: DateTime
    isbn: string }

/// The Elmish application's update messages.
type Message =
  | SetPage of Page
  | Increment
  | Decrement
  | SetCounter of int
  | GetBooks
  | GotBooks of Book[]
  | Error of exn
  | ClearError
  | SetOutput of string

let update (http: HttpClient) message model =
  match message with
  | SetPage page -> { model with page = page }, Cmd.none

  | Increment ->
    { model with
        counter = model.counter + 1 },
    Cmd.none
  | Decrement ->
    { model with
        counter = model.counter - 1 },
    Cmd.none
  | SetCounter value -> { model with counter = value }, Cmd.none

  | GetBooks ->
    let getBooks () =
      http.GetFromJsonAsync<Book[]>("/escalier-next/books.json")

    let cmd = Cmd.OfTask.either getBooks () GotBooks Error
    { model with books = None }, cmd
  | GotBooks books -> { model with books = Some books }, Cmd.none

  | Error exn -> { model with error = Some exn.Message }, Cmd.none
  | ClearError -> { model with error = None }, Cmd.none
  | SetOutput output -> { model with output = output }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

type MyComponent() =
  inherit Component()

  override this.Render() = div { "Hello, world!" }

let myElement: Node = comp<MyComponent> { attr.empty () }

let view (model: Model) dispatch =
  Main()
    .Input(model.input)
    .Output(model.output)
    // .Test(myElement)
    .Change(fun e ->
      let input = e.Value.ToString()
      printfn $"Input changed to: {input}"

      let writer = new StringWriter()
      let baseDir = "/"

      async {
        let! result = model.compiler.compileString writer baseDir input

        match result with
        | Ok(js, dts) ->
          printfn $"JS: {js}"
          printfn $"DTS: {dts}"
          dispatch (SetOutput js)
        | Result.Error e -> printfn $"Error: {e}"

      }
      |> Async.Start

    )
    .Elt()

let makeFileSystem (runtime: IJSRuntime) : IFileSystem =
  { new IFileSystem with
      member self.ReadAllTextAsync(path: string) : Async<string> =
        runtime.InvokeAsync("window.ReadAllTextAsync", path).AsTask()
        |> Async.AwaitTask

      member self.WriteAllTextAsync(path, text) =
        runtime.InvokeVoidAsync("window.WriteAllTextAsync", path, text).AsTask()
        |> Async.AwaitTask

      member self.FileExistsAsync path =
        runtime.InvokeAsync("window.FileExistsAsync", path).AsTask()
        |> Async.AwaitTask

      member self.DirExistsAsync path =
        runtime.InvokeAsync("window.DirExistsAsync", path).AsTask()
        |> Async.AwaitTask

      member self.GetParentAsync path =
        runtime.InvokeAsync<string>("window.GetParentAsync", path).AsTask()
        |> Async.AwaitTask }


type MyApp() =
  inherit ProgramComponent<Model, Message>()

  override _.CssScope = CssScopes.MyApp

  [<Inject>]
  member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

  override this.Program =
    let update = update this.HttpClient

    let fs = makeFileSystem this.JSRuntime
    let compiler = Compiler(fs)

    async {
      do!
        this.JSRuntime.InvokeVoidAsync("window.load").AsTask()
        |> Async.AwaitTask

      printfn "Finished loading"

      let writer = new StringWriter()
      let baseDir = "/"
      let src = "let x = 10;"
      let! result = compiler.compileString writer baseDir src

      match result with
      | Ok(js, dts) ->
        printfn $"JS: {js}"
        printfn $"DTS: {dts}"
      | Result.Error e -> printfn $"Error: {e}"

    }
    |> Async.Start

    // TODO: include the `compiler` instance on the model
    let initModel =
      { page = Home
        counter = 0
        books = None
        error = None
        input = "let x = 10;"
        output = "let x = 10;"
        compiler = compiler }

    Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetBooks) update view
    |> Program.withRouter router
