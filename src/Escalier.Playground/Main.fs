module Escalier.Playground.Client.Main

open Elmish
open FsToolkit.ErrorHandling
open Bolero
open Bolero.Html

open Escalier.TypeChecker
open Escalier.Parser
open Escalier.Codegen

type Model = { src: string }

let initModel =
  { src =
      """
type Point = {x: number, y: number}
let add = fn (x, y) => x + y
let sum = add(5, 10)
let fst = fn (x, y) => x
""" }

type Message = Recompile of string

type CompileError =
  | ParseError of FParsec.Error.ParserError
  | TypeError of Error.TypeError

type CompilerOutput = { Js: string; Dts: string }

let compile (src: string) : Result<CompilerOutput, CompileError> =
  result {
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError
    let ctx: Codegen.Ctx = { NextTempId = 0 }
    let block = Codegen.buildScript ctx ast
    let printCtx: Printer.PrintCtx = { Indent = 0; Precedence = 0 }

    let js =
      block.Body |> List.map (Printer.printStmt printCtx) |> String.concat "\n"

    let env = Prelude.getEnv ()

    let! env =
      TypeChecker.inferScript ast.Stmts env
      |> Result.mapError CompileError.TypeError

    let mod' = Codegen.buildModuleTypes env ctx ast
    let dts = Printer.printModule printCtx mod'

    return { Js = js; Dts = dts }
  }

let update message model =
  match message with
  | Recompile src -> { model with src = src }

let view model dispatch =
  let src = model.src

  div {
    attr.style "display: flex; flex-direction: column;"
    pre { "foo.esc:" }

    textarea {
      attr.style "height: 200px;"
      attr.value model.src

      on.input (fun e ->
        let value = e.Value.ToString()
        dispatch (Recompile value))
    }

    match compile src with
    | Ok({ Js = js; Dts = dts }) ->
      div {
        attr.style
          "display: flex; flex-direction: row; justify-content: space-between;"

        div {
          attr.style "flex: 1;"
          pre { $"foo.js:\n{js}" }
        }

        div {
          attr.style "flex: 1;"
          pre { $"foo.d.ts:\n{dts}" }
        }
      }
    | Error(err) -> div { $"Error: {err}" }
  }

type MyApp() =
  inherit ProgramComponent<Model, Message>()

  override this.Program = Program.mkSimple (fun _ -> initModel) update view
