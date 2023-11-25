module Escalier.Playground.Client.Main

open Elmish
open Bolero
open Bolero.Html

open Escalier.TypeChecker
open Escalier.Parser
open Escalier.Codegen

type Model = { x: string }

let initModel = { x = "" }

type Message = | Ping

let update message model =
  match message with
  | Ping -> model

let view model dispatch =
  let src = "let add = fn (x, y) => x + y"
  let ast = Parser.parseScript src

  match ast with
  | Ok(ast) ->
    let ctx: Codegen.Ctx = { NextTempId = 0 }
    let block = Codegen.buildScript ctx ast

    let printCtx: Printer.PrintCtx = { Indent = 0; Precedence = 0 }

    let js =
      block.Body |> List.map (Printer.printStmt printCtx) |> String.concat "\n"

    let env = Prelude.getEnv ()
    let t = TypeChecker.inferScript ast.Stmts env

    match t with
    | Ok(t) ->
      let mod' = Codegen.buildModuleTypes ctx ast
      let dts = Printer.printModule printCtx mod'

      code {
        attr.style "white-space: pre;"
        $"src.esc:\n{src}\n\nout.js:\n{js}\n\nout.d.ts:\n{dts}"
      }
    | Error(err) -> p { "No type" }
  | Error(err) -> p { "Parse Error" }

type MyApp() =
  inherit ProgramComponent<Model, Message>()

  override this.Program = Program.mkSimple (fun _ -> initModel) update view
