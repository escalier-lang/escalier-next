module Escalier.Playground.Client.Main

open Elmish
open Bolero
open Bolero.Html

open Escalier.Data.Type
open Escalier.TypeChecker
open Escalier.Parser
open Escalier.Codegen


let makeParam (name: string) (ty: Type) : FuncParam =
  { Pattern = Pattern.Identifier name
    Type = ty
    Optional = false }

let getEnv () : TypeChecker.Env =
  let arithemtic =
    (TypeChecker.makeFunctionType
      None
      [ makeParam "left" TypeChecker.numType
        makeParam "right" TypeChecker.numType ]
      TypeChecker.numType,
     false)

  let comparison =
    (TypeChecker.makeFunctionType
      None
      [ makeParam "left" TypeChecker.numType
        makeParam "right" TypeChecker.numType ]
      TypeChecker.boolType,
     false)

  let logical =
    (TypeChecker.makeFunctionType
      None
      [ makeParam "left" TypeChecker.boolType
        makeParam "right" TypeChecker.boolType ]
      TypeChecker.boolType,
     false)

  let typeRefA =
    { Kind = TypeChecker.makePrimitiveKind "A"
      Provenance = None }

  let typeRefB =
    { Kind = TypeChecker.makePrimitiveKind "B"
      Provenance = None }

  let typeParams: list<TypeParam> =
    [ { Name = "A"
        Constraint = None
        Default = None }
      { Name = "B"
        Constraint = None
        Default = None } ]

  // TODO: figure out how to make quality polymorphic
  let equality =
    (TypeChecker.makeFunctionType
      (Some(typeParams))
      [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
      TypeChecker.boolType,
     false)

  { TypeChecker.Env.Values =
      Map.ofList
        [ ("+", arithemtic)
          ("-", arithemtic)
          ("*", arithemtic)
          ("/", arithemtic)
          ("%", arithemtic)
          ("**", arithemtic)
          ("<", comparison)
          ("<=", comparison)
          (">", comparison)
          (">=", comparison)
          ("==", equality)
          ("!=", equality)
          ("||", logical)
          ("&&", logical) ]
    TypeChecker.Env.Schemes = Map([])
    TypeChecker.Env.IsAsync = false }


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

    let env = getEnv ()
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
