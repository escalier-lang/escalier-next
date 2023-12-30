[<VerifyXunit.UsesVerify>]
module PrattTests

open Xunit
open VerifyTests
open FParsec


open Pratt

let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

[<Fact>]
let ParseArithmetic () =
  let parser = exprParser

  let result = run (parser.ParseExpr(0)) "-a"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "+a"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "--a"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "a + b"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "a + b + c"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "a * b + c"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "a + b * c"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "a * (b + c)"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "A | B | C"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "A & B | C & D | E & F"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "a!"
  printfn "result %A" result

  // once we handle precendence and add a while loop inside ParseExpr() this
  // should start working correctly.
  let result = run (parser.ParseExpr(0)) "a!!"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "a <= b"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "add(x, y)"
  printfn "result %A" result

  let result = run (parser.ParseExpr(0)) "add(x)(y)"
  printfn "result %A" result

// Test Cases
// - a < b
// - a <= b
// - a ≤ b
// - 0..10
// - 0.1
// - .1
// - 1.
// - x || y && z
// - a?.b?.c
