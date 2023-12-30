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
let ParseSuccessCases () =
  let parser = exprParser

  let result = run (parser.Parse(0)) "-a"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "+a"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "--a"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a + b"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a + b + c"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a * b + c"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a + b * c"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a * (b + c)"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "A | B | C"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "A & B | C & D | E & F"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "A || B && C"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a!"
  printfn "result %A" result

  // once we handle precendence and add a while loop inside ParseExpr() this
  // should start working correctly.
  let result = run (parser.Parse(0)) "a!!"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a <= b"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a ≤ b"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "add(x, y)"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "add(x)(y)"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "foo()"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "array[index]"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "matrix[i1][i2]"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "operators[index](x, y)"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a.b.c"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "a?.b?.c"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "1 + 2 * 3"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "123"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "1.23"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "0..10"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "0.1"
  printfn "result %A" result

  let result = run (parser.Parse(0)) "1.foo()"
  printfn "result %A" result

[<Fact>]
let ParseErrorCases () =
  let parser = exprParser

  let result = run (parser.Parse(0)) "a + "
  printfn "result %A" result

  let result = run (parser.Parse(0)) "1."
  printfn "result %A" result

  let result = run (parser.Parse(0)) ".1"
  printfn "result %A" result
