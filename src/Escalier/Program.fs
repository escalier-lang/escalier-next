﻿// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open FParsec
open Escalier.Parser

let str s = pstring s
// let floatBetweenBrackets = (str "[") >>. pfloat .>> (str "]")
// let betweenStrings s1 s2 p = str s1 >>. p .>> str s2
// let floatBetweenBrackets = pfloat |> betweenStrings "[" "]"

let ws = spaces

let strWs s = pstring s .>> ws
let floatWs = pfloat .>> ws

let numberList = (strWs "[") >>. sepBy floatWs (strWs ",") .>> (strWs "]")

let numberListFile = ws >>. numberList .>> eof

let ab = str "a" <|> str "b"

let identifier =
  let isIdentifierFirstChar c = isLetter c || c = '_'
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'

  many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

let stringLiteral2 =
  let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')

  let escapedChar =
    pstring "\\"
    >>. (anyOf "\\nrt\""
         |>> function
           | 'n' -> "\n"
           | 'r' -> "\r"
           | 't' -> "\t"
           | c -> string c)

  between
    (pstring "\"")
    (pstring "\"")
    (manyStrings (normalCharSnippet <|> escapedChar))

let stringLiteral3 =
  let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')

  let escapedChar =
    pstring "\\"
    >>. (anyOf "\\nrt\""
         |>> function
           | 'n' -> "\n"
           | 'r' -> "\r"
           | 't' -> "\t"
           | c -> string c)

  between
    (pstring "\"")
    (pstring "\"")
    (stringsSepBy normalCharSnippet escapedChar)

let test p str =
  match run p str with
  | Success(result, _, _) -> printfn "Success: %A" result
  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"
test numberList "[1, 2, 3]"
test (many ab) "abba"
test identifier "_test1="
test stringLiteral2 "\"hello,\tworld!\""
test stringLiteral3 "\"hello,\t\tworld!\""

let name = "world"
let msg = $"hello, {name}!"

let test' p str =
  match p str with
  | Success(result, _, _) -> printfn "Success: %A" result
  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test' (run Parser.expr) "1 + 2 * -abc"
test' (run Parser.expr) "msg = \"Hello,\\tworld!\""
// test' (run Parser.expr) "`a ${b} c ${d} e`"
test' (run Parser.expr) "`a ${`b ${c} d`} e`"
test' (run Parser.expr) "`a ${`b ${c} d` e`"

test' (run Parser.expr) "array[0]()"
test' (run Parser.expr) "foo()[0]"

test' (run Parser.expr) "fn (x, y) { return x + y }"
test' (run Parser.expr) "fn (x, y) { return }"
test' (run Parser.stmt) "let sum = x + y"

test' (run Parser.typeAnn) "number | string | boolean"
test' (run Parser.typeAnn) "number & string & boolean"
// TODO: figure out how to parse this
test' (run Parser.typeAnn) "number[]"
test' (run Parser.typeAnn) "Foo"

let src =
  """
if (true) {
  5
} else if (false) {
  10
} else {
  "hello"
}
"""

test' (run (many Parser.stmt)) src

test' (run Parser.expr) "fn (x, y) => x + y"
