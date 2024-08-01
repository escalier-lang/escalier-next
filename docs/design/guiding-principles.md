# Guiding Principles

The over-arching goal of Escalier is to improve on certain aspects of TypeScript
while still being highlight backwards compatible.

## Immutable by Default

Mutability is often the source of issues. Making immutability the default will
make it easier to isolate code that performs mutations which should make it easier
to track down issues when they arise.

## Expression Heavy Language

JavaScript is a statement-heavy language. Escalier is an expression heavy
language. The following statement types in JavaScript will be expressions in
Escalier:

- ✅ `if`-`else`
- ✅ `try`-`catch`-`finally`
- 😴 `throw`
- ✅ `switch`-`case` (superseded by `match`)

## Amazing TypeScript Interop

- 🚧 Support all of TypeScript's types
  - Syntax will differ in places for improved readability
  - Semantics will differ in places for improved type safety
  - ✅ Mapped types
    - ✅ Utility types based on mapped types (`Pick<T>`, `Omit<T>`, etc.)
  - ✅ Conditional types
  - ✅ Generics
    - 😴 Variance
  - 😴 Template literal types
  - 😴 Predicate functions
- 🚧 Support all of TypeScript's language features
  - ✅ `async`/`await`
  - 😴 Generators (and async generators)
  - 🚧 Iterators
  - 🚧 Classes
  - 🚧 Enums
- 🚧 Bidirectional interop, the compiler can:
  - ✅ Read .d.ts files, e.g. standard library definitions that ship with TypeScript
    as well as the React lib defs
  - 🚧 Write .d.ts files generated from Escalier source
- 😴 Migration tool to help with converting TypeScript code to Escalier

## Improved Safety:

This includes improving both type safety and semantic safety.

- ✅ Properly variance handling when assigning and passing collection values (for
  both immutable and mutable collections)
- ✅ Automatically rethrow unhandled exceptions
- 😴 Require promises to be awaited or returned from functions
- 😴 Require that captures variables be initialized before calling a closure that
  captures them
- ...

## Improved Type Inference:

The goal is not to be able to infer everything all of the time, but to infer
more things than are currently inferred at the moment.

- ✅ Function params and return types are inferred from the function body

## Implement highly request TypeScript features

- 😴 [#12936: Exact Types](https://github.com/microsoft/TypeScript/issues/12936)
- 😴 [#16607: Allow "Compiler Plugins"](https://github.com/microsoft/TypeScript/issues/16607)
- 😴 [#10727: Add spread/rest higher-order types operator](https://github.com/microsoft/TypeScript/issues/10727)
- 🚧 [#17592: Extending string-based enums](https://github.com/microsoft/TypeScript/issues/17592)
- 😴 [#6579: Suggestion: Regex-validated string type](https://github.com/microsoft/TypeScript/issues/6579)
- 🚧 [#13219: Suggestion: throws clause and typed catch clause](https://github.com/microsoft/TypeScript/issues/13219)
- ...

## Lean Into Type-Level Programming

- ✅ Reuse value-level syntax at the type-level, e.g.
  - ✅ Use `if`-`else` for conditional types (nested/chained `if`-`else` are easier
    to read than nested/chained ternaries)
  - ✅ Allow pattern matching at the type-level
- 😴 Regex validated strings
- 😴 Template literal types
- 🚧 Dependent types (post-MVP)

## Future JavaScript Language Features

- ✅ Pattern matching (this doesn't follow the TC39 proposal which I found to be overly complicated)
- ✅ Immutable Records & Tuples
- ✅ do-expressions

## Extensible (Post-MVP)

- 😴 Provide access to the typed AST to support type-aware lint rules
- 😴 Plugin system to allow other developers to provide types to type-system, e.g.
  - provide types for GraphQL queries
  - more accurate types for CSS styles
  - ...
