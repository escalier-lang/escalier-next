namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Type
open Escalier.Data.Syntax

open Error
open Env

module Mutability =
  type BindingPaths = Map<string, list<string> * bool>

  let getPatBindingPaths (pat: Pattern) : BindingPaths =
    let mutable result: BindingPaths = Map.empty

    let rec walkPattern (pat: Pattern) (path: list<string>) =
      match pat.Kind with
      | PatternKind.Ident { Name = name; IsMut = mut } ->
        result <- Map.add name (path, mut) result
      | PatternKind.Object { Elems = elems } ->
        for elem in elems do
          match elem with
          | KeyValuePat { Key = key; Value = value } ->
            walkPattern value (key :: path)
          | ShorthandPat { Name = name; IsMut = mut } ->
            result <- Map.add name ((name :: path), mut) result
          | RestPat { Target = target; IsMut = mut } ->
            // TODO: What should be the path for the rest pattern?
            // It should be multiple paths, one for each property in the pattern
            // We could use a "wildcard" path element to model this as long as
            // we only check paths with wildcards after checking those without
            printfn "TODO: getBindingPaths - ObjPatElem.RestPat"
      | PatternKind.Tuple tuple ->
        for i, elem in tuple.Elems |> List.indexed do
          walkPattern elem (string i :: path)
      | PatternKind.Wildcard wildcardPattern -> ()
      | PatternKind.Literal literal -> ()
      | PatternKind.Rest pattern ->
        // TODO: What should be the path for the rest pattern?
        // It should be multiple paths, one for each property in the pattern
        // We could use a "wildcard" path element to model this as long as
        // we only check paths with wildcards after checking those without
        printfn "TODO: getPatBindingPaths - Rest"
      | PatternKind.Enum(_) -> failwith "TODO: getPatBindingPaths - Enum"

    walkPattern pat []
    result

  let getTypePatBindingPaths (pat: Type.Pattern) : BindingPaths =
    let mutable result: BindingPaths = Map.empty

    let rec walkPattern (pat: Type.Pattern) (path: list<string>) =
      match pat with
      | Pattern.Identifier { Name = name; IsMut = mut } ->
        result <- Map.add name (path, mut) result
      | Pattern.Object object ->
        for elem in object.Elems do
          match elem with
          | Type.KeyValuePat { Key = key; Value = value } ->
            walkPattern value (key :: path)
          | Type.ShorthandPat { Name = name; IsMut = mut } ->
            result <- Map.add name ((name :: path), mut) result
          | Type.RestPat rest ->
            // TODO: What should be the path for the rest pattern?
            // It should be multiple paths, one for each property in the pattern
            // We could use a "wildcard" path element to model this as long as
            // we only check paths with wildcards after checking those without
            printfn "TODO: getBindingPaths - ObjPatElem.RestPat"
      | Pattern.Tuple tuple ->
        for i, elem in tuple.Elems |> List.indexed do
          match elem with
          | Some elem -> walkPattern elem (string i :: path)
          | None -> ()
      | Pattern.Wildcard -> ()
      | Pattern.Literal literal -> ()
      | Pattern.Rest pattern ->
        // TODO: What should be the path for the rest pattern?
        // It should be multiple paths, one for each property in the pattern
        // We could use a "wildcard" path element to model this as long as
        // we only check paths with wildcards after checking those without
        printfn "TODO: getPatBindingPaths - Rest"
      | Pattern.Enum(_) -> failwith "TODO: getPatBindingPaths - Enum"

    walkPattern pat []
    result

  let getExprBindingPaths (env: Env) (expr: Expr) : BindingPaths =
    let mutable result: BindingPaths = Map.empty

    let rec walkExpr (expr: Expr) (path: list<string>) =
      match expr.Kind with
      | ExprKind.Identifier ident ->
        // TODO: lookup `ident` in the current environment
        match env.GetBinding ident with
        | Ok(_, mut) -> result <- Map.add ident (path, mut) result
        | Error _ -> failwith $"{ident} isn't in scope"
      | ExprKind.Tuple { Elems = elems } ->
        for i, elem in elems |> List.indexed do
          walkExpr elem (string i :: path)
      | ExprKind.Object { Elems = elems } ->
        for elem in elems do
          match elem with
          | ObjElem.Property(span, name, value) ->
            let name =
              match name with
              | Syntax.Ident s -> s
              | Syntax.String s -> s
              | Syntax.Number n -> string (n)
              | Computed expr -> failwith "TODO: handle computed property names"

            walkExpr value (name :: path)
          | Shorthand(span, name) ->
            match env.GetBinding name with
            | Ok(_, mut) -> result <- Map.add name ((name :: path), mut) result
            | Error _ -> failwith $"{name} isn't in scope"
          | Spread(span, value) ->
            // TODO: What should be the path for the spread expression?
            // It should be multiple paths, one for each property in the object
            // We could use a "wildcard" path element to model this as long as
            // we only check paths with wildcards after checking those without
            printfn "TODO: getExprBindingPaths - Spread"
      | _ -> ()

    walkExpr expr []
    result

  let checkMutability
    (patBindingPaths: BindingPaths)
    (exprBindingPaths: BindingPaths)
    : Result<option<list<list<string>>>, TypeError> =
    result {
      let mutable invariantPaths: list<list<string>> = []

      // let patBindingPaths = getPatBindingPaths pattern
      // let exprBindingPaths = getExprBindingPaths env init

      // iterate over all of the exprBindingPaths and find the corresponding
      // patBindingPath

      for KeyValue(exprName, (exprPath, exprMut)) in exprBindingPaths do
        for KeyValue(patName, (patPath, patMut)) in patBindingPaths do

          // Handles cases like
          // - `{a: {b: {c}}} = {a: {b}}`
          // - `{a: {b: {c}}} = obj`
          if patPath.Length > exprPath.Length then
            if List.take exprPath.Length patPath = exprPath then
              if patMut && not exprMut then
                return!
                  Error(
                    TypeError.SemanticError
                      $"immutable binding '{patName}' cannot alias '{exprName}'"
                  )
              else if patMut && exprMut then
                invariantPaths <- patPath :: invariantPaths
              else
                ()
          // Handles cases like
          // - `{a: {b}} = {a: {b: {c}}}`
          // - `obj = {a: {b: {c}}}`
          else if patPath = List.take patPath.Length exprPath then
            if patMut && not exprMut then
              return!
                Error(
                  TypeError.SemanticError
                    $"immutable binding '{patName}' cannot alias '{exprName}'"
                )
            else if patMut && exprMut then
              invariantPaths <- exprPath :: invariantPaths
            else
              ()

      return if invariantPaths.IsEmpty then None else Some invariantPaths
    }

  let findPathTails (name: string) (ips: list<list<string>>) =
    ips
    |> List.filter (fun path -> List.head path = name)
    |> List.map List.tail
    |> List.distinct

  /// Searches for paths that start with `name` and returns the tails of those.
  let tryFindPathTails (name: string) (ips: option<list<list<string>>>) =
    Option.map (findPathTails name) ips
