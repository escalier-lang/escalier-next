namespace Escalier.TypeChecker

open Escalier.Data.Type
open Escalier.Data.Common

module rec Prune =
  let simplify (t: Type) : Type =
    match t.Kind with
    | TypeKind.Binary(left, op, right) ->
      // printfn $"simplify binary: t = {t}"
      let left = prune left
      let right = prune right

      match left.Kind, right.Kind with
      | TypeKind.Literal(Literal.Number n1), TypeKind.Literal(Literal.Number n2) ->
        let n1 = float n1
        let n2 = float n2

        let result =
          match op with
          | "+" -> n1 + n2
          | "-" -> n1 - n2
          | "*" -> n1 * n2
          | "/" -> n1 / n2
          | "%" -> n1 % n2
          | "**" -> n1 ** n2
          | _ -> failwith "TODO: simplify binary"

        { Kind = TypeKind.Literal(Literal.Number result)
          Provenance = None }
      // TODO: Check `op` when collapsing binary expressions involving numbers
      | _, TypeKind.Primitive Primitive.Number -> right
      | TypeKind.Primitive Primitive.Number, _ -> left
      | TypeKind.Literal(Literal.String s1), TypeKind.Literal(Literal.String s2) ->

        let result =
          match op with
          | "++" -> s1 + s2
          | _ -> failwith "TODO: simplify binary"

        { Kind = TypeKind.Literal(Literal.String result)
          Provenance = None }
      // TODO: Check `op` when collapsing binary expressions involving strings
      | _, TypeKind.Primitive Primitive.String -> right
      | TypeKind.Primitive Primitive.String, _ -> left
      | _ -> t
    | _ -> t

  /// Returns the currently defining instance of t.
  /// As a side effect, collapses the list of type instances. The function Prune
  /// is used whenever a type expression has to be inspected: it will always
  /// return a type expression which is either an uninstantiated type variable or
  /// a type operator; i.e. it will skip instantiated variables, and will
  /// prune them from expressions to remove long chains of instantiated variables.
  let rec prune (t: Type) : Type =
    match t.Kind with
    | TypeKind.TypeVar({ Instance = Some(instance) } as v) ->
      let newInstance = prune instance
      v.Instance <- Some(newInstance)
      newInstance
    | _ -> simplify t
