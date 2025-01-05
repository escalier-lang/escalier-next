namespace Escalier.TypeChecker

open Escalier.Data.Type
open Escalier.Data.Common

module rec Prune =
  let simplify (t: Type) : Type =
    match t.Kind with
    | TypeKind.Binary { Op = op; Left = left; Right = right } ->
      let left = prune left
      let right = prune right

      match left.Kind, right.Kind with
      | TypeKind.Literal(Literal.Number n1), TypeKind.Literal(Literal.Number n2) ->
        let result =
          match op with
          | "+" -> n1.Add(n2) |> Literal.Number
          | "-" -> n1.Sub(n2) |> Literal.Number
          | "*" -> n1.Mul(n2) |> Literal.Number
          | "/" -> n1.Div(n2) |> Literal.Number
          | "%" -> n1.Mod(n2) |> Literal.Number
          | "**" -> n1.Exp(n2) |> Literal.Number
          | ">" -> n1.GreaterThan(n2)
          | ">=" -> n1.GreaterThanOrEqual(n2)
          | "<" -> n1.LessThan(n2)
          | "<=" -> n1.LessThanOrEqual(n2)
          | _ -> failwith "TODO: simplify binary"

        { Kind = TypeKind.Literal result
          Provenance = None }
      | TypeKind.Primitive Primitive.Number, _
      | _, TypeKind.Primitive Primitive.Number ->
        let result =
          match op with
          | "+" -> Primitive.Number
          | "-" -> Primitive.Number
          | "*" -> Primitive.Number
          | "/" -> Primitive.Number
          | "%" -> Primitive.Number
          | "**" -> Primitive.Number
          | ">" -> Primitive.Boolean
          | ">=" -> Primitive.Boolean
          | "<" -> Primitive.Boolean
          | "<=" -> Primitive.Boolean
          | _ -> failwith "TODO: simplify binary"

        { Kind = TypeKind.Primitive result
          Provenance = None }
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
