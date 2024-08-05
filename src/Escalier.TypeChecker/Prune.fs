namespace Escalier.TypeChecker

open Escalier.Data.Type
open Escalier.Data.Common

module rec Prune =
  let simplifyRangeMath
    (op: string)
    (n: Number)
    (r: Range<Type>)
    : Range<Type> =
    let min = prune r.Min
    let max = prune r.Max

    match min.Kind, max.Kind with
    | TypeKind.Literal(Literal.Number min), TypeKind.Literal(Literal.Number max) ->

      let min, max =
        match op with
        | "+" -> min.Add(n), max.Add(n)
        | "-" -> min.Sub(n), max.Sub(n)
        | "*" -> min.Mul(n), max.Mul(n)
        | "/" -> min.Div(n), max.Div(n)
        | "%" -> min.Mod(n), max.Mod(n)
        | "**" -> min.Exp(n), max.Exp(n)
        | _ -> failwith $"TODO: simplifyRangeMath {r}"

      { Min =
          { Kind = TypeKind.Literal(Literal.Number min)
            Provenance = None }
        Max =
          { Kind = TypeKind.Literal(Literal.Number max)
            Provenance = None } }
    | _ -> r

  let simplify (t: Type) : Type =
    match t.Kind with
    | TypeKind.Binary(left, op, right) ->
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
      | TypeKind.Literal(Literal.Number n), TypeKind.Range range ->
        { Kind = TypeKind.Range(simplifyRangeMath op n range)
          Provenance = None }
      | TypeKind.Range range, TypeKind.Literal(Literal.Number n) ->
        { Kind = TypeKind.Range(simplifyRangeMath op n range)
          Provenance = None }
      | TypeKind.Range { Min = min1; Max = max1 },
        TypeKind.Range { Min = min2; Max = max2 } ->

        match min1.Kind, max1.Kind, min1.Kind, max1.Kind with
        | TypeKind.Literal(Literal.Number min1),
          TypeKind.Literal(Literal.Number max1),
          TypeKind.Literal(Literal.Number min2),
          TypeKind.Literal(Literal.Number max2) ->

          let min, max =
            match op with
            | "+" -> min1.Add(min2), max1.Add(max2)
            | "-" -> min1.Sub(min2), max1.Sub(max2)
            | "*" -> min1.Mul(min2), max1.Mul(max2)
            | "/" -> min1.Div(min2), max1.Div(max2)
            | "%" -> min1.Mod(min2), max1.Mod(max2)
            | "**" -> min1.Exp(min2), max1.Exp(max2)
            | _ -> failwith $"TODO: simplify {t}"

          let range =
            { Min =
                { Kind = TypeKind.Literal(Literal.Number min)
                  Provenance = None }
              Max =
                { Kind = TypeKind.Literal(Literal.Number max)
                  Provenance = None } }

          { Kind = TypeKind.Range range
            Provenance = None }
        | _ -> t
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
