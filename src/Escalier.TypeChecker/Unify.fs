namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling
open Escalier.Data.Type
open Escalier.Data.Syntax
open Escalier.TypeChecker.Errors

module Unify =
  let rec unify (t1: Type) (t2: Type) : Result<unit, TypeError> =
    result {
      match t1.kind, t2.kind with
      | TypeKind.Primitive(p1), TypeKind.Primitive(p2) ->
        match p1, p2 with
        | Primitive.Number, Primitive.Number -> return ()
        | Primitive.String, Primitive.String -> return ()
        | Primitive.Boolean, Primitive.Boolean -> return ()
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Literal(lit), TypeKind.Primitive(prim) ->
        match lit, prim with
        | Literal.Number _, Primitive.Number -> return ()
        | Literal.String _, Primitive.String -> return ()
        | Literal.Boolean _, Primitive.Boolean -> return ()
        | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Keyword(kw1), TypeKind.Keyword(kw2) ->
        if kw1 = kw2 then
          return ()
        else
          return! Error(TypeError.TypeMismatch(t1, t2))
      | _, TypeKind.Keyword(KeywordType.Unknown) -> return ()
      | TypeKind.Union(types), _ ->
        for t in types do
          do! unify t t2

        return ()
      | _, TypeKind.Union(types) ->
        // If t1 is a subtype of any of the types in the union, then it is a
        // subtype of the union.
        for t in types do
          if Result.isOk (unify t1 t) then return () else ()

        return! Error(TypeError.TypeMismatch(t1, t2))
      | TypeKind.Tuple(tt1), TypeKind.Tuple(tt2) ->
        if tt1.Length < tt2.Length then
          // TODO: check for rest patterns
          return! Error(TypeMismatch(t1, t2))
        else
          ()

        for (p, q) in List.zip tt1 tt2 do
          // TODO: check for rest patterns
          do! unify p q
      | TypeKind.Tuple tt, TypeKind.Array at ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Array at, TypeKind.Tuple tt ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Rest rest, TypeKind.Array _ -> return! unify rest t2
      | TypeKind.Rest rest, TypeKind.Tuple _ -> return! unify rest t2
      | TypeKind.Array _, TypeKind.Rest rest -> return! unify t1 rest
      | TypeKind.Tuple _, TypeKind.Rest rest -> return! unify t1 rest
      | TypeKind.Object _, TypeKind.Object _ ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Intersection _, TypeKind.Object _ ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Object _, TypeKind.Intersection _ ->
        return! Error(TypeError.NotImplemented)
      | TypeKind.Function fn1, TypeKind.Function fn2 ->
        return! Error(TypeError.NotImplemented)
      | _ -> return! Error(TypeError.TypeMismatch(t1, t2))
    }
