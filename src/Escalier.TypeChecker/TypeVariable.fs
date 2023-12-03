namespace Escalier.TypeChecker

open Escalier.Data.Type

module TypeVariable =
  let mutable nextVariableId = 0

  let makeVariable bound =
    let newVar =
      { Id = nextVariableId
        Bound = bound
        Instance = None }

    nextVariableId <- nextVariableId + 1

    { Kind = TypeKind.TypeVar(newVar)
      Provenance = None }
//
// /// Returns the currently defining instance of t.
// /// As a side effect, collapses the list of type instances. The function Prune
// /// is used whenever a type expression has to be inspected: it will always
// /// return a type expression which is either an uninstantiated type variable or
// /// a type operator; i.e. it will skip instantiated variables, and will
// /// prune them from expressions to remove long chains of instantiated variables.
// let rec prune
//   (unify: Env -> Type -> Type -> Result<unit, TypeError>)
//   (t: Type)
//   : Type =
//   match t.Kind with
//   | TypeKind.TypeVar({ Instance = Some(instance) } as v) ->
//     let newInstance = prune instance
//     v.Instance <- Some(newInstance)
//     newInstance
//   | _ -> t
//
// let rec bind (t1: Type) (t2: Type) =
//   let t1 = prune t1
//   let t2 = prune t2
//
//   result {
//     if t1.Kind <> t2.Kind then
//       if occursInType t1 t2 then
//         return! Error(TypeError.RecursiveUnification)
//
//       match t1.Kind with
//       | TypeKind.TypeVar(v) ->
//         // printfn "Binding %A to %A" t1 t2
//         if v.Bound <> None then
//           do! unify v.Bound t2
//         // return! Error(TypeError.TypeBoundMismatch(t1, t2))
//         v.Instance <- Some(t2)
//         return ()
//       | _ -> return! Error(TypeError.NotImplemented "bind error")
//   }
//
// and occursInType (v: Type) (t2: Type) : bool =
//   match (prune t2).Kind with
//   | pruned when pruned = v.Kind -> true
//   | TypeKind.TypeRef({ TypeArgs = typeArgs }) ->
//     match typeArgs with
//     | Some(typeArgs) -> occursIn v typeArgs
//     | None -> false
//   | _ -> false
//
// and occursIn (t: Type) (types: list<Type>) : bool =
//   List.exists (occursInType t) types
