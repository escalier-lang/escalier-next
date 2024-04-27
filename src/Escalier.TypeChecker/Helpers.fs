module rec Escalier.TypeChecker.Helpers

open Escalier.Data.Common
open Escalier.Data.Type

open Poly

let rec generalizeFunctionsInType (t: Type) : Type =
  match (prune t).Kind with
  | TypeKind.Function f ->
    { t with
        Kind = generalizeFunc f |> TypeKind.Function }
  | TypeKind.Object({ Elems = elems } as objectKind) ->
    let elems =
      elems
      |> List.map (fun elem ->
        match elem with
        | ObjTypeElem.Callable fn -> ObjTypeElem.Callable(generalizeFunc fn)
        | ObjTypeElem.Constructor fn ->
          ObjTypeElem.Constructor(generalizeFunc fn)
        | ObjTypeElem.Method(name, fn) ->
          ObjTypeElem.Method(name, (generalizeFunc fn))
        | ObjTypeElem.Getter(name, fn) ->
          ObjTypeElem.Getter(name, (generalizeFunc fn))
        | ObjTypeElem.Setter(name, fn) ->
          ObjTypeElem.Setter(name, (generalizeFunc fn))
        | ObjTypeElem.Property { Name = name
                                 Optional = optional
                                 Readonly = readonly
                                 Type = t } ->
          ObjTypeElem.Property
            { Name = name
              Optional = optional
              Readonly = readonly
              Type = generalizeFunctionsInType t }
        | _ -> elem)

    { t with
        Kind = TypeKind.Object { objectKind with Elems = elems } }
  | TypeKind.Tuple({ Elems = elems } as tupleKind) ->
    let elems = elems |> List.map generalizeFunctionsInType

    { t with
        Kind = TypeKind.Tuple { tupleKind with Elems = elems } }
  | _ -> t

let generalizeBindings (bindings: Map<string, Binding>) : Map<string, Binding> =
  let mutable newBindings = Map.empty

  for KeyValue(name, (t, isMut)) in bindings do
    let t = generalizeFunctionsInType t
    newBindings <- newBindings.Add(name, (t, isMut))

  newBindings
