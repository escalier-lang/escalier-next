module Escalier.GraphQL

open Escalier.Data.Common
open Escalier.Data.Type

open GraphQLParser.AST

let tryFindObjType
  (schema: GraphQLDocument)
  (name: string)
  : Option<GraphQLObjectTypeDefinition> =
  let mutable result = None

  for def in schema.Definitions do
    if def.Kind = ASTNodeKind.ObjectTypeDefinition then
      let def = def :?> GraphQLObjectTypeDefinition

      if def.Name.StringValue = name then
        result <- Some def
      else
        ()

  result

let tryFindFragmentDef
  (doc: GraphQLDocument)
  (name: string)
  : Option<GraphQLFragmentDefinition> =
  let mutable result = None

  for def in doc.Definitions do
    if def.Kind = ASTNodeKind.FragmentDefinition then
      let def = def :?> GraphQLFragmentDefinition

      if def.FragmentName.Name.StringValue = name then
        result <- Some def
      else
        ()

  result

let tryFindQueryType
  (schema: GraphQLDocument)
  : Option<GraphQLObjectTypeDefinition> =
  let mutable result = None

  for def in schema.Definitions do
    if def.Kind = ASTNodeKind.SchemaDefinition then
      let def = def :?> GraphQLSchemaDefinition

      for opType in def.OperationTypes do
        if opType.Operation = OperationType.Query then
          result <- tryFindObjType schema opType.Type.Name.StringValue

    if def.Kind = ASTNodeKind.RootOperationTypeDefinition then
      let root = def :?> GraphQLRootOperationTypeDefinition

      if root.Operation = OperationType.Query then
        result <- tryFindObjType schema root.Type.Name.StringValue

  result

let rec getTypeForField
  (schema: GraphQLDocument)
  (doc: GraphQLDocument)
  (fieldDefType: GraphQLType)
  (field: GraphQLField)
  : Type =
  match fieldDefType.Kind with
  | ASTNodeKind.NamedType ->
    let namedTyped = fieldDefType :?> GraphQLNamedType

    match tryFindObjType schema namedTyped.Name.StringValue with
    | Some objType ->
      let elems = getObjectElems schema doc field.SelectionSet objType.Fields

      let objType =
        { Kind =
            TypeKind.Object
              { Elems = List.rev elems
                Extends = None
                Implements = None
                Exact = true
                Mutable = false
                Immutable = false
                Interface = false }
          Provenance = None }

      let nullType =
        { Kind = TypeKind.Literal Literal.Null
          Provenance = None }

      { Kind = TypeKind.Union [ objType; nullType ]
        Provenance = None }
    | None ->
      let t =
        match namedTyped.Name.StringValue with
        | "Boolean" ->
          { Kind = TypeKind.Primitive Primitive.Boolean
            Provenance = None }
        | "Int"
        | "Float" ->
          { Kind = TypeKind.Primitive Primitive.Number
            Provenance = None }
        | "String"
        | "ID" ->
          { Kind = TypeKind.Primitive Primitive.String
            Provenance = None }
        | _ -> failwith $"couldn't find object type {namedTyped.Name}"

      let nullType =
        { Kind = TypeKind.Literal Literal.Null
          Provenance = None }

      { Kind = TypeKind.Union [ t; nullType ]
        Provenance = None }
  | ASTNodeKind.NonNullType ->
    let nonNullType = fieldDefType :?> GraphQLNonNullType
    let t = getTypeForField schema doc nonNullType.Type field

    match t.Kind with
    | TypeKind.Union types ->
      let typesWithoutNull =
        types
        |> List.filter (fun t ->
          match t.Kind with
          | TypeKind.Literal Literal.Null -> false
          | _ -> true)

      match typesWithoutNull with
      | [] -> failwith "expected at least one type in the union"
      | [ t ] -> t
      | types ->
        { Kind = TypeKind.Union types
          Provenance = None }
    | _ -> t
  | ASTNodeKind.ListType ->
    let listType = fieldDefType :?> GraphQLListType
    let t = getTypeForField schema doc listType.Type field

    { Kind =
        TypeKind.TypeRef
          { Name = QualifiedIdent.Ident "Array"
            TypeArgs = Some [ t ]
            Mutable = false
            Scheme = None }
      Provenance = None }
  | _ -> failwith "unexpected field type"

and getObjectElems
  (schema: GraphQLDocument)
  (doc: GraphQLDocument)
  (ss: GraphQLSelectionSet)
  (fieldsDef: GraphQLFieldsDefinition)
  : list<ObjTypeElem> =
  let mutable fieldDefMap = Map.empty

  for fieldDef in fieldsDef do
    fieldDefMap <- fieldDefMap.Add(fieldDef.Name.StringValue, fieldDef)

  let mutable elems: list<ObjTypeElem> = []

  for s in ss.Selections do
    match s.Kind with
    | ASTNodeKind.Field ->
      let field = s :?> GraphQLField

      match fieldDefMap.TryFind field.Name.StringValue with
      | Some fieldDef ->
        let t = getTypeForField schema doc fieldDef.Type field

        elems <-
          ObjTypeElem.Property
            { Name = PropName.String field.Name.StringValue
              Type = t
              Optional = false
              Readonly = true }
          :: elems
      | None -> printfn $"couldn't find {field.Name}"
    | ASTNodeKind.FragmentSpread ->
      let spread = s :?> GraphQLFragmentSpread

      match tryFindFragmentDef doc spread.FragmentName.Name.StringValue with
      | Some fragDef ->
        match
          tryFindObjType schema fragDef.TypeCondition.Type.Name.StringValue
        with
        | Some objType ->
          let fragElems =
            getObjectElems schema doc fragDef.SelectionSet objType.Fields

          elems <- fragElems @ elems
        | None ->
          printfn
            $"couldn't find object type {spread.FragmentName.Name.StringValue} in schema"
      | None ->
        printfn
          $"couldn't find fragment {spread.FragmentName.Name.StringValue} in doc"
    | ASTNodeKind.InlineFragment -> failwith "TODO - InlineFragment"
    | _ -> failwith "unexpected selection"

  elems
