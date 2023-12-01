namespace Escalier.Interop

open Escalier.Data.Type
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.Interop.TypeScript

// NOTES:
// - we need to track global decls and module decls separately
// - this is kind of tricky because module decls might reference global decls
// - we also need to create placeholder decls for everything in the module as
//   opposed to how we're handling scripts where we infer top-level decls one at
//   a time

module rec Infer =
  let inferTsType (env: Env) (t: TsType) : Type = failwith "TODO"

  let inferTsTypeAnn (env: Env) (ta: TsTypeAnn) : Type =
    inferTsType env ta.TypeAnn

  let inferPattern (env: Env) (p: Pat) : BindingAssump * Type =
    let mutable assump = BindingAssump([])

    let rec infer_pattern_rec (pat: Pat) : Type =
      match pat with
      | Pat.Ident id ->
        let t = TypeVariable.makeVariable None
        // TODO:
        let isMut = false
        // TODO: check if `name` already exists in `assump`
        assump <- assump.Add(id.Id.Name, (t, isMut))
        t
      | Pat.Array arrayPat -> failwith "TODO: infer_pattern_rec - Array"
      | Pat.Rest restPat -> failwith "TODO: infer_pattern_rec - Rest"
      | Pat.Object objectPat -> failwith "TODO: infer_pattern_rec - Object"
      | Pat.Assign assignPat -> failwith "TODO: infer_pattern_rec - Assign"
      | Pat.Invalid invalid -> failwith "TODO: infer_pattern_rec - Invalid"
      // TODO: remove Pat.Expr from Pat
      | Pat.Expr expr -> failwith "TODO: infer_pattern_rec - Expr"

    let t = infer_pattern_rec p
    (assump, t)

  let patToPattern (env: Env) (p: Pat) : Pattern =
    match p with
    | Pat.Ident ident -> Pattern.Identifier ident.Id.Name
    | Pat.Array arrayPat ->
      let elems = arrayPat.Elems |> List.map (Option.map <| (patToPattern env))
      Pattern.Tuple elems
    | Pat.Rest rest -> patToPattern env rest.Arg |> Pattern.Rest
    | Pat.Object objectPat ->
      let elems: list<ObjPatElem> =
        objectPat.Props
        |> List.map (fun prop ->
          match prop with
          | ObjectPatProp.KeyValue { Key = key; Value = value } ->
            let key =
              match key with
              | PropName.Ident id -> id.Name
              | PropName.Str str -> str.Value
              | PropName.Num num -> num.Value |> string
              | PropName.Computed computedPropName ->
                failwith "TODO: computed property name"

            ObjPatElem.KeyValuePat(key, patToPattern env value, None)
          | ObjectPatProp.Assign { Key = key; Value = _ } ->
            ObjPatElem.ShorthandPat(key.Name, None)
          | ObjectPatProp.Rest { Arg = arg } ->
            patToPattern env arg |> ObjPatElem.RestPat)

      Pattern.Object elems
    // TODO: add assign patterns to Escalier's AST
    | Pat.Assign assignPat -> failwith "TODO: patToPattern - Assign"
    | Pat.Invalid invalid -> failwith "TODO: patToPattern - Invalid"
    // TODO: remove Pat.Expr from Pat
    | Pat.Expr expr -> failwith "TODO: remove Pat.Expr from Expr"

  let inferFunction (env: Env) (f: Function) : Type =
    let paramList: list<FuncParam> =
      f.Params
      |> List.map (fun param ->
        let typeAnn =
          match param.TypeAnn with
          | Some(t) -> inferTsTypeAnn env t
          | None -> TypeVariable.makeVariable None

        let pat = patToPattern env param.Pat

        { Pattern = pat
          Type = typeAnn
          Optional = false })

    let ret =
      match f.ReturnType with
      | Some(retType) -> inferTsTypeAnn env retType
      | None -> failwith "Function decl has no return type"

    makeFunctionType None paramList ret

  let inferDecl (env: Env) (decl: Decl) : Env =
    let mutable newEnv = env

    match decl with
    | Decl.Class classDecl -> failwith "TODO: classDecl"
    | Decl.Fn fnDecl ->
      let t = inferFunction env fnDecl.Fn
      let name = fnDecl.Id.Name
      let isMut = false
      newEnv <- env.AddValue name (t, isMut)
    | Decl.Var varDecl ->
      for decl in varDecl.Decls do
        let assumps, _ = inferPattern env decl.Id

        for Operators.KeyValue(name, binding) in assumps do
          newEnv <- env.AddValue name binding

    | Decl.Using usingDecl -> failwith "TODO: usingDecl"
    | Decl.TsInterface tsInterfaceDecl ->
      // TODO: handle interface merging
      failwith "TODO: tsInterfaceDecl"
    | Decl.TsTypeAlias decl ->
      let typeParams = None
      let t = inferTsType env decl.TypeAnn
      let scheme = { TypeParams = typeParams; Type = t }

      // TODO: if decl.Global is true, add to the global env
      newEnv <- env.AddScheme decl.Id.Name scheme
    | Decl.TsEnum tsEnumDecl -> failwith "TODO: tsEnumDecl"
    | Decl.TsModule tsModuleDecl -> failwith "TODO: tsModuleDecl"

    newEnv

  let inferStmt (env: Env) (stmt: Stmt) : Env =
    match stmt with
    | Stmt.Decl decl -> inferDecl env decl
    | _ -> env // .d.ts files shouldn't have any other statement kinds

  let inferModuleDecl (env: Env) (decl: ModuleDecl) : Env =
    match decl with
    | ModuleDecl.Import importDecl ->
      failwith "TODO: inferModuleDecl - importDecl"
    | ModuleDecl.ExportDecl exportDecl ->
      failwith "TODO: inferModuleDecl - exportDecl"
    | ModuleDecl.ExportNamed namedExport ->
      failwith "TODO: inferModuleDecl - namedExport"
    | ModuleDecl.ExportDefaultDecl exportDefaultDecl ->
      failwith "TODO: inferModuleDecl - exportDefaultDecl"
    | ModuleDecl.ExportDefaultExpr exportDefaultExpr ->
      failwith "TODO: inferModuleDecl - exportDefaultExpr"
    | ModuleDecl.ExportAll exportAll ->
      failwith "TODO: inferModuleDecl - exportAll"
    | ModuleDecl.TsImportEquals tsImportEqualsDecl ->
      failwith "TODO: inferModuleDecl - tsImportEqualsDecl"
    | ModuleDecl.TsExportAssignment tsExportAssignment ->
      failwith "TODO: inferModuleDecl - tsExportAssignment"
    | ModuleDecl.TsNamespaceExport tsNamespaceExportDecl ->
      failwith "TODO: inferModuleDecl - tsNamespaceExportDecl"

  let inferModuleItem (env: Env) (item: ModuleItem) : Env =
    match item with
    | ModuleItem.Stmt stmt -> inferStmt env stmt
    | ModuleItem.ModuleDecl decl -> env

  let inferModule (env: Env) (m: Module) : Env =
    let mutable newEnv = env

    for item in m.Body do
      newEnv <- inferModuleItem newEnv item

    newEnv
