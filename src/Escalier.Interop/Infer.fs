namespace Escalier.Interop

open Escalier.Data.Type
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

  let inferPat (env: Env) (p: Pat) : option<Type> =
    match p with
    | Pat.Ident ident ->

      failwith "TODO: inferPat - Ident"
    | Pat.Array arrayPat -> failwith "todo"
    | Pat.Rest restPat -> failwith "todo"
    | Pat.Object objectPat -> failwith "todo"
    | Pat.Assign assignPat -> failwith "todo"
    | Pat.Invalid invalid -> failwith "todo"
    | Pat.Expr expr -> failwith "TODO: remove Pat.Expr from Expr"

  let inferFunction (env: Env) (f: Function) : Type =
    let paramList: list<FuncParam> =
      f.Params
      |> List.map (fun param ->
        // { Pattern = patternToPattern param.Pattern
        //   Type = paramType
        //   Optional = false }

        failwith "todo")

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
      // let ret = fnDecl.Fn.
      failwith "TODO: fnDecl"
    | Decl.Var varDecl -> failwith "TODO: varDecl"
    | Decl.Using usingDecl -> failwith "TODO: usingDecl"
    | Decl.TsInterface tsInterfaceDecl -> failwith "TODO: tsInterfaceDecl"
    | Decl.TsTypeAlias tsTypeAliasDecl -> failwith "TODO: tsTypeAliasDecl"
    | Decl.TsEnum tsEnumDecl -> failwith "TODO: tsEnumDecl"
    | Decl.TsModule tsModuleDecl -> failwith "TODO: tsModuleDecl"

  let inferStmt (env: Env) (stmt: Stmt) : Env =
    let mutable newEnv = env

    match stmt with
    | Stmt.Decl decl -> newEnv
    | _ -> newEnv // .d.ts files shouldn't have any other statement kinds

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
