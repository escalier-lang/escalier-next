namespace Escalier.Codegen

open Escalier.Interop
open Escalier.Interop.TypeScript

module rec Printer =
  type PrintCtx = { Precedence: int; Indent: int }

  let getBinaryPrecedence (op: BinOp) : int =
    match op with
    | BinOp.Exp -> 14
    | BinOp.Mul -> 13
    | BinOp.Div -> 13
    | BinOp.Mod -> 13
    | BinOp.Add -> 12
    | BinOp.Sub -> 12
    | BinOp.LShift -> 11
    | BinOp.RShift -> 11
    | BinOp.ZeroFillRShift -> 11
    | BinOp.Lt -> 10
    | BinOp.LtEq -> 10
    | BinOp.Gt -> 10
    | BinOp.GtEq -> 10
    | BinOp.In -> 10
    | BinOp.InstanceOf -> 10
    | BinOp.EqEq -> 9
    | BinOp.NotEq -> 9
    | BinOp.EqEqEq -> 9
    | BinOp.NotEqEq -> 9
    | BinOp.BitAnd -> 8
    | BinOp.BitXor -> 7
    | BinOp.BitOr -> 6
    | BinOp.LogicalAnd -> 3
    | BinOp.LogicalOr -> 2
    | BinOp.NullishCoalescing -> 2

  let printLit (lit: Lit) : string =
    match lit with
    | Lit.Str { Value = value } -> $"\"{value}\"" // TODO: escape special characters
    | Lit.Num { Value = value } -> value.ToString()
    | Lit.Bool { Value = value } -> if value then "true" else "false"
    | Lit.Regex { Exp = exp; Flags = flags } -> $"/{exp}/{flags}"
    | Lit.Null _ -> "null"
    | Lit.JSXText jsxText -> failwith "todo"

  let printIdent (ident: Ident) : string = ident.Name

  let rec printExpr (ctx: PrintCtx) (e: TypeScript.Expr) : string =

    match e with
    | Expr.Ident { Name = name } -> name
    | Expr.Lit lit -> printLit lit
    | Expr.This _ -> "this"
    | Expr.Array { Elements = elements } ->
      let elements =
        List.map
          (fun (e: option<ExprOrSpread>) ->
            match e with
            | Some(e) -> printExpr { ctx with Precedence = 0 } e.Expr
            | None -> "")
          elements

        |> String.concat ", "

      $"[{elements}]"
    // TODO: support shorthand syntax
    | Expr.Object { Properties = props } ->

      let props =
        props
        |> List.map
          (fun
               { Key = key
                 Value = value
                 Kind = _kind } ->

            let key =
              match key with
              | PropertyKey.Lit lit -> printLit lit // wrap this?
              | PropertyKey.Ident id -> id.Name

            let value = printExpr { ctx with Precedence = 0 } value

            // TODO: handle getter/setter kinds
            $"{key}: {value}")
        |> String.concat ", "

      $"{{{props}}}"
    | Expr.Fn { Id = id
                Fn = { Params = ps; Body = body } } ->
      let id =
        match id with
        | Some(id) -> id.Name
        | None -> ""

      let ctx = { ctx with Precedence = 0 }

      let ps =
        ps |> List.map (fun p -> printPattern ctx p.Pat) |> String.concat ", "

      match body with
      | Some(body) -> $"function {id}({ps}) {printBlock ctx body}"
      | None -> $"function {id}({ps}) {{}}"
    | Expr.Arrow { Params = ps; Body = body } ->
      let ctx = { ctx with Precedence = 0 }
      let ps = ps |> List.map (printPattern ctx) |> String.concat ", "

      $"({ps}) => {printBlock ctx body}"
    | Expr.Unary { Operator = op
                   Prefix = _prefix
                   Argument = arg } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 15

      let op =
        match op with
        | UnaryOperator.Minus -> "-"
        | UnaryOperator.Plus -> "+"
        | UnaryOperator.Not -> "!"
        | UnaryOperator.BitwiseNot -> "~"
        | UnaryOperator.Typeof -> "typeof "
        | UnaryOperator.Void -> "void "
        | UnaryOperator.Delete -> "delete "
        | UnaryOperator.Await -> "await "

      let arg = printExpr { ctx with Precedence = innerPrec } arg
      let expr = $"{op}{arg}"

      if innerPrec < outerPrec then $"({expr})" else expr

    | Expr.Update { Operator = op
                    Argument = arg
                    Prefix = prefix } ->

      let outerPrec = ctx.Precedence
      let innerPrec = if prefix then 15 else 16

      let op =
        match op with
        | Decrement -> "--"
        | Increment -> "++"

      let arg = printExpr { ctx with Precedence = innerPrec } arg
      let expr = if prefix then $"{op}{arg}" else $"{arg}{op}"

      if innerPrec < outerPrec then $"({expr})" else expr
    | Expr.Bin { Operator = op
                 Left = left
                 Right = right } ->

      let outerPrec = ctx.Precedence
      let innerPrec = getBinaryPrecedence op

      let op =
        match op with
        | BinOp.EqEq -> "=="
        | BinOp.NotEq -> "!="
        | BinOp.EqEqEq -> "==="
        | BinOp.NotEqEq -> "!=="
        | BinOp.Lt -> "<"
        | BinOp.LtEq -> "<="
        | BinOp.Gt -> ">"
        | BinOp.GtEq -> ">="
        | BinOp.LShift -> "<<"
        | BinOp.RShift -> ">>"
        | BinOp.ZeroFillRShift -> ">>>"
        | BinOp.Add -> "+"
        | BinOp.Sub -> "-"
        | BinOp.Mul -> "*"
        | BinOp.Div -> "/"
        | BinOp.Mod -> "%"
        | BinOp.BitAnd -> "&"
        | BinOp.BitOr -> "|"
        | BinOp.BitXor -> "^"
        | BinOp.In -> "in"
        | BinOp.InstanceOf -> "instanceof"
        | BinOp.LogicalOr -> "||"
        | BinOp.LogicalAnd -> "&&"
        | BinOp.Exp -> "**"
        | BinOp.NullishCoalescing -> "??"

      let left = printExpr { ctx with Precedence = innerPrec } left
      let right = printExpr { ctx with Precedence = innerPrec } right
      let expr = $"{left} {op} {right}"

      if innerPrec < outerPrec then $"({expr})" else expr

    | Expr.Assign { Operator = op
                    Left = left
                    Right = right } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 2

      let op =
        match op with
        | AssignOp.Assign -> "="
        | AssignOp.PlusAssign -> "+="
        | AssignOp.MinusAssign -> "-="
        | AssignOp.MultiplyAssign -> "*="
        | AssignOp.DivideAssign -> "/="
        | AssignOp.ModuloAssign -> "%="
        | AssignOp.LeftShiftAssign -> "<<="
        | AssignOp.RightShiftAssign -> ">>="
        | AssignOp.UnsignedRightShiftAssign -> ">>>="
        | AssignOp.BitwiseAndAssign -> "&="
        | AssignOp.BitwiseOrAssign -> "|="
        | AssignOp.BitwiseXorAssign -> "^="

      let left = printExpr { ctx with Precedence = innerPrec } left
      let right = printExpr { ctx with Precedence = innerPrec } right
      let expr = $"{left} {op} {right}"

      if innerPrec < outerPrec then $"({expr})" else expr

    | Expr.Member { Object = obj
                    Property = prop
                    Computed = computed } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 18

      let obj = printExpr { ctx with Precedence = innerPrec } obj
      let prop = printExpr { ctx with Precedence = innerPrec } prop
      let expr = if computed then $"{obj}[{prop}]" else $"{obj}.{prop}"

      if innerPrec < outerPrec then $"({expr})" else expr
    | Expr.Cond { Test = test
                  Consequent = cons
                  Alternate = alt } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 3

      let test = printExpr { ctx with Precedence = innerPrec } test
      let cons = printExpr { ctx with Precedence = innerPrec } cons
      let alt = printExpr { ctx with Precedence = innerPrec } alt

      let expr = $"{test} ? {cons} : {alt}"

      if innerPrec < outerPrec then $"({expr}" else expr

    | Expr.Call { Callee = callee; Arguments = args } ->
      let outerPrec = ctx.Precedence
      let innerPrec = 18

      let callee = printExpr { ctx with Precedence = innerPrec } callee

      let args =
        args
        |> List.map (printExpr { ctx with Precedence = innerPrec })
        |> String.concat ", "

      let expr = $"{callee}({args})"

      if innerPrec < outerPrec then $"({expr}" else expr
    | Expr.New { Callee = callee; Arguments = args } ->
      let outerPrec = ctx.Precedence
      let innerPrec = 18

      let callee = printExpr { ctx with Precedence = innerPrec } callee

      let args =
        args
        |> List.map (printExpr { ctx with Precedence = innerPrec })
        |> String.concat ", "

      let expr = $"{callee}({args})"

      if innerPrec < outerPrec then $"new ({expr}" else expr
    | Expr.Seq { Exprs = exprs } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 1

      let exprs =
        exprs
        |> List.map (printExpr { ctx with Precedence = innerPrec })
        |> String.concat ", "

      if innerPrec < outerPrec then $"({exprs})" else exprs
    | Expr.SuperProp _ -> failwith "TODO: printExpr - SuperProp"
    | Expr.Tpl _ -> failwith "TODO: printExpr - Tpl"
    | Expr.TaggedTpl _ -> failwith "TODO: printExpr - TaggedTpl"
    | Expr.Class _ -> failwith "TODO: printExpr - Class"
    | Expr.Yield _ -> failwith "TODO: printExpr - Yield"
    | Expr.MetaProp _ -> failwith "TODO: printExpr - MetaProp"
    | Expr.Await { Arg = arg } ->
      let outerPrec = ctx.Precedence
      let innerPrec = 14 // prefix operators
      let arg = printExpr { ctx with Precedence = innerPrec } arg
      let expr = $"await {arg}"
      if innerPrec < outerPrec then $"({expr})" else expr
    | Expr.Paren _ -> failwith "TODO: printExpr - Paren"
    | Expr.JSXMember _ -> failwith "TODO: printExpr - JSXMember"
    | Expr.JSXNamespacedName _ -> failwith "TOOD: printExpr - JSXNamespacedName"
    | Expr.JSXEmpty _ -> failwith "TODO: printExpr - JSXEmpty"
    | Expr.JSXElement _ -> failwith "TODO: printExpr - JSXElement"
    | Expr.JSXFragment _ -> failwith "TODO: printExpr - JSXFragment"
    | Expr.TsTypeAssertion _ -> failwith "TODO: printExpr - TsTypeAssertion"
    | Expr.TsConstAssertion _ -> failwith "TODO: printExpr - TsConstAssertion"
    | Expr.TsNonNull _ -> failwith "TODO: printExpr - TsNonNull"
    | Expr.TsAs _ -> failwith "TODO: printExpr - TsAs"
    | Expr.TsInstantiation _ -> failwith "TODO: printExpr - TsInstantiation"
    | Expr.TsSatisfies _ -> failwith "TODO: printExpr - TsSatisfies"
    | Expr.PrivateName _ -> failwith "TODO: printExpr - PrivateName"
    | Expr.OptChain _ -> failwith "TODO: printExpr - OptChain"
    | Expr.Invalid _ -> failwith "TODO: printExpr - Invalid"

  let printStmt (ctx: PrintCtx) (stmt: TypeScript.Stmt) : string =
    match stmt with
    | Stmt.Block block -> printBlock ctx block
    | Stmt.Expr { Expr = expr } ->
      let ctx = { ctx with Precedence = 0 }
      $"{printExpr ctx expr};"
    | Stmt.Empty _ -> ";"
    | Stmt.Debugger _ -> "debugger;"
    | Stmt.Return { Argument = arg } ->
      let ctx = { ctx with Precedence = 0 }

      match arg with
      | Some(arg) -> $"return {printExpr ctx arg};"
      | None -> "return;"
    | Stmt.Labeled { Label = label; Body = body } ->
      let label = label.Name
      let body = printStmt ctx body
      $"{label}: {body}"
    | Stmt.Break { Label = label } ->
      match label with
      | Some(label) -> $"break {label.Name};"
      | None -> "break;"
    | Stmt.Continue { Label = label } ->
      match label with
      | Some(label) -> $"continue {label.Name};"
      | None -> "continue;"
    | Stmt.If { Test = test
                Consequent = cons
                Alternate = alt } ->
      let ctx = { ctx with Precedence = 0 }
      let test = printExpr ctx test
      let cons = printStmt ctx cons

      match alt with
      | Some(alt) -> $"if ({test}) {cons} else {printStmt ctx alt}"
      | None -> $"if ({test}) {cons}"
    | Stmt.Switch { Discriminant = disc; Cases = cases } ->
      let ctx = { ctx with Precedence = 0 }
      let disc = printExpr ctx disc

      let cases =
        cases
        |> List.map (fun case ->
          let test =
            match case.Test with
            | Some(test) -> $"case {printExpr ctx test}:"
            | None -> "default:"

          let cons =
            match case.Consequent with
            | [ stmt ] -> printStmt ctx stmt
            | stmts -> stmts |> List.map (printStmt ctx) |> String.concat "\n"

          $"{test} {cons}")
        |> String.concat "\n"

      $"switch ({disc}) {{\n{cases}\n}}"
    | Stmt.Throw { Argument = arg } ->
      let ctx = { ctx with Precedence = 0 }
      let arg = printExpr ctx arg
      $"throw {arg};"
    | Stmt.Try { Block = block
                 Handler = handler
                 Finalizer = finalizer } ->
      let ctx = { ctx with Precedence = 0 }

      let block = block.Body |> List.map (printStmt ctx) |> String.concat "\n"
      let block = $"try {{\n{block}\n}}"

      let handler =
        match handler with
        | Some(handler) ->
          let param = printPattern ctx handler.Param

          let body =
            handler.Body.Body |> List.map (printStmt ctx) |> String.concat "\n"

          $"catch ({param}) {{\n{body}\n}}"
        | None -> ""

      let finalizer =
        match finalizer with
        | Some(finalizer) ->
          let body =
            finalizer.Body |> List.map (printStmt ctx) |> String.concat "\n"

          $"finally {{\n{body}\n}}"
        | None -> ""

      $"{block}{handler}{finalizer}"
    | Stmt.While { Test = test; Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      let test = printExpr ctx test
      let body = printStmt ctx body

      $"while ({test}) {body}"
    | Stmt.DoWhile { Test = test; Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      let test = printExpr ctx test
      let body = printStmt ctx body

      $"do {body} while ({test});"
    | Stmt.For { Init = init
                 Test = test
                 Update = update
                 Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      let init =
        match init with
        | Some(ForInit.Variable decl) ->
          printStmt ctx (Stmt.Decl(Decl.Var decl))
        | Some(ForInit.Expr expr) -> printExpr ctx expr
        | None -> ""

      let test =
        match test with
        | Some(test) -> printExpr ctx test
        | None -> ""

      let update =
        match update with
        | Some(update) -> printExpr ctx update
        | None -> ""

      let body = printStmt ctx body

      $"for ({init}; {test}; {update}) {body}"
    | Stmt.ForIn { Left = left
                   Right = right
                   Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      let left =
        match left with
        | ForHead.VarDecl decl -> printStmt ctx (Stmt.Decl(Decl.Var decl))
        | ForHead.Pat p -> printPattern ctx p
        | ForHead.UsingDecl decl -> failwith "TODO"

      let right = printExpr ctx right
      let body = printStmt ctx body

      $"for ({left} in {right}) {body}"
    | Stmt.Decl decl ->
      let ctx = { ctx with Precedence = 0 }

      match decl with
      | Decl.Fn { Id = id
                  Fn = { Params = ps; Body = body } } ->
        let id = id.Name

        let ps =
          ps |> List.map (fun p -> printPattern ctx p.Pat) |> String.concat ", "

        match body with
        | Some(body) ->
          let body = body.Body |> List.map (printStmt ctx) |> String.concat "\n"
          $"function {id}({ps}) {{\n{body}\n}}"
        | None -> $"function {id}({ps}) {{}}"
      | Decl.Var { Decls = decls
                   Declare = declare
                   Kind = kind } ->
        let decls =
          List.map
            (fun { VarDeclarator.Id = id; Init = init } ->
              let id = printPattern ctx id

              match init with
              | Some(init) -> $"{id} = {printExpr ctx init}"
              | None -> id)
            decls
          |> String.concat ", "

        let declare = if declare then "declare " else ""

        let kind =
          match kind with
          | VariableDeclarationKind.Var -> "var"
          | VariableDeclarationKind.Let -> "let"
          | VariableDeclarationKind.Const -> "const"

        $"{declare}{kind} {decls};"
      | Decl.Class(_) -> failwith "TODO: printStmt - Class"
      | Decl.Using(_) -> failwith "TODO: printStmt - Using"
      | Decl.TsInterface(_) -> failwith "TODO: printStmt - TsInterface"
      | Decl.TsTypeAlias(_) -> failwith "TODO: printStmt - TsTypeAlias"
      | Decl.TsEnum(_) -> failwith "TODO: printStmt - TsEnum"
      | Decl.TsModule(_) -> failwith "TODO: printStmt - TsModule"
    | Stmt.ForOf(_) -> failwith "TODO: printStmt - ForOf"

  let printPattern (ctx: PrintCtx) (p: Pat) : string =
    match p with
    | Pat.Ident { Id = id } -> id.Name
    | Pat.Array { Elems = elems } ->
      let elems =
        elems
        |> List.map (fun elem ->
          match elem with
          | None -> " "
          | Some pat -> printPattern ctx pat)
        |> String.concat ", "

      $"[{elems}]"
    | Pat.Rest { Arg = arg } ->
      let arg = printPattern ctx arg
      $"...{arg}"
    | Pat.Object { Props = props } ->
      let props =
        props
        |> List.map (fun prop ->
          match prop with
          | ObjectPatProp.Rest { Arg = arg } ->
            let arg = printPattern ctx arg
            $"...{arg}"
          | ObjectPatProp.Assign { Key = key; Value = _ } -> key.Name
          | ObjectPatProp.KeyValue { Key = key; Value = value } ->
            let key =
              match key with
              | PropName.Ident id -> id.Name
              | PropName.Str { Value = value } -> $"\"{value}\""
              | PropName.Num { Value = value } -> $"{value}"
              | Computed { Expr = expr } ->
                let expr = printExpr ctx expr
                $"[{expr}]"

            let value = printPattern ctx value
            $"{key}: {value}")
        |> String.concat ", "

      $"{{{props}}}"
    | Pat.Assign assignPat -> failwith "TODO: printPattern - Assign"
    | Pat.Invalid invalid -> failwith "TODO: printPattern - Invalid"

  let printBlock (ctx: PrintCtx) (block: BlockStmt) =
    let oldIdent = String.replicate ctx.Indent " "
    let ctx = { ctx with Indent = ctx.Indent + 2 }
    let ident = String.replicate ctx.Indent " "

    let body =
      block.Body
      |> List.map (fun stmt -> ident + printStmt ctx stmt)
      |> String.concat "\n"

    $"{{\n{body}\n{oldIdent}}}"

  let printModule (ctx: PrintCtx) (m: Module) : string =

    let body =
      m.Body
      |> List.map (fun stmt -> printModuleItem ctx stmt)
      |> String.concat "\n"

    body

  let printModuleItem (ctx: PrintCtx) (mi: ModuleItem) : string =
    match mi with
    | ModuleItem.Stmt stmt -> printStmt ctx stmt
    | ModuleItem.ModuleDecl decl -> printModuleDecl ctx decl

  let printModuleDecl (ctx: PrintCtx) (decl: ModuleDecl) : string =
    match decl with
    | ModuleDecl.Import importDecl ->
      let mutable hasNamedSpecifiers = false

      let specifiers =
        importDecl.Specifiers
        |> List.map (fun spec ->
          match spec with
          | ImportSpecifier.Named { Local = local; Imported = imported } ->
            hasNamedSpecifiers <- true
            let local = printIdent local

            match imported with
            | None -> $"{local}"
            | Some imported ->
              let imported =
                match imported with
                | ModuleExportName.Ident ident -> printIdent ident
                | ModuleExportName.Str str -> str.Value

              $"{imported} as {local}"
          | ImportSpecifier.Default importDefaultSpecifier ->
            failwith "todo - default import"
          | ImportSpecifier.Namespace { Local = local } ->
            let local = printIdent local
            $"* as {local}")
        |> String.concat ", "

      let source = importDecl.Src.Value

      match hasNamedSpecifiers with
      | true -> $"import {{{specifiers}}} from \"{source}\""
      | false -> $"import {specifiers} from \"{source}\""
    | ModuleDecl.ExportDecl { Decl = decl; Loc = loc } ->
      $"export {printDecl ctx decl}"
    | ModuleDecl.ExportNamed namedExport ->
      failwith "TODO: printModuleDecl - ExportNamed"
    | ModuleDecl.ExportDefaultDecl exportDefaultDecl ->
      failwith "TODO: printModuleDecl - ExportDefaultDecl"
    | ModuleDecl.ExportDefaultExpr exportDefaultExpr ->
      failwith "TODO: printModuleDecl - ExportDefaultExpr"
    | ModuleDecl.ExportAll exportAll ->
      failwith "TODO: printModuleDecl - ExportAll"
    | ModuleDecl.TsImportEquals tsImportEqualsDecl ->
      failwith "TODO: printModuleDecl - TsImportEquals"
    | ModuleDecl.TsExportAssignment tsExportAssignment ->
      failwith "TODO: printModuleDecl - TsExportAssignment"
    | ModuleDecl.TsNamespaceExport tsNamespaceExportDecl ->
      failwith "TODO: printModuleDecl - TsNamespaceExport"

  let printDecl (ctx: PrintCtx) (decl: Decl) : string =
    match decl with
    | Decl.Class _ -> failwith "TODO: printDecl - Class"
    | Decl.Fn _ -> failwith "TODO: printDecl - Fn"
    | Decl.Var { Decls = decls
                 Declare = declare
                 Kind = kind } ->
      let decls =
        List.map
          (fun ({ Id = id; Init = init }: VarDeclarator) ->
            let id = printPattern ctx id

            match init with
            | Some(init) -> $"{id} = {printExpr ctx init}"
            | None -> id)
          decls
        |> String.concat ", "

      let kind =
        match kind with
        | VariableDeclarationKind.Var -> "var"
        | VariableDeclarationKind.Let -> "let"
        | VariableDeclarationKind.Const -> "const"

      let declare = if declare then "declare " else ""

      $"{declare}{kind} {decls};"
    | Decl.Using usingDecl -> failwith "TODO: printDecl - Using"
    | Decl.TsInterface tsInterfaceDecl ->
      failwith "TODO: printDecl - TsInterface"
    | Decl.TsTypeAlias decl ->

      let name = decl.Id.Name

      let typeParams =
        match decl.TypeParams with
        | Some(typeParams) ->
          let typeParams =
            typeParams.Params
            |> List.map (printTsTypeParam ctx)
            |> String.concat ", "

          $"<{typeParams}>"
        | None -> ""

      let typeAnn = printType ctx decl.TypeAnn

      match decl.Declare with
      | true -> $"declare type {name}{typeParams} = {typeAnn};"
      | false -> $"type {name}{typeParams} = {typeAnn};"
    | Decl.TsEnum tsEnumDecl -> failwith "TODO: printDecl - TsEnum"
    | Decl.TsModule tsModuleDecl -> failwith "TODO: printDecl - TsModule"

  let printTypeAnn (ctx: PrintCtx) (typeAnn: TsTypeAnn) : string =
    printType ctx typeAnn.TypeAnn

  // TODO: handle precedence
  let printType (ctx: PrintCtx) (t: TsType) : string =
    match t with
    | TsType.TsKeywordType { Kind = kind } ->
      match kind with
      | TsAnyKeyword -> "any"
      | TsUnknownKeyword -> "unknown"
      | TsNumberKeyword -> "number"
      | TsObjectKeyword -> "object"
      | TsBooleanKeyword -> "boolean"
      | TsBigIntKeyword -> "BigInt"
      | TsStringKeyword -> "string"
      | TsSymbolKeyword -> "symbol"
      | TsVoidKeyword -> "void"
      | TsUndefinedKeyword -> "undefined"
      | TsNullKeyword -> "null"
      | TsNeverKeyword -> "never"
      | TsIntrinsicKeyword -> "intrinsic"
    | TsType.TsThisType _ -> "this"
    | TsType.TsFnOrConstructorType fnOrConst ->
      match fnOrConst with
      | TsConstructorType { Params = ps
                            TypeParams = typeParams
                            TypeAnn = typeAnn } ->
        let typeParams =
          match typeParams with
          | Some({ Params = typeParams }) ->
            let typeParams =
              typeParams
              |> List.map (printTsTypeParam ctx)
              |> String.concat ", "

            $"<{typeParams}>"
          | None -> ""

        let ps = ps |> List.map (printTsFnParam ctx) |> String.concat ", "
        let typeAnn = printTypeAnn ctx typeAnn

        $"new {typeParams}({ps}) => {typeAnn}"
      | TsFnType { Params = ps
                   TypeParams = typeParams
                   TypeAnn = typeAnn } ->
        let typeParams =
          match typeParams with
          | Some({ Params = typeParams }) ->
            let typeParams =
              typeParams
              |> List.map (printTsTypeParam ctx)
              |> String.concat ", "

            $"<{typeParams}>"
          | None -> ""

        let ps = ps |> List.map (printTsFnParam ctx) |> String.concat ", "
        let typeAnn = printTypeAnn ctx typeAnn

        $"{typeParams}({ps}) => {typeAnn}"
    | TsType.TsTypeRef { TypeName = name
                         TypeParams = typeParams } ->
      let name = printEntityName name

      match typeParams with
      | Some(typeParams: TsTypeParamInstantiation) ->
        let typeParams =
          typeParams.Params |> List.map (printType ctx) |> String.concat ", "

        $"{name}<{typeParams}>"
      | None -> name
    | TsType.TsTypeQuery { ExprName = name; TypeArgs = typeArgs } ->
      let name =
        match name with
        | TsEntityName name -> printEntityName name
        | Import _ -> failwith "TODO: printType - TsTypeQuery - Import"

      match typeArgs with
      | Some(typeArgs: TsTypeParamInstantiation) ->
        let typeArgs =
          typeArgs.Params |> List.map (printType ctx) |> String.concat ", "

        $"{name}<{typeArgs}>"
      | None -> name
    | TsType.TsTypeLit { Members = members } ->
      let oldIdent = String.replicate ctx.Indent " "
      let ctx = { ctx with Indent = ctx.Indent + 2 }
      let ident = String.replicate ctx.Indent " "

      let members =
        members
        |> List.map (fun m -> $"{ident}{printTypeMember ctx m};")
        |> String.concat "\n"

      $"{{\n{members}\n{oldIdent}}}"
    | TsType.TsArrayType { ElemType = t } ->
      let t = printType ctx t
      $"{t}[]"
    | TsType.TsTupleType { ElemTypes = types } ->
      let types =
        types
        |> List.map (fun { Label = label; Type = t } ->
          match label with
          | Some(label) -> $"{label}: {printType ctx t}"
          | None -> printType ctx t)
        |> String.concat ", "

      $"[{types}]"
    | TsType.TsOptionalType { TypeAnn = t } ->
      let t = printType ctx t
      $"{t}?" // can appear in tuple types
    | TsType.TsRestType { TypeAnn = t } ->
      let t = printType ctx t
      $"...{t}"
    | TsType.TsUnionOrIntersectionType tsUnionOrIntersectionType ->
      match tsUnionOrIntersectionType with
      | TsIntersectionType { Types = types } ->
        types |> List.map (printType ctx) |> String.concat " & "
      | TsUnionType { Types = types } ->
        types |> List.map (printType ctx) |> String.concat " | "
    | TsType.TsConditionalType { CheckType = checkType
                                 ExtendsType = extendsType
                                 TrueType = trueType
                                 FalseType = falseType } ->
      let checkType = printType ctx checkType
      let extendsType = printType ctx extendsType
      let trueType = printType ctx trueType
      let falseType = printType ctx falseType

      $"({checkType} extends {extendsType} ? {trueType} : {falseType})"
    | TsType.TsInferType { TypeParam = tp } ->
      $"infer {printTsTypeParam ctx tp}"
    | TsType.TsParenthesizedType { TypeAnn = t } -> $"({printType ctx t})"
    | TsType.TsTypeOperator { Op = op; TypeAnn = t } ->
      match op with
      | TsTypeOperatorOp.KeyOf -> $"keyof {printType ctx t}"
      | TsTypeOperatorOp.Unique -> $"unique {printType ctx t}"
      | TsTypeOperatorOp.Readonly -> $"readonly {printType ctx t}"
    | TsType.TsIndexedAccessType { ObjType = objType
                                   IndexType = indexType } ->
      let objType = printType ctx objType
      let indexType = printType ctx indexType
      $"{objType}[{indexType}]"
    | TsType.TsMappedType mappedType ->

      let tp = printTsTypeParam ctx mappedType.TypeParam

      let nameType =
        match mappedType.NameType with
        | Some(nameType) -> $"as {printType ctx nameType}"
        | None -> ""

      let readonly =
        match mappedType.Readonly with
        | Some(True) -> "readonly "
        | Some(Plus) -> "+readonly "
        | Some(Minus) -> "-readonly "
        | None -> ""

      let optional =
        match mappedType.Optional with
        | Some(True) -> "?"
        | Some(Plus) -> "+?"
        | Some(Minus) -> "-?"
        | None -> ""

      let typeAnn = printType ctx mappedType.TypeAnn

      $"{readonly}[{tp} in {nameType}]{optional}: {typeAnn}"

    | TsType.TsLitType { Lit = lit } ->
      match lit with
      | Bool { Value = value } -> if value then "true" else "false"
      | Number { Value = value } -> value.ToString()
      | Str { Value = value } -> $"\"{value}\""
      | Tpl _ -> failwith "TODO: printType - TsLitType - Tpl"
    | TsType.TsTypePredicate _ -> failwith "TODO: printType - TsTypePredicate"
    | TsType.TsImportType _ -> failwith "TODO: printType - TsImportType"

  let printTypeMember (ctx: PrintCtx) (typeMember: TsTypeElement) : string =
    match typeMember with
    | TsCallSignatureDecl { Params = ps
                            TypeAnn = typeAnn
                            TypeParams = typeParams } ->
      let typeParams =
        match typeParams with
        | Some(typeParams) ->
          let typeParams =
            typeParams.Params
            |> List.map (printTsTypeParam ctx)
            |> String.concat ", "

          $"<{typeParams}>"
        | None -> ""

      let ps = ps |> List.map (printTsFnParam ctx) |> String.concat ", "

      let typeAnn =
        match typeAnn with
        | Some({ TypeAnn = t }) -> $": {printType ctx t}"
        | None -> ""

      $"{typeParams}({ps}){typeAnn}"
    | TsConstructSignatureDecl { Params = ps
                                 TypeAnn = typeAnn
                                 TypeParams = typeParams } ->
      let typeParams =
        match typeParams with
        | Some(typeParams) ->
          let typeParams =
            typeParams.Params
            |> List.map (printTsTypeParam ctx)
            |> String.concat ", "

          $"<{typeParams}>"
        | None -> ""

      let ps = ps |> List.map (printTsFnParam ctx) |> String.concat ", "

      let typeAnn =
        match typeAnn with
        | Some({ TypeAnn = t }) -> $": {printType ctx t}"
        | None -> ""

      $"new {typeParams}({ps}){typeAnn}"
    | TsPropertySignature propSig ->
      let key =
        match propSig.Computed with
        | true -> $"[{printExpr ctx propSig.Key}]"
        | false -> printExpr ctx propSig.Key

      let typeAnn = printTypeAnn ctx propSig.TypeAnn

      match propSig.Readonly, propSig.Optional with
      | true, true -> $"readonly {key}?: {typeAnn}"
      | true, false -> $"readonly {key}: {typeAnn}"
      | false, true -> $"{key}?: {typeAnn}"
      | false, false -> $"{key}: {typeAnn}"

    | TsGetterSignature { Key = key
                          Computed = computed
                          Optional = optional
                          TypeAnn = typeAnn } ->
      let key =
        match computed with
        | true -> $"[printExpr ctx key]"
        | false -> printExpr ctx key

      let typeAnn =
        match typeAnn with
        | Some({ TypeAnn = t }) -> $": {printType ctx t}"
        | None -> ""

      match optional with
      | true -> $"get {key}?(): {typeAnn}"
      | false -> $"get {key}(): {typeAnn}"
    | TsSetterSignature { Key = key
                          Computed = computed
                          Optional = optional
                          Param = param } ->
      let key =
        match computed with
        | true -> $"[printExpr ctx key]"
        | false -> printExpr ctx key

      let param = printTsFnParam ctx param

      match optional with
      | true -> $"set {key}?({param})"
      | false -> $"set {key}({param})"
    | TsMethodSignature method ->
      let key = printExpr ctx method.Key

      let ps =
        method.Params |> List.map (printTsFnParam ctx) |> String.concat ", "

      let typeParams =
        match method.TypeParams with
        | Some(typeParams) ->
          let typeParams =
            typeParams.Params
            |> List.map (printTsTypeParam ctx)
            |> String.concat ", "

          $"<{typeParams}>"
        | None -> ""

      let typeAnn =
        match method.TypeAnn with
        | Some(typeAnn) -> $": {printTypeAnn ctx typeAnn}"
        | None -> ""

      match method.Optional with
      | true -> $"{key}?{typeParams}({ps}){typeAnn}"
      | false -> $"{key}{typeParams}({ps}){typeAnn}"
    | TsIndexSignature indexSig ->
      let name = indexSig.Param.Name
      let c = indexSig.Param.Constraint
      let param = $"{name}: {c}"
      let typeAnn = $"{printTypeAnn ctx indexSig.TypeAnn}"

      match indexSig.IsStatic, indexSig.Readonly with
      | true, true -> $"static readonly [{param}]: {typeAnn}"
      | true, false -> $"static [{param}]: {typeAnn}"
      | false, true -> $"readonly [{param}]: {typeAnn}"
      | false, false -> $"[{param}]: {typeAnn}"

  let printTsFnParam (ctx: PrintCtx) (param: TsFnParam) : string =
    let pat =
      match param.Pat with
      | TsFnParamPat.Ident id -> id.Id.Name
      | TsFnParamPat.Array arrayPat ->
        failwith "TODO: printTsFnParam - arrayPat"
      | TsFnParamPat.Rest restPat -> failwith "TODO: printTsFnParam - restPat"
      | TsFnParamPat.Object objectPat ->
        failwith "TODO: printTsFnParam - objectPat"

    match param.TypeAnn with
    | Some(typeAnn) -> $"{pat}: {printTypeAnn ctx typeAnn}"
    | None -> pat

  let printTsTypeParam (ctx: PrintCtx) (typeParam: TsTypeParam) : string =
    let c =
      match typeParam.Constraint with
      | Some(c) -> $" extends {printType ctx c}"
      | None -> ""

    let d =
      match typeParam.Default with
      | Some(d) -> $" = {printType ctx d}"
      | None -> ""

    $"{typeParam.Name.Name}{c}{d}"

  let printEntityName (name: TsEntityName) : string =
    match name with
    | TsQualifiedName { Left = left; Right = right } ->
      let left = printEntityName left
      let right = right.Name
      $"{left}.{right}"
    | Identifier { Name = name } -> name
