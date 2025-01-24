namespace Escalier.Codegen

open System.Text

open Escalier.Interop
open Escalier.Interop.TypeScript

module rec Printer =
  type PrintCtx =
    { Precedence: int
      Indent: int
      StringBuilder: StringBuilder }

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

  let printLit (ctx: PrintCtx) (lit: Lit) : unit =
    let sb = ctx.StringBuilder

    match lit with
    | Lit.Str { Value = value } -> sb.Append($"\"{value}\"") // TODO: escape special characters
    | Lit.Num { Value = value } -> sb.Append(value)
    | Lit.Bool { Value = value } ->
      if value then sb.Append("true") else sb.Append("false")
    | Lit.Regex { Exp = exp; Flags = flags } -> sb.Append($"/{exp}/{flags}")
    | Lit.Null _ -> sb.Append("null")
    | Lit.JSXText _ -> failwith "todo"

    |> ignore

  let printParams (ctx: PrintCtx) (ps: list<Param>) : unit =
    let sb = ctx.StringBuilder

    sb.Append("(") |> ignore

    Seq.ofList ps
    |> Seq.iteri (fun i p ->
      printPattern ctx p.Pat

      match p.TypeAnn with
      | Some typeAnn ->
        sb.Append(": ") |> ignore
        printTypeAnn ctx typeAnn
      | None -> ()

      if i < ps.Length - 1 then
        sb.Append(", ") |> ignore)

    sb.Append(")") |> ignore

  let rec printExpr (ctx: PrintCtx) (e: TypeScript.Expr) : unit =
    let sb = ctx.StringBuilder

    match e with
    | Expr.Ident { Name = name } -> sb.Append(name) |> ignore
    | Expr.Lit lit -> printLit ctx lit
    | Expr.This _ -> sb.Append("this") |> ignore
    | Expr.Array { Elements = elements } ->
      sb.Append("[") |> ignore

      Seq.ofList elements
      |> Seq.iteri (fun i elem ->
        match elem with
        | Some(elem) -> printExpr { ctx with Precedence = 0 } elem.Expr
        | None -> ()

        if i < elements.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append("]") |> ignore

    // TODO: support shorthand syntax
    | Expr.Object { Properties = props } ->
      sb.Append("{") |> ignore

      Seq.ofList props
      |> Seq.iteri (fun i prop ->
        match prop with
        | Property.KeyValueProperty { Key = key
                                      Value = value
                                      Kind = _kind } ->
          // TODO: handle getter/setter kinds
          printPropName ctx key
          sb.Append(": ") |> ignore
          printExpr { ctx with Precedence = 0 } value
        | Property.Ident { Name = name } -> sb.Append(name) |> ignore
        | Property.SpreadElement { Expr = expr } ->
          sb.Append("...") |> ignore
          printExpr { ctx with Precedence = 0 } expr

        if i < props.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append("}") |> ignore
    | Expr.Fn { Id = id
                Fn = { Params = ps
                       Body = body
                       IsAsync = isAsync } } ->
      let ctx = { ctx with Precedence = 0 }

      if isAsync then
        sb.Append("async ") |> ignore

      sb.Append("function ") |> ignore

      match id with
      | Some id -> sb.Append(id.Name) |> ignore
      | None -> ()

      printParams ctx ps

      match body with
      | Some(body) ->
        sb.Append(" ") |> ignore
        printBlock ctx body
      | None -> ()

    | Expr.Arrow { Params = ps; Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      printParams ctx ps
      sb.Append(" => ") |> ignore
      printBlock ctx body
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

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      sb.Append(op) |> ignore
      printExpr { ctx with Precedence = innerPrec } arg

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore

    | Expr.Update { Operator = op
                    Argument = arg
                    Prefix = prefix } ->

      let outerPrec = ctx.Precedence
      let innerPrec = if prefix then 15 else 16

      let op =
        match op with
        | Decrement -> "--"
        | Increment -> "++"

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      if prefix then
        sb.Append(op) |> ignore
        printExpr { ctx with Precedence = innerPrec } arg
      else
        printExpr { ctx with Precedence = innerPrec } arg
        sb.Append(op) |> ignore

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore
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

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      printExpr { ctx with Precedence = innerPrec } left
      sb.Append($" {op} ") |> ignore
      printExpr { ctx with Precedence = innerPrec } right

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore

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

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      printExpr { ctx with Precedence = innerPrec } left
      sb.Append($" {op} ") |> ignore
      printExpr { ctx with Precedence = innerPrec } right

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore

    | Expr.Member { Object = object
                    Property = property
                    Computed = computed
                    OptChain = optChain } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 18

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      printExpr { ctx with Precedence = innerPrec } object

      match optChain with
      | true ->
        if computed then
          sb.Append("?.") |> ignore
          sb.Append("[") |> ignore
          printExpr { ctx with Precedence = innerPrec } property
          sb.Append("]") |> ignore
        else
          sb.Append("?.") |> ignore
          printExpr { ctx with Precedence = innerPrec } property
      | false ->
        if computed then
          sb.Append("[") |> ignore
          printExpr { ctx with Precedence = innerPrec } property
          sb.Append("]") |> ignore
        else
          sb.Append(".") |> ignore
          printExpr { ctx with Precedence = innerPrec } property

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore
    | Expr.Cond { Test = test
                  Consequent = cons
                  Alternate = alt } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 3

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      printExpr { ctx with Precedence = innerPrec } test
      sb.Append(" ? ") |> ignore
      printExpr { ctx with Precedence = innerPrec } cons
      sb.Append(" : ") |> ignore
      printExpr { ctx with Precedence = innerPrec } alt

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore

    | Expr.Call { Callee = callee; Arguments = args } ->
      let outerPrec = ctx.Precedence
      let innerPrec = 18

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      printExpr { ctx with Precedence = innerPrec } callee
      sb.Append("(") |> ignore

      Seq.ofList args
      |> Seq.iteri (fun i arg ->
        printExpr { ctx with Precedence = innerPrec } arg

        if i < args.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append(")") |> ignore

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore
    | Expr.New { Callee = callee; Arguments = args } ->
      let outerPrec = ctx.Precedence
      let innerPrec = 18

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      sb.Append("new ") |> ignore

      printExpr { ctx with Precedence = innerPrec } callee
      sb.Append("(") |> ignore

      Seq.ofList args
      |> Seq.iteri (fun i arg ->
        printExpr { ctx with Precedence = innerPrec } arg

        if i < args.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append(")") |> ignore

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore
    | Expr.Seq { Exprs = exprs } ->

      let outerPrec = ctx.Precedence
      let innerPrec = 1

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      Seq.ofList exprs
      |> Seq.iteri (fun i expr ->
        printExpr { ctx with Precedence = innerPrec } expr

        if i < exprs.Length - 1 then
          sb.Append(", ") |> ignore)

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore
    | Expr.SuperProp _ -> failwith "TODO: printExpr - SuperProp"
    | Expr.Tpl template -> printTemplateLiteral ctx template
    | Expr.TaggedTpl { Tag = tag
                       TypeParams = _
                       Tpl = template } ->
      printExpr ctx tag
      printTemplateLiteral ctx template
    | Expr.Class classExpr -> printClassExpr ctx classExpr
    | Expr.Yield _ -> failwith "TODO: printExpr - Yield"
    | Expr.MetaProp _ -> failwith "TODO: printExpr - MetaProp"
    | Expr.Await { Arg = arg } ->
      let outerPrec = ctx.Precedence
      let innerPrec = 14 // prefix operators

      if innerPrec < outerPrec then
        sb.Append("(") |> ignore

      sb.Append("await ") |> ignore
      printExpr { ctx with Precedence = innerPrec } arg

      if innerPrec < outerPrec then
        sb.Append(")") |> ignore
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
    | Expr.TsInstantiation { Expr = expr; TypeArgs = typeArgs } ->
      printExpr ctx expr

      sb.Append("<") |> ignore

      Seq.ofList typeArgs.Params
      |> Seq.iteri (fun i typeArg ->
        printType ctx typeArg

        if i < typeArgs.Params.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append(">") |> ignore

    | Expr.TsSatisfies _ -> failwith "TODO: printExpr - TsSatisfies"
    | Expr.PrivateName _ -> failwith "TODO: printExpr - PrivateName"
    | Expr.OptChain _ -> failwith "TODO: printExpr - OptChain"
    | Expr.Invalid _ -> failwith "TODO: printExpr - Invalid"

  let printClassExpr (ctx: PrintCtx) (classExpr: ClassExpr) : unit =
    let sb = ctx.StringBuilder

    let { Id = id; Class = cls } = classExpr
    let { Super = super; Body = body } = cls

    sb.Append("class ") |> ignore

    match id with
    | Some id -> sb.Append(id.Name) |> ignore
    | None -> ()

    match super with
    | Some { TypeName = name } ->
      sb.Append(" extends ") |> ignore
      sb.Append(name) |> ignore
    | None -> ()

    let oldIndent = String.replicate ctx.Indent " "
    let ctx = { ctx with Indent = ctx.Indent + 2 }
    let indent = String.replicate ctx.Indent " "

    // TODO: indent the body
    sb.Append("{\n") |> ignore

    for m in body do
      sb.Append(indent) |> ignore
      printClassMember ctx m
      sb.Append("\n") |> ignore

    sb.Append(oldIndent).Append("}") |> ignore

  let printClassMember (ctx: PrintCtx) (classMember: ClassMember) : unit =
    let sb = ctx.StringBuilder

    match classMember with
    | ClassMember.Constructor { Params = ps; Body = body } ->

      let ctx = { ctx with Precedence = 0 }

      sb.Append("constructor") |> ignore
      sb.Append("(") |> ignore

      Seq.ofList ps
      |> Seq.iteri (fun i p ->
        match p with
        | ParamOrTsParamProp.Param p -> printPattern ctx p.Pat
        | ParamOrTsParamProp.TsParamProp _ ->
          failwith "TODO: printClassMember - TsParamProp"

        if i < ps.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append(") ") |> ignore

      match body with
      | Some(body) -> printBlock ctx body
      | None -> ()

    | ClassMember.Method { Key = key
                           Function = { IsAsync = isAsync
                                        Body = body
                                        Params = ps } } ->
      if isAsync then
        sb.Append("async ") |> ignore

      printPropName ctx key

      let ctx = { ctx with Precedence = 0 }

      printParams ctx ps

      match body with
      | Some body ->
        sb.Append(" ") |> ignore
        printBlock ctx body
      | None -> ()
    | ClassMember.PrivateMethod _ ->
      failwith "TODO: printClassMember - PrivateMethod"
    | ClassMember.ClassProp classProp ->
      if classProp.IsStatic then
        sb.Append "static " |> ignore

      printPropName ctx classProp.Key

      match classProp.Value with
      | Some value ->
        sb.Append(" = ") |> ignore
        printExpr ctx value
      | None -> ()
    | ClassMember.PrivateProp _ ->
      failwith "TODO: printClassMember - PrivateProp"
    | ClassMember.TsIndexSignature _ ->
      failwith "TODO: printClassMember - TsIndexSignature"
    | ClassMember.Empty _ -> failwith "TODO: printClassMember - Empty"
    | ClassMember.StaticBlock _ ->
      failwith "TODO: printClassMember - StaticBlock"
    | ClassMember.AutoAccessor _ ->
      failwith "TODO: printClassMember - AutoAccessor"

  let printTemplateLiteral (ctx: PrintCtx) (template: Tpl) : unit =
    let { Exprs = exprs; Quasis = quasis } = template
    let sb = ctx.StringBuilder

    sb.Append("`") |> ignore

    for quasi, expr in List.zip (List.take exprs.Length quasis) exprs do
      sb.Append(quasi.Raw) |> ignore
      sb.Append("${") |> ignore
      printExpr ctx expr
      sb.Append("}") |> ignore

    sb.Append(quasis[quasis.Length - 1].Raw).Append("`") |> ignore

  let printVarDecl (ctx: PrintCtx) (varDecl: VarDecl) : unit =
    let sb = ctx.StringBuilder

    let { Export = export
          Declare = declare
          Decls = decls
          Kind = kind } =
      varDecl

    if export then
      sb.Append("export ") |> ignore

    if declare then
      sb.Append("declare ") |> ignore

    let kind =
      match kind with
      | VariableDeclarationKind.Var -> "var"
      | VariableDeclarationKind.Let -> "let"
      | VariableDeclarationKind.Const -> "const"

    sb.Append(kind).Append(" ") |> ignore

    Seq.ofList decls
    |> Seq.iteri (fun i decl ->
      let { VarDeclarator.Id = id
            Init = init
            TypeAnn = typeAnn } =
        decl

      printPattern ctx id

      match typeAnn with
      | Some typeAnn ->
        sb.Append(": ") |> ignore
        printTypeAnn ctx typeAnn
      | None -> ()

      match init with
      | Some init ->
        sb.Append(" = ") |> ignore
        printExpr ctx init
      | None -> ()

      if i < decls.Length - 1 then
        sb.Append(", ") |> ignore)

  let printStmt (ctx: PrintCtx) (stmt: Stmt) : unit =
    let sb = ctx.StringBuilder

    // sb.Append(String.replicate ctx.Indent " ") |> ignore

    match stmt with
    | Stmt.Block block -> printBlock ctx block
    | Stmt.Expr { Expr = expr } ->
      let ctx = { ctx with Precedence = 0 }
      printExpr ctx expr
      sb.Append(";") |> ignore
    | Stmt.Empty _ -> sb.Append(";") |> ignore
    | Stmt.Debugger _ -> sb.Append("debugger;") |> ignore
    | Stmt.Return { Argument = arg } ->
      let ctx = { ctx with Precedence = 0 }

      match arg with
      | Some(arg) ->
        sb.Append("return ") |> ignore
        printExpr ctx arg
        sb.Append(";") |> ignore
      | None -> sb.Append("return;") |> ignore
    | Stmt.Labeled { Label = label; Body = body } ->
      sb.Append(label.Name) |> ignore
      sb.Append(": ") |> ignore
      printStmt ctx body
    | Stmt.Break { Label = label } ->
      sb.Append("break ") |> ignore

      match label with
      | Some(label) -> sb.Append(label.Name) |> ignore
      | None -> ()

      sb.Append(";") |> ignore
    | Stmt.Continue { Label = label } ->
      sb.Append("continue ") |> ignore

      match label with
      | Some(label) -> sb.Append(label.Name) |> ignore
      | None -> ()

      sb.Append(";") |> ignore
    | Stmt.If { Test = test
                Consequent = cons
                Alternate = alt } ->
      let ctx = { ctx with Precedence = 0 }

      sb.Append("if (") |> ignore
      printExpr ctx test
      sb.Append(") ") |> ignore
      printStmt ctx cons

      match alt with
      | Some alt ->
        sb.Append(" else ") |> ignore
        printStmt ctx alt
      | None -> ()
    | Stmt.Switch { Discriminant = disc; Cases = cases } ->
      let ctx = { ctx with Precedence = 0 }

      sb.Append("switch (") |> ignore
      printExpr ctx disc
      sb.Append(") {\n") |> ignore

      for case in cases do
        match case.Test with
        | Some test ->
          sb.Append("case ") |> ignore
          printExpr ctx test
        | None -> sb.Append("default") |> ignore

        sb.Append(":") |> ignore

        match case.Consequent with
        | [ stmt ] ->
          printStmt ctx stmt
          sb.Append("\n") |> ignore
        | stmts ->
          // TODO: handle indentation
          sb.Append("\n") |> ignore

          for stmt in stmts do
            printStmt ctx stmt
            sb.Append("\n") |> ignore

      sb.Append("}") |> ignore
    | Stmt.Throw { Argument = arg } ->
      let ctx = { ctx with Precedence = 0 }

      sb.Append("throw ") |> ignore
      printExpr ctx arg
      sb.Append(";") |> ignore
    | Stmt.Try { TryBlock = block
                 Handler = handler
                 Finalizer = finalizer } ->
      let ctx = { ctx with Precedence = 0 }

      sb.Append("try ") |> ignore
      printBlock ctx block

      match handler with
      | Some handler ->
        sb.Append(" catch (") |> ignore
        printPattern ctx handler.Param
        sb.Append(") ") |> ignore
        printBlock ctx handler.Body
      | None -> ()

      match finalizer with
      | Some finalizer ->
        sb.Append(" finally ") |> ignore
        printBlock ctx finalizer
      | None -> ()
    | Stmt.While { Test = test; Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      sb.Append("while (") |> ignore
      printExpr ctx test
      sb.Append(") ") |> ignore
      printStmt ctx body
    | Stmt.DoWhile { Test = test; Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      sb.Append("do ") |> ignore
      printStmt ctx body
      sb.Append(" while (") |> ignore
      printExpr ctx test
      sb.Append(");") |> ignore
    | Stmt.For { Init = init
                 Test = test
                 Update = update
                 Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      sb.Append("for (") |> ignore

      match init with
      | Some(ForInit.Variable decl) -> printStmt ctx (Stmt.Decl(Decl.Var decl))
      | Some(ForInit.Expr expr) -> printExpr ctx expr
      | None -> ()

      sb.Append("; ") |> ignore

      match test with
      | Some(test) -> printExpr ctx test
      | None -> ()

      sb.Append("; ") |> ignore

      match update with
      | Some(update) -> printExpr ctx update
      | None -> ()

      sb.Append(") ") |> ignore
      printStmt ctx body
    | Stmt.ForIn { Left = left
                   Right = right
                   Body = body } ->
      let ctx = { ctx with Precedence = 0 }

      sb.Append("for (") |> ignore

      match left with
      | ForHead.VarDecl decl -> printStmt ctx (Stmt.Decl(Decl.Var decl))
      | ForHead.Pat p -> printPattern ctx p
      | ForHead.UsingDecl _ -> failwith "TODO"

      sb.Append(" in ") |> ignore
      printExpr ctx right
      sb.Append(") ") |> ignore
      printStmt ctx body

    | Stmt.ForOf { IsAwait = _
                   Left = left
                   Right = right
                   Body = body } ->

      sb.Append("for (") |> ignore

      match left with
      | ForHead.VarDecl decl -> printVarDecl ctx decl
      | ForHead.Pat p -> printPattern ctx p
      | ForHead.UsingDecl _ -> failwith "TODO"

      sb.Append(" of ") |> ignore
      printExpr ctx right
      sb.Append(") ") |> ignore
      printStmt ctx body

    | Stmt.Decl decl ->
      let ctx = { ctx with Precedence = 0 }

      // TODO: move this into each decl type below
      let comments =
        match decl with
        | Decl.Fn { Comments = comments } -> comments
        | Decl.Class _ -> []
        | Decl.Var { Comments = comments } -> comments
        | Decl.Using _ -> []
        | Decl.TsInterface { Comments = comments } -> comments
        | Decl.TsTypeAlias { Comments = comments } -> comments
        | Decl.TsEnum { Comments = comments } -> comments
        | Decl.TsModule _ -> []

      for comment in comments do
        match comment with
        | LineComment { Text = text } ->
          sb.Append(text) |> ignore
          sb.Append("\n") |> ignore
        | BlockComment _ -> failwith "TODO: printStmt - BlockComment"

      match decl with
      | Decl.Fn { Id = id
                  Fn = { Params = ps
                         Body = body
                         ReturnType = retType } } ->
        let id = id.Name

        sb.Append("function ").Append(id) |> ignore

        printParams ctx ps

        match retType with
        | Some(retType) ->
          sb.Append(": ") |> ignore
          printType ctx retType.TypeAnn
        | None -> ()

        match body with
        | Some(body) ->
          sb.Append(" ") |> ignore
          printBlock ctx body
        | None -> sb.Append(";") |> ignore

      | Decl.Var varDecl ->
        printVarDecl ctx varDecl
        sb.Append(";") |> ignore
      | Decl.Class _ -> failwith "TODO: printStmt - Class"
      | Decl.Using _ -> failwith "TODO: printStmt - Using"
      | Decl.TsInterface decl ->
        // TODO: print comments
        if decl.Export then
          sb.Append("export ") |> ignore

        if decl.Declare then
          sb.Append("declare ") |> ignore

        sb.Append("interface ").Append(decl.Id.Name) |> ignore
        decl.TypeParams |> Option.iter (printTypeParamDeclaration ctx)

        let oldIndent = String.replicate ctx.Indent " "
        let ctx = { ctx with Indent = ctx.Indent + 2 }
        let indent = String.replicate ctx.Indent " "

        sb.Append("{\n") |> ignore

        for m in decl.Body.Body do
          sb.Append(indent) |> ignore
          printTypeMember ctx m
          sb.Append(";\n") |> ignore

        sb.Append(oldIndent).Append("}") |> ignore
      | Decl.TsTypeAlias decl ->
        if decl.Export then
          sb.Append("export ") |> ignore

        if decl.Declare then
          sb.Append("declare ") |> ignore

        sb.Append("type ").Append(decl.Id.Name) |> ignore
        decl.TypeParams |> Option.iter (printTypeParamDeclaration ctx)
        sb.Append(" = ") |> ignore
        printType ctx decl.TypeAnn
        sb.Append(";") |> ignore
      | Decl.TsEnum _ -> failwith "TODO: printStmt - TsEnum"
      | Decl.TsModule _ -> failwith "TODO: printStmt - TsModule"

  let printPattern (ctx: PrintCtx) (p: Pat) : unit =
    let sb = ctx.StringBuilder

    match p with
    | Pat.Ident { Id = id } -> sb.Append(id.Name) |> ignore
    | Pat.Array { Elems = elems } ->
      sb.Append("[") |> ignore

      Seq.ofList elems
      |> Seq.iteri (fun i elem ->
        match elem with
        | Some(elem) -> printPattern ctx elem
        | None -> ()

        if i < elems.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append("]") |> ignore
    | Pat.Rest { Arg = arg } ->
      sb.Append("...") |> ignore
      printPattern ctx arg
    | Pat.Object { Props = props } ->
      sb.Append("{") |> ignore

      Seq.ofList props
      |> Seq.iteri (fun i prop ->
        match prop with
        | ObjectPatProp.Rest { Arg = arg } ->
          sb.Append("...") |> ignore
          printPattern ctx arg
        | ObjectPatProp.Assign { Key = key; Value = _ } ->
          sb.Append(key.Name) |> ignore
        | ObjectPatProp.KeyValue { Key = key; Value = value } ->
          printPropName ctx key
          sb.Append(": ") |> ignore
          printPattern ctx value

        if i < props.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append("}") |> ignore
    | Pat.Assign _ -> failwith "TODO: printPattern - Assign"

  let printBlock (ctx: PrintCtx) (block: BlockStmt) : unit =
    let sb = ctx.StringBuilder

    sb.Append("{\n") |> ignore

    let oldIndent = String.replicate ctx.Indent " "
    let ctx = { ctx with Indent = ctx.Indent + 2 }
    let indent = String.replicate ctx.Indent " "

    for stmt in block.Body do
      sb.Append(indent) |> ignore
      printStmt ctx stmt
      sb.Append("\n") |> ignore

    sb.Append(oldIndent) |> ignore
    sb.Append("}") |> ignore

  let printModule (m: Module) : string =
    let ctx: PrintCtx =
      { Indent = 0
        Precedence = 0
        StringBuilder = StringBuilder() }

    for stmt in m.Body do
      printModuleItem ctx stmt
      ctx.StringBuilder.Append("\n") |> ignore

    ctx.StringBuilder.ToString()

  let printModuleItem (ctx: PrintCtx) (mi: ModuleItem) : unit =
    match mi with
    | ModuleItem.Stmt stmt -> printStmt ctx stmt
    | ModuleItem.ModuleDecl decl -> printModuleDecl ctx decl

  let printModuleDecl (ctx: PrintCtx) (decl: ModuleDecl) : unit =
    let sb = ctx.StringBuilder

    match decl with
    | ModuleDecl.Import importDecl ->
      let hasNamedSpecifiers =
        List.exists
          (fun spec ->
            match spec with
            | ImportSpecifier.Named _ -> true
            | _ -> false)
          importDecl.Specifiers

      sb.Append("import ") |> ignore

      if hasNamedSpecifiers then
        sb.Append("{") |> ignore

      Seq.ofList importDecl.Specifiers
      |> Seq.iteri (fun i spec ->
        match spec with
        | ImportSpecifier.Named { Local = local; Imported = imported } ->

          match imported with
          | None -> sb.Append(local.Name) |> ignore
          | Some imported ->
            let imported =
              match imported with
              | ModuleExportName.Ident ident -> ident.Name
              | ModuleExportName.Str str -> str.Value

            sb.Append(imported).Append(" as ").Append(local.Name) |> ignore

        | ImportSpecifier.Default _ -> failwith "todo - default import"
        | ImportSpecifier.Namespace { Local = local } ->
          sb.Append("* as ").Append(local.Name) |> ignore

        if i < importDecl.Specifiers.Length - 1 then
          sb.Append(", ") |> ignore)

      if hasNamedSpecifiers then
        sb.Append("}") |> ignore

      sb.Append(" from ").Append($"\"{importDecl.Src.Value}\"") |> ignore
    | ModuleDecl.ExportNamed _ -> failwith "TODO: printModuleDecl - ExportNamed"
    | ModuleDecl.ExportDefaultDecl _ ->
      failwith "TODO: printModuleDecl - ExportDefaultDecl"
    | ModuleDecl.ExportDefaultExpr _ ->
      failwith "TODO: printModuleDecl - ExportDefaultExpr"
    | ModuleDecl.ExportAll _ -> failwith "TODO: printModuleDecl - ExportAll"
    | ModuleDecl.TsImportEquals _ ->
      failwith "TODO: printModuleDecl - TsImportEquals"
    | ModuleDecl.TsExportAssignment _ ->
      failwith "TODO: printModuleDecl - TsExportAssignment"
    | ModuleDecl.TsNamespaceExport _ ->
      failwith "TODO: printModuleDecl - TsNamespaceExport"

  let printDecl (ctx: PrintCtx) (decl: Decl) : unit =
    let sb = ctx.StringBuilder

    match decl with
    | Decl.Class _ -> failwith "TODO: printDecl - Class"
    | Decl.Fn _ -> failwith "TODO: printDecl - Fn"
    | Decl.Var { Decls = decls
                 Declare = declare
                 Kind = kind } ->
      if declare then
        sb.Append("declare ") |> ignore

      let kind =
        match kind with
        | VariableDeclarationKind.Var -> "var"
        | VariableDeclarationKind.Let -> "let"
        | VariableDeclarationKind.Const -> "const"

      sb.Append(kind).Append(" ") |> ignore

      Seq.ofList decls
      |> Seq.iteri (fun i { Id = id; Init = init } ->
        printPattern ctx id

        match init with
        | Some(init) ->
          sb.Append(" = ") |> ignore
          printExpr ctx init
        | None -> ()

        if i < decls.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append(";") |> ignore
    | Decl.Using _ -> failwith "TODO: printDecl - Using"
    | Decl.TsInterface _ -> failwith "TODO: printDecl - TsInterface"
    | Decl.TsTypeAlias decl ->

      if decl.Declare then
        sb.Append("declare ") |> ignore

      sb.Append("type ").Append(decl.Id.Name) |> ignore
      decl.TypeParams |> Option.iter (printTypeParamDeclaration ctx)
      sb.Append(" = ") |> ignore
      printType ctx decl.TypeAnn
      sb.Append(";") |> ignore
    | Decl.TsEnum _ -> failwith "TODO: printDecl - TsEnum"
    | Decl.TsModule _ -> failwith "TODO: printDecl - TsModule"

  let printTypeAnn (ctx: PrintCtx) (typeAnn: TsTypeAnn) : unit =
    printType ctx typeAnn.TypeAnn

  let printTypeParamDeclaration (ctx: PrintCtx) (tpd: TsTypeParamDecl) : unit =
    let sb = ctx.StringBuilder

    sb.Append("<") |> ignore

    Seq.ofList tpd.Params
    |> Seq.iteri (fun i typeParam ->
      printTsTypeParam ctx typeParam

      if i < tpd.Params.Length - 1 then
        sb.Append(", ") |> ignore)

    sb.Append(">") |> ignore

  let printTypeParamInstantiation
    (ctx: PrintCtx)
    (tpi: TsTypeParamInstantiation)
    : unit =
    let sb = ctx.StringBuilder

    sb.Append("<") |> ignore

    Seq.ofList tpi.Params
    |> Seq.iteri (fun i typeParam ->
      printType ctx typeParam

      if i < tpi.Params.Length - 1 then
        sb.Append(", ") |> ignore)

    sb.Append(">") |> ignore

  let printTsFnParams (ctx: PrintCtx) (ps: TsFnParam list) : unit =
    let sb = ctx.StringBuilder

    sb.Append("(") |> ignore

    Seq.ofList ps
    |> Seq.iteri (fun i p ->
      printTsFnParam ctx p

      if i < ps.Length - 1 then
        sb.Append(", ") |> ignore)

    sb.Append(")") |> ignore

  // TODO: handle precedence
  let printType (ctx: PrintCtx) (t: TsType) : unit =
    let sb = ctx.StringBuilder

    match t with
    | TsType.TsKeywordType { Kind = kind } ->
      let kind =
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

      sb.Append(kind) |> ignore
    | TsType.TsThisType _ -> sb.Append("this") |> ignore
    | TsType.TsFnOrConstructorType fnOrConst ->
      match fnOrConst with
      | TsConstructorType { Params = ps
                            TypeParams = typeParams
                            TypeAnn = typeAnn } ->
        sb.Append("new ") |> ignore

        typeParams |> Option.iter (printTypeParamDeclaration ctx)
        printTsFnParams ctx ps
        sb.Append(" => ") |> ignore
        printTypeAnn ctx typeAnn
      | TsFnType { Params = ps
                   TypeParams = typeParams
                   TypeAnn = typeAnn } ->

        typeParams |> Option.iter (printTypeParamDeclaration ctx)
        printTsFnParams ctx ps
        sb.Append(" => ") |> ignore
        printTypeAnn ctx typeAnn
    | TsType.TsTypeRef { TypeName = name
                         TypeParams = typeParams } ->
      printEntityName ctx name

      match typeParams with
      | Some(tpi: TsTypeParamInstantiation) ->
        printTypeParamInstantiation ctx tpi
      | None -> ()
    | TsType.TsTypeQuery { ExprName = name; TypeArgs = typeArgs } ->
      match name with
      | TsEntityName name -> printEntityName ctx name
      | Import _ -> failwith "TODO: printType - TsTypeQuery - Import"

      match typeArgs with
      | Some(tpi: TsTypeParamInstantiation) ->
        printTypeParamInstantiation ctx tpi
      | None -> ()
    | TsType.TsTypeLit { Members = members } ->
      let oldIndent = String.replicate ctx.Indent " "
      let ctx = { ctx with Indent = ctx.Indent + 2 }
      let indent = String.replicate ctx.Indent " "

      sb.Append("{\n") |> ignore

      for m in members do
        sb.Append(indent) |> ignore
        printTypeMember ctx m
        sb.Append(";\n") |> ignore

      sb.Append(oldIndent).Append("}") |> ignore
    | TsType.TsArrayType { ElemType = t } ->
      printType ctx t
      sb.Append("[]") |> ignore
    | TsType.TsTupleType { ElemTypes = types } ->
      sb.Append("[") |> ignore

      Seq.ofList types
      |> Seq.iteri (fun i { Label = label; Type = t } ->
        match label with
        | Some label -> sb.Append(label).Append(": ") |> ignore
        | None -> ()

        printType ctx t

        if i < types.Length - 1 then
          sb.Append(", ") |> ignore)

      sb.Append("]") |> ignore
    | TsType.TsOptionalType { TypeAnn = t } ->
      printType ctx t
      sb.Append("?") |> ignore // can appear in tuple types
    | TsType.TsRestType { TypeAnn = t } ->
      sb.Append("...") |> ignore
      printType ctx t
    | TsType.TsUnionOrIntersectionType tsUnionOrIntersectionType ->
      match tsUnionOrIntersectionType with
      | TsIntersectionType { Types = types } ->
        Seq.ofList types
        |> Seq.iteri (fun i t ->
          printType ctx t

          if i < types.Length - 1 then
            sb.Append(" & ") |> ignore)
      | TsUnionType { Types = types } ->
        Seq.ofList types
        |> Seq.iteri (fun i t ->
          printType ctx t

          if i < types.Length - 1 then
            sb.Append(" | ") |> ignore)
    | TsType.TsConditionalType { CheckType = checkType
                                 ExtendsType = extendsType
                                 TrueType = trueType
                                 FalseType = falseType } ->
      sb.Append("(") |> ignore // TODO: precedence handling

      printType ctx checkType
      sb.Append(" extends ") |> ignore
      printType ctx extendsType
      sb.Append(" ? ") |> ignore
      printType ctx trueType
      sb.Append(" : ") |> ignore
      printType ctx falseType

      sb.Append(")") |> ignore // TODO: precedence handling
    | TsType.TsInferType { TypeParam = tp } ->
      sb.Append("infer ") |> ignore
      printTsTypeParam ctx tp
    | TsType.TsParenthesizedType { TypeAnn = t } ->
      sb.Append("(") |> ignore
      printType ctx t
      sb.Append(")") |> ignore
    | TsType.TsTypeOperator { Op = op; TypeAnn = t } ->
      let op =
        match op with
        | TsTypeOperatorOp.KeyOf -> "keyof "
        | TsTypeOperatorOp.Unique -> "unique "
        | TsTypeOperatorOp.Readonly -> "readonly "

      sb.Append(op) |> ignore
      printType ctx t
    | TsType.TsIndexedAccessType { ObjType = objType
                                   IndexType = indexType } ->
      printType ctx objType
      sb.Append("[") |> ignore
      printType ctx indexType
      sb.Append("]") |> ignore
    | TsType.TsMappedType mappedType ->
      sb.Append("{") |> ignore

      match mappedType.Readonly with
      | Some(True) -> sb.Append("readonly ") |> ignore
      | Some(Plus) -> sb.Append("+readonly ") |> ignore
      | Some(Minus) -> sb.Append("-readonly ") |> ignore
      | None -> ()

      sb.Append("[") |> ignore
      // TODO: Handle mappedType.NameType to handle renaming of keys
      sb.Append(mappedType.TypeParam.Name.Name) |> ignore
      sb.Append(" in ") |> ignore

      match mappedType.TypeParam.Constraint with
      | Some c -> printType ctx c
      | None -> failwith "missing constraint for type param in mapped type"

      sb.Append("]") |> ignore

      match mappedType.Optional with
      | Some(True) -> sb.Append("?") |> ignore
      | Some(Plus) -> sb.Append("+?") |> ignore
      | Some(Minus) -> sb.Append("-?") |> ignore
      | None -> ()

      sb.Append(": ") |> ignore
      printType ctx mappedType.TypeAnn
    | TsType.TsLitType { Lit = lit } ->
      match lit with
      | Bool { Value = value } ->
        if value then
          sb.Append("true") |> ignore
        else
          sb.Append("false") |> ignore
      | Number { Value = value } -> sb.Append(value) |> ignore
      | Str { Value = value } -> sb.Append($"\"{value}\"") |> ignore
      | Tpl _ -> failwith "TODO: printType - TsLitType - Tpl"
    | TsType.TsTypePredicate _ -> failwith "TODO: printType - TsTypePredicate"
    | TsType.TsImportType _ -> failwith "TODO: printType - TsImportType"

  let printTypeMember (ctx: PrintCtx) (typeMember: TsTypeElement) : unit =
    let sb = ctx.StringBuilder

    match typeMember with
    | TsCallSignatureDecl { Params = ps
                            TypeAnn = typeAnn
                            TypeParams = typeParams } ->

      typeParams |> Option.iter (printTypeParamDeclaration ctx)
      printTsFnParams ctx ps

      match typeAnn with
      | Some({ TypeAnn = t }) ->
        sb.Append(": ") |> ignore
        printType ctx t
      | None -> ()
    | TsConstructSignatureDecl { Params = ps
                                 TypeAnn = typeAnn
                                 TypeParams = typeParams } ->
      sb.Append("new ") |> ignore
      typeParams |> Option.iter (printTypeParamDeclaration ctx)
      printTsFnParams ctx ps

      match typeAnn with
      | Some({ TypeAnn = t }) ->
        sb.Append(": ") |> ignore
        printType ctx t
      | None -> ()
    | TsPropertySignature propSig ->
      if propSig.Readonly then
        sb.Append("readonly ") |> ignore

      printPropName ctx propSig.Key

      if propSig.Optional then
        sb.Append("?") |> ignore

      sb.Append(": ") |> ignore
      printTypeAnn ctx propSig.TypeAnn
    | TsGetterSignature { Key = key
                          Optional = optional
                          TypeAnn = typeAnn } ->
      sb.Append("get ") |> ignore
      printPropName ctx key

      if optional then
        sb.Append("?") |> ignore

      sb.Append("()") |> ignore

      match typeAnn with
      | Some { TypeAnn = t } ->
        sb.Append(": ") |> ignore
        printType ctx t
      | None -> ()
    | TsSetterSignature { Key = key
                          Optional = optional
                          Param = param } ->
      sb.Append("set ") |> ignore
      printPropName ctx key

      if optional then
        sb.Append("?") |> ignore

      sb.Append("(") |> ignore
      printTsFnParam ctx param
      sb.Append("): void") |> ignore
    | TsMethodSignature method ->
      printPropName ctx method.Key

      if method.Optional then
        sb.Append("?") |> ignore

      method.TypeParams |> Option.iter (printTypeParamDeclaration ctx)
      printTsFnParams ctx method.Params

      match method.TypeAnn with
      | Some typeAnn ->
        sb.Append(": ") |> ignore
        printTypeAnn ctx typeAnn
      | None -> ()
    | TsIndexSignature indexSig ->
      if indexSig.IsStatic then
        sb.Append("static ") |> ignore

      if indexSig.Readonly then
        sb.Append("readonly ") |> ignore

      sb.Append("[") |> ignore
      sb.Append(indexSig.Param.Name) |> ignore
      sb.Append(": ") |> ignore
      printType ctx indexSig.Param.Constraint
      sb.Append("]: ") |> ignore
      printTypeAnn ctx indexSig.TypeAnn

  let printTsFnParam (ctx: PrintCtx) (param: TsFnParam) : unit =
    let sb = ctx.StringBuilder

    printPattern ctx param.Pat

    if param.Optional then
      sb.Append("?") |> ignore

    sb.Append(": ") |> ignore
    param.TypeAnn |> Option.iter (printTypeAnn ctx)

  let printTsTypeParam (ctx: PrintCtx) (typeParam: TsTypeParam) : unit =
    let sb = ctx.StringBuilder

    sb.Append(typeParam.Name.Name) |> ignore

    match typeParam.Constraint with
    | Some(c) ->
      sb.Append(" extends ") |> ignore
      printType ctx c
    | None -> ()

    match typeParam.Default with
    | Some(d) ->
      sb.Append(" = ") |> ignore
      printType ctx d
    | None -> ()

  let printEntityName (ctx: PrintCtx) (name: TsEntityName) : unit =
    let sb = ctx.StringBuilder

    match name with
    | TsQualifiedName { Left = left; Right = right } ->
      printEntityName ctx left
      sb.Append(".").Append(right.Name) |> ignore
    | Identifier { Name = name } -> sb.Append(name) |> ignore

  let printPropName (ctx: PrintCtx) (name: PropName) : unit =
    let sb = ctx.StringBuilder

    match name with
    | PropName.Ident id -> sb.Append(id.Name) |> ignore
    | PropName.Str { Value = value } -> sb.Append($"\"{value}\"") |> ignore
    | PropName.Num { Value = value } -> sb.Append(value) |> ignore
    | PropName.Computed { Expr = expr } ->
      sb.Append("[") |> ignore
      printExpr ctx expr
      sb.Append("]") |> ignore
