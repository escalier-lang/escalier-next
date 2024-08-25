namespace Escalier.Codegen

open Escalier.Interop
open Escalier.Interop.TypeScript
open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.TypeChecker.Prune
open Escalier.TypeChecker.Env

module rec Codegen =
  module TS = TypeScript

  type Ctx =
    { mutable NextTempId: int
      mutable AutoImports: Set<string> }

    member this.GetTempId() =
      let tempId = $"temp{this.NextTempId}"
      this.NextTempId <- this.NextTempId + 1
      tempId

  type Finalizer =
    | Assign of string
    | Return
    | Empty

  let dummySpan =
    { Start = FParsec.Position("", 0, 0, 0)
      Stop = FParsec.Position("", 0, 0, 0) }

  let buildModule (ctx: Ctx) (m: Module) : TS.Module =
    let mutable stmts: list<Stmt> =
      m.Items
      |> List.choose (fun item ->
        match item with
        | ModuleItem.Stmt stmt -> Some stmt
        | _ -> None)

    let block = { Stmts = stmts; Span = dummySpan }
    let block = buildBlock ctx block Finalizer.Empty

    let mutable items: list<TS.ModuleItem> =
      List.map TS.ModuleItem.Stmt block.Body

    let mutable importMap = Map.empty
    importMap <- Map.add "_jsx" "react/jsx-runtime" importMap
    importMap <- Map.add "_jsxs" "react/jsx-runtime" importMap
    importMap <- Map.add "_Fragment" "react/jsx-runtime" importMap
    importMap <- Map.add "Record" "@bloomberg/record-tuple-polyfill" importMap
    importMap <- Map.add "Tuple" "@bloomberg/record-tuple-polyfill" importMap

    let mutable importsByPath: Map<string, Set<string>> = Map.empty

    for import in ctx.AutoImports do
      let path = Map.find import importMap

      let imports =
        match Map.tryFind path importsByPath with
        | Some imports -> imports
        | None -> Set.empty

      importsByPath <- Map.add path (Set.add import imports) importsByPath

    for KeyValue(path, imports) in importsByPath do
      let specifiers =
        imports
        |> Set.toList
        |> List.map (fun localName ->
          let importName =
            if localName.[0] = '_' then localName.[1..] else localName

          TS.ImportSpecifier.Named
            { Local = { Name = localName; Loc = None }
              Imported =
                if localName = importName then
                  None
                else
                  Some(
                    ModuleExportName.Ident { Name = importName; Loc = None }
                  )
              IsTypeOnly = false
              Loc = None })

      items <-
        ModuleItem.ModuleDecl(
          ModuleDecl.Import
            { Specifiers = specifiers
              Src = { Value = path; Raw = None; Loc = None }
              IsTypeOnly = false
              With = None
              Loc = None }
        )
        :: items

    { Body = items
      Shebang = None
      Loc = None }

  let flattenLogicalOr (expr: Expr) : list<Expr> =
    match expr.Kind with
    | ExprKind.Binary("||", left, right) ->
      flattenLogicalOr left @ flattenLogicalOr right
    | _ -> [ expr ]

  let flattenLogicalAnd (expr: Expr) : list<Expr> =
    match expr.Kind with
    | ExprKind.Binary("&&", left, right) ->
      flattenLogicalAnd left @ flattenLogicalAnd right
    | _ -> [ expr ]

  let buildTempDecl (ctx: Ctx) (tempId: string) : TS.Stmt =
    let tempDecl =
      { Decls =
          [ { Id =
                Pat.Ident
                  { Id = { Name = tempId; Loc = None }
                    Loc = None }
              TypeAnn = None
              Init = None } ]
        Declare = false
        Kind = VariableDeclarationKind.Var }

    tempDecl |> Decl.Var |> Stmt.Decl

  type Logical =
    | And
    | Or

  let buildLogicalRec
    (ctx: Ctx)
    (op: Logical)
    (exprs: list<Expr>)
    : TS.Expr * list<TS.Stmt> =
    match exprs with
    | [] -> failwith "Empty list of expressions"
    | [ expr ] -> buildExpr ctx expr
    | head :: tail ->
      let tempId = ctx.GetTempId()
      let finalizer = Finalizer.Assign tempId
      let tempDecl = buildTempDecl ctx tempId

      let headExpr, headStmts = buildExpr ctx head
      let tailExpr, tailStmts = buildLogicalRec ctx op tail

      let cond =
        match op with
        | Logical.Or -> headExpr
        | Logical.And ->
          Expr.Unary
            { Operator = UnaryOperator.Not
              Prefix = true
              Argument = headExpr
              Loc = None }

      let ifStmt =
        Stmt.If
          { Test = cond
            Consequent =
              Stmt.Block
                { Body = buildFinalizer ctx headExpr finalizer
                  Loc = None }
            Alternate =
              Some(
                Stmt.Block
                  { Body = tailStmts @ (buildFinalizer ctx tailExpr finalizer)
                    Loc = None }
              )
            Loc = None }

      let stmts = [ tempDecl ] @ headStmts @ [ ifStmt ]
      let expr = Expr.Ident { Name = tempId; Loc = None }

      (expr, stmts)

  let buildMatchRec
    (ctx: Ctx)
    (target: Expr)
    (defaultBlock: option<BlockStmt>)
    (cases: list<MatchCase>)
    : option<TS.Expr> * list<TS.Stmt> =
    match cases with
    | [] ->
      match defaultBlock with
      | None -> None, []
      | Some defaultBlock -> None, defaultBlock.Body
    | head :: tail ->
      let tempId = ctx.GetTempId()
      let finalizer = Finalizer.Assign tempId
      let tempDecl = buildTempDecl ctx tempId

      let targetExpr, targetStmts = buildExpr ctx target
      let pattern, checks = buildPattern ctx head.Pattern (Some targetExpr)
      let checkExprs = checks |> List.map (buildPatternCheck ctx)

      let conditionExpr =
        match checkExprs with
        | [] ->
          // TODO: Don't bother processing the `tail` since this is catch-all
          Expr.Lit(Lit.Bool { Value = true; Loc = None })
        | _ ->
          checkExprs
          |> List.reduce (fun acc check ->
            Expr.Bin
              { Operator = BinOp.LogicalAnd
                Left = acc
                Right = check
                Loc = None })

      let consequent =
        match head.Body with
        | BlockOrExpr.Block block ->
          buildBlock ctx block finalizer |> Stmt.Block
        | BlockOrExpr.Expr expr ->
          let headExpr, headStmts = buildExpr ctx expr

          Stmt.Block
            { Body = headStmts @ (buildFinalizer ctx headExpr finalizer)
              Loc = None }

      let tailExpr, tailStmts = buildMatchRec ctx target defaultBlock tail

      let alternative =
        match tailExpr with
        | Some tailExpr ->
          Some(
            Stmt.Block
              { Body = tailStmts @ (buildFinalizer ctx tailExpr finalizer)
                Loc = None }
          )
        | None ->
          match tailStmts with
          | [] -> None
          | _ -> Some(Stmt.Block { Body = tailStmts; Loc = None })

      let ifStmt =
        Stmt.If
          { Test = conditionExpr
            Consequent = consequent
            Alternate = alternative
            Loc = None }

      let stmts = [ tempDecl; ifStmt ]
      let expr = Expr.Ident { Name = tempId; Loc = None }

      (Some expr, stmts)

  let buildLiteral (lit: Literal) : TS.Expr =
    match lit with
    | Literal.Boolean b -> Lit.Bool { Value = b; Loc = None } |> Expr.Lit
    | Literal.String s ->
      Lit.Str { Value = s; Raw = None; Loc = None } |> Expr.Lit
    | Literal.Number n ->
      Lit.Num { Value = n; Raw = None; Loc = None } |> Expr.Lit
    | Literal.Null -> Lit.Null { Loc = None } |> Expr.Lit
    | Literal.Undefined ->
      { Ident.Name = "undefined"; Loc = None } |> Expr.Ident

  let buildExpr (ctx: Ctx) (expr: Expr) : TS.Expr * list<TS.Stmt> =

    match expr.Kind with
    | ExprKind.Call { Callee = callee; Args = args } ->
      let calleeExpr, calleeStmts = buildExpr ctx callee
      let argExprs, argStmts = args |> List.map (buildExpr ctx) |> List.unzip

      let callExpr =
        Expr.Call
          { Callee = calleeExpr
            Arguments = argExprs
            Loc = None }

      (callExpr, calleeStmts @ (argStmts |> List.concat))
    | ExprKind.New { Callee = callee; Args = args } ->
      let calleeExpr, calleeStmts = buildExpr ctx callee

      let args =
        match args with
        | Some args -> args
        | None -> []

      let argExprs, argStmts = args |> List.map (buildExpr ctx) |> List.unzip

      let callExpr =
        Expr.New
          { Callee = calleeExpr
            Arguments = argExprs
            Loc = None }

      (callExpr, calleeStmts @ (argStmts |> List.concat))
    | ExprKind.Identifier name -> Expr.Ident { Name = name; Loc = None }, []
    | ExprKind.Literal lit ->
      let lit =
        match lit with
        | Literal.Boolean b -> Lit.Bool { Value = b; Loc = None } |> Expr.Lit
        | Literal.String s ->
          Lit.Str { Value = s; Raw = None; Loc = None } |> Expr.Lit
        | Literal.Number n ->
          Lit.Num { Value = n; Raw = None; Loc = None } |> Expr.Lit
        | Literal.Null -> Lit.Null { Loc = None } |> Expr.Lit
        | Literal.Undefined ->
          { Ident.Name = "undefined"; Loc = None } |> Expr.Ident

      lit, []
    | ExprKind.Binary(op, left, right) ->
      if op = "&&" then
        buildLogicalRec ctx Logical.And (flattenLogicalAnd expr)
      elif op = "||" then
        buildLogicalRec ctx Logical.Or (flattenLogicalOr expr)
      else
        let op =
          match op with
          | "+" -> BinOp.Add
          | "-" -> BinOp.Sub
          | "*" -> BinOp.Mul
          | "/" -> BinOp.Div
          | "==" -> BinOp.EqEq
          | "!=" -> BinOp.NotEq
          | "<" -> BinOp.Lt
          | "<=" -> BinOp.LtEq
          | ">" -> BinOp.Gt
          | ">=" -> BinOp.GtEq
          | "++" -> BinOp.Add // Escalier uses "++" for string addition
          | _ -> failwith $"Invalid binary operator: {op}"

        let leftExpr, leftStmts = buildExpr ctx left
        let rightExpr, rightStmts = buildExpr ctx right

        let binExpr =
          Expr.Bin
            { Operator = op
              Left = leftExpr
              Right = rightExpr
              Loc = None }

        (binExpr, leftStmts @ rightStmts)
    | ExprKind.Do block ->
      let tempId = ctx.GetTempId()
      let finalizer = Finalizer.Assign tempId
      let tempDecl = buildTempDecl ctx tempId

      let blockStmt = buildBlock ctx block finalizer
      let expr = Expr.Ident { Name = tempId; Loc = None }

      let stmts = [ tempDecl; Stmt.Block blockStmt ]

      (expr, stmts)
    | ExprKind.Function { Sig = s; Body = body } ->
      match body with
      | BlockOrExpr.Block block ->
        let ps: list<Param> =
          s.ParamList
          |> List.map (fun (p: Syntax.FuncParam) ->
            let pat, _ = buildPattern ctx p.Pattern None

            { Pat = pat
              TypeAnn = None
              Loc = None })

        let body = buildBlock ctx block Finalizer.Empty

        let func: TS.Function =
          { Params = ps
            Body = Some({ Body = body.Body; Loc = None })
            IsGenerator = false
            IsAsync = false
            TypeParams = None
            ReturnType = None
            Loc = None }

        let expr = Expr.Fn { Id = None; Fn = func }
        let stmts = []

        (expr, stmts)
      | BlockOrExpr.Expr expr ->
        let ps =
          s.ParamList
          |> List.map (fun p ->
            let pat, _ = buildPattern ctx p.Pattern None
            pat)

        let bodyExpr, bodyStmts = buildExpr ctx expr

        let body =
          bodyStmts @ [ Stmt.Return { Argument = Some bodyExpr; Loc = None } ]

        let body: BlockStmt = { Body = body; Loc = None }
        let expr = Expr.Arrow { Params = ps; Body = body }

        (expr, [])
    | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
      let tempId = ctx.GetTempId()
      let finalizer = Finalizer.Assign tempId
      let tempDecl = buildTempDecl ctx tempId

      let conditionExpr, conditionStmts = buildExpr ctx condition
      let thenBlock = buildBlock ctx thenBranch finalizer

      let alt =
        Option.map
          (fun elseBranch ->
            match elseBranch with
            | BlockOrExpr.Block block -> buildBlock ctx block finalizer
            | BlockOrExpr.Expr expr ->
              let expr, stmts = buildExpr ctx expr
              let finalizer = buildFinalizer ctx expr finalizer
              { Body = stmts @ finalizer; Loc = None })
          elseBranch

      let ifStmt =
        Stmt.If
          { Test = conditionExpr
            Consequent = Stmt.Block thenBlock
            Alternate = Option.map Stmt.Block alt
            Loc = None }

      let stmts = [ tempDecl ] @ conditionStmts @ [ ifStmt ]
      let expr = Expr.Ident { Name = tempId; Loc = None }

      (expr, stmts)
    | ExprKind.Member(target, name, opt_chain) ->

      // TODO: support outputting optional chaining
      let object, objStmts = buildExpr ctx target

      let property = Expr.Ident { Name = name; Loc = None }

      let expr =
        Expr.Member
          { Object = object
            Property = property
            Computed = false
            OptChain = opt_chain
            Loc = None }

      let stmts = objStmts

      (expr, stmts)
    | ExprKind.Object { Elems = elems; Immutable = immutable } ->
      let mutable stmts: list<TS.Stmt> = []

      let properties: list<TS.Property> =
        elems
        |> List.map (fun elem ->
          match elem with
          | ObjElem.Property(span, name, value) ->
            let (value, valueStmts) = buildExpr ctx value
            stmts <- stmts @ valueStmts

            let key =
              match name with
              | Syntax.PropName.Ident name ->
                PropertyKey.Ident { Name = name; Loc = None }
              | Syntax.PropName.String value ->
                PropertyKey.Lit(
                  Lit.Str
                    { Value = value
                      Raw = None
                      Loc = None }
                )
              | Syntax.PropName.Number value ->
                PropertyKey.Lit(
                  Lit.Num
                    { Value = value
                      Raw = None
                      Loc = None }
                )
              | Syntax.PropName.Computed expr ->
                failwith "TODO: Computed property names"

            { Key = key
              Value = value
              Kind = PropertyKind.Init
              Loc = None }
          | ObjElem.Shorthand(span, name) ->
            failwith "TODO - ObjElem.Shorthand"
          | ObjElem.Spread(span, value) -> failwith "TODO - ObjElem.Spread")

      let obj: TS.ObjectLit = { Properties = properties; Loc = None }

      let expr =
        if immutable then
          ctx.AutoImports <- Set.add "Record" ctx.AutoImports

          Expr.Call
            { Callee = Expr.Ident { Name = "Record"; Loc = None }
              Arguments = [ Expr.Object obj ]
              Loc = None }
        else
          Expr.Object obj

      (expr, [])
    | ExprKind.ExprWithTypeArgs(target, typeArgs) ->
      failwith "TODO: buildExpr - ExprWithTypeArgs"
    | ExprKind.Class ``class`` -> failwith "TODO: buildExpr - Class"
    | ExprKind.Tuple { Elems = elems; Immutable = immutable } ->
      let mutable stmts: list<TS.Stmt> = []

      let elems: list<option<TS.ExprOrSpread>> =
        elems
        |> List.map (fun elem ->
          let elemExpr, elemStmts = buildExpr ctx elem
          stmts <- stmts @ elemStmts
          Some({ Expr = elemExpr; Spread = false }))

      let tuple = TS.Expr.Array { Elements = elems; Loc = None }

      let tuple =
        if immutable then
          ctx.AutoImports <- Set.add "Tuple" ctx.AutoImports

          Expr.Call
            { Callee = Expr.Ident { Name = "Tuple"; Loc = None }
              Arguments = [ tuple ]
              Loc = None }
        else
          tuple

      (tuple, stmts)
    | ExprKind.Range range -> failwith "TODO: buildExpr - Range"
    | ExprKind.Index(target, index, optChain) ->
      let targetExpr, targetStmts = buildExpr ctx target
      let indexExpr, indexStmts = buildExpr ctx index

      let expr =
        Expr.Member
          { Object = targetExpr
            Property = indexExpr
            Computed = true
            OptChain = optChain
            Loc = None }

      (expr, targetStmts @ indexStmts)
    | ExprKind.IfLet(pattern, target, thenBranch, elseBranch) ->
      let tempId = ctx.GetTempId()
      let finalizer = Finalizer.Assign tempId
      let tempDecl = buildTempDecl ctx tempId

      let targetExpr, targetStmts = buildExpr ctx target
      let pattern, checks = buildPattern ctx pattern (Some targetExpr)

      let checkExprs = checks |> List.map (buildPatternCheck ctx)

      let conditionExpr =
        checkExprs
        |> List.reduce (fun acc check ->
          Expr.Bin
            { Operator = BinOp.LogicalAnd
              Left = acc
              Right = check
              Loc = None })

      let decl =
        { Decls =
            [ { Id = pattern
                TypeAnn = None
                Init = Some targetExpr } ]
          Declare = false
          Kind = VariableDeclarationKind.Var }
        |> Decl.Var
        |> Stmt.Decl

      let thenBlock = buildBlock ctx thenBranch finalizer

      let thenBlock =
        { thenBlock with
            Body = decl :: thenBlock.Body }

      let alt =
        Option.map
          (fun elseBranch ->
            match elseBranch with
            | BlockOrExpr.Block block -> buildBlock ctx block finalizer
            | BlockOrExpr.Expr expr ->
              let expr, stmts = buildExpr ctx expr
              let finalizer = buildFinalizer ctx expr finalizer
              { Body = stmts @ finalizer; Loc = None })
          elseBranch

      let ifStmt =
        Stmt.If
          { Test = conditionExpr
            Consequent = Stmt.Block thenBlock
            Alternate = Option.map Stmt.Block alt
            Loc = None }

      let stmts = [ tempDecl; ifStmt ]
      let expr = Expr.Ident { Name = tempId; Loc = None }

      (expr, stmts)
    | ExprKind.Match(target, cases) ->
      match buildMatchRec ctx target None cases with
      | Some expr, stmts -> (expr, stmts)
      | None, stmts -> failwith "Failure compiling 'match' expression"
    | ExprKind.Assign(op, left, right) ->
      let leftExpr, leftStmts = buildExpr ctx left
      let rightExpr, rightStmts = buildExpr ctx right

      let op =
        match op with
        | "=" -> AssignOp.Assign
        | "+=" -> AssignOp.PlusAssign
        | "-=" -> AssignOp.MinusAssign
        | "*=" -> AssignOp.MultiplyAssign
        | "/=" -> AssignOp.DivideAssign
        | "%=" -> AssignOp.ModuloAssign
        | "&=" -> AssignOp.BitwiseAndAssign
        | "|=" -> AssignOp.BitwiseOrAssign
        | "^=" -> AssignOp.BitwiseXorAssign
        | "<<=" -> AssignOp.LeftShiftAssign
        | ">>=" -> AssignOp.RightShiftAssign
        | ">>>=" -> AssignOp.UnsignedRightShiftAssign
        | _ -> failwith $"Invalid assignment operator: {op}"

      let assign =
        Expr.Assign
          { Operator = op
            Left = leftExpr
            Right = rightExpr
            Loc = None }

      (assign, leftStmts @ rightStmts)
    | ExprKind.Unary(op, value) ->
      let valueExpr, valueStmts = buildExpr ctx value

      let op =
        match op with
        | "-" -> UnaryOperator.Minus
        | "+" -> UnaryOperator.Plus
        | "!" -> UnaryOperator.Not
        | "~" -> UnaryOperator.BitwiseNot
        | "typeof" -> UnaryOperator.Typeof
        | "void" -> UnaryOperator.Void
        | "delete" -> UnaryOperator.Delete
        | _ -> failwith $"Invalid unary operator: {op}"

      let unaryExpr =
        Expr.Unary
          { Operator = op
            Prefix = true
            Argument = valueExpr
            Loc = None }

      (unaryExpr, valueStmts)
    | ExprKind.Try { Body = body
                     Catch = catchBlock
                     Finally = finallyBlock } ->
      let tempId = ctx.GetTempId()
      let finalizer = Finalizer.Assign tempId
      let tempDecl = buildTempDecl ctx tempId

      let bodyBlock = buildBlock ctx body finalizer

      let target: Expr =
        { Kind = ExprKind.Identifier "__error__"
          Span = dummySpan
          InferredType = None }

      let handler =
        match catchBlock with
        | Some cases ->
          // Rethrow the error if it doesn't match any of the cases.
          let throwBlock: BlockStmt =
            { Body =
                [ Stmt.Throw
                    { Argument = Expr.Ident { Name = "__error__"; Loc = None }
                      Loc = None } ]
              Loc = None }

          let catchExpr, catchStmts =
            buildMatchRec ctx target (Some throwBlock) cases

          let handler =
            { Param =
                Pat.Ident
                  { Id = { Name = "__error__"; Loc = None }
                    Loc = None }
              TypeAnn = None
              Body = { Body = catchStmts; Loc = None } }

          Some handler
        | None -> None

      let finallyBlock =
        finallyBlock
        |> Option.map (fun block -> buildBlock ctx block Finalizer.Empty)

      let tryStmt: TS.Stmt =
        Stmt.Try
          { TryBlock = bodyBlock
            Handler = handler
            Finalizer = finallyBlock }

      let expr = Expr.Ident { Name = tempId; Loc = None }

      expr, [ tempDecl; tryStmt ]
    | ExprKind.Await { Value = value } ->
      let valueExpr, valueStmts = buildExpr ctx value
      let expr = Expr.Await { Arg = valueExpr; Loc = None }
      (expr, valueStmts)
    | ExprKind.Throw value -> failwith "TODO: buildExpr - Throw"
    | ExprKind.TemplateLiteral template ->
      let tpl, stmts = buildTemplateLiteral ctx template
      (Expr.Tpl tpl, stmts)
    | ExprKind.TaggedTemplateLiteral(tag, template, _throws) ->
      let tagExpr, tagStmts = buildExpr ctx tag
      let tpl, tplStmts = buildTemplateLiteral ctx template

      let taggedTplExpr =
        Expr.TaggedTpl
          { Tag = tagExpr
            TypeParams = None
            Tpl = tpl
            Loc = None }

      (taggedTplExpr, tagStmts @ tplStmts)
    | ExprKind.JSXElement jsxElement -> buildJsxElement ctx jsxElement
    | ExprKind.JSXFragment jsxFragment -> buildJsxFragment ctx jsxFragment

  let buildTemplateLiteral
    (ctx: Ctx)
    (template: Common.TemplateLiteral<Expr>)
    : TS.Tpl * list<TS.Stmt> =
    let { Exprs = exprs; Parts = parts } = template
    let mutable stmts = []

    let exprs =
      exprs
      |> List.map (fun expr ->
        let expr, exprStmts = buildExpr ctx expr
        stmts <- stmts @ exprStmts
        expr)

    let quasis =
      parts
      |> List.map (fun part ->
        { Tail = false
          Cooked = None
          Raw = part
          Loc = None })

    let tpl =
      { Exprs = exprs
        Quasis = quasis
        Loc = None }

    (tpl, stmts)

  let buildJsxElement
    (ctx: Ctx)
    (jsxElement: JSXElement)
    : TS.Expr * list<TS.Stmt> =

    let { JSXElement.Opening = { Name = name; Attrs = attrs }
          Children = children } =
      jsxElement

    let mutable stmts: list<TS.Stmt> = []

    let mutable properties: list<TS.Property> =
      attrs
      |> List.map (fun attr ->
        let value =
          match attr.Value with
          | None -> failwith "TODO: treat this the same as `obj = {x, y}`"
          | Some value ->
            match value with
            | Str(Common.Literal.String s) ->
              Lit.Str { Value = s; Raw = None; Loc = None } |> Expr.Lit
            | Str _ -> failwith "Not a valid string literal"
            | JSXAttrValue.JSXExprContainer jsxExprContainer ->
              let expr, exprStmts = buildExpr ctx jsxExprContainer.Expr
              stmts <- stmts @ exprStmts
              expr
            | JSXAttrValue.JSXElement jsxElement ->
              let jsxElemExpr, jsxElemStmts = buildJsxElement ctx jsxElement
              stmts <- stmts @ jsxElemStmts
              jsxElemExpr
            | JSXAttrValue.JSXFragment jsxFragment ->
              let jsxFragExpr, jsxFragStmts = buildJsxFragment ctx jsxFragment
              stmts <- stmts @ jsxFragStmts
              jsxFragExpr

        { Key = PropertyKey.Ident { Name = attr.Name; Loc = None }
          Value = value
          Kind = PropertyKind.Init
          Loc = None })

    let children: list<option<ExprOrSpread>> =
      children
      |> List.map (fun child ->
        match child with
        | JSXElementChild.JSXElement jsxElement ->
          let jsxElemExpr, jsxElemStmts = buildJsxElement ctx jsxElement
          stmts <- stmts @ jsxElemStmts
          Some({ Spread = false; Expr = jsxElemExpr })
        | JSXElementChild.JSXFragment jsxFragment ->
          let jsxFragExpr, jsxFragStmts = buildJsxFragment ctx jsxFragment
          stmts <- stmts @ jsxFragStmts
          Some({ Spread = false; Expr = jsxFragExpr })
        | JSXElementChild.JSXText { Text = text } ->
          let jsxTextExpr =
            Lit.Str { Value = text; Raw = None; Loc = None } |> Expr.Lit

          Some({ Spread = false; Expr = jsxTextExpr })
        | JSXElementChild.JSXExprContainer jsxExprContainer ->
          let expr, exprStmts = buildExpr ctx jsxExprContainer.Expr
          stmts <- stmts @ exprStmts
          Some({ Spread = false; Expr = expr }))

    if not children.IsEmpty then
      let childrenValue = Expr.Array { Elements = children; Loc = None }

      let childrenProp =
        { Key = PropertyKey.Ident { Name = "children"; Loc = None }
          Value = childrenValue
          Kind = PropertyKind.Init
          Loc = None }

      properties <- properties @ [ childrenProp ]

    let propsObj = Expr.Object { Properties = properties; Loc = None }

    let componentExpr =
      match name with
      | QualifiedIdent.Ident s when System.Char.IsLower(s, 0) ->
        Expr.Lit(Lit.Str { Value = s; Raw = None; Loc = None })
      | _ -> failwith "TODO: buildJsxElement - JSXElement"

    let callExpr =
      let name =
        if children.Length > 1 then
          ctx.AutoImports <- Set.add "_jsxs" ctx.AutoImports
          "_jsxs"
        else
          ctx.AutoImports <- Set.add "_jsx" ctx.AutoImports
          "_jsx"

      Expr.Call
        { Callee = Expr.Ident { Name = name; Loc = None }
          Arguments = [ componentExpr; propsObj ]
          Loc = None }

    (callExpr, stmts)

  let buildJsxFragment
    (ctx: Ctx)
    (jsxFragment: JSXFragment)
    : TS.Expr * list<TS.Stmt> =
    let mutable stmts: list<TS.Stmt> = []

    let { Children = children } = jsxFragment

    let children: list<option<ExprOrSpread>> =
      children
      |> List.map (fun child ->
        match child with
        | JSXElementChild.JSXElement jsxElement ->
          let jsxElementExpr, jsxElementStmts = buildJsxElement ctx jsxElement

          stmts <- stmts @ jsxElementStmts

          Some(
            { Spread = false
              Expr = jsxElementExpr }
          )
        | JSXElementChild.JSXFragment jsxFragment ->
          let jsxElementExpr, jsxElementStmts =
            buildJsxFragment ctx jsxFragment

          stmts <- stmts @ jsxElementStmts

          Some(
            { Spread = false
              Expr = jsxElementExpr }
          )
        | JSXElementChild.JSXText { Text = text } ->
          let jsxTextExpr =
            Lit.Str { Value = text; Raw = None; Loc = None } |> Expr.Lit

          Some({ Spread = false; Expr = jsxTextExpr })
        | JSXElementChild.JSXExprContainer jsxExprContainer ->
          let expr, exprStmts = buildExpr ctx jsxExprContainer.Expr
          stmts <- stmts @ exprStmts
          Some({ Spread = false; Expr = expr }))

    let mutable properties: list<TS.Property> = []

    if not children.IsEmpty then
      let childrenValue = Expr.Array { Elements = children; Loc = None }

      let childrenProp =
        { Key = PropertyKey.Ident { Name = "children"; Loc = None }
          Value = childrenValue
          Kind = PropertyKind.Init
          Loc = None }

      properties <- properties @ [ childrenProp ]

    let propsObj = Expr.Object { Properties = properties; Loc = None }

    let callExpr =
      let name =
        if children.Length > 1 then
          ctx.AutoImports <- Set.add "_jsxs" ctx.AutoImports
          "_jsxs"
        else
          ctx.AutoImports <- Set.add "_jsx" ctx.AutoImports
          "_jsx"

      let componentExpr = Expr.Ident { Name = "_Fragment"; Loc = None }
      ctx.AutoImports <- Set.add "_Fragment" ctx.AutoImports

      Expr.Call
        { Callee = Expr.Ident { Name = name; Loc = None }
          Arguments = [ componentExpr; propsObj ]
          Loc = None }

    (callExpr, stmts)

  let buildTypeof (name: string) : TS.Expr =
    TS.Expr.Unary
      { Operator = UnaryOperator.Typeof
        Prefix = true
        Argument = TS.Expr.Ident { Name = name; Loc = None }
        Loc = None }

  let buildString (value: string) : TS.Expr =
    TS.Expr.Lit(
      TS.Lit.Str
        { Value = value
          Raw = None
          Loc = None }
    )

  let checkExprFromParam (param: Syntax.FuncParam) : TS.Expr =
    match param.Pattern.InferredType with
    | Some t ->
      match param.Pattern.Kind with
      | PatternKind.Ident { Name = name } ->
        match (prune t).Kind with
        | TypeKind.Primitive(Primitive.Number) ->
          TS.Expr.Bin
            { Operator = BinOp.EqEq
              Left = buildTypeof name
              Right = buildString "number"
              Loc = None }
        | TypeKind.Primitive(Primitive.String) ->
          TS.Expr.Bin
            { Operator = BinOp.EqEq
              Left = buildTypeof name
              Right = buildString "string"
              Loc = None }
        | _ -> failwith "TODO - checkExprFromParam"
      | _ -> failwith "Function param pattern must be an identifier"
    | None -> failwith "Function param must have an inferred type"

  let buildBlock (ctx: Ctx) (body: Block) (finalizer: Finalizer) : BlockStmt =
    // TODO: check if the last statement is an expression statement
    // and use the appropriate finalizer with it

    let mutable stmts: list<TS.Stmt> = []
    let lastStmt = body.Stmts |> List.last

    let mutable fnDecls: Map<string, list<FnDecl>> = Map.empty

    for stmt in body.Stmts do
      match stmt.Kind with
      | StmtKind.Decl { Kind = DeclKind.FnDecl fnDecl } ->
        let decls =
          match Map.tryFind fnDecl.Name fnDecls with
          | Some decls -> decls
          | None -> []

        fnDecls <- Map.add fnDecl.Name (decls @ [ fnDecl ]) fnDecls
      | _ -> ()

    for stmt in body.Stmts do
      let stmts' =
        match stmt.Kind with
        | StmtKind.Expr expr ->
          let expr, stmts = buildExpr ctx expr
          let stmtExpr = Stmt.Expr { Expr = expr; Loc = None }

          if stmt = lastStmt && finalizer <> Finalizer.Empty then
            stmts @ buildFinalizer ctx expr finalizer
          else
            stmts @ [ stmtExpr ]
        | StmtKind.Decl decl ->
          match decl.Kind with
          | VarDecl { Pattern = pattern; Init = Some init } ->
            let pattern, _ = buildPattern ctx pattern None
            let initExpr, initStmts = buildExpr ctx init

            let decl =
              { Decls =
                  [ { Id = pattern
                      TypeAnn = None
                      Init = Some initExpr } ]
                Declare = false
                Kind = VariableDeclarationKind.Var }

            let declStmt = Stmt.Decl(Decl.Var decl)

            initStmts @ [ declStmt ]
          | TypeDecl _ -> [] // Ignore types when generating JS code
          | VarDecl _ -> [] // Nothing to generate because `init` is `None`
          | FnDecl fnDecl ->
            match Map.tryFind fnDecl.Name fnDecls with
            | Some decls ->
              // Remove the function declaration from the map so that we only
              // generate a single function declaration for it.
              fnDecls <- Map.remove fnDecl.Name fnDecls

              match decls with
              | [] -> failwith "Function declaration list must not be empty"
              | [ decl ] ->
                let ps: list<Param> =
                  decl.Sig.ParamList
                  |> List.map (fun (p: Syntax.FuncParam) ->
                    let pat, _ = buildPattern ctx p.Pattern None

                    { Pat = pat
                      TypeAnn = None
                      Loc = None })

                let body =
                  match decl.Body with
                  | Some(BlockOrExpr.Block block) ->
                    buildBlock ctx block Finalizer.Empty
                  | Some(BlockOrExpr.Expr expr) ->
                    let expr, stmts = buildExpr ctx expr

                    let body =
                      stmts
                      @ [ Stmt.Return { Argument = Some expr; Loc = None } ]

                    { Body = body; Loc = None }
                  | None -> failwith "Function declaration must have a body"

                let func: TS.Function =
                  { Params = ps
                    Body = Some({ Body = body.Body; Loc = None })
                    IsGenerator = false
                    IsAsync = false
                    TypeParams = None
                    ReturnType = None
                    Loc = None }

                let fnDecl =
                  { FnDecl.Id = { Name = fnDecl.Name; Loc = None }
                    Declare = false
                    Fn = func }

                let funcDecl = Stmt.Decl(Decl.Fn fnDecl)

                [ funcDecl ]
              | decls ->

                let ps =
                  decls[0].Sig.ParamList
                  |> List.map (fun p ->
                    let pat, _ = buildPattern ctx p.Pattern None

                    { Pat = pat
                      TypeAnn = None
                      Loc = None })

                let typeError =
                  TS.Expr.New
                    { Callee = TS.Expr.Ident { Name = "TypeError"; Loc = None }
                      Arguments = []
                      Loc = None }

                let throw = TS.Stmt.Throw { Argument = typeError; Loc = None }
                let block: TS.BlockStmt = { Body = [ throw ]; Loc = None }
                let mutable ifElse: TS.Stmt = TS.Stmt.Block(block)

                // NOTE: We reverse the order of the declarations because the
                // if-else chain is built in reverse order.
                for decl in List.rev decls do
                  let checks =
                    decl.Sig.ParamList |> List.map (checkExprFromParam)

                  let check =
                    checks
                    |> List.reduce (fun acc check ->
                      TS.Expr.Bin
                        { Operator = BinOp.LogicalAnd
                          Left = acc
                          Right = check
                          Loc = None })

                  let body =
                    match decl.Body with
                    | Some(BlockOrExpr.Block block) ->
                      buildBlock ctx block Finalizer.Empty
                    | Some(BlockOrExpr.Expr expr) ->
                      let expr, stmts = buildExpr ctx expr

                      let body =
                        stmts
                        @ [ Stmt.Return { Argument = Some expr; Loc = None } ]

                      { Body = body; Loc = None }
                    | None -> failwith "Function declaration must have a body"

                  ifElse <-
                    TS.Stmt.If
                      { Test = check
                        Consequent = TS.Stmt.Block(body)
                        Alternate = Some(ifElse)
                        Loc = None }

                let func: TS.Function =
                  { Params = ps
                    Body = Some({ Body = [ ifElse ]; Loc = None })
                    IsGenerator = false
                    IsAsync = false
                    TypeParams = None
                    ReturnType = None
                    Loc = None }

                let ident: TS.Ident = { Name = fnDecl.Name; Loc = None }

                let fnDecl =
                  { FnDecl.Id = { Name = fnDecl.Name; Loc = None }
                    Declare = false
                    Fn = func }

                let funcDecl = Stmt.Decl(Decl.Fn fnDecl)

                [ funcDecl ]
            | None -> []
          | ClassDecl(_) -> failwith "TODO: buildBlock - ClassDecl"
          | EnumDecl(_) -> failwith "TODO: buildBlock - EnumDecl"
          | NamespaceDecl(_) -> failwith "TODO: buildBlock - NamespaceDecl"
          | InterfaceDecl(_) -> [] // Ignore types when generating JS code
        | StmtKind.Return expr ->
          match expr with
          | Some expr ->
            let expr, stmts = buildExpr ctx expr
            let retStmt = Stmt.Return { Argument = Some expr; Loc = None }
            stmts @ [ retStmt ]
          | None -> [ Stmt.Return { Argument = None; Loc = None } ]
        | StmtKind.For(left, right, body) -> failwith "TODO: buildBlock - For"

      stmts <- stmts @ stmts'

    { Body = stmts; Loc = None }

  let buildFinalizer
    (ctx: Ctx)
    (expr: TS.Expr)
    (finalizer: Finalizer)
    : list<TS.Stmt> =
    match finalizer with
    | Finalizer.Assign id ->
      let left = Expr.Ident { Name = id; Loc = None }

      let assignStmt =
        Stmt.Expr
          { Expr =
              Expr.Assign
                { Operator = AssignOp.Assign
                  Left = left
                  Right = expr
                  Loc = None }
            Loc = None }

      [ assignStmt ]

    | Finalizer.Return -> [ Stmt.Return { Argument = None; Loc = None } ]
    | Finalizer.Empty -> []

  type PatternCheck =
    | ObjectCheck of TS.Expr
    | PropertyCheck of TS.Expr * string // parent, property
    | ArrayCheck of TS.Expr * int // parent, length
    | ValueCheck of TS.Expr * TS.Expr
    | InstanceOfCheck of TS.Expr * TS.Expr
    | TypeOfCheck of TS.Expr * string

  let buildPatternCheck (ctx: Ctx) (check: PatternCheck) : TS.Expr =
    match check with
    | ObjectCheck expr ->
      let typeof =
        Expr.Unary
          { Operator = UnaryOperator.Typeof
            Prefix = true
            Argument = expr
            Loc = None }

      Expr.Bin
        { Operator = BinOp.EqEq
          Left = typeof
          Right =
            Expr.Lit(
              Lit.Str
                { Value = "object"
                  Raw = None
                  Loc = None }
            )
          Loc = None }
    | PropertyCheck(expr, s) ->
      Expr.Bin
        { Operator = BinOp.In
          Left = Expr.Lit(Lit.Str { Value = s; Raw = None; Loc = None })
          Right = expr
          Loc = None }
    | ArrayCheck(expr, i) ->
      let exprDotLength =
        Expr.Member
          { Object = expr
            Property = Expr.Ident { Name = "length"; Loc = None }
            Computed = false
            OptChain = false
            Loc = None }

      let length =
        Expr.Lit(
          Lit.Num
            { Value = Int i
              Raw = None
              Loc = None }
        )

      Expr.Bin
        { Operator = BinOp.EqEq
          Left = exprDotLength
          Right = length
          Loc = None }
    | ValueCheck(left, right) ->
      Expr.Bin
        { Operator = BinOp.EqEq
          Left = left
          Right = right
          Loc = None }
    | InstanceOfCheck(expr, expr1) -> failwith "todo"
    | TypeOfCheck(expr, s) -> failwith "todo"

  let buildPattern
    (ctx: Ctx)
    (pattern: Syntax.Pattern)
    (parent: option<TS.Expr>)
    : TS.Pat * list<PatternCheck> =
    match pattern.Kind with
    | PatternKind.Ident { Name = name } ->
      let pat =
        Pat.Ident
          { Id = { Name = name; Loc = None }
            Loc = None }

      pat, []
    | PatternKind.Object { Elems = elems } ->
      let mutable checks: list<PatternCheck> = []

      parent
      |> Option.iter (fun parent -> checks <- checks @ [ ObjectCheck parent ])

      let props =
        elems
        |> List.choose (fun elem ->
          match elem with
          | Syntax.ObjPatElem.KeyValuePat { Key = key
                                            Value = value
                                            Default = _ } ->
            let parent =
              parent
              |> Option.map (fun parent ->
                checks <- checks @ [ PropertyCheck(parent, key) ]

                TS.Expr.Member
                  { Object = parent
                    Property =
                      TS.Expr.Lit(
                        Lit.Str { Value = key; Raw = None; Loc = None }
                      )
                    Computed = true
                    OptChain = false
                    Loc = None })

            let keyValuePat, keyValueChecks = buildPattern ctx value parent
            checks <- checks @ keyValueChecks

            match keyValuePat with
            | Pat.Invalid _ -> None
            | _ ->
              // TODO: add support for default values in ObjectPatProp.KeyValue
              Some(
                ObjectPatProp.KeyValue
                  { Key = TS.PropName.Ident { Name = key; Loc = None }
                    Value = keyValuePat
                    Loc = None }
              )
          | Syntax.ObjPatElem.ShorthandPat { Name = name
                                             Default = init
                                             Assertion = _ } ->

            parent
            |> Option.iter (fun parent ->
              checks <- checks @ [ PropertyCheck(parent, name) ])

            Some(
              ObjectPatProp.Assign
                { Key = { Name = name; Loc = None }
                  Value = None // TODO: handle default values
                  Loc = None }
            )
          | Syntax.ObjPatElem.RestPat { Target = target } ->
            let argPat, argExprs = buildPattern ctx target parent
            checks <- checks @ argExprs

            match argPat with
            | Pat.Invalid _ -> None
            | _ -> Some(ObjectPatProp.Rest { Arg = argPat; Loc = None }))

      Pat.Object { Props = props; Loc = None }, checks
    | PatternKind.Tuple { Elems = elems } ->
      let mutable checks: list<PatternCheck> = []

      parent
      |> Option.iter (fun parent ->
        checks <- checks @ [ ArrayCheck(parent, elems.Length) ])

      let elems =
        elems
        |> List.mapi (fun index elem ->

          let parent =
            parent
            |> Option.map (fun parent ->

              TS.Expr.Member
                { Object = parent
                  Property =
                    TS.Expr.Lit(
                      Lit.Num
                        { Value = Int index
                          Raw = None
                          Loc = None }
                    )
                  Computed = true
                  OptChain = false
                  Loc = None })

          let elemPat, elemChecks = buildPattern ctx elem parent
          checks <- checks @ elemChecks

          match elemPat with
          | Pat.Invalid _ -> None
          | _ -> Some elemPat)

      Pat.Array { Elems = elems; Loc = None }, checks
    | PatternKind.Enum enumVariantPattern ->
      failwith "TODO: buildPattern - Enum"
    | PatternKind.Wildcard wildcardPattern ->
      failwith "TODO: buildPattern - Wildcard"
    | PatternKind.Literal literal ->
      let checks =
        match parent with
        | Some parent -> [ ValueCheck(parent, buildLiteral literal) ]
        | None -> []

      Pat.Invalid { Loc = None }, checks
    | PatternKind.Rest pattern ->
      let argPat, argExprs = buildPattern ctx pattern parent

      Pat.Rest { Arg = argPat; Loc = None }, argExprs

  // TODO: our ModuleItem enum should contain: Decl and Imports
  // TODO: pass in `env: Env` so that we can look up the types of
  // the exported symbols since we aren't tracking provenance consistently yet
  let buildModuleTypes (env: Env) (ctx: Ctx) (m: Module) : TS.Module =
    let mutable items: list<TS.ModuleItem> = []

    for item in m.Items do
      match item with
      | ModuleItem.Import { Path = path; Specifiers = specifiers } ->
        let specifiers =
          specifiers
          |> List.map (fun spec ->

            match spec with
            | ImportSpecifier.Named(name, alias) ->
              match alias with
              | None ->
                let local: TS.Ident = { Name = name; Loc = None }

                TS.ImportSpecifier.Named
                  { Local = local
                    Imported = None
                    IsTypeOnly = false
                    Loc = None }
              | Some value ->
                let local: TS.Ident = { Name = value; Loc = None }
                let imported: TS.Ident = { Name = name; Loc = None }

                TS.ImportSpecifier.Named
                  { Local = local
                    Imported = Some(ModuleExportName.Ident imported)
                    IsTypeOnly = false
                    Loc = None }

            | ImportSpecifier.ModuleAlias(alias) ->
              let local: TS.Ident = { Name = alias; Loc = None }
              TS.ImportSpecifier.Namespace { Local = local; Loc = None })

        let decl =
          ModuleDecl.Import
            { Specifiers = specifiers
              Src = { Value = path; Raw = None; Loc = None }
              IsTypeOnly = false
              With = None
              Loc = None }

        let item = ModuleItem.ModuleDecl(decl)
        items <- item :: items
      | ModuleItem.Stmt stmt ->
        match stmt.Kind with
        | StmtKind.Decl decl ->
          match decl.Kind with
          | TypeDecl { Name = name; TypeAnn = typeAnn } ->
            match typeAnn.InferredType with
            | Some(typeAnn) ->
              let decl =
                TS.Decl.TsTypeAlias
                  { Declare = false
                    Id = { Name = name; Loc = None }
                    TypeParams = None // TODO: typeParams
                    TypeAnn = buildType ctx typeAnn
                    Loc = None }

              let item =
                ModuleItem.ModuleDecl(
                  ModuleDecl.ExportDecl { Decl = decl; Loc = None }
                )

              items <- item :: items
            | None -> ()
          | VarDecl { Pattern = pattern } ->
            for Operators.KeyValue(name, _) in findBindings pattern do
              let n: string = name

              let t =
                match env.GetValue name with
                | Ok(t) -> t
                | Error(e) -> failwith $"Couldn't find symbol: {name}"

              let decl =
                { Id =
                    Pat.Ident
                      { Id = { Name = n; Loc = None }
                        Loc = None }
                  TypeAnn = Some(buildTypeAnn ctx t)
                  Init = None }

              let varDecl =
                TS.Decl.Var
                  { Decls = [ decl ]
                    Declare = true
                    Kind = VariableDeclarationKind.Const }

              let item =
                ModuleItem.ModuleDecl(
                  ModuleDecl.ExportDecl { Decl = varDecl; Loc = None }
                )

              items <- item :: items
          | FnDecl(_) -> failwith "TODO: buildModuleTypes - FnDecl"
          | ClassDecl(_) -> failwith "TODO: buildModuleTypes - ClassDecl"
          | EnumDecl(_) -> failwith "TODO: buildModuleTypes - EnumDecl"
          | NamespaceDecl(_) ->
            failwith "TODO: buildModuleTypes - NamespaceDecl"
          | InterfaceDecl(_) ->
            failwith "TODO: buildModuleTypes - InterfaceDecl"
        | _ -> ()

    { Body = List.rev items
      Shebang = None
      Loc = None }

  let buildTypeAnn (ctx: Ctx) (t: Type) : TsTypeAnn =
    { TypeAnn = buildType ctx t
      Loc = None }

  let qualifiedIdentToTsEntityName (id: QualifiedIdent) : TsEntityName =
    match id with
    | QualifiedIdent.Ident name ->
      TsEntityName.Identifier { Name = name; Loc = None }
    | QualifiedIdent.Member(qualifier, name) ->
      TsEntityName.TsQualifiedName
        { Left = qualifiedIdentToTsEntityName qualifier
          Right = { Name = name; Loc = None } }

  let buildType (ctx: Ctx) (t: Type) : TsType =
    let t = prune t

    match t.Kind with
    | TypeKind.TypeVar _ -> failwith "TODO: buildType - TypeVar"
    | TypeKind.TypeRef { Name = name; TypeArgs = typeArgs } ->
      let typeArgs =
        typeArgs
        |> Option.map (fun args ->
          { Params = args |> List.map (buildType ctx)
            Loc = None })

      TsType.TsTypeRef
        { TypeName = qualifiedIdentToTsEntityName name
          TypeParams = typeArgs
          Loc = None }
    | TypeKind.Primitive p ->
      let kind =
        match p with
        | Boolean -> TsKeywordTypeKind.TsBooleanKeyword
        | Number -> TsKeywordTypeKind.TsNumberKeyword
        | String -> TsKeywordTypeKind.TsStringKeyword
        | Symbol -> TsKeywordTypeKind.TsSymbolKeyword
        | BigInt -> TsKeywordTypeKind.TsBigIntKeyword

      TsType.TsKeywordType { Kind = kind; Loc = None }
    | TypeKind.Keyword keyword ->
      let kind =
        match keyword with
        | Keyword.Never -> TsKeywordTypeKind.TsNeverKeyword
        | Keyword.Object -> TsKeywordTypeKind.TsObjectKeyword
        | Keyword.Unknown -> TsKeywordTypeKind.TsUnknownKeyword
        | Keyword.GlobalThis -> failwith "TODO: buildType - GlobalThis"

      TsType.TsKeywordType { Kind = kind; Loc = None }
    | TypeKind.Function f ->
      let ps: list<TsFnParam> =
        f.ParamList
        |> List.map (fun p ->
          let t = buildTypeAnn ctx p.Type

          match p.Pattern with
          | Pattern.Identifier { Name = name } ->
            let pat =
              TsFnParamPat.Ident
                { Id = { Name = name; Loc = None }
                  Loc = None }

            { Pat = pat
              TypeAnn = Some(t)
              Optional = p.Optional
              Loc = None }
          | Pattern.Object _ -> failwith "TODO"
          | Pattern.Tuple _ -> failwith "TODO"
          | Pattern.Rest _ -> failwith "TODO"
          | _ -> failwith "Invalid pattern for function parameter")

      let typeParams: option<TsTypeParamDecl> =
        f.TypeParams
        |> Option.map (fun typeParams ->
          { Params =
              typeParams
              |> List.map
                (fun
                     { Name = name
                       Constraint = c
                       Default = d } ->
                  { Name = { Name = name; Loc = None }
                    IsIn = false
                    IsOut = false
                    IsConst = false
                    Constraint = Option.map (buildType ctx) c
                    Default = Option.map (buildType ctx) d
                    Loc = None })
            Loc = None })

      TsFnOrConstructorType.TsFnType
        { Params = ps
          TypeParams = typeParams
          TypeAnn = buildTypeAnn ctx f.Return
          Loc = None }
      |> TsType.TsFnOrConstructorType
    | TypeKind.Object { Elems = elems } ->
      let members = elems |> List.map (buildObjTypeElem ctx)
      TsType.TsTypeLit { Members = members; Loc = None }
    | TypeKind.RestSpread rest ->
      TsType.TsRestType
        { TypeAnn = buildType ctx rest
          Loc = None }
    | TypeKind.Literal lit ->
      match lit with
      | Literal.Boolean value ->
        TsType.TsLitType
          { Lit = TsLit.Bool { Value = value; Loc = None }
            Loc = None }
      | Literal.Number value ->
        TsType.TsLitType
          { Lit =
              TsLit.Number
                { Value = value
                  Raw = None
                  Loc = None }
            Loc = None }
      | Literal.String value ->
        TsType.TsLitType
          { Lit =
              TsLit.Str
                { Value = value
                  Raw = None
                  Loc = None }
            Loc = None }
      | Literal.Null ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsNullKeyword
            Loc = None }
      | Literal.Undefined ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsNullKeyword
            Loc = None }
    | TypeKind.Union types ->
      let types = types |> List.map (buildType ctx)

      TsType.TsUnionOrIntersectionType(
        TS.TsUnionType { Types = types; Loc = None }
      )
    | TypeKind.Intersection types ->
      let types = types |> List.map (buildType ctx)

      TsType.TsUnionOrIntersectionType(
        TS.TsIntersectionType { Types = types; Loc = None }
      )
    | TypeKind.Tuple { Elems = elems } ->
      let elemTypes: list<TsTupleElement> =
        elems
        |> List.map (buildType ctx)
        |> List.map (fun t -> { Label = None; Type = t; Loc = None })

      TsType.TsTupleType { ElemTypes = elemTypes; Loc = None }
    | TypeKind.Array { Elem = elem; Length = length } ->
      TsType.TsArrayType
        { ElemType = buildType ctx elem
          Loc = None }
    | TypeKind.KeyOf t ->
      TsType.TsTypeOperator
        { Op = TsTypeOperatorOp.KeyOf
          TypeAnn = buildType ctx t
          Loc = None }
    | TypeKind.Index(target, index) ->
      TsType.TsIndexedAccessType
        { Readonly = false // TODO: figure out when this should be true
          ObjType = buildType ctx target
          IndexType = buildType ctx index
          Loc = None }
    | TypeKind.Condition { Check = check
                           Extends = extends
                           TrueType = trueType
                           FalseType = falseType } ->
      TsType.TsConditionalType
        { CheckType = buildType ctx check
          ExtendsType = buildType ctx extends
          TrueType = buildType ctx trueType
          FalseType = buildType ctx falseType
          Loc = None }
    | TypeKind.Infer name ->
      let typeParam =
        { Name = { Name = name; Loc = None }
          IsIn = false
          IsOut = false
          IsConst = false
          Constraint = None
          Default = None
          Loc = None }

      TsType.TsInferType { TypeParam = typeParam; Loc = None }
    | TypeKind.Binary _ ->
      // TODO: This should be const time evaluated to determine the
      // actual type to export
      failwith "TODO: buildType - Binary"
    | TypeKind.Wildcard ->
      // TODO: Use `any`?
      failwith "TODO: buildType - Wildcard"
    | TypeKind.Namespace _ -> failwith "TODO: buildType - Namespace"
    | TypeKind.Range _ -> failwith "TODO: buildType - Range"
    | TypeKind.UniqueSymbol id -> failwith "TODO: buildType - UniqueSymbol"
    | TypeKind.UniqueNumber id -> failwith "TODO: buildType - UniqueNumber"
    | TypeKind.Typeof _ -> failwith "TODO: buildType - Typeof"
    | TypeKind.Unary(op, arg) -> failwith "TODO: buildType - Unary"
    | TypeKind.TemplateLiteral _ -> failwith "TODO: buildType - TemplateLiteral"
    | TypeKind.Intrinsic -> failwith "TODO: buildType - Intrinsic"
    | TypeKind.IntrinsicInstance _ ->
      failwith "TODO: buildType - IntrinsicInstance"

  let buildObjTypeElem (ctx: Ctx) (elem: ObjTypeElem) : TsTypeElement =
    match elem with
    | Callable(callable) -> failwith "TODO: buildObjTypeElem - Callable"
    | Constructor(ctor) -> failwith "TODO: buildObjTypeElem - Constructor"
    | Property(prop) ->
      match prop.Name with
      | PropName.String s ->
        TsTypeElement.TsPropertySignature
          { Readonly = false
            Key = Expr.Ident { Name = s; Loc = None }
            Computed = false
            Optional = false
            TypeAnn = buildTypeAnn ctx prop.Type
            Loc = None }
      | _ -> failwith "TODO: buildObjTypeElem - Property"
    | Method(name, fn) -> failwith "TODO: buildObjTypeElem - Method"
    | Getter(name, fn) -> failwith "TODO: buildObjTypeElem - Getter"
    | Setter(name, fn) -> failwith "TODO: buildObjTypeElem - Setter"
    | Mapped mapped -> failwith "TODO: buildObjTypeElem - Mapped"
    | RestSpread t -> failwith "TODO: buildObjTypeElem - Rest"

  type Binding = Type * bool
  type BindingAssump = Map<string, Binding>

  let findBindings (pat: Syntax.Pattern) : BindingAssump =
    let mutable assump: BindingAssump = Map.empty

    let rec walk (pat: Syntax.Pattern) : unit =
      match pat.Kind with
      | PatternKind.Ident { Name = name; IsMut = isMut } ->
        match pat.InferredType with
        | Some(t) ->
          let t = prune t
          let binding = (t, isMut)
          assump <- Map.add name binding assump
        | None -> ()
      | PatternKind.Object { Elems = elems } ->
        List.iter
          (fun (elem: Syntax.ObjPatElem) ->
            match elem with
            | Syntax.ObjPatElem.KeyValuePat { Value = value } -> walk value
            | Syntax.ObjPatElem.ShorthandPat { Name = name; IsMut = isMut } ->
              // TODO: how do we get the type for this binding?
              // Do we need to add an `InferredType` field to `ShorthandPat`?
              ()
            | Syntax.ObjPatElem.RestPat { Target = target } -> walk target)
          elems
      | PatternKind.Tuple { Elems = elems } -> List.iter walk elems
      | PatternKind.Wildcard _ -> ()
      | PatternKind.Literal _ -> ()
      | PatternKind.Enum(_) -> failwith "TODO: findBinding - Enum"
      | PatternKind.Rest(_) -> failwith "TODO: findBinding - Rest"

    walk pat

    assump
