namespace Escalier.Codegen

open Escalier.Interop
open Escalier.Interop.TypeScript
open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Syntax
open Escalier.Data.Type
open Escalier.Data.Visitor
open Escalier.TypeChecker
open Escalier.TypeChecker.Prune
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Unify

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
            if localName[0] = '_' then localName[1..] else localName

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
    | ExprKind.Binary { Op = "||"
                        Left = left
                        Right = right } ->
      flattenLogicalOr left @ flattenLogicalOr right
    | _ -> [ expr ]

  let flattenLogicalAnd (expr: Expr) : list<Expr> =
    match expr.Kind with
    | ExprKind.Binary { Op = "&&"
                        Left = left
                        Right = right } ->
      flattenLogicalAnd left @ flattenLogicalAnd right
    | _ -> [ expr ]

  let buildTempDecl (ctx: Ctx) (tempId: string) : TS.Stmt =
    let tempDecl =
      { Export = false
        Declare = false
        Decls =
          [ { Id =
                Pat.Ident
                  { Id = { Name = tempId; Loc = None }
                    Loc = None }
              TypeAnn = None
              Init = None } ]
        Kind = VariableDeclarationKind.Var
        Loc = None
        Comments = [] }

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
      let _, checks = buildPattern ctx head.Pattern (Some targetExpr)
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
    | ExprKind.Identifier { Name = name } ->
      Expr.Ident { Name = name; Loc = None }, []
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
    | ExprKind.Binary { Op = op; Left = left; Right = right } ->
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

            let pat =
              match pat with
              | Some pat -> pat
              | None ->
                failwith "Function parameter pattern must be irrefutable"

            { Pat = pat
              Optional = p.Optional
              TypeAnn = None
              Loc = None })

        let body = buildBlock ctx block Finalizer.Empty

        let func: TS.Function =
          { Params = ps
            Body = Some({ Body = body.Body; Loc = None })
            IsGenerator = false
            IsAsync = s.IsAsync
            TypeParams = None
            ReturnType = None
            Loc = None }

        let expr = Expr.Fn { Id = None; Fn = func }
        let stmts = []

        (expr, stmts)
      | BlockOrExpr.Expr expr ->
        let ps: list<Param> =
          s.ParamList
          |> List.map (fun (p: Syntax.FuncParam) ->
            let pat, _ = buildPattern ctx p.Pattern None

            let pat =
              match pat with
              | Some pat -> pat
              | None ->
                failwith "Function parameter pattern must be irrefutable"

            { Pat = pat
              Optional = p.Optional
              TypeAnn = None
              Loc = None })

        let bodyExpr, bodyStmts = buildExpr ctx expr

        let body =
          bodyStmts @ [ Stmt.Return { Argument = Some bodyExpr; Loc = None } ]

        let body: BlockStmt = { Body = body; Loc = None }
        let expr = Expr.Arrow { Params = ps; Body = body }

        (expr, [])
    | ExprKind.IfElse { Condition = condition
                        Then = thenBranch
                        Else = elseBranch } ->
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
    | ExprKind.Member { Target = target
                        Name = name
                        OptChain = opt_chain } ->

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
          | ObjElem.Property { Name = name; Value = value } ->
            let value, valueStmts = buildExpr ctx value
            stmts <- stmts @ valueStmts

            Property.KeyValueProperty
              { Key = propNameToPropName name
                Value = value
                Kind = PropertyKind.Init
                Loc = None }
          | ObjElem.Shorthand { Name = name } ->
            Property.Ident { Name = name; Loc = None }
          | ObjElem.Spread { Value = value } ->
            let value, valueStmts = buildExpr ctx value
            stmts <- stmts @ valueStmts

            Property.SpreadElement { Expr = value; Loc = None })

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
    | ExprKind.ExprWithTypeArgs { Expr = target; TypeArgs = typeArgs } ->
      buildExpr ctx target
    | ExprKind.Class { Name = name
                       Extends = extends
                       Implements = _
                       TypeParams = typeParams
                       Elems = elems } ->

      let mutable stmts: list<TS.Stmt> = []

      let id: Option<TS.Ident> =
        match name with
        | Some name -> { TS.Name = name; Loc = None } |> Some
        | None -> None

      let body: list<ClassMember> =
        elems
        |> List.map (fun elem ->
          match elem with
          | ClassElem.Property p ->
            ClassMember.ClassProp
              { Key = propNameToPropName p.Name
                Value =
                  p.Value
                  |> Option.map (fun value ->
                    let valueExpr, valueStmts = buildExpr ctx value
                    stmts <- stmts @ valueStmts
                    valueExpr)
                TypeAnn =
                  p.TypeAnn
                  |> Option.map (fun typeAnn ->
                    let t = buildTypeFromTypeAnn ctx typeAnn
                    { TypeAnn = t; Loc = None })
                IsStatic = p.Static
                // TODO: Decorators
                // decorators: list<Decorator>
                Accessibility = None
                IsAbstract = false
                IsOptional = p.Optional
                IsOverride = false
                Readonly = p.Readonly
                Declare = false
                Definite = false
                Loc = None }

          | ClassElem.Constructor ctor ->
            let fn = buildFn ctx ctor.Sig ctor.Body

            ClassMember.Constructor
              { Params =
                  fn.Params
                  |> List.map (fun p ->
                    ParamOrTsParamProp.Param
                      { Pat = p.Pat
                        Optional = p.Optional
                        TypeAnn = p.TypeAnn
                        Loc = None })
                Body = fn.Body
                Accessibility = None
                IsOptional = false
                Loc = None }

          | ClassElem.Method method ->
            ClassMember.Method
              { Key = propNameToPropName method.Name
                Function = buildFn ctx method.Sig method.Body
                Kind = MethodKind.Method
                IsStatic = method.Static
                Accessibility = None
                IsAbstract = false
                IsOptional = false
                IsOverride = false
                Loc = None }

          | ClassElem.Getter _ -> failwith "TODO: ClassElem.Get"
          | ClassElem.Setter _ -> failwith "TODO: ClassElem.Set")

      let extends: option<TsTypeRef> =
        extends
        |> Option.map (fun (typeRef: Syntax.TypeRef) ->
          let typeParams =
            typeRef.TypeArgs
            |> Option.map (fun args ->
              { Params = args |> List.map (buildTypeFromTypeAnn ctx)
                Loc = None })

          let typeRef: TsTypeRef =
            { Loc = None
              TypeName = qualifiedIdentToTsEntityName typeRef.Ident
              TypeParams = typeParams }

          typeRef)

      let cls: TS.Class =
        { TypeParams = None // TODO
          IsAbstract = false // TODO
          Super = extends
          Implements = None // intentionally None since we're outputting JS
          Body = body
          Loc = None }

      let classExpr = TS.Expr.Class { Id = id; Class = cls; Loc = None }

      (classExpr, [])

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
    | ExprKind.Index { Target = target
                       Index = index
                       OptChain = optChain } ->
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
    | ExprKind.IfLet { Pattern = pattern
                       Target = target
                       Then = thenBranch
                       Else = elseBranch } ->
      let tempId = ctx.GetTempId()
      let finalizer = Finalizer.Assign tempId
      let tempDecl = buildTempDecl ctx tempId

      let targetExpr, targetStmts = buildExpr ctx target
      let pattern, checks = buildPattern ctx pattern (Some targetExpr)

      let mutable checkExprs = checks |> List.map (buildPatternCheck ctx)

      if checkExprs.IsEmpty then
        // if-let's main use case is unwrapping `T | null` or `T | undefined`
        // JS treats `x != null` the same as `x !== null && x !== undefined`
        let notNullCheck =
          Expr.Bin
            { Operator = BinOp.NotEq
              Left = targetExpr
              Right = Expr.Lit(Lit.Null { Loc = None })
              Loc = None }

        checkExprs <- notNullCheck :: checkExprs

      let conditionExpr =
        checkExprs
        |> List.reduce (fun acc check ->
          Expr.Bin
            { Operator = BinOp.LogicalAnd
              Left = acc
              Right = check
              Loc = None })

      let thenBlock = buildBlock ctx thenBranch finalizer

      let thenBlock =
        match pattern with
        | Some pattern ->
          let decl =
            { Export = false
              Declare = false
              Decls =
                [ { Id = pattern
                    TypeAnn = None
                    Init = Some targetExpr } ]
              Kind = VariableDeclarationKind.Var
              Loc = None
              Comments = [] }
            |> Decl.Var
            |> Stmt.Decl

          { thenBlock with
              Body = decl :: thenBlock.Body }
        | None -> thenBlock // no declaration is need if the pattern is None

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
    | ExprKind.Match { Target = target; Cases = cases } ->
      match buildMatchRec ctx target None cases with
      | Some expr, stmts -> (expr, stmts)
      | None, stmts -> failwith "Failure compiling 'match' expression"
    | ExprKind.Assign { Op = op; Left = left; Right = right } ->
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
    | ExprKind.Unary { Op = op; Value = value } ->
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
        { Kind = ExprKind.Identifier { Name = "__error__" }
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
    | ExprKind.Throw arg ->
      let argExpr, argStmts = buildExpr ctx arg

      let callee =
        Expr.Member
          { Object = Expr.Ident { Name = "Escalier"; Loc = None }
            Property = Expr.Ident { Name = "throw"; Loc = None }
            Computed = false
            OptChain = false
            Loc = None }

      let call =
        Expr.Call
          { Callee = callee
            Arguments = [ argExpr ]
            Loc = None }

      (call, argStmts)
    | ExprKind.TemplateLiteral template ->
      let tpl, stmts = buildTemplateLiteral ctx template
      (Expr.Tpl tpl, stmts)
    | ExprKind.TaggedTemplateLiteral { Tag = tag; Template = template } ->
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

  let propNameToPropName (name: Syntax.PropName) : TS.PropName =
    let key =
      match name with
      | Syntax.PropName.Ident name ->
        TS.PropName.Ident { Name = name; Loc = None }
      | Syntax.PropName.String value ->
        TS.PropName.Str
          { Value = value
            Raw = None
            Loc = None }
      | Syntax.PropName.Number value ->
        TS.PropName.Num
          { Value = value
            Raw = None
            Loc = None }
      | Syntax.PropName.Computed expr ->
        failwith "TODO: propNameToPropName - Computed property names"

    key

  // TODO: Replace with Syntax.PropName after moving it to Common
  let typePropNameToPropName (name: PropName) : TS.PropName =
    let key =
      match name with
      | PropName.String name -> TS.PropName.Ident { Name = name; Loc = None }
      | PropName.Number value ->
        TS.PropName.Num
          { Value = value
            Raw = None
            Loc = None }
      | PropName.Symbol symbol ->
        failwith "TODO: typePropNameToPropName - Computed property names"

    key

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

        Property.KeyValueProperty
          { Key = TS.PropName.Ident { Name = attr.Name; Loc = None }
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
        Property.KeyValueProperty
          { Key = TS.PropName.Ident { Name = "children"; Loc = None }
            Value = childrenValue
            Kind = PropertyKind.Init
            Loc = None }

      properties <- properties @ [ childrenProp ]

    let propsObj = Expr.Object { Properties = properties; Loc = None }

    let componentExpr =
      match name with
      | QualifiedIdent.Ident s when System.Char.IsLower(s, 0) ->
        Expr.Lit(Lit.Str { Value = s; Raw = None; Loc = None })
      | ident -> qualifiedIdentToMemberExpr ident

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
        Property.KeyValueProperty
          { Key = TS.PropName.Ident { Name = "children"; Loc = None }
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

  let buildFn
    (ctx: Ctx)
    (fnSig: FuncSig)
    (body: option<BlockOrExpr>)
    : TS.Function =
    let ps: list<Param> =
      fnSig.ParamList
      |> List.map (fun (p: Syntax.FuncParam) ->
        let pat, _ = buildPattern ctx p.Pattern None

        let pat =
          match pat with
          | Some pat -> pat
          | None -> failwith "Function parameter pattern must be irrefutable"

        { Pat = pat
          Optional = p.Optional
          TypeAnn = None
          Loc = None })

    let body =
      match body with
      | Some(BlockOrExpr.Block block) -> buildBlock ctx block Finalizer.Empty
      | Some(BlockOrExpr.Expr expr) ->
        let expr, stmts = buildExpr ctx expr

        let body = stmts @ [ Stmt.Return { Argument = Some expr; Loc = None } ]

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

    func

  let buildFnDecl (ctx: Ctx) (decl: FnDecl) : list<TS.Stmt> =
    if decl.Declare then
      [] // Nothing to emit for `declare` function declarations
    else
      let func = buildFn ctx decl.Sig decl.Body

      let fnDecl =
        { Export = false
          Declare = false
          Id = { Name = decl.Name; Loc = None }
          Fn = func
          Loc = None
          Comments = [] }

      let funcDecl = Stmt.Decl(Decl.Fn fnDecl)

      [ funcDecl ]

  // TODO: unify Pat with TsFnPat
  // NOTE: This should only be used by buildModuleTypes since it doesn't output
  // any statements that might be generated by `buildExpr ctx init` when handling
  // object assignment shorthand with an initializer.
  let funcParamPatternToPat (ctx: Ctx) (pattern: Pattern) : TS.Pat =
    match pattern with
    | Pattern.Identifier { Name = name } ->
      Pat.Ident
        { Id = { Name = name; Loc = None }
          Loc = None }
    | Pattern.Object { Elems = elems } ->
      let props =
        elems
        |> List.map (fun elem ->
          match elem with
          | ObjPatElem.KeyValuePat { Key = key; Value = value } ->
            let pat = funcParamPatternToPat ctx value

            ObjectPatProp.KeyValue
              { Key = TS.PropName.Ident { Name = key; Loc = None }
                Value = pat
                Loc = None }
          | ObjPatElem.ShorthandPat { Name = name; Init = init; IsMut = _ } ->

            ObjectPatProp.Assign
              { Key = { Name = name; Loc = None }
                Value =
                  init |> Option.map (fun init -> buildExpr ctx init |> fst)
                Loc = None }
          | ObjPatElem.RestPat pattern -> failwith "todo")

      Pat.Object { Props = props; Loc = None }
    | Pattern.Tuple { Elems = elems; Immutable = _ } ->
      Pat.Array
        { Elems = elems |> List.map (Option.map (funcParamPatternToPat ctx))
          Loc = None }
    | Pattern.Rest pattern ->
      let arg = funcParamPatternToPat ctx pattern
      Pat.Rest { Arg = arg; Loc = None }
    | _ -> failwith "Invalid pattern for function parameter"

  let buildFnDeclType (ctx: Ctx) (env: Env) (fnDecl: FnDecl) : TS.Stmt =
    let f = fnDecl.InferredFunction.Value

    let t =
      match env.GetValue fnDecl.Name with
      | Ok t -> t
      | Error _ -> failwith $"buildFnDeclType - Type of {fnDecl.Name} not found"

    printfn $"Type of {fnDecl.Name}: {t}"

    // TODO(#402): use this type to build the function declaration
    // This should have a better type since it's a fully inferred type
    // whereas the TypeScript type we currently generate depends on explicit
    // type annotations on function parameters and the return type annotation.
    let tsType = buildType ctx t

    // TODO: unify Param and TsFnParam
    let ps: list<Param> =
      f.ParamList
      |> List.map (fun p ->
        let pat = funcParamPatternToPat ctx p.Pattern

        { Pat = pat
          TypeAnn = Some(buildTypeAnn ctx p.Type)
          Optional = p.Optional
          Loc = None })

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

    let func =
      { Params = ps
        // TODO: Decorators
        // Decorators: list<Decorator>
        Body = None
        IsGenerator = false // TODO
        IsAsync = fnDecl.Sig.IsAsync
        TypeParams = typeParams
        ReturnType = Some(buildTypeAnn ctx f.Return)
        Loc = None }

    let fnDecl =
      { Export = fnDecl.Export
        Declare = false
        Id = { Name = fnDecl.Name; Loc = None }
        Fn = func
        Loc = None
        Comments = [] }

    Stmt.Decl(Decl.Fn fnDecl)

  let buildOverloadedFnDecl (ctx: Ctx) (decls: list<FnDecl>) : list<TS.Stmt> =
    let typeError =
      TS.Expr.New
        { Callee = TS.Expr.Ident { Name = "TypeError"; Loc = None }
          Arguments = []
          Loc = None }

    let throw = TS.Stmt.Throw { Argument = typeError; Loc = None }
    let block: TS.BlockStmt = { Body = [ throw ]; Loc = None }
    let mutable ifElse: TS.Stmt = TS.Stmt.Block(block)

    if List.forall (fun (decl: FnDecl) -> decl.Declare) decls then
      [] // Nothing to emit for `declare` function declarations
    else
      // NOTE: We reverse the order of the declarations because the
      // if-else chain is built in reverse order.
      for decl in List.rev decls do
        let mutable map = Map.empty

        decl.Sig.ParamList
        |> List.iteri (fun i param ->
          let name =
            match param.Pattern.Kind with
            | PatternKind.Ident { Name = name } -> name
            | _ -> failwith "Function param pattern must be an identifier"

          map <- Map.add name $"__arg{i}__" map)

        // TODO: keep track of shadowed variables and avoid renaming them
        let visitor: ExprVisitor.SyntaxVisitor<Set<string>> =
          { ExprVisitor.VisitExpr =
              fun (expr, state) ->
                match expr.Kind with
                | ExprKind.Identifier ident ->
                  if not (Set.contains ident.Name state) then
                    match map.TryFind ident.Name with
                    | Some newName -> ident.Name <- newName
                    | None -> ()
                  else
                    ()

                  (true, state)
                | ExprKind.Function func ->
                  let paramBindingNames =
                    func.Sig.ParamList
                    |> List.map (fun param ->
                      Helpers.findBindingNames param.Pattern)
                    |> Set.unionMany

                  (true, Set.union state paramBindingNames)
                | _ -> (true, state)
            VisitJsxElement = fun (_, state) -> (true, state)
            VisitJsxFragment = fun (_, state) -> (true, state)
            VisitJsxText = fun (_, state) -> (false, state)
            VisitStmt = fun (_, state) -> (true, state)
            VisitPattern =
              fun (pat, state) ->
                match pat.Kind with
                | PatternKind.Ident ident ->
                  if not (Set.contains ident.Name state) then
                    match map.TryFind ident.Name with
                    | Some newName -> ident.Name <- newName
                    | None -> ()
                  else
                    ()
                | _ -> ()

                (false, state)
            VisitTypeAnn = fun (_, state) -> (false, state)
            VisitTypeAnnObjElem = fun (_, state) -> (false, state) }

        let checks =
          decl.Sig.ParamList
          |> List.map (fun param ->
            ExprVisitor.walkPattern visitor Set.empty param.Pattern
            checkExprFromParam param)

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
            List.iter (ExprVisitor.walkStmt visitor Set.empty) block.Stmts
            buildBlock ctx block Finalizer.Empty
          | Some(BlockOrExpr.Expr expr) ->
            ExprVisitor.walkExpr visitor Set.empty expr
            let expr, stmts = buildExpr ctx expr

            let body =
              stmts @ [ Stmt.Return { Argument = Some expr; Loc = None } ]

            { Body = body; Loc = None }
          | None -> failwith "Function declaration must have a body"

        ifElse <-
          TS.Stmt.If
            { Test = check
              Consequent = TS.Stmt.Block(body)
              Alternate = Some(ifElse)
              Loc = None }

      let maxParamCount =
        decls |> List.map (fun decl -> decl.Sig.ParamList.Length) |> List.max

      let ps: List<Param> =
        seq { 0 .. maxParamCount - 1 }
        |> Seq.map (fun i ->
          { Param.Pat =
              Pat.Ident
                { Id = { Name = $"__arg{i}__"; Loc = None }
                  Loc = None }
            Optional = false
            TypeAnn = None
            Loc = None })
        |> List.ofSeq

      let func: TS.Function =
        { Params = ps
          Body = Some({ Body = [ ifElse ]; Loc = None })
          IsGenerator = false
          IsAsync = false
          TypeParams = None
          ReturnType = None
          Loc = None }

      let fnDecl =
        { Export = false
          Declare = false
          Id = { Name = decls[0].Name; Loc = None }
          Fn = func
          Loc = None
          Comments = [] }

      let funcDecl = Stmt.Decl(Decl.Fn fnDecl)

      [ funcDecl ]

  let buildBlock (ctx: Ctx) (body: Block) (finalizer: Finalizer) : BlockStmt =
    // TODO: check if the last statement is an expression statement
    // and use the appropriate finalizer with it

    let mutable stmts: list<TS.Stmt> = []

    let lastStmt =
      match body.Stmts.IsEmpty with
      | true ->
        // If the body is empty, we synthesize an `undefined` expression statement
        // to be the last statement in the block.
        let undefined: Syntax.Expr =
          { Kind = ExprKind.Literal(Common.Literal.Undefined)
            Span = dummySpan
            InferredType = None }

        { Kind = StmtKind.Expr undefined
          Span = dummySpan }
      | false -> body.Stmts |> List.last

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

            let pattern =
              match pattern with
              | Some pat -> pat
              | None -> failwith "Var declaration patterns must be irrefutable"

            let initExpr, initStmts = buildExpr ctx init

            let decl =
              { Export = false
                Declare = false
                Decls =
                  [ { Id = pattern
                      TypeAnn = None
                      Init = Some initExpr } ]
                Kind = VariableDeclarationKind.Var
                Loc = None
                Comments = [] }

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
              | [ decl ] -> buildFnDecl ctx decl
              | decls -> buildOverloadedFnDecl ctx decls
            | None -> []
          | ClassDecl _ -> failwith "TODO: buildBlock - ClassDecl"
          | EnumDecl _ -> failwith "TODO: buildBlock - EnumDecl"
          | NamespaceDecl _ ->
            // TODO(#404): Codegen Namespaces
            failwith "TODO: buildBlock - NamespaceDecl"
          | InterfaceDecl _ -> [] // Ignore types when generating JS code
        | StmtKind.Return expr ->
          match expr with
          | Some expr ->
            let expr, stmts = buildExpr ctx expr
            let retStmt = Stmt.Return { Argument = Some expr; Loc = None }
            stmts @ [ retStmt ]
          | None -> [ Stmt.Return { Argument = None; Loc = None } ]
        | StmtKind.For { Left = left
                         Right = right
                         Body = body } ->

          let leftPat, _ = buildPattern ctx left None

          let leftPat =
            match leftPat with
            | Some pat -> pat
            | None -> failwith "For loop patterns must be irrefutable"

          // TODO: if the right is a range, we should codegen a regular for loop
          // This will require store the min and max values as properties on the
          // range object.
          let rightExpr, rightStmts = buildExpr ctx right

          let decl =
            { Id = leftPat
              TypeAnn = None
              Init = None }

          let left =
            ForHead.VarDecl
              { Export = false
                Declare = false
                Decls = [ decl ]
                Kind = VariableDeclarationKind.Const
                Loc = None
                Comments = [] }

          let forStmt =
            TS.Stmt.ForOf
              { IsAwait = false
                Left = left
                Right = rightExpr
                Body = buildBlock ctx body Finalizer.Empty |> TS.Stmt.Block
                Loc = None }

          rightStmts @ [ forStmt ]

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

  // If buildPattern returns None then the pattern was refutable and thus only
  // the return checks are useful.  The buildPattern returns Some(pat) then the
  // pattern is irrefutable and should be used for destructuring.
  let buildPattern
    (ctx: Ctx)
    (pattern: Syntax.Pattern)
    (parent: option<TS.Expr>)
    : option<TS.Pat> * list<PatternCheck> =
    match pattern.Kind with
    | PatternKind.Ident { Name = name } ->
      let pat =
        Pat.Ident
          { Id = { Name = name; Loc = None }
            Loc = None }

      Some pat, []
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

            // TODO: add support for default values in ObjectPatProp.KeyValue
            keyValuePat
            |> Option.map (fun keyValuePat ->
              ObjectPatProp.KeyValue
                { Key = TS.PropName.Ident { Name = key; Loc = None }
                  Value = keyValuePat
                  Loc = None })
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

            argPat
            |> Option.map (fun argPat ->
              ObjectPatProp.Rest { Arg = argPat; Loc = None }))

      Some(Pat.Object { Props = props; Loc = None }), checks
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

          elemPat)

      Some(Pat.Array { Elems = elems; Loc = None }), checks
    | PatternKind.Enum enumVariantPattern ->
      failwith "TODO: buildPattern - Enum"
    | PatternKind.Wildcard wildcardPattern ->
      failwith "TODO: buildPattern - Wildcard"
    | PatternKind.Literal literal ->
      let checks =
        match parent with
        | Some parent -> [ ValueCheck(parent, buildLiteral literal) ]
        | None -> []

      None, checks
    | PatternKind.Rest pattern ->
      let argPat, argExprs = buildPattern ctx pattern parent

      argPat |> Option.map (fun argPat -> Pat.Rest { Arg = argPat; Loc = None }),
      argExprs

  // TODO: our ModuleItem enum should contain: Decl and Imports
  // TODO: pass in `env: Env` so that we can look up the types of
  // the exported symbols since we aren't tracking provenance consistently yet
  let buildModuleTypes
    (env: Env)
    (ctx: Ctx)
    (typeCtx: Env.Ctx)
    (m: Module)
    (expand: bool)
    : TS.Module =
    let mutable items: list<TS.ModuleItem> = []

    let mutable fnDecls: Map<string, list<FnDecl>> = Map.empty

    for item in m.Items do
      match item with
      | ModuleItem.Stmt stmt ->
        match stmt.Kind with
        | StmtKind.Decl { Kind = DeclKind.FnDecl fnDecl } ->
          let decls =
            match Map.tryFind fnDecl.Name fnDecls with
            | Some decls -> decls
            | None -> []

          fnDecls <- Map.add fnDecl.Name (decls @ [ fnDecl ]) fnDecls
        | _ -> ()
      | _ -> ()

    for item in m.Items do
      match item with
      | ModuleItem.Export _ -> failwith "TODO: buildModuleTypes - Export"
      | ModuleItem.Import { Path = path; Specifiers = specifiers } ->
        let specifiers =
          specifiers
          |> List.map (fun spec ->

            match spec with
            | ImportSpecifier.Named { Name = name; Alias = alias } ->
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

            | ImportSpecifier.ModuleAlias { Alias = alias } ->
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
          | TypeDecl { Name = name
                       TypeParams = typeParams
                       TypeAnn = typeAnn
                       Export = export
                       Declare = declare } ->

            let typeParams: option<TsTypeParamDecl> =
              typeParams
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
                          Constraint =
                            c |> Option.map (buildTypeFromTypeAnn ctx)
                          Default = d |> Option.map (buildTypeFromTypeAnn ctx)
                          Loc = None })
                  Loc = None })

            // The `expand` flag is used for the utilty_types fixture tests
            if expand then
              let scheme = env.FindScheme name

              // NOTE: There's no need for a mapping between from type arg to
              // type param because they have the same name in this situation:
              // type MyPoint<T> = Point<T> -> type MyPoint<T> = { x: T, y: T }
              let comments =
                match expandScheme typeCtx env None scheme Map.empty None with
                | Ok t ->
                  if t.ToString() <> scheme.Type.ToString() then
                    [ Comment.LineComment
                        { Text = $"// expansion - {t}"
                          Loc = None } ]
                  else
                    []
                | Error _ -> []

              match typeAnn.InferredType with
              | Some t ->
                let decl =
                  TS.Decl.TsTypeAlias
                    { Export = export
                      Declare = declare
                      Id = { Name = name; Loc = None }
                      TypeParams = typeParams
                      TypeAnn = buildType ctx t
                      Loc = None
                      Comments = comments }

                let item = TS.ModuleItem.Stmt(Stmt.Decl decl)

                items <- item :: items
              | None -> ()
            else
              match typeAnn.InferredType with
              | Some t ->
                let decl =
                  TS.Decl.TsTypeAlias
                    { Export = export
                      Declare = declare
                      Id = { Name = name; Loc = None }
                      TypeParams = typeParams
                      TypeAnn = buildType ctx t
                      Loc = None
                      Comments = [] }

                let item = TS.ModuleItem.Stmt(Stmt.Decl decl)

                items <- item :: items
              | None -> ()
          | VarDecl { Pattern = pattern
                      Export = export
                      Declare = declare } ->
            // TODO: instead of splitting up the pattern, we could generate a
            // type annotation for the entire pattern.
            for KeyValue(name, _) in findBindings pattern do
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

              let comment =
                Comment.LineComment
                  { Text = $"// @escType - {t}"
                    Loc = None }

              let decl =
                TS.Decl.Var
                  { Export = export
                    Declare = declare
                    Decls = [ decl ]
                    Kind = VariableDeclarationKind.Const
                    Loc = None
                    Comments = [ comment ] }

              let item = TS.ModuleItem.Stmt(Stmt.Decl decl)

              items <- item :: items
          | FnDecl fnDecl ->
            let t =
              match env.GetValue fnDecl.Name with
              | Ok(t) -> t
              | Error(e) -> failwith $"Couldn't find symbol: {fnDecl.Name}"

            match Map.tryFind fnDecl.Name fnDecls with
            | Some decls ->

              // Remove the function declaration from the map so that we only
              // generate a single function declaration for it.
              fnDecls <- Map.remove fnDecl.Name fnDecls

              match decls with
              | [] -> failwith "Function declaration list must not be empty"
              | [ decl ] ->
                let stmt = buildFnDeclType ctx env decl
                items <- (TS.ModuleItem.Stmt stmt) :: items
              | decls ->
                for decl in decls do
                  let stmt = buildFnDeclType ctx env decl
                  items <- (TS.ModuleItem.Stmt stmt) :: items
            | None -> ()
          | ClassDecl _ -> failwith "TODO: buildModuleTypes - ClassDecl"
          | EnumDecl _ -> failwith "TODO: buildModuleTypes - EnumDecl"
          | NamespaceDecl _ -> failwith "TODO: buildModuleTypes - NamespaceDecl"
          | InterfaceDecl decl ->
            let decl =
              TS.Decl.TsInterface
                { Export = decl.Export
                  Declare = decl.Declare
                  Id = { Name = decl.Name; Loc = None }
                  TypeParams = buildTypeParams ctx decl.TypeParams
                  Extends = None // TODO: extends
                  Body = { Body = []; Loc = None }
                  Loc = None
                  Comments = [] }

            let stmt = TS.Stmt.Decl decl
            items <- (TS.ModuleItem.Stmt stmt) :: items
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

  let qualifiedIdentToMemberExpr (id: QualifiedIdent) : TS.Expr =
    match id with
    | QualifiedIdent.Ident name -> TS.Expr.Ident { Name = name; Loc = None }
    | QualifiedIdent.Member(qualifier, name) ->
      TS.Expr.Member
        { Object = qualifiedIdentToMemberExpr qualifier
          Property = TS.Expr.Ident { Name = name; Loc = None }
          Computed = false
          OptChain = false
          Loc = None }

  let buildTypeRef (ctx: Ctx) (typeRef: TypeRef) : TsType =
    let typeParams =
      typeRef.TypeArgs
      |> Option.map (fun args ->
        { Params = args |> List.map (buildType ctx)
          Loc = None })

    TsType.TsTypeRef
      { TypeName = qualifiedIdentToTsEntityName typeRef.Name
        TypeParams = typeParams
        Loc = None }

  // TODO: do the same thing for type params with Types instead of TypeAnns
  let buildTypeParams
    (ctx: Ctx)
    (typeParams: option<list<Syntax.TypeParam>>)
    : option<TsTypeParamDecl> =
    let typeParams: option<TsTypeParamDecl> =
      typeParams
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
                  Constraint = c |> Option.map (buildTypeFromTypeAnn ctx)
                  Default = d |> Option.map (buildTypeFromTypeAnn ctx)
                  Loc = None })
          Loc = None })

    typeParams

  let buildTypeFromTypeAnn (ctx: Ctx) (typeAnn: TypeAnn) : TsType =
    match typeAnn.Kind with
    | TypeAnnKind.Array elemTypeAnn ->
      TsType.TsArrayType
        { ElemType = buildTypeFromTypeAnn ctx elemTypeAnn
          Loc = None }
    | TypeAnnKind.Literal literal ->
      match literal with
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
      | Literal.String s ->
        TsType.TsLitType
          { Lit = TsLit.Str { Value = s; Raw = None; Loc = None }
            Loc = None }
      | Literal.Null ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsNullKeyword
            Loc = None }
      | Literal.Undefined ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsUndefinedKeyword
            Loc = None }
    | TypeAnnKind.Keyword keywordTypeAnn ->
      match keywordTypeAnn with
      | KeywordTypeAnn.Never ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsNeverKeyword
            Loc = None }
      | KeywordTypeAnn.Boolean ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsBooleanKeyword
            Loc = None }

      | KeywordTypeAnn.Number ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsNumberKeyword
            Loc = None }
      | KeywordTypeAnn.String ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsStringKeyword
            Loc = None }
      | KeywordTypeAnn.Symbol ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsSymbolKeyword
            Loc = None }
      | KeywordTypeAnn.UniqueSymbol ->
        TsType.TsTypeOperator
          { TypeAnn = buildTypeFromTypeAnn ctx typeAnn
            Op = TsTypeOperatorOp.Unique
            Loc = None }
      | KeywordTypeAnn.Null ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsNullKeyword
            Loc = None }
      | KeywordTypeAnn.Undefined ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsUndefinedKeyword
            Loc = None }
      | KeywordTypeAnn.Unknown ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsUnknownKeyword
            Loc = None }
      | KeywordTypeAnn.Object ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsObjectKeyword
            Loc = None }
      | KeywordTypeAnn.BigInt ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsBigIntKeyword
            Loc = None }
      | KeywordTypeAnn.Any ->
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsAnyKeyword
            Loc = None }

    | TypeAnnKind.Object { Elems = elems
                           Immutable = _
                           Exact = _ } -> // not supported by TypeScript

      failwith "TODO: buildTypeFromTypeAnn - Object"
    | TypeAnnKind.Tuple { Elems = elems; Immutable = _ } ->
      let elems =
        elems
        |> List.map (fun elem ->
          { Label = None // TODO
            Type = buildTypeFromTypeAnn ctx elem
            IsRest = false // TODO: support rest elements in tuple types
            Loc = None })

      TsType.TsTupleType { ElemTypes = elems; Loc = None }
    | TypeAnnKind.Union typeAnns ->
      TsType.TsUnionOrIntersectionType(
        TsUnionType
          { Types = typeAnns |> List.map (buildTypeFromTypeAnn ctx)
            Loc = None }
      )
    | TypeAnnKind.Intersection typeAnns ->
      TsType.TsUnionOrIntersectionType(
        TsIntersectionType
          { Types = typeAnns |> List.map (buildTypeFromTypeAnn ctx)
            Loc = None }
      )
    | TypeAnnKind.TypeRef typeRef ->
      let typeParams =
        typeRef.TypeArgs
        |> Option.map (fun args ->
          { Params = args |> List.map (buildTypeFromTypeAnn ctx)
            Loc = None })

      let typeRef: TsTypeRef =
        { Loc = None
          TypeName = qualifiedIdentToTsEntityName typeRef.Ident
          TypeParams = typeParams }

      TsType.TsTypeRef typeRef
    | TypeAnnKind.Function f ->
      let funcParams: list<TsFnParam> =
        f.ParamList
        |> List.map (fun p ->
          let typeAnn =
            match p.TypeAnn with
            | Some typeAnn -> typeAnn
            | None -> failwith "Function parameter must have a type annotation"

          let t = buildTypeFromTypeAnn ctx typeAnn

          match p.Pattern.Kind with
          | PatternKind.Ident { Name = name } ->
            let pat =
              Pat.Ident
                { Id = { Name = name; Loc = None }
                  Loc = None }

            { Pat = pat
              TypeAnn = Some { TypeAnn = t; Loc = None }
              Optional = p.Optional
              Loc = None }
          | PatternKind.Object _ ->
            failwith "TODO - buildTypeFromTypeAnn - Object"
          | PatternKind.Tuple _ ->
            failwith "TODO - buildTypeFromTypeAnn - Tuple"
          | PatternKind.Rest _ -> failwith "TODO - buildTypeFromTypeAnn - Rest"
          | _ -> failwith "Invalid pattern for function parameter")

      let typeAnn: TsTypeAnn =
        match f.ReturnType with
        | Some retTypeAnn ->
          let t = buildTypeFromTypeAnn ctx retTypeAnn
          { TypeAnn = t; Loc = None }
        | None -> failwith "Function type must have a return type"

      let fnType: TsFnType =
        { Params = funcParams
          TypeParams = buildTypeParams ctx f.TypeParams
          TypeAnn = typeAnn
          Loc = None }

      TsType.TsFnOrConstructorType(TsFnOrConstructorType.TsFnType fnType)
    | TypeAnnKind.Keyof typeAnn ->
      TsType.TsTypeOperator
        { TypeAnn = buildTypeFromTypeAnn ctx typeAnn
          Op = TsTypeOperatorOp.KeyOf
          Loc = None }
    | TypeAnnKind.Rest typeAnn -> failwith "TODO: buildTypeFromTypeAnn - Rest"
    | TypeAnnKind.Typeof qualifiedIdent ->
      failwith "TODO: buildTypeFromTypeAnn - Typeof"
    | TypeAnnKind.Index indexType ->
      failwith "TODO: buildTypeFromTypeAnn - Index"
    | TypeAnnKind.Condition conditionType ->
      failwith "TODO: buildTypeFromTypeAnn - Condition"
    | TypeAnnKind.Match matchType ->
      // NOTE: This will need to be converted to nested conditional types
      failwith "TODO: buildTypeFromTypeAnn - Match"
    | TypeAnnKind.Infer s -> failwith "TODO: buildTypeFromTypeAnn - Infer"
    | TypeAnnKind.Wildcard -> failwith "TODO: buildTypeFromTypeAnn - Wildcard"
    | TypeAnnKind.TemplateLiteral templateLiteral ->
      failwith "TODO: buildTypeFromTypeAnn - TemplateLiteral"
    | TypeAnnKind.Intrinsic -> failwith "TODO: buildTypeFromTypeAnn - Intrinsic"
    | TypeAnnKind.ImportType importType ->
      failwith "TODO: buildTypeFromTypeAnn - ImportType"


  let buildType (ctx: Ctx) (t: Type) : TsType =
    let t = prune t

    match t.Kind with
    | TypeKind.TypeVar _ -> failwith "TODO: buildType - TypeVar"
    | TypeKind.TypeRef typeRef -> buildTypeRef ctx typeRef
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
          let pat = funcParamPatternToPat ctx p.Pattern

          { Pat = pat
            TypeAnn = Some t
            Optional = p.Optional
            Loc = None })

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
    | TypeKind.Object objType -> buildObjType ctx objType
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
          { Kind = TsKeywordTypeKind.TsUndefinedKeyword
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
        |> List.map (fun t ->
          { Label = None
            Type = t
            IsRest = false
            Loc = None })

      TsType.TsTupleType { ElemTypes = elemTypes; Loc = None }
    | TypeKind.Array { Elem = elem } ->
      TsType.TsArrayType
        { ElemType = buildType ctx elem
          Loc = None }
    | TypeKind.KeyOf t ->
      TsType.TsTypeOperator
        { Op = TsTypeOperatorOp.KeyOf
          TypeAnn = buildType ctx t
          Loc = None }
    | TypeKind.Index { Target = target; Index = index } ->
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
    | TypeKind.Wildcard ->
      TsType.TsKeywordType
        { Kind = TsKeywordTypeKind.TsAnyKeyword
          Loc = None }
    | TypeKind.Namespace _ -> failwith "TODO: buildType - Namespace"
    | TypeKind.UniqueSymbol _ ->
      let typeAnn =
        TsType.TsKeywordType
          { Kind = TsKeywordTypeKind.TsSymbolKeyword
            Loc = None }

      TsType.TsTypeOperator
        { TypeAnn = typeAnn
          Op = TsTypeOperatorOp.Unique
          Loc = None }
    | TypeKind.Typeof _ -> failwith "TODO: buildType - Typeof"
    | TypeKind.TemplateLiteral { Parts = quasis; Exprs = types } ->

      let quasis =
        quasis
        |> List.mapi (fun i quasi ->
          { Tail = i = quasis.Length - 1
            Cooked = None
            Raw = quasi
            Loc = None })

      let types = types |> List.map (buildType ctx)

      TsType.TsLitType
        { Lit =
            TsLit.Tpl
              { Types = types
                Quasis = quasis
                Loc = None }
          Loc = None }
    | TypeKind.Intrinsic -> failwith "TODO: buildType - Intrinsic"
    | TypeKind.IntrinsicInstance _ ->
      failwith "TODO: buildType - IntrinsicInstance"

  let buildObjType (ctx: Ctx) (objType: Type.Object) : TsType =
    let mutable types = []
    let mutable elems = []

    for elem in objType.Elems do
      match elem with
      | ObjTypeElem.Mapped { NameType = nameType
                             TypeAnn = typeAnn
                             TypeParam = typeParam
                             Readonly = readonly
                             Optional = optional } ->

        let nameType = nameType |> Option.map (buildType ctx)
        let typeAnn = buildType ctx typeAnn

        // TODO: extract this into a function
        let typeParam =
          { Name = { Name = typeParam.Name; Loc = None }
            IsIn = false
            IsOut = false
            IsConst = false
            Constraint = buildType ctx typeParam.Constraint |> Some
            Default = None
            Loc = None }

        let readonly =
          readonly
          |> Option.map (fun value ->
            match value with
            | MappedModifier.Add -> TruePlusMinus.Plus
            | MappedModifier.Remove -> TruePlusMinus.Minus)

        let optional =
          optional
          |> Option.map (fun value ->
            match value with
            | MappedModifier.Add -> TruePlusMinus.Plus
            | MappedModifier.Remove -> TruePlusMinus.Minus)

        let t =
          TsType.TsMappedType
            { Readonly = readonly
              Optional = optional
              TypeParam = typeParam
              NameType = nameType
              TypeAnn = typeAnn
              Loc = None }

        types <- t :: types
      | Callable callable -> failwith "TODO: buildObjTypeElem - Callable"
      | Constructor ctor ->
        let ps: list<TsFnParam> =
          ctor.ParamList
          |> List.map (fun p ->
            let t = buildTypeAnn ctx p.Type
            let pat = funcParamPatternToPat ctx p.Pattern

            { Pat = pat
              TypeAnn = Some(t)
              Optional = p.Optional
              Loc = None })

        let typeParams: option<TsTypeParamDecl> =
          ctor.TypeParams
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

        let elem =
          TsTypeElement.TsConstructSignatureDecl
            { Params = ps
              TypeAnn = Some(buildTypeAnn ctx ctor.Return)
              TypeParams = typeParams
              Loc = None }

        elems <- elem :: elems
      | Property prop ->
        let key = typePropNameToPropName prop.Name

        let elem =
          TsTypeElement.TsPropertySignature
            { Readonly = prop.Readonly
              Key = key
              Optional = prop.Optional
              TypeAnn = buildTypeAnn ctx prop.Type
              Loc = None }

        elems <- elem :: elems
      | Method { Name = name; Fn = f } ->
        let key = typePropNameToPropName name

        let ps: list<TsFnParam> =
          f.ParamList
          |> List.map (fun p ->
            let t = buildTypeAnn ctx p.Type
            let pat = funcParamPatternToPat ctx p.Pattern

            { Pat = pat
              TypeAnn = Some(t)
              Optional = p.Optional
              Loc = None })

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

        let elem =
          TsTypeElement.TsMethodSignature
            { Key = key
              Optional = false
              Params = ps
              TypeAnn = Some(buildTypeAnn ctx f.Return)
              TypeParams = typeParams
              Loc = None }

        elems <- elem :: elems
      | Getter { Name = name; Fn = fn } ->
        failwith "TODO: buildObjTypeElem - Getter"
      | Setter { Name = name; Fn = fn } ->
        failwith "TODO: buildObjTypeElem - Setter"
      | RestSpread t -> types <- buildType ctx t :: types

    let members = List.rev elems
    let objType = TsType.TsTypeLit { Members = members; Loc = None }

    if types.IsEmpty then
      objType
    else
      let types = if elems.IsEmpty then types else objType :: types

      // This isn't always correct because spread types can override properties,
      // but TypeScript doesn't have spread types.  In the future we'll want to
      // figure out a better solution to this.
      TsType.TsUnionOrIntersectionType(
        TsIntersectionType { Types = List.rev types; Loc = None }
      )

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
      | PatternKind.Enum _ -> failwith "TODO: findBinding - Enum"
      | PatternKind.Rest pat ->
        let patAssump = findBindings pat

        for KeyValue(name, binding) in patAssump do
          assump <- Map.add name binding assump

    walk pat

    assump
