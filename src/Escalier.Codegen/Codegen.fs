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

  type Ctx = { mutable NextTempId: int }

  type Finalizer =
    | Assign of string
    | Return
    | Empty

  let dummySpan =
    { Start = FParsec.Position("", 0, 0, 0)
      Stop = FParsec.Position("", 0, 0, 0) }

  let buildScript (ctx: Ctx) (m: Script) =
    let stmts: list<Stmt> =
      m.Items
      |> List.choose (fun item ->
        match item with
        | Stmt stmt -> Some stmt
        | _ -> None)

    let block = { Stmts = stmts; Span = dummySpan }
    buildBlock ctx block Finalizer.Empty

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
      let binaryOp =
        match op with
        | "+" -> Some(BinOp.Add)
        | "-" -> Some(BinOp.Sub)
        | "*" -> Some(BinOp.Mul)
        | "/" -> Some(BinOp.Div)
        | "==" -> Some(BinOp.EqEq)
        | "!=" -> Some(BinOp.NotEq)
        | "<" -> Some(BinOp.Lt)
        | "<=" -> Some(BinOp.LtEq)
        | ">" -> Some(BinOp.Gt)
        | ">=" -> Some(BinOp.GtEq)
        | _ -> None

      let leftExpr, leftStmts = buildExpr ctx left
      let rightExpr, rightStmts = buildExpr ctx right

      let binExpr =
        match binaryOp with
        | Some(op) ->
          Expr.Bin
            { Operator = op
              Left = leftExpr
              Right = rightExpr
              Loc = None }
        | None -> failwith "TODO"

      (binExpr, leftStmts @ rightStmts)
    | ExprKind.Do block ->
      let tempId = $"temp{ctx.NextTempId}"
      ctx.NextTempId <- ctx.NextTempId + 1

      let tempDecl =
        { Decls =
            [ { Id =
                  Pat.Ident
                    { Id = { Name = tempId; Loc = None }
                      Optional = false
                      Loc = None }
                TypeAnn = None
                Init = None } ]
          Declare = false
          Kind = VariableDeclarationKind.Var }

      let finalizer = Finalizer.Assign tempId
      let blockStmt = buildBlock ctx block finalizer
      let expr = Expr.Ident { Name = tempId; Loc = None }

      let stmts = [ Stmt.Decl(Decl.Var tempDecl); Stmt.Block blockStmt ]

      (expr, stmts)
    | ExprKind.Function { Sig = s; Body = body } ->
      match body with
      | BlockOrExpr.Block block ->
        let ps: list<Param> =
          s.ParamList
          |> List.map (fun (p: FuncParam<TypeAnn option>) ->
            let pat = buildPattern ctx p.Pattern

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
        let ps = s.ParamList |> List.map (fun p -> buildPattern ctx p.Pattern)
        let bodyExpr, bodyStmts = buildExpr ctx expr

        let body =
          bodyStmts @ [ Stmt.Return { Argument = Some bodyExpr; Loc = None } ]

        let body: BlockStmt = { Body = body; Loc = None }
        let expr = Expr.Arrow { Params = ps; Body = body }

        (expr, [])
    | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
      let tempId = $"temp{ctx.NextTempId}"
      ctx.NextTempId <- ctx.NextTempId + 1
      let finalizer = Finalizer.Assign tempId

      let tempDecl =
        { Decls =
            [ { Id =
                  Pat.Ident
                    { Id = { Name = tempId; Loc = None }
                      Optional = false
                      Loc = None }
                TypeAnn = None
                Init = None } ]
          Declare = false
          Kind = VariableDeclarationKind.Var }

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

      let stmts = [ Stmt.Decl(Decl.Var tempDecl) ] @ conditionStmts @ [ ifStmt ]

      let expr = Expr.Ident { Name = tempId; Loc = None }

      (expr, stmts)
    | _ -> failwith (sprintf "TODO: buildExpr - %A" expr)

  let buildBlock (ctx: Ctx) (body: Block) (finalizer: Finalizer) : BlockStmt =
    // TODO: check if the last statement is an expression statement
    // and use the appropriate finalizer with it

    let mutable stmts: list<TS.Stmt> = []
    let lastStmt = body.Stmts |> List.last

    for stmt in body.Stmts do
      let stmts' =
        match stmt.Kind with
        | StmtKind.Expr expr ->
          let expr, stmts = buildExpr ctx expr

          if stmt = lastStmt then
            stmts @ buildFinalizer ctx expr finalizer
          else
            stmts
        | StmtKind.Decl decl ->
          match decl.Kind with
          | VarDecl(pattern, init, typeAnnOption) ->
            let pattern = buildPattern ctx pattern
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
        | StmtKind.Return expr -> failwith "TODO"
        | StmtKind.For(left, right, body) -> failwith "todo"

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

  let buildPattern (ctx: Ctx) (pattern: Syntax.Pattern) : TS.Pat =
    match pattern.Kind with
    | PatternKind.Ident { Name = name } ->
      Pat.Ident
        { Id = { Name = name; Loc = None }
          Optional = false
          Loc = None }
    | _ -> failwith "TODO"

  // TODO: our ModuleItem enum should contain: Decl and Imports
  // TODO: pass in `env: Env` so that we can look up the types of
  // the exported symbols since we aren't tracking provenance consistently yet
  let buildModuleTypes (env: Env) (ctx: Ctx) (m: Script) : TS.Module =
    let mutable items: list<TS.ModuleItem> = []

    for item in m.Items do
      match item with
      | ScriptItem.Import _ -> failwith "TODO: buildModuleTypes - Import"
      | Stmt stmt ->
        match stmt.Kind with
        | StmtKind.Decl decl ->
          match decl.Kind with
          | TypeDecl(name, typeAnn, typeParams) ->
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
          | VarDecl(pattern, init, typeAnnOption) ->
            for Operators.KeyValue(name, _) in findBindings pattern do
              let n: string = name

              let t =
                match env.GetType name with
                | Ok(t) -> t
                | Error(e) -> failwith $"Couldn't find symbol: {name}"

              let decl =
                { Id =
                    Pat.Ident
                      { Id = { Name = n; Loc = None }
                        Optional = false
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
        | _ -> ()

    { Body = List.rev items
      Shebang = None
      Loc = None }

  let buildTypeAnn (ctx: Ctx) (t: Type) : TsTypeAnn =
    { TypeAnn = buildType ctx t
      Loc = None }

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
        { TypeName = TsEntityName.Identifier { Name = name; Loc = None }
          TypeParams = typeArgs
          Loc = None }
    | TypeKind.Primitive p ->
      let kind =
        match p with
        | Boolean -> TsKeywordTypeKind.TsBooleanKeyword
        | Number -> TsKeywordTypeKind.TsNumberKeyword
        | String -> TsKeywordTypeKind.TsStringKeyword
        | Symbol -> TsKeywordTypeKind.TsSymbolKeyword

      TsType.TsKeywordType { Kind = kind; Loc = None }
    | TypeKind.Keyword keyword ->
      let kind =
        match keyword with
        | Keyword.Never -> TsKeywordTypeKind.TsNeverKeyword
        | Keyword.Object -> TsKeywordTypeKind.TsObjectKeyword
        | Keyword.Unknown -> TsKeywordTypeKind.TsUnknownKeyword

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
                  Optional = false
                  Loc = None }

            { Pat = pat
              TypeAnn = Some(t)
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
    | TypeKind.Rest rest ->
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
    | Method(name, isMut, fn) -> failwith "todo"
    | Getter(name, returnType, throws) -> failwith "todo"
    | Setter(name, param, throws) -> failwith "todo"
    | Mapped mapped -> failwith "todo"

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

    walk pat

    assump
