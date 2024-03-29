namespace Escalier.TypeChecker

// TODO: move to Escalier.Data.Visitor
module rec ExprVisitor =
  open Escalier.Data.Syntax

  type SyntaxVisitor =
    { VisitExpr: Expr -> bool
      VisitStmt: Stmt -> bool
      VisitPattern: Pattern -> bool
      VisitTypeAnn: TypeAnn -> bool }

  let walkExpr (visitor: SyntaxVisitor) (expr: Expr) : unit =
    let rec walk (expr: Expr) : unit =
      if visitor.VisitExpr expr then
        match expr.Kind with
        | ExprKind.Identifier _ -> ()
        | ExprKind.Literal _ -> ()
        | ExprKind.Function f ->
          // TODO: walk type annotations
          match f.Body with
          | BlockOrExpr.Block block -> List.iter (walkStmt visitor) block.Stmts
          | BlockOrExpr.Expr expr -> walk expr
        | ExprKind.Call call ->
          walk call.Callee
          List.iter walk call.Args
        | ExprKind.New { Callee = callee; Args = args } ->
          walk callee
          // TODO: make args required
          Option.iter (List.iter walk) args
        | ExprKind.Tuple { Elems = elems } -> List.iter walk elems
        | ExprKind.Index(target, index, _optChain) ->
          walk target
          walk index
        | ExprKind.Member(target, _name, _optChain) -> walk target
        | ExprKind.IfElse(condition, thenBranch, elseBranch) ->
          walk condition

          List.iter (walkStmt visitor) thenBranch.Stmts

          Option.iter
            (fun elseBranch ->
              match elseBranch with
              | BlockOrExpr.Block block ->
                List.iter (walkStmt visitor) block.Stmts
              | BlockOrExpr.Expr expr -> walk expr)
            elseBranch
        | ExprKind.Match(target, cases) ->
          walk target

          List.iter
            (fun (case: MatchCase) ->
              walkPattern visitor case.Pattern

              match case.Body with
              | BlockOrExpr.Block block ->
                List.iter (walkStmt visitor) block.Stmts
              | BlockOrExpr.Expr expr -> walk expr

              Option.iter walk case.Guard)
            cases
        | ExprKind.Assign(_op, left, right)
        | ExprKind.Binary(_op, left, right) ->
          walk left
          walk right
        | ExprKind.Unary(_op, value) -> walk value
        | ExprKind.Object elems ->
          // TODO:
          ()
        | ExprKind.Try { Body = body
                         Catch = catch
                         Finally = fin } ->
          List.iter (walkStmt visitor) body.Stmts

          Option.iter
            (fun cases ->
              List.iter
                (fun (case: MatchCase) ->
                  walkPattern visitor case.Pattern

                  match case.Body with
                  | BlockOrExpr.Block block ->
                    List.iter (walkStmt visitor) block.Stmts
                  | BlockOrExpr.Expr expr -> walk expr

                  Option.iter walk case.Guard)
                cases)
            catch

          Option.iter (fun body -> List.iter (walkStmt visitor) body.Stmts) fin
        | ExprKind.Do body -> List.iter (walkStmt visitor) body.Stmts
        | ExprKind.Await await -> walk await.Value
        | ExprKind.Throw value -> walk value
        | ExprKind.TemplateLiteral { Exprs = exprs } -> List.iter walk exprs
        | ExprKind.TaggedTemplateLiteral(tag, template, throws) ->
          List.iter walk template.Exprs

    walk expr

  let walkStmt (visitor: SyntaxVisitor) (stmt: Stmt) : unit =
    let rec walk (stmt: Stmt) : unit =
      if visitor.VisitStmt stmt then
        match stmt.Kind with
        | StmtKind.Expr expr -> walkExpr visitor expr
        | StmtKind.For(left, right, body) ->
          walkPattern visitor left
          walkExpr visitor right
          List.iter walk body.Stmts
        | StmtKind.Decl({ Kind = DeclKind.VarDecl { Init = init } }) ->
          // TODO: walk typeAnn
          walkExpr visitor init
        | StmtKind.Decl({ Kind = DeclKind.TypeDecl { TypeAnn = typeAnn } }) ->
          // TODO: walk type params
          walkTypeAnn visitor typeAnn
        | StmtKind.Return exprOption ->
          Option.iter (walkExpr visitor) exprOption

    walk stmt

  let walkPattern (visitor: SyntaxVisitor) (pat: Pattern) : unit =
    let rec walk (pat: Pattern) : unit =
      if visitor.VisitPattern pat then
        match pat.Kind with
        | PatternKind.Ident _ -> ()
        | PatternKind.Object { Elems = elems } ->
          List.iter
            (fun (elem: ObjPatElem) ->
              match elem with
              | ObjPatElem.KeyValuePat { Value = value; Default = init } ->
                walk value
                Option.iter (walkExpr visitor) init
              | ObjPatElem.ShorthandPat { Default = init } ->
                Option.iter (walkExpr visitor) init
              | ObjPatElem.RestPat { Target = target } -> walk target)
            elems
        | PatternKind.Tuple { Elems = elems } -> List.iter walk elems
        | PatternKind.Wildcard _ -> ()
        | PatternKind.Literal _ -> ()
        | PatternKind.Rest arg -> walk arg

    walk pat

  let walkTypeAnn (visitor: SyntaxVisitor) (typeAnn: TypeAnn) : unit =
    let rec walk (typeAnn: TypeAnn) : unit =
      if visitor.VisitTypeAnn typeAnn then
        match typeAnn.Kind with
        | TypeAnnKind.Array elem -> walk elem
        | TypeAnnKind.Literal _ -> ()
        | TypeAnnKind.Keyword _ -> ()
        | TypeAnnKind.Object elems -> failwith "todo"
        | TypeAnnKind.Tuple { Elems = elems } -> List.iter walk elems
        | TypeAnnKind.Union types -> List.iter walk types
        | TypeAnnKind.Intersection types -> List.iter walk types
        | TypeAnnKind.TypeRef { TypeArgs = typeArgs } ->
          Option.iter (List.iter walk) typeArgs
        | TypeAnnKind.Function f ->
          walk f.ReturnType
          Option.iter walk f.Throws

          List.iter
            (fun (param: FuncParam<TypeAnn>) -> walk param.TypeAnn)
            f.ParamList
        | TypeAnnKind.Keyof target -> walk target
        | TypeAnnKind.Rest target -> walk target
        | TypeAnnKind.Typeof _ -> () // TODO: walk QualifiedIdents
        | TypeAnnKind.Index(target, index) ->
          walk target
          walk index
        | TypeAnnKind.Condition conditionType ->
          walk conditionType.Check
          walk conditionType.Extends
          walk conditionType.TrueType
          walk conditionType.FalseType
        | TypeAnnKind.Match matchType -> failwith "todo"
        | TypeAnnKind.Infer _name -> ()
        | TypeAnnKind.Wildcard -> ()
        | TypeAnnKind.Binary(left, op, right) ->
          walk left
          walk right

    walk typeAnn

module rec TypeVisitor =
  open Escalier.Data.Type

  open Prune

  let walkFunction (walk: Type -> unit) (f: Function) : unit =
    List.iter (fun (param: FuncParam) -> walk param.Type) f.ParamList
    walk f.Return
    walk f.Throws

  // TODO: support early termination
  let walkType (f: Type -> unit) (t: Type) : unit =
    let rec walk (t: Type) : unit =
      let t = prune t

      match t.Kind with
      | TypeKind.TypeVar _ -> ()
      | TypeKind.Primitive _ -> ()
      | TypeKind.Keyword _ -> ()
      | TypeKind.Function fn -> walkFunction walk fn
      | TypeKind.Tuple { Elems = elems } -> List.iter walk elems
      | TypeKind.TypeRef { TypeArgs = typeArgs } ->
        // We explicitly don't walk the scheme here, it isn't necessary for any
        // use cases we're aware of.  This avoids infinite loops in the case of
        // recursive types.
        Option.iter (List.iter walk) typeArgs
      | TypeKind.Literal _ -> ()
      | TypeKind.Wildcard -> ()
      | TypeKind.Object { Elems = elems } ->
        List.iter
          (fun elem ->
            match elem with
            | Property p -> walk p.Type
            | Mapped m ->
              walk m.TypeParam.Constraint
              Option.iter walk m.NameType
              walk m.TypeAnn
            | Constructor fn -> walkFunction walk fn
            | Callable fn -> walkFunction walk fn
            | Method(_, fn) -> walkFunction walk fn
            | _ ->
              printfn "elem = %A" elem
              failwith "TODO: walkType - ObjTypeElem")
          elems

      | TypeKind.Rest t -> walk t
      | TypeKind.Union types -> List.iter walk types
      | TypeKind.Intersection types -> List.iter walk types
      | TypeKind.Array { Elem = elem; Length = length } ->
        walk elem
        walk length
      | TypeKind.EnumVariant { Types = types } -> List.iter walk types
      | TypeKind.KeyOf t -> walk t
      | TypeKind.Index(target, index) ->
        walk target
        walk index
      | TypeKind.Condition { Check = check
                             Extends = extends
                             TrueType = trueType
                             FalseType = falseType } ->
        walk check
        walk extends
        walk trueType
        walk falseType
      | TypeKind.Infer _ -> ()
      | TypeKind.Binary(left, op, right) ->
        walk left
        walk right
      | TypeKind.Unary(op, arg) -> walk arg
      | TypeKind.Range { Min = min; Max = max } ->
        walk min
        walk max
      | TypeKind.UniqueNumber _ -> ()
      | TypeKind.UniqueSymbol _ -> ()
      | TypeKind.TemplateLiteral { Exprs = exprs } -> List.iter walk exprs
      | TypeKind.Typeof _ -> ()
      | kind ->
        printfn "kind = %A" kind
        failwith "TODO: walkType"

      f t

    walk t
