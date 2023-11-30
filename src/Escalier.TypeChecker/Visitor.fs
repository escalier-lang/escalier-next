namespace Escalier.TypeChecker

open Escalier.Data.Syntax

module rec Visitor =
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
          match f.Body with
          | BlockOrExpr.Block block -> List.iter (walkStmt visitor) block.Stmts
          | BlockOrExpr.Expr expr -> walk expr
        | ExprKind.Call call ->
          walk call.Callee
          List.iter walk call.Args
        | ExprKind.Tuple elements -> List.iter walk elements
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
              walk case.Body
              Option.iter walk case.Guard)
            cases

          failwith "todo"
        | ExprKind.Assign(_op, left, right)
        | ExprKind.Binary(_op, left, right) ->
          walk left
          walk right
        | ExprKind.Unary(_op, value) -> walk value
        | ExprKind.Object elems -> failwith "todo"
        | ExprKind.Try(body, catch, fin) ->
          List.iter (walkStmt visitor) body.Stmts

          Option.iter
            (fun (e, body) ->
              walk e
              List.iter (walkStmt visitor) body.Stmts)
            catch

          Option.iter (fun body -> List.iter (walkStmt visitor) body.Stmts) fin
        | ExprKind.Do body -> List.iter (walkStmt visitor) body.Stmts
        | ExprKind.Await value -> walk value
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
        | StmtKind.Decl({ Kind = DeclKind.VarDecl(_name, init, typeAnn) }) ->
          // TODO: walk typeAnn
          walkExpr visitor init
        | StmtKind.Decl({ Kind = DeclKind.TypeDecl _ }) ->
          failwith "TODO: walkStmt - TypeDecl"
        | StmtKind.Return exprOption ->
          Option.iter (walkExpr visitor) exprOption

    walk stmt

  let walkPattern (visitor: SyntaxVisitor) (pat: Pattern) : unit =
    let rec walk (pat: Pattern) : unit =
      if visitor.VisitPattern pat then
        match pat.Kind with
        | PatternKind.Identifier _ -> ()
        | PatternKind.Object elems ->
          List.iter
            (fun (elem: ObjPatElem) ->
              match elem with
              | ObjPatElem.KeyValuePat(span, key, value, init) -> walk value
              | ObjPatElem.ShorthandPat(span, name, init, isMut) ->
                Option.iter (walkExpr visitor) init
              | ObjPatElem.RestPat(span, target, isMut) -> walk target)
            elems
        | PatternKind.Tuple elems -> List.iter walk elems
        | PatternKind.Wildcard -> ()
        | PatternKind.Literal(span, value) -> ()
        | PatternKind.Is(span, ident, isName, isMut) -> ()

    walk pat

  let walkTypeAnn (visitor: SyntaxVisitor) (typeAnn: TypeAnn) : unit =
    let rec walk (typeAnn: TypeAnn) : unit =
      if visitor.VisitTypeAnn typeAnn then
        match typeAnn.Kind with
        | TypeAnnKind.Array elem -> walk elem
        | TypeAnnKind.Literal _ -> ()
        | TypeAnnKind.Keyword _ -> ()
        | TypeAnnKind.Object elems -> failwith "todo"
        | TypeAnnKind.Tuple elems -> List.iter walk elems
        | TypeAnnKind.Union types -> List.iter walk types
        | TypeAnnKind.Intersection types -> List.iter walk types
        | TypeAnnKind.TypeRef(_name, typeArgs) ->
          Option.iter (List.iter walk) typeArgs
        | TypeAnnKind.Function f ->
          walk f.ReturnType
          Option.iter walk f.Throws

          List.iter
            (fun (param: FuncParam<TypeAnn>) -> walk param.TypeAnn)
            f.ParamList
        | TypeAnnKind.Keyof target -> walk target
        | TypeAnnKind.Rest target -> walk target
        | TypeAnnKind.Typeof target -> walkExpr visitor target
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