namespace Escalier.TypeChecker

open Escalier.Data.Syntax

// TODO: move to Escalier.Data.Visitor
module rec ExprVisitor =

  type SyntaxVisitor<'S> =
    { VisitExpr: Expr * 'S -> bool * 'S
      VisitJsxElement: JSXElement * 'S -> bool * 'S
      VisitJsxFragment: JSXFragment * 'S -> bool * 'S
      VisitJsxText: JSXText * 'S -> bool * 'S
      VisitStmt: Stmt * 'S -> bool * 'S
      VisitPattern: Pattern * 'S -> bool * 'S
      VisitTypeAnn: TypeAnn * 'S -> bool * 'S
      VisitTypeAnnObjElem: ObjTypeAnnElem * 'S -> bool * 'S }

  let walkExpr (visitor: SyntaxVisitor<'S>) (state: 'S) (expr: Expr) : unit =
    let rec walk (expr: Expr) : unit =
      let cont, state = visitor.VisitExpr(expr, state)

      if cont then
        match expr.Kind with
        | ExprKind.Identifier _ -> ()
        | ExprKind.Literal _ -> ()
        | ExprKind.Function f ->
          for p in f.Sig.ParamList do
            walkPattern visitor state p.Pattern
            Option.iter (walkTypeAnn visitor state) p.TypeAnn

          match f.Body with
          | BlockOrExpr.Block block ->
            List.iter (walkStmt visitor state) block.Stmts
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

          List.iter (walkStmt visitor state) thenBranch.Stmts

          Option.iter
            (fun elseBranch ->
              match elseBranch with
              | BlockOrExpr.Block block ->
                List.iter (walkStmt visitor state) block.Stmts
              | BlockOrExpr.Expr expr -> walk expr)
            elseBranch
        | ExprKind.Match(target, cases) ->
          walk target

          List.iter
            (fun (case: MatchCase) ->
              walkPattern visitor state case.Pattern

              match case.Body with
              | BlockOrExpr.Block block ->
                List.iter (walkStmt visitor state) block.Stmts
              | BlockOrExpr.Expr expr -> walk expr

              Option.iter walk case.Guard)
            cases
        | ExprKind.Assign(_op, left, right)
        | ExprKind.Binary(_op, left, right) ->
          walk left
          walk right
        | ExprKind.Unary(_op, value) -> walk value
        | ExprKind.Object obj ->
          for elem in obj.Elems do
            match elem with
            | ObjElem.Property(span, key, value) -> walk value
            | ObjElem.Shorthand(span, name) -> ()
            | ObjElem.Spread(span, value) -> walk value
        | ExprKind.Try { Body = body
                         Catch = catch
                         Finally = fin } ->
          List.iter (walkStmt visitor state) body.Stmts

          Option.iter
            (fun cases ->
              List.iter
                (fun (case: MatchCase) ->
                  walkPattern visitor state case.Pattern

                  match case.Body with
                  | BlockOrExpr.Block block ->
                    List.iter (walkStmt visitor state) block.Stmts
                  | BlockOrExpr.Expr expr -> walk expr

                  Option.iter walk case.Guard)
                cases)
            catch

          Option.iter
            (fun body -> List.iter (walkStmt visitor state) body.Stmts)
            fin
        | ExprKind.Do body -> List.iter (walkStmt visitor state) body.Stmts
        | ExprKind.Await await -> walk await.Value
        | ExprKind.Throw value -> walk value
        | ExprKind.TemplateLiteral { Exprs = exprs } -> List.iter walk exprs
        | ExprKind.TaggedTemplateLiteral(tag, template, throws) ->
          List.iter walk template.Exprs
        | ExprKind.ExprWithTypeArgs(target, typeArgs) ->
          walk target
          List.iter (walkTypeAnn visitor state) typeArgs
        | ExprKind.Class(_) ->
          // TODO: implement this so that we can find dependencies inside classes
          ()
        | ExprKind.Range(_) -> failwith "TODO: walkExpr - Range"
        | ExprKind.IfLet(pattern, target, thenBranch, elseBranch) ->
          walkPattern visitor state pattern
          walk target
          List.iter (walkStmt visitor state) thenBranch.Stmts

          Option.iter
            (fun (elseBranch: BlockOrExpr) ->
              match elseBranch with
              | BlockOrExpr.Block block ->
                List.iter (walkStmt visitor state) block.Stmts
              | BlockOrExpr.Expr expr -> walk expr)
            elseBranch
        | ExprKind.JSXElement jsxElement ->
          walkJsxElement visitor state jsxElement
        | ExprKind.JSXFragment jsxFragment ->
          walkJsxFragment visitor state jsxFragment

    walk expr

  let walkJsxElement
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (jsxElement: JSXElement)
    : unit =
    let rec walk (jsxElement: JSXElement) : unit =
      let cont, state = visitor.VisitJsxElement(jsxElement, state)

      if cont then
        for attr in jsxElement.Opening.Attrs do
          match attr.Value with
          | None -> failwith "TODO: walkJsxElement - attr.Value = None"
          | Some value ->
            match value with
            | JSXAttrValue.Str literal -> ()
            | JSXAttrValue.JSXExprContainer { Expr = expr } ->
              walkExpr visitor state expr
            | JSXAttrValue.JSXElement jsxElement -> walk jsxElement
            | JSXAttrValue.JSXFragment jsxFragment ->
              walkJsxFragment visitor state jsxFragment

        for child in jsxElement.Children do
          match child with
          | JSXElementChild.JSXText jsxText -> walkJsxText visitor state jsxText
          | JSXElementChild.JSXElement jsxElement -> walk jsxElement
          | JSXElementChild.JSXFragment jsxFragment ->
            walkJsxFragment visitor state jsxFragment
          | JSXElementChild.JSXExprContainer { Expr = expr } ->
            walkExpr visitor state expr

    walk jsxElement

  let walkJsxFragment
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (jsxFragment: JSXFragment)
    : unit =
    let rec walk (jsxFragment: JSXFragment) : unit =
      let cont, state = visitor.VisitJsxFragment(jsxFragment, state)

      if cont then
        for child in jsxFragment.Children do
          match child with
          | JSXElementChild.JSXText jsxText -> walkJsxText visitor state jsxText
          | JSXElementChild.JSXElement jsxElement ->
            walkJsxElement visitor state jsxElement
          | JSXElementChild.JSXFragment jsxFragment -> walk jsxFragment
          | JSXElementChild.JSXExprContainer { Expr = expr } ->
            walkExpr visitor state expr

    walk jsxFragment

  let walkJsxText
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (jsxText: JSXText)
    : unit =
    let cont, state = visitor.VisitJsxText(jsxText, state)

    if cont then
      ()

  // TODO: update walkDecl to be more like walkExpr and walkStmt
  let walkDecl (visitor: SyntaxVisitor<'S>) (state: 'S) (decl: Decl) : unit =
    match decl.Kind with
    | DeclKind.VarDecl { Pattern = pattern
                         TypeAnn = typeAnn
                         Init = init } ->
      walkPattern visitor state pattern
      Option.iter (walkTypeAnn visitor state) typeAnn
      Option.iter (walkExpr visitor state) init
    | DeclKind.FnDecl { Sig = fnSig; Body = body } ->
      match fnSig.TypeParams with
      | Some typeParams ->
        List.iter
          (fun (typeParam: TypeParam) ->
            // TODO: include type param name in visitor state
            Option.iter (walkTypeAnn visitor state) typeParam.Constraint
            Option.iter (walkTypeAnn visitor state) typeParam.Default)
          typeParams
      | _ -> ()

      List.iter
        (fun (param: FuncParam) ->
          Option.iter (walkTypeAnn visitor state) param.TypeAnn)
        fnSig.ParamList

      Option.iter
        (fun body ->
          match body with
          | BlockOrExpr.Expr expr -> walkExpr visitor state expr
          | BlockOrExpr.Block block ->
            List.iter (walkStmt visitor state) block.Stmts)
        body

      Option.iter (walkTypeAnn visitor state) fnSig.ReturnType
    | DeclKind.TypeDecl { TypeAnn = typeAnn } ->
      // TODO: walk type params
      walkTypeAnn visitor state typeAnn
    | DeclKind.EnumDecl { Variants = variants } ->
      List.iter
        (fun (variant: EnumVariant) ->
          Option.iter (walkTypeAnn visitor state) variant.TypeAnn)
        variants
    | DeclKind.NamespaceDecl { Body = body } ->
      List.iter (walkDecl visitor state) body
    | DeclKind.ClassDecl(_) -> failwith "TODO: walkDecl - ClassDecl"
    | DeclKind.InterfaceDecl(_) -> failwith "TODO: walkDecl - InterfaceDecl"

  let walkStmt (visitor: SyntaxVisitor<'S>) (state: 'S) (stmt: Stmt) : unit =
    let rec walk (state: 'S) (stmt: Stmt) : unit =
      let cont, state = visitor.VisitStmt(stmt, state)

      if cont then
        match stmt.Kind with
        | StmtKind.Expr expr -> walkExpr visitor state expr
        | StmtKind.For(left, right, body) ->
          walkPattern visitor state left
          walkExpr visitor state right
          List.iter (walk state) body.Stmts
        | StmtKind.Decl decl -> walkDecl visitor state decl
        | StmtKind.Return exprOption ->
          Option.iter (walkExpr visitor state) exprOption

    walk state stmt

  let walkPattern
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (pat: Pattern)
    : unit =
    let rec walk (state: 'S) (pat: Pattern) : unit =
      let cont, state = visitor.VisitPattern(pat, state)

      if cont then
        match pat.Kind with
        | PatternKind.Ident _ -> ()
        | PatternKind.Object { Elems = elems } ->
          List.iter
            (fun (elem: ObjPatElem) ->
              match elem with
              | ObjPatElem.KeyValuePat { Value = value; Default = init } ->
                walk state value
                Option.iter (walkExpr visitor state) init
              | ObjPatElem.ShorthandPat { Default = init } ->
                Option.iter (walkExpr visitor state) init
              | ObjPatElem.RestPat { Target = target } -> walk state target)
            elems
        | PatternKind.Tuple { Elems = elems } -> List.iter (walk state) elems
        | PatternKind.Wildcard _ -> ()
        | PatternKind.Literal _ -> ()
        | PatternKind.Rest arg -> (walk state) arg
        | PatternKind.Enum { Arg = arg } -> Option.iter (walk state) arg

    walk state pat

  let walkTypeAnn
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (typeAnn: TypeAnn)
    : unit =
    let rec walk (state: 'S) (typeAnn: TypeAnn) : unit =
      let cont, state = visitor.VisitTypeAnn(typeAnn, state)

      if cont then
        match typeAnn.Kind with
        | TypeAnnKind.Array elem -> walk state elem
        | TypeAnnKind.Literal _ -> ()
        | TypeAnnKind.Keyword _ -> ()
        | TypeAnnKind.Object { Elems = elems } ->
          for elem in elems do
            walkTypeAnnObjElem visitor state elem
        | TypeAnnKind.Tuple { Elems = elems } -> List.iter (walk state) elems
        | TypeAnnKind.Union types -> List.iter (walk state) types
        | TypeAnnKind.Intersection types -> List.iter (walk state) types
        | TypeAnnKind.TypeRef { TypeArgs = typeArgs } ->
          Option.iter (List.iter (walk state)) typeArgs
        | TypeAnnKind.Function f ->
          Option.iter (walk state) f.ReturnType
          Option.iter (walk state) f.Throws

          List.iter
            (fun (param: FuncParam) -> Option.iter (walk state) param.TypeAnn)
            f.ParamList
        | TypeAnnKind.Keyof target -> walk state target
        | TypeAnnKind.Rest target -> walk state target
        | TypeAnnKind.Typeof _ -> () // TODO: walk QualifiedIdents
        | TypeAnnKind.Index(target, index) ->
          walk state target
          walk state index
        | TypeAnnKind.Condition conditionType ->
          walk state conditionType.Check
          walk state conditionType.Extends
          walk state conditionType.TrueType
          walk state conditionType.FalseType
        | TypeAnnKind.Match matchType -> failwith "todo"
        | TypeAnnKind.Infer _name -> ()
        | TypeAnnKind.Wildcard -> ()
        | TypeAnnKind.Binary(left, op, right) ->
          walk state left
          walk state right
        | TypeAnnKind.Range { Min = min; Max = max } ->
          walk state min
          walk state max
        | TypeAnnKind.TemplateLiteral { Exprs = expr } ->
          List.iter (walk state) expr

    walk state typeAnn

  let walkTypeAnnObjElem
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (elem: ObjTypeAnnElem)
    : unit =
    let cont, state = visitor.VisitTypeAnnObjElem(elem, state)

    if cont then
      let walk = walkTypeAnn visitor state

      match elem with
      | Callable f ->
        Option.iter walk f.ReturnType
        Option.iter walk f.Throws

        List.iter
          (fun (param: FuncParam) -> Option.iter walk param.TypeAnn)
          f.ParamList
      | Constructor f ->
        Option.iter walk f.ReturnType
        Option.iter walk f.Throws

        List.iter
          (fun (param: FuncParam) -> Option.iter walk param.TypeAnn)
          f.ParamList
      | Method { Type = f } ->
        Option.iter walk f.ReturnType
        Option.iter walk f.Throws

        List.iter
          (fun (param: FuncParam) -> Option.iter walk param.TypeAnn)
          f.ParamList
      | Getter { ReturnType = retType } -> walk retType
      | Setter { Param = { TypeAnn = paramType } } -> Option.iter walk paramType
      | Property { TypeAnn = typeAnn } -> walk typeAnn
      | Mapped { TypeParam = typeParam
                 TypeAnn = typeAnn } ->
        walk typeParam.Constraint
        walk typeAnn
      | Spread { Arg = arg } -> walk arg

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
            | Getter(_, fn) -> walkFunction walk fn
            | Setter(_, fn) -> walkFunction walk fn
            | RestSpread t -> walk t)
          elems

      | TypeKind.RestSpread t -> walk t
      | TypeKind.Union types -> List.iter walk types
      | TypeKind.Intersection types -> List.iter walk types
      | TypeKind.Array { Elem = elem; Length = length } ->
        walk elem
        walk length
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
