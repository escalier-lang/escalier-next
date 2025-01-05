module Escalier.Data.Visitor

module rec ExprVisitor =
  open Escalier.Data.Syntax

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
    let cont, state = visitor.VisitExpr(expr, state)

    if cont then
      let walk = walkExpr visitor state

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
      | ExprKind.Index { Target = target; Index = index } ->
        walk target
        walk index
      | ExprKind.Member { Target = target } -> walk target
      | ExprKind.IfElse { Condition = condition
                          Then = thenBranch
                          Else = elseBranch } ->
        walk condition

        List.iter (walkStmt visitor state) thenBranch.Stmts

        Option.iter
          (fun elseBranch ->
            match elseBranch with
            | BlockOrExpr.Block block ->
              List.iter (walkStmt visitor state) block.Stmts
            | BlockOrExpr.Expr expr -> walk expr)
          elseBranch
      | ExprKind.Match { Target = target; Cases = cases } ->
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
      | ExprKind.Assign { Left = left; Right = right }
      | ExprKind.Binary { Left = left; Right = right } ->
        walk left
        walk right
      | ExprKind.Unary { Value = value } -> walk value
      | ExprKind.Object obj ->
        for elem in obj.Elems do
          match elem with
          | ObjElem.Property { Value = value } -> walk value
          | ObjElem.Shorthand _ -> ()
          | ObjElem.Spread { Value = value } -> walk value
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
      | ExprKind.TaggedTemplateLiteral { Tag = tag; Template = template } ->
        walk tag
        List.iter walk template.Exprs
      | ExprKind.ExprWithTypeArgs { Expr = expr; TypeArgs = typeArgs } ->
        walk expr
        List.iter (walkTypeAnn visitor state) typeArgs
      | ExprKind.Class _ ->
        // TODO: implement this so that we can find dependencies inside classes
        ()
      | ExprKind.IfLet { Pattern = pattern
                         Target = target
                         Then = thenBranch
                         Else = elseBranch } ->
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

  let walkJsxElement
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (jsxElement: JSXElement)
    : unit =
    let cont, state = visitor.VisitJsxElement(jsxElement, state)

    if cont then
      let walk = walkJsxElement visitor state

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

  let walkJsxFragment
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (jsxFragment: JSXFragment)
    : unit =

    let cont, state = visitor.VisitJsxFragment(jsxFragment, state)

    if cont then
      for child in jsxFragment.Children do
        match child with
        | JSXElementChild.JSXText jsxText -> walkJsxText visitor state jsxText
        | JSXElementChild.JSXElement jsxElement ->
          walkJsxElement visitor state jsxElement
        | JSXElementChild.JSXFragment jsxFragment ->
          walkJsxFragment visitor state jsxFragment
        | JSXElementChild.JSXExprContainer { Expr = expr } ->
          walkExpr visitor state expr

  let walkJsxText
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (jsxText: JSXText)
    : unit =
    let cont, state = visitor.VisitJsxText(jsxText, state)
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
    | DeclKind.ClassDecl _ -> failwith "TODO: walkDecl - ClassDecl"
    | DeclKind.InterfaceDecl _ -> failwith "TODO: walkDecl - InterfaceDecl"

  let walkStmt (visitor: SyntaxVisitor<'S>) (state: 'S) (stmt: Stmt) : unit =
    let cont, state = visitor.VisitStmt(stmt, state)

    if cont then
      match stmt.Kind with
      | StmtKind.Expr expr -> walkExpr visitor state expr
      | StmtKind.For { Left = left
                       Right = right
                       Body = body } ->
        walkPattern visitor state left
        walkExpr visitor state right
        List.iter (walkStmt visitor state) body.Stmts
      | StmtKind.Decl decl -> walkDecl visitor state decl
      | StmtKind.Return exprOption ->
        Option.iter (walkExpr visitor state) exprOption

  let walkPattern
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (pat: Pattern)
    : unit =
    let cont, state = visitor.VisitPattern(pat, state)

    if cont then
      let walk = walkPattern visitor state

      match pat.Kind with
      | PatternKind.Ident _ -> ()
      | PatternKind.Object { Elems = elems } ->
        List.iter
          (fun (elem: ObjPatElem) ->
            match elem with
            | ObjPatElem.KeyValuePat { Value = value; Default = init } ->
              walk value
              Option.iter (walkExpr visitor state) init
            | ObjPatElem.ShorthandPat { Default = init } ->
              Option.iter (walkExpr visitor state) init
            | ObjPatElem.RestPat { Target = target } -> walk target)
          elems
      | PatternKind.Tuple { Elems = elems } -> List.iter walk elems
      | PatternKind.Wildcard _ -> ()
      | PatternKind.Literal _ -> ()
      | PatternKind.Rest arg -> walk arg
      | PatternKind.Enum { Arg = arg } -> Option.iter walk arg

  let walkTypeAnn
    (visitor: SyntaxVisitor<'S>)
    (state: 'S)
    (typeAnn: TypeAnn)
    : unit =
    let cont, state = visitor.VisitTypeAnn(typeAnn, state)

    if cont then
      let walk = walkTypeAnn visitor state

      match typeAnn.Kind with
      | TypeAnnKind.Array elem -> walk elem
      | TypeAnnKind.Literal _ -> ()
      | TypeAnnKind.Keyword _ -> ()
      | TypeAnnKind.Object { Elems = elems } ->
        for elem in elems do
          walkTypeAnnObjElem visitor state elem
      | TypeAnnKind.Tuple { Elems = elems } -> List.iter walk elems
      | TypeAnnKind.Union types -> List.iter walk types
      | TypeAnnKind.Intersection types -> List.iter walk types
      | TypeAnnKind.TypeRef { TypeArgs = typeArgs } ->
        Option.iter (List.iter walk) typeArgs
      | TypeAnnKind.Function f ->
        Option.iter walk f.ReturnType
        Option.iter walk f.Throws

        List.iter
          (fun (param: FuncParam) -> Option.iter walk param.TypeAnn)
          f.ParamList
      | TypeAnnKind.Keyof target -> walk target
      | TypeAnnKind.Rest target -> walk target
      | TypeAnnKind.Typeof _ -> () // TODO: walk QualifiedIdents
      | TypeAnnKind.Index { Target = target; Index = index } ->
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
      | TypeAnnKind.Binary { Left = left; Right = right } ->
        walk left
        walk right
      | TypeAnnKind.TemplateLiteral { Exprs = expr } -> List.iter walk expr
      | TypeAnnKind.Intrinsic -> ()
      | TypeAnnKind.ImportType _ -> ()

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
      | Property { TypeAnn = typeAnn; Value = value } ->
        Option.iter walk typeAnn
        Option.iter (walkExpr visitor state) value
      | Mapped { TypeParam = typeParam
                 TypeAnn = typeAnn } ->
        walk typeParam.Constraint
        walk typeAnn
      | Spread { Arg = arg } -> walk arg

module rec TypeVisitor =
  open Escalier.Data.Type

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
            | Method { Fn = fn } -> walkFunction walk fn
            | Getter { Fn = fn } -> walkFunction walk fn
            | Setter { Fn = fn } -> walkFunction walk fn
            | RestSpread t -> walk t)
          elems

      | TypeKind.RestSpread t -> walk t
      | TypeKind.Union types -> List.iter walk types
      | TypeKind.Intersection types -> List.iter walk types
      | TypeKind.Array { Elem = elem } -> walk elem
      | TypeKind.KeyOf t -> walk t
      | TypeKind.Index { Target = target; Index = index } ->
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
      | TypeKind.Binary { Left = left; Right = right } ->
        walk left
        walk right
      | TypeKind.Unary { Arg = arg } -> walk arg
      | TypeKind.UniqueSymbol _ -> ()
      | TypeKind.TemplateLiteral { Exprs = exprs } -> List.iter walk exprs
      | TypeKind.Typeof _ -> ()
      | kind ->
        printfn "kind = %A" kind
        failwith "TODO: walkType"

      f t

    walk t
