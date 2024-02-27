namespace Escalier.Interop

open Escalier.Data
open Escalier.Interop.TypeScript

module Migrate =
  let rec migrateExpr (expr: TypeScript.Expr) : Syntax.Expr =

    let kind =
      match expr with
      | Expr.This thisExpr -> failwith "todo"
      | Expr.Array arrayLit -> failwith "todo"
      | Expr.Object objectLit -> failwith "todo"
      | Expr.Fn fnExpr -> failwith "todo"
      | Expr.Unary unaryExpr -> failwith "todo"
      | Expr.Update updateExpr -> failwith "todo"
      | Expr.Bin binExpr -> failwith "todo"
      | Expr.Assign assignExpr -> failwith "todo"
      | Expr.Member memberExpr ->
        let obj = migrateExpr memberExpr.Object

        let prop =
          match memberExpr.Property with
          | Expr.Ident ident -> ident.Name
          | _ -> failwith "todo"

        Syntax.ExprKind.Member(obj, prop, false)
      | Expr.SuperProp superPropExpr -> failwith "todo"
      | Expr.Cond condExpr -> failwith "todo"
      | Expr.Call callExpr -> failwith "todo"
      | Expr.New newExpr -> failwith "todo"
      | Expr.Seq seqExpr -> failwith "todo"
      | Expr.Ident ident -> Syntax.ExprKind.Identifier ident.Name
      | Expr.Lit lit -> failwith "todo"
      | Expr.Tpl tpl -> failwith "todo"
      | Expr.TaggedTpl taggedTpl -> failwith "todo"
      | Expr.Arrow arrowExpr -> failwith "todo"
      | Expr.Class classExpr -> failwith "todo"
      | Expr.Yield yieldExpr -> failwith "todo"
      | Expr.MetaProp metaPropExpr -> failwith "todo"
      | Expr.Await awaitExpr -> failwith "todo"
      | Expr.Paren parenExpr -> failwith "todo"
      | Expr.JSXMember jsxMemberExpr -> failwith "todo"
      | Expr.JSXNamespacedName jsxNamespacedName -> failwith "todo"
      | Expr.JSXEmpty jsxEmptyExpr -> failwith "todo"
      | Expr.JSXElement jsxElement -> failwith "todo"
      | Expr.JSXFragment jsxFragment -> failwith "todo"
      | Expr.TsTypeAssertion tsTypeAssertion -> failwith "todo"
      | Expr.TsConstAssertion tsConstAssertion -> failwith "todo"
      | Expr.TsNonNull tsNonNullExpr -> failwith "todo"
      | Expr.TsAs tsAsExpr -> failwith "todo"
      | Expr.TsInstantiation tsInstantiation -> failwith "todo"
      | Expr.TsSatisfies tsSatisfiesExpr -> failwith "todo"
      | Expr.PrivateName privateName -> failwith "todo"
      | Expr.OptChain optChainExpr -> failwith "todo"
      | Expr.Invalid invalid -> failwith "todo"

    let start = FParsec.Position("", 0, 1, 1)
    let stop = FParsec.Position("", 0, 1, 1)
    let span: Syntax.Span = { Start = start; Stop = stop }

    let expr: Syntax.Expr =
      { Syntax.Expr.Kind = kind
        Syntax.Expr.Span = span
        Syntax.InferredType = None }

    expr
