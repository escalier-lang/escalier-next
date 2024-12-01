namespace Escalier.TypeChecker

open FsToolkit.ErrorHandling

open Escalier.Data
open Escalier.Data.Common
open Escalier.Data.Type

open Error
open Prune
open Env
open Mutability
open Poly
open Unify

module rec UnifyCall =
  let unifyCall
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (args: list<Syntax.Expr>)
    (typeArgs: option<list<Type>>)
    (callee: Type)
    : Result<Type * Type, TypeError> =

    result {
      let callee = prune callee

      match callee.Kind with
      | TypeKind.Function func ->
        return! unifyFuncCall ctx env ips args typeArgs func
      | TypeKind.TypeRef { Name = name; Scheme = scheme } ->
        let callee =
          match scheme with
          | Some scheme ->
            // TODO: handle typeArgs
            scheme.Type
          | None ->
            match env.GetScheme(name) with
            | Result.Ok scheme ->
              // TODO: handle typeArgs
              scheme.Type
            | Result.Error _ -> failwith "'{name}' is not in scope"

        return! unifyCall ctx env ips args typeArgs callee
      | TypeKind.Object { Elems = elems } ->
        let mutable callable = None

        for elem in elems do
          match elem with
          | Callable c ->
            callable <-
              Some
                { Kind = TypeKind.Function c
                  Provenance = None }
          | _ -> ()

        match callable with
        | Some c -> return! unifyCall ctx env ips args typeArgs c
        | None ->
          return!
            Error(TypeError.SemanticError $"No callable signature in {callee}")
      | TypeKind.Intersection types ->
        let mutable result = None

        // TODO: handle an intersection of intersections

        let mutable reports = []
        let mutable retTypes = []
        let mutable throwTypes = []

        for t in types do
          if result.IsNone then
            ctx.PushReport()

            match unifyCall ctx env ips args typeArgs t with
            | Result.Ok(retType, throwType) ->
              retTypes <- retType :: retTypes
              throwTypes <- throwType :: throwTypes

              if ctx.Report.Diagnostics.IsEmpty then
                result <- Some(retType, throwType)
            | Result.Error _ -> ()

            reports <- ctx.Report :: reports
            ctx.PopReport()

        match result with
        | Some(value) -> return value
        | None ->
          let retType =
            { Kind = TypeKind.Intersection(List.rev retTypes)
              Provenance = None }

          let throwType =
            { Kind = TypeKind.Intersection(List.rev throwTypes)
              Provenance = None }

          // TODO: come up with a better way of merging diagnostics
          for report in reports do
            ctx.Report.Diagnostics <-
              ctx.Report.Diagnostics @ report.Diagnostics

          return retType, throwType
      | TypeKind.TypeVar _ ->

        // TODO: use a `result {}` CE here
        let! argTypes = List.traverseResultM (ctx.InferExpr ctx env None) args

        let paramList =
          List.mapi
            (fun i t ->
              let name = $"arg{i}"

              let p: Pattern =
                Pattern.Identifier { Name = name; IsMut = false }

              { Pattern = p
                Type = t
                Optional = false })
            argTypes

        let retType = ctx.FreshTypeVar None None
        let throwsType = ctx.FreshTypeVar None None

        let fn =
          { ParamList = paramList
            Self = None // TODO: pass in the receiver if this is a method call
            Return = retType
            Throws = throwsType
            TypeParams = None } // TODO

        let callType =
          { Type.Kind = TypeKind.Function fn
            Provenance = None }

        match bind ctx env ips callee callType with
        | Ok _ -> return (prune retType, prune throwsType)
        | Error e -> return! Error e
      | kind ->
        printfn $"callee = {callee}"
        return! Error(TypeError.NotImplemented $"kind = {kind}")
    }

  // Returns a Result with 2-tuple containing the return and throws types.
  let unifyFuncCall
    (ctx: Ctx)
    (env: Env)
    (ips: option<list<list<string>>>)
    (args: list<Syntax.Expr>)
    (typeArgs: option<list<Type>>)
    (callee: Function)
    : Result<Type * Type, TypeError> =

    result {
      let! callee =
        result {
          if callee.TypeParams.IsSome then
            return! instantiateFunc ctx callee typeArgs
          else
            return callee
        }

      // TODO: require the optional params come after the required params
      // TODO: require that if there is a rest param, it comes last
      let optionalParams, requiredParams =
        callee.ParamList
        |> List.partition (fun p ->
          match p.Pattern with
          | Pattern.Rest _ -> true
          | _ -> p.Optional)

      if args.Length < requiredParams.Length then
        // TODO: make this into a diagnostic instead of an error
        return!
          Error(
            TypeError.SemanticError "function called with too few arguments"
          )

      let requiredArgs, optionalArgs = List.splitAt requiredParams.Length args

      for arg, param in List.zip requiredArgs requiredParams do
        let! invariantPaths =
          checkMutability
            (getTypePatBindingPaths param.Pattern)
            (getExprBindingPaths env arg)

        let! argType = ctx.InferExpr ctx env (Some param.Type) arg

        if
          param.Optional && argType.Kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          match unify ctx env invariantPaths argType param.Type with
          | Ok _ -> ()
          | Error reason ->
            // QUESTION: Does unifying the param with `never` actually do
            // anything or could we skip it?  Does this have to do with
            // params whose type annotations are or include type params?
            let never =
              { Kind = TypeKind.Keyword Keyword.Never
                Provenance = None }

            do! unify ctx env ips never param.Type

            ctx.Report.AddDiagnostic(
              { Description =
                  $"arg type '{argType}' doesn't satisfy param '{param.Pattern}' type '{param.Type}' in function call"
                Reasons = [ reason ] }
            )

      let optionalParams, restParams =
        optionalParams
        |> List.partition (fun p ->
          match p.Pattern with
          | Pattern.Rest _ -> false
          | _ -> true)

      let! restParam =
        match restParams with
        | [] -> Result.Ok None
        | [ restParam ] -> Result.Ok(Some(restParam))
        | _ -> Error(TypeError.SemanticError "Too many rest params!")

      let restArgs =
        match restParam with
        | None -> None
        | Some _ ->
          if optionalArgs.Length > optionalParams.Length then
            Some(List.skip optionalParams.Length optionalArgs)
          else
            Some []

      // Functions can be passed more args than parameters as well as
      // fewer args that the number optional params.  We handle both
      // cases here.
      let minLength = min optionalArgs.Length optionalParams.Length
      let optionalParams = List.take minLength optionalParams
      let optionalArgs = List.take minLength optionalArgs

      let mutable reasons: list<TypeError> = []

      for arg, param in List.zip optionalArgs optionalParams do
        let! argType = ctx.InferExpr ctx env None arg

        if
          param.Optional && argType.Kind = TypeKind.Literal(Literal.Undefined)
        then
          ()
        else
          let! invariantPaths =
            checkMutability
              (getTypePatBindingPaths param.Pattern)
              (getExprBindingPaths env arg)

          match unify ctx env invariantPaths argType param.Type with
          | Ok _ -> ()
          | Error(reason) -> reasons <- reason :: reasons

      match restArgs, restParam with
      | Some args, Some param ->
        let! args = List.traverseResultM (ctx.InferExpr ctx env None) args

        let tuple =
          { Kind = TypeKind.Tuple { Elems = args; Immutable = false }
            Provenance = None }

        // TODO: check the result type and add a `reason` to `reasons` if there's
        // a type error
        do! unify ctx env ips tuple param.Type
      | _ -> ()

      if not reasons.IsEmpty then
        let diagnostic =
          { Description = "Calling function with incorrect args"
            Reasons = List.rev reasons }

        ctx.Report.AddDiagnostic diagnostic

      return (callee.Return, callee.Throws)
    }
