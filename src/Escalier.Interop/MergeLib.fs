namespace Escalier.Interop

open FParsec.Error
open FsToolkit.ErrorHandling
open FSharp.Data
open System.IO

open Escalier.Data.Syntax

module MergeLib =
  let mutable memoizedNodeModulesDir: Map<string, string> = Map.empty

  let rec findNearestAncestorWithNodeModules (currentDir: string) =
    match memoizedNodeModulesDir.TryFind currentDir with
    | Some(nodeModulesDir) -> nodeModulesDir
    | None ->
      let nodeModulesDir = Path.Combine(currentDir, "node_modules")

      if Directory.Exists(nodeModulesDir) then
        currentDir
      else
        let parentDir = Directory.GetParent(currentDir)

        match parentDir with
        | null ->
          failwith "node_modules directory not found in any ancestor directory."
        | _ -> findNearestAncestorWithNodeModules parentDir.FullName

  let private packageJsonHasTypes (packageJsonPath: string) : bool =
    if File.Exists packageJsonPath then
      let packageJson = File.ReadAllText(packageJsonPath)
      let packageJsonObj = JsonValue.Parse(packageJson)

      match packageJsonObj.TryGetProperty("types") with
      | None -> false
      | Some _ -> true
    else
      false

  let resolvePath
    (projectRoot: string)
    (currentPath: string)
    (importPath: string)
    : string =
    if importPath.StartsWith "~" then
      Path.GetFullPath(Path.Join(projectRoot, importPath.Substring(1)))
    else if importPath.StartsWith "." then
      let resolvedPath =
        Path.GetFullPath(
          Path.Join(Path.GetDirectoryName(currentPath), importPath)
        )

      if currentPath.EndsWith(".d.ts") then
        Path.ChangeExtension(resolvedPath, ".d.ts")
      else
        resolvedPath
    else
      // TODO: once this is implemented, move it over to Escalier.Interop
      // TODO: check if there's `/` in the import path, if so, the first
      // part before the `/` is the name of the module and the rost is a
      // path to a .d.ts file within the module.

      // determine if importPath contains a '/' and split on it
      // if it does, the first part is the module name and the second part is
      // the path to the .d.ts file within the module

      // It's possible that the module name is a scoped package, in which case
      // the module name will be the first part of the second part of the split
      // and the second part will be the path to the .d.ts file within the module.
      let moduleName, subpath =
        match importPath.Split('/') |> List.ofArray with
        | [] -> failwith "This should never happen."
        | [ name ] -> name, None
        | name :: path ->
          if name.StartsWith("@") then
            let ns = name

            match path with
            | [] -> failwith "This should never happen."
            | [ name ] ->
              let moduleName = String.concat "/" [ ns; name ]
              moduleName, None
            | name :: path ->
              let moduleName = String.concat "/" [ ns; name ]
              moduleName, Some(String.concat "/" path)
          else
            name, Some(String.concat "/" path)

      let rootDir = findNearestAncestorWithNodeModules projectRoot
      let nodeModulesDir = Path.Combine(rootDir, "node_modules")

      let pkgJsonPath1 =
        Path.Combine(nodeModulesDir, moduleName, "package.json")

      let pkgJsonPath2 =
        Path.Combine(nodeModulesDir, "@types", moduleName, "package.json")

      let pkgJsonPath =
        if packageJsonHasTypes pkgJsonPath1 then
          pkgJsonPath1
        elif packageJsonHasTypes pkgJsonPath2 then
          pkgJsonPath2
        else
          failwith
            $"package.json not found for module {moduleName}, rootDir = {rootDir}, nodeModulesDir = {nodeModulesDir}."

      // read package.json and parse it
      let pkgJson = File.ReadAllText(pkgJsonPath)
      let pkgJsonObj = JsonValue.Parse(pkgJson)

      let combinedPath =
        match subpath with
        | None ->
          match pkgJsonObj.TryGetProperty("types") with
          | None -> failwith "Invalid package.json: missing `types` field."
          | Some value ->
            let types = value.InnerText()

            if types.EndsWith(".d.ts") then
              Path.Combine(Path.GetDirectoryName(pkgJsonPath), types)
            else
              Path.Combine(Path.GetDirectoryName(pkgJsonPath), $"{types}.d.ts")
        // Path.Combine(Path.GetDirectoryName(pkgJsonPath), types)
        | Some value ->
          Path.Combine(Path.GetDirectoryName(pkgJsonPath), $"{value}.d.ts")

      let di = DirectoryInfo(combinedPath)
      di.FullName

  let getDependencies
    (projectRoot: string)
    (importSrc: string)
    : Result<list<string>, ParserError> =
    let mutable visitedPaths: list<string> = []

    let rec traverse (currentPath: string) (importSrc: string) =
      result {
        if not (importSrc.StartsWith ".") && visitedPaths.Length <> 0 then
          return ()
        else
          let resolvedPath = resolvePath projectRoot currentPath importSrc
          printfn $"resolvedPath = {resolvedPath}"

          if List.contains resolvedPath visitedPaths then
            return ()
          else
            visitedPaths <- resolvedPath :: visitedPaths

            let contents = File.ReadAllText(resolvedPath)

            let! ast =
              match Parser.parseModule contents with
              | FParsec.CharParsers.Success(value, _, _) -> Result.Ok(value)
              | FParsec.CharParsers.Failure(_, parserError, _) ->
                printfn $"parserError = {parserError}"
                Result.Error(parserError)

            let ast = Migrate.migrateModule ast

            for item in ast.Items do
              match item with
              | Import { Path = path } -> do! traverse resolvedPath path
              | Export(Export.NamedExport { Src = src }) ->
                do! traverse resolvedPath src
              | Export(Export.ExportAll { Src = src }) ->
                do! traverse resolvedPath src
              | _ -> ()

        return ()
      }

    match traverse projectRoot importSrc with
    | Ok _ -> Ok(visitedPaths)
    | Error e -> Error(e)
