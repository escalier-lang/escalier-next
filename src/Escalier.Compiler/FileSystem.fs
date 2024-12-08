module Escalier.Compiler.FileSystem

open System.IO

type IFileSystem =
  abstract member ReadAllTextAsync: string -> Async<string>
  abstract member WriteAllTextAsync: string * string -> Async<unit>
  abstract member FileExistsAsync: string -> Async<bool>

  abstract member DirExistsAsync: string -> Async<bool>
  abstract member GetParentAsync: string -> Async<string>

let makeFileSystem () : IFileSystem =
  { new IFileSystem with
      member self.ReadAllTextAsync path =
        async {
          let! text = File.ReadAllTextAsync path |> Async.AwaitTask
          return text
        }

      member self.WriteAllTextAsync(path, text) =
        async { do! File.WriteAllTextAsync(path, text) |> Async.AwaitTask }

      member self.FileExistsAsync path = async { return File.Exists path }

      member self.DirExistsAsync path = async { return Directory.Exists path }

      member self.GetParentAsync path =
        async {
          let dir = Directory.GetParent path

          match dir with
          | null -> return failwith "No parent directory"
          | _ -> return dir.FullName
        }

  }
