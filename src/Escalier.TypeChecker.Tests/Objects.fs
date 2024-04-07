module Objects

open FsToolkit.ErrorHandling
open Xunit

open TestUtils

[<Fact>]
let SimpleObjectType () =
  let result =
    result {
      let src =
        """
        let p = {x: 5, y: 10};
        """

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Diagnostics)

      Assert.Value(env, "p", "{x: 5, y: 10}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let ObjectWithComputeProperties () =
  let result =
    result {
      let src =
        """
        let KEYS = {
            foo: "foo",
            bar: "bar",
        };
        let obj = {
            [KEYS.foo]: 5,
            [KEYS.bar]: "hello",
        };
        """

      // when KEYS.foo is used as a property but it still hasn't been inferred
      // yet we can constrain its type to string | number | symbol.

      let! ctx, env = inferModule src

      Assert.Empty(ctx.Diagnostics)

      Assert.Value(env, "obj", "")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)
