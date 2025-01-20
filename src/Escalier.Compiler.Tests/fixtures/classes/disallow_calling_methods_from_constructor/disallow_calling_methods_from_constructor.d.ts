// @escType - {new fn (mut self: Self, msg: string) -> Foo}
const Foo: {
  new (msg: string): Foo;
};
// @escType - Foo
const foo: Foo;
