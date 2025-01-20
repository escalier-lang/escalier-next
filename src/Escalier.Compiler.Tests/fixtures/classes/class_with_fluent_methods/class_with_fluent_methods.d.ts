// @escType - {new fn () -> Foo}
const Foo: {
  new (): Foo;
};
// @escType - Foo
const foo: Foo;
// @escType - Foo
const bar: Foo;
