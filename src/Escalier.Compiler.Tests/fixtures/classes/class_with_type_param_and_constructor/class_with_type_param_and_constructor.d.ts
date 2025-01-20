// @escType - {new fn <T>(mut self: Self, msg: T) -> Foo<T>}
const Foo: {
  new <T>(msg: T): Foo<T>;
};
// @escType - Foo<string>
const foo: Foo<string>;
