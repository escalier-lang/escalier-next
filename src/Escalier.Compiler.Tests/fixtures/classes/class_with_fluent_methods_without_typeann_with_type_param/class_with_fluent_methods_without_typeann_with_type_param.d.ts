// @escType - {new fn <T>() -> Foo<T>}
const Foo: {
  new <T>(): Foo<T>;
};
// @escType - Foo<string>
const foo: Foo<string>;
// @escType - Foo<string>
const bar: Foo<string>;
