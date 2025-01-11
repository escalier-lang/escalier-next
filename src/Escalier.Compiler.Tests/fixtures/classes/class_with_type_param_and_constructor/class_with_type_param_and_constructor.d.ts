const Foo: {
  new <T>(msg: T): Foo<T>;
};
const foo: Foo<string>;
