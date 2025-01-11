const Foo: {
  new <T>(): Foo<T>;
};
const foo: Foo<string>;
const bar: Foo<string>;
