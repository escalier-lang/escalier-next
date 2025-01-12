type Foo = {
  bar: Bar<T>;
};
type Bar = {
  foo: Foo<T>;
};
