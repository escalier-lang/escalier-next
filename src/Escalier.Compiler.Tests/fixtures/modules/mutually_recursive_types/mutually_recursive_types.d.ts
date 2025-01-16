type Foo<T> = {
  bar: Bar<T>;
};
type Bar<T> = {
  foo: Foo<T>;
};
