type Point = {
  x: number;
  y: number;
};
type Foo<T> = {
  bar: T | Array<Foo<T>>;
};
