type Point = {
  x: number;
  y: number;
};
type Foo<T> = {
  bar: T | Foo<T>[];
};
