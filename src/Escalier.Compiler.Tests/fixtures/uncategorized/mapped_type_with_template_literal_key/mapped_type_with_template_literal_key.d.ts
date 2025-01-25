type Foo<T> = {[K in keyof T]: T[K];
type Point = {
  x: number;
  y: number;
};
type Bar = Foo<Point>;
