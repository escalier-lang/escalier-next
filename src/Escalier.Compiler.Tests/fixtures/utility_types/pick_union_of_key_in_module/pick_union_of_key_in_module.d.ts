type MyPick<T, K extends keyof T> = {[P in K]: T[P]};
type Foo = {
  a: number;
  b: string;
  c: boolean;
};
type Bar = {
  a: number;
  c: boolean;
};
