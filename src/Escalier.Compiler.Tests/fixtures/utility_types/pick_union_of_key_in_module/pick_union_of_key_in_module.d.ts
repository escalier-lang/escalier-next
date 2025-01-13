type MyPick = {[P in K]: T[P]};
type Foo = {
  a: number;
  b: string;
  c: boolean;
};
type Bar = {
  a: number;
  c: boolean;
};
