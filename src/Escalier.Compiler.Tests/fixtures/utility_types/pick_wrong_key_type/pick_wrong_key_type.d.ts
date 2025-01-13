type Pick = {[P in K]: T[P]};
type Foo = {
  a: number;
  b: string;
  c: boolean;
};
type Bar = Pick<Foo, 5 | 10>;
