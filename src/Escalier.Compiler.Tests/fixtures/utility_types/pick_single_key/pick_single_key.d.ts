type Pick<T, K extends keyof T> = {[P in K]: T[P];
type Foo = {
  a: number;
  b: string;
  c: boolean;
};
// expansion - {a: number, c: boolean}
type Bar = Pick<Foo, "a" | "c">;
