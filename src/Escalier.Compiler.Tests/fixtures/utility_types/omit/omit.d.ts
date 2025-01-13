type Pick = {[P in K]: T[P]};
type Exclude = (T extends U ? never : T);
type AnyKey = string | number | symbol;
type Omit = Pick<T, Exclude<keyof T, K>>;
type Foo = {
  a: number;
  b: string;
  c: boolean;
};
type Bar = {
  a: number;
  c: boolean;
};
