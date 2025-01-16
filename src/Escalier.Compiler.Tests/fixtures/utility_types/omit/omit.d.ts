type Pick<T, K extends keyof T> = {[P in K]: T[P]};
type Exclude<T, U> = (T extends U ? never : T);
type AnyKey = string | number | symbol;
type Omit<T, K extends AnyKey> = Pick<T, Exclude<keyof T, K>>;
type Foo = {
  a: number;
  b: string;
  c: boolean;
};
type Bar = {
  a: number;
  c: boolean;
};
