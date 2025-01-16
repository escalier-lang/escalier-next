type MyPartial<T> = {[K in keyof T]+?: T[K]};
type Exact = {
  msg: string;
  flag: boolean;
};
type Inexact = {
  msg: string;
  flag: boolean;
};
type PartialExact = MyPartial<Exact>;
type PartialInexact = MyPartial<Inexact>;
