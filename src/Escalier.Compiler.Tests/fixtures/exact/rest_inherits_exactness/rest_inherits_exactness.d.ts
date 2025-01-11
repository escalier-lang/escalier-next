type Exact = {
  foo: string;
  bar: number;
};
type Inexact = {
  foo: string;
  bar: number;
};
declare const exact: Exact;
declare const inexact: Inexact;
const exactRest: {
  bar: number;
};
const inexactRest: {
  bar: number;
};
