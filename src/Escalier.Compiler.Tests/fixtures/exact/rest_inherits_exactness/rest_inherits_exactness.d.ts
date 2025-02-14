type Exact = {
  foo: string;
  bar: number;
};
type Inexact = {
  foo: string;
  bar: number;
};
// @escType - Exact
declare const exact: Exact;
// @escType - Inexact
declare const inexact: Inexact;
// @escType - {bar: number}
const exactRest: {
  bar: number;
};
// @escType - string
const foo: string;
// @escType - string
const foo: string;
// @escType - {bar: number, ...}
const inexactRest: {
  bar: number;
};
