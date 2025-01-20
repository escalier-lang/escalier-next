// @escType - fn <A: number>(x: A) -> A throws "RangeError"
const foo: <A extends number>(x: A) => A;
// @escType - fn <A: number>(x: A) -> A throws "RangeError"
const bar: <A extends number>(x: A) => A;
