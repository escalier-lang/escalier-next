// @escType - fn <A: number>(x: A) -> never throws ("BoundsError" | "RangeError")
const foo: <A extends number>(x: A) => never;
// @escType - fn <A: number>(x: A) -> 0 throws "BoundsError"
const bar: <A extends number>(x: A) => 0;
