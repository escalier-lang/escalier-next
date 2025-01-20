// @escType - fn <A: number>(x: A) -> Promise<A, "RangeError">
const foo: <A extends number>(x: A) => Promise<A, "RangeError">;
