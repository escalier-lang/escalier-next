// @escType - fn <A: number>(x: A) -> Promise<A, "RangeError">
const foo: <A extends number>(x: A) => Promise<A, "RangeError">;
// @escType - fn <A: number>(x: A) -> Promise<number, never>
const bar: <A extends number>(x: A) => Promise<number, never>;
