// @escType - fn <A: number>(x: A) -> never throws ("BoundsError" | "RangeError")
const foo: <A extends number>(x: A) => never;
