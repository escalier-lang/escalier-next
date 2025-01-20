// @escType - fn <T: number>(x: T) -> T
const foo: <T extends number>(x: T) => T;
// @escType - number
const bar: number;
