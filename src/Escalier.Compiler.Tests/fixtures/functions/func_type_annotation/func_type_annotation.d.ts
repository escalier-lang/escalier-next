// @escType - fn <T: number>(x: T) -> T
const foo: <T extends number>(x: T) => T;
// @escType - 5
const x: 5;
// @escType - number
const y: number;
