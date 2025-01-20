// @escType - fn <A: number>(x: A, y: string) -> string | A
const foo: <A extends number>(x: A, y: string) => string | A;
// @escType - string | 5
const bar: string | 5;
