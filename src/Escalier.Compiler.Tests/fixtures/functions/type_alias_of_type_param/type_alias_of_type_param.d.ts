// @escType - fn <A>(x: A) -> B
const foo: <A>(x: A) => B;
// @escType - fn <A>(x: A) -> A
const bar: <A>(x: A) => A;
// @escType - B
const z: B;
