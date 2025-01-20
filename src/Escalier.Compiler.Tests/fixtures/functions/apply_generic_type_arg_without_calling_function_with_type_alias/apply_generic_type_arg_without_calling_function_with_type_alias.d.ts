type Identity = <A>(x: A) => A;
// @escType - Identity
declare const foo: Identity;
// @escType - fn (x: number) -> number
const bar: (x: number) => number;
