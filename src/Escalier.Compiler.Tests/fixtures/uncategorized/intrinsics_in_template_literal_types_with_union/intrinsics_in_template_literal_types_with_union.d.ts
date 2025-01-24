type A = "foo" | "bar";
type B = `${Uppercase<A>}123`;
// @escType - B
const b1: B;
// @escType - B
const b2: B;
type C = `${Capitalize<A>}123`;
// @escType - C
const c1: C;
// @escType - C
const c2: C;
