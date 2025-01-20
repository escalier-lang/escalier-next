type Obj = {
};
// @escType - fn <T: Obj, U: T>(t: T, u: U) -> U
const foo: <T extends Obj, U extends T>(t: T, u: U) => U;
// @escType - {a: 5, b: "hello"}
const bar: {
  a: 5;
  b: "hello";
};
