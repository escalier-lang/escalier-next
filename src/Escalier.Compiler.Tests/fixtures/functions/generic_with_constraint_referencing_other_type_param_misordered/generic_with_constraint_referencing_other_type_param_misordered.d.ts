// @escType - fn <U: T, T: {...}>(t: T, u: U) -> U
const foo: <U extends T, T extends {
}>(t: T, u: U) => U;
// @escType - {a: 5, b: "hello"}
const bar: {
  a: 5;
  b: "hello";
};
