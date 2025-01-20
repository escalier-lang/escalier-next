// @escType - fn <T: {...}, U: T>(t: T, u: U) -> U
const foo: <T extends {
}, U extends T>(t: T, u: U) => U;
// @escType - {a: 5, b: "hello"}
const bar: {
  a: 5;
  b: "hello";
};
