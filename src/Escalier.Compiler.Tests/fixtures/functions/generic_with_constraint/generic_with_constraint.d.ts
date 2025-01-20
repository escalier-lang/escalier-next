// @escType - fn <T: {...}>(obj: T) -> T
const foo: <T extends {
}>(obj: T) => T;
// @escType - {a: 5, b: "hello"}
const bar: {
  a: 5;
  b: "hello";
};
