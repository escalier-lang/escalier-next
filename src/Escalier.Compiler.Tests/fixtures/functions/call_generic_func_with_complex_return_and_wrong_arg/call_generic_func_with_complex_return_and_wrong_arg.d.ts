// @escType - fn <T: number>(x: T) -> {value: T}
const foo: <T extends number>(x: T) => {
  value: T;
};
// @escType - {value: number}
const bar: {
  value: number;
};
