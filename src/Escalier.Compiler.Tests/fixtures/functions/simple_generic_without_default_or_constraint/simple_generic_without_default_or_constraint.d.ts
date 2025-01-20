function foo(bar: T): {
  value: T;
};
// @escType - {value: unknown}
const x: {
  value: unknown;
};
// @escType - {value: {a: 5}}
const y: {
  value: {
    a: 5;
  };
};
