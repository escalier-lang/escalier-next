function foo(bar: T): {
  value: T;
};
// @escType - {value: {...}}
const x: {
  value: {
  };
};
// @escType - {value: {a: 5}}
const y: {
  value: {
    a: 5;
  };
};
