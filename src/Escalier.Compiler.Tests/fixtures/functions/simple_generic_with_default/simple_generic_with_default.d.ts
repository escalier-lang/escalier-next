function foo(bar: T): {
  value: T;
};
// @escType - {value: {b: 10}}
const x: {
  value: {
    b: 10;
  };
};
// @escType - {value: {a: 5}}
const y: {
  value: {
    a: 5;
  };
};
