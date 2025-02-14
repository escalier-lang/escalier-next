// @escType - {a: 5, b: "hello", c: true}
const obj1: {
  a: 5;
  b: "hello";
  c: true;
};
// @escType - 5
const a: 5;
// @escType - {b: "hello", c: true}
const rest: {
  b: "hello";
  c: true;
};
