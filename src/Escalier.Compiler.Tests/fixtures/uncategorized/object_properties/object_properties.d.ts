// @escType - {a: {b: 5, c: "hello"}}
const obj: {
  a: {
    b: 5;
    c: "hello";
  };
};
// @escType - 5
const b: 5;
// @escType - "hello"
const c: "hello";
