function foo(props: T & {
  x: number;
}): T;
// @escType - {y: "hello", z: true, ...}
const bar: {
  y: "hello";
  z: true;
};
