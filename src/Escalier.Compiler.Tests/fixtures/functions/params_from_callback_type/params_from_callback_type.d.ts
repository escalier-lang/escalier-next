type Point = {
  x: number;
  y: number;
};
// @escType - fn (cb: fn (p: Point) -> number) -> number
declare const foo: (cb: (p: Point) => number) => number;
// @escType - number
const result: number;
