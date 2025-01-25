type Point = {
  x: number;
  y: number;
};
// @escType - Point
const p: Point;
// @escType - fn ({x, y}: Point) -> number
const foo: ({x, y}: Point) => number;
// @escType - number
const sum: number;
