type Point = {
  x: number;
  y: number;
};
type Line = {
  p0: Point;
  p1: Point;
};
// @escType - Point
const p0: Point;
// @escType - Point
const p1: Point;
// @escType - Line
const line: Line;
// @escType - Point
const end: Point;
// @escType - Point
const start: Point;
