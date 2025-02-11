type Point = {
  x: number;
  y: number;
};
// @escType - {new fn (mut self: Self, data: Point) -> C}
const C: {
  new (data: Point): C;
};
// @escType - C
const subject: C;
// @escType - number
const x: number;
// @escType - number
const y: number;
