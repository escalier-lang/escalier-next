// @escType - {new fn (mut self: Self, x: number, y: number) -> Point, makePoint fn (x: number, y: number) -> Self}
const Point: {
  new (x: number, y: number): Point;
  makePoint(x: number, y: number): Self;
};
// @escType - Point
const p1: Point;
// @escType - Point
const p2: Point;
// @escType - Point
const p3: Point;
