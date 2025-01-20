type Point = {
  x: number;
  y: number;
};
type Shape = {
  type: "circle";
  radius: number;
  center: Point;
} | {
  type: "line";
  start: Point;
  end: Point;
};
// @escType - Shape
declare const shape: Shape;
// @escType - Point | {x: number, y: number}
const centroid: Point | {
  x: number;
  y: number;
};
