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
declare const shape: Shape;
const centroid: Point | {
  x: number;
  y: number;
};
