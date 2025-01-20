type AnyKey = string | number | symbol;
type Record<K extends AnyKey, T> = {[P in K]: T;
type Point = {
  x: number;
  y: number;
};
// @escType - Point
const p: Point;
