type Point<T> = {
  x: T;
  y: T;
};
type Line<T> = {
  p: Point<T>;
  q: Point<T>;
};
// @escType - Line<number>
declare const line: Line<number>;
