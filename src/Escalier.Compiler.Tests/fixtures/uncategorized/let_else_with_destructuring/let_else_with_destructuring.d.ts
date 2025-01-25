type Point = {
  x: number;
  y: number;
};
type Line = {
  start: Point;
  end: Point;
};
// @escType - Point | Line | undefined
declare const value: Point | Line | undefined;
// @escType - fn (msg: string) -> undefined
declare const print: (msg: string) => undefined;
// @escType - number
const sum: number;
