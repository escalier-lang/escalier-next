type Obj = {
  a?: {
    b?: {
      c: number;
    };
  };
};
// @escType - Obj
const obj: Obj;
// @escType - {b?: {c: number}} | undefined
const a: {
  b?: {
    c: number;
  };
} | undefined;
// @escType - {c: number} | undefined
const b: {
  c: number;
} | undefined;
// @escType - number | undefined
const c: number | undefined;
type Point = {
  x: number;
  y: number;
};
// @escType - Point
const p: Point;
// @escType - number
const x: number;
