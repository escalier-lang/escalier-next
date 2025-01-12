type Point = {
  x: number;
  y: number;
};
declare const foo: (cb: (p: Point) => number) => number;
const result: number;
