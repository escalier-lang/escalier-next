type Point = {
  x: number;
  y: number;
};
class C {
  data: Point
  constructor(temp1: Point) 
  [Symbol.customMatcher]()
}
// @escType - C
const subject: C;
// @escType - number
const x: number;
// @escType - number
const y: number;
