class D {
  data: string
  constructor(temp8: string) 
  [Symbol.customMatcher]()
}
type Point = {
  x: number;
  y: number;
};
class E {
  data: Point
  constructor(temp9: Point) 
  [Symbol.customMatcher]()
}
class C {
  data1: D
  data2: E
  constructor(temp10: D, temp11: E) 
  [Symbol.customMatcher]()
}
// @escType - C
const subject: C;
// @escType - string
const msg: string;
// @escType - number
const x: number;
// @escType - number
const y: number;
