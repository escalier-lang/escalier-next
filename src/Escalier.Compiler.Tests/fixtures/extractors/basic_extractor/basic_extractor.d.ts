class C {
  msg: string
  constructor(temp1: string) 
  [Symbol.customMatcher]()
}
// @escType - C
const subject: C;
// @escType - string
const msg: string;
