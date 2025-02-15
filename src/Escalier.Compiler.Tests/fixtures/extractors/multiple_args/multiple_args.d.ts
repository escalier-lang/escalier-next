class C {
  first: string
  second: number
  constructor(temp4: string, temp5: number) 
  [Symbol.customMatcher]()
}
// @escType - C
const subject: C;
// @escType - string
const x: string;
// @escType - number
const y: number;
