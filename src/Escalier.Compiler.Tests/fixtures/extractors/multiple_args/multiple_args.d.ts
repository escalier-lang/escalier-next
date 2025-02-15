class C {
  first: string
  second: number
  constructor(temp2: string, temp3: number) 
  [Symbol.customMatcher]()
}
// @escType - C
const subject: C;
// @escType - string
const x: string;
// @escType - number
const y: number;
