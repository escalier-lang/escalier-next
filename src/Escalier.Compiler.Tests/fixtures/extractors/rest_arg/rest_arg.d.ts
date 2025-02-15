class C {
  first: string
  second: number
  third: boolean
  constructor(temp5: string, temp6: number, temp7: boolean) 
  [Symbol.customMatcher]()
}
// @escType - C
const subject: C;
// @escType - string
const x: string;
// @escType - [number, boolean]
const y: [number, boolean];
