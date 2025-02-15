class C {
  msg: string
  constructor(temp2: string) 
  [Symbol.customMatcher]()
}
// @escType - {foo: C}
const subject: {
  foo: C;
};
// @escType - string
const msg: string;
