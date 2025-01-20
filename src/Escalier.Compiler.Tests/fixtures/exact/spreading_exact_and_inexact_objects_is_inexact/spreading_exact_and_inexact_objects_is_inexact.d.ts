// @escType - {msg: string}
declare const foo: {
  msg: string;
};
// @escType - {flag: boolean, ...}
declare const bar: {
  flag: boolean;
};
// @escType - {msg: string, flag: boolean, ...}
const foobar: {
  msg: string;
  flag: boolean;
};
