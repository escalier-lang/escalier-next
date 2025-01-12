type Obj = {

};
const foo: <T extends Obj, U extends T>(t: T, u: U) => U;
const bar: {
  a: 5;
  b: "hello";
};
