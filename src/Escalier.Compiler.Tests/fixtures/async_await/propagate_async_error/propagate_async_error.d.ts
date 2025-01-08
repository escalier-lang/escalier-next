const foo: <A extends number>(x: A) => Promise<A, "RangeError">;
const bar: <A extends number>(x: A) => Promise<number, "RangeError">;
