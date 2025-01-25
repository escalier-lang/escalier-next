type MyReturnType<T> = (T extends (...args: any) => infer R ? R : never);
// expansion - number
type Foo = MyReturnType<() => number>;
// expansion - number
type Bar = MyReturnType<(a: string, b: boolean) => number>;
