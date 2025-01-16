type MyReturnType<T> = (T extends (...args: any) => infer R ? R : never);
type Foo = number;
type Bar = number;
