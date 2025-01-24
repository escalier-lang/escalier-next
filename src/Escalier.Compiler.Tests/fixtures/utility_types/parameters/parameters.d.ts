type Parameters<T> = (T extends (...args: infer P) => any ? P : never);
// expansion - []
type Foo = Parameters<() => number>;
// expansion - [string, boolean]
type Bar = Parameters<(a: string, b: boolean) => number>;
