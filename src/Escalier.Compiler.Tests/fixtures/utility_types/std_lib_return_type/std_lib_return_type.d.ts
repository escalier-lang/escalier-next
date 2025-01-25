// expansion - number
type Foo = ReturnType<() => number>;
// expansion - number
type Bar = ReturnType<(a: string, b: boolean) => number>;
