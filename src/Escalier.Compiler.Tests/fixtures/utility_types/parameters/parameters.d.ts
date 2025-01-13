type Parameters = (T extends (...args: infer P) => any ? P : never);
type Foo = [];
type Bar = [string, boolean];
