type Foo = number | Foo[];
// @escType - Foo
const foo: Foo;
// @escType - number | Foo[]
const bar: number | Foo[];
