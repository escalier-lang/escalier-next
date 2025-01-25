type Foo = number | Foo[];
// @escType - Foo
const x: Foo;
// @escType - Foo
const y: Foo;
// @escType - Foo
const z: Foo;
