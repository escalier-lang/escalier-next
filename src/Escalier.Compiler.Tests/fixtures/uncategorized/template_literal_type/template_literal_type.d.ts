type Foo = `foo${number}`;
// @escType - Foo
const x: Foo;
type Bar = `A${string}B`;
// @escType - Bar
const y: Bar;
type Baz = `A${string}B${string}C`;
// @escType - Baz
const z: Baz;
// @escType - Baz
const w: Baz;
