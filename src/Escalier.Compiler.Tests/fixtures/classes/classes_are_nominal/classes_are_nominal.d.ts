class Foo {
  constructor() 
}
class Bar {
  constructor() 
}
// @escType - Foo
const foo: Foo;
// @escType - Bar
const bar: Bar;
type Qux = Foo;
// @escType - Qux
const valid: Qux;
// @escType - Qux
const invalid: Qux;
