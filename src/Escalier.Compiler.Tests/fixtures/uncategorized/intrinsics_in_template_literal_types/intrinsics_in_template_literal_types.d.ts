type FooBar1 = `foo${Uppercase<string>}bar`;
// @escType - FooBar1
const foobar1: FooBar1;
type FooBar2 = `foo${Lowercase<string>}bar`;
// @escType - FooBar2
const foobar2: FooBar2;
type FooBar3 = `foo${Capitalize<string>}bar`;
// @escType - FooBar3
const foobar3: FooBar3;
