type Foo = {[P in "a" | "b"]: number;
type Bar = {[P in "b" | "c"]+?: string;
type FooBar = Foo & Bar;
