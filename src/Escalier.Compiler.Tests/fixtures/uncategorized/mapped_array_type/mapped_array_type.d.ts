type Foo<T> = {[K in keyof T]: T[K][];
type Bar = string[];
type Baz = Foo<Bar>;
