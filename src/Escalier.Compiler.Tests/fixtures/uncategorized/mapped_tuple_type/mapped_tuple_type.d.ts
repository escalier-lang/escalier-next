type Foo<T> = {[K in keyof T]: T[K][];
type Bar = [string, number];
type Baz = Foo<Bar>;
