type Foo<T> = {[K in keyof T]: Array<T[K]>;
type Bar = Array<string>;
type Baz = Foo<Bar>;
