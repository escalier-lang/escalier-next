type Foo<T> = {[K in keyof T]: Array<T[K]>;
type Bar = [string, number];
type Baz = Foo<Bar>;
