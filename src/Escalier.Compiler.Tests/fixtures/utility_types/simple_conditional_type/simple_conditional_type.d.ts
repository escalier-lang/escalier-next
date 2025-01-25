type Foo<T> = (T extends string ? "string" : (T extends number ? "number" : "other"));
// expansion - "string"
type A = Foo<string>;
// expansion - "number"
type B = Foo<number>;
// expansion - "other"
type C = Foo<boolean>;
