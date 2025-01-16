type Foo<T> = (T extends string | number ? (T extends string ? "string" : "number") : "other");
type A = "string";
type B = "number";
type C = "other";
