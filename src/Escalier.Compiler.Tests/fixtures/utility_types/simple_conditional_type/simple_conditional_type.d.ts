type Foo = (T extends string ? "string" : (T extends number ? "number" : "other"));
type A = "string";
type B = "number";
type C = "other";
