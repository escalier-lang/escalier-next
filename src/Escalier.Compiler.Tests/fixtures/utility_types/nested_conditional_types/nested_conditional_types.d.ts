type Foo = (T extends string | number ? (T extends string ? "string" : "number") : "other");
type A = "string";
type B = "number";
type C = "other";
