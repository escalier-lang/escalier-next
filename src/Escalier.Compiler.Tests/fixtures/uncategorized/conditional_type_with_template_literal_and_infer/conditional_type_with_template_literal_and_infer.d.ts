type StripUnderscore<T> = (T extends `_${infer U}` ? U : T);
type Foo = StripUnderscore<"_foo">;
type Bar = StripUnderscore<"bar">;
