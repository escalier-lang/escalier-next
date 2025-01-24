type Kebab<T extends string, U extends string> = `-${T}-${U}-`;
type Foo = Kebab<"hello", "world">;
type Bar = Kebab<string, number>;
