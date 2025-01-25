type Exclude<T, U> = (T extends U ? never : T);
// expansion - "b" | "c" | "d"
type Result = Exclude<"a" | "b" | "c" | "d" | "e", "a" | "e">;
