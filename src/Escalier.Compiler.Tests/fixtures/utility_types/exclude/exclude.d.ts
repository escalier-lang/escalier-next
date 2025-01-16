type Exclude<T, U> = (T extends U ? never : T);
type Result = "b" | "c" | "d";
