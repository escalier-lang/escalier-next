type A = Uppercase<"HeLlo">;
// @escType - A
const a: A;
// @escType - Lowercase<"HeLlo">
const b: Lowercase<"HeLlo">;
// @escType - Capitalize<"HeLlo">
const c: Capitalize<"HeLlo">;
// @escType - Uncapitalize<"HeLlo">
const d: Uncapitalize<"HeLlo">;
