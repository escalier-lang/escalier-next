type A = Uppercase<string>;
// @escType - A
const a: A;
// @escType - Lowercase<string>
const b: Lowercase<string>;
// @escType - Capitalize<string>
const c: Capitalize<string>;
// @escType - Uncapitalize<string>
const d: Uncapitalize<string>;
