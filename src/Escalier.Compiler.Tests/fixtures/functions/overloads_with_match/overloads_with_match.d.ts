// @escType - fn (a: number, b: number) -> number & fn (a: string, b: string) -> string
declare const add: (a: number, b: number) => number & (a: string, b: string) => string;
// @escType - number
const num: number;
// @escType - string
const str: string;
