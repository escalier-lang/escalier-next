type AnyKey = string | number | symbol;
type Record<K extends AnyKey, T> = {[P in K]: T;
// expansion - {x: number, y: number}
type Point = Record<"x" | "y", number>;
// @escType - Point
const p: Point;
