// @escType - {new fn (mut self: Self, first: string, second: number) -> C}
const C: {
  new (first: string, second: number): C;
};
// @escType - C
const subject: C;
// @escType - string
const x: string;
// @escType - number
const y: number;
