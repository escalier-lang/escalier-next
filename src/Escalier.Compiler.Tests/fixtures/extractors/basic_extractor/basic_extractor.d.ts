// @escType - {new fn (mut self: Self, value: string) -> C}
const C: {
  new (value: string): C;
};
// @escType - C
const subject: C;
// @escType - string
const msg: string;
