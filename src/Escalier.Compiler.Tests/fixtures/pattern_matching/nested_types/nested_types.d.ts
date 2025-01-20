// @escType - {a: [number] | {x: number}} | {b: [number] | {y: number}}
declare const value: {
  a: [number] | {
    x: number;
  };
} | {
  b: [number] | {
    y: number;
  };
};
// @escType - number
const result: number;
