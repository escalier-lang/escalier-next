type MyNode<T> = {
  value: T;
  left?: MyNode<T>;
  right?: MyNode<T>;
};
// @escType - fn () -> MyNode<number>
const makeTree: () => MyNode<number>;
// @escType - MyNode<number>
const node: MyNode<number>;
// @escType - number | undefined
const x: number | undefined;
