type MyNode<T> = {
  value: T;
  left?: MyNode<T>;
  right?: MyNode<T>;
};
// @escType - MyNode<number>
const node: MyNode<number>;
