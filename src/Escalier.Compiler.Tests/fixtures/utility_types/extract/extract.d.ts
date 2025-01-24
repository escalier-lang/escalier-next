type Point = {
  x: number;
  y: number;
};
type Extract<T, U> = (T extends Point ? T : never);
// expansion - {x: 5, y: 10}
type Result = Extract<{
  x: 5;
  y: 10;
} | number | string, Point>;
