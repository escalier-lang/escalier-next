type Point = {
  x: number;
  y: number;
};
type Extract<T, U> = (T extends Point ? T : never);
type Result = {
  x: 5;
  y: 10;
};
