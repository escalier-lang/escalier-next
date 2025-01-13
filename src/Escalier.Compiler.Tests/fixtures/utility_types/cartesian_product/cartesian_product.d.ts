type CartesianProduct = (X extends unknown ? (Y extends unknown ? [X, Y] : never) : never);
type Cells = ["A", 1] | ["A", 2] | ["B", 1] | ["B", 2];
