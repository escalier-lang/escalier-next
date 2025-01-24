type CartesianProduct<X, Y> = (X extends unknown ? (Y extends unknown ? [X, Y] : never) : never);
// expansion - ["A", 1] | ["A", 2] | ["B", 1] | ["B", 2]
type Cells = CartesianProduct<"A" | "B", 1 | 2>;
