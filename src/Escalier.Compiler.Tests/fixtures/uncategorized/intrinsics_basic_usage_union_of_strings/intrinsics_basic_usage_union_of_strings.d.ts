type A = "HeLlo" | "wOrLd";
type B = Uppercase<A>;
// @escType - B
const x: B;
// @escType - B
const y: B;
