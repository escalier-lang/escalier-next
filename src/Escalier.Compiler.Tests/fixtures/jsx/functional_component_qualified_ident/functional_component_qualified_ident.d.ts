import * as React from "react"
type Props = {
  message: string;
};
// @escType - {Comp: fn (props: Props) -> React.ReactNode}
const Comps: {
  Comp: (props: Props) => React.ReactNode;
};
// @escType - React.ReactNode
const comp: React.ReactNode;
