import {createRoot} from "react-dom/client";

import Playground from "./playground";

const domNode = document.getElementById("root");
if (domNode) {
    const root = createRoot(domNode);
    root.render(<Playground />);
}
