import { nodeResolve } from "@rollup/plugin-node-resolve";
import typescript from '@rollup/plugin-typescript';

export default {
    input: "playground/index.tsx",
    output: {
        dir: "dist",
        format: "es",
    },
    plugins: [typescript(), nodeResolve()],
};
