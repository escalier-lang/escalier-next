import createPlugin from '@extism/extism';
import { File, PreopenDirectory, Directory } from "@bjorn3/browser_wasi_shim";

const inputTxt = await fetch(`/input.txt`);
const inputBuffer = await inputTxt.arrayBuffer();

// TODO: fetch these in parallel
const libs = {
    es5: await fetch(`/node_modules/typescript/lib/lib.es5.d.ts`).then((res) => res.arrayBuffer()),
    es2015Core: await fetch(`/node_modules/typescript/lib/lib.es2015.core.d.ts`).then((res) => res.arrayBuffer()),
    es2015Symbol: await fetch(`/node_modules/typescript/lib/lib.es2015.symbol.d.ts`).then((res) => res.arrayBuffer()),
    es2015SymbolWellknown: await fetch(`/node_modules/typescript/lib/lib.es2015.symbol.wellknown.d.ts`).then((res) => res.arrayBuffer()),
    es2015Iterable: await fetch(`/node_modules/typescript/lib/lib.es2015.iterable.d.ts`).then((res) => res.arrayBuffer()),
    es2015Generator: await fetch(`/node_modules/typescript/lib/lib.es2015.generator.d.ts`).then((res) => res.arrayBuffer()),
    es2015Proxy: await fetch(`/node_modules/typescript/lib/lib.es2015.proxy.d.ts`).then((res) => res.arrayBuffer()),
    dom: await fetch(`/node_modules/typescript/lib/lib.dom.d.ts`).then((res) => res.arrayBuffer()),
};

const pkgName = "Escalier.Playground";
const build: "Debug" | "Release" = "Release";
const url = new URL(`${location.protocol}/${location.host}/src/${pkgName}/bin/${build}/net8.0/wasi-wasm/AppBundle/${pkgName}.wasm`);
const options = {
    useWasi: true,
    enableWasiOutput: true,
    fileDescriptors: [
        new PreopenDirectory(".", {
            "input.txt": new File(inputBuffer),
            "node_modules": new Directory({
                "typescript": new Directory({
                    "lib": new Directory({
                        "lib.es5.d.ts": new File(libs.es5),
                        "lib.es2015.core.d.ts": new File(libs.es2015Core),
                        "lib.es2015.symbol.d.ts": new File(libs.es2015Symbol),
                        "lib.es2015.symbol.wellknown.d.ts": new File(libs.es2015SymbolWellknown),
                        "lib.es2015.iterable.d.ts": new File(libs.es2015Iterable),
                        "lib.es2015.generator.d.ts": new File(libs.es2015Generator),
                        "lib.es2015.proxy.d.ts": new File(libs.es2015Proxy),
                        "lib.dom.d.ts": new File(libs.dom),
                    }),
                }),
            }),
        }),
    ]
};
const plugin = await createPlugin(url, options);

const input = JSON.stringify({ greeting: "hello", name: "world" });
const out1 = await plugin.call("greet", input);
if (out1) {
    console.log(out1.text());
}

let start = Date.now();
const src: string = `
let x = 5;
let y = 10;
`;
const out2 = await plugin.call("compile", src);
if (out2) {
    console.log(out2.text());
}
let elapsed = Date.now() - start;
console.log(`Elapsed time: ${elapsed} ms`);

start = Date.now();
const out3 = await plugin.call("compile", src);
if (out3) {
    console.log(out3.text());
}
elapsed = Date.now() - start;
console.log(`Elapsed time: ${elapsed} ms`);
