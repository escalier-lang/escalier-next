import createPlugin, {Plugin} from './extism/mod';
import { File, PreopenDirectory, Directory } from "@bjorn3/browser_wasi_shim";

export async function greet () {
    const inputTxt = await fetch(`/input.txt`);
    const inputBuffer = await inputTxt.arrayBuffer();

    const pkgName = "Escalier.Playground";
    const build: "Debug" | "Release" = "Release";
    const url = new URL(`${location.protocol}/${location.host}/src/${pkgName}/bin/${build}/net8.0/wasi-wasm/AppBundle/${pkgName}.wasm`);
    const options = {
        useWasi: true,
        enableWasiOutput: true,
        fileDescriptors: [
            new PreopenDirectory(".", {
                "input.txt": new File(inputBuffer),
            }),
        ]
    };
    const plugin = await createPlugin(url, options);

    const input = JSON.stringify({ greeting: "hello", name: "world" });
    const out1 = await plugin.call("greet", input);
    if (out1) {
        console.log(out1.text());
    }
}

export async function loadPlugin(): Promise<Plugin> {
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
    
    console.log(plugin);
    return plugin;
}

export async function typecheck(src: string, plugin: Plugin) {
    const out = await plugin.call("compile", src);
    if (out) {
        const data: {js: string, dts: string} = out.json();
        return data;
    }
    throw new Error("Failed to typecheck");
}
