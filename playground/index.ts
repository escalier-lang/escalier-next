import createPlugin from '@extism/extism';
import { File, PreopenDirectory } from "@bjorn3/browser_wasi_shim";

const input = await fetch(`/input.txt`);
const inputBuffer = await input.arrayBuffer();

const pkgName = "Escalier.Playground";
const url = new URL(`${location.protocol}/${location.host}/src/${pkgName}/bin/Debug/net8.0/wasi-wasm/AppBundle/${pkgName}.wasm`);
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

const input1 = JSON.stringify({ greeting: "hello", name: "world" });
const out1 = await plugin.call("greet", input1);
if (out1) {
    console.log(out1.text());
}
