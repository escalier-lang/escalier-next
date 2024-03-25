import type { ManifestLike, ExtismPluginOptions, Plugin } from './interfaces.ts';
export { CAPABILITIES } from './polyfills/deno-capabilities.ts';
export type { Capabilities, ExtismPluginOptions, ManifestLike, ManifestWasmResponse, ManifestWasmModule, ManifestWasmData, ManifestWasmUrl, ManifestWasmPath, ManifestWasm, Manifest, Plugin, PluginConfig, PluginConfigLike, PluginOutput, } from './interfaces.ts';
export type { CallContext, CallContext as CurrentPlugin } from './call-context.ts';
/**
 * Create a {@link Plugin} given a {@link ManifestLike} and {@link ExtismPluginOptions}.
 *
 * Plugins wrap Wasm modules, exposing rich access to exported functions.
 *
 * ```ts
 * const plugin = await createPlugin(
 *   'https://github.com/extism/plugins/releases/download/v0.3.0/count_vowels.wasm',
 *   { useWasi: true }
 * );
 *
 * try {
 *   const result = await plugin.call('count_vowels', 'hello world');
 *   const parsed = result.json();
 *
 *   console.log(parsed); // { count: 3, total: 3, vowels: "aeiouAEIOU" }
 * } finally {
 *   await plugin.close();
 * }
 * ```
 *
 * {@link Plugin | `Plugin`} can run on a background thread when the
 * environment supports it. You can see if the current environment supports
 * background plugins by checking the {@link Capabilities#hasWorkerCapability |
 * `hasWorkerCapability`} property of {@link CAPABILITIES}.
 *
 * @param manifest A {@link ManifestLike | `ManifestLike`}. May be a `string`
 * representing a URL, JSON, a path to a wasm file ({@link
 * Capabilities#manifestSupportsPaths | in environments} where paths are
 * supported); an [ArrayBuffer](https://mdn.io/ArrayBuffer); or a {@link
 * Manifest}.
 *
 * @param opts {@link ExtismPluginOptions | options} for controlling the behavior
 * of the plugin.
 *
 * @returns a promise for a {@link Plugin}.
 */
export declare function createPlugin(manifest: ManifestLike | PromiseLike<ManifestLike>, opts?: ExtismPluginOptions): Promise<Plugin>;
export { createPlugin as newPlugin };
export default createPlugin;
