import type { ManifestLike } from './interfaces.ts';
export declare function toWasmModuleData(input: ManifestLike, _fetch: typeof fetch): Promise<[string[], WebAssembly.Module[]]>;
