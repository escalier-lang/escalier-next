import { CallContext } from './call-context.ts';
import { PluginOutput, type InternalConfig, InternalWasi } from './interfaces.ts';
export declare const EXTISM_ENV = "extism:host/env";
type InstantiatedModule = [WebAssembly.Module, WebAssembly.Instance];
export declare class ForegroundPlugin {
    #private;
    constructor(context: CallContext, instancePair: InstantiatedModule, wasi: InternalWasi[]);
    reset(): Promise<boolean>;
    isActive(): boolean;
    functionExists(funcName: string): Promise<boolean>;
    callBlock(funcName: string, input: number | null): Promise<[number | null, number | null]>;
    call(funcName: string, input?: string | Uint8Array): Promise<PluginOutput | null>;
    getExports(): Promise<WebAssembly.ModuleExportDescriptor[]>;
    getImports(): Promise<WebAssembly.ModuleImportDescriptor[]>;
    getInstance(): Promise<WebAssembly.Instance>;
    close(): Promise<void>;
}
export declare function createForegroundPlugin(opts: InternalConfig, names: string[], modules: WebAssembly.Module[], context?: CallContext): Promise<ForegroundPlugin>;
export {};
