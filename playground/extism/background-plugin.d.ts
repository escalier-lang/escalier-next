/// <reference types="node" />
import { CallContext } from './call-context.ts';
import { PluginOutput, type InternalConfig } from './interfaces.ts';
import { Worker } from 'node:worker_threads';
declare class BackgroundPlugin {
    #private;
    worker: Worker;
    sharedData: SharedArrayBuffer;
    sharedDataView: DataView;
    hostFlag: Int32Array;
    opts: InternalConfig;
    constructor(worker: Worker, sharedData: SharedArrayBuffer, opts: InternalConfig, context: CallContext);
    reset(): Promise<boolean>;
    isActive(): boolean;
    functionExists(funcName: string): Promise<boolean>;
    call(funcName: string, input?: string | Uint8Array): Promise<PluginOutput | null>;
    callBlock(funcName: string, input: number | null): Promise<[number | null, number | null]>;
    getExports(): Promise<WebAssembly.ModuleExportDescriptor[]>;
    getImports(): Promise<WebAssembly.ModuleImportDescriptor[]>;
    getInstance(): Promise<WebAssembly.Instance>;
    close(): Promise<void>;
}
export declare function createBackgroundPlugin(opts: InternalConfig, names: string[], modules: WebAssembly.Module[]): Promise<BackgroundPlugin>;
export {};
