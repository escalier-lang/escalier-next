export declare function responseToModule(response: Response, hasHash?: boolean): Promise<{
    module: WebAssembly.Module;
    data?: ArrayBuffer;
}>;
