import { type PluginConfig, PluginOutput } from './interfaces.ts';
export declare const BEGIN: unique symbol;
export declare const END: unique symbol;
export declare const ENV: unique symbol;
export declare const GET_BLOCK: unique symbol;
export declare const IMPORT_STATE: unique symbol;
export declare const EXPORT_STATE: unique symbol;
export declare const STORE: unique symbol;
export declare const RESET: unique symbol;
export declare class Block {
    buffer: ArrayBufferLike;
    view: DataView;
    local: boolean;
    get byteLength(): number;
    constructor(arrayBuffer: ArrayBufferLike, local: boolean);
    static indexToAddress(idx: bigint | number): bigint;
    static addressToIndex(addr: bigint | number): number;
    static maskAddress(addr: bigint | number): number;
}
export type CallState = {
    blocks: [ArrayBufferLike | null, number][];
    stack: [number | null, number | null, number | null][];
};
export declare class CallContext {
    #private;
    /** @hidden */
    constructor(type: {
        new (size: number): ArrayBufferLike;
    }, logger: Console, config: PluginConfig);
    /**
     * Allocate a chunk of host memory visible to plugins via other extism host functions.
     * Returns the start address of the block.
     */
    alloc(size: bigint | number): bigint;
    /**
     * Read a variable from extism memory by name.
     *
     * @returns {@link PluginOutput}
     */
    getVariable(name: string): PluginOutput | null;
    /**
     * Set a variable to a given string or byte array value. Returns the start
     * address of the variable. The start address is reused when changing the
     * value of an existing variable.
     *
     * @returns bigint
     */
    setVariable(name: string, value: string | Uint8Array): bigint;
    /**
     * Given an address in extism memory, return a {@link PluginOutput} that represents
     * a view of that memory. Returns null if the address is invalid.
     *
     * @returns bigint
     */
    read(addr: bigint | number): PluginOutput | null;
    /**
     * Store a string or Uint8Array value in extism memory.
     *
     * @returns bigint
     */
    store(input: string | Uint8Array): bigint;
    length(addr: bigint): bigint;
    /** @hidden */
    [ENV]: {
        alloc: (n: bigint) => bigint;
        free: (addr: number) => void;
        load_u8: (addr: bigint) => number;
        load_u64: (addr: bigint) => bigint;
        store_u8: (addr: bigint, n: number) => void;
        store_u64: (addr: bigint, n: bigint) => void;
        input_offset: () => bigint;
        input_length: () => bigint;
        input_load_u8: (addr: bigint) => number;
        input_load_u64: (addr: bigint) => bigint;
        output_set: (addr: bigint, length: bigint) => void;
        error_set: (addr: bigint) => void;
        config_get: (addr: bigint) => bigint;
        var_get: (addr: bigint) => bigint;
        var_set: (addr: bigint, valueaddr: bigint) => 0n | undefined;
        http_request: (_requestOffset: bigint, _bodyOffset: bigint) => bigint;
        http_status_code: () => number;
        length: (addr: bigint) => bigint;
        length_unsafe: (addr: bigint) => bigint;
        log_warn: (addr: bigint) => void;
        log_info: (addr: bigint) => void;
        log_debug: (addr: bigint) => void;
        log_error: (addr: bigint) => void;
    };
    /** @hidden */
    [RESET](): void;
    /** @hidden */
    [GET_BLOCK](index: number): Block;
    /** @hidden */
    [IMPORT_STATE](state: CallState, copy?: boolean): void;
    /** @hidden */
    [EXPORT_STATE](): CallState;
    /** @hidden */
    [STORE](input?: string | Uint8Array): number | null;
    /** @hidden */
    [BEGIN](input: number | null): void;
    /** @hidden */
    [END](): [number | null, number | null];
}
