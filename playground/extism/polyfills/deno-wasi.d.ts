import { Fd } from '@bjorn3/browser_wasi_shim';
import { type InternalWasi } from '../interfaces.ts';
export declare function loadWasi(allowedPaths: {
    [from: string]: string;
}, enableWasiOutput: boolean, fileDescriptors: Fd[]): Promise<InternalWasi>;
