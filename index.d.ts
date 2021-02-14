/*
 * Note: this file exists only for typescript declaration.
 * It is not meant to be compiled.
 * See `./index.js` for the manual output.
 */



import type {SolidConfig} from './src/core/';


/**
 * Compile Solid source code into a readable text format for development purposes.
 * Output text is in [WAT](https://webassembly.org/) format.
 * @param sourcecode - the Solid source text
 * @return the output text
 */
export declare function print(sourcecode: string, config?: SolidConfig): string;

/**
 * Compile Solid source code into an executable binary format.
 * Output text is in [WASM](https://webassembly.org/) format.
 * @param sourcecode - the Solid source text
 * @return the output as a binary format
 */
export declare function compile(sourcecode: string, config?: SolidConfig): Promise<Uint8Array>;
