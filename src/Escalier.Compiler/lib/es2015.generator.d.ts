/// <reference lib="es2015.iterable" />

interface Generator<T = unknown, TReturn = any, TNext = unknown> extends Iterator<T, TReturn, TNext> {
    // NOTE: 'next' is defined using a tuple to ensure we report the correct assignability errors in all places.
    next(...args: [] | [TNext]): IteratorResult<T, TReturn>;

    return(value: TReturn): IteratorResult<T, TReturn>;

    throw(e: any): IteratorResult<T, TReturn>;

    [Symbol.iterator](): Generator<T, TReturn, TNext>;
}

interface GeneratorFunction {
    /**
     * The length of the arguments.
     */
    readonly length: number;
    /**
     * Returns the name of the function.
     */
    readonly name: string;
    /**
     * A reference to the prototype.
     */
    readonly prototype: Generator;

    /**
     * Creates a new Generator object.
     * @param args A list of arguments the function accepts.
     */
    new(...args: any[]): Generator;

    /**
     * Creates a new Generator object.
     * @param args A list of arguments the function accepts.
     */
    (...args: any[]): Generator;
}

interface GeneratorFunctionConstructor {
    /**
     * The length of the arguments.
     */
    readonly length: number;
    /**
     * Returns the name of the function.
     */
    readonly name: string;
    /**
     * A reference to the prototype.
     */
    readonly prototype: GeneratorFunction;

    /**
     * Creates a new Generator function.
     * @param args A list of arguments the function accepts.
     */
    new(...args: string[]): GeneratorFunction;

    /**
     * Creates a new Generator function.
     * @param args A list of arguments the function accepts.
     */
    (...args: string[]): GeneratorFunction;
}
