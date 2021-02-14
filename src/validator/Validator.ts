import {
	SolidConfig,
	CONFIG_DEFAULT,
} from '../core/';
import type {
	SymbolStructure,
} from './SymbolStructure';



/**
 * The Validator is responsible for semantically analyzing, type-checking, and validating source code.
 *
 * Part of semantic analysis is the Decorator, which transforms concrete parse nodes into abstract semantic nodes.
 * It prepares the nodes for the Validator by performing certian operations such as:
 * - removing unnecessary nested nodes, e.g. `(unary (unit (prim 2)))` becomes `(const 2)`
 * - replacing certain syntaxes with data, e.g.
 * 	from `(additive (additive (... 2)) (token '+') (multiplicative (... 3)))`
 * 	to `(sum (const 2) (const 3))`
 */
export class Validator {
	/** A symbol table, which keeps tracks of variables. */
	private readonly symbol_table: Map<bigint, SymbolStructure> = new Map();

	/**
	 * Construct a new Validator object.
	 * @param config - The configuration settings for an instance program.
	 */
	constructor (
		readonly config: SolidConfig = CONFIG_DEFAULT,
	) {
	}

	/**
	 * Add a symbol representing a value variable or type variable to this Validator’s symbol table.
	 * @param symbol the object encoding data of the symbol
	 * @returns this
	 */
	addSymbol(symbol: SymbolStructure): this {
		this.symbol_table.set(symbol.id, symbol);
		return this
	}
	/**
	 * Remove a symbol from this Validator’s symbol table.
	 * @param id the id of the symbol to remove
	 * @returns this
	 */
	removeSymbol(id: bigint): this {
		this.symbol_table.delete(id);
		return this
	}
	/**
	 * Check whether this Validator’s symbol table has the symbol.
	 * @param id the symbol id to check
	 * @returns Does the symbol table have a symbol with the given id?
	 */
	hasSymbol(id: bigint): boolean {
		return this.symbol_table.has(id);
	}
	/**
	 * Return the information of a symbol in this Validator’s symbol table.
	 * @param id the symbol id to check
	 * @returns the symbol information of `id`, or `null` if there is no corresponding entry
	 */
	getSymbolInfo(id: bigint): SymbolStructure | null {
		return this.symbol_table.get(id) || null;
	}
	/**
	 * Remove all symbols from this Validator’s symbol table.
	 * @returns this
	 */
	clearSymbols(): this {
		this.symbol_table.clear();
		return this;
	}
}
