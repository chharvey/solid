import {
	ErrorCode,
} from '@chharvey/parser';

import type {AST} from '../validator/';
import {
	SymbolKind,
} from '../validator/SymbolStructure'; // FIXME circular imports



/**
 * A ReferenceError is thrown when the validator fails to dereference an identifier.
 */
class SolidReferenceError extends ErrorCode {
	/** The name of this class of errors. */
	static readonly NAME: string = 'ReferenceError';
	/** The number series of this class of errors. */
	static readonly CODE: number = 2100;
	/**
	 * Construct a new ReferenceError object.
	 * @param message a message to the user
	 * @param code    the error number
	 * @param line    the line index in source code
	 * @param col     the column index in source code
	 */
	constructor (message: string, code: number = 0, line?: number, col?: number) {
		super({
			message,
			name:       SolidReferenceError.NAME,
			code:       SolidReferenceError.CODE + code,
			line_index: line,
			col_index:  col,
		});
	}
}


/**
 * A ReferenceError01 is thrown when the validator encounters an undeclared variable.
 * @example
 * my_var; % ReferenceError01: `my_var` is never declared.
 */
export class ReferenceError01 extends SolidReferenceError {
	/** The number series of this class of errors. */
	static readonly CODE = 1;
	/**
	 * Construct a new ReferenceError01 object.
	 * @param variable the undeclared variable
	 */
	constructor (variable: AST.ASTNodeTypeAlias | AST.ASTNodeVariable) {
		super(`\`${ variable.source }\` is never declared.`, ReferenceError01.CODE, variable.line_index, variable.col_index);
	}
}
/**
 * A ReferenceError02 is thrown when the validator encounters a not-yet-declared variable.
 * @example
 * my_var; % ReferenceError02: `my_var` is used before it is declared.
 * % (This is called a Temporal Dead Zone.)
 * let my_var: int = 42;
 */
export class ReferenceError02 extends SolidReferenceError {
	/** The number series of this class of errors. */
	static readonly CODE = 2;
	/**
	 * Construct a new ReferenceError02 object.
	 * @param variable the not-yet-declared variable
	 */
	constructor (variable: AST.ASTNodeTypeAlias | AST.ASTNodeVariable) {
		super(`\`${ variable.source }\` is used before it is declared.`, ReferenceError02.CODE, variable.line_index, variable.col_index);
	}
}
/**
 * A ReferenceError03 is thrown when a symbol of the wrong kind is used.
 * @example
 * let FOO: int = 42;
 * type T = FOO | float; % ReferenceError03: `FOO` refers to a value, but is used as a type.
 * @example
 * type FOO = int;
 * 42 || FOO;      % ReferenceError03: `FOO` refers to a type, but is used as a value.
 */
export class ReferenceError03 extends SolidReferenceError {
	private static SYMBOL_KINDS: ReadonlyMap<SymbolKind, string> = new Map([
		[SymbolKind.VALUE, 'value'],
		[SymbolKind.TYPE,  'type'],
	]);
	/** The number series of this class of errors. */
	static readonly CODE = 3;
	/**
	 * Construct a new ReferenceError03 object.
	 * @param symbol    the referenced symbol
	 * @param refers_to what the symbol was declared as
	 * @param used_as   what the symbol is used as
	 */
	constructor (symbol: AST.ASTNodeTypeAlias | AST.ASTNodeVariable, refers_to: SymbolKind, used_as: SymbolKind) {
		const kind_refer: string = ReferenceError03.SYMBOL_KINDS.get(refers_to)!;
		const kind_used:  string = ReferenceError03.SYMBOL_KINDS.get(used_as)!;
		super(`\`${ symbol.source }\` refers to a ${ kind_refer }, but is used as a ${ kind_used }.`, ReferenceError03.CODE, symbol.line_index, symbol.col_index);
	}
}
