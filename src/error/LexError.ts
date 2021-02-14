import {
	Char,
	LexError,
} from '@chharvey/parser';



/**
 * A LexError03 is thrown when an integer literal or string literal contains an invalid Unicode escape sequence.
 * @example
 * 'this is invalid: \u{defg}'; % LexError03: Invalid escape sequence...
 */
export class LexError03 extends LexError {
	/** The number series of this class of errors. */
	static readonly CODE = 3
	/**
	 * Construct a new LexError03 object.
	 * @param span - the invalid escape sequence
	 * @param line - the line   index of the string’s location
	 * @param col  - the column index of the string’s location
	 */
	constructor (span: string, line: number, col: number) {
		super(`Invalid escape sequence: \`${span}\` at line ${line + 1} col ${col + 1}.`, LexError03.CODE, line, col)
	}
}
/**
 * A LexError04 is thrown when a numeric separator was found in a number literal but was not expected.
 *
 * The numeric separator is the underscore character, `_` **U+005F LOW LINE**,
 * and may be used to separate the digits in a number literal token to improve readability.
 * This error is thrown when a numeric separator is found where it does not belong.
 * @example
 * 1_000__000; % LexError04: Numeric separator not allowed...
 * 1_000_000_; % LexError04: Numeric separator not allowed...
 */
export class LexError04 extends LexError {
	/** The number series of this class of errors. */
	static readonly CODE = 4
	/**
	 * Construct a new LexError03 object.
	 * @param char - the numeric separator character
	 */
	constructor (char: Char) {
		super(`Numeric separator not allowed: at line ${char.line_index + 1} col ${char.col_index + 1}.`, LexError04.CODE, char.line_index, char.col_index)
	}
}
/**
 * A LexError05 is thrown when a float literal has an invalid format.
 * @example
 * 5.e+2; % LexError05: Invalid floating-point literal format...
 * 5.0E2; % LexError05: Invalid floating-point literal format...
 * 5.0e;  % LexError05: Invalid floating-point literal format...
 */
export class LexError05 extends LexError {
	/** The number series of this class of errors. */
	static readonly CODE = 5
	/**
	 * Construct a new LexError05 object.
	 * @param token - the float literal token
	 */
	constructor (char: Char) {
		super(`Invalid exponential notation: at line ${char.line_index + 1} col ${char.col_index + 1}.`, LexError05.CODE, char.line_index, char.col_index)
	}
}
