import {
	ErrorCode,
} from '@chharvey/parser';

import type {
	AST,
	SolidLanguageType,
} from '../validator/'



/**
 * A TypeError is thrown when the validator recognizes a type mismatch.
 */
class SolidTypeError extends ErrorCode {
	/** The name of this class of errors. */
	static readonly NAME: string = 'TypeError'
	/** The number series of this class of errors. */
	static readonly CODE: number = 2300
	/**
	 * Construct a new TypeError object.
	 * @param message - a message to the user
	 * @param code    - the error number
	 * @param line    - the line index in source code
	 * @param col     - the column index in source code
	 */
	constructor (message: string, code: number = 0, line?: number, col?: number) {
		super({
			message,
			name       : SolidTypeError.NAME,
			code       : SolidTypeError.CODE + code,
			line_index : line,
			col_index  : col,
		})
	}
}


/**
 * A TypeError01 is thrown when the validator encounters an invalid operation.
 * @example
 * true + false; % TypeError01: Invalid operation.
 */
export class TypeError01 extends SolidTypeError {
	/** The number series of this class of errors. */
	static readonly CODE = 1
	/**
	 * Construct a new TypeError01 object.
	 * @param expression - the invalid operation expression
	 */
	constructor (expression: AST.ASTNodeOperation) {
		super(`Invalid operation: \`${ expression.source }\` at line ${ expression.line_index + 1 } col ${ expression.col_index + 1 }.`, TypeError01.CODE, expression.line_index, expression.col_index)
	}
}
/**
 * A TypeError02 is thrown when one type is expected to narrow another type, but does not.
 * A general error used for different cases, such as compound types’s components, generic constraints, or throwing non-Exceptions.
 */
// @ts-expect-error noUnusedLocals
class TypeError02 extends SolidTypeError {
	/** The number series of this class of errors. */
	static readonly CODE = 2
	/**
	 * Construct a new TypeError02 object.
	 * @param subtype   - the expected subtype
	 * @param supertype - the supertype
	 */
	constructor (subtype: SolidLanguageType, supertype: SolidLanguageType, line_index: number, col_index: number) {
		super(`Type ${ subtype } is not a subtype of type ${ supertype }.`, TypeError02.CODE, line_index, col_index)
	}
}
/**
 * A TypeError03 is thrown when an expression is assigned to a type to which it is not assignable.
 * @example
 * let x: int = true; % TypeError03: Expression of type `true` is not assignable to type `int`.
 */
export class TypeError03 extends SolidTypeError {
	/** The number series of this class of errors. */
	static readonly CODE = 3
	/**
	 * Construct a new TypeError03 object.
	 * @param assignment    - the node where the assignment took place
	 * @param assignee_type - the type to which the expression is assigned
	 * @param assigned_type - the type of the expression
	 */
	constructor (assignment: AST.ASTNodeDeclarationVariable | AST.ASTNodeAssignment, assignee_type: SolidLanguageType, assigned_type: SolidLanguageType) {
		super(`Expression of type ${ assigned_type } is not assignable to type ${ assignee_type }.`, TypeError03.CODE, assignment.line_index, assignment.col_index)
	}
}
