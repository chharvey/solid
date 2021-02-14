import type * as AST from './ASTNode';
import type {SolidLanguageType} from './SolidLanguageType';



/** Kinds of symbols. */
export enum SymbolKind {
	/** A value variable (a variable holding a Solid Language Value). */
	VALUE,
	/** A type variable / type alias. */
	TYPE,
}



export class SymbolStructure {
	constructor (
		/** The unique identifier of the symbol, the cooked value of the token. */
		readonly id: bigint,
		/** The 0-based line index of where the symbol was declared. */
		readonly line: number,
		/** Tthe 0-based column index of where the symbol was declared. */
		readonly col: number,
	) {
	}
}



export class SymbolStructureType extends SymbolStructure {
	constructor (
		id: bigint,
		line: number,
		col: number,
		/** The static definition of the symbol. */
		readonly defn: AST.ASTNodeType,
	) {
		super(id, line, col);
	}
}



export class SymbolStructureVar extends SymbolStructure {
	constructor (
		id: bigint,
		line: number,
		col: number,
		/** The variable’s Type. */
		readonly type: SolidLanguageType,
		/** May the symbol be reassigned? */
		readonly unfixed: boolean,
		/** The static definition of the symbol, or `null` if the symbol is unfixed. */
		readonly defn: AST.ASTNodeExpression | null,
	) {
		super(id, line, col);
	}
}
