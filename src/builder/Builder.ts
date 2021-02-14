import * as fs from 'fs'
import * as path from 'path'

import wabt from 'wabt' // need `tsconfig.json#compilerOptions.esModuleInterop = true`

import {
	SolidConfig,
	CONFIG_DEFAULT,
} from '../core/';
import {
	ParserSolid as Parser,
} from '../parser/';
import {
	Decorator,
	Validator,
	AST,
} from '../validator/'



/**
 * The Builder generates assembly code.
 */
export class Builder {
	static readonly IMPORTS: readonly string[] = [
		fs.readFileSync(path.join(__dirname, '../../src/builder/not.wat'), 'utf8'),
		fs.readFileSync(path.join(__dirname, '../../src/builder/emp.wat'), 'utf8'),
		fs.readFileSync(path.join(__dirname, '../../src/builder/neg.wat'), 'utf8'),
		fs.readFileSync(path.join(__dirname, '../../src/builder/exp.wat'), 'utf8'),
		fs.readFileSync(path.join(__dirname, '../../src/builder/fis.wat'), 'utf8'),
	]


	/** The Validator for conducting semantic analysis. */
	readonly validator: Validator;
	/** An AST goal produced by a Decorator. */
	private readonly ast_goal: AST.ASTNodeGoal;
	/** A counter for internal variables. Used for optimizing short-circuited expressions. */
	private var_count: bigint = 0n
	/** A counter for statements. */
	private stmt_count: bigint = 0n;

	/**
	 * Construct a new Builder object.
	 * @param source - the source text
	 * @param config - The configuration settings for an instance program.
	 */
	constructor (
		source: string,
		readonly config: SolidConfig = CONFIG_DEFAULT,
	) {
		this.validator = new Validator(this.config);
		this.ast_goal  = Decorator.decorate(new Parser(source, config).parse());
		this.ast_goal.varCheck (this.validator); // assert does not throw
		this.ast_goal.typeCheck(this.validator); // assert does not throw
	}

	/**
	 * Return this Builder’s short-circuit variable count, and then increment it.
	 * @return this Builder’s current variable counter
	 */
	get varCount(): bigint {
		return this.var_count++
	}

	/**
	 * Return this Builder’s statement count, and then increment it.
	 * Also resets the short-circuit variable count.
	 * @return this Builder’s current statement counter
	 */
	get stmtCount(): bigint {
		this.var_count = 0n
		return this.stmt_count++
	}

	/**
	 * Return the instructions to print to file.
	 * @return a readable text output in WAT format, to be compiled into WASM
	 */
	print(): string {
		return this.ast_goal.build(this).toString()
	}

	/**
	 * Return a binary format of the program.
	 * @return a binary output in WASM format, which can be executed
	 */
	async compile(): Promise<Uint8Array> {
		const waModule = (await wabt()).parseWat('', this.print(), {})
		waModule.validate()
		return waModule.toBinary({}).buffer
	}
}
