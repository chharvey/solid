import * as fs from 'fs'
import * as path from 'path'

import wabt from 'wabt' // need `tsconfig.json#compilerOptions.esModuleInterop = true`

import type SolidConfig from '../SolidConfig'
import type {
	SemanticNodeExpression,
	SemanticStatementType,
	SemanticNodeGoal,
} from '../typer'
import {
	Parser,
} from '../parser'
import {
	InstructionStatement,
	InstructionModule,
} from './Instruction.class'



/**
 * The Builder generates assembly code.
 */
export default class Builder {
	static readonly IMPORTS: readonly string[] = [
		fs.readFileSync(path.join(__dirname, '../../src/builder/not.wat'), 'utf8'),
		fs.readFileSync(path.join(__dirname, '../../src/builder/emp.wat'), 'utf8'),
		fs.readFileSync(path.join(__dirname, '../../src/builder/neg.wat'), 'utf8'),
		fs.readFileSync(path.join(__dirname, '../../src/builder/exp.wat'), 'utf8'),
		fs.readFileSync(path.join(__dirname, '../../src/builder/fis.wat'), 'utf8'),
	]
	/** A counter for statements. */
	private stmt_count: bigint = 0n;
	/** The goal symbol of the program. */
	private readonly _goal: SemanticNodeGoal;

	/**
	 * Construct a new Builder object.
	 * @param source - the entire source text
	 * @param config - The configuration settings for an instance program.
	 */
	constructor (source: string, config: SolidConfig) {
		this._goal = new Parser(source, config).parse().decorate()
	}

	/**
	 * Return a new statement-expression.
	 * @param expr a semantic expression
	 * @return a call to {@link CodeGenerator.stmt}
	 */
	stmt(expr: SemanticNodeExpression): InstructionStatement {
		return new InstructionStatement(this.stmt_count++, expr.build(this))
	}

	/**
	 * Return the semantic goal of a program.
	 * @param comps the top-level components
	 * @return an instruction for the list of top-level components
	 */
	goal(comps: readonly SemanticStatementType[]): InstructionModule {
		return new InstructionModule([
			...Builder.IMPORTS,
			...comps.map((comp) => comp.build(this)),
		])
	}

	/**
	 * Return the instructions to print to file.
	 * @return a readable text output in WAT format, to be compiled into WASM
	 */
	print(): string {
		return this._goal.build(this).toString();
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
