import type SolidNumber from './SolidNumber.class'
import Float64 from './Float64.class'



// HACK: this is defined here, instead of in `../class/SemanticNode.class`, to avoid circular imports.
export enum Operator {
	NOT,
	EMP,
	AFF,
	NEG,
	EXP,
	MUL,
	DIV,
	ADD,
	SUB,
	LT,
	GT,
	LE,
	GE,
	NLT,
	NGT,
	IS,
	ISNT,
	EQ,
	NEQ,
	AND,
	NAND,
	OR,
	NOR,
	COND,
}



export default abstract class Instruction {
	/**
	 * Compare two instructions.
	 * @param instruction the instruction to compare
	 * @returns Is this instruction equal to the argument?
	 * @final
	 */
	equals(instruction: Instruction): boolean {
		return this === instruction || this.equals_helper(instruction)
	}
	protected equals_helper(instruction: Instruction): boolean {
		return this.toString() === instruction.toString()
	}
}



/**
 * Absence of instruction.
 */
export class InstructionNone extends Instruction {
	/**
	 * @return `''`
	 */
	toString(): string {
		return ''
	}
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionNone
	}
}
/**
 * Throw an error at runtime.
 */
class InstructionUnreachable extends Instruction {
	/**
	 * @return `'(unreachable)'`
	 */
	toString(): string {
		return `(unreachable)`
	}
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionUnreachable
	}
}
/**
 * Do nothing at runtime.
 */
class InstructionNop extends Instruction {
	/**
	 * @return `'(nop)'`
	 */
	toString(): string {
		return `(nop)`
	}
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionNop
	}
}
/**
 * A superclass abstracting:
 * - InstructionConst
 * - InstructionUnop
 * - InstructionBinop
 */
export abstract class InstructionExpression extends Instruction {
	abstract get isFloat(): boolean;
}
/**
 * Push a constant onto the stack.
 */
export class InstructionConst extends InstructionExpression {
	/**
	 * @param value the constant to push
	 */
	constructor (private readonly value: SolidNumber) {
		super()
	}
	/**
	 * @return `'({i32|f64}.const ‹value›)'`
	 */
	toString(): string {
		return `(${ (!this.isFloat) ? 'i32' : 'f64' }.const ${ this.value })`
	}
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionConst
			&& this.value === instruction.value
	}
	get isFloat(): boolean {
		return this.value instanceof Float64
	}
}
/**
 * Local variable operations.
 */
abstract class InstructionLocal extends InstructionExpression {
	/**
	 * @param name the variable name (must begin with `'$'`)
	 * @param op an optional expression to manipulate, or a type to declare
	 */
	constructor (
		protected readonly name: string,
		protected readonly op: InstructionExpression | boolean = false,
	) {
		super()
	}
	/** @final */
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionLocal
			&& this.name === instruction.name
			&& (
				(this.op instanceof Instruction && instruction.op instanceof Instruction)
					? this.op.equals(instruction.op)
					: this.op === instruction.op
			)
	}
	get isFloat(): boolean {
		return this.op instanceof InstructionExpression ? this.op.isFloat : this.op
	}
}
/**
 * Set a local variable.
 */
class InstructionSet extends InstructionLocal {
	/** @return `'(local.set ‹name› ‹op›?)'` */
	toString(): string {
		return `(local.set ${ this.name } ${ this.op instanceof InstructionExpression ? this.op : ''})`
	}
}
/**
 * Get a local variable.
 */
export class InstructionGet extends InstructionLocal {
	/** @return `'(local.get ‹name› ‹op›?)'` */
	toString(): string {
		return `(local.get ${ this.name } ${ this.op instanceof InstructionExpression ? this.op : ''})`
	}
}
/**
 * Tee a local variable.
 */
export class InstructionTee extends InstructionLocal {
	/** @return `'(local.tee ‹name› ‹op›?)'` */
	toString(): string {
		return `(local.tee ${ this.name } ${ this.op instanceof InstructionExpression ? this.op : ''})`
	}
}
/**
 * Perform a unary operation on the stack.
 */
export class InstructionUnop extends InstructionExpression {
	/**
	 * @param op a punctuator representing the operation to perform
	 * @param arg the operand
	 */
	constructor (
		private readonly op: Operator,
		private readonly arg: InstructionExpression,
	) {
		super()
	}
	/**
	 * @return `'(‹op› ‹arg›)'`
	 */
	toString(): string {
		return `(${ new Map<Operator, string>([
			[Operator.AFF, `nop`],
			[Operator.NEG, (!this.arg.isFloat) ? `call $neg`  : `f64.neg`],
			[Operator.NOT, (!this.arg.isFloat) ? `call $inot` : `call $fnot`],
			[Operator.EMP, (!this.arg.isFloat) ? `call $iemp` : `call $femp`],
		]).get(this.op) || (() => { throw new TypeError('Invalid operation.') })() } ${ this.arg })`
	}
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionUnop
			&& this.op === instruction.op
			&& this.arg.equals(instruction.arg)
	}
	get isFloat(): boolean {
		return [Operator.AFF, Operator.NEG].includes(this.op) && this.arg.isFloat
	}
}
/**
 * Perform a binary operation on the stack.
 */
export class InstructionBinop extends InstructionExpression {
	/**
	 * @param op a punctuator representing the operation to perform
	 * @param arg0 the first operand
	 * @param arg1 the second operand
	 */
	constructor (
		private readonly op: Operator,
		private readonly arg0: InstructionExpression,
		private readonly arg1: InstructionExpression,
	) {
		super()
		if (this.isFloat && (!this.arg0.isFloat || !this.arg1.isFloat)) {
			throw new TypeError(`Both operands must be either integers or floats, but not a mix.\nOperands: ${ this.arg0 } ${ this.arg1 }`)
		}
	}
	/**
	 * @return `'(‹op› ‹arg0› ‹arg1›)'`
	 */
	toString(): string {
		if ([Operator.AND, Operator.OR].includes(this.op)) {
			const varname: string = `$operand0`
			const condition: InstructionExpression = new InstructionUnop(Operator.NOT, new InstructionUnop(Operator.NOT, new InstructionTee(varname, this.arg0)))
			const left:      InstructionExpression = new InstructionGet(varname, this.arg0.isFloat)
			const right:     InstructionExpression = this.arg1
			return `(local ${ varname } ${ (!this.arg0.isFloat) ? `i32` : `f64` }) ${
				(this.op === Operator.AND)
					? new InstructionCond(condition, right, left)
					: new InstructionCond(condition, left, right)
			}`
		}
		return `(${ new Map<Operator, string>([
			[Operator.ADD, (!this.isFloat) ? `i32.add`   : `f64.add`],
			[Operator.SUB, (!this.isFloat) ? `i32.sub`   : `f64.sub`],
			[Operator.MUL, (!this.isFloat) ? `i32.mul`   : `f64.mul`],
			[Operator.DIV, (!this.isFloat) ? `i32.div_s` : `f64.div`],
			[Operator.EXP, (!this.isFloat) ? `call $exp` : new InstructionUnreachable().toString()], // TODO Runtime exponentiation not yet supported.
			[Operator.LT,  (!this.isFloat) ? `i32.lt_s`  : `f64.lt`],
			[Operator.GT,  (!this.isFloat) ? `i32.gt_s`  : `f64.gt`],
			[Operator.LE,  (!this.isFloat) ? `i32.le_s`  : `f64.le`],
			[Operator.GE,  (!this.isFloat) ? `i32.ge_s`  : `f64.ge`],
			[Operator.IS,  (!this.isFloat) ? `i32.eq`    : `call $fis`],
			[Operator.EQ,  (!this.isFloat) ? `i32.eq`    : `f64.eq`],
		]).get(this.op) || (() => { throw new TypeError('Invalid operation.') })() } ${ this.arg0 } ${ this.arg1 })`
	}
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionBinop
			&& this.op === instruction.op
			&& this.arg0.equals(instruction.arg0)
			&& this.arg1.equals(instruction.arg1)
	}
	get isFloat(): boolean {
		return this.arg0.isFloat || this.arg1.isFloat
	}
}
/**
 * Perform a conditional operation on the stack.
 */
export class InstructionCond extends InstructionExpression {
	/**
	 * @param arg0 the first operand
	 * @param arg1 the second operand
	 * @param arg2 the third operand
	 */
	constructor (
		private readonly arg0: InstructionExpression,
		private readonly arg1: InstructionExpression,
		private readonly arg2: InstructionExpression,
	) {
		super()
		if (this.isFloat && (!this.arg1.isFloat || !this.arg2.isFloat)) {
			throw new TypeError(`Both branches must be either integers or floats, but not a mix.\nOperands: ${ this.arg1 } ${ this.arg2 }`)
		}
	}
	/**
	 * @return `'(if (result {i32|f64}) ‹arg0› (then ‹arg1›) (else ‹arg2›))'`
	 */
	toString(): string {
		return `(if (result ${ (!this.isFloat) ? `i32` : `f64` }) ${ this.arg0 } (then ${ this.arg1 }) (else ${ this.arg2 }))`
	}
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionCond
			&& this.arg0.equals(instruction.arg0)
			&& this.arg1.equals(instruction.arg1)
			&& this.arg2.equals(instruction.arg2)
	}
	get isFloat(): boolean {
		return this.arg1.isFloat || this.arg2.isFloat
	}
}
/**
 * Create a new operand stack.
 */
export class InstructionStatement extends Instruction {
	/**
	 * @param count the index of the statement within its scope
	 * @param expr the expression
	 */
	constructor (
		private readonly count: bigint,
		private readonly expr: InstructionExpression,
	) {
		super()
	}
	/**
	 * @return a new function evaluating the argument
	 */
	toString(): string {
		return `
			(func (export "f${ this.count }") (result ${ (!this.expr.isFloat) ? `i32` : `f64` })
				${ this.expr }
			)
		`
	}
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionStatement
			&& this.count === instruction.count
			&& this.expr.equals(instruction.expr)
	}
}
/**
 * Create a program.
 */
export class InstructionModule extends Instruction {
	/**
	 * @param comps the components of the program
	 */
	constructor (private readonly comps: (string | Instruction)[] = []) {
		super()
	}
	/**
	 * @return a new module containing the components
	 */
	toString(): string {
		return `
			(module
				${ this.comps.join('\n') }
			)
		`
	}
	protected equals_helper(instruction: Instruction): boolean {
		return instruction instanceof InstructionModule
			&& this.comps.every((comp, i) => {
				const comp_i: string | Instruction = instruction.comps[i]
				return (comp instanceof Instruction && comp_i instanceof Instruction)
					? comp.equals(comp_i)
					: comp === comp_i
			})
	}
}
