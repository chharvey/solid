import type {
	KleenePlus,
	EBNFObject,
} from '../types.d'
import type {
	Token,
} from '../lexer/'
import type {
	ParseNode
} from '../parser/'
import {SemanticNode} from '../validator/'
import type * as TOKEN from './Token.class'



function NonemptyArray_flatMap<T, U>(arr: KleenePlus<T>, callback: (it: T) => KleenePlus<U>): KleenePlus<U> {
	return arr.flatMap((it) => callback(it)) as readonly U[] as KleenePlus<U>
}
/** @temp */ function toDefn(op: SemanticNodeExpr, nt: SemanticNodeNonterminal, data: EBNFObject[]): EBNFObject['defn'] {
	return (op instanceof SemanticNodeOp) ? op.expand(nt, data) : [[op.source]]
}



export class SemanticNodeEBNF extends SemanticNode {
	constructor (
		start_node: ParseNode | Token,
		attributes: {[key: string]: boolean | string} = {},
		children: readonly SemanticNode[] = [],
	) {
		super(start_node, attributes, children)
	}
}



export class SemanticNodeParam extends SemanticNodeEBNF {
	constructor (start_node: TOKEN.TokenIdentifier) {
		super(start_node, {name: start_node.source})
	}
}
export class SemanticNodeArg extends SemanticNodeEBNF {
	constructor (
		start_node: TOKEN.TokenIdentifier,
		append:     boolean | 'inherit',
	) {
		super(start_node, {name: start_node.source, append})
	}
}
export class SemanticNodeCondition extends SemanticNodeEBNF {
	constructor (
		start_node: TOKEN.TokenIdentifier,
		include:    boolean,
	) {
		super(start_node, {name: start_node.source, include})
	}
}
export abstract class SemanticNodeExpr extends SemanticNodeEBNF {
}
export class SemanticNodeConst extends SemanticNodeExpr {
	constructor (start_node: TOKEN.TokenCharCode | TOKEN.TokenString | TOKEN.TokenCharClass) {
		super(start_node, {value: start_node.source})
	}
}
export class SemanticNodeRef extends SemanticNodeExpr {
	private readonly name: string;
	constructor (start_node: ParseNode, ref: TOKEN.TokenIdentifier);
	constructor (start_node: ParseNode, ref: SemanticNodeRef, args: readonly SemanticNodeArg[]);
	constructor (
		start_node: ParseNode,
		ref:        TOKEN.TokenIdentifier | SemanticNodeRef,
		args:       readonly SemanticNodeArg[] = [],
	) {
		const name_: string = (ref instanceof SemanticNodeRef) ? ref.name : ref.source
		super(
			start_node,
			{name: name_},
			(ref instanceof SemanticNodeRef) ? [ref, ...args] : args,
		)
		this.name = name_
	}
}
export class SemanticNodeItem extends SemanticNodeExpr {
	constructor (
		start_node: ParseNode,
		item:       SemanticNodeExpr,
		conditions: readonly SemanticNodeCondition[] = [],
	) {
		super(start_node, {}, [item, ...conditions])
	}
}
abstract class SemanticNodeOp extends SemanticNodeExpr {
	constructor (start_node: ParseNode, operator: string, operands: KleenePlus<SemanticNodeExpr>) {
		super(start_node, {operator}, operands)
	}
	abstract expand(nt: SemanticNodeNonterminal, data: EBNFObject[]): EBNFObject['defn'];
}
export class SemanticNodeOpUn extends SemanticNodeOp {
	constructor (
		start_node: ParseNode,
		private readonly operator: 'plus' | 'star' | 'hash' | 'opt',
		private readonly operand:  SemanticNodeExpr,
	) {
		super(start_node, operator, [operand])
	}
	expand(nt: SemanticNodeNonterminal, data: EBNFObject[]): EBNFObject['defn'] {
		const name: string = `${ nt.source }_${ nt.subCount }_List`
		return new Map<string, () => EBNFObject['defn']>([
			['plus', () => {
				data.push({
					name,
					defn: NonemptyArray_flatMap(toDefn(this.operand, nt, data), (seq) => [
						seq,
						[{prod: name}, ...seq],
					]),
				})
				return [
					[{prod: name}],
				]
			}],
			['star', () => {
				data.push({
					name,
					defn: NonemptyArray_flatMap(toDefn(this.operand, nt, data), (seq) => [
						seq,
						[{prod: name}, ...seq],
					]),
				})
				return [
					['\'\''],
					[{prod: name}],
				]
			}],
			['hash', () => {
				data.push({
					name,
					defn: NonemptyArray_flatMap(toDefn(this.operand, nt, data), (seq) => [
						seq,
						[{prod: name}, '\',\'', ...seq],
					]),
				})
				return [
					[{prod: name}],
				]
			}],
			['opt', () => {
				return [
					['\'\''],
					...toDefn(this.operand, nt, data),
				]
			}],
		]).get(this.operator)!()
	}
}
export class SemanticNodeOpBin extends SemanticNodeOp {
	constructor (
		start_node: ParseNode,
		private readonly operator: 'order' | 'concat' | 'altern',
		private readonly operand0: SemanticNodeExpr,
		private readonly operand1: SemanticNodeExpr,
	) {
		super(start_node, operator, [operand0, operand1])
	}
	expand(nt: SemanticNodeNonterminal, data: EBNFObject[]): EBNFObject['defn'] {
		return new Map<string, () => EBNFObject['defn']>([
			['order', () => NonemptyArray_flatMap(toDefn(this.operand0, nt, data), (seq0) =>
				NonemptyArray_flatMap(toDefn(this.operand1, nt, data), (seq1) => [
					[...seq0, ...seq1],
				])
			)],
			['concat', () => NonemptyArray_flatMap(toDefn(this.operand0, nt, data), (seq0) =>
				NonemptyArray_flatMap(toDefn(this.operand1, nt, data), (seq1) => [
					[...seq0, ...seq1],
					[...seq1, ...seq0],
				])
			)],
			['altern', () => [
				...toDefn(this.operand0, nt, data),
				...toDefn(this.operand1, nt, data),
			]],
		]).get(this.operator)!()
	}
}
export class SemanticNodeNonterminal extends SemanticNodeEBNF {
	/** A counter for internal sub-expressions. Used for naming automated productions. */
	private sub_count: bigint = 0n
	constructor (
		start_node: TOKEN.TokenIdentifier,
		private readonly params: readonly SemanticNodeParam[] = [],
	) {
		super(start_node, {name: start_node.source}, params)
	}
	/**
	 * Return the sub-expression count, and then increment it.
	 * @return this ConcreteNonterminal’s current sub-expression counter
	 */
	get subCount(): bigint {
		return this.sub_count++
	}
	/**
	 * Expands this nonterminal in its abstract form into a set of nonterminals with concrete parameters.
	 * E.g., expands `N<X, Y>` into `[N, N__X, N__Y, N__X__Y]`.
	 * @returns an array of objects representing nonterminals
	 */
	expand(): ConcreteNonterminal[] {
		return [...new Array(2 ** this.params.length)].map((_, count) => new ConcreteNonterminal(
			this.source,
			[...count.toString(2).padStart(this.params.length, '0')].map((d, i) =>
				[this.params[i], !!+d] as const
			).filter(([_param, b]) => !!b).map(([param, _b]) => param),
		))
	}
}
export class SemanticNodeProduction extends SemanticNodeEBNF {
	constructor (
		start_node: ParseNode,
		private readonly nonterminal: SemanticNodeNonterminal,
		private readonly definition:  SemanticNodeExpr,
	) {
		super(start_node, {}, [nonterminal, definition])
	}
	transform(): EBNFObject[] {
		const productions_data: EBNFObject[] = []
		productions_data.push(...this.nonterminal.expand().map((n) => ({
			name: n.toString(),
			defn: toDefn(this.definition, this.nonterminal, productions_data),
		})))
		return productions_data
	}
}
export class SemanticNodeGrammar extends SemanticNodeEBNF {
	constructor (
		start_node: ParseNode,
		private readonly productions: readonly SemanticNodeProduction[] = [],
	) {
		super(start_node, {}, productions)
	}
	transform(): EBNFObject[] {
		return this.productions.flatMap((prod) => prod.transform())
	}
}



class ConcreteNonterminal {
	constructor(
		private name: string,
		private suffixes: SemanticNodeParam[],
	) {
	}
	toString(): string {
		return `${ this.name }${ this.suffixes.map((s) => `__${ s.source }`).join('') }`
	}
}
