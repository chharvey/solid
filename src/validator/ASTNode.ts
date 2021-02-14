import {
	Token,
	ParseNode,
	ASTNode,
} from '@chharvey/parser';
import * as xjs from 'extrajs'

import type {
	NonemptyArray,
} from '../types.d';
import {
	memoizeMethod,
} from '../decorators';
import {
	Operator,
	ValidTypeOperator,
	ValidOperatorUnary,
	ValidOperatorBinary,
	ValidOperatorArithmetic,
	ValidOperatorComparative,
	ValidOperatorEquality,
	ValidOperatorLogical,
} from '../enum/Operator.enum'
import {
	Validator,
} from './Validator';
import {
	SymbolKind,
	SymbolStructure,
	SymbolStructureVar,
	SymbolStructureType,
} from './SymbolStructure';
import {
	SolidLanguageType,
	SolidTypeConstant,
} from './SolidLanguageType';
import {SolidObject}  from './SolidObject';
import {SolidNull}    from './SolidNull';
import {SolidBoolean} from './SolidBoolean';
import {SolidNumber}  from './SolidNumber';
import {Int16}        from './Int16';
import {Float64}      from './Float64';
import {SolidString}  from './SolidString';
import {
	Builder,
	Instruction,
	InstructionNone,
	InstructionExpression,
	InstructionConst,
	InstructionUnop,
	InstructionBinopArithmetic,
	InstructionBinopComparative,
	InstructionBinopEquality,
	InstructionBinopLogical,
	InstructionCond,
	InstructionStatement,
	InstructionModule,
} from '../builder/'
import {
	ReferenceError01,
	ReferenceError03,
	AssignmentError01,
	AssignmentError10,
	TypeError01,
	TypeError03,
	NanError01,
	NanError02,
} from '../error/';
import {
	Keyword,
	CookValueType,
	TOKEN,
	PARSER,
} from '../parser/';



function bothNumeric(t0: SolidLanguageType, t1: SolidLanguageType): boolean {
	return t0.isSubtypeOf(SolidNumber) && t1.isSubtypeOf(SolidNumber)
}
function eitherFloats(t0: SolidLanguageType, t1: SolidLanguageType): boolean {
	return t0.isSubtypeOf(Float64) || t1.isSubtypeOf(Float64)
}
function bothFloats(t0: SolidLanguageType, t1: SolidLanguageType): boolean {
	return t0.isSubtypeOf(Float64) && t1.isSubtypeOf(Float64)
}
function neitherFloats(t0: SolidLanguageType, t1: SolidLanguageType): boolean {
	return !eitherFloats(t0, t1)
}
function oneFloats(t0: SolidLanguageType, t1: SolidLanguageType): boolean {
	return !neitherFloats(t0, t1) && !bothFloats(t0, t1)
}



export abstract class ASTNodeSolid extends ASTNode {
	/**
	 * Construct a new ASTNodeSolid object.
	 *
	 * @param start_node - The initial node in the parse tree to which this ASTNode corresponds.
	 * @param children   - The set of child inputs that creates this ASTNode.
	 * @param attributes - Any other attributes to attach.
	 */
	constructor(
		start_node: Token|ParseNode,
		attributes: {[key: string]: CookValueType | SolidObject} = {},
		children: readonly ASTNodeSolid[] = [],
	) {
		super(start_node, attributes, children)
	}

	/**
	 * Perform definite assignment phase of semantic analysis:
	 * - Check that all variables have been assigned before being used.
	 * - Check that no varaible is declared more than once.
	 * - Check that fixed variables are not reassigned.
	 * @param validator a record of declared variable symbols
	 */
	abstract varCheck(validator?: Validator): void;

	/**
	 * Type-check the node as part of semantic analysis.
	 * @param validator stores validation information
	 */
	abstract typeCheck(validator?: Validator): void;

	/**
	 * Give directions to the runtime code builder.
	 * @param builder the builder to direct
	 * @return the directions to print
	 */
	abstract build(builder: Builder): Instruction;
}



export class ASTNodeKey extends ASTNodeSolid {
	declare children: readonly [];
	readonly id: bigint;
	constructor (start_node: TOKEN.TokenKeyword | TOKEN.TokenIdentifier) {
		super(start_node, {id: start_node.cook()});
		this.id = start_node.cook()!;
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeKey#varCheck not yet supported.';
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeKey#typeCheck not yet supported.';
	}
	/** @implements ASTNodeSolid */
	build(builder: Builder): Instruction {
		throw builder && 'ASTNodeKey#build not yet supported.';
	}
}
export class ASTNodeTypeProperty extends ASTNodeSolid {
	constructor (
		start_node: PARSER.ParseNodeTypeProperty,
		readonly children: readonly [ASTNodeKey, ASTNodeType],
	) {
		super(start_node, {}, children);
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeTypeProperty#varCheck not yet supported.';
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeTypeProperty#typeCheck not yet supported.';
	}
	/** @implements ASTNodeSolid */
	build(builder: Builder): Instruction {
		throw builder && 'ASTNodeTypeProperty#build not yet supported.';
	}
}
/**
 * A sematic node representing a type.
 * Known subclasses:
 * - ASTNodeTypeConstant
 * - ASTNodeTypeAlias
 * - ASTNodeTypeEmptyCollection
 * - ASTNodeTypeList
 * - ASTNodeTypeRecord
 * - ASTNodeTypeOperation
 */
export abstract class ASTNodeType extends ASTNodeSolid {
	/** @implements ASTNodeSolid */
	varCheck(_validator: Validator = new Validator()): void {
		return; // for now, there are no type variables // TODO: dereferencing type variables
	}
	/**
	 * @implements ASTNodeSolid
	 * @final
	 */
	typeCheck(_validator: Validator = new Validator()): void {
		return; // no type-checking necessary for types
	}
	/**
	 * @implements ASTNodeSolid
	 * @final
	 */
	build(_builder: Builder): InstructionNone {
		return new InstructionNone()
	}
	/**
	 * Assess the type-value of this node at compile-time.
	 * @param validator a record of declared variable symbols
	 * @returns the computed type-value of this node
	 */
	abstract assess(validator?: Validator): SolidLanguageType;
}
export class ASTNodeTypeConstant extends ASTNodeType {
	declare children:
		| readonly []
	readonly value: SolidLanguageType;
	constructor (start_node: TOKEN.TokenKeyword | TOKEN.TokenNumber | TOKEN.TokenString) {
		const value: SolidLanguageType =
			(start_node instanceof TOKEN.TokenKeyword) ?
				(start_node.source === Keyword.BOOL)  ? SolidBoolean :
				(start_node.source === Keyword.FALSE) ? SolidBoolean.FALSETYPE :
				(start_node.source === Keyword.TRUE ) ? SolidBoolean.TRUETYPE :
				(start_node.source === Keyword.INT)   ? Int16 :
				(start_node.source === Keyword.FLOAT) ? Float64 :
				(start_node.source === Keyword.OBJ)   ? SolidObject :
				SolidNull
			: (start_node instanceof TOKEN.TokenNumber) ?
				new SolidTypeConstant(
					start_node.isFloat
						? new Float64(start_node.cook())
						: new Int16(BigInt(start_node.cook()))
				)
			: SolidString
		super(start_node, {value: value.toString()})
		this.value = value
	}
	/** @implements ASTNodeSolid */
	varCheck(_validator: Validator = new Validator()): void {
		return; // no validation necessary for constants
	}
	/** @implements ASTNodeType */
	@memoizeMethod
	assess(_validator: Validator = new Validator()): SolidLanguageType {
		return this.value
	}
}
export class ASTNodeTypeAlias extends ASTNodeType {
	declare children:
		| readonly []
	readonly id: bigint;
	constructor (start_node: TOKEN.TokenIdentifier) {
		super(start_node, {id: start_node.cook()})
		this.id = start_node.cook()!;
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		if (!validator.hasSymbol(this.id)) {
			throw new ReferenceError01(this);
		};
		if (validator.getSymbolInfo(this.id)! instanceof SymbolStructureVar) {
			throw new ReferenceError03(this, SymbolKind.VALUE, SymbolKind.TYPE);
		};
	}
	/** @implements ASTNodeType */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidLanguageType {
		if (validator.hasSymbol(this.id)) {
			const symbol: SymbolStructure = validator.getSymbolInfo(this.id)!;
			if (symbol instanceof SymbolStructureType) {
				return symbol.defn.assess(validator);
			};
		};
		return SolidLanguageType.UNKNOWN;
	}
}
export class ASTNodeTypeEmptyCollection extends ASTNodeType {
	declare children: readonly [];
	constructor (
		start_node: PARSER.ParseNodeTypeUnit,
	) {
		super(start_node);
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeTypeEmptyCollection#varCheck not yet supported.';
	}
	/** @implements ASTNodeType */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidLanguageType {
		throw validator && 'ASTNodeTypeEmptyCollection#assess not yet supported.';
	}
}
export class ASTNodeTypeList extends ASTNodeType {
	constructor (
		start_node: PARSER.ParseNodeTypeTupleLiteral,
		readonly children: readonly ASTNodeType[],
	) {
		super(start_node, {}, children);
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeTypeList#varCheck not yet supported.';
	}
	/** @implements ASTNodeType */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidLanguageType {
		throw validator && 'ASTNodeTypeList#assess not yet supported.';
	}
}
export class ASTNodeTypeRecord extends ASTNodeType {
	constructor (
		start_node: PARSER.ParseNodeTypeRecordLiteral,
		readonly children: readonly ASTNodeTypeProperty[],
	) {
		super(start_node, {}, children);
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeTypeRecord#varCheck not yet supported.';
	}
	/** @implements ASTNodeType */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidLanguageType {
		throw validator && 'ASTNodeTypeRecord#assess not yet supported.';
	}
}
export abstract class ASTNodeTypeOperation extends ASTNodeType {
	constructor (
		start_node: ParseNode,
		readonly operator: ValidTypeOperator,
		readonly children:
			| readonly ASTNodeType[]
	) {
		super(start_node, {operator}, children)
	}
	/**
	 * @implements ASTNodeSolid
	 * @final
	 */
	varCheck(validator: Validator = new Validator()): void {
		return this.children.forEach((c) => c.varCheck(validator));
	}
}
export class ASTNodeTypeOperationUnary extends ASTNodeTypeOperation {
	constructor (
		start_node: ParseNode,
		operator: ValidTypeOperator,
		readonly children:
			| readonly [ASTNodeType]
	) {
		super(start_node, operator, children)
	}
	/** @implements ASTNodeType */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidLanguageType {
		return (this.operator === Operator.ORNULL)
			? this.children[0].assess(validator).union(SolidNull)
			: (() => { throw new Error(`Operator ${ Operator[this.operator] } not found.`) })()
	}
}
export class ASTNodeTypeOperationBinary extends ASTNodeTypeOperation {
	constructor (
		start_node: ParseNode,
		operator: ValidTypeOperator,
		readonly children:
			| readonly [ASTNodeType, ASTNodeType]
	) {
		super(start_node, operator, children)
	}
	/** @implements ASTNodeType */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidLanguageType {
		return (
			(this.operator === Operator.AND) ? this.children[0].assess(validator).intersect(this.children[1].assess(validator)) :
			(this.operator === Operator.OR)  ? this.children[0].assess(validator).union    (this.children[1].assess(validator)) :
			(() => { throw new Error(`Operator ${ Operator[this.operator] } not found.`) })()
		)
	}
}
export class ASTNodeProperty extends ASTNodeSolid {
	constructor (
		start_node: PARSER.ParseNodeProperty,
		readonly children: readonly [ASTNodeKey, ASTNodeExpression],
	) {
		super(start_node, {}, children);
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeProperty#varCheck not yet supported.';
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeProperty#typeCheck not yet supported.';
	}
	/** @implements ASTNodeSolid */
	build(builder: Builder): Instruction {
		throw builder && 'ASTNodeProperty#build not yet supported.';
	}
}
export class ASTNodeCase extends ASTNodeSolid {
	constructor (
		start_node: PARSER.ParseNodeCase,
		readonly children: NonemptyArray<ASTNodeExpression>,
	) {
		super(start_node, {}, children);
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeCase#varCheck not yet supported.';
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeCase#typeCheck not yet supported.';
	}
	/** @implements ASTNodeSolid */
	build(builder: Builder): Instruction {
		throw builder && 'ASTNodeCase#build not yet supported.';
	}
}
/**
 * A sematic node representing an expression.
 * Known subclasses:
 * - ASTNodeConstant
 * - ASTNodeVariable
 * - ASTNodeTemplate
 * - ASTNodeEmptyCollection
 * - ASTNodeList
 * - ASTNodeRecord
 * - ASTNodeMapping
 * - ASTNodeOperation
 */
export abstract class ASTNodeExpression extends ASTNodeSolid {
	/**
	 * Decorator for {@link ASTNodeExpression#type} method and any overrides.
	 * Type-checks and re-throws any type errors first,
	 * then computes assessed value (if applicable), and if successful,
	 * returns a constant type equal to that assessed value.
	 * @param   _prototype    the prototype that has the method to be decorated
	 * @param   _property_key the name of the method to be decorated
	 * @param   descriptor    the Property Descriptor of the prototype’s method
	 * @returns               `descriptor`, with a new value that is the decorated method
	 */
	protected static typeDeco(
		_prototype: ASTNodeExpression,
		_property_key: string,
		descriptor: TypedPropertyDescriptor<(this: ASTNodeExpression, validator?: Validator) => SolidLanguageType>,
	): typeof descriptor {
		const method = descriptor.value!;
		descriptor.value = function (validator = new Validator()) {
			const typ: SolidLanguageType = method.call(this, validator); // type-check first, to re-throw any TypeErrors
			if (validator.config.compilerOptions.constantFolding) {
				const assessed: SolidObject | null = this.assess(validator);
				if (!!assessed) {
					return new SolidTypeConstant(assessed);
				}
			}
			return typ;
		};
		return descriptor;
	}
	/**
	 * Decorator for {@link ASTNodeExpression#build} method and any overrides.
	 * First tries to compute the assessed value, and if successful, builds the assessed value.
	 * Otherwise builds this node.
	 * @param   _prototype    the prototype that has the method to be decorated
	 * @param   _property_key the name of the method to be decorated
	 * @param   descriptor    the Property Descriptor of the prototype’s method
	 * @returns               `descriptor`, with a new value that is the decorated method
	 */
	protected static buildDeco<T extends InstructionExpression>(
		_prototype: ASTNodeExpression,
		_property_key: string,
		descriptor: TypedPropertyDescriptor<(this: ASTNodeExpression, builder: Builder, to_float?: boolean) => InstructionConst | T>,
	): typeof descriptor {
		const method = descriptor.value!;
		descriptor.value = function (builder, to_float = false) {
			const assessed: SolidObject | null = (builder.config.compilerOptions.constantFolding) ? this.assess(builder.validator) : null;
			return (!!assessed) ? InstructionConst.fromAssessment(assessed, to_float) : method.call(this, builder, to_float);
		};
		return descriptor;
	}
	/**
	 * Determine whether this expression should build to a float-type instruction.
	 * @return Should the built instruction be type-coerced into a floating-point number?
	 */
	abstract get shouldFloat(): boolean;
	/**
	 * @implements ASTNodeSolid
	 * @final
	 */
	typeCheck(validator: Validator = new Validator()): void {
		this.type(validator); // assert does not throw
	}
	/**
	 * @overrides ASTNodeSolid
	 * @param to_float Should the returned instruction be type-coerced into a floating-point number?
	 */
	abstract build(builder: Builder, to_float?: boolean): InstructionExpression;
	/**
	 * The Type of this expression.
	 * @param validator stores validation and configuration information
	 * @return the compile-time type of this node
	 */
	abstract type(validator?: Validator): SolidLanguageType;
	/**
	 * Assess the value of this node at compile-time, if possible.
	 * If {@link SolidConfig|constant folding} is off, this should not be called.
	 * @param validator stores validation and configuration information
	 * @return the computed value of this node, or an abrupt completion if the value cannot be computed by the compiler
	 */
	abstract assess(validator?: Validator): SolidObject | null;
}
export class ASTNodeConstant extends ASTNodeExpression {
	declare children:
		| readonly []
	readonly value: string | SolidObject;
	constructor (start_node: TOKEN.TokenKeyword | TOKEN.TokenNumber | TOKEN.TokenString | TOKEN.TokenTemplate) {
		const value: string | SolidObject =
			(start_node instanceof TOKEN.TokenKeyword) ?
				(start_node.source === Keyword.FALSE) ? SolidBoolean.FALSE :
				(start_node.source === Keyword.TRUE ) ? SolidBoolean.TRUE  :
				SolidNull.NULL
			:
			(start_node instanceof TOKEN.TokenNumber) ?
				start_node.isFloat ? new Float64(start_node.cook()) : new Int16(BigInt(start_node.cook()))
			:
			start_node.cook()
		super(start_node, {value})
		this.value = value
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		return this.value instanceof Float64
	}
	/** @implements ASTNodeSolid */
	varCheck(_validator: Validator = new Validator()): void {
		return; // no validation necessary for constants
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder, to_float: boolean = false): InstructionConst {
		return InstructionConst.fromAssessment(this.assess(builder.validator), to_float);
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(validator: Validator = new Validator()): SolidLanguageType {
		// No need to call `this.assess(validator)` and then unwrap again; just use `this.value`.
		return (validator.config.compilerOptions.constantFolding && (
			this.value instanceof SolidNull ||
			this.value instanceof SolidBoolean ||
			this.value instanceof SolidNumber
		)) ? new SolidTypeConstant(this.value) :
		(this.value instanceof SolidNull)    ? SolidNull :
		(this.value instanceof SolidBoolean) ? SolidBoolean :
		(this.value instanceof Int16)        ? Int16 :
		(this.value instanceof Float64)      ? Float64 :
		SolidString
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(_validator: Validator = new Validator()): SolidObject {
		if (this.value instanceof SolidObject) {
			return this.value;
		} else {
			throw new Error('ASTNodeConstant[value:string]#assess not yet supported.')
		}
	}
}
export class ASTNodeVariable extends ASTNodeExpression {
	declare children:
		| readonly []
	readonly id: bigint;
	constructor (start_node: TOKEN.TokenIdentifier) {
		super(start_node, {id: start_node.cook()})
		this.id = start_node.cook()!;
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		return this.type().isSubtypeOf(Float64);
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		if (!validator.hasSymbol(this.id)) {
			throw new ReferenceError01(this);
		};
		if (validator.getSymbolInfo(this.id)! instanceof SymbolStructureType) {
			throw new ReferenceError03(this, SymbolKind.TYPE, SymbolKind.VALUE);
			// TODO: When Type objects are allowed as runtime values, this should be removed and checked by the type checker (`this#typeCheck`).
		};
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(_builder: Builder): InstructionExpression {
		throw new Error('ASTNodeVariable#build not yet supported.');
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(validator: Validator = new Validator()): SolidLanguageType {
		if (validator.hasSymbol(this.id)) {
			const symbol: SymbolStructure = validator.getSymbolInfo(this.id)!;
			if (symbol instanceof SymbolStructureVar) {
				return symbol.type;
			};
		};
		return SolidLanguageType.UNKNOWN;
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		if (validator.hasSymbol(this.id)) {
			const symbol: SymbolStructure = validator.getSymbolInfo(this.id)!;
			if (symbol instanceof SymbolStructureVar && !symbol.unfixed) {
				return symbol.defn!.assess(validator);
			};
		};
		return null;
	}
}
export class ASTNodeTemplate extends ASTNodeExpression {
	constructor(
		start_node: ParseNode,
		readonly children: // FIXME spread types
			| readonly [ASTNodeConstant]
			| readonly [ASTNodeConstant,                                                           ASTNodeConstant]
			| readonly [ASTNodeConstant, ASTNodeExpression,                                        ASTNodeConstant]
			// | readonly [ASTNodeConstant,                    ...ASTNodeTemplatePartialChildrenType, ASTNodeConstant]
			// | readonly [ASTNodeConstant, ASTNodeExpression, ...ASTNodeTemplatePartialChildrenType, ASTNodeConstant]
			| readonly ASTNodeExpression[]
	) {
		super(start_node, {}, children)
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		throw new Error('ASTNodeTemplate#shouldFloat not yet supported.');
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		return this.children.forEach((c) => c.varCheck(validator));
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(_builder: Builder): InstructionExpression {
		throw new Error('ASTNodeTemplate#build not yet supported.');
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(_validator: Validator = new Validator()): SolidLanguageType {
		return SolidString
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		throw validator && new Error('ASTNodeTemplate#assess not yet supported.');
	}
}
export class ASTNodeEmptyCollection extends ASTNodeExpression {
	declare children: readonly [];
	constructor (start_node: PARSER.ParseNodeExpressionUnit) {
		super(start_node);
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		throw 'ASTNodeEmptyCollection#shouldFloat not yet supported.';
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeEmptyCollection#varCheck not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder): InstructionExpression {
		throw builder && 'ASTNodeEmptyCollection#build not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(validator: Validator = new Validator()): SolidLanguageType {
		throw validator && 'ASTNodeEmptyCollection#type not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		throw validator && 'ASTNodeEmptyCollection#assess not yet supported.';
	}
}
export class ASTNodeList extends ASTNodeExpression {
	constructor (
		start_node: PARSER.ParseNodeListLiteral,
		readonly children: readonly ASTNodeExpression[],
	) {
		super(start_node, {}, children);
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		throw 'ASTNodeList#shouldFloat not yet supported.';
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeList#varCheck not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder): InstructionExpression {
		throw builder && 'ASTNodeList#build not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(validator: Validator = new Validator()): SolidLanguageType {
		throw validator && 'ASTNodeList#type not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		throw validator && 'ASTNodeList#assess not yet supported.';
	}
}
export class ASTNodeRecord extends ASTNodeExpression {
	constructor (
		start_node: PARSER.ParseNodeRecordLiteral,
		readonly children: readonly ASTNodeProperty[],
	) {
		super(start_node, {}, children);
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		throw 'ASTNodeRecord#shouldFloat not yet supported.';
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeRecord#varCheck not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder): InstructionExpression {
		throw builder && 'ASTNodeRecord#build not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(validator: Validator = new Validator()): SolidLanguageType {
		throw validator && 'ASTNodeRecord#type not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		throw validator && 'ASTNodeRecord#assess not yet supported.';
	}
}
export class ASTNodeMapping extends ASTNodeExpression {
	constructor (
		start_node: PARSER.ParseNodeMappingLiteral,
		readonly children: readonly ASTNodeCase[],
	) {
		super(start_node, {}, children);
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		throw 'ASTNodeMapping#shouldFloat not yet supported.';
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		throw validator && 'ASTNodeMapping#varCheck not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder): InstructionExpression {
		throw builder && 'ASTNodeMapping#build not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(validator: Validator = new Validator()): SolidLanguageType {
		throw validator && 'ASTNodeMapping#type not yet supported.';
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		throw validator && 'ASTNodeMapping#assess not yet supported.';
	}
}
export abstract class ASTNodeOperation extends ASTNodeExpression {
	/** @override */
	readonly tagname: string = 'Operation' // TODO remove after refactoring tests using `#serialize`
	constructor(
		start_node: ParseNode,
		operator: Operator,
		readonly children:
			| readonly ASTNodeExpression[]
	) {
		super(start_node, {operator}, children)
	}
	/**
	 * @implements ASTNodeSolid
	 * @final
	 */
	varCheck(validator: Validator = new Validator()): void {
		return this.children.forEach((c) => c.varCheck(validator));
	}
}
export class ASTNodeOperationUnary extends ASTNodeOperation {
	constructor(
		start_node: ParseNode,
		readonly operator: ValidOperatorUnary,
		readonly children:
			| readonly [ASTNodeExpression]
	) {
		super(start_node, operator, children)
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		return this.children[0].shouldFloat
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder, to_float: boolean = false): InstructionConst | InstructionUnop {
		const tofloat: boolean = to_float || this.shouldFloat
		return new InstructionUnop(
			this.operator,
			this.children[0].build(builder, tofloat),
		)
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(validator: Validator = new Validator()): SolidLanguageType {
		if ([Operator.NOT, Operator.EMP].includes(this.operator)) {
			return SolidBoolean
		}
		const t0: SolidLanguageType = this.children[0].type(validator);
		return (t0.isSubtypeOf(SolidNumber)) ? t0 : (() => { throw new TypeError01(this) })()
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		const assess0: SolidObject | null = this.children[0].assess(validator);
		if (!assess0) {
			return assess0
		}
		const v0: SolidObject = assess0;
		return (
			(this.operator === Operator.NOT) ? v0.isTruthy.not :
			(this.operator === Operator.EMP) ? v0.isTruthy.not.or(SolidBoolean.fromBoolean(v0 instanceof SolidNumber && v0.eq0())) :
			(this.operator === Operator.NEG) ? this.foldNumeric(v0 as SolidNumber<any>) :
			(() => { throw new ReferenceError(`Operator ${ Operator[this.operator] } not found.`) })()
		)
	}
	private foldNumeric<T extends SolidNumber<T>>(z: T): T {
		try {
			return new Map<Operator, (z: T) => T>([
				[Operator.AFF, (z) => z],
				[Operator.NEG, (z) => z.neg()],
			]).get(this.operator)!(z)
		} catch (err) {
			if (err instanceof xjs.NaNError) {
				throw new NanError01(this)
			} else {
				throw err
			}
		}
	}
}
export abstract class ASTNodeOperationBinary extends ASTNodeOperation {
	constructor(
		start_node: ParseNode,
		readonly operator: ValidOperatorBinary,
		readonly children:
			| readonly [ASTNodeExpression, ASTNodeExpression]
	) {
		super(start_node, operator, children)
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		return this.children[0].shouldFloat || this.children[1].shouldFloat
	}
	/**
	 * @implements ASTNodeExpression
	 * @final
	 */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(validator: Validator = new Validator()): SolidLanguageType {
		return this.type_do(
			this.children[0].type(validator),
			this.children[1].type(validator),
			validator.config.compilerOptions.intCoercion,
		)
	}
	protected abstract type_do(t0: SolidLanguageType, t1: SolidLanguageType, int_coercion: boolean): SolidLanguageType;
}
export class ASTNodeOperationBinaryArithmetic extends ASTNodeOperationBinary {
	constructor (
		start_node: ParseNode,
		readonly operator: ValidOperatorArithmetic,
		children: readonly [ASTNodeExpression, ASTNodeExpression]
	) {
		super(start_node, operator, children)
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder, to_float: boolean = false): InstructionConst | InstructionBinopArithmetic {
		const tofloat: boolean = to_float || this.shouldFloat
		return new InstructionBinopArithmetic(
			this.operator,
			this.children[0].build(builder, tofloat),
			this.children[1].build(builder, tofloat),
		)
	}
	/** @implements ASTNodeOperationBinary */
	protected type_do(t0: SolidLanguageType, t1: SolidLanguageType, int_coercion: boolean): SolidLanguageType {
		if (bothNumeric(t0, t1)) {
			if (int_coercion) {
				return (eitherFloats(t0, t1)) ? Float64 : Int16
			}
			if (bothFloats   (t0, t1)) { return Float64 }
			if (neitherFloats(t0, t1)) { return Int16 }
		}
		throw new TypeError01(this)
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		const assess0: SolidObject | null = this.children[0].assess(validator);
		if (!assess0) {
			return assess0
		}
		const assess1: SolidObject | null = this.children[1].assess(validator);
		if (!assess1) {
			return assess1
		}
		const [v0, v1]: [SolidObject, SolidObject] = [assess0, assess1];
		if (this.operator === Operator.DIV && v1 instanceof SolidNumber && v1.eq0()) {
			throw new NanError02(this.children[1])
		}
		if (!(v0 instanceof SolidNumber) || !(v1 instanceof SolidNumber)) {
			// using an internal TypeError, not a SolidTypeError, as it should already be valid per `this#type`
			throw new TypeError('Both operands must be of type `SolidNumber`.')
		}
		return (
			(v0 instanceof Int16 && v1 instanceof Int16)
				? this.foldNumeric(v0, v1)
				: this.foldNumeric(
					(v0 as SolidNumber).toFloat(),
					(v1 as SolidNumber).toFloat(),
				)
		)
	}
	private foldNumeric<T extends SolidNumber<T>>(x: T, y: T): T {
		try {
			return new Map<Operator, (x: T, y: T) => T>([
				[Operator.EXP, (x, y) => x.exp(y)],
				[Operator.MUL, (x, y) => x.times(y)],
				[Operator.DIV, (x, y) => x.divide(y)],
				[Operator.ADD, (x, y) => x.plus(y)],
				// [Operator.SUB, (x, y) => x.minus(y)],
			]).get(this.operator)!(x, y)
		} catch (err) {
			if (err instanceof xjs.NaNError) {
				throw new NanError01(this)
			} else {
				throw err
			}
		}
	}
}
export class ASTNodeOperationBinaryComparative extends ASTNodeOperationBinary {
	constructor (
		start_node: ParseNode,
		readonly operator: ValidOperatorComparative,
		children: readonly [ASTNodeExpression, ASTNodeExpression]
	) {
		super(start_node, operator, children)
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder, to_float: boolean = false): InstructionConst | InstructionBinopComparative {
		const tofloat: boolean = to_float || this.shouldFloat
		return new InstructionBinopComparative(
			this.operator,
			this.children[0].build(builder, tofloat),
			this.children[1].build(builder, tofloat),
		)
	}
	/** @implements ASTNodeOperationBinary */
	protected type_do(t0: SolidLanguageType, t1: SolidLanguageType, int_coercion: boolean): SolidLanguageType {
		if (bothNumeric(t0, t1) && (int_coercion || (
			bothFloats(t0, t1) || neitherFloats(t0, t1)
		))) {
			return SolidBoolean
		}
		throw new TypeError01(this)
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		const assess0: SolidObject | null = this.children[0].assess(validator);
		if (!assess0) {
			return assess0
		}
		const assess1: SolidObject | null = this.children[1].assess(validator);
		if (!assess1) {
			return assess1
		}
		const [v0, v1]: [SolidObject, SolidObject] = [assess0, assess1];
		if (!(v0 instanceof SolidNumber) || !(v1 instanceof SolidNumber)) {
			// using an internal TypeError, not a SolidTypeError, as it should already be valid per `this#type`
			throw new TypeError('Both operands must be of type `SolidNumber`.')
		}
		return (
			(v0 instanceof Int16 && v1 instanceof Int16)
				? this.foldComparative(v0, v1)
				: this.foldComparative(
					(v0 as SolidNumber).toFloat(),
					(v1 as SolidNumber).toFloat(),
				)
		)
	}
	private foldComparative<T extends SolidNumber<T>>(x: T, y: T): SolidBoolean {
		return SolidBoolean.fromBoolean(new Map<Operator, (x: T, y: T) => boolean>([
			[Operator.LT, (x, y) => x.lt(y)],
			[Operator.GT, (x, y) => y.lt(x)],
			[Operator.LE, (x, y) => x.equal(y) || x.lt(y)],
			[Operator.GE, (x, y) => x.equal(y) || y.lt(x)],
			// [Operator.NLT, (x, y) => !x.lt(y)],
			// [Operator.NGT, (x, y) => !y.lt(x)],
		]).get(this.operator)!(x, y))
	}
}
export class ASTNodeOperationBinaryEquality extends ASTNodeOperationBinary {
	constructor (
		start_node: ParseNode,
		readonly operator: ValidOperatorEquality,
		children: readonly [ASTNodeExpression, ASTNodeExpression]
	) {
		super(start_node, operator, children)
	}
	/** @override */
	get shouldFloat(): boolean {
		return this.operator === Operator.EQ && super.shouldFloat
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder, _to_float: boolean = false): InstructionConst | InstructionBinopEquality {
		const tofloat: boolean = builder.config.compilerOptions.intCoercion && this.shouldFloat
		return new InstructionBinopEquality(
			this.operator,
			this.children[0].build(builder, tofloat),
			this.children[1].build(builder, tofloat),
		)
	}
	/** @implements ASTNodeOperationBinary */
	protected type_do(t0: SolidLanguageType, t1: SolidLanguageType, int_coercion: boolean): SolidLanguageType {
		// If `a` and `b` are of disjoint numeric types, then `a is b` will always return `false`.
		// If `a` and `b` are of disjoint numeric types, then `a == b` will return `false` when `intCoercion` is off.
		if (bothNumeric(t0, t1)) {
			if (oneFloats(t0, t1) && (this.operator === Operator.IS || !int_coercion)) {
				return SolidBoolean.FALSETYPE
			}
			return SolidBoolean
		}
		if (t0.intersect(t1).isEmpty) {
			return SolidBoolean.FALSETYPE
		}
		return SolidBoolean
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		const assess0: SolidObject | null = this.children[0].assess(validator);
		if (!assess0) {
			return assess0
		}
		const assess1: SolidObject | null = this.children[1].assess(validator);
		if (!assess1) {
			return assess1
		}
		const [v0, v1]: [SolidObject, SolidObject] = [assess0, assess1];
		return this.foldEquality(v0, v1);
	}
	private foldEquality(x: SolidObject, y: SolidObject): SolidBoolean {
		return SolidBoolean.fromBoolean(new Map<Operator, (x: SolidObject, y: SolidObject) => boolean>([
			[Operator.IS, (x, y) => x.identical(y)],
			[Operator.EQ, (x, y) => x.equal(y)],
			// [Operator.ISNT, (x, y) => !x.identical(y)],
			// [Operator.NEQ,  (x, y) => !x.equal(y)],
		]).get(this.operator)!(x, y))
	}
}
export class ASTNodeOperationBinaryLogical extends ASTNodeOperationBinary {
	constructor (
		start_node: ParseNode,
		readonly operator: ValidOperatorLogical,
		children: readonly [ASTNodeExpression, ASTNodeExpression]
	) {
		super(start_node, operator, children)
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder, to_float: boolean = false): InstructionConst | InstructionBinopLogical {
		const tofloat: boolean = to_float || this.shouldFloat
		return new InstructionBinopLogical(
			builder.varCount,
			this.operator,
			this.children[0].build(builder, tofloat),
			this.children[1].build(builder, tofloat),
		)
	}
	/** @implements ASTNodeOperationBinary */
	protected type_do(t0: SolidLanguageType, t1: SolidLanguageType, _int_coercion: boolean): SolidLanguageType {
		const null_union_false: SolidLanguageType = SolidNull.union(SolidBoolean.FALSETYPE);
		function truthifyType(t: SolidLanguageType): SolidLanguageType {
			const values: Set<SolidObject> = new Set(t.values);
			values.delete(SolidNull.NULL);
			values.delete(SolidBoolean.FALSE);
			return [...values].map<SolidLanguageType>((v) => new SolidTypeConstant(v)).reduce((a, b) => a.union(b));
		}
		return (this.operator === Operator.AND)
			? (t0.isSubtypeOf(null_union_false))
				? t0
				: (t0.includes(SolidNull.NULL))
					? (t0.includes(SolidBoolean.FALSE))
						? null_union_false.union(t1)
						: SolidNull.union(t1)
					: (t0.includes(SolidBoolean.FALSE))
						? SolidBoolean.FALSETYPE.union(t1)
						: t1
			: (t0.isSubtypeOf(null_union_false))
				? t1
				: (t0.includes(SolidNull.NULL) || t0.includes(SolidBoolean.FALSE))
					? truthifyType(t0).union(t1)
					: t0
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		const assess0: SolidObject | null = this.children[0].assess(validator);
		if (!assess0) {
			return assess0
		}
		const v0: SolidObject = assess0;
		if (
			this.operator === Operator.AND && !v0.isTruthy.value ||
			this.operator === Operator.OR  &&  v0.isTruthy.value
		) {
			return v0;
		}
		return this.children[1].assess(validator);
	}
}
export class ASTNodeOperationTernary extends ASTNodeOperation {
	constructor(
		start_node: ParseNode,
		readonly operator: Operator.COND,
		readonly children:
			| readonly [ASTNodeExpression, ASTNodeExpression, ASTNodeExpression]
	) {
		super(start_node, operator, children)
	}
	/** @implements ASTNodeExpression */
	get shouldFloat(): boolean {
		return this.children[1].shouldFloat || this.children[2].shouldFloat
	}
	/** @implements ASTNodeExpression */
	@ASTNodeExpression.buildDeco
	build(builder: Builder, to_float: boolean = false): InstructionConst | InstructionCond {
		const tofloat: boolean = to_float || this.shouldFloat
		return new InstructionCond(
			this.children[0].build(builder, false),
			this.children[1].build(builder, tofloat),
			this.children[2].build(builder, tofloat),
		)
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	@ASTNodeExpression.typeDeco
	type(validator: Validator = new Validator()): SolidLanguageType {
		// If `a` is of type `false`, then `typeof (if a then b else c)` is `typeof c`.
		// If `a` is of type `true`,  then `typeof (if a then b else c)` is `typeof b`.
		const t0: SolidLanguageType = this.children[0].type(validator);
		const t1: SolidLanguageType = this.children[1].type(validator);
		const t2: SolidLanguageType = this.children[2].type(validator);
		return (t0.isSubtypeOf(SolidBoolean))
			? (t0 instanceof SolidTypeConstant)
				? (t0.value === SolidBoolean.FALSE) ? t2 : t1
				: t1.union(t2)
			: (() => { throw new TypeError01(this) })()
	}
	/** @implements ASTNodeExpression */
	@memoizeMethod
	assess(validator: Validator = new Validator()): SolidObject | null {
		const assess0: SolidObject | null = this.children[0].assess(validator);
		if (!assess0) {
			return assess0
		}
		return (assess0 === SolidBoolean.TRUE)
			? this.children[1].assess(validator)
			: this.children[2].assess(validator)
	}
}
/**
 * A sematic node representing a statement.
 * There are 3 known subclasses:
 * - ASTNodeStatementExpression
 * - ASTNodeDeclaration
 * - ASTNodeAssignment
 */
export type ASTNodeStatement =
	| ASTNodeStatementExpression
	| ASTNodeDeclaration
	| ASTNodeAssignment
export class ASTNodeStatementExpression extends ASTNodeSolid {
	constructor(
		start_node: ParseNode,
		readonly children:
			| readonly []
			| readonly [ASTNodeExpression]
	) {
		super(start_node, {}, children)
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		return this.children.forEach((c) => c.varCheck(validator));
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		return this.children[0]?.typeCheck(validator);
	}
	/** @implements ASTNodeSolid */
	build(builder: Builder): InstructionNone | InstructionStatement {
		return (!this.children.length)
			? new InstructionNone()
			: new InstructionStatement(builder.stmtCount, this.children[0].build(builder))
	}
}
/**
 * A sematic node representing a declaration.
 * There are 2 known subclasses:
 * - ASTNodeDeclarationVariable
 * - ASTNodeDeclarationType
 */
export type ASTNodeDeclaration =
	| ASTNodeDeclarationVariable
	| ASTNodeDeclarationType
;
export class ASTNodeDeclarationVariable extends ASTNodeSolid {
	constructor (
		start_node: ParseNode,
		readonly unfixed: boolean,
		readonly children:
			| readonly [ASTNodeVariable, ASTNodeType, ASTNodeExpression]
	) {
		super(start_node, {unfixed}, children)
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		const variable: ASTNodeVariable = this.children[0];
		if (validator.hasSymbol(variable.id)) {
			throw new AssignmentError01(variable);
		};
		this.children[1].varCheck(validator);
		this.children[2].varCheck(validator);
		validator.addSymbol(new SymbolStructureVar(
			variable.id,
			variable.line_index,
			variable.col_index,
			this.children[1].assess(validator),
			this.unfixed,
			(!this.unfixed) ? this.children[2] : null,
		));
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		const assignee_type: SolidLanguageType = this.children[1].assess(validator);
		const assigned_type: SolidLanguageType = this.children[2].type(validator);
		if (
			assigned_type.isSubtypeOf(assignee_type) ||
			validator.config.compilerOptions.intCoercion && assigned_type.isSubtypeOf(Int16) && Float64.isSubtypeOf(assignee_type)
		) {
		} else {
			throw new TypeError03(this, assignee_type, assigned_type)
		}
	}
	/** @implements ASTNodeSolid */
	build(_builder: Builder): Instruction {
		throw new Error('ASTNodeDeclarationVariable#build not yet supported.');
	}
}
export class ASTNodeDeclarationType extends ASTNodeSolid {
	constructor (
		start_node: ParseNode,
		readonly children:
			| readonly [ASTNodeTypeAlias, ASTNodeType]
		,
	) {
		super(start_node, {}, children);
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		const variable: ASTNodeTypeAlias = this.children[0];
		if (validator.hasSymbol(variable.id)) {
			throw new AssignmentError01(variable);
		};
		this.children[1].varCheck(validator);
		validator.addSymbol(new SymbolStructureType(
			variable.id,
			variable.line_index,
			variable.col_index,
			this.children[1],
		));
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		return this.children[1].typeCheck(validator);
	}
	/** @implements ASTNodeSolid */
	build(_builder: Builder): Instruction {
		throw new Error('ASTNodeDeclarationType#build not yet supported.');
	}
}
export class ASTNodeAssignment extends ASTNodeSolid {
	constructor (
		start_node: ParseNode,
		readonly children:
			| readonly [ASTNodeAssignee, ASTNodeExpression]
	) {
		super(start_node, {}, children)
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		return this.children.forEach((c) => c.varCheck(validator));
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		const assignee_type: SolidLanguageType = this.children[0].children[0].type(validator);
		const assigned_type: SolidLanguageType = this.children[1].type(validator);
		if (
			assigned_type.isSubtypeOf(assignee_type) ||
			validator.config.compilerOptions.intCoercion && assigned_type.isSubtypeOf(Int16) && Float64.isSubtypeOf(assignee_type)
		) {
		} else {
			throw new TypeError03(this, assignee_type, assigned_type);
		};
	}
	/** @implements ASTNodeSolid */
	build(_builder: Builder): Instruction {
		throw new Error('ASTNodeAssignment#build not yet supported.');
	}
}
export class ASTNodeAssignee extends ASTNodeSolid {
	constructor(
		start_node: Token,
		readonly children:
			| readonly [ASTNodeVariable]
	) {
		super(start_node, {}, children)
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		const variable: ASTNodeVariable = this.children[0];
		variable.varCheck(validator);
		if (!(validator.getSymbolInfo(variable.id) as SymbolStructureVar).unfixed) {
			throw new AssignmentError10(variable);
		};
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		return this.children[0].typeCheck(validator);
	}
	/** @implements ASTNodeSolid */
	build(_builder: Builder): Instruction {
		throw new Error('ASTNodeAssignee#build not yet supported.')
	}
}
export class ASTNodeGoal extends ASTNodeSolid {
	constructor(
		start_node: ParseNode,
		readonly children:
			| readonly []
			| readonly ASTNodeStatement[]
	) {
		super(start_node, {}, children)
	}
	/** @implements ASTNodeSolid */
	varCheck(validator: Validator = new Validator()): void {
		this.children.forEach((c) => c.varCheck(validator));
	}
	/** @implements ASTNodeSolid */
	typeCheck(validator: Validator = new Validator()): void {
		return this.children.forEach((child) => child.typeCheck(validator));
	}
	/** @implements ASTNodeSolid */
	build(builder: Builder): InstructionNone | InstructionModule {
		return (!this.children.length)
			? new InstructionNone()
			: new InstructionModule([
				...Builder.IMPORTS,
				...(this.children as readonly ASTNodeStatement[]).map((child) => child.build(builder)),
			])
	}
}
