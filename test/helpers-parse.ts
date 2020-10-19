import {
	Filebound,
	Token,
	TokenFilebound,
} from '@chharvey/parser';
import * as assert from 'assert'

import SolidConfig, {CONFIG_DEFAULT} from '../src/SolidConfig'
import {
	Punctuator,
	TokenPunctuator,
	TokenKeyword,
	TokenIdentifier,
	TokenNumber,
	TokenString,
} from '../src/parser/Token';
import {
	PARSER,
	ParserSolid as Parser,
} from '../src/parser/';
import {
	assert_arrayLength,
} from './assert-helpers'



export function tokenLiteralFromTypeString(typestring: string, config: SolidConfig = CONFIG_DEFAULT): TokenKeyword | TokenNumber | TokenString {
	const token: Token = primitiveTypeFromString(typestring, config).children[0]
	assert.ok(
		token instanceof TokenKeyword ||
		token instanceof TokenNumber  ||
		token instanceof TokenString
	, 'token should be a TokenKeyword or TokenNumber or TokenString')
	return token
}
export function tokenKeywordFromTypeString(typestring: string, config: SolidConfig = CONFIG_DEFAULT): TokenKeyword {
	const token: Token = keywordTypeFromString(typestring, config).children[0]
	assert.ok(token instanceof TokenKeyword, 'token should be a TokenKeyword')
	return token
}
export function primitiveTypeFromString(typestring: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodePrimitiveLiteral {
	const type_unit: PARSER.ParseNodeTypeUnit = unitTypeFromString(typestring, config)
	assert_arrayLength(type_unit.children, 1, 'type unit should have 1 child')
	const unit: PARSER.ParseNodePrimitiveLiteral | PARSER.ParseNodeTypeKeyword = type_unit.children[0]
	assert.ok(unit instanceof PARSER.ParseNodePrimitiveLiteral, 'unit should be a ParseNodePrimitiveLiteral')
	return unit
}
export function keywordTypeFromString(typestring: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeTypeKeyword {
	const type_unit: PARSER.ParseNodeTypeUnit = unitTypeFromString(typestring, config)
	assert_arrayLength(type_unit.children, 1, 'type unit should have 1 child')
	const unit: PARSER.ParseNodePrimitiveLiteral | PARSER.ParseNodeTypeKeyword = type_unit.children[0]
	assert.ok(unit instanceof PARSER.ParseNodeTypeKeyword, 'unit should be a ParseNodeTypeKeyword')
	return unit
}
export function unitTypeFromString(typestring: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeTypeUnit {
	const type_unary: PARSER.ParseNodeTypeUnarySymbol = unaryTypeFromString(typestring, config)
	assert_arrayLength(type_unary.children, 1, 'unary type should have 1 child')
	return type_unary.children[0]
}
export function unaryTypeFromString(typestring: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeTypeUnarySymbol {
	const type_intersection: PARSER.ParseNodeTypeIntersection = intersectionTypeFromString(typestring, config)
	assert_arrayLength(type_intersection.children, 1, 'intersection type should have 1 child')
	return type_intersection.children[0]
}
export function intersectionTypeFromString(typestring: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeTypeIntersection {
	const type_union: PARSER.ParseNodeTypeUnion = unionTypeFromString(typestring, config)
	assert_arrayLength(type_union.children, 1, 'union type should have 1 child')
	return type_union.children[0]
}
export function unionTypeFromString(typestring: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeTypeUnion {
	return typeFromString(typestring, config).children[0]
}
function typeFromString(typestring: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeType {
	return typeFromSource(`let x: ${ typestring } = null;`, config)
}
export function tokenLiteralFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): TokenKeyword | TokenNumber | TokenString {
	const token: Token = primitiveLiteralFromSource(src, config).children[0]
	assert.ok(
		token instanceof TokenKeyword ||
		token instanceof TokenNumber  ||
		token instanceof TokenString
	, 'token should be a TokenKeyword or TokenNumber or TokenString')
	return token
}
export function tokenIdentifierFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): TokenIdentifier {
	const expression_unit: PARSER.ParseNodeExpressionUnit = unitExpressionFromSource(src, config)
	assert_arrayLength(expression_unit.children, 1, 'expression unit should have 1 child')
	const unit: Token | PARSER.ParseNodePrimitiveLiteral | PARSER.ParseNodeStringTemplate = expression_unit.children[0]
	assert.ok(unit instanceof TokenIdentifier, 'unit should be a TokenIdentifier')
	return unit
}
export function primitiveLiteralFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodePrimitiveLiteral {
	const expression_unit: PARSER.ParseNodeExpressionUnit = unitExpressionFromSource(src, config)
	assert_arrayLength(expression_unit.children, 1, 'expression unit should have 1 child')
	const unit: Token | PARSER.ParseNodePrimitiveLiteral | PARSER.ParseNodeStringTemplate = expression_unit.children[0]
	assert.ok(unit instanceof PARSER.ParseNodePrimitiveLiteral, 'unit should be a ParseNodePrimitiveLiteral')
	return unit
}
export function unitExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionUnit {
	const expression_unary: PARSER.ParseNodeExpressionUnarySymbol = unaryExpressionFromSource(src, config)
	assert_arrayLength(expression_unary.children, 1, 'unary expression should have 1 child')
	return expression_unary.children[0]
}
export function unaryExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionUnarySymbol {
	const expression_exp: PARSER.ParseNodeExpressionExponential = exponentialExpressionFromSource(src, config)
	assert_arrayLength(expression_exp.children, 1, 'exponential expression should have 1 child')
	return expression_exp.children[0]
}
export function exponentialExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionExponential {
	const expression_mul: PARSER.ParseNodeExpressionMultiplicative = multiplicativeExpressionFromSource(src, config)
	assert_arrayLength(expression_mul.children, 1, 'multiplicative expression should have 1 child')
	return expression_mul.children[0]
}
export function multiplicativeExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionMultiplicative {
	const expression_add: PARSER.ParseNodeExpressionAdditive = additiveExpressionFromSource(src, config)
	assert_arrayLength(expression_add.children, 1, 'additive expression should have 1 child')
	return expression_add.children[0]
}
export function additiveExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionAdditive {
	const expression_compare: PARSER.ParseNodeExpressionComparative = comparativeExpressionFromSource(src, config)
	assert_arrayLength(expression_compare.children, 1, 'comparative expression should have 1 child')
	return expression_compare.children[0]
}
export function comparativeExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionComparative {
	const expression_eq: PARSER.ParseNodeExpressionEquality = equalityExpressionFromSource(src, config)
	assert_arrayLength(expression_eq.children, 1, 'equality expression should have 1 child')
	return expression_eq.children[0]
}
export function equalityExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionEquality {
	const expression_conj: PARSER.ParseNodeExpressionConjunctive = conjunctiveExpressionFromSource(src, config)
	assert_arrayLength(expression_conj.children, 1, 'conjunctive expression should have 1 child')
	return expression_conj.children[0]
}
export function conjunctiveExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionConjunctive {
	const expression_disj: PARSER.ParseNodeExpressionDisjunctive = disjunctiveExpressionFromSource(src, config)
	assert_arrayLength(expression_disj.children, 1, 'disjunctive expression should have 1 child')
	return expression_disj.children[0]
}
export function disjunctiveExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionDisjunctive {
	const expression: PARSER.ParseNodeExpression = expressionFromSource(src, config)
	const expression_disj: PARSER.ParseNodeExpressionDisjunctive | PARSER.ParseNodeExpressionConditional = expression.children[0]
	assert.ok(expression_disj instanceof PARSER.ParseNodeExpressionDisjunctive, 'expression_disj should be a ParseNodeExpressionDisjunctive')
	return expression_disj
}
export function conditionalExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpressionConditional {
	const expression: PARSER.ParseNodeExpression = expressionFromSource(src, config)
	const expression_cond: PARSER.ParseNodeExpressionDisjunctive | PARSER.ParseNodeExpressionConditional = expression.children[0]
	assert.ok(expression_cond instanceof PARSER.ParseNodeExpressionConditional, 'expression_cond should be a ParseNodeExpressionConditional')
	return expression_cond
}
function expressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeExpression {
	const statement: PARSER.ParseNodeStatement = statementFromSource(src, config)
	assert_arrayLength(statement.children, 2, 'statment should have 2 children')
	const [expression, endstat]: readonly [PARSER.ParseNodeExpression, Token] = statement.children
	assert.ok(endstat instanceof TokenPunctuator)
	assert.strictEqual(endstat.source, Punctuator.ENDSTAT)
	return expression
}
function typeFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeType {
	const var_decl: PARSER.ParseNodeDeclarationVariable = variableDeclarationFromSource(src, config)
	return (var_decl.children.length === 7) ? var_decl.children[3] : var_decl.children[4]
}
export function variableDeclarationFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeDeclarationVariable {
	const statement: PARSER.ParseNodeStatement = statementFromSource(src, config)
	assert_arrayLength(statement.children, 1, 'statement should have 1 child')
	const var_decl: Token | PARSER.ParseNodeDeclarationVariable | PARSER.ParseNodeStatementAssignment = statement.children[0]
	assert.ok(var_decl instanceof PARSER.ParseNodeDeclarationVariable)
	return var_decl
}
export function statementFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): PARSER.ParseNodeStatement {
	const goal: PARSER.ParseNodeGoal = new Parser(src).parse()
	assert_arrayLength(goal.children, 3, 'goal should have 3 children')
	const [sot, stat_list, eot]: readonly [Token, PARSER.ParseNodeGoal__0__List, Token] = goal.children
	assert.ok(sot instanceof TokenFilebound)
	assert.ok(eot instanceof TokenFilebound)
	assert.strictEqual(sot.source, Filebound.SOT)
	assert.strictEqual(eot.source, Filebound.EOT)
	assert_arrayLength(stat_list.children, 1, 'statement list should have 1 child')
	return stat_list.children[0]
}
