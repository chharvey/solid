import * as assert from 'assert'
import * as fs from 'fs'
import * as path from 'path'

import {CONFIG_DEFAULT} from '../src/SolidConfig'
import Dev from '../src/class/Dev.class'
import Parser from '../src/class/Parser.class'
import CodeGenerator from '../src/class/CodeGenerator.class'
import type {
	ParseNodeExpression,
	ParseNodeStatement,
	ParseNodeGoal__0__List,
} from '../src/class/ParseNode.class'
import {
	SolidLanguageTypeDraft,
	SemanticNodeExpression,
	SemanticNodeConstant,
	SemanticNodeIdentifier,
	SemanticNodeTemplate,
	SemanticNodeOperation,
	SemanticNodeStatementExpression,
} from '../src/class/SemanticNode.class'
import SolidLanguageValue, {
	SolidNull,
	SolidBoolean,
} from '../src/vm/SolidLanguageValue.class'
import {
	InstructionConst,
	InstructionStatement,
	InstructionModule,
} from '../src/vm/Instruction.class'



describe('SemanticNode', () => {
	describe('.constructor', () => {
		context('SemanticNodeExpression', () => {
			it('rethrows `this.type()`.', () => {
				[
					`null;`,
					`42;`,
					`21 + 21;`,
				].forEach((src) => {
					(new Parser(src, CONFIG_DEFAULT).parse().decorate()
						.children[0] as SemanticNodeStatementExpression)
						.children[0] as SemanticNodeExpression
				})
				assert.throws(() => {
					(new Parser(`null + 5;`, CONFIG_DEFAULT).parse().decorate()
						.children[0] as SemanticNodeStatementExpression)
						.children[0] as SemanticNodeExpression
				}, /Invalid operation./)
			})
		})

		context('SemanticNodeStatementExpression', () => {
			it('rethrows the type of the expression.', () => {
				[
					`null;`,
					`42;`,
					`21 + 21;`,
				].forEach((src) => {
					new Parser(src, CONFIG_DEFAULT).parse().decorate()
						.children[0] as SemanticNodeStatementExpression
				})
				assert.throws(() => {
					new Parser(`null + 5;`, CONFIG_DEFAULT).parse().decorate()
						.children[0] as SemanticNodeStatementExpression
				}, /Invalid operation./)
			})
		})

		context('SemanticNodeGoal', () => {
			it('rethrows the type of each statement.', () => {
				[
					`null;`,
					`42;`,
					`21 + 21;`,
				].forEach((src) => {
					new Parser(src, CONFIG_DEFAULT).parse().decorate()
				})
				assert.throws(() => {
					new Parser(`null + 5;`, CONFIG_DEFAULT).parse().decorate()
				}, TypeError)
			})
		})
	})


	describe('#build', () => {
		const i32_neg: string = fs.readFileSync(path.join(__dirname, '../src/neg.wat'), 'utf8')
		const i32_exp: string = fs.readFileSync(path.join(__dirname, '../src/exp.wat'), 'utf8')
		function boilerplate(expected: string): string {
			return boilerplate_mod(stmt(0n, expected))
		}
		function boilerplate_mod(...stmts: string[]): string {
			return new InstructionModule([i32_neg, i32_exp, ...stmts]).toString()
		}
		function stmt(count: bigint, expr: string): string {
			return new InstructionStatement(count, new InstructionConst(+expr.slice(11, -1))).toString()
		}

		context('SemanticNodeGoal ::= SOT EOT', () => {
			it('prints nothing.', () => {
				assert.strictEqual(new CodeGenerator('', CONFIG_DEFAULT).print(), '')
			})
		})

		context('SemanticNodeStatement ::= ";"', () => {
			it('prints empty module.', () => {
				assert.strictEqual(new CodeGenerator(';', CONFIG_DEFAULT).print(), boilerplate_mod(''))
			})
		})

		context('SemanticNodeConstant', () => {
			it('pushes the constant onto the stack.', () => {
				assert.deepStrictEqual([
					'null;',
					'false;',
					'true;',
					'42;',
					'+42;',
					'-42;',
				].map((src) => new CodeGenerator(src, CONFIG_DEFAULT).print()), [
					`(i32.const 0)`,
					`(i32.const 0)`,
					`(i32.const 1)`,
					`(i32.const 42)`,
					`(i32.const 42)`,
					`(i32.const -42)`,
				].map(boilerplate))
			})
		})

		context('SemanticNodeOperation', () => {
			specify('ExpressionAdditive ::= ExpressionAdditive "+" ExpressionMultiplicative', () => {
				assert.strictEqual(new CodeGenerator('42 + 420;', CONFIG_DEFAULT).print(), boilerplate(`
					(i32.const ${ 42 + 420 })
				`.trim()))
			})
			specify('ExpressionAdditive ::= ExpressionAdditive "-" ExpressionMultiplicative', () => {
				assert.strictEqual(new CodeGenerator('42 - 420;', CONFIG_DEFAULT).print(), boilerplate(`
					(i32.const ${ 42 + -420 })
				`.trim()))
			})
			specify('ExpressionMultiplicative ::= ExpressionMultiplicative "/" ExpressionExponential', () => {
				assert.deepStrictEqual([
					'126 / 3;',
					'-126 / 3;',
					'126 / -3;',
					'-126 / -3;',
					'200 / 3;',
					'200 / -3;',
					'-200 / 3;',
					'-200 / -3;',
				].map((src) => new CodeGenerator(src, CONFIG_DEFAULT).print()), [
					126 / 3,
					-126 / 3,
					126 / -3,
					-126 / -3,
					200 / 3,
					200 / -3,
					-200 / 3,
					-200 / -3,
				].map((v) => boilerplate(`(i32.const ${ Math.trunc(v) })`)))
			})
			specify('compound expression.', () => {
				assert.strictEqual(new CodeGenerator('42 ^ 2 * 420;', CONFIG_DEFAULT).print(), boilerplate(`
					(i32.const ${ (42 ** 2 * 420) % (2 ** 16) })
				`.trim()))
			})
			specify('overflow.', () => {
				assert.strictEqual(new CodeGenerator('2 ^ 15 + 2 ^ 14;', CONFIG_DEFAULT).print(), boilerplate(`
					(i32.const ${ -(2 ** 14) })
				`.trim()))
				assert.strictEqual(new CodeGenerator('-(2 ^ 14) - 2 ^ 15;', CONFIG_DEFAULT).print(), boilerplate(`
					(i32.const ${ 2 ** 14 })
				`.trim()))
			})
			specify('compound expression with grouping.', () => {
				assert.strictEqual(new CodeGenerator('-(5) ^ +(2 * 3);', CONFIG_DEFAULT).print(), boilerplate(`
					(i32.const ${ (-(5)) ** +(2 * 3) })
				`.trim()))
			})
			specify('multiple statements.', () => {
				assert.strictEqual(new CodeGenerator('42; 420;', CONFIG_DEFAULT).print(), boilerplate_mod(...[
					`(i32.const 42)`,
					`(i32.const 420)`,
				].map((out, i) => stmt(BigInt(i), out))))
			})
		})
	})


	context('SemanticNodeExpression', () => {
		describe('#type', () => {
			it('returns `Null` for SemanticNodeConstant with null value.', () => {
				assert.strictEqual(((new Parser(`null;`, CONFIG_DEFAULT).parse().decorate()
					.children[0] as SemanticNodeStatementExpression)
					.children[0] as SemanticNodeConstant).type(), SolidNull)
			})
			it('returns `Boolean` for SemanticNodeConstant with bool value.', () => {
				[
					`false;`,
					`true;`,
				].forEach((src) => {
					assert.strictEqual(((new Parser(src, CONFIG_DEFAULT).parse().decorate()
						.children[0] as SemanticNodeStatementExpression)
						.children[0] as SemanticNodeConstant).type(), SolidBoolean)
				})
			})
			it('returns `Integer` for SemanticNodeConstant with number value.', () => {
				assert.strictEqual(((new Parser(`42;`, CONFIG_DEFAULT).parse().decorate()
					.children[0] as SemanticNodeStatementExpression)
					.children[0] as SemanticNodeConstant).type(), SolidLanguageTypeDraft.NUMBER)
			})
			Dev.supports('variables') && it('throws for identifiers.', () => {
				assert.throws(() => ((new Parser(`x;`, CONFIG_DEFAULT).parse().decorate()
					.children[0] as SemanticNodeStatementExpression)
					.children[0] as SemanticNodeIdentifier).type(), /Not yet supported./)
			})
			it('returns `String` for SemanticNodeConstant with string value.', () => {
				;[
					...(Dev.supports('literalString') ? [
						(new Parser(`'42';`, CONFIG_DEFAULT).parse().decorate()
							.children[0] as SemanticNodeStatementExpression)
							.children[0] as SemanticNodeConstant,
					] : []),
					...(Dev.supports('literalTemplate') ? [
						(new Parser(`'''42''';`, CONFIG_DEFAULT).parse().decorate()
							.children[0] as SemanticNodeStatementExpression)
							.children[0] as SemanticNodeTemplate,
						(new Parser(`'''the answer is {{ 7 * 3 * 2 }} but what is the question?''';`, CONFIG_DEFAULT).parse().decorate()
							.children[0] as SemanticNodeStatementExpression)
							.children[0] as SemanticNodeTemplate,
					] : []),
				].forEach((node) => {
					assert.strictEqual(node.type(), SolidLanguageTypeDraft.STRING)
				})
			})
			it('returns `Integer` or any operation of numbers.', () => {
				assert.strictEqual(((new Parser(`7 * 3 * 2;`, CONFIG_DEFAULT).parse().decorate()
					.children[0] as SemanticNodeStatementExpression)
					.children[0] as SemanticNodeOperation).type(), SolidLanguageTypeDraft.NUMBER)
			})
			it('throws for operation of non-numbers.', () => {
				[
					`null + 5;`,
					`5 * null;`,
					`false - 2;`,
					`2 / true;`,
					`null ^ false;`,
					...(Dev.supports('literalString') ? [`'hello' + 5;`] : []),
				].forEach((src) => {
					assert.throws(() => ((new Parser(src, CONFIG_DEFAULT).parse().decorate()
						.children[0] as SemanticNodeStatementExpression)
						.children[0] as SemanticNodeOperation).type(), /Invalid operation./)
				})
			})
		})

		describe('#assess', () => {
			it('computes the value of constant null or boolean expression.', () => {
				assert.deepStrictEqual([
					'null;',
					'false;',
					'true;',
				].map((src) => (((new Parser(src, CONFIG_DEFAULT).parse()
					.children[1] as ParseNodeGoal__0__List)
					.children[0] as ParseNodeStatement)
					.children[0] as ParseNodeExpression
				).decorate().assess().value), [
					SolidNull.NULL,
					SolidBoolean.FALSE,
					SolidBoolean.TRUE,
				])
			})
			it('computes the value of a constant numeric expression.', () => {
				const values: (number | SolidLanguageValue | SemanticNodeExpression)[] = [
					'42 + 420;',
					'42 - 420;',
					'126 / 3;',
					'-126 / 3;',
					'126 / -3;',
					'-126 / -3;',
					'200 / 3;',
					'200 / -3;',
					'-200 / 3;',
					'-200 / -3;',
					'42 ^ 2 * 420;',
					'2 ^ 15 + 2 ^ 14;',
					'-(2 ^ 14) - 2 ^ 15;',
					'-(5) ^ +(2 * 3);',
				].map((src) => (((new Parser(src, CONFIG_DEFAULT).parse()
					.children[1] as ParseNodeGoal__0__List)
					.children[0] as ParseNodeStatement)
					.children[0] as ParseNodeExpression
				).decorate().assess().value)
				values.forEach((value) => {
					assert.strictEqual(typeof value, 'number')
				})
				assert.deepStrictEqual(values, [
					42 + 420,
					42 + -420,
					126 / 3,
					-126 / 3,
					126 / -3,
					-126 / -3,
					Math.trunc(200 / 3),
					Math.trunc(200 / -3),
					Math.trunc(-200 / 3),
					Math.trunc(-200 / -3),
					(42 ** 2 * 420) % (2 ** 16),
					-(2 ** 14),
					2 ** 14,
					(-(5)) ** +(2 * 3),
				])
			})
		})
	})
})
