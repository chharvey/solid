import type {
	Token,
} from '@chharvey/parser';
import * as assert from 'assert'

import {
	CONFIG_DEFAULT,
	Dev,
	Util,
} from '../../src/core/';
import {
	TOKEN,
	LexerSolid as Lexer,
} from '../../src/parser/';



describe('TokenSolid', () => {
	describe('#cook', () => {
		context('TokenPunctuator', () => {
			it('assigns values 0n–127n to punctuator tokens.', () => {
				const cooked: bigint[] = [...new Lexer(TOKEN.TokenPunctuator.PUNCTUATORS.join(' '), CONFIG_DEFAULT).generate()]
					.filter((token): token is TOKEN.TokenPunctuator => token instanceof TOKEN.TokenPunctuator)
					.map((punctuator) => punctuator.cook())
				const expected: bigint[] = [...new Array(128)].map((_, i) => BigInt(i)).slice(0, TOKEN.TokenPunctuator.PUNCTUATORS.length)
				assert.deepStrictEqual(cooked, expected)
				cooked.forEach((value) => {
					assert.ok(0n <= value, 'cooked value should be >= 0n.')
					assert.ok(value < 128n, 'cooked value should be < 128n.')
				})
			})
		})

		context('TokenKeyword', () => {
			it('assigns values 128n–255n to reserved keywords.', () => {
				const cooked: bigint[] = [...new Lexer(TOKEN.TokenKeyword.KEYWORDS.join(' '), CONFIG_DEFAULT).generate()]
					.filter((token): token is TOKEN.TokenKeyword => token instanceof TOKEN.TokenKeyword)
					.map((keyword) => keyword.cook())
				const expected: bigint[] = [...new Array(128)].map((_, i) => BigInt(i + 128)).slice(0, TOKEN.TokenKeyword.KEYWORDS.length)
				assert.deepStrictEqual(cooked, expected)
				cooked.forEach((value) => {
					assert.ok(128n <= value, 'cooked value should be >= 128n.')
					assert.ok(value < 256n, 'cooked value should be < 256n.')
				})
			})
		})

		Dev.supports('variables') && context('TokenIdentifier', () => {
			context('TokenIdentifierBasic', () => {
				const cooked: (bigint|null)[] = [...new Lexer(`
					this be a word
					_words _can _start _with _underscores
					_and0 _can1 contain2 numb3rs

					a word _can repeat _with the same id
				`, CONFIG_DEFAULT).generate()]
					.filter((token): token is TOKEN.TokenIdentifier => token instanceof TOKEN.TokenIdentifier)
					.map((identifier) => identifier.cook())
				it('assigns values 256n or greater.', () => {
					cooked.forEach((value) => {
						assert.ok(value !== null, 'cooked value should not be null.')
						assert.ok(value ! >= 256n, 'cooked value should be >= 256n.')
					})
				})
				it('assigns the same value to identical identifier names.', () => {
					assert.deepStrictEqual([
						cooked[2],
						cooked[3],
						cooked[5],
						cooked[7],
					], [
						cooked[13],
						cooked[14],
						cooked[15],
						cooked[17],
					])
				})
			})

			context('TokenIdentifierUnicode', () => {
				const cooked: (bigint|null)[] = [...new Lexer(`
					\`this\` \`is\` \`a\` \`unicode word\`
					\`any\` \`unicode word\` \`can\` \`contain\` \`any\` \`character\`
					\`except\` \`back-ticks\` \`.\`
				`, CONFIG_DEFAULT).generate()]
					.filter((token): token is TOKEN.TokenIdentifierUnicode => token instanceof TOKEN.TokenIdentifierUnicode)
					.map((identifier) => identifier.cook())
				it('assigns values 256n or greater.', () => {
					cooked.forEach((value) => {
						assert.ok(value !== null, 'cooked value should not be null.')
						assert.ok(value ! >= 256n, 'cooked value should be >= 256n.')
					})
				})
				it('assigns the same value to identical identifier names.', () => {
					assert.deepStrictEqual([
						cooked[3],
						cooked[4],
					], [
						cooked[5],
						cooked[8],
					])
				})
			})
		})

		context('TokenNumber', () => {
			;[...new Map<string, [string, number[]]>([
				['implicit radix integers', [`
					370  037  +9037  -9037  +06  -06
				`, [
					370, 37, 9037, -9037, 6, -6,
				]]],
				['explicit radix integers', [`
					\\b100  \\b001  +\\b1000  -\\b1000  +\\b01  -\\b01
					\\q320  \\q032  +\\q1032  -\\q1032  +\\q03  -\\q03
					\\o370  \\o037  +\\o1037  -\\o1037  +\\o06  -\\o06
					\\d370  \\d037  +\\d9037  -\\d9037  +\\d06  -\\d06
					\\xe70  \\x0e7  +\\x90e7  -\\x90e7  +\\x06  -\\x06
					\\ze70  \\z0e7  +\\z90e7  -\\z90e7  +\\z06  -\\z06
				`, [
						    4,  1,       8,      -8, 1, -1,
						   56, 14,      78,     -78, 3, -3,
						  248, 31,     543,    -543, 6, -6,
						  370, 37,    9037,   -9037, 6, -6,
						 3696, 231,  37095,  -37095, 6, -6,
						18396, 511, 420415, -420415, 6, -6,
				]]],
				['floats', [`
					55.  -55.  033.  -033.  2.007  -2.007
					91.27e4  -91.27e4  91.27e-4  -91.27e-4
					-0.  -0.0  6.8e+0  6.8e-0  0.0e+0  -0.0e-0
				`, [
					55.0, -55.0, 33.0, -33.0, 2.007, -2.007,
					91.27e4, -91.27e4, 91.27e-4, -91.27e-4,
					-0.0, -0.0, 6.8, 6.8, 0.0, -0.0,
				]]],
				['implicit radix integers with separators', [`
					12_345  +12_345  -12_345  0123_4567  +0123_4567  -0123_4567  012_345_678  +012_345_678  -012_345_678
				`, [
					12345, 12345, -12345, 1234567, 1234567, -1234567, 12345678, 12345678, -12345678,
				]]],
				['explicit radix integers with separators', [`
					\\b1_00  \\b0_01  +\\b1_000  -\\b1_000  +\\b0_1  -\\b0_1
					\\q3_20  \\q0_32  +\\q1_032  -\\q1_032  +\\q0_3  -\\q0_3
					\\o3_70  \\o0_37  +\\o1_037  -\\o1_037  +\\o0_6  -\\o0_6
					\\d3_70  \\d0_37  +\\d9_037  -\\d9_037  +\\d0_6  -\\d0_6
					\\xe_70  \\x0_e7  +\\x9_0e7  -\\x9_0e7  +\\x0_6  -\\x0_6
					\\ze_70  \\z0_e7  +\\z9_0e7  -\\z9_0e7  +\\z0_6  -\\z0_6
				`, [
						    4,  1,       8,      -8, 1, -1,
						   56, 14,      78,     -78, 3, -3,
						  248, 31,     543,    -543, 6, -6,
						  370, 37,    9037,   -9037, 6, -6,
						 3696, 231,  37095,  -37095, 6, -6,
						18396, 511, 420415, -420415, 6, -6,
				]]],
			])].forEach(([name, [source, values]]) => {
				it(`correctly cooks ${name}.`, () => {
					assert.deepStrictEqual([...new Lexer(source, {
						...CONFIG_DEFAULT,
						languageFeatures: {
							...CONFIG_DEFAULT.languageFeatures,
							integerRadices: true,
							numericSeparators: true,
						},
					}).generate()]
						.filter((token) => token instanceof TOKEN.TokenNumber)
						.map((token) => (token as TOKEN.TokenNumber).cook()), values)
				})
			})
		})

		Dev.supports('literalString') && context('TokenString', () => {
			it('produces the cooked string value.', () => {
				const tokens: Token[] = [...new Lexer(Util.dedent(`
					5 + 03 + '' * 'hello' *  -2;
					'0 \\' 1 \\\\ 2 \\s 3 \\t 4 \\n 5 \\r 6';
					'0 \\u{24} 1 \\u{005f} 2 \\u{} 3';
					'012\\
					345
					678';
					'\u{10001}' '\\\u{10001}';
				`), CONFIG_DEFAULT).generate()]
				assert.strictEqual((tokens[ 5] as TOKEN.TokenSolid).cook(), ``)
				assert.strictEqual((tokens[ 7] as TOKEN.TokenSolid).cook(), `hello`)
				assert.strictEqual((tokens[11] as TOKEN.TokenSolid).cook(), `0 \' 1 \\ 2 \u0020 3 \t 4 \n 5 \r 6`)
				assert.strictEqual((tokens[13] as TOKEN.TokenSolid).cook(), `0 $ 1 _ 2 \0 3`)
				assert.strictEqual((tokens[15] as TOKEN.TokenSolid).cook(), `012 345\n678`)
				assert.strictEqual((tokens[17] as TOKEN.TokenSolid).cook(), `\u{10001}`)
				assert.strictEqual((tokens[18] as TOKEN.TokenSolid).cook(), `\u{10001}`)
			})
		})

		Dev.supports('literalTemplate') && context('TokenTemplate', () => {
			it('produces the cooked template value.', () => {
				const tokens: Token[] = [...new Lexer(Util.dedent(`
					600  /  '''''' * 3 + '''hello''' *  2;
					3 + '''head{{ * 2
					3 + }}midl{{ * 2
					3 + }}tail''' * 2
					'''0 \\\` 1''';
					'''0 \\' 1 \\\\ 2 \\s 3 \\t 4 \\n 5 \\r 6 \\\\\` 7''';
					'''0 \\u{24} 1 \\u{005f} 2 \\u{} 3''';
					'''012\\
					345
					678''';
				`), CONFIG_DEFAULT).generate()]
				assert.strictEqual((tokens[ 3] as TOKEN.TokenSolid).cook(), ``)
				assert.strictEqual((tokens[ 7] as TOKEN.TokenSolid).cook(), `hello`)
				assert.strictEqual((tokens[13] as TOKEN.TokenSolid).cook(), `head`)
				assert.strictEqual((tokens[18] as TOKEN.TokenSolid).cook(), `midl`)
				assert.strictEqual((tokens[23] as TOKEN.TokenSolid).cook(), `tail`)
				assert.strictEqual((tokens[26] as TOKEN.TokenSolid).cook(), `0 \\\` 1`)
				assert.strictEqual((tokens[28] as TOKEN.TokenSolid).cook(), `0 \\' 1 \\\\ 2 \\s 3 \\t 4 \\n 5 \\r 6 \\\\\` 7`)
				assert.strictEqual((tokens[30] as TOKEN.TokenSolid).cook(), `0 \\u{24} 1 \\u{005f} 2 \\u{} 3`)
				assert.strictEqual((tokens[32] as TOKEN.TokenSolid).cook(), `012\\\n345\n678`)
			})
		})

		Dev.supports('literalString') && it('throws when UTF-16 encoding input is out of range.', () => {
			const stringtoken: TOKEN.TokenString = [...new Lexer(Util.dedent(`
				'a string literal with a unicode \\u{a00061} escape sequence out of range';
			`), CONFIG_DEFAULT).generate()][1] as TOKEN.TokenString
			assert.throws(() => stringtoken.cook(), RangeError)
		})
	})

	describe('#serialize', () => {
		specify('TokenCommentLine', () => {
			assert.strictEqual([...new Lexer(Util.dedent(`
				500  +  30; ;  % line comment  *  2
				8;
			`), CONFIG_DEFAULT).generate()][11].serialize(), Util.dedent(`
				<COMMENT line="1" col="16">% line comment  *  2\n</COMMENT>
			`).trim())
		})
		specify('TokenCommentMulti', () => {
			assert.strictEqual([...new Lexer(Util.dedent(`
				%% multiline
				that has a
				comment %%
			`), CONFIG_DEFAULT).generate()][2].serialize(), Util.dedent(`
				<COMMENT line="1" col="1">%% multiline
				that has a
				comment %%</COMMENT>
			`).trim())
		})
	})
})
