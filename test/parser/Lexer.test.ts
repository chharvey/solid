import {
	Token,
	TokenWhitespace,
	LexError01,
	LexError02,
} from '@chharvey/parser';
import * as assert from 'assert'

import {
	SolidConfig,
	CONFIG_DEFAULT,
	Dev,
	Util,
} from '../../src/core/';
import {
	TemplatePosition,
	TOKEN,
	LexerSolid as Lexer,
} from '../../src/parser/'
import {
	LexError03,
	LexError04,
	LexError05,
} from '../../src/error/';



describe('LexerSolid', () => {
	describe('#generate', () => {
		it('rejects unrecognized characters.', () => {
			`. ~ { } # $ @ "`.split(' ').map((c) => new Lexer(`
				5  +  30
				+ 6 ^ - (${c} - 37 *
			`, CONFIG_DEFAULT)).forEach((lexer) => {
				assert.throws(() => [...lexer.generate()], LexError01)
			})
		})

		context('unfinished tokens.', () => {
			;[...new Map<string, string[]>([
				['line comment', [`
					% line \u0003 comment containing U+0003 END OF TEXT
					8;
				`]],
				['multiline comment', [`
					%%unfinished multiline
					comment
				`, `
					%%unfinished multiline containing U+0003 END OF TEXT
					\u0003
					comment
				`]],
				...(Dev.supports('variables') ? [
					['unicode identifier', [`
						\`unicode identifier without end delimiter
					`, `
						\`unicode identifier with end delimiter but contains \u0003 U+0003 END OF TEXT character\`
					`]] as [string, string[]],
				] : []),
				...(Dev.supports('literalString') ? [
					['string', [`
						'string without end delimiter
					`, `
						'string with end delimiter but contains \u0003 U+0003 END OF TEXT character'
						8;
					`]] as [string, string[]],
				] : []),
				...(Dev.supports('literalTemplate') ? [
					['template', [`
						'''template without end delimiter
					`, `
						'''template with end delimiter but contains \u0003 U+0003 END OF TEXT character'''
						8;
					`]] as [string, string[]],
				] : []),
			])].forEach(([name, sources]) => {
				it(`throws when ${name} token is unfinished.`, () => {
					sources.map((source) => new Lexer(source, CONFIG_DEFAULT)).forEach((lexer) => {
						assert.throws(() => [...lexer.generate()], LexError02)
					})
				})
			})
		})

		it('recognizes `TokenPunctuator` conditions.', () => {
			;[...new Lexer(TOKEN.TokenPunctuator.PUNCTUATORS.join(' '), CONFIG_DEFAULT).generate()].slice(1, -1).filter((token) => !(token instanceof TokenWhitespace)).forEach((value) => {
				assert.ok(value instanceof TOKEN.TokenPunctuator)
			})
		})

		it('recognizes `TokenKeyword` conditions.', () => {
			;[...new Lexer(TOKEN.TokenKeyword.KEYWORDS.join(' '), CONFIG_DEFAULT).generate()].slice(1, -1).filter((token) => !(token instanceof TokenWhitespace)).forEach((token) => {
				assert.ok(token instanceof TOKEN.TokenKeyword)
			})
		})

		!Dev.supports('variables') && context('throws when variables are turned off and a non-keywords is given.', () => {
			it('throws when non-keyword /^[a-z]+$/ is given', () => {
				assert.throws(() => [...new Lexer(`abc`, CONFIG_DEFAULT).generate()], /Identifier `abc` not yet allowed./)
			})
			it('throws when /^[a-z]+[A-Za-z0-9_]+$/ is given', () => {
				assert.throws(() => [...new Lexer(`falseTrue`,  CONFIG_DEFAULT).generate()], LexError01)
				assert.throws(() => [...new Lexer(`false_true`, CONFIG_DEFAULT).generate()], LexError01)
			})
		})

		Dev.supports('variables') && context('recognizes `TokenIdentifier` conditions.', () => {
			context('recognizes `TokenIdentifierBasic` conditions.', () => {
				specify('Basic identifier beginners.', () => {
					;[...new Lexer(`
						A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f g h i j k l m n o p q r s t u v w x y z _
					`, CONFIG_DEFAULT).generate()].slice(1, -1).filter((token) => !(token instanceof TokenWhitespace)).forEach((token) => {
						assert.ok(token instanceof TOKEN.TokenIdentifierBasic)
					})
				})
				specify('Identifier continuations.', () => {
					const tokens: Token[] = [...new Lexer(`
						this be a word
						_words _can _start _with _underscores
						_and0 _can1 contain2 numb3rs
					`, CONFIG_DEFAULT).generate()]
						.slice(1, -1).filter((token) => !(token instanceof TokenWhitespace))
					tokens.forEach((token) => {
						assert.ok(token instanceof TOKEN.TokenIdentifierBasic)
					})
					assert.strictEqual(tokens.length, 13)
				})
				specify('Identifiers cannot start with a digit.', () => {
					assert.deepStrictEqual([...new Lexer(`
						this be 0a word
						_words 1c_an _start 2w_ith _underscores
						_and0 3c_an1 contain2 44numb3rs
					`, CONFIG_DEFAULT).generate()].slice(1, -1).filter((token) => token instanceof TOKEN.TokenIdentifierBasic).map((token) => token.source), `
						this be a word _words c_an _start w_ith _underscores _and0 c_an1 contain2 numb3rs
					`.trim().split(' '))
				})
			})
			context('recognizes `TokenIdentifierUnicode` conditions.', () => {
				specify('Identifier boundaries.', () => {
					let tokens: Token[] = [...new Lexer(`
						\`this\` \`is\` \`a\` \`unicode word\`
						\`any\` \`unicode word\` \`can\` \`contain\` \`any\` \`character\`
						\`except\` \`back-ticks\` \`.\`
						\`<hello world>\` \`Æther\` \`5 × 3\` \`\\u{24}hello\` \`\`
					`, CONFIG_DEFAULT).generate()]
					tokens = tokens.slice(1, -1).filter((token) => !(token instanceof TokenWhitespace))
					tokens.forEach((token) => {
						assert.ok(token instanceof TOKEN.TokenIdentifierUnicode)
					})
					assert.strictEqual(tokens.length, 18)
				})
				it('should throw if Unicode identifier contains U+0060 GRAVE ACCENT.', () => {
					assert.throws(() => [...new Lexer(`
						\`a \\\` grave accent\`
					`, CONFIG_DEFAULT).generate()], LexError02)
				})
			})
		})

		context('recognizes `TokenNumber` conditions.', () => {
			const radices_on: SolidConfig = {
				...CONFIG_DEFAULT,
				languageFeatures: {
					...CONFIG_DEFAULT.languageFeatures,
					integerRadices: true,
				},
			}
			const separators_on: SolidConfig = {
				...CONFIG_DEFAULT,
				languageFeatures: {
					...CONFIG_DEFAULT.languageFeatures,
					numericSeparators: true,
				},
			}
			const both_on: SolidConfig = {
				...CONFIG_DEFAULT,
				languageFeatures: {
					...CONFIG_DEFAULT.languageFeatures,
					integerRadices: true,
					numericSeparators: true,
				},
			}
			specify('implicit radix integers.', () => {
				;[...new Lexer(TOKEN.TokenNumber.DIGITS.get(TOKEN.TokenNumber.RADIX_DEFAULT) !.join(' '), CONFIG_DEFAULT).generate()]
					.slice(1, -1).filter((token) => !(token instanceof TokenWhitespace)).forEach((token) => {
						assert.ok(token instanceof TOKEN.TokenNumber)
					})
				const tokens: Token[] = [...new Lexer(`
					+  55  -  33  2  007  700  +91  -27  +091  -0027
				`, CONFIG_DEFAULT).generate()]
				assert.strictEqual(tokens[ 4].source, `55`)
				assert.strictEqual(tokens[ 8].source, `33`)
				assert.strictEqual(tokens[10].source, `2`)
				assert.strictEqual(tokens[12].source, `007`)
				assert.strictEqual(tokens[14].source, `700`)
				assert.strictEqual(tokens[16].source, `+91`)
				assert.strictEqual(tokens[18].source, `-27`)
				assert.strictEqual(tokens[20].source, `+091`)
				assert.strictEqual(tokens[22].source, `-0027`)
			})
			context('explicit radix integers.', () => {
				it('throws when `config.languageFeatures.integerRadices` is turned off.', () => {
					assert.throws(() => [...new Lexer(`
						\\b100  \\b001  +\\b1000  -\\b1000  +\\b01  -\\b01
						\\q320  \\q032  +\\q1032  -\\q1032  +\\q03  -\\q03
						\\o370  \\o037  +\\o1037  -\\o1037  +\\o06  -\\o06
						\\d370  \\d037  +\\d9037  -\\d9037  +\\d06  -\\d06
						\\xe70  \\x0e7  +\\x90e7  -\\x90e7  +\\x06  -\\x06
						\\ze70  \\z0e7  +\\z90e7  -\\z90e7  +\\z06  -\\z06
					`, CONFIG_DEFAULT).generate()], LexError01)
				})
				it('recognizes radix prefix as the start of a number token.', () => {
					;[...TOKEN.TokenNumber.BASES].flatMap(([base, radix]) =>
						[...new Lexer(
							TOKEN.TokenNumber.DIGITS.get(radix)!.map((d) => `\\${ base }${ d }`).join(' '),
							radices_on,
						).generate()].slice(1, -1)
					).filter((token) => !(token instanceof TokenWhitespace)).forEach((token) => {
						assert.ok(token instanceof TOKEN.TokenNumber)
					})
				})
				it('`config.languageFeatures.integerRadices` allows integers with explicit radices.', () => {
					const source: string = `
						\\b100  \\b001  +\\b1000  -\\b1000  +\\b01  -\\b01
						\\q320  \\q032  +\\q1032  -\\q1032  +\\q03  -\\q03
						\\o370  \\o037  +\\o1037  -\\o1037  +\\o06  -\\o06
						\\d370  \\d037  +\\d9037  -\\d9037  +\\d06  -\\d06
						\\xe70  \\x0e7  +\\x90e7  -\\x90e7  +\\x06  -\\x06
						\\ze70  \\z0e7  +\\z90e7  -\\z90e7  +\\z06  -\\z06
					`.trim().replace(/\n\t+/g, '  ')
					;[...new Lexer(source, radices_on).generate()].slice(1, -1).filter((token) => !(token instanceof TokenWhitespace)).forEach((token, i) => {
						assert.ok(token instanceof TOKEN.TokenNumber)
						assert.strictEqual(token.source, source.split('  ')[i])
					})
				})
				specify('invalid sequence.', () => {
					Dev.supports('variables')
						? assert.deepStrictEqual([...new Lexer(`
							\\d39c
						`, radices_on).generate()].slice(2, -2).map((token) => token.source), ['\\d39', 'c'])
						: assert.throws(() => [...new Lexer(`
							\\d39c
						`, radices_on).generate()], /Identifier `c` not yet allowed./)
				})
				specify('invalid escape characters.', () => {
					`
						\\a0  \\c0  \\e0  \\f0  \\g0  \\h0  \\i0  \\j0  \\k0  \\l0  \\m0  \\n0  \\p0  \\r0  \\s0  \\t0  \\u0  \\v0  \\w0  \\y0  \\
						+\\a0 +\\c0 +\\e0 +\\f0 +\\g0 +\\h0 +\\i0 +\\j0 +\\k0 +\\l0 +\\m0 +\\n0 +\\p0 +\\r0 +\\s0 +\\t0 +\\u0 +\\v0 +\\w0 +\\y0 +\\
						-\\a0 -\\c0 -\\e0 -\\f0 -\\g0 -\\h0 -\\i0 -\\j0 -\\k0 -\\l0 -\\m0 -\\n0 -\\p0 -\\r0 -\\s0 -\\t0 -\\u0 -\\v0 -\\w0 -\\y0 -\\
					`.trim().split(' ').filter((src) => src !== '').map((src) => new Lexer(src, radices_on)).forEach((lexer) => {
						assert.throws(() => [...lexer.generate()], LexError03)
					})
				})
				specify('integers with invalid digits start a new token.', () => {
					assert.deepStrictEqual([...new Lexer(`
						\\b100400000  \\q1231423  \\o12345678
					`, radices_on).generate()].filter((token) => token instanceof TOKEN.TokenNumber).map((token) => token.source), [
						'\\b100', '400000', '\\q1231', '423', '\\o1234567', '8'
					])
				})
			})
			context('floats.', () => {
				it('tokenizes floats.', () => {
					const tokens: Token[] = [...new Lexer(`
						55.  -55.  033.  -033.  2.007  -2.007
						91.27e4  -91.27e4  91.27e-4  -91.27e-4
						-0.  -0.0  6.8e+0  6.8e-0  0.0e+0  -0.0e-0
						34.-78
					`, CONFIG_DEFAULT).generate()]
					assert.strictEqual(tokens[ 2].source, `55.`)
					assert.strictEqual(tokens[ 4].source, `-55.`)
					assert.strictEqual(tokens[ 6].source, `033.`)
					assert.strictEqual(tokens[ 8].source, `-033.`)
					assert.strictEqual(tokens[10].source, `2.007`)
					assert.strictEqual(tokens[12].source, `-2.007`)
					assert.strictEqual(tokens[14].source, `91.27e4`)
					assert.strictEqual(tokens[16].source, `-91.27e4`)
					assert.strictEqual(tokens[18].source, `91.27e-4`)
					assert.strictEqual(tokens[20].source, `-91.27e-4`)
					assert.strictEqual(tokens[22].source, `-0.`)
					assert.strictEqual(tokens[24].source, `-0.0`)
					assert.strictEqual(tokens[26].source, `6.8e+0`)
					assert.strictEqual(tokens[28].source, `6.8e-0`)
					assert.strictEqual(tokens[30].source, `0.0e+0`)
					assert.strictEqual(tokens[32].source, `-0.0e-0`)
					assert.strictEqual(tokens[34].source, `34.`)
					assert.strictEqual(tokens[35].source, `-78`)
				})
				it('recognizes exponent part not following fraction part as identifier.', () => {
					if (Dev.supports('variables')) {
						const tokens: Token[] = [...new Lexer(`91.e27`, CONFIG_DEFAULT).generate()]
						assert.ok(tokens[2] instanceof TOKEN.TokenNumber)
						assert.strictEqual(tokens[2].source, `91.`)
						assert.ok(tokens[3] instanceof TOKEN.TokenIdentifier)
						assert.strictEqual(tokens[3].source, `e27`)
					} else {
						assert.throws(() => [...new Lexer(`91.e27`, CONFIG_DEFAULT).generate()], /Identifier `e` not yet allowed./)
					}
				})
			})
			context('numbers with separators.', () => {
				it('tokenizes numeric separators as identifiers when `config.languageFeatures.numericSeparators` is turned off.', () => {
					if (Dev.supports('variables')) {
						const tokens: Token[] = [...new Lexer(`
							12_345  +12_345  -12_345  0123_4567  +0123_4567  -0123_4567  012_345_678  +012_345_678  -012_345_678
							\\b1_00  \\q0_32  +\\o1_037  -\\d9_037  +\\x0_6  -\\z0_6
							91.2e4_7  81.2e+4_7  71.2e-4_7  2.00_7  -1.00_7
						`, radices_on).generate()].slice(1, -1).filter((token) => !(token instanceof TokenWhitespace))
						const expected: string[] = `
							12 +12 -12 0123 +0123 -0123 012 +012 -012
							\\b1 \\q0 +\\o1 -\\d9 +\\x0 -\\z0
							91.2e4 81.2e+4 71.2e-4 2.00 -1.00
						`.trim().replace(/\n\t+/g, ' ').split(' ')
						tokens.filter((_, i) => i % 2 === 0).forEach((token, j) => {
							assert.ok(token instanceof TOKEN.TokenNumber, 'this token instanceof TokenNumber')
							assert.ok(tokens[j * 2 + 1] instanceof TOKEN.TokenIdentifier, 'next token instanceof TokenIdentifierBasic')
							assert.strictEqual(token.source, expected[j])
						})
						;`5_5.  -5_5.`.split('  ').forEach((src) => {
							assert.throws(() => [...new Lexer(src, radices_on).generate()], LexError01)
						})
					} else {
						;`
							12_345  +12_345  -12_345  0123_4567  +0123_4567  -0123_4567  012_345_678  +012_345_678  -012_345_678
							\\b1_00  \\q0_32  +\\o1_037  -\\d9_037  +\\x0_6  -\\z0_6
							91.2e4_7  81.2e+4_7  71.2e-4_7  2.00_7  -1.00_7
						`.trim().replace(/\n\t*/g, '  ').split('  ').forEach((src) => {
							assert.throws(() => [...new Lexer(src, radices_on).generate()], LexError01)
						})
					}
				})
				it('`config.languageFeatures.numericSeparators` allows numbers with separators.', () => {
					const source: string = `
						12_345  +12_345  -12_345  0123_4567  +0123_4567  -0123_4567  012_345_678  +012_345_678  -012_345_678
						\\b1_00  \\b0_01  +\\b1_000  -\\b1_000  +\\b0_1  -\\b0_1
						\\q3_20  \\q0_32  +\\q1_032  -\\q1_032  +\\q0_3  -\\q0_3
						\\o3_70  \\o0_37  +\\o1_037  -\\o1_037  +\\o0_6  -\\o0_6
						\\d3_70  \\d0_37  +\\d9_037  -\\d9_037  +\\d0_6  -\\d0_6
						\\xe_70  \\x0_e7  +\\x9_0e7  -\\x9_0e7  +\\x0_6  -\\x0_6
						\\ze_70  \\z0_e7  +\\z9_0e7  -\\z9_0e7  +\\z0_6  -\\z0_6
						5_5.  -5_5.  2.00_7  -2.00_7
						91.2e4_7  91.2e+4_7  91.2e-4_7
					`.trim().replace(/\n\t+/g, '  ')
					;[...new Lexer(source, both_on).generate()].slice(1, -1).filter((token) => !(token instanceof TokenWhitespace)).forEach((token, i) => {
						assert.ok(token instanceof TOKEN.TokenNumber)
						assert.strictEqual(token.source, source.split('  ')[i])
					})
				})
				specify('numeric separator cannot appear at end of token.', () => {
					assert.throws(() => [...new Lexer(`12_345_`, separators_on).generate()], LexError04)
				})
				specify('numeric separators cannot appear consecutively.', () => {
					assert.throws(() => [...new Lexer(`12__345`, separators_on).generate()], LexError04)
				})
				specify('numeric separator at beginning of token is an identifier.', () => {
					assert.throws(() => [...new Lexer(`6.7e_12345`,  separators_on).generate()], LexError05)
					assert.throws(() => [...new Lexer(`6.7e-_12345`, separators_on).generate()], LexError05)
					if (Dev.supports('variables')) {
						function tokenTypeAndSource(index: number, type: NewableFunction, source: string) {
							assert.ok(tokens[index] instanceof type, `Token \`${ tokens[index].source }\` (${ index }) is not instance of ${ type.name }.`)
							assert.strictEqual(tokens[index].source, source)
						}
						const tokens: Token[] = [...new Lexer(`
							_12345  -_12345  6._12345  6.-_12345
						`, separators_on).generate()]
						tokenTypeAndSource(2, TOKEN.TokenIdentifier, `_12345`)

						tokenTypeAndSource(4, TOKEN.TokenPunctuator, `-`)
						tokenTypeAndSource(5, TOKEN.TokenIdentifier, `_12345`)

						tokenTypeAndSource(7, TOKEN.TokenNumber, `6.`)
						tokenTypeAndSource(8, TOKEN.TokenIdentifier, `_12345`)

						tokenTypeAndSource(10, TOKEN.TokenNumber, `6.`)
						tokenTypeAndSource(11, TOKEN.TokenPunctuator, `-`)
						tokenTypeAndSource(12, TOKEN.TokenIdentifier, `_12345`)
					} else {
						assert.throws(() => [...new Lexer(`_12345`,    separators_on).generate()], LexError01)
						assert.throws(() => [...new Lexer(`-_12345`,   separators_on).generate()], LexError01)
						assert.throws(() => [...new Lexer(`6._12345`,  separators_on).generate()], LexError01)
						assert.throws(() => [...new Lexer(`6.-_12345`, separators_on).generate()], LexError01)
					}
				})
			})
		})

		Dev.supports('literalString') && context('recognizes `TokenString` conditions.', () => {
			specify('Basic strings.', () => {
				const tokens: Token[] = [...new Lexer(`
					3 - 50 + * 2
					5 + 03 + '' * 'hello' *  -2
					600  /  3  *  2
					600  /  (3  *  2
					4 * 2 ^ 3
				`, CONFIG_DEFAULT).generate()]
				assert.ok(tokens[22] instanceof TOKEN.TokenString)
				assert.strictEqual(tokens[22].source.length, 2)
				assert.ok(tokens[26] instanceof TOKEN.TokenString)
				assert.strictEqual(tokens[26].source, `'hello'`)
			})
			specify('Escaped characters.', () => {
				const tokenstring: Token = [...new Lexer(`
					'0 \\' 1 \\\\ 2 \\s 3 \\t 4 \\n 5 \\r 6';
				`, CONFIG_DEFAULT).generate()][2]
				assert.strictEqual(tokenstring.source.slice( 3,  5), `\\'`)
				assert.strictEqual(tokenstring.source.slice( 8, 10), `\\\\`)
				assert.strictEqual(tokenstring.source.slice(13, 15), `\\s`)
				assert.strictEqual(tokenstring.source.slice(18, 20), `\\t`)
				assert.strictEqual(tokenstring.source.slice(23, 25), `\\n`)
				assert.strictEqual(tokenstring.source.slice(28, 30), `\\r`)
			})
			specify('Escaped character sequences.', () => {
				const tokenstring: Token = [...new Lexer(`
					'0 \\u{24} 1 \\u{005f} 2 \\u{} 3';
				`, CONFIG_DEFAULT).generate()][2]
				assert.strictEqual(tokenstring.source.slice( 3,  9), `\\u{24}`)
				assert.strictEqual(tokenstring.source.slice(12, 20), `\\u{005f}`)
				assert.strictEqual(tokenstring.source.slice(23, 27), `\\u{}`)
			})
			specify('Line continuation.', () => {
				const tokenstring: Token = [...new Lexer(`
					'012\\
					345
					678';
				`, CONFIG_DEFAULT).generate()][2]
				assert.strictEqual(tokenstring.source.slice( 4,  6), `\\\n`)
				assert.strictEqual(tokenstring.source.slice(14, 15), `\n`)
			})
			specify('Strings containing comment syntax.', () => {
				;[`
					'Here is a string % that contains a line comment start marker.'
				`, `
					'Here is a string {% that contains %} a multiline comment.'
				`, `
					'Here is a string {% that contains a comment start marker but no end.'
				`].map((source) => new Lexer(source, CONFIG_DEFAULT)).forEach((lexer) => {
					assert.doesNotThrow(() => [...lexer.generate()])
				})
			})
			specify('Invalid escape sequences.', () => {
				;[`
					'a string literal with \\u{6g} an invalid escape sequence'
				`, `
					'a string literal with \\u{61 an invalid escape sequence'
				`].map((source) => new Lexer(source, CONFIG_DEFAULT)).forEach((lexer) => {
					assert.throws(() => [...lexer.generate()], LexError03)
				})
			})
		})

		Dev.supports('literalTemplate') && context('recognizes `TokenTemplate` conditions.', () => {
			specify('Basic templates.', () => {
				const tokens: Token[] = [...new Lexer(`
					600  /  '''''' * 3 + '''hello''' *  2
				`, CONFIG_DEFAULT).generate()]
				assert.ok(tokens[ 6] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[ 6] as TOKEN.TokenTemplate).position, TemplatePosition.FULL)
				assert.strictEqual(tokens[ 6].source.length, 6)
				assert.ok(tokens[14] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[14] as TOKEN.TokenTemplate).position, TemplatePosition.FULL)
				assert.strictEqual(tokens[14].source, `'''hello'''`)
			})
			specify('Template interpolation.', () => {
				const tokens: Token[] = [...new Lexer(`
					3 + '''head{{ * 2
					3 + }}midl{{ * 2
					3 + }}tail''' * 2
				`, CONFIG_DEFAULT).generate()]
				assert.ok(tokens[ 6] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[ 6] as TOKEN.TokenTemplate).position, TemplatePosition.HEAD)
				assert.strictEqual(tokens[ 6].source, `'''head{{`)
				assert.ok(tokens[16] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[16] as TOKEN.TokenTemplate).position, TemplatePosition.MIDDLE)
				assert.strictEqual(tokens[16].source, `}}midl{{`)
				assert.ok(tokens[26] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[26] as TOKEN.TokenTemplate).position, TemplatePosition.TAIL)
				assert.strictEqual(tokens[26].source, `}}tail'''`)
			})
			specify('Empty/comment interpolation.', () => {
				const tokens: Token[] = [...new Lexer(`
					'''abc{{ }}def'''
					'''ghi{{}}jkl'''
					'''mno{{ {% pqr %} }}stu'''
				`, CONFIG_DEFAULT).generate()]
				assert.ok(tokens[ 2] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[ 2] as TOKEN.TokenTemplate).position, TemplatePosition.HEAD)
				assert.strictEqual(tokens[ 2].source, `'''abc{{`)
				assert.ok(tokens[ 4] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[ 4] as TOKEN.TokenTemplate).position, TemplatePosition.TAIL)
				assert.strictEqual(tokens[ 4].source, `}}def'''`)
				assert.ok(tokens[ 6] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[ 6] as TOKEN.TokenTemplate).position, TemplatePosition.HEAD)
				assert.strictEqual(tokens[ 6].source, `'''ghi{{`)
				assert.ok(tokens[ 7] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[ 7] as TOKEN.TokenTemplate).position, TemplatePosition.TAIL)
				assert.strictEqual(tokens[ 7].source, `}}jkl'''`)
				assert.ok(tokens[ 9] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[ 9] as TOKEN.TokenTemplate).position, TemplatePosition.HEAD)
				assert.strictEqual(tokens[ 9].source, `'''mno{{`)
				assert.ok(tokens[13] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[13] as TOKEN.TokenTemplate).position, TemplatePosition.TAIL)
				assert.strictEqual(tokens[13].source, `}}stu'''`)
			})
			specify('Nested interpolation.', () => {
				const tokens: Token[] = [...new Lexer(`
					1 + '''head1 {{ 2 + '''head2 {{ 3 ^ 3 }} tail2''' * 2 }} tail1''' * 1
				`, CONFIG_DEFAULT).generate()]
				assert.ok(tokens[ 6] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[ 6] as TOKEN.TokenTemplate).position, TemplatePosition.HEAD)
				assert.strictEqual(tokens[ 6].source, `'''head1 {{`)
				assert.ok(tokens[12] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[12] as TOKEN.TokenTemplate).position, TemplatePosition.HEAD)
				assert.strictEqual(tokens[12].source, `'''head2 {{`)
				assert.ok(tokens[20] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[20] as TOKEN.TokenTemplate).position, TemplatePosition.TAIL)
				assert.strictEqual(tokens[20].source, `}} tail2'''`)
				assert.ok(tokens[26] instanceof TOKEN.TokenTemplate)
				assert.strictEqual((tokens[26] as TOKEN.TokenTemplate).position, TemplatePosition.TAIL)
				assert.strictEqual(tokens[26].source, `}} tail1'''`)
			})
			specify('Non-escaped characters.', () => {
				const tokentemplate: Token = [...new Lexer(`
					'''0 \\' 1 \\\\ 2 \\s 3 \\t 4 \\n 5 \\r 6 \\\\\` 7''';
				`, CONFIG_DEFAULT).generate()][2]
				assert.strictEqual(tokentemplate.source.slice( 5,  7), `\\'`)
				assert.strictEqual(tokentemplate.source.slice(10, 12), `\\\\`)
				assert.strictEqual(tokentemplate.source.slice(15, 17), `\\s`)
				assert.strictEqual(tokentemplate.source.slice(20, 22), `\\t`)
				assert.strictEqual(tokentemplate.source.slice(25, 27), `\\n`)
				assert.strictEqual(tokentemplate.source.slice(30, 32), `\\r`)
				assert.strictEqual(tokentemplate.source.slice(35, 38), `\\\\\``)
			})
			specify('Non-escaped character sequences.', () => {
				const tokentemplate: Token = [...new Lexer(`
					'''0 \\u{24} 1 \\u{005f} 2 \\u{} 3''';
				`, CONFIG_DEFAULT).generate()][2]
				assert.strictEqual(tokentemplate.source.slice( 5, 11), `\\u{24}`)
				assert.strictEqual(tokentemplate.source.slice(14, 22), `\\u{005f}`)
				assert.strictEqual(tokentemplate.source.slice(25, 29), `\\u{}`)
			})
			specify('Line breaks.', () => {
				const tokentemplate: Token = [...new Lexer(`
					'''012\\
					345
					678''';
				`, CONFIG_DEFAULT).generate()][2]
				assert.strictEqual(tokentemplate.source.slice( 6,  8), `\\\n`)
				assert.strictEqual(tokentemplate.source.slice(16, 17), `\n`)
			})
			describe('Invalid characters at end of template.', () => {
				it('should not recognize `{{{` at end of head/middle.', () => {
					;[`
						'''template-head that ends with a single open brace {{{
					`, `
						}}template-middle that ends with a single open brace {{{
					`].map((source) => new Lexer(source, CONFIG_DEFAULT)).forEach((lexer) => {
						assert.throws(() => [...lexer.generate()], LexError01) // TODO change to parse error when `{` becomes punctuator
					})
				})
				it('should not recognize `\'\'\'\'` at end of full/tail.', () => {
					;[`
						'''template-full that ends with a single apostrophe ''''
					`, `
						}}template-tail that ends with a single apostrophe ''''
					`].map((source) => new Lexer(source, CONFIG_DEFAULT)).forEach((lexer) => {
						assert.throws(() => [...lexer.generate()], LexError02)
					})
				})
			})
		})

		context('recgnizes `TokenComment` conditions.', () => {
			const comments_off: SolidConfig = {
				...CONFIG_DEFAULT,
				languageFeatures: {
					...CONFIG_DEFAULT.languageFeatures,
					comments: false,
				},
			}
			context('TokenCommentLine', () => {
				specify('Empty line comment.', () => {
					const comment: Token = [...new Lexer(`
						%
						8;
					`, CONFIG_DEFAULT).generate()][2]
					assert.ok(comment instanceof TOKEN.TokenCommentLine)
					assert.strictEqual(comment.source, '%\n')
				})
				specify('Basic line comment.', () => {
					assert.ok([...new Lexer(`
						500  +  30; ;  % line comment  *  2
						8;
					`, CONFIG_DEFAULT).generate()][11] instanceof TOKEN.TokenCommentLine)
				})
				specify('Line comment at end of file not followed by LF.', () => {
					assert.doesNotThrow(() => [...new Lexer(`
						% line comment not followed by LF
					`.trimEnd(), CONFIG_DEFAULT).generate()])
				})
				it('throws when `config.languageFeatures.comments` is turned off.', () => {
					assert.throws(() => [...new Lexer(`
						%
						8;
					`, comments_off).generate()], LexError01)
				})
			})
			context('TokenCommentMulti', () => {
				specify('Empty multiline comment.', () => {
					const tokens: Token[] = [...new Lexer(`
						%%%%
						%% %%
					`, CONFIG_DEFAULT).generate()]
					assert.ok(tokens[2] instanceof TOKEN.TokenCommentMulti)
					assert.ok(tokens[4] instanceof TOKEN.TokenCommentMulti)
					assert.strictEqual(tokens[2].source, '%%%%')
					assert.strictEqual(tokens[4].source, '%% %%')
				})
				specify('Nonempty multiline comment.', () => {
					const comment: Token = [...new Lexer(`
						%% multiline
						that has contents
						comment %%
					`, CONFIG_DEFAULT).generate()][2]
					assert.ok(comment instanceof TOKEN.TokenCommentMulti)
				})
				specify('Simulate inline documentation comment.', () => {
					const tokens: Token[] = [...new Lexer(Util.dedent(`
						%%% The third power of 2. %%
						8;
					`), CONFIG_DEFAULT).generate()]
					assert.ok(tokens[2] instanceof TOKEN.TokenCommentMulti)
					assert.strictEqual(tokens[2].source, `
						%%% The third power of 2. %%
					`.trim())
					assert.strictEqual(tokens[4].source, '8')
				})
				specify('Simulate block documentation comment.', () => {
					const tokens: Token[] = [...new Lexer(Util.dedent(`
						%%%
						The third power of 2.
						%%%
						8;
					`), CONFIG_DEFAULT).generate()]
					assert.ok(tokens[2] instanceof TOKEN.TokenCommentMulti)
					assert.ok(tokens[3] instanceof TOKEN.TokenCommentLine)
					assert.strictEqual(tokens[2].source, Util.dedent(`
						%%%
						The third power of 2.
						%%
					`).trim())
					assert.strictEqual(tokens[3].source, `%\n`)
					assert.strictEqual(tokens[4].source, '8')
				})
				it('throws when `config.languageFeatures.comments` is turned off.', () => {
					assert.throws(() => [...new Lexer(`
						%% multiline
						comment %%
					`, comments_off).generate()], LexError01)
				})
			})
		})
	})
})
