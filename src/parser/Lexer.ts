import {
	Char,
	Token,
	Lexer,
} from '@chharvey/parser';

import {
	SolidConfig,
	CONFIG_DEFAULT,
	Dev,
} from '../core/';
import type {
	Punctuator,
} from './Token';
import * as TOKEN from './Token';

import {
	LexError03,
} from '../error/';



export class LexerSolid extends Lexer {
	private static readonly PUNCTUATORS_3: readonly Punctuator[] = TOKEN.TokenPunctuator.PUNCTUATORS.filter((p) => p.length === 3)
	private static readonly PUNCTUATORS_2: readonly Punctuator[] = TOKEN.TokenPunctuator.PUNCTUATORS.filter((p) => p.length === 2)
	private static readonly PUNCTUATORS_1: readonly Punctuator[] = TOKEN.TokenPunctuator.PUNCTUATORS.filter((p) => p.length === 1)
	private static readonly BASES_KEYS: readonly string[] = [...TOKEN.TokenNumber.BASES.keys()]
	private static readonly DIGITS_DEFAULT: readonly string[] = TOKEN.TokenNumber.DIGITS.get(TOKEN.TokenNumber.RADIX_DEFAULT)!


	/** A set of all unique identifiers in the program, for optimization purposes. */
	private _ids: Set<string> = new Set()

	/**
	 * Construct a new LexerSolid object.
	 * @param source - the source text
	 * @param config - The configuration settings for an instance program.
	 */
	constructor (
		source: string,
		readonly config: SolidConfig = CONFIG_DEFAULT,
	) {
		super(source)
	}

	protected generate_do(): Token | null {
			let token: Token;
			if (Char.inc(LexerSolid.PUNCTUATORS_3, this.c0, this.c1, this.c2)) {
				token = new TOKEN.TokenPunctuator(this, 3n)
			} else if (Char.inc(LexerSolid.PUNCTUATORS_2, this.c0, this.c1)) {
				token = new TOKEN.TokenPunctuator(this, 2n)
			} else if (Char.inc(LexerSolid.PUNCTUATORS_1, this.c0)) {
				/* we found a punctuator or a number literal prefixed with a unary operator */
				if (Char.inc(TOKEN.TokenNumber.UNARY, this.c0)) {
					if (Char.inc(LexerSolid.DIGITS_DEFAULT, this.c1)) {
						/* a number literal with a unary operator and without an explicit radix */
						token = new TOKEN.TokenNumber(this, true)
					} else if (this.config.languageFeatures.integerRadices && Char.eq(TOKEN.TokenNumber.ESCAPER, this.c1)) {
						if (Char.inc(LexerSolid.BASES_KEYS, this.c2)) {
							/* a number literal with a unary operator and with an explicit radix */
							token = new TOKEN.TokenNumber(this, true, true)
						} else {
							throw new LexError03(`${ this.c0.source }${ this.c1 && this.c1.source || '' }${ this.c2 && this.c2.source || '' }`, this.c0.line_index, this.c0.col_index)
						}
					} else {
						/* a punctuator "+" or "-" */
						token = new TOKEN.TokenPunctuator(this)
					}
				} else {
					/* a different punctuator */
					token = new TOKEN.TokenPunctuator(this)
				}

			} else if (TOKEN.TokenKeyword.CHAR.test(this.c0.source)) {
				/* we found a keyword or a basic identifier */
				const buffer: Char[] = [this.c0]
				this.advance()
				while (!this.isDone && (Dev.supports('variables') ? TOKEN.TokenIdentifierBasic.CHAR_REST : TOKEN.TokenKeyword.CHAR).test(this.c0.source)) {
					buffer.push(this.c0)
					this.advance()
				}
				const bufferstring: string = buffer.map((char) => char.source).join('')
				if ((TOKEN.TokenKeyword.KEYWORDS as string[]).includes(bufferstring)) {
					token = new TOKEN.TokenKeyword(this, buffer[0], ...buffer.slice(1))
				} else if (Dev.supports('variables')) {
					token = new TOKEN.TokenIdentifierBasic(this, buffer[0], ...buffer.slice(1))
					this.setIdentifierValue(token as TOKEN.TokenIdentifierBasic);
				} else {
					throw new Error(`Identifier \`${ bufferstring }\` not yet allowed.`)
				}
			} else if (Dev.supports('variables') && TOKEN.TokenIdentifierBasic.CHAR_START.test(this.c0.source)) {
				/* we found a basic identifier */
				token = new TOKEN.TokenIdentifierBasic(this)
				this.setIdentifierValue(token as TOKEN.TokenIdentifierBasic);
			} else if (Dev.supports('variables') && Char.eq(TOKEN.TokenIdentifierUnicode.DELIM, this.c0)) {
				/* we found a unicode identifier */
				token = new TOKEN.TokenIdentifierUnicode(this)
				this.setIdentifierValue(token as TOKEN.TokenIdentifierUnicode);

			} else if (Char.inc(LexerSolid.DIGITS_DEFAULT, this.c0)) {
				/* a number literal without a unary operator and without an explicit radix */
				token = new TOKEN.TokenNumber(this, false)
			} else if (this.config.languageFeatures.integerRadices && Char.eq(TOKEN.TokenNumber.ESCAPER, this.c0)) {
				if (Char.inc(LexerSolid.BASES_KEYS, this.c1)) {
					/* a number literal without a unary operator and with an explicit radix */
					token = new TOKEN.TokenNumber(this, false, true)
				} else {
					throw new LexError03(`${this.c0.source}${this.c1 && this.c1.source || ''}`, this.c0.line_index, this.c0.col_index)
				}

			} else if (Dev.supports('literalTemplate') && Char.eq(TOKEN.TokenTemplate.DELIM, this.c0, this.c1, this.c2)) {
				/* we found a template literal full or head */
				token = new TOKEN.TokenTemplate(this, TOKEN.TokenTemplate.DELIM)
			} else if (Dev.supports('literalTemplate') && Char.eq(TOKEN.TokenTemplate.DELIM_INTERP_END, this.c0, this.c1)) {
				/* we found a template literal middle or tail */
				token = new TOKEN.TokenTemplate(this, TOKEN.TokenTemplate.DELIM_INTERP_END)
			} else if (Dev.supports('literalString') && Char.eq(TOKEN.TokenString.DELIM, this.c0)) {
				/* we found a string literal */
				token = new TOKEN.TokenString(this)

			} else if (this.config.languageFeatures.comments && Char.eq(TOKEN.TokenCommentMulti.DELIM_START, this.c0, this.c1)) {
				/* we found a multiline comment */
				token = new TOKEN.TokenCommentMulti(this)
			} else if (this.config.languageFeatures.comments && Char.eq(TOKEN.TokenCommentLine.DELIM_START, this.c0)) {
				/* we found a single-line comment */
				token = new TOKEN.TokenCommentLine(this)

			} else {
				return null
			}
			return token
	}

	/**
	 * Sets a unique integer value to an Identifier token to optimize performance.
	 * @param id_token the Identifier token
	 */
	private setIdentifierValue(id_token: TOKEN.TokenIdentifier): void {
		this._ids.add(id_token.source);
		return id_token.setValue(BigInt([...this._ids].indexOf(id_token.source)));
	}
}
