import {
	Filebound,
	Char,
	Token,
	TokenComment,
	Lexer,
	LexError02,
} from '@chharvey/parser';

import {
	SolidConfig,
	CONFIG_DEFAULT,
	Dev,
	Util,
} from '../core/';

import type {
	LexerSolid,
} from './Lexer';

import {
	LexError03,
	LexError04,
	LexError05,
} from '../error/';



export enum Punctuator {
	// grouping
		GRP_OPN = '(',
		GRP_CLS = ')',
		BRAK_OPN = '[',   // Dev.supports('literalCollection')
		BRAK_CLS = ']',   // Dev.supports('literalCollection')
		COMMA    = ',',   // Dev.supports('literalCollection')
		MAPTO    = '|->', // Dev.supports('literalCollection')
	// unary
		NOT = '!',
		EMP = '?',
		AFF = '+',
		NEG = '-',
		ORNULL = '!', // Dev.supports('typingExplicit')
	// binary
		EXP  = '^',
		MUL  = '*',
		DIV  = '/',
		ADD  = '+',
		SUB  = '-',
		LT   = '<',
		GT   = '>',
		LE   = '<=',
		GE   = '>=',
		NLT  = '!<',
		NGT  = '!>',
		EQ   = '==',
		NEQ  = '!=',
		AND  = '&&',
		NAND = '!&',
		OR   = '||',
		NOR  = '!|',
		INTER = '&', // Dev.supports('typingExplicit')
		UNION = '|', // Dev.supports('typingExplicit')
	// statement
		ENDSTAT = ';',
		ISTYPE  = ':', // Dev.supports('typingExplicit')
		ASSIGN  = '=', // Dev.supportsAll('variables', 'typingExplicit', 'literalCollection')
}

export enum Keyword {
	// literal
		NULL  = 'null',
		BOOL  = 'bool',   // Dev.supports('typingExplicit')
		FALSE = 'false',
		TRUE  = 'true',
		INT   = 'int',    // Dev.supports('typingExplicit')
		FLOAT = 'float',  // Dev.supports('typingExplicit')
		OBJ   = 'obj',    // Dev.supports('typingExplicit')
	// operator
		IS   = 'is',
		ISNT = 'isnt',
		IF   = 'if',
		THEN = 'then',
		ELSE = 'else',
	// storage
		LET  = 'let',  // Dev.supports('variables')
		TYPE = 'type', // Dev.supportsAll('variables', 'typingExplicit')
	// modifier
		UNFIXED = 'unfixed', // Dev.supports('variables')
}



export type RadixType = 2n|4n|8n|10n|16n|36n

export enum TemplatePosition {
	FULL,
	HEAD,
	MIDDLE,
	TAIL,
}

export type CookValueType = string|number|bigint|boolean|null



export abstract class TokenSolid extends Token {
	/**
	 * Return this Token’s cooked value.
	 * The cooked value is the computed or evaluated contents of this Token,
	 * to be sent to the parser and compiler.
	 * If this Token is not to be sent to the parser, then return `null`.
	 * @returns              the computed value of this token, or `null`
	 */
	abstract cook(): CookValueType;
}



export class TokenPunctuator extends TokenSolid {
	static readonly PUNCTUATORS: readonly Punctuator[] = [...new Set( // remove duplicates
		Object.values(Punctuator).filter((p) => Dev.supports('variables') ? true : ![
			Punctuator.ASSIGN,
		].includes(p)).filter((p) => Dev.supports('literalCollection') ? true : ![
			Punctuator.BRAK_OPN,
			Punctuator.BRAK_CLS,
			Punctuator.COMMA,
			Punctuator.MAPTO,
		].includes(p))
	)]
	// declare readonly source: Punctuator; // NB: https://github.com/microsoft/TypeScript/issues/40220
	constructor (lexer: Lexer, count: 1n | 2n | 3n | 4n = 1n) {
		super('PUNCTUATOR', lexer, ...lexer.advance())
		if (count >= 4n) {
			this.advance(2n)
		} else if (count >= 3n) {
			this.advance(2n)
		} else if (count >= 2n) {
			this.advance()
		}
	}
	cook(): bigint {
		return BigInt(TokenPunctuator.PUNCTUATORS.indexOf(this.source as Punctuator))
	}
}
export class TokenKeyword extends TokenSolid {
	private static readonly MINIMUM_VALUE: 0x80n = 0x80n
	static readonly CHAR: RegExp = /^[a-z]$/
	static readonly KEYWORDS: readonly Keyword[] = [...new Set<Keyword>( // remove duplicates
		Object.values(Keyword).filter((kw) => Dev.supports('variables') ? true : ![
			Keyword.LET,
			Keyword.UNFIXED,
		].includes(kw))
	)]
	// declare readonly source: Keyword; // NB: https://github.com/microsoft/TypeScript/issues/40220
	constructor (lexer: Lexer, start_char: Char, ...more_chars: Char[]) {
		super('KEYWORD', lexer, start_char, ...more_chars)
	}
	cook(): bigint {
		return BigInt(TokenKeyword.KEYWORDS.indexOf(this.source as Keyword)) + TokenKeyword.MINIMUM_VALUE
	}
}
export abstract class TokenIdentifier extends TokenSolid {
	private static readonly MINIMUM_VALUE: 0x100n = 0x100n
	/**
	 * The cooked value of this Token.
	 * If the token is a keyword, the cooked value is its contents.
	 * If the token is an identifier, the cooked value is set by a {@link Screener},
	 * which indexes unique identifier tokens.
	 */
	private _cooked: bigint|null;
	constructor (lexer: Lexer, start_char: Char, ...more_chars: Char[]) {
		super('IDENTIFIER', lexer, start_char, ...more_chars)
		this._cooked = null
	}
	/**
	 * Set the numeric integral value of this Token.
	 * The value must be 128 or higher.
	 * This operation can only be done once.
	 * @param value - the value to set, unique among all identifiers in a program
	 */
	/** @final */ setValue(value: bigint): void {
		if (this._cooked === null) {
			this._cooked = value + TokenIdentifier.MINIMUM_VALUE
		}
	}
	/** @final */ cook(): bigint|null {
		return this._cooked
	}
}
export class TokenIdentifierBasic extends TokenIdentifier {
	static readonly CHAR_START: RegExp = /^[A-Za-z_]$/
	static readonly CHAR_REST : RegExp = /^[A-Za-z0-9_]$/
	constructor (lexer: Lexer, start_char?: Char, ...more_chars: Char[]) {
		if (start_char) {
			super(lexer, start_char, ...more_chars)
		} else {
			super(lexer, ...lexer.advance())
			while (!this.lexer.isDone && TokenIdentifierBasic.CHAR_REST.test(this.lexer.c0.source)) {
				this.advance()
			}
		}
	}
}
export class TokenIdentifierUnicode extends TokenIdentifier {
	static readonly DELIM: '`' = '`'
	constructor (lexer: Lexer) {
		super(lexer, ...lexer.advance())
		while (!this.lexer.isDone && !Char.eq(TokenIdentifierUnicode.DELIM, this.lexer.c0)) {
			if (Char.eq(Filebound.EOT, this.lexer.c0)) {
				throw new LexError02(this)
			}
			this.advance()
		}
		// add ending delim to token
		this.advance()
	}
}
export class TokenNumber extends TokenSolid {
	static readonly RADIX_DEFAULT: 10n = 10n
	static readonly ESCAPER:   '\\' = '\\'
	static readonly SEPARATOR: '_' = '_'
	static readonly POINT:     '.' = '.'
	static readonly EXPONENT:  'e' = 'e'
	static readonly UNARY: readonly Punctuator[] = [
		Punctuator.AFF,
		Punctuator.NEG,
	]
	static readonly BASES: ReadonlyMap<string, RadixType> = new Map<string, RadixType>([
		['b',  2n],
		['q',  4n],
		['o',  8n],
		['d', 10n],
		['x', 16n],
		['z', 36n],
	])
	static readonly DIGITS: ReadonlyMap<RadixType, readonly string[]> = new Map<RadixType, readonly string[]>([
		[ 2n, '0 1'                                                                     .split(' ')],
		[ 4n, '0 1 2 3'                                                                 .split(' ')],
		[ 8n, '0 1 2 3 4 5 6 7'                                                         .split(' ')],
		[10n, '0 1 2 3 4 5 6 7 8 9'                                                     .split(' ')],
		[16n, '0 1 2 3 4 5 6 7 8 9 a b c d e f'                                         .split(' ')],
		[36n, '0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z' .split(' ')],
	])
	/**
	 * Compute the token worth of a `TokenNumber` token in Integer format.
	 * @param   text  - the string to compute
	 * @param   radix - the base in which to compute
	 * @param   allow_separators - Should numeric separators be allowed?
	 * @returns         the mathematical value of the string in the given base
	 */
	static tokenWorthInt(
		text: string,
		radix: RadixType = TokenNumber.RADIX_DEFAULT,
		allow_separators: SolidConfig['languageFeatures']['numericSeparators'] = CONFIG_DEFAULT.languageFeatures.numericSeparators,
	): number {
		if (text[0] === Punctuator.AFF) { return  TokenNumber.tokenWorthInt(text.slice(1), radix, allow_separators) }
		if (text[0] === Punctuator.NEG) { return -TokenNumber.tokenWorthInt(text.slice(1), radix, allow_separators) }
		if (allow_separators && text[text.length-1] === TokenNumber.SEPARATOR) {
			text = text.slice(0, -1)
		}
		if (text.length === 0) throw new Error('Cannot compute mathematical value of empty string.')
		if (text.length === 1) {
			const digitvalue: number = parseInt(text, Number(radix))
			if (Number.isNaN(digitvalue)) throw new Error(`Invalid number format: \`${text}\``)
			return digitvalue
		}
		return Number(radix) *
			TokenNumber.tokenWorthInt(text.slice(0, -1),     radix, allow_separators) +
			TokenNumber.tokenWorthInt(text[text.length - 1], radix, allow_separators)
	}
	/**
	 * Compute the token worth of a `TokenNumber` token in Float format.
	 * @param   text  - the string to compute
	 * @param   allow_separators - Should numeric separators be allowed?
	 * @returns the mathematical value of the string in the given base
	 */
	static tokenWorthFloat(
		text: string,
		allow_separators: SolidConfig['languageFeatures']['numericSeparators'] = CONFIG_DEFAULT.languageFeatures.numericSeparators,
	): number {
		const base:       number = Number(TokenNumber.RADIX_DEFAULT)
		const pointindex: number = text.indexOf(TokenNumber.POINT)
		const expindex:   number = text.indexOf(TokenNumber.EXPONENT)
		const wholepart:  string = text.slice(0, pointindex)
		const fracpart:   string = ((expindex < 0) ? text.slice(pointindex + 1) : text.slice(pointindex + 1, expindex)) || '0'
		const exppart:    string =  (expindex < 0) ? '0'                        : text.slice(expindex   + 1)
		const wholevalue: number =                  TokenNumber.tokenWorthInt(wholepart, TokenNumber.RADIX_DEFAULT, allow_separators)
		const fracvalue:  number =                  TokenNumber.tokenWorthInt(fracpart,  TokenNumber.RADIX_DEFAULT, allow_separators) * base ** -fracpart.length
		const expvalue:   number = parseFloat(`1e${ TokenNumber.tokenWorthInt(exppart,   TokenNumber.RADIX_DEFAULT, allow_separators) }`) // HACK: more accurate than `base ** exp`
		// const expvalue: number = base ** TokenNumber.tokenWorthInt(exppart, TokenNumber.RADIX_DEFAULT, allow_separators)
		return (wholevalue + fracvalue) * expvalue
	}
	declare protected readonly lexer: LexerSolid;
	private readonly has_unary: boolean;
	private readonly has_radix: boolean;
	private readonly radix: RadixType;
	constructor (lexer: LexerSolid, has_unary: boolean, has_radix: boolean = false) {
		// NB https://github.com/microsoft/TypeScript/issues/8277
		const buffer: Char[] = []
		if (has_unary) { // prefixed with leading unary operator "+" or "-"
			buffer.push(...lexer.advance())
		}
		const radix: RadixType = has_radix ? TokenNumber.BASES.get(lexer.c1 !.source) ! : TokenNumber.RADIX_DEFAULT
		const digits: readonly string[] = TokenNumber.DIGITS.get(radix) !
		if (has_radix) { // an explicit base
			if (!Char.inc(digits, lexer.c2)) {
				throw new LexError03(`${lexer.c0.source}${lexer.c1 !.source}`, lexer.c0.line_index, lexer.c0.col_index)
			}
			buffer.push(...lexer.advance(3n))
		} else { // implicit default base
			buffer.push(...lexer.advance())
		}
		super('NUMBER', lexer, buffer[0], ...buffer.slice(1))
		this.has_unary = has_unary
		this.has_radix = has_radix
		this.radix     = radix
		this.lexDigitSequence(digits)
		if (!this.has_radix && Char.eq(TokenNumber.POINT, this.lexer.c0)) {
			this.advance()
			if (Char.inc(digits, this.lexer.c0)) {
				this.lexDigitSequence(digits)
				if (Char.eq(TokenNumber.EXPONENT, this.lexer.c0)) {
					const err: LexError05 = new LexError05(this.lexer.c0)
					this.advance()
					if (Char.inc(TokenNumber.UNARY, this.lexer.c0) && Char.inc(digits, this.lexer.c1)) {
						this.advance(2n)
						this.lexDigitSequence(digits)
					} else if (Char.inc(digits, this.lexer.c0)) {
						this.advance()
						this.lexDigitSequence(digits)
					} else {
						throw err
					}
				}
			}
		}
	}
	private lexDigitSequence(digits: readonly string[]): void {
		const allowedchars: string[] = [
			...digits,
			...(this.lexer.config.languageFeatures.numericSeparators ? [TokenNumber.SEPARATOR] : [])
		]
		while (!this.lexer.isDone && Char.inc(allowedchars, this.lexer.c0)) {
			if (Char.inc(digits, this.lexer.c0)) {
				this.advance()
			} else if (this.lexer.config.languageFeatures.numericSeparators && Char.eq(TokenNumber.SEPARATOR, this.lexer.c0)) {
				if (Char.inc(digits, this.lexer.c1)) {
					this.advance(2n)
				} else {
					throw new LexError04(Char.eq(TokenNumber.SEPARATOR, this.lexer.c1) ? this.lexer.c1 ! : this.lexer.c0)
				}
			}
		}
	}
	cook(): number {
		let text: string = this.source
		const multiplier: number = (text[0] === Punctuator.NEG) ? -1 : 1
		if (this.has_unary) text = text.slice(1) // cut off unary, if any
		if (this.has_radix) text = text.slice(2) // cut off radix, if any
		return multiplier * (this.isFloat
			? TokenNumber.tokenWorthFloat(text,             this.lexer.config.languageFeatures.numericSeparators)
			: TokenNumber.tokenWorthInt  (text, this.radix, this.lexer.config.languageFeatures.numericSeparators)
		)
	}
	/**
	 * Is this token a floating-point number?
	 * @returns whether this token contains a decimal point
	 */
	get isFloat(): boolean {
		return this.source.indexOf(TokenNumber.POINT) > 0
	}
}
export class TokenString extends TokenSolid {
	static readonly DELIM:   '\'' = '\''
	static readonly ESCAPER: '\\' = '\\'
	static readonly ESCAPES: readonly string[] = [TokenString.DELIM, TokenString.ESCAPER, 's','t','n','r']
	/**
	 * Compute the token worth of a `TokenString` token or any segment of such token.
	 * @param   text - the string to compute
	 * @param   allow_separators - Should numeric separators be allowed?
	 * @returns        the string value of the argument, a sequence of code units
	 */
	private static tokenWorth(
		text: string,
		allow_separators: SolidConfig['languageFeatures']['numericSeparators'] = CONFIG_DEFAULT.languageFeatures.numericSeparators,
	): number[] {
		if (text.length === 0) return []
		if (TokenString.ESCAPER === text[0]) {
			/* possible escape or line continuation */
			if (TokenString.ESCAPES.includes(text[1])) {
				/* an escaped character literal */
				return [
					new Map<string, number>([
						[TokenString.DELIM, TokenString.DELIM.charCodeAt(0)!],
						[TokenString.ESCAPER , 0x5c],
						['s'                 , 0x20],
						['t'                 , 0x09],
						['n'                 , 0x0a],
						['r'                 , 0x0d],
					]).get(text[1]) !,
					...TokenString.tokenWorth(text.slice(2), allow_separators),
				]

			} else if ('u{' === `${text[1]}${text[2]}`) {
				/* an escape sequence */
				const sequence: RegExpMatchArray = text.match(/\\u{[0-9a-f_]*}/) !
				return [
					...Util.utf16Encoding(TokenNumber.tokenWorthInt(sequence[0].slice(3, -1) || '0', 16n, allow_separators)),
					...TokenString.tokenWorth(text.slice(sequence[0].length), allow_separators),
				]

			} else if ('\n' === text[1]) {
				/* a line continuation (LF) */
				return [0x20, ...TokenString.tokenWorth(text.slice(2), allow_separators)]

			} else {
				/* a backslash escapes the following character */
				return [
					text.charCodeAt(1),
					...TokenString.tokenWorth(text.slice(2), allow_separators),
				]
			}
		} else return [
			text.charCodeAt(0),
			...TokenString.tokenWorth(text.slice(1), allow_separators),
		]
	}
	declare protected readonly lexer: LexerSolid;
	constructor (lexer: LexerSolid) {
		super('STRING', lexer, ...lexer.advance())
		while (!this.lexer.isDone && !Char.eq(TokenString.DELIM, this.lexer.c0)) {
			if (Char.eq(Filebound.EOT, this.lexer.c0)) {
				throw new LexError02(this)
			}
			if (Char.eq(TokenString.ESCAPER, this.lexer.c0)) {
				/* possible escape or line continuation */
				if (Char.inc(TokenString.ESCAPES, this.lexer.c1)) {
					/* an escaped character literal */
					this.advance(2n)

				} else if (Char.eq('u{', this.lexer.c1, this.lexer.c2)) {
					/* an escape sequence */
					const digits: readonly string[] = TokenNumber.DIGITS.get(16n) !
					let cargo: string = `${this.lexer.c0.source}${this.lexer.c1 !.source}${this.lexer.c2 !.source}`
					this.advance(3n)
					if (Char.inc(digits, this.lexer.c0)) {
						cargo += this.lexer.c0.source
						this.advance()
						while(!this.lexer.isDone && Char.inc([...digits, TokenNumber.SEPARATOR], this.lexer.c0)) {
							if (Char.inc(digits, this.lexer.c0)) {
								cargo += this.lexer.c0.source
								this.advance()
							} else if (Char.eq(TokenNumber.SEPARATOR, this.lexer.c0)) {
								if (Char.inc(digits, this.lexer.c1)) {
									cargo += `${this.lexer.c0.source}${this.lexer.c1 !.source}`
									this.advance(2n)
								} else {
									throw new LexError04(Char.eq(TokenNumber.SEPARATOR, this.lexer.c1) ? this.lexer.c1 ! : this.lexer.c0)
								}
							}
						}
					}
					// add ending escape delim
					if (Char.eq('}', this.lexer.c0)) {
						this.advance()
					} else {
						throw new LexError03(cargo, this.lexer.c0.line_index, this.lexer.c0.col_index)
					}

				} else if (Char.eq('\n', this.lexer.c1)) {
					/* a line continuation (LF) */
					this.advance(2n)

				} else {
					/* a backslash escapes the following character */
					this.advance()
				}
			} else {
				this.advance()
			}
		}
		// add ending delim to token
		this.advance()
	}
	cook(): string {
		return String.fromCharCode(...TokenString.tokenWorth(
			this.source.slice(1, -1), // cut off the string delimiters
			this.lexer.config.languageFeatures.numericSeparators,
		))
	}
}
export class TokenTemplate extends TokenSolid {
	static readonly DELIM              : '\'\'\'' = '\'\'\''
	static readonly DELIM_INTERP_START : '{{' = '{{'
	static readonly DELIM_INTERP_END   : '}}' = '}}'
	/**
	 * Compute the token worth of a `TokenTemplate` token or any segment of such token.
	 * @param   text - the string to compute
	 * @returns        the template value of the argument, a sequence of code units
	 */
	private static tokenWorth(text: string): number[] {
		if (text.length === 0) return []
		return [
			text.charCodeAt(0),
			...TokenTemplate.tokenWorth(text.slice(1)),
		]
	}
	private readonly delim_end  : typeof TokenTemplate.DELIM | typeof TokenTemplate.DELIM_INTERP_START;
	readonly position: TemplatePosition;
	constructor (
		lexer: Lexer,
		private delim_start: typeof TokenTemplate.DELIM | typeof TokenTemplate.DELIM_INTERP_END,
	) {
		super('TEMPLATE', lexer, ...lexer.advance())
		let delim_end: typeof TokenTemplate.DELIM | typeof TokenTemplate.DELIM_INTERP_START;
		const positions: Set<TemplatePosition> = new Set<TemplatePosition>()
		if (delim_start === TokenTemplate.DELIM) {
			positions.add(TemplatePosition.FULL).add(TemplatePosition.HEAD)
			this.advance(2n)
		} else { // delim_start === TokenTemplate.DELIM_INTERP_END
			positions.add(TemplatePosition.MIDDLE).add(TemplatePosition.TAIL)
			this.advance()
		}
		while (!this.lexer.isDone) {
			if (Char.eq(Filebound.EOT, this.lexer.c0)) {
				throw new LexError02(this)
			}
			if (Char.eq(TokenTemplate.DELIM, this.lexer.c0, this.lexer.c1, this.lexer.c2)) {
				/* end string template full/tail */
				delim_end = TokenTemplate.DELIM
				positions.delete(TemplatePosition.HEAD)
				positions.delete(TemplatePosition.MIDDLE)
				// add ending delim to token
				this.advance(3n)
				break;

			} else if (Char.eq(TokenTemplate.DELIM_INTERP_START, this.lexer.c0, this.lexer.c1)) {
				/* end string template head/middle */
				delim_end = TokenTemplate.DELIM_INTERP_START
				positions.delete(TemplatePosition.FULL)
				positions.delete(TemplatePosition.TAIL)
				// add start interpolation delim to token
				this.advance(2n)
				break;

			} else {
				this.advance()
			}
		}
		this.delim_end   = delim_end !
		this.position = [...positions][0]
	}
	cook(): string {
		return String.fromCharCode(...TokenTemplate.tokenWorth(
			this.source.slice(this.delim_start.length, -this.delim_end.length) // cut off the template delimiters
		))
	}
}
export class TokenCommentLine extends TokenComment {
	static readonly DELIM_START: '%'  = '%'
	static readonly DELIM_END:   '\n' = '\n'
	constructor (lexer: Lexer) {
		super(lexer, TokenCommentLine.DELIM_START, TokenCommentLine.DELIM_END)
	}
	protected stopAdvancing() {
		return Char.eq(TokenCommentLine.DELIM_END, this.lexer.c0)
	}
}
export class TokenCommentMulti extends TokenComment {
	static readonly DELIM_START: '%%' = '%%'
	static readonly DELIM_END:   '%%' = '%%'
	constructor (lexer: Lexer) {
		super(lexer, TokenCommentMulti.DELIM_START, TokenCommentMulti.DELIM_END)
	}
	protected stopAdvancing() {
		return Char.eq(TokenCommentMulti.DELIM_END, this.lexer.c0, this.lexer.c1)
	}
}
