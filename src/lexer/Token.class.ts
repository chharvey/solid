import SolidConfig, {CONFIG_DEFAULT} from '../SolidConfig'

import Util from '../class/Util.class'
import Dev from '../class/Dev.class'
import type Serializable from '../iface/Serializable.iface'
import Char from './Char.class'
import type Lexer from './Lexer.class'

import {
	LexError02,
	LexError03,
	LexError04,
	LexError05,
} from '../error/LexError.class'



export enum Filebound {
	SOT = '\u0002',
	EOT = '\u0003',
}

export enum Punctuator {
	// grouping
		GRP_OPN = '(',
		GRP_CLS = ')',
	// unary
		NOT = '!',
		EMP = '?',
		AFF = '+',
		NEG = '-',
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
	// statement
		ENDSTAT = ';',
		ASSIGN  = '=', // Dev.supports('variables')
}

export enum Keyword {
	// literal
		NULL  = 'null',
		FALSE = 'false',
		TRUE  = 'true',
	// operator
		IS   = 'is',
		ISNT = 'isnt',
		IF   = 'if',
		THEN = 'then',
		ELSE = 'else',
	// storage
		LET = 'let', // Dev.supports('variables')
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



/**
 * A Token object is the kind of thing that the Lexer returns.
 * It holds:
 * - the text of the token (self.cargo)
 * - the line number and column index where the token starts
 *
 * @see http://parsingintro.sourceforge.net/#contents_item_6.4
 */
export default abstract class Token implements Serializable {
	/** @implements Serializable */
	readonly source_index: number;
	/** @implements Serializable */
	readonly line_index: number;
	/** @implements Serializable */
	readonly col_index: number;

	/** All the characters in this Token. */
	private _cargo: string;

	/**
	 * Construct a new Token object.
	 * @param tagname    - The name of the type of this Token.
	 * @param lexer      - The lexer used to construct this Token.
	 * @param start_char - the starting character of this Token
	 * @param more_chars - additional characters to add upon construction
	 */
	constructor (
		/** @implements Serializable */
		readonly tagname: string,
		protected readonly lexer: Lexer,
		start_char: Char,
		...more_chars: Char[]
	) {
		this._cargo       = [start_char, ...more_chars].map((char) => char.source).join('')
		this.source_index = start_char.source_index
		this.line_index   = start_char.line_index
		this.col_index    = start_char.col_index
	}

	/**
	 * Get the sum of this Token’s cargo.
	 * @implements Serializable
	 * @returns all the source characters in this Token
	 */
	get source(): string {
		return this._cargo
	}

	/**
	 * Add to this Token’s cargo.
	 * @param lexer - the lexer whose characters to take from
	 * @param n     - the number of characters to append
	 */
	protected advance(n: bigint = 1n): void {
		this._cargo += this.lexer.advance(n).map((char) => char.source).join('')
	}

	/**
	 * Return this Token’s cooked value.
	 * The cooked value is the computed or evaluated contents of this Token,
	 * to be sent to the parser and compiler.
	 * If this Token is not to be sent to the parser, then return `null`.
	 * @returns              the computed value of this token, or `null`
	 */
	abstract cook(): CookValueType;

	/**
	 * @implements Serializable
	 */
	serialize(): string {
		const cooked: CookValueType = this.cook()
		const attributes: Map<string, string> = new Map<string, string>()
		if (!(this instanceof TokenFilebound)) {
			attributes.set('line', `${this.line_index + 1}`)
			attributes.set('col' , `${this.col_index  + 1}`)
		}
		if (cooked !== null) {
			attributes.set('value', cooked.toString())
		}
		return `<${this.tagname} ${Util.stringifyAttributes(attributes)}>${Util.sanitizeContent(this.source)}</${this.tagname}>`
	}
}



export class TokenFilebound extends Token {
	static readonly CHARS: readonly Filebound[] = [Filebound.SOT, Filebound.EOT]
	declare readonly source: Filebound;
	constructor (lexer: Lexer) {
		super('FILEBOUND', lexer, ...lexer.advance())
	}
	cook(): boolean {
		return this.source === Filebound.SOT
	}
}
export class TokenWhitespace extends Token {
	static readonly CHARS: readonly string[] = [' ', '\t', '\n']
	constructor (lexer: Lexer) {
		super('WHITESPACE', lexer, ...lexer.advance())
		while (!this.lexer.isDone && Char.inc(TokenWhitespace.CHARS, this.lexer.c0)) {
			this.advance()
		}
	}
	cook(): null {
		return null // we do not want to send whitespace to the parser
	}
}
export class TokenPunctuator extends Token {
	static readonly PUNCTUATORS: readonly Punctuator[] = [...new Set( // remove duplicates
		Object.values(Punctuator).filter((p) => Dev.supports('variables') ? true : ![
			Punctuator.ASSIGN,
		].includes(p))
	)]
	declare readonly source: Punctuator;
	constructor (lexer: Lexer, count: 1n|2n|3n = 1n) {
		super('PUNCTUATOR', lexer, ...lexer.advance())
		if (count >= 3n) {
			this.advance(2n)
		} else if (count >= 2n) {
			this.advance()
		}
	}
	cook(): bigint {
		return BigInt(TokenPunctuator.PUNCTUATORS.indexOf(this.source))
	}
}
export class TokenKeyword extends Token {
	private static readonly MINIMUM_VALUE: 0x80n = 0x80n
	static readonly CHAR: RegExp = /^[a-z]$/
	static readonly KEYWORDS: readonly Keyword[] = [...new Set<Keyword>( // remove duplicates
		Object.values(Keyword).filter((kw) => Dev.supports('variables') ? true : ![
			Keyword.LET,
			Keyword.UNFIXED,
		].includes(kw))
	)]
	declare readonly source: Keyword;
	constructor (lexer: Lexer, start_char: Char, ...more_chars: Char[]) {
		super('KEYWORD', lexer, start_char, ...more_chars)
	}
	cook(): bigint {
		return BigInt(TokenKeyword.KEYWORDS.indexOf(this.source)) + TokenKeyword.MINIMUM_VALUE
	}
}
export abstract class TokenIdentifier extends Token {
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
export class TokenNumber extends Token {
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
	private readonly has_unary: boolean;
	private readonly has_radix: boolean;
	private readonly radix: RadixType;
	constructor (lexer: Lexer, has_unary: boolean, has_radix: boolean = false) {
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
export class TokenString extends Token {
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
	constructor (lexer: Lexer) {
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
export class TokenTemplate extends Token {
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
export abstract class TokenComment extends Token {
	constructor (lexer: Lexer, start_delim: string, end_delim: string) {
		super('COMMENT', lexer, ...lexer.advance(BigInt(start_delim.length)))
		while (!this.lexer.isDone && !this.stopAdvancing()) {
			if (Char.eq(Filebound.EOT, this.lexer.c0)) {
				throw new LexError02(this)
			}
			this.advance()
		}
		// add end delim to token
		this.advance(BigInt(end_delim.length))
	}
	/** @final */ cook(): null {
		return null // we do not want to send comments to the parser
	}
	/**
	 * Helper method used in the constructor.
	 * @return When should the lexer stop advancing?
	 */
	protected abstract stopAdvancing(): boolean;
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