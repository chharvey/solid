import Serializable from '../iface/Serializable.iface'
import {Char, STX, ETX} from './Scanner.class'
import {
	TerminalFilebound,
	TerminalWhitespace,
	TerminalComment,
	TerminalStringLiteral,
	TerminalStringTemplateFull,
	TerminalStringTemplateHead,
	TerminalStringTemplateMiddle,
	TerminalStringTemplateTail,
	TerminalNumber,
	TerminalWord,
	TerminalPunctuator,
} from './Terminal.class'
import Translator from './Translator.class'


/**
 * A Token object is the kind of thing that the Lexer returns.
 * It holds:
 * - the text of the token (self.cargo)
 * - the line number and column index where the token starts
 *
 * @see http://parsingintro.sourceforge.net/#contents_item_6.4
 */
export default abstract class Token implements Serializable {
	/** All the characters in this Token. */
	private _cargo: string;
	/** Zero-based line number of the first character (first line is line 0). */
	readonly line_index: number;
	/** Zero-based column number of the first character (first col is col 0). */
	readonly col_index: number;

	/**
	 * Construct a new Token object.
	 *
	 * @param tagname    - the name of the type of this Token
	 * @param start_char - the starting character of this Token
	 * @param more_chars - additional characters to add upon construction
	 */
	constructor(
		readonly tagname: string,
		start_char: Char,
		...more_chars: Char[]
	) {
		this._cargo     = start_char.source + more_chars.map((char) => char.source).join('')
		this.line_index = start_char.line_index
		this.col_index  = start_char.col_index
	}

	/**
	 * Get the sum of this Token’s cargo.
	 * @returns all the source characters in this Token
	 */
	get source(): string {
		return this._cargo
	}

	/**
	 * Add to this Token’s cargo.
	 * @param chars - the characters to append
	 */
	add(...chars: Char[]): void {
		this._cargo += chars.map((char) => char.source).join('')
	}

	/**
	 * Return this Token’s cooked value.
	 * The cooked value is the computed or evaluated contents of this Token,
	 * to be sent to the parser and compiler.
	 * If this Token is not to be sent to the parser, then return `null`.
	 * @returns the computed value of this token, or `null`
	 */
	abstract get cooked(): string|number|boolean|null;

	/**
	 * @implements Serializable
	 */
	serialize(): string {
		const cooked: string|number|boolean|null = this.cooked; // getter is called only once
		const attributes: string = ' ' + [
			`line="${this.line_index+1}"`,
			`col="${this.col_index+1}"`,
			(cooked !== null) ? `value="${(typeof cooked === 'string') ? cooked
				.replace(/\&/g, '&amp;' )
				.replace(/\</g, '&lt;'  )
				.replace(/\>/g, '&gt;'  )
				.replace(/\'/g, '&apos;')
				.replace(/\"/g, '&quot;')
				.replace(/\\/g, '&#x5c;')
				.replace(/\t/g, '&#x09;')
				.replace(/\n/g, '&#x0a;')
				.replace(/\r/g, '&#x0d;')
				.replace(/\u0000/g, '&#x00;')
			: cooked.toString()}"` : '',
		].join(' ').trim()
		const formatted: string = this.source
			.replace(STX, '\u2402') /* SYMBOL FOR START OF TEXT */
			.replace(ETX, '\u2403') /* SYMBOL FOR START OF TEXT */
		return `<${this.tagname}${attributes}>${formatted}</${this.tagname}>`
	}
}


export class TokenFilebound extends Token {
	static readonly CHARS: readonly string[] = [STX, ETX]
	constructor(start_char: Char, ...more_chars: Char[]) {
		super(TerminalFilebound.instance.TAGNAME, start_char, ...more_chars)
	}
	get cooked(): boolean {
		return this.source === STX /* || !this.source === ETX */
	}
	serialize(): string {
		const formatted: string = new Map<string, string>([
			[STX, '\u2402' /* SYMBOL FOR START OF TEXT */],
			[ETX, '\u2403' /* SYMBOL FOR END OF TEXT   */],
		]).get(this.source) !
		return `<${TerminalFilebound.instance.TAGNAME}>${formatted}</${TerminalFilebound.instance.TAGNAME}>`
	}
}
export class TokenWhitespace extends Token {
	static readonly CHARS: readonly string[] = [' ', '\t', '\n', '\r']
	constructor(start_char: Char, ...more_chars: Char[]) {
		super(TerminalWhitespace.instance.TAGNAME, start_char, ...more_chars)
	}
	get cooked(): null {
		return null // we do not want to send whitespace to the parser
	}
}
export abstract class TokenComment extends Token {
	constructor(kind: string, start_char: Char, ...more_chars: Char[]) {
		super(`${TerminalComment.instance.TAGNAME}-${kind}`, start_char, ...more_chars)
	}
	/** @final */ get cooked(): null {
		return null // we do not want to send comments to the parser
	}
}
export class TokenCommentLine extends TokenComment {
	static readonly DELIM: '\\' = '\\'
	constructor(start_char: Char, ...more_chars: Char[]) {
		super('LINE', start_char, ...more_chars)
	}
}
export class TokenCommentMulti extends TokenComment {
	static readonly DELIM_START : '"' = '"'
	static readonly DELIM_END   : '"' = '"'
	constructor(start_char: Char, ...more_chars: Char[]) {
		super('MULTI', start_char, ...more_chars)
	}
}
export class TokenCommentMultiNest extends TokenComment {
	static readonly DELIM_START : '"{' = '"{'
	static readonly DELIM_END   : '}"' = '}"'
	constructor(start_char: Char, ...more_chars: Char[]) {
		super('MULTI_NEST', start_char, ...more_chars)
	}
}
export class TokenCommentDoc extends TokenComment {
	static readonly DELIM_START : '"""' = '"""'
	static readonly DELIM_END   : '"""' = '"""'
	constructor(start_char: Char, ...more_chars: Char[]) {
		super('DOC', start_char, ...more_chars)
	}
}
export class TokenStringLiteral extends Token {
	static readonly DELIM: '\'' = '\''
	static readonly ESCAPES: readonly string[] = [TokenStringLiteral.DELIM, '\\', 's','t','n','r']
	constructor(start_char: Char, ...more_chars: Char[]) {
		super(TerminalStringLiteral.instance.TAGNAME, start_char, ...more_chars)
	}
	get cooked(): string {
		return String.fromCodePoint(...Translator.svl(
			this.source.slice(1, -1) // cut off the string delimiters
		))
	}
}
export abstract class TokenStringTemplate extends Token {
	static readonly DELIM              : '`'  = '`'
	static readonly DELIM_INTERP_START : '{{' = '{{'
	static readonly DELIM_INTERP_END   : '}}' = '}}'
	cook(start: number = 0, end: number = Infinity): string {
		return String.fromCodePoint(...Translator.svt(
			this.source.slice(start, end) // cut off the string delimiters
		))
	}
}
export class TokenStringTemplateFull extends TokenStringTemplate {
	constructor(start_char: Char, ...more_chars: Char[]) {
		super(TerminalStringTemplateFull.instance.TAGNAME, start_char, ...more_chars)
	}
	get cooked(): string {
		return super.cook(TokenStringTemplate.DELIM.length, -TokenStringTemplate.DELIM.length)
	}
}
export class TokenStringTemplateHead extends TokenStringTemplate {
	constructor(start_char: Char, ...more_chars: Char[]) {
		super(TerminalStringTemplateHead.instance.TAGNAME, start_char, ...more_chars)
	}
	get cooked(): string {
		return super.cook(TokenStringTemplate.DELIM.length, -TokenStringTemplate.DELIM_INTERP_START.length)
	}
}
export class TokenStringTemplateMiddle extends TokenStringTemplate {
	constructor(start_char: Char, ...more_chars: Char[]) {
		super(TerminalStringTemplateMiddle.instance.TAGNAME, start_char, ...more_chars)
	}
	get cooked(): string {
		return super.cook(TokenStringTemplate.DELIM_INTERP_END.length, -TokenStringTemplate.DELIM_INTERP_START.length)
	}
}
export class TokenStringTemplateTail extends TokenStringTemplate {
	constructor(start_char: Char, ...more_chars: Char[]) {
		super(TerminalStringTemplateTail.instance.TAGNAME, start_char, ...more_chars)
	}
	get cooked(): string {
		return super.cook(TokenStringTemplate.DELIM_INTERP_END.length, -TokenStringTemplate.DELIM.length)
	}
}
export class TokenNumber extends Token {
	static readonly RADIX_DEFAULT: number = 10
	static readonly SEPARATOR: string = '_'
	static readonly BASES: ReadonlyMap<string, number> = new Map<string, number>([
		['b',  2],
		['q',  4],
		['o',  8],
		['d', 10],
		['x', 16],
		['z', 36],
	])
	static readonly DIGITS: ReadonlyMap<number, readonly string[]> = new Map<number, readonly string[]>([
		[ 2, '0 1'                                                                     .split(' ')],
		[ 4, '0 1 2 3'                                                                 .split(' ')],
		[ 8, '0 1 2 3 4 5 6 7'                                                         .split(' ')],
		[10, '0 1 2 3 4 5 6 7 8 9'                                                     .split(' ')],
		[16, '0 1 2 3 4 5 6 7 8 9 a b c d e f'                                         .split(' ')],
		[36, '0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z' .split(' ')],
	])
	constructor(
		private readonly radix: number,
		start_char: Char,
		...more_chars: Char[]
	) {
		super(TerminalNumber.instance.TAGNAME, start_char, ...more_chars)
	}
	get cooked(): number {
		return Translator.mv(this.source[0] === '\\' ? this.source.slice(2) : this.source, this.radix)
	}
}
export class TokenWord extends Token {
	static readonly CHARS_START: readonly string[] = ''.split(' ')
	static readonly CHARS_REST : readonly string[] = ''.split(' ')
	constructor(start_char: Char, ...more_chars: Char[]) {
		super(TerminalWord.instance.TAGNAME, start_char, ...more_chars)
	}
	/**
	 * @param   id the running identifier count
	 */
	cook(id?: number /* bigint */): number {
		return id || -1 // TODO
	}
	get cooked(): number {
		return -1 // TODO
	}
}
export class TokenPunctuator extends Token {
	static readonly CHARS_1: readonly string[] = '+ - * / ^ ( )'.split(' ')
	static readonly CHARS_2: readonly string[] = ''.split(' ')
	static readonly CHARS_3: readonly string[] = ''.split(' ')
	constructor(start_char: Char, ...more_chars: Char[]) {
		super(TerminalPunctuator.instance.TAGNAME, start_char, ...more_chars)
	}
	get cooked(): string {
		return this.source
	}
}
