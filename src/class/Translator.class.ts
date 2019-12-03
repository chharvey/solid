import Serializable from '../iface/Serializable.iface'
import {STX, ETX} from './Scanner.class'
import Lexer, {
	Token,
	TokenStringLiteral,
	TokenStringTemplate,
	TokenWord,
} from './Lexer.class'


/**
 * A ParseLeaf is a leaf in the parse tree. It consists of only a single token
 * (a terminal in the syntactic grammar), and a cooked value.
 */
export class ParseLeaf implements Serializable {
	/**
	 * Construct a new ParseNode object.
	 * @param   token - the raw token to prepare
	 * @param   value - the cooked value of the raw text
	 */
	constructor(
		private readonly token: Token,
		private readonly value: string|number|boolean,
	) {
	}
	/**
	 * @implements Serializable
	 */
	serialize(): string {
		const tagname: string = this.token.tagname
		const attributes: string = ' ' + [
			`line="${this.token.line_index+1}"`,
			`col="${this.token.col_index+1}"`,
			`value="${(typeof this.value === 'string') ? this.value
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
			: this.value.toString()}"`,
		].join(' ').trim()
		const formatted: string = this.token.source
			.replace(STX, '\u2402') /* SYMBOL FOR START OF TEXT */
			.replace(ETX, '\u2403') /* SYMBOL FOR START OF TEXT */
		return `<${tagname}${attributes}>${formatted}</${tagname}>`
	}
}


/**
 * A translator prepares the tokens for the parser.
 * It performs certian operations such as
 * - removing whitespace and comment tokens
 * - stripping out compiler directives (“pragmas”) and sending them
 * 	separately to the compiler
 * - computing the mathematical values of numerical constants
 * - computing the string values, including escaping, of string constants (“cooking”)
 * - optimizing identifiers
 */
export default class Translator {
	/**
	 * The UTF16Encoding of a numeric code point value.
	 * @see http://ecma-international.org/ecma-262/10.0/#sec-utf16encoding
	 * @param   codepoint a positive integer within [0x0, 0x10ffff]
	 * @returns a code unit sequence representing the code point
	 */
	private static utf16Encoding(codepoint: number): [number] | [number, number] {
		if (codepoint < 0 || 0x10ffff < codepoint) throw new RangeError(`Code point ${codepoint} must be within [0x0, 0x10ffff].`)
		if (codepoint <= 0xffff) return [codepoint]
		const cu1: number = Math.floor(codepoint - 0x10000) / 0x400
		const cu2: number =           (codepoint - 0x10000) % 0x400
		return [cu1 + 0xd800, cu2 + 0xdc00]
	}
	/**
	 * Compute the string literal value of a `TokenStringLiteral` token.
	 * The string literal value is a sequence of Unicode code points.
	 * ```
	 * SVL(StringLiteral ::= "'" "'")
	 * 	is the empty array
	 * SVL(StringLiteral ::= "'" StringLiteralChars "'")
	 * 	is SVL(StringLiteralChars)
	 * SVL(StringLiteralChars ::= [^'\#x03])
	 * 	is {@link Translator.utf16Encoding|UTF16Encoding}(code point of that character)
	 * SVL(StringLiteralChars ::= [^'\#x03] StringLiteralChars)
	 * 	is {@link Translator.utf16Encoding|UTF16Encoding}(code point of that character) followed by SVL(StringLiteralChars)
	 * SVL(StringLiteralChars ::= "\" StringLiteralEscape)
	 * 	is SVL(StringLiteralEscape)
	 * SVL(StringLiteralChars ::= "\" StringLiteralEscape StringLiteralChars)
	 * 	is SVL(StringLiteralEscape) followed by SVL(StringLiteralChars)
	 * SVL(StringLiteralEscape ::= EscapeChar)
	 * 	is SVL(EscapeChar)
	 * SVL(StringLiteralEscape ::= EscapeCode)
	 * 	is SVL(EscapeCode)
	 * SVL(StringLiteralEscape ::= LineContinuation)
	 * 	is SVL(LineContinuation)
	 * SVL(StringLiteralEscape ::= NonEscapeChar)
	 * 	is SVL(NonEscapeChar)
	 * SVL(EscapeChar ::= "'" | "\" | "s" | "t" | "n" | "r")
	 * 	is given by the following map: {
	 * 		"'" : 0x27, // APOSTROPHE           U+0027
	 * 		"\" : 0x5c, // REVERSE SOLIDUS      U+005C
	 * 		"s" : 0x20, // SPACE                U+0020
	 * 		"t" : 0x09, // CHARACTER TABULATION U+0009
	 * 		"n" : 0x0a, // LINE FEED (LF)       U+000A
	 * 		"r" : 0x0d, // CARRIAGE RETURN (CR) U+000D
	 * 	}
	 * SVL(EscapeCode ::= "u{" [0-9a-f]* "}")
	 * 	is UTF16Encoding({@link Translator.mv|MV}([0-9a-f]*))
	 * SVL(LineContinuation ::= #x0A)
	 * 	is 0x20
	 * SVL(LineContinuation ::= #x0D #x0A)
	 * 	is 0x20
	 * SVL(NonEscapeChar ::= [^'\stnru#x0D#x0A#x03]
	 * 	is {@link Translator.utf16Encoding|UTF16Encoding}(code point of that character)
	 * SVL(NonEscapeChar ::= "u" /*? lookahead: [^{] ?/)
	 * 	is 0x75
	 * ```
	 * @param   text the string to compute
	 * @returns the string literal value, a sequence of code points
	 */
	static svl(text: string): number[] {
		if (text.length === 0) return []
		if ('\\' === text[0]) { // possible escape or line continuation
			if (TokenStringLiteral.CHARS_LITERAL_DELIM === text[1] || /[\\stnr]/.test(text[1])) { // an escaped character literal
				return [
					new Map<string, number>([
						[TokenStringLiteral.CHARS_LITERAL_DELIM, TokenStringLiteral.CHARS_LITERAL_DELIM.codePointAt(0) !],
						['\\' , 0x5c],
						['s'  , 0x20],
						['t'  , 0x09],
						['n'  , 0x0a],
						['r'  , 0x0d],
					]).get(text[1]) !,
					...Translator.svl(text.slice(2)),
				]
			} else if ('u{' === text[1] + text[2]) { // an escape sequence
				const sequence: RegExpMatchArray = text.match(/\\u{[0-9a-f]*}/) !
				return [
					...Translator.utf16Encoding(Translator.mv(sequence[0].slice(3, -1) || '0', 16)),
					...Translator.svl(text.slice(sequence[0].length)),
				]
			} else if ('\n' === text[1]) { // a line continuation (LF)
				return [0x20, ...Translator.svl(text.slice(2))]
			} else if ('\r\n' === text[1] + text [2]) { // a line continuation (CRLF)
				return [0x20, ...Translator.svl(text.slice(2))]
			} else { // a backslash escapes the following character
				return [
					...Translator.utf16Encoding(text.codePointAt(1) !),
					...Translator.svl(text.slice(2)),
				]
			}
		} else return [
			...Translator.utf16Encoding(text.codePointAt(0) !),
			...Translator.svl(text.slice(1)),
		]
	}
	/**
	 * Compute the string template value of a `TokenStringTemplate` token.
	 * The string template value is a sequence of Unicode code points.
	 * ```
	 * SVT(StringTemplateFull ::= "`" "`")
	 * 	is the empty array
	 * SVT(StringTemplateFull ::= "`" TemplateCharacters "`")
	 * 	is SVT(StringTemplateCharacters)
	 * SVT(StringTemplateHead ::= "`" "{{")
	 * 	is the empty array
	 * SVT(StringTemplateHead ::= "`" TemplateCharacters "{{")
	 * 	is SVT(StringTemplateCharacters)
	 * SVT(StringTempalteMiddle ::= "}}" "{{")
	 * 	is the empty array
	 * SVT(StringTempalteMiddle ::= "}}" TemplateCharacters "{{")
	 * 	is SVT(StringTemplateCharacters)
	 * SVT(StringTempalteTail ::= "}}" "`")
	 * 	is the empty array
	 * SVT(StringTempalteTail ::= "}}" TemplateCharacters "`")
	 * 	is SVT(StringTemplateCharacters)
	 * SVT(StringTemplateCharacters ::= [^`{\#x03])
	 * 	is {@link Translator.utf16Encoding|UTF16Encoding}(code point of that character)
	 * SVT(StringTemplateCharacters ::= [^`{\#x03] StringTemplateCharacters)
	 * 	is {@link Translator.utf16Encoding|UTF16Encoding}(code point of that character) followed by SVT(StringTemplateCharacters)
	 * SVT(StringTemplateCharacters ::= "{"
	 * 	is 0x7b
	 * SVT(StringTemplateCharacters ::= "{" [^`{\#x03])
	 * 	is 0x7b followed by {@link Translator.utf16Encoding|UTF16Encoding}(code point of that character)
	 * SVT(StringTemplateCharacters ::= "{" [^`{\#x03] StringTemplateCharacters)
	 * 	is 0x7b followed by {@link Translator.utf16Encoding|UTF16Encoding}(code point of that character) followed by SVT(StringTemplateCharacters)
	 * SVT(StringTemplateCharacters ::= "\`")
	 * 	is 0x60
	 * SVT(StringTemplateCharacters ::= "\`" StringTemplateCharacters)
	 * 	is 0x60 followed by SVT(StringTemplateCharacters)
	 * SVT(StringTemplateCharacters ::= "\\")
	 * 	is 0x5c
	 * SVT(StringTemplateCharacters ::= "\\" StringTemplateCharacters)
	 * 	is 0x5c followed by SVT(StringTemplateCharacters)
	 * SVT(StringTemplateCharacters ::= "\" [^`{\#x03] StringTemplateCharacters)
	 * 	is 0x5c followed by {@link Translator.utf16Encoding|UTF16Encoding}(code point of that character) followed by SVT(StringTemplateCharacters)
	 * ```
	 * @param   text the string to compute
	 * @returns the string template value of the string, a sequence of code points
	 */
	static svt(text: string): number[] {
		if (text.length === 0) return []
		if ('\\' === text[0]) { // possible escape
			if (TokenStringTemplate.CHARS_TEMPLATE_DELIM === text[1] || '\\' === text[1]) { // an escaped character literal
				return [
					new Map<string, number>([
						[TokenStringTemplate.CHARS_TEMPLATE_DELIM, TokenStringTemplate.CHARS_TEMPLATE_DELIM.codePointAt(0) !],
						['\\' , 0x5c],
					]).get(text[1]) !,
					...Translator.svt(text.slice(2)),
				]
			} else { // a backslash escapes nothing
				return [
					...Translator.utf16Encoding(text.codePointAt(0) !),
					...Translator.svt(text.slice(1)),
				]
			}
		} else return [
			...Translator.utf16Encoding(text.codePointAt(0) !),
			...Translator.svt(text.slice(1)),
		]
	}
	/**
	 * Compute the mathematical value of a `TokenNumber` token.
	 * ```
	 * MV(DigitSequenceHex ::= [0-9a-f])
	 * 	is MV([0-9a-f])
	 * MV(DigitSequenceHex ::= DigitSequenceHex [0-9a-f])
	 * 	is 16 * MV(DigitSequenceHex) + MV([0-9a-f])
	 * MV([0-9a-f] ::= 0) is 0
	 * MV([0-9a-f] ::= 1) is 1
	 * MV([0-9a-f] ::= 2) is 2
	 * MV([0-9a-f] ::= 3) is 3
	 * MV([0-9a-f] ::= 4) is 4
	 * MV([0-9a-f] ::= 5) is 5
	 * MV([0-9a-f] ::= 6) is 6
	 * MV([0-9a-f] ::= 7) is 7
	 * MV([0-9a-f] ::= 8) is 8
	 * MV([0-9a-f] ::= 9) is 9
	 * MV([0-9a-f] ::= a) is 10
	 * MV([0-9a-f] ::= b) is 11
	 * MV([0-9a-f] ::= c) is 12
	 * MV([0-9a-f] ::= d) is 13
	 * MV([0-9a-f] ::= e) is 14
	 * MV([0-9a-f] ::= f) is 15
	 *
	 * MV(DigitSequenceDec ::= [0-9])
	 * 	is MV([0-9])
	 * MV(DigitSequenceDec ::= DigitSequenceDec [0-9])
	 * 	is 10 * MV(DigitSequenceDec) + MV([0-9])
	 * MV([0-9] ::= 0) is 0
	 * MV([0-9] ::= 1) is 1
	 * MV([0-9] ::= 2) is 2
	 * MV([0-9] ::= 3) is 3
	 * MV([0-9] ::= 4) is 4
	 * MV([0-9] ::= 5) is 5
	 * MV([0-9] ::= 6) is 6
	 * MV([0-9] ::= 7) is 7
	 * MV([0-9] ::= 8) is 8
	 * MV([0-9] ::= 9) is 9
	 * ```
	 * @param   text the string to compute
	 * @param   radix the base in which to compute
	 * @returns the mathematical value of the string in base 16
	 */
	static mv(text: string, radix = 10): number { // TODO let `base` be an instance field of `TokenNumber`
		if (text.length === 0) throw new Error('Cannot compute mathematical value of empty string.')
		return (text.length === 1) ? parseInt(text, radix)
			: radix * Translator.mv(text.slice(0, -1), radix) + Translator.mv(text[text.length-1], radix)
	}


	/** The lexer returning tokens for each iteration. */
	private readonly lexer: Iterator<Token>;
	/** The result of the lexer iterator. */
	private iterator_result_token: IteratorResult<Token>;
	/** The current token. */
	private t0: Token;
	/** The running identifier count. Used as an id for identifier tokens. */
	private idcount: number /* bigint */ = 0;

	/**
	 * Construct a new Translator object.
	 * @param   source_text - the entire source text
	 */
	constructor(source_text: string) {
		this.lexer = new Lexer(source_text).generate()
		this.iterator_result_token = this.lexer.next()
		this.t0 = this.iterator_result_token.value
	}

	/**
	 * Prepare the next token for the parser.
	 * Whitespace and comment tokens are filtered out.
	 * @returns the next token, with modified contents
	 */
	* generate(): Iterator<ParseLeaf> {
		while (!this.iterator_result_token.done) {
			const cooked: ParseLeaf|null = (this.t0 instanceof TokenWord) ? this.t0.cook(this.idcount++) : this.t0.cook()
			if (cooked) yield cooked
			this.iterator_result_token = this.lexer.next()
			this.t0 = this.iterator_result_token.value
		}
	}
}
