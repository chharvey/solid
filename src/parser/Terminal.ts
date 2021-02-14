import {
	Filebound,
	Token,
	Terminal,
} from '@chharvey/parser';

import {
	Util,
} from '../core/';
import {
	RadixType,
	TemplatePosition,
} from './Token';
import * as TOKEN from './Token';



function maybe(fun: () => string): string {
	return Util.randomBool() ? '' : fun()
}
function maybeA(fun: () => string[]): string[] {
	return Util.randomBool() ? [] : fun()
}



export class TerminalKeyword extends Terminal {
	static readonly instance: TerminalKeyword = new TerminalKeyword();
	random(): TOKEN.Keyword {
		return Util.arrayRandom(Object.values(TOKEN.Keyword));
	}
	match(candidate: Token): boolean {
		return candidate instanceof TOKEN.TokenKeyword;
	}
}
export class TerminalIdentifier extends Terminal {
	static readonly instance: TerminalIdentifier = new TerminalIdentifier()
	private static charsBasic(start: boolean = false): string {
		let c: string;
		const pass: RegExp = start ? TOKEN.TokenIdentifierBasic.CHAR_START : TOKEN.TokenIdentifierBasic.CHAR_REST
		do {
			c = Util.randomChar()
		} while (!pass.test(c))
		return start ? c : [c, maybe(TerminalIdentifier.charsBasic)].join('')
	}
	private static charsUnicode(): string {
		return [maybe(TerminalIdentifier.charsUnicode), Util.randomChar(['`'])].join('')
	}
	random(): string {
		let returned: string;
		if (Util.randomBool()) {
			do {
				returned = [TerminalIdentifier.charsBasic(true), maybe(TerminalIdentifier.charsBasic)].join('')
			} while ((TOKEN.TokenKeyword.KEYWORDS as string[]).includes(returned))
		} else {
			returned = `\`${ maybe(TerminalIdentifier.charsUnicode) }\``
		}
		return returned
	}
	match(candidate: Token): boolean {
		return candidate instanceof TOKEN.TokenIdentifier
	}
}
export class TerminalInteger extends Terminal {
	static readonly instance: TerminalInteger = new TerminalInteger()
	static digitSequence(radix: RadixType = TOKEN.TokenNumber.RADIX_DEFAULT): string {
		return [
			...maybeA(() => [
				TerminalInteger.digitSequence(radix),
				maybe(() => TOKEN.TokenNumber.SEPARATOR),
			]),
			Util.arrayRandom(TOKEN.TokenNumber.DIGITS.get(radix)!),
		].join('')
	}
	random(): string {
		const [base, radix]: [string, RadixType] = Util.arrayRandom([...TOKEN.TokenNumber.BASES])
		return [
			maybe(() => Util.arrayRandom(TOKEN.TokenNumber.UNARY)),
			...(Util.randomBool() ? [
				TerminalInteger.digitSequence(),
			] : [
				TOKEN.TokenNumber.ESCAPER,
				base,
				TerminalInteger.digitSequence(radix),
			]),
		].join('')
	}
	match(candidate: Token): boolean {
		return candidate instanceof TOKEN.TokenNumber && !candidate.isFloat
	}
}
export class TerminalFloat extends Terminal {
	static readonly instance: TerminalFloat = new TerminalFloat()
	random(): string {
		return [
			maybe(() => Util.arrayRandom(TOKEN.TokenNumber.UNARY)),
			TerminalInteger.digitSequence(),
			TOKEN.TokenNumber.POINT,
			...maybeA(() => [
				TerminalInteger.digitSequence(),
				...maybeA(() => [
					TOKEN.TokenNumber.EXPONENT,
					maybe(() => Util.arrayRandom(TOKEN.TokenNumber.UNARY)),
					TerminalInteger.digitSequence(),
				]),
			]),
		].join('')
	}
	match(candidate: Token): boolean {
		return candidate instanceof TOKEN.TokenNumber && candidate.isFloat
	}
}
export class TerminalString extends Terminal {
	static readonly instance: TerminalString = new TerminalString()
	private static readonly escape_opts: readonly (() => string)[] = [
		(): string => Util.arrayRandom(TOKEN.TokenString.ESCAPES),
		(): string => `u{${ maybe(() => TerminalInteger.digitSequence(16n)) }}`,
		(): string => `\u000a`,
		(): string => Util.randomChar([TOKEN.TokenString.DELIM, TOKEN.TokenString.ESCAPER, ...'s t n r u \u000a'.split(' '), Filebound.EOT]),
	]
	private static maybeChars(): string {
		const random: number = Math.random()
		return maybe(() => (
			random < 1/3 ? [Util.randomChar([TOKEN.TokenString.DELIM, TOKEN.TokenString.ESCAPER, Filebound.EOT]),                                     TerminalString.maybeChars()]   :
			random < 2/3 ? [TOKEN.TokenString.ESCAPER,                                Util.arrayRandom(TerminalString.escape_opts)(),           TerminalString.maybeChars()]   :
			               [TOKEN.TokenString.ESCAPER, 'u', ...maybeA(() => [Util.randomChar([TOKEN.TokenString.DELIM, '{', Filebound.EOT]), TerminalString.maybeChars()])]
		).join(''))
	}
	random(): string {
		return [TOKEN.TokenString.DELIM, TerminalString.maybeChars(), TOKEN.TokenString.DELIM].join('')
	}
	match(candidate: Token): boolean {
		return candidate instanceof TOKEN.TokenString
	}
}
abstract class TerminalTemplate extends Terminal {
	private static forbidden(): string { return Util.randomChar('\' { \u0003'.split(' ')) }
	private static charsEndDelim(): string {
		const random: number = Math.random()
		return (
			random < 1/3 ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndDelim)].join('') :
			random < 2/3 ? TerminalTemplate.charsEndDelimStartDelim() :
			               TerminalTemplate.charsEndDelimStartInterp()
		)
	}
	private static charsEndDelimStartDelim(): string {
		const random: number = Math.random()
		return (
			random < 1/4 ? [`'`,                                                 TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndDelim)                                                 ] :
			random < 2/4 ? [`''`,                                                TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndDelim)                                                 ] :
			random < 3/4 ? [`'{`,  ...maybeA(() => Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndDelim)] : [TerminalTemplate.charsEndDelimStartDelim()])] :
			               [`''{`, ...maybeA(() => Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndDelim)] : [TerminalTemplate.charsEndDelimStartDelim()])]
		).join('')
	}
	private static charsEndDelimStartInterp(): string {
		const random: number = Math.random()
		return (
			random < 1/3 ? [`{`,   ...(Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndDelim)] : [''                                         ])] :
			random < 2/3 ? [`{'`,  ...(Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndDelim)] : [TerminalTemplate.charsEndDelimStartInterp()])] :
			               [`{''`, ...(Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndDelim)] : [TerminalTemplate.charsEndDelimStartInterp()])]
		).join('')
	}
	private static charsEndInterp(): string {
		const random: number = Math.random()
		return (
			random < 1/3 ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndInterp)].join('') :
			random < 2/3 ? TerminalTemplate.charsEndInterpStartDelim() :
			               TerminalTemplate.charsEndInterpStartInterp()
		)
	}
	private static charsEndInterpStartDelim(): string {
		const random: number = Math.random()
		return (
			random < 1/4 ? [`'`,   ...(Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndInterp)] : [''                                         ])] :
			random < 2/4 ? [`''`,  ...(Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndInterp)] : [''                                         ])] :
			random < 3/4 ? [`'{`,  ...(Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndInterp)] : [TerminalTemplate.charsEndInterpStartDelim()])] :
			               [`''{`, ...(Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndInterp)] : [TerminalTemplate.charsEndInterpStartDelim()])]
		).join('')
	}
	private static charsEndInterpStartInterp(): string {
		const random: number = Math.random()
		return (
			random < 1/3 ? [`{`,                                                 TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndInterp)                                                   ] :
			random < 2/3 ? [`{'`,  ...maybeA(() => Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndInterp)] : [TerminalTemplate.charsEndInterpStartInterp()])] :
			               [`{''`, ...maybeA(() => Util.randomBool() ? [TerminalTemplate.forbidden(), maybe(TerminalTemplate.charsEndInterp)] : [TerminalTemplate.charsEndInterpStartInterp()])]
		).join('')
	}
	random(start: string = TOKEN.TokenTemplate.DELIM, end: string = TOKEN.TokenTemplate.DELIM): string {
		return [start, maybe(end === TOKEN.TokenTemplate.DELIM ? TerminalTemplate.charsEndDelim : TerminalTemplate.charsEndInterp), end].join('')
	}
	match(candidate: Token, position: TemplatePosition = TemplatePosition.FULL): boolean {
		return candidate instanceof TOKEN.TokenTemplate && candidate.position === position
	}
}
export class TerminalTemplateFull extends TerminalTemplate {
	static readonly instance: TerminalTemplateFull = new TerminalTemplateFull()
	random(): string {
		return super.random(TOKEN.TokenTemplate.DELIM, TOKEN.TokenTemplate.DELIM)
	}
	match(candidate: Token): boolean {
		return super.match(candidate, TemplatePosition.FULL)
	}
}
export class TerminalTemplateHead extends TerminalTemplate {
	static readonly instance: TerminalTemplateHead = new TerminalTemplateHead()
	random(): string {
		return super.random(TOKEN.TokenTemplate.DELIM, TOKEN.TokenTemplate.DELIM_INTERP_START)
	}
	match(candidate: Token): boolean {
		return super.match(candidate, TemplatePosition.HEAD)
	}
}
export class TerminalTemplateMiddle extends TerminalTemplate {
	static readonly instance: TerminalTemplateMiddle = new TerminalTemplateMiddle()
	random(): string {
		return super.random(TOKEN.TokenTemplate.DELIM_INTERP_END, TOKEN.TokenTemplate.DELIM_INTERP_START)
	}
	match(candidate: Token): boolean {
		return super.match(candidate, TemplatePosition.MIDDLE)
	}
}
export class TerminalTemplateTail extends TerminalTemplate {
	static readonly instance: TerminalTemplateTail = new TerminalTemplateTail()
	random(): string {
		return super.random(TOKEN.TokenTemplate.DELIM_INTERP_END, TOKEN.TokenTemplate.DELIM)
	}
	match(candidate: Token): boolean {
		return super.match(candidate, TemplatePosition.TAIL)
	}
}
