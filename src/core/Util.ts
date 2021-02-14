/**
 * Utility fields and methods.
 */
export class Util {
	/**
	 * Return a random boolean value.
	 * @returns a random boolean value
	 */
	static randomBool(): boolean {
		return Math.random() < 0.5
	}

	/**
	 * Return a random integer from 0 to the argument.
	 * @example
	 * randomInt(16); // returns a random integer within the interval [0, 16)
	 * @param   n - the upper bound, exclusive
	 * @returns a random integer between 0 (inclusive) and n (exclusive)
	 */
	static randomInt(n: number): number {
		return Math.floor(Math.random() * n)
	}

	/**
	 * Return a random code point, an integer between 0x20 (inclusive) and
	 * 0x7f (the highest ASCII code point, inclusive),
	 * as a character in a string.
	 * Any exceptions given will not be returned.
	 * @param   except - a list of exceptions
	 * @returns          a random character whose code point is not in the exceptions list
	 */
	static randomChar(except: string[] = []): string {
		const exceptcodes: number[] = except.map((s) => s.codePointAt(0) !)
		let code: number;
		do {
			code = Util.randomInt(0x80 - 0x20) + 0x20
		} while (exceptcodes.includes(code))
		return String.fromCodePoint(code)
	}

	/**
	 * Remove indentation from string templates, and appends a line feed.
	 * @param   s the string to remove indentation from each line
	 * @returns   the string with indentation removed and a line feed appended
	 */
	static dedent(s: string): string {
		const indents: RegExpMatchArray|null = s.match(/\n\t*/)
		return `${
			(indents) ? s.replace(new RegExp(`\\n\\t{${indents[0].length - 1}}`, 'g'), '\n').trim() : s
		}\n`
	}

	/**
	 * Select a random element from the provided array.
	 * @param   array - the array to select from
	 * @returns         a uniformly-distributed random element from the array
	 */
	static arrayRandom<T>(array: readonly T[]): T {
		return array[Math.floor(Math.random() * array.length)]
	}

	/**
	 * The UTF16Encoding of a numeric code point value.
	 * @see http://ecma-international.org/ecma-262/10.0/#sec-utf16encoding
	 * @param   codepoint - a positive integer within [0x0, 0x10ffff]
	 * @returns             a code unit sequence representing the code point
	 */
	static utf16Encoding(codepoint: number): [number] | [number, number] {
		if (codepoint < 0 || 0x10ffff < codepoint) throw new RangeError(`Code point \`0x${codepoint.toString(16)}\` must be within [0x0, 0x10ffff].`) // TODO this should be a ParseError
		if (codepoint <= 0xffff) return [codepoint]
		const cu1: number = (codepoint - 0x10000) / 0x400
		const cu2: number = (codepoint - 0x10000) % 0x400
		return [Math.floor(cu1) + 0xd800, cu2 + 0xdc00]
	}
}
