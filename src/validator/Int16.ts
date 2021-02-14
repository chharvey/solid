import type {SolidLanguageType} from './SolidLanguageType';
import {SolidObject} from './SolidObject';
import {SolidNumber} from './SolidNumber';
import {Float64} from './Float64';



type Int16Datatype = readonly [boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean]
type Int16DatatypeMutable =   [boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean, boolean]



/**
 * A 16-bit signed integer in two’s complement.
 * @final
 */
export class Int16 extends SolidNumber<Int16> {
	private static readonly BITCOUNT: number = 16

	        static readonly ZERO  : Int16 = new Int16(0n)
	        static readonly UNIT  : Int16 = new Int16(1n)
	private static readonly RADIX : Int16 = new Int16(2n)

	/** @override */
	static values: SolidLanguageType['values'] = new Set([Int16.ZERO])
	private static mod(n: bigint, modulus: bigint): bigint {
		return (n % modulus + modulus) % modulus
	}

	private readonly internal: Int16Datatype;

	/**
	 * Construct a new Int16 object from a bigint or from data.
	 * @param data - a numeric value or data
	 * @returns the value represented as a 16-bit signed integer
	 */
	constructor (data: bigint | Int16Datatype = 0n) {
		super()
		this.internal = (typeof data === 'bigint')
			? [...Int16.mod(data, 2n ** BigInt(Int16.BITCOUNT)).toString(2).padStart(Int16.BITCOUNT, '0')].map((bit) => !!+bit) as Int16DatatypeMutable
			: data
	}

	/** @override */
	toString(): string {
		return `${ this.toNumeric() }`
	}
	/** @override */
	@SolidObject.identicalDeco
	identical(value: SolidObject): boolean {
		return value instanceof Int16 && this.internal.every((bit, i) => bit === value.internal[i]);
	}
	/** @override */
	@SolidObject.equalsDeco
	equal(value: SolidObject): boolean {
		return value instanceof Float64 && value.equal(this);
	}
	/** @override */
	toFloat(): Float64 {
		return new Float64(Number(this.toNumeric()))
	}
	/**
	 * Return the signed interpretation of this integer.
	 * @returns the numeric value
	 */
	toNumeric(): bigint {
		const unsigned: number = this.internal.map((bit, i) => +bit * 2 ** (Int16.BITCOUNT - 1 - i)).reduce((a, b) => a + b)
		return BigInt(unsigned < 2 ** (Int16.BITCOUNT - 1) ? unsigned : unsigned - 2 ** Int16.BITCOUNT)
	}

	/** @override */
	plus(addend: Int16): Int16 {
		type Carry = [bigint,bigint,bigint,bigint,bigint,bigint,bigint,bigint,bigint,bigint,bigint,bigint,bigint,bigint,bigint,bigint]
		const sum   : Carry = [...new Array(Int16.BITCOUNT).fill(0n)] as Carry
		const carry : Carry = [...new Array(Int16.BITCOUNT).fill(0n)] as Carry
		for (let i = Int16.BITCOUNT - 1; i >= 0; i--) {
			const digit: bigint = carry[i] + BigInt(this.internal[i]) + BigInt(addend.internal[i])
			if (digit <= 1n) {
				sum[i] = digit
			} else {
				sum[i] = digit - 2n
				if (i > 0) {
					carry[i - 1] = 1n
				} // else do nothing (drop the overflow)
			}
		}
		return new Int16(sum.map((bit) => !!bit) as Int16DatatypeMutable)
	}
	/** @override */
	minus(subtrahend: Int16): Int16 {
		return this.plus(subtrahend.neg())
	}
	/**
	 * @override
	 * ```ts
	 * function mulSlow(multiplier: number, multiplicand: number): number {
	 * 	return (
	 * 		(multiplier === 0) ? 0                 :
	 * 		(multiplier === 1) ? multiplicand      :
	 * 		(multiplier === 2) ? multiplicand << 1 :
	 * 		(multiplicand <   0) ? -mulSlow(multiplier, -multiplicand) :
	 * 		(multiplicand === 0) ? 0                                   :
	 * 		(multiplicand === 1) ? multiplier                          :
	 * 		(multiplicand === 2) ? multiplier << 1                     :
	 * 		multiplier + mulSlow(multiplier, multiplicand - 1)
	 * 	)
	 * }
	 * function mulFast(multiplier: number, multiplicand: number): number {
	 * 	return (
	 * 		(multiplier === 0) ? 0                 :
	 * 		(multiplier === 1) ? multiplicand      :
	 * 		(multiplier === 2) ? multiplicand << 1 :
	 * 		(multiplicand <   0) ? -mulFast(multiplier, -multiplicand) :
	 * 		(multiplicand === 0) ? 0                                   :
	 * 		(multiplicand === 1) ? multiplier                          :
	 * 		(multiplicand === 2) ? multiplier << 1                     :
	 * 		(multiplicand % 2 === 0)
	 * 			?              mulFast(multiplier * 2,  multiplicand      / 2)
	 * 			: multiplier + mulFast(multiplier * 2, (multiplicand - 1) / 2)
	 * 	)
	 * }
	 * ```
	 */
	times(multiplicand: Int16): Int16 {
		return (
			(this.eq0()) ? Int16.ZERO         :
			(this.eq1()) ? multiplicand       :
			(this.eq2()) ? multiplicand.bsl() :
			(multiplicand.lt0()) ? this.times(multiplicand.neg()).neg() :
			(multiplicand.eq0()) ? Int16.ZERO                           :
			(multiplicand.eq1()) ? this                                 :
			(multiplicand.eq2()) ? this.bsl()                           :
			(multiplicand.isEven())
				?           this.times(Int16.RADIX).times(multiplicand                  .divide(Int16.RADIX))
				: this.plus(this.times(Int16.RADIX).times(multiplicand.minus(Int16.UNIT).divide(Int16.RADIX)))
		)
	}
	/**
	 * @override
	 * ```ts
	 * function divSlow(dividend: number, divisor: number): number {
	 * 	return (
	 * 		(divisor  === 0) ? throw new NanError('Division by zero.') :
	 * 		(dividend === 0) ? 0                                       :
	 * 		(divisor  <   0) ? -divSlow(dividend, -divisor)            :
	 * 		(dividend <   0) ? -divSlow(-dividend, divisor)            :
	 * 		(divisor === 1) ? dividend      :
	 * 		(divisor === 2) ? dividend >> 1 :
	 * 		(() => {
	 * 			let quotient: int = 0
	 * 			while (dividend  >= divisor) {
	 * 				dividend -= divisor
	 * 				++quotient
	 * 			}
	 * 			return quotient
	 * 		})()
	 * 	)
	 * }
	 * function divFast(dividend: number, divisor: number): number {
	 * 	return (
	 * 		(divisor  === 0) ? throw new NanError('Division by zero.') :
	 * 		(dividend === 0) ? 0                                       :
	 * 		(divisor  <   0) ? -divFast( dividend, -divisor)           :
	 * 		(dividend <   0) ? -divFast(-dividend,  divisor)           :
	 * 		(divisor === 1) ? dividend      :
	 * 		(divisor === 2) ? dividend >> 1 :
	 * 		(() => {
	 * 			let quotient = 0
	 * 			let remainder = 0
	 * 			for (let i = 0; i < BITCOUNT; i++) {
	 * 				remainder = remainder << 1
	 * 				remainder[BITCOUNT - 1] = dividend[i]
	 * 				if (remainder >= divisor) {
	 * 					remainder = remainder - divisor
	 * 					quotient[i] = true
	 * 				}
	 * 			}
	 * 			return quotient
	 * 		})()
	 * 	)
	 * }
	 * ```
	 */
	divide(divisor: Int16): Int16 {
		return (
			(divisor.eq0()) ? (() => { throw new RangeError('Division by zero.') })() :
			(this   .eq0()) ? Int16.ZERO                       :
			(divisor.lt0()) ? this.divide(divisor.neg()).neg() :
			(this   .lt0()) ? this.neg().divide(divisor).neg() :
			(divisor.eq1()) ? this       :
			(divisor.eq2()) ? this.bsr() :
			(() => {
				const quotient  : Int16DatatypeMutable = [...new Array(Int16.BITCOUNT).fill(false)] as Int16DatatypeMutable
				let   remainder : Int16DatatypeMutable = [...new Array(Int16.BITCOUNT).fill(false)] as Int16DatatypeMutable
				for (let i: number = 0; i < Int16.BITCOUNT; i++) {
					remainder = [
						...remainder.slice(1),
						this.internal[i],
					] as Int16DatatypeMutable
					const diff: Int16 = new Int16(remainder).minus(divisor)
					if (!diff.lt0()) {
						remainder = diff.internal as Int16DatatypeMutable
						quotient[i] = true
					}
				}
				return new Int16(quotient)
			})()
		)
	}
	/**
	 * @override
	 * ```ts
	 * function expSlow(base: number, exponent: number): number {
	 * 	return (
	 * 		(exponent <   0) ? 0 :
	 * 		(exponent === 0) ? 1 :
	 * 		(exponent === 1) ? base :
	 * 		(exponent === 2) ? base * base :
	 * 		base * expSlow(base, exponent - 1)
	 * 	)
	 * }
	 * function expFast(base: number, exponent: number): number {
	 * 	return (
	 * 		(exponent <   0) ? 0 :
	 * 		(exponent === 0) ? 1 :
	 * 		(exponent === 1) ? base :
	 * 		(exponent === 2) ? base * base :
	 * 		(base === 0) ? 0 :
	 * 		(base === 1) ? 1 :
	 * 		(exponent % 2 === 0)
	 * 			?        expFast(base ** 2,  exponent      / 2)
	 * 			: base * expFast(base ** 2, (exponent - 1) / 2)
	 * 	)
	 * }
	 * ```
	 * @see https://stackoverflow.com/a/101613/877703
	 */
	exp(exponent: Int16): Int16 {
		return (
			(exponent.lt0()) ? Int16.ZERO       :
			(exponent.eq0()) ? Int16.UNIT       :
			(exponent.eq1()) ? this             :
			(exponent.eq2()) ? this.times(this) :
			(this.eq0()) ? Int16.ZERO :
			(this.eq1()) ? Int16.UNIT :
			(exponent.isEven())
				?            this.exp(Int16.RADIX).exp(exponent                  .divide(Int16.RADIX))
				: this.times(this.exp(Int16.RADIX).exp(exponent.minus(Int16.UNIT).divide(Int16.RADIX)))
		)
		// let returned: Int16 = Int16.UNIT
		// while (true) {
		// 	if (!exponent.isEven()) {
		// 		returned = returned.times(base) // returned *= base
		// 	}
		// 	exponent = exponent.divide(Int16.RADIX) // exponent /= 2n
		// 	if (exponent.eq0()) break
		// 	base = base.times(base) // base *= base
		// }
		// return returned
	}
	/**
	 * @override
	 * Equivalently, this is the “two’s complement” of the integer.
	 */
	neg(): Int16 {
		return this.cpl().plus(Int16.UNIT)
	}
	/** @override */
	eq0(): boolean {
		return this.equal(Int16.ZERO);
	}
	/**
	 * Is the 16-bit signed integer equal to `1`?
	 */
	private eq1(): boolean {
		return this.equal(Int16.UNIT);
	}
	/**
	 * Is the 16-bit signed integer equal to `2`?
	 */
	private eq2(): boolean {
		return this.equal(Int16.RADIX);
	}
	/** @override */
	lt(y: Int16): boolean {
		return this.minus(y).lt0()
	}
	/**
	 * Return the ones’ complement of a 16-bit signed integer.
	 * @see https://en.wikipedia.org/wiki/Ones%27_complement
	 * @param int - the integer
	 * @return the ones’ complement, equivalent to `-a - 1`
	 */
	private cpl(): Int16 {
		return new Int16(this.internal.map((bit) => !bit) as Int16DatatypeMutable)
	}
	/**
	 * Perform an arithmetic left bit shift of a 16-bit signed integer.
	 * @param int - the integer
	 * @return a left bit shift of 1 bit, dropping the first bit and appending `false` to the end
	 */
	private bsl(): Int16 {
		return new Int16([
			...this.internal.slice(1),
			false,
		] as Int16DatatypeMutable)
	}
	/**
	 * Perform an arithmetic right bit shift of a 16-bit signed integer.
	 * The shift is arithmetic, which means that the new first bit is the same as the original first bit.
	 * @param int - the integer
	 * @return a right bit shift of 1 bit, dropping the last bit and prepending a copy of the original first bit
	 */
	private bsr(): Int16 {
		return new Int16([
			this.internal[0],
			...this.internal.slice(0, -1)
		] as Int16DatatypeMutable)
	}
	/**
	 * Is the 16-bit signed integer negative?
	 * @returns Is this integer less than `0`?
	 */
	private lt0(): boolean {
		return this.internal[0] === true
	}
	/**
	 * Is the 16-bit signed integer even?
	 * @returns Is this integer divisible by `2`?
	 */
	private isEven(): boolean {
		return this.internal[Int16.BITCOUNT - 1] === false
	}
}
