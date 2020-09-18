import {
	SolidTypeConstant,
} from './SolidLanguageType.class'
import SolidObject from './SolidObject.class'



/**
 * The Solid Language Type `Boolean` has two values: `true` and `false`.
 * These values are constant and the only two instances of this class.
 *
 * The type of the boolean values is this class (the class `Boolean`),
 * but as a shorthand in type declarations that type is referred to as `bool`.
 *
 * @final
 */
export default class SolidBoolean extends SolidObject {
	/**
	 * @override
	 * @final
	 */
	static get isBooleanType(): boolean {
		return true
	}
	/** The Solid Language Value `false`. */
	static readonly FALSE: SolidBoolean = new SolidBoolean()
	/** The Solid Language Value `true`. */
	static readonly TRUE: SolidBoolean = new SolidBoolean(true)
	/** A Unit Type containing only the Solid Language Value `false`. */
	static readonly FALSETYPE: SolidTypeConstant = new SolidTypeConstant(SolidBoolean.FALSE)
	/** A Unit Type containing only the Solid Language Value `true`. */
	static readonly TRUETYPE: SolidTypeConstant = new SolidTypeConstant(SolidBoolean.TRUE)
	/**
	 * Return the Solid Language Value `true` or `false` based on the argument.
	 * @param b a native boolean value
	 * @returns the argument converted into a SolidBoolean
	 */
	static fromBoolean(b: boolean): SolidBoolean {
		return (b) ? SolidBoolean.TRUE : SolidBoolean.FALSE
	}
	/**
	 * Construct a new SolidBoolean object.
	 * @param value The native boolean value of this object.
	 */
	protected constructor (readonly value: boolean = false) {
		super()
	}
	/** @override */
	toString(): string {
		return `${ this.value }`
	}
	/** @override */
	protected identical_helper(value: SolidObject): boolean {
		return value instanceof SolidBoolean && this.value === value.value
	}
	/**
	 * Return the negation of this Boolean.
	 * @returns `true <-|-> false`
	 */
	get not(): SolidBoolean {
		return SolidBoolean.fromBoolean(!this.value)
	}
	/**
	 * Compute the logical conjunction of this value with the argument.
	 * @param sb the right-hand operator
	 * @returns `this && sb`
	 */
	and(sb: SolidBoolean): SolidBoolean {
		return SolidBoolean.fromBoolean(this.value && sb.value)
	}
	/**
	 * Compute the logical disjunction of this value with the argument.
	 * @param sb the right-hand operator
	 * @returns `this || sb`
	 */
	or(sb: SolidBoolean): SolidBoolean {
		return SolidBoolean.fromBoolean(this.value || sb.value)
	}
}
