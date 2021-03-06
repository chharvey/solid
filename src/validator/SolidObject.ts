import {SolidLanguageType} from './SolidLanguageType';
import type {SolidBoolean} from './SolidBoolean';



/**
 * Parent class for all Solid Language Values.
 * Known subclasses:
 * - Null
 * - Boolean
 * - Int16
 * - Float64
 * - String
 */
export abstract class SolidObject {
	/** @implements SolidLanguageType */
	static isEmpty: SolidLanguageType['isEmpty'] = false
	/** @implements SolidLanguageType */
	static isUniverse: SolidLanguageType['isUniverse'] = false
	/** @implements SolidLanguageType */
	static values: SolidLanguageType['values'] = new Set()
	/** @implements SolidLanguageType */
	static includes(v: SolidObject): boolean {
		return v instanceof this/*static*/
	}
	/** @implements SolidLanguageType */
	static intersect: SolidLanguageType['intersect'] = SolidLanguageType.prototype.intersect
	/** @implements SolidLanguageType */
	static intersect_do: SolidLanguageType['intersect_do'] = SolidLanguageType.prototype.intersect_do
	/** @implements SolidLanguageType */
	static union: SolidLanguageType['union'] = SolidLanguageType.prototype.union
	/** @implements SolidLanguageType */
	static union_do: SolidLanguageType['union_do'] = SolidLanguageType.prototype.union_do
	/** @implements SolidLanguageType */
	static isSubtypeOf: SolidLanguageType['isSubtypeOf'] = SolidLanguageType.prototype.isSubtypeOf
	/** @implements SolidLanguageType */
	static isSubtypeOf_do(t: SolidLanguageType): boolean {
		return (t instanceof Function)
			? this/*static*/.prototype instanceof t
			: SolidLanguageType.prototype.isSubtypeOf_do.call(this, t)
	}
	/** @implements SolidLanguageType */
	static equals: SolidLanguageType['equals'] = SolidLanguageType.prototype.equals


	/**
	 * Return the “logical value” of this value.
	 * @returns the associated Boolean value of this value
	 */
	get isTruthy(): SolidBoolean {
		const SolidBoolean_Class: typeof SolidBoolean = require('./SolidBoolean').SolidBoolean;
		return SolidBoolean_Class.TRUE;
	}
	/**
	 * Return whether this value is “empty”, that is,
	 * it is either falsy, a zero number, an empty string, or an empty collection.
	 */
	get isEmpty(): SolidBoolean {
		return this.isTruthy.not;
	}
	/**
	 * Is this value the same exact object as the argument?
	 * @param value the object to compare
	 * @returns are the objects identically the same?
	 * @final
	 */
	identical(value: SolidObject): boolean {
		return this === value || this.identical_helper(value)
	}
	/**
	 * Helper method for {@link this.identical}. Override as needed.
	 * @param _value the object to compare
	 * @returns are the objects identically the same?
	 */
	protected identical_helper(_value: SolidObject): boolean {
		return false
	}
	/**
	 * Are the values considered equal?
	 * If {@link this.identical} returns `true`, this method will return `true`.
	 * @param value the object to compare
	 * @returns are the objects equal?
	 * @final
	 */
	equal(value: SolidObject): boolean {
		return this.identical(value) || this.equal_helper(value)
	}
	/**
	 * Helper method for {@link this.equal}. Override as needed.
	 * @param _value the object to compare
	 * @returns are the objects equal?
	 */
	protected equal_helper(_value: SolidObject): boolean {
		return false
	}
}
