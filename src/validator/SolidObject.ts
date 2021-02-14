import {
	strictEqual,
} from '../decorators';
import {SolidLanguageType} from './SolidLanguageType';
import type {SolidNull}    from './SolidNull';
import type {SolidBoolean} from './SolidBoolean';



/**
 * Parent class for all Solid Language Values.
 * Known subclasses:
 * - Null
 * - Boolean
 * - Int16
 * - Float64
 */
export class SolidObject {
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
	static union: SolidLanguageType['union'] = SolidLanguageType.prototype.union
	/** @implements SolidLanguageType */
	@SolidLanguageType.subtypeDeco
	static isSubtypeOf(t: SolidLanguageType): boolean {
		return (t instanceof Function)
			? this/*static*/.prototype instanceof t
			: SolidLanguageType.prototype.isSubtypeOf.call(this, t)
	}
	/** @implements SolidLanguageType */
	static equals: SolidLanguageType['equals'] = SolidLanguageType.prototype.equals

	/**
	 * Decorator for {@link SolidObject#equal} method and any overrides.
	 * Performs the Equality algorithm — returns whether two Objects (Solid Language Values)
	 * are equal by some definition.
	 * @param   _prototype    the prototype that has the method to be decorated
	 * @param   _property_key the name of the method to be decorated
	 * @param   descriptor    the Property Descriptor of the prototype’s method
	 * @returns               `descriptor`, with a new value that is the decorated method
	 */
	protected static equalsDeco(
		_prototype: SolidObject,
		_property_key: string,
		descriptor: TypedPropertyDescriptor<(this: SolidObject, value: SolidObject) => boolean>,
	): typeof descriptor {
		const method = descriptor.value!;
		descriptor.value = function (value) {
			return this.identical(value) || method.call(this, value);
		};
		return descriptor;
	}


	/**
	 * Return the “logical value” of this value.
	 * @returns the associated Boolean value of this value
	 */
	get isTruthy(): SolidBoolean {
		const SolidNull_Class:    typeof SolidNull    = require('./SolidNull')   .SolidNull;
		const SolidBoolean_Class: typeof SolidBoolean = require('./SolidBoolean').SolidBoolean;
		return (
			(this instanceof SolidNull_Class)    ? SolidBoolean_Class.FALSE :
			(this instanceof SolidBoolean_Class) ? this :
			SolidBoolean_Class.TRUE
		)
	}
	/**
	 * Is this value the same exact object as the argument?
	 * @param value the object to compare
	 * @returns are the objects identically the same?
	 */
	@strictEqual
	identical(_value: SolidObject): boolean {
		return false
	}
	/**
	 * Are the values considered equal?
	 * If {@link this.identical} returns `true`, this method will return `true`.
	 * @param value the object to compare
	 * @returns are the objects equal?
	 */
	@strictEqual
	@SolidObject.equalsDeco
	equal(_value: SolidObject): boolean {
		return false
	}
}
