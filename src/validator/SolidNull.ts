import {
	strictEqual,
} from '../decorators';
import type {SolidLanguageType} from './SolidLanguageType';
import {SolidObject} from './SolidObject';



/**
 * The class for the Solid Language Value `null`.
 *
 * A Null object is used as a placeholder for missing values.
 * It has no fields or methods, and it is “falsy” when used as a condition.
 *
 * This class is a singleton: there exists only one instance.
 * The reference to the instance of this class is a constant named `null`.
 *
 * The type of the value `null` is this class (the class `Null`),
 * but as a shorthand in type declarations that type is referred to as `null`.
 *
 * @final
 */
export class SolidNull extends SolidObject {
	/** The Solid Language Value `null`. */
	static readonly NULL: SolidNull = new SolidNull()
	/** @override */
	static values: SolidLanguageType['values'] = new Set([SolidNull.NULL])


	private constructor () {
		super()
	}
	/** @override */
	toString(): string {
		return 'null'
	}
	/** @overrides SolidObject */
	@strictEqual
	identical(value: SolidObject): boolean {
		return value instanceof SolidNull
	}
}
