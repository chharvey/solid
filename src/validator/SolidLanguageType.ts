import * as xjs from 'extrajs'

import type {SolidObject} from './SolidObject';



/**
 * Parent class for all Solid Language Types.
 * Known subclasses:
 * - Intersection
 * - Union
 * - TypeInterface
 * - TypeNever
 * - TypeConstant
 * - TypeUnknown
 */
export abstract class SolidLanguageType {
	/** The Bottom Type, containing no values. */
	static get NEVER(): SolidTypeNever { return SolidTypeNever.INSTANCE }
	/** The Top Type, containing all values. */
	static get UNKNOWN(): SolidTypeUnknown { return SolidTypeUnknown.INSTANCE }


	/**
	 * Whether this type has no values assignable to it,
	 * i.e., it is equal to the Bottom Type (`never`).
	 * Used internally for special cases of computations.
	 */
	readonly isEmpty: boolean = this.values.size === 0
	/**
	 * Whether this type has all values assignable to it,
	 * i.e., it is equal to the Top Type (`unknown`).
	 * Used internally for special cases of computations.
	 */
	readonly isUniverse: boolean = false // this.equals(SolidLanguageType.UNKNOWN)

	/**
	 * Construct a new SolidLanguageType object.
	 * @param values an enumerated set of values that are assignable to this type
	 */
	constructor (
		readonly values: ReadonlySet<SolidObject> = new Set(),
	) {
	}

	/**
	 * Return whether this type “includes” the value, i.e.,
	 * whether the value is assignable to this type.
	 * @param v the value to check
	 * @returns Is `v` assignable to this type?
	 */
	includes(v: SolidObject): boolean {
		return this.values.has(v)
	}
	/**
	 * Return the type intersection of this type with another.
	 * @param t the other type
	 * @returns the type intersection
	 * @final
	 */
	intersect(t: SolidLanguageType): SolidLanguageType {
		/** 1-5 | `T  & never   == never` */
		if (t.isEmpty) { return SolidLanguageType.NEVER }
		if (this.isEmpty) { return this }
		/** 1-6 | `T  & unknown == T` */
		if (t.isUniverse) { return this }
		if (this.isUniverse) { return t }

		/** 3-3 | `A <: B  <->  A  & B == A` */
		if (this.isSubtypeOf(t)) { return this }
		if (t.isSubtypeOf(this)) { return t }

		return this.intersect_do(t)
	}
	intersect_do(t: SolidLanguageType): SolidLanguageType { // NOTE: should be protected, but needs to be public because need to implement in SolidObject
		return new SolidTypeIntersection(this, t)
	}
	/**
	 * Return the type union of this type with another.
	 * @param t the other type
	 * @returns the type union
	 * @final
	 */
	union(t: SolidLanguageType): SolidLanguageType {
		/** 1-7 | `T \| never   == T` */
		if (t.isEmpty) { return this }
		if (this.isEmpty) { return t }
		/** 1-8 | `T \| unknown == unknown` */
		if (t.isUniverse) { return t }
		if (this.isUniverse) { return this }

		/** 3-4 | `A <: B  <->  A \| B == B` */
		if (this.isSubtypeOf(t)) { return t }
		if (t.isSubtypeOf(this)) { return this }

		return this.union_do(t)
	}
	union_do(t: SolidLanguageType): SolidLanguageType { // NOTE: should be protected, but needs to be public because need to implement in SolidObject
		return new SolidTypeUnion(this, t)
	}
	/**
	 * Return whether this type is a structural subtype of the given type.
	 * @param t the type to compare
	 * @returns Is this type a subtype of the argument?
	 * @final
	 */
	isSubtypeOf(t: SolidLanguageType): boolean {
		/** 1-3 | `T       <: never  <->  T == never` */
		if (t.isEmpty) { return this.isEmpty }
		/** 1-2 | `T     <: unknown` */
		if (t.isUniverse) { return true }
		/** 2-7 | `A <: A` */
		if (this === t) { return true }

		if (t instanceof SolidTypeIntersection) {
			/** 3-5 | `A <: C    &&  A <: D  <->  A <: C  & D` */
			return this.isSubtypeOf(t.left) && this.isSubtypeOf(t.right)
		}
		if (t instanceof SolidTypeUnion) {
			/** 3-6 | `A <: C  \|\|  A <: D  -->  A <: C \| D` */
			if (this.isSubtypeOf(t.left) || this.isSubtypeOf(t.right)) { return true }
			/** 3-2 | `A <: A \| B  &&  B <: A \| B` */
			if (this.equals(t.left) || this.equals(t.right)) { return true }
		}

		return this.isSubtypeOf_do(t)
	}
	isSubtypeOf_do(t: SolidLanguageType): boolean { // NOTE: should be protected, but needs to be public because need to implement in SolidObject
		return [...this.values].every((v) => t.includes(v))
	}
	/**
	 * Return whether this type is structurally equal to the given type.
	 * Two types are structurally equal if they are subtypes of each other.
	 *
	 * 2-8 | `A <: B  &&  B <: A  -->  A == B`
	 * @param t the type to compare
	 * @returns Is this type equal to the argument?
	 */
	equals(t: SolidLanguageType): boolean {
		return this.isSubtypeOf(t) && t.isSubtypeOf(this)
	}
}



/**
 * A type intersection of two types `T` and `U` is the type
 * that contains values either assignable to `T` *or* assignable to `U`.
 */
class SolidTypeIntersection extends SolidLanguageType {
	/**
	 * Construct a new SolidTypeIntersection object.
	 * @param left the first type
	 * @param right the second type
	 */
	constructor (
		readonly left:  SolidLanguageType,
		readonly right: SolidLanguageType,
	) {
		super(xjs.Set.intersection(left.values, right.values))
	}
	/** @override */
	includes(v: SolidObject): boolean {
		return this.left.includes(v) && this.right.includes(v)
	}
	/** @implements SolidLanguageType */
	isSubtypeOf_do(t: SolidLanguageType): boolean {
		/** 3-8 | `A <: C  \|\|  B <: C  -->  A  & B <: C` */
		if (this.left.isSubtypeOf(t) || this.right.isSubtypeOf(t)) { return true }
		/** 3-1 | `A  & B <: A  &&  A  & B <: B` */
		if (t.equals(this.left) || t.equals(this.right)) { return true }
		return super.isSubtypeOf_do(t)
	}
}



/**
 * A type union of two types `T` and `U` is the type
 * that contains values both assignable to `T` *and* assignable to `U`.
 */
class SolidTypeUnion extends SolidLanguageType {
	/**
	 * Construct a new SolidTypeUnion object.
	 * @param left the first type
	 * @param right the second type
	 */
	constructor (
		readonly left:  SolidLanguageType,
		readonly right: SolidLanguageType,
	) {
		super(xjs.Set.union(left.values, right.values))
	}
	/** @override */
	includes(v: SolidObject): boolean {
		return this.left.includes(v) || this.right.includes(v)
	}
	/** @implements SolidLanguageType */
	isSubtypeOf_do(t: SolidLanguageType): boolean {
		/** 3-7 | `A <: C    &&  B <: C  <->  A \| B <: C` */
		return this.left.isSubtypeOf(t) && this.right.isSubtypeOf(t)
	}
}



/**
 * An Interface Type is a set of properties that a value must have.
 */
export class SolidTypeInterface extends SolidLanguageType {
	/** @override */
	readonly isEmpty: boolean = [...this.properties.values()].some((value) => value.isEmpty)
	/** @override */
	readonly isUniverse: boolean = this.properties.size === 0

	/**
	 * Construct a new SolidLanguageType object.
	 * @param properties a map of this type’s members’ names along with their associated types
	 */
	constructor (readonly properties: ReadonlyMap<string, SolidLanguageType>) {
		super()
	}

	/** @override */
	includes(v: SolidObject): boolean {
		return [...this.properties.keys()].every((key) => key in v)
	}
	/**
	 * @override
	 * The *intersection* of types `S` and `T` is the *union* of the set of properties on `T` with the set of properties on `S`.
	 * If any properties disagree on type, their type intersection is taken.
	 */
	intersect_do(t: SolidTypeInterface): SolidTypeInterface {
		const props: Map<string, SolidLanguageType> = new Map([...this.properties])
		;[...t.properties].forEach(([name, type_]) => {
			props.set(name, (props.has(name)) ? props.get(name)!.intersect(type_) : type_)
		})
		return new SolidTypeInterface(props)
	}
	/**
	 * @override
	 * The *union* of types `S` and `T` is the *intersection* of the set of properties on `T` with the set of properties on `S`.
	 * If any properties disagree on type, their type union is taken.
	 */
	union_do(t: SolidTypeInterface): SolidTypeInterface {
		const props: Map<string, SolidLanguageType> = new Map()
		;[...this.properties].forEach(([name, type_]) => {
			if (t.properties.has(name)) {
				props.set(name, type_.union(t.properties.get(name)!))
			}
		})
		return new SolidTypeInterface(props)
	}
	/**
	 * @implements SolidLanguageType
	 * In the general case, `S` is a subtype of `T` if every property of `T` exists in `S`,
	 * and for each of those properties `#prop`, the type of `S#prop` is a subtype of `T#prop`.
	 * In other words, `S` is a subtype of `T` if the set of properties on `T` is a subset of the set of properties on `S`.
	 */
	isSubtypeOf_do(t: SolidTypeInterface) {
		return [...t.properties].every(([name, type_]) =>
			this.properties.has(name) && this.properties.get(name)!.isSubtypeOf(type_)
		)
	}
}



/**
 * Class for constructing the Bottom Type, the type containing no values.
 */
class SolidTypeNever extends SolidLanguageType {
	static readonly INSTANCE: SolidTypeNever = new SolidTypeNever()

	/** @override */
	readonly isEmpty: boolean = true
	/** @override */
	readonly isUniverse: boolean = false

	private constructor () {
		super()
	}

	/** @override */
	includes(_v: SolidObject): boolean {
		return false
	}
	/**
	 * @implements SolidLanguageType
	 * 1-1 | `never <: T`
	 */
	isSubtypeOf_do(_t: SolidLanguageType): boolean {
		return true
	}
	/** @override */
	equals(t: SolidLanguageType): boolean {
		return t.isEmpty
	}
}



/**
 * Class for constructing constant types / unit types, types that contain one value.
 */
export class SolidTypeConstant extends SolidLanguageType {
	/** @override */
	readonly isEmpty: boolean = false
	/** @override */
	readonly isUniverse: boolean = false

	constructor (readonly value: SolidObject) {
		super(new Set([value]))
	}

	/** @override */
	includes(_v: SolidObject): boolean {
		return this.value.equal(_v)
	}
	/** @implements SolidLanguageType */
	isSubtypeOf_do(t: SolidLanguageType): boolean {
		return t instanceof Function && this.value instanceof t || t.includes(this.value)
	}
}



/**
 * Class for constructing the Top Type, the type containing all values.
 */
class SolidTypeUnknown extends SolidLanguageType {
	static readonly INSTANCE: SolidTypeUnknown = new SolidTypeUnknown()

	/** @override */
	readonly isEmpty: boolean = false
	/** @override */
	readonly isUniverse: boolean = true

	private constructor () {
		super()
	}

	/** @override */
	includes(_v: SolidObject): boolean {
		return true
	}
	/**
	 * @implements SolidLanguageType
	 * 1-4 | `unknown <: T      <->  T == unknown`
	 */
	isSubtypeOf_do(t: SolidLanguageType): boolean {
		return t.isUniverse
	}
	/** @override */
	equals(t: SolidLanguageType): boolean {
		return t.isUniverse
	}
}
