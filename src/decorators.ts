/**
 * Decorator for memoizing properties.
 * When getting a property, check whether it exists in the “database”.
 * If it does, return that value.
 * If it doen’t, compute the value, store it in the database, and then return it.
 * @param   _prototype    the prototype that has the method to be decorated
 * @param   _property_key the name of the method to be decorated
 * @param   descriptor    the Property Descriptor of the prototype’s method
 * @returns               `descriptor`, with a new value that is the decorated method
 */
export function memoizeMethod<Ps extends unknown[], R>(
	_prototype: object,
	_property_key: string,
	descriptor: TypedPropertyDescriptor<(this: object, ...args: Ps) => R>,
): typeof descriptor {
	const method = descriptor.value!;
	const memomap: WeakMap<object, R> = new WeakMap();
	descriptor.value = function (...args) {
		memomap.has(this) || memomap.set(this, method.call(this, ...args));
		return memomap.get(this)!;
	};
	return descriptor;
}



/**
 * Like {@link memoizeMethod} but for getters.
 * @param   _prototype    the prototype that has the getter to be decorated
 * @param   _property_key the name of the getter to be decorated
 * @param   descriptor    the Property Descriptor of the prototype’s getter
 * @returns               `descriptor`, with a new `get` that is the decorated getter
 */
export function memoizeGetter<R>(
	_prototype: object,
	_property_key: string,
	descriptor: TypedPropertyDescriptor<R>,
): typeof descriptor {
	const method = descriptor.get!;
	const memomap: WeakMap<object, R> = new WeakMap();
	descriptor.get = function () {
		memomap.has(this) || memomap.set(this, method.call(this));
		return memomap.get(this)!;
	};
	return descriptor;
}



/**
 * Decorator for performing strict equality (`===`), and then disjuncting (`||`) that result
 * with the results of performing the method.
 * @param   _prototype    the prototype that has the method to be decorated
 * @param   _property_key the name of the method to be decorated
 * @param   descriptor    the Property Descriptor of the prototype’s method
 * @returns               `descriptor`, with a new value that is the decorated method
 */
export function strictEqual<Proto extends object, Ps extends unknown[]>(
	_prototype: Proto,
	_property_key: string,
	descriptor: TypedPropertyDescriptor<(this: Proto, that: Proto, ...args: Ps) => boolean>,
): typeof descriptor {
	const method = descriptor.value!;
	descriptor.value = function (that, ...args) {
		return this === that || method.call(this, that, ...args);
	};
	return descriptor;
}
