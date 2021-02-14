/**
 * Interface for Solid configurations.
 */
export type SolidConfig = {
	/**
	 * Toggles for which language features to enable/disable.
	 * Disabling features can improve compiler speed.
	 */
	readonly languageFeatures: {
		/**
		 * Solid code comments.
		 * @version v0.2.0
		 * @default true
		 */
		readonly comments: boolean,
		/**
		 * Integer literals with a specified radix or “base.”
		 * @version v0.2.0
		 * @default false
		 */
		readonly integerRadices: boolean,
		/**
		 * Separators in numeric tokens.
		 * @version v0.2.0
		 * @default false
		 */
		readonly numericSeparators: boolean,
	},
	/**
	 * Options for the compiler.
	 */
	readonly compilerOptions: {
		/**
		 * Compute constant expressions at compile-time.
		 * @version v0.1.0
		 * @default true
		 */
		readonly constantFolding: boolean,
		/**
		 * Coerce integers into floats if mixed with floats in arithmetic expressions.
		 * @version v0.2.0
		 * @default true
		 */
		readonly intCoercion: boolean,
	},
}

export const CONFIG_DEFAULT: SolidConfig = require('../../src/core/solid-config.default.json');
