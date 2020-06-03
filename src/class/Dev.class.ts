import * as semver from 'semver'

/**
 * Development utilities. Not for production.
 */
export default class Dev {
	/** The current version of this project (as defined in `package.json`). */
	static readonly VERSION: string = require('../../package.json').version

	/**
	 * A map of features to their version numbers.
	 *
	 * These are flags for enabling development of features in pre-production;
	 * they are *not* {@link SolidConfig|compiler feature toggles} for production
	 * (which are used by consumers).
	 * These feature flags are given here in the codebase, whereas
	 * feature toggles are not given until a consumer provides them per each compile.
	 *
	 * Before each release, the feature flags for that release should be removed
	 * and those features should become fully enabled.
	 * Released features may have an optional feature toggle defined in {@link SolidConfig}.
	 */
	private static readonly FEATURES: {
		// v0.1.0
		readonly literalNumber   : string,
		readonly operatorsMath   : string,
		readonly expressions     : string,
		readonly constantFolding : string,
		// v0.2.0
		readonly comments          : string,
		readonly integerRadices    : string,
		readonly numericSeparators : string,
		readonly literalNull       : string,
		readonly literalBoolean    : string,
		readonly operatorsLogic    : string,
		readonly typingImplicit    : string,
		readonly statements        : string,
		// v0.3.0
		readonly literalString   : string,
		readonly literalTemplate : string,
		readonly variables       : string,
		readonly typingExplicit  : string,
	} = {
		// v0.1.0
		literalNumber   : '>=0.1.0',
		operatorsMath   : '>=0.1.0',
		expressions     : '>=0.1.0',
		constantFolding : '>=0.1.0',
		// v0.2.0
		comments          : '>=0.2.0',
		integerRadices    : '>=0.2.0',
		numericSeparators : '>=0.2.0',
		literalNull       : '>=0.2.0',
		literalBoolean    : '>=0.2.0',
		operatorsLogic    : '>=0.2.0',
		typingImplicit    : '>=0.2.0',
		statements        : '>=0.2.0',
		// v0.3.0
		literalString   : '>=0.3.0',
		literalTemplate : '>=0.3.0',
		variables       : '>=0.3.0',
		typingExplicit  : '>=0.3.0',
	}

	/**
	 * Return `true` if this project supports the given feature.
	 * @param feature the feature to test
	 * @return is this project’s version number in the range of the feature?
	 */
	static supports(feature: keyof typeof Dev.FEATURES): boolean {
		return semver.satisfies(Dev.VERSION, Dev.FEATURES[feature])
	}
	/**
	 * Returns `true` if this project supports at least one of the given features.
	 * @param features the features to test
	 * @see Dev.supports
	 * @return are any of the given features supported?
	 */
	static supportsAny(...features: (keyof typeof Dev.FEATURES)[]): boolean {
		return features.some((feature) => Dev.supports(feature))
	}
	/**
	 * Returns `true` if this project supports every one of the given features.
	 * @param features the features to test
	 * @see Dev.supports
	 * @return are all of the given features supported?
	 */
	static supportsAll(...features: (keyof typeof Dev.FEATURES)[]): boolean {
		return features.every((feature) => Dev.supports(feature))
	}
}
