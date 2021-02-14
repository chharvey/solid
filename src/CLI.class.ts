import * as fs from 'fs'
import * as path from 'path'

import minimist from 'minimist' // need `tsconfig.json#compilerOptions.esModuleInterop = true`

import {
	SolidConfig,
	CONFIG_DEFAULT,
} from './core/';
import {
	Builder,
} from './builder/';



type Mutable<T> = { // NB https://github.com/microsoft/TypeScript/issues/24509
	-readonly[P in keyof T]: Mutable<T[P]>
}

type PartialSolidConfig = Partial<{
	readonly languageFeatures: Partial<SolidConfig['languageFeatures']>,
	readonly compilerOptions:  Partial<SolidConfig['compilerOptions']>,
}>

export enum Command {
	HELP,
	VERSION,
	COMPILE,
	DEV,
	RUN,
}

type CustomArgsType = {
	// CLI Options
	/** Display help text. */
	help: boolean;
	/** Display version number. */
	version: boolean;
	/** Specify output filepath. */
	out: string;
	/** Specify configuration filepath. */
	project: string;
	/** Display configuration options. */
	config: boolean;

	// Language Feature Toggles
	comments          : null | boolean,
	integerRadices    : null | boolean,
	numericSeparators : null | boolean,

	// Compiler Options
	constantFolding: null | boolean,
	intCoercion:     null | boolean,
}



/**
 * Code for the command line interface.
 * A CLI object is a single instance of a CLI run.
 */
export class CLI {
	/** Text to print on --help. */
	static readonly HELPTEXT: string = `
		Usage: solid <command> <filepath> [<options>]

		Parse, analyze, and compile a Solid source code file.
		Executables are in WASM binary format. Plaintext outputs are in WAT format.
		See [WebAssembly](https://webassembly.org/) for details.

		Examples:
		\`\`\`
		# Compile \`test.solid\` to \`test.wasm\`:
		$ solid compile test.solid

		# Compile \`src/input.solid\` to \`build/output.wasm\`:
		$ solid compile src/input.solid --out build/output.wasm

		# Debug \`program.solid\` (writes to \`program.wat\`):
		$ solid dev program.solid

		# Execute \`program.wasm\`:
		$ solid run program.wasm
		\`\`\`

		Commands:
		help                           Print this help message.
		version                        Print the version of Solid currently installed.
		c, compile                     Compile a Solid file into a \`*.wasm\` executable binary.
		d, dev                         Compile a Solid file into a \`*.wat\` text file for inspection.
		r, run                         Execute a compiled binary file. Ignore the \`--out\` option.

		Options:
		-h, --help                     Print this help message.
		-v, --version                  Print the version of Solid currently installed.
		-o, --out=file                 Specify the output file.
		                               Otherwise, the default output filepath is the input filepath except
		                               with the extension changed to \`.wasm\` (compile) or \`.wat\` (dev).
		-p, --project=file             Specify a configuration file.
		--config                       Print all possible configuration options.
	`.trim().replace(/\n\t\t/g, '\n')

	/** Text to print on --config. */
	static readonly CONFIGTEXT: string = `
		The following options set individual language feature toggles and compiler options.
		These options will override those in the configuration file provided by \`--project\`.

		Language Feature Toggles:
		--[no-]comments                (on by default)
		--[no-]integerRadices
		--[no-]numericSeparators

		Compiler Options:
		--[no-]constantFolding         (on by default)
		--[no-]intCoercion             (on by default)
	`.trim().replace(/\n\t\t/g, '\n')

	/** Options argument to `minimist` function. */
	private static readonly MINIMIST_OPTS: minimist.Opts = {
		boolean: [
			// CLI Options
			'help',
			'version',
			'config',
			// Language Feature Toggles
			'comments',
			'integerRadices',
			'numericSeparators',
			// Compiler Options
			'constantFolding',
			'intCoercion',
		],
		string: [
			// CLI Options
			'out',
			'project',
		],
		alias: {
			h: 'help',
			v: 'version',
			o: 'out',
			p: 'project',
		},
		default: {
			// CLI Options
			help    : false,
			version : false,
			config  : false,
			// Language Feature Toggles
			comments          : null,
			integerRadices    : null,
			numericSeparators : null,
			// Compiler Options
			constantFolding: null,
			intCoercion:     null,
		},
		unknown(arg) {
			if (arg[0] === '-') { // only check unsupported options // NB https://github.com/substack/minimist/issues/86
				throw new Error(`
					Unknown CLI option: ${ arg }
					${ CLI.HELPTEXT }
				`)
			}
			return true
		},
	}


	readonly command: Command;
	readonly argv: CustomArgsType & minimist.ParsedArgs;
	/**
	 * Construct a new CLI object.
	 * @param process_argv the arguments sent to NodeJS.Process.argv
	 */
	constructor (process_argv: readonly string[]) {
		this.argv = minimist<CustomArgsType>(process_argv.slice(2), CLI.MINIMIST_OPTS)
		this.command =
			(this.argv.help || this.argv.config) ? Command.HELP :
			(this.argv.version) ? Command.VERSION :
			new Map<string, Command>([
				['help'    , Command.HELP],
				['version' , Command.VERSION],
				['compile' , Command.COMPILE],
				['c'       , Command.COMPILE],
				['dev'     , Command.DEV],
				['d'       , Command.DEV],
				['run'     , Command.RUN],
				['r'       , Command.RUN],
			]).get(this.argv._[0]) || Command.HELP
		if (!(
			(this.argv.out     === void 0 || typeof this.argv.out     === 'string' && this.argv.out     !== '') &&
			(this.argv.project === void 0 || typeof this.argv.project === 'string' && this.argv.project !== '')
		)) throw new Error(`
			Invalid CLI arguments!
			${ CLI.HELPTEXT }
		`)
	}

	/**
	 * Compute the cascading configuration for this CLI.
	 * Cascade in order of precedence (1 is lowest):
	 * 1. The Solid Default Configuration file.
	 * 2. A configuration file specified via `--project` CLI option.
	 * 3. Any individual CLI options.
	 * @param cwd the current working directory, `process.cwd()`
	 * @return the computed configuration object
	 */
	private async computeConfig(cwd: string): Promise<SolidConfig> {
		const config: PartialSolidConfig | Promise<PartialSolidConfig> = this.argv.project ?
			fs.promises.readFile(path.join(cwd, path.normalize(this.argv.project)), 'utf8').then((text) => JSON.parse(text))
		: {}

		const returned: Mutable<SolidConfig> = {
			...CONFIG_DEFAULT,
			...await config,
			languageFeatures: {
				...CONFIG_DEFAULT.languageFeatures,
				...(await config).languageFeatures,
			},
			compilerOptions: {
				...CONFIG_DEFAULT.compilerOptions,
				...(await config).compilerOptions,
			},
		}

		if (this.argv.comments          !== null) returned.languageFeatures.comments          = this.argv.comments
		if (this.argv.integerRadices    !== null) returned.languageFeatures.integerRadices    = this.argv.integerRadices
		if (this.argv.numericSeparators !== null) returned.languageFeatures.numericSeparators = this.argv.numericSeparators
		if (this.argv.constantFolding   !== null) returned.compilerOptions.constantFolding    = this.argv.constantFolding
		if (this.argv.intCoercion       !== null) returned.compilerOptions.intCoercion        = this.argv.intCoercion

		return returned
	}

	/**
	 * Helper method for validating input path.
	 * @param cwd the current working directory, `process.cwd()`
	 * @return a valid path to the input file
	 */
	private inputPath(cwd: string): string {
		if (!this.argv._[1]) throw new Error(`
			No path specified!
			${ CLI.HELPTEXT }
		`)
		return path.join(cwd, path.normalize(this.argv._[1]))
	}

	/**
	 * Run the command `compile` or `dev`.
	 * @param cwd the current working directory, `process.cwd()`
	 */
	async compileOrDev(cwd: string): Promise<[string, void]> {
		const inputfilepath: string = this.inputPath(cwd)
		const outputfilepath: string = this.argv.out ? path.join(cwd, path.normalize(this.argv.out)) : path.format({
			...path.parse(inputfilepath),
			base: void 0,
			ext: this.command === Command.DEV ? '.wat' : '.wasm',
		})
		const cg: Builder = new Builder(...await Promise.all([
			fs.promises.readFile(inputfilepath, 'utf8'),
			this.computeConfig(cwd),
		]))
		return Promise.all([
			`
				Compiling………
				Source file: ${ inputfilepath }
				${this.command === Command.DEV
					? `Intermediate text file (for debugging):`
					: `Destination binary file:`
				} ${ outputfilepath }
			`.trim().replace(/\n\t\t\t\t/g, '\n'),
			fs.promises.writeFile(outputfilepath, this.command === Command.DEV ? cg.print() : await cg.compile()),
		])
	}

	/**
	 * Run the command `run`.
	 * @param cwd the current working directory, `process.cwd()`
	 */
	async run(cwd: string): Promise<[string, ...unknown[]]> {
		const inputfilepath: string = this.inputPath(cwd)
		const bytes: Promise<Buffer> = fs.promises.readFile(inputfilepath)
		return [
			`
				Executing………
				Binary path: ${ inputfilepath }
			`.trim().replace(/\n\t\t\t\t/g, '\n'),
			...(Object.values((await WebAssembly.instantiate(await bytes)).instance.exports) as Function[]).map((func) => func()),
		]
	}
}
