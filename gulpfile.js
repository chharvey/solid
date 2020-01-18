const fs = require('fs')
const util = require('util')

const gulp       = require('gulp')
const typescript = require('gulp-typescript')
// require('typescript') // DO NOT REMOVE … peerDependency of `gulp-typescript`

const tsconfig      = require('./tsconfig.json')
const typedocconfig = tsconfig.typedocOptions


function dist() {
	return gulp.src('./src/**/*.ts')
		.pipe(typescript(tsconfig.compilerOptions))
		.pipe(gulp.dest('./build/'))
}

async function test() {
	const {Scanner, Lexer, Translator, Parser} = require('./')
	const {default: Grammar} = require('./build/class/Grammar.class')
	const input = util.promisify(fs.readFile)('./test/test-v0.1.solid', 'utf8')

	console.log("\n\nHere are the characters returned by the scanner:")
	console.log("  line col  character")
	const scanner = new Scanner(await input).generate()
	let iterator_result_char = scanner.next()
	while (!iterator_result_char.done) {
		console.log(iterator_result_char.value.toString())
		iterator_result_char = scanner.next()
	}

	console.log("\n\nHere are the tokens returned by the lexer:")
	const lexer = new Lexer(await input).generate()
	let iterator_result_token = lexer.next()
	while (!iterator_result_token.done) {
		console.log(iterator_result_token.value.serialize())
		iterator_result_token = lexer.next()
	}

	console.log("\n\nHere are the tokens returned by the translator:")
	const translator = new Translator(await input).generate()
	let iterator_result_tokentrans = translator.next()
	while (!iterator_result_tokentrans.done) {
		if (iterator_result_tokentrans.value !== null) console.log(iterator_result_tokentrans.value.serialize())
		iterator_result_tokentrans = translator.next()
	}

	const {
		ProductionFile,
		ProductionExpression,
		ProductionExpressionAdditive,
		ProductionExpressionMultiplicative,
		ProductionExpressionExponential,
		ProductionExpressionUnarySymbol,
		ProductionExpressionUnit,
	} = require('./build/class/Production.class')
	const solid_grammar = new Grammar([
		ProductionFile.instance,
		ProductionExpression.instance,
		ProductionExpressionAdditive.instance,
		ProductionExpressionMultiplicative.instance,
		ProductionExpressionExponential.instance,
		ProductionExpressionUnarySymbol.instance,
		ProductionExpressionUnit.instance,
	])
	console.log("\n\nThe parse tree returned by the parser is written to file: `./sample/output.xml`")
	const tree = new Parser(solid_grammar).parse(await input)
	fs.writeFileSync('./sample/output.xml', tree.serialize())

	console.log("\n\nThe semantic tree returned by the decorator is written to file: `./sample/output-1.xml`")
	fs.writeFileSync('./sample/output-1.xml', tree.decorate().serialize())

	return Promise.resolve(null)
}

const build = gulp.series(dist, test)

async function random() {
	const Grammar = require('./build/class/Grammar.class.js').default
	const {
		ProductionFile,
		ProductionExpression,
		ProductionExpressionAdditive,
		ProductionExpressionMultiplicative,
		ProductionExpressionExponential,
		ProductionExpressionUnarySymbol,
		ProductionExpressionUnit,
	} = require('./build/class/Production.class')
	const solid_grammar = new Grammar([
		ProductionFile.instance,
		ProductionExpression.instance,
		ProductionExpressionAdditive.instance,
		ProductionExpressionMultiplicative.instance,
		ProductionExpressionExponential.instance,
		ProductionExpressionUnarySymbol.instance,
		ProductionExpressionUnit.instance,
	])
	console.log(solid_grammar.rules.map((r) => r.toString()))
	console.log(solid_grammar.random().join(' ').replace(/\u000d/g, ' '))
	return Promise.resolve(null)
}


module.exports = {
	build,
		dist,
		test,
	random,
}
