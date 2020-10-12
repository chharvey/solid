import {
	Char,
	Token,
	TokenWhitespace,
	TokenComment,
	ParseNode,
	Lexer,
} from '@chharvey/parser';

import type {NonemptyArray} from '../types.d'
import {
	Screener,
} from '../lexer/'
import {
	Parser,
	Grammar,
	Production,
} from '../parser/'
import * as TOKEN from './Token.class'
import * as PRODUCTION from './Production.auto'
import * as PARSENODE from './ParseNode.auto'
import * as SEMANTICNODE from './SemanticNode.class'



export class LexerEBNF extends Lexer {
	protected generate_do(): Token | null {
			let token: Token;
			if (Char.inc(TOKEN.TokenPunctuator.PUNCTUATORS_4, this.c0, this.c1, this.c2, this.c3)) {
				token = new TOKEN.TokenPunctuator(this, 4n)
			} else if (Char.inc(TOKEN.TokenPunctuator.PUNCTUATORS_3, this.c0, this.c1, this.c2)) {
				token = new TOKEN.TokenPunctuator(this, 3n)
			} else if (Char.inc(TOKEN.TokenPunctuator.PUNCTUATORS_2, this.c0, this.c1)) {
				token = new TOKEN.TokenPunctuator(this, 2n)
			} else if (Char.inc(TOKEN.TokenPunctuator.PUNCTUATORS_1, this.c0)) {
				if (Char.eq(TOKEN.TokenCharCode.START, this.c0, this.c1)) {
					/* we found a char code */
					token = new TOKEN.TokenCharCode(this)
				} else {
					/* we found a Kleene hash or another punctuator */
					token = new TOKEN.TokenPunctuator(this)
				}

			} else if (TOKEN.TokenIdentifier.START.test(this.c0.source)) {
				token = new TOKEN.TokenIdentifier(this)

			} else if (Char.eq(TOKEN.TokenString.DELIM, this.c0)) {
				token = new TOKEN.TokenString(this)

			} else if (Char.eq(TOKEN.TokenCharClass.DELIM_START, this.c0)) {
				token = new TOKEN.TokenCharClass(this)

			} else if (Char.eq(TOKEN.TokenCommentEBNF.DELIM_START, this.c0, this.c1)) {
				token = new TOKEN.TokenCommentEBNF(this)

			} else {
				return null
			}
			return token
	}
}



export class ScreenerEBNF extends Screener {
	constructor (source: string) {
		super(new LexerEBNF(source).generate())
	}
	* generate(): Generator<Token> {
		while (!this.isDone) {
			if (!(this.t0 instanceof TokenWhitespace) && !(this.t0 instanceof TokenComment)) {
				yield this.t0
			}
			this.advance()
		}
	}
}



export class ParserEBNF extends Parser {
	constructor (source: string) {
		super(new ScreenerEBNF(source).generate(), new Grammar([
			PRODUCTION.ProductionParameterSet__0__List .instance,
			PRODUCTION.ProductionParameterSet          .instance,
			PRODUCTION.ProductionArgumentSet__0__List  .instance,
			PRODUCTION.ProductionArgumentSet           .instance,
			PRODUCTION.ProductionConditionSet__0__List .instance,
			PRODUCTION.ProductionConditionSet          .instance,
			PRODUCTION.ProductionReference             .instance,
			PRODUCTION.ProductionUnit                  .instance,
			PRODUCTION.ProductionUnary                 .instance,
			PRODUCTION.ProductionItem                  .instance,
			PRODUCTION.ProductionOrder                 .instance,
			PRODUCTION.ProductionConcat                .instance,
			PRODUCTION.ProductionAltern                .instance,
			PRODUCTION.ProductionDefinition            .instance,
			PRODUCTION.ProductionNonterminalName       .instance,
			PRODUCTION.ProductionProduction            .instance,
			PRODUCTION.ProductionGrammar__0__List      .instance,
			PRODUCTION.ProductionGrammar               .instance,
		], PRODUCTION.ProductionGrammar.instance), new Map<Production, typeof ParseNode>([
			[PRODUCTION.ProductionParameterSet__0__List .instance, PARSENODE.ParseNodeParameterSet__0__List],
			[PRODUCTION.ProductionParameterSet          .instance, PARSENODE.ParseNodeParameterSet],
			[PRODUCTION.ProductionArgumentSet__0__List  .instance, PARSENODE.ParseNodeArgumentSet__0__List],
			[PRODUCTION.ProductionArgumentSet           .instance, PARSENODE.ParseNodeArgumentSet],
			[PRODUCTION.ProductionConditionSet__0__List .instance, PARSENODE.ParseNodeConditionSet__0__List],
			[PRODUCTION.ProductionConditionSet          .instance, PARSENODE.ParseNodeConditionSet],
			[PRODUCTION.ProductionReference             .instance, PARSENODE.ParseNodeReference],
			[PRODUCTION.ProductionUnit                  .instance, PARSENODE.ParseNodeUnit],
			[PRODUCTION.ProductionUnary                 .instance, PARSENODE.ParseNodeUnary],
			[PRODUCTION.ProductionItem                  .instance, PARSENODE.ParseNodeItem],
			[PRODUCTION.ProductionOrder                 .instance, PARSENODE.ParseNodeOrder],
			[PRODUCTION.ProductionConcat                .instance, PARSENODE.ParseNodeConcat],
			[PRODUCTION.ProductionAltern                .instance, PARSENODE.ParseNodeAltern],
			[PRODUCTION.ProductionDefinition            .instance, PARSENODE.ParseNodeDefinition],
			[PRODUCTION.ProductionNonterminalName       .instance, PARSENODE.ParseNodeNonterminalName],
			[PRODUCTION.ProductionProduction            .instance, PARSENODE.ParseNodeProduction],
			[PRODUCTION.ProductionGrammar__0__List      .instance, PARSENODE.ParseNodeGrammar__0__List],
			[PRODUCTION.ProductionGrammar               .instance, PARSENODE.ParseNodeGrammar],
		]))
	}
}



export class Decorator {
	private static readonly OPS_UN: ReadonlyMap<string, 'plus' | 'star' | 'hash' | 'opt'> = new Map<string, 'plus' | 'star' | 'hash' | 'opt'>([
		[`+`, 'plus'],
		[`*`, 'star'],
		[`#`, 'hash'],
		[`?`, 'opt'],
	])
	private static readonly OPS_BIN: ReadonlyMap<string, 'order' | 'concat' | 'altern'> = new Map<string, 'order' | 'concat' | 'altern'>([
		[`.`, 'order'],
		[`&`, 'concat'],
		[`|`, 'altern'],
	])
	private static readonly PARAMOPS: ReadonlyMap<string, boolean | 'inherit'> = new Map<string, boolean | 'inherit'>([
		[`+`, true],
		[`-`, false],
		[`?`, 'inherit'],
	])


	/**
	 * Return a JSON object describing an EBNF production.
	 * Similar to a node of the Semantic Tree or “decorated/abstract syntax tree”.
	 * @returns a JSON object containing the parse node’s semantics
	 */
	decorate(node: PARSENODE.ParseNodeParameterSet__0__List): NonemptyArray<SEMANTICNODE.SemanticNodeParam>;
	decorate(node: PARSENODE.ParseNodeParameterSet):          NonemptyArray<SEMANTICNODE.SemanticNodeParam>;
	decorate(node: PARSENODE.ParseNodeArgumentSet__0__List):  NonemptyArray<SEMANTICNODE.SemanticNodeArg>;
	decorate(node: PARSENODE.ParseNodeArgumentSet):           NonemptyArray<SEMANTICNODE.SemanticNodeArg>;
	decorate(node: PARSENODE.ParseNodeConditionSet__0__List): NonemptyArray<SEMANTICNODE.SemanticNodeCondition>;
	decorate(node: PARSENODE.ParseNodeConditionSet):          NonemptyArray<SEMANTICNODE.SemanticNodeCondition>;
	decorate(node: PARSENODE.ParseNodeReference):             SEMANTICNODE.SemanticNodeRef;
	decorate(node: PARSENODE.ParseNodeUnit):                  SEMANTICNODE.SemanticNodeExpr;
	decorate(node: PARSENODE.ParseNodeUnary):                 SEMANTICNODE.SemanticNodeExpr;
	decorate(node: PARSENODE.ParseNodeItem):                  SEMANTICNODE.SemanticNodeExpr;
	decorate(node: PARSENODE.ParseNodeOrder):                 SEMANTICNODE.SemanticNodeExpr;
	decorate(node: PARSENODE.ParseNodeConcat):                SEMANTICNODE.SemanticNodeExpr;
	decorate(node: PARSENODE.ParseNodeAltern):                SEMANTICNODE.SemanticNodeExpr;
	decorate(node: PARSENODE.ParseNodeDefinition):            SEMANTICNODE.SemanticNodeExpr;
	decorate(node: PARSENODE.ParseNodeNonterminalName):       SEMANTICNODE.SemanticNodeNonterminal;
	decorate(node: PARSENODE.ParseNodeProduction):            SEMANTICNODE.SemanticNodeProduction;
	decorate(node: PARSENODE.ParseNodeGrammar__0__List):      NonemptyArray<SEMANTICNODE.SemanticNodeProduction>;
	decorate(node: PARSENODE.ParseNodeGrammar):               SEMANTICNODE.SemanticNodeGrammar;
	decorate(node: ParseNode): SEMANTICNODE.SemanticNodeEBNF | readonly SEMANTICNODE.SemanticNodeEBNF[];
	decorate(node: ParseNode): SEMANTICNODE.SemanticNodeEBNF | readonly SEMANTICNODE.SemanticNodeEBNF[] {
		if (node instanceof PARSENODE.ParseNodeParameterSet__0__List) {
			function decorateParam(identifier: TOKEN.TokenIdentifier): SEMANTICNODE.SemanticNodeParam {
				return new SEMANTICNODE.SemanticNodeParam(identifier)
			}
			return (node.children.length === 1)
				? [
					decorateParam(node.children[0] as TOKEN.TokenIdentifier),
				]
				: [
					...this.decorate(node.children[0]),
					decorateParam(node.children[2] as TOKEN.TokenIdentifier),
				]

		} else if (node instanceof PARSENODE.ParseNodeParameterSet) {
			return this.decorate(node.children[1])

		} else if (node instanceof PARSENODE.ParseNodeArgumentSet__0__List) {
			function decorateArg(identifier: TOKEN.TokenIdentifier, append: TOKEN.TokenPunctuator): SEMANTICNODE.SemanticNodeArg {
				return new SEMANTICNODE.SemanticNodeArg(identifier, Decorator.PARAMOPS.get(append.source)!)
			}
			return (node.children.length === 2)
				? [
					decorateArg(
						node.children[1] as TOKEN.TokenIdentifier,
						node.children[0] as TOKEN.TokenPunctuator,
					),
				]
				: [
					...this.decorate(node.children[0]),
					decorateArg(
						node.children[3] as TOKEN.TokenIdentifier,
						node.children[2] as TOKEN.TokenPunctuator,
					),
				]

		} else if (node instanceof PARSENODE.ParseNodeArgumentSet) {
			return this.decorate(node.children[1])

		} else if (node instanceof PARSENODE.ParseNodeConditionSet__0__List) {
			function decorateCondition(identifier: TOKEN.TokenIdentifier, include: TOKEN.TokenPunctuator): SEMANTICNODE.SemanticNodeCondition {
				return new SEMANTICNODE.SemanticNodeCondition(identifier, Decorator.PARAMOPS.get(include.source) as boolean)
			}
			return (node.children.length === 2)
				? [
					decorateCondition(
						node.children[0] as TOKEN.TokenIdentifier,
						node.children[1] as TOKEN.TokenPunctuator,
					),
				]
				: [
					...this.decorate(node.children[0]),
					decorateCondition(
						node.children[2] as TOKEN.TokenIdentifier,
						node.children[3] as TOKEN.TokenPunctuator,
					),
				]

		} else if (node instanceof PARSENODE.ParseNodeConditionSet) {
			return this.decorate(node.children[1])

		} else if (node instanceof PARSENODE.ParseNodeReference) {
			return (node.children.length === 1)
				? new SEMANTICNODE.SemanticNodeRef(
					node,
					node.children[0] as TOKEN.TokenIdentifier,
				)
				: new SEMANTICNODE.SemanticNodeRef(
					node,
					this.decorate(node.children[0]),
					this.decorate(node.children[1]),
				)

		} else if (node instanceof PARSENODE.ParseNodeUnit) {
			return (node.children.length === 1)
				? (node.children[0] instanceof Token)
					? new SEMANTICNODE.SemanticNodeConst(node.children[0] as TOKEN.TokenCharCode | TOKEN.TokenString | TOKEN.TokenCharClass)
					: this.decorate(node.children[0])
				: this.decorate(node.children[1])

		} else if (node instanceof PARSENODE.ParseNodeUnary) {
			let operand = this.decorate(node.children[0])
			if (node.children.length === 1) {
				return operand
			}
			operand = new SEMANTICNODE.SemanticNodeOpUn(
				node,
				Decorator.OPS_UN.get(node.children[1].source)!,
				operand,
			)
			if (node.children.length === 2) {
				return operand
			}
			operand = new SEMANTICNODE.SemanticNodeOpUn(
				node,
				'opt',
				operand,
			)
			return operand

		} else if (node instanceof PARSENODE.ParseNodeItem) {
			return (node.children.length === 1)
				? this.decorate(node.children[0])
				: new SEMANTICNODE.SemanticNodeItem(
					node,
					this.decorate(node.children[1]),
					this.decorate(node.children[0]) as unknown as NonemptyArray<SEMANTICNODE.SemanticNodeCondition>,
				)

		} else if (node instanceof PARSENODE.ParseNodeOrder) {
			return (node.children.length === 1)
				? this.decorate(node.children[0])
				: new SEMANTICNODE.SemanticNodeOpBin(
					node,
					'order',
					this.decorate(node.children[0]),
					this.decorate((node.children.length === 2) ? node.children[1] : node.children[2]),
				)

		} else if (
			node instanceof PARSENODE.ParseNodeConcat ||
			node instanceof PARSENODE.ParseNodeAltern
		) {
			return (node.children.length === 1)
				? this.decorate(node.children[0])
				: new SEMANTICNODE.SemanticNodeOpBin(
					node,
					Decorator.OPS_BIN.get(node.children[1].source)!,
					this.decorate(node.children[0]) as SEMANTICNODE.SemanticNodeExpr,
					this.decorate(node.children[2]) as SEMANTICNODE.SemanticNodeExpr,
				)

		} else if (node instanceof PARSENODE.ParseNodeDefinition) {
			return this.decorate(node.children[0])

		} else if (node instanceof PARSENODE.ParseNodeNonterminalName) {
			return (node.children.length === 1)
				? new SEMANTICNODE.SemanticNodeNonterminal(
					node,
					node.children[0] as TOKEN.TokenIdentifier,
				)
				: new SEMANTICNODE.SemanticNodeNonterminal(
					node,
					this.decorate(node.children[0]),
					this.decorate(node.children[1]),
				)

		} else if (node instanceof PARSENODE.ParseNodeProduction) {
			return new SEMANTICNODE.SemanticNodeProduction(
				node,
				this.decorate(node.children[0]),
				this.decorate((node.children.length === 4) ? node.children[2] : node.children[3]),
			)

		} else if (node instanceof PARSENODE.ParseNodeGrammar__0__List) {
			return (node.children.length === 1)
				? [
					this.decorate(node.children[0]),
				]
				: [
					...this.decorate(node.children[0]),
					this.decorate(node.children[1]),
				]

		} else if (node instanceof PARSENODE.ParseNodeGrammar) {
			return new SEMANTICNODE.SemanticNodeGrammar(node, (node.children.length === 2) ? [] : this.decorate(node.children[1]))

		} else {
			throw new ReferenceError(`Could not find type of parse node ${ node }.`)
		}
	}
}
