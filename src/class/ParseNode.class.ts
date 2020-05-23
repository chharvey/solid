import Util from './Util.class'
import type Serializable from '../iface/Serializable.iface'
import Token, {
	Filebound,
	Punctuator,
	Keyword,
	TokenFilebound,
	TokenPunctuator,
	TokenKeyword,
	TokenIdentifier,
	TokenNumber,
	TokenString,
	TokenTemplate,
} from './Token.class'
import SemanticNode, {
	SemanticExpressionType,
	SemanticStatementType,
	SemanticNodeNull,
	SemanticNodeConstant,
	SemanticNodeIdentifier,
	SemanticNodeTemplate,
	SemanticNodeTemplatePartial,
	SemanticNodeExpression,
	SemanticNodeDeclaration,
	SemanticNodeAssignment,
	SemanticNodeAssignee,
	SemanticNodeAssigned,
	SemanticNodeStatementEmpty,
	SemanticNodeStatementExpression,
	SemanticNodeStatementList,
	SemanticNodeGoal,
} from './SemanticNode.class'
import type {Rule} from './Grammar.class'
import Production, {
	ProductionPrimitiveLiteral,
	ProductionStringTemplate,
	ProductionStringTemplate__0__List,
	ProductionExpressionUnit,
	ProductionExpressionUnarySymbol,
	ProductionExpressionExponential,
	ProductionExpressionMultiplicative,
	ProductionExpressionAdditive,
	ProductionExpression,
	ProductionDeclarationVariable,
	ProductionStatementAssignment,
	ProductionStatement,
	ProductionGoal,
} from './Production.class'



/**
 * A ParseNode is a node in a parse tree for a given input stream.
 * It holds:
 * - the group of child inputs ({@link Token}s and/or other ParseNodes)
 * - the line number and column index where the text code of the node starts
 *
 * @see http://parsingintro.sourceforge.net/#contents_item_8.2
 */
export default class ParseNode implements Serializable {
	/**
	 * Construct a speific subtype of ParseNode depending on which production the rule belongs to.
	 *
	 * @param rule     - The Rule used to create this ParseNode.
	 * @param children - The set of child inputs that creates this ParseNode.
	 * @returns          a new ParseNode object
	 */
	static from(rule: Rule, children: readonly (Token|ParseNode)[]): ParseNode {
		return new ([...new Map<Production, typeof ParseNode>([
			[ProductionPrimitiveLiteral         .instance, ParseNodePrimitiveLiteral   ],
			[ProductionStringTemplate           .instance, ParseNodeStringTemplate     ],
			[ProductionStringTemplate__0__List  .instance, ParseNodeStringTemplate__0__List],
			[ProductionExpressionUnit           .instance, ParseNodeExpressionUnit     ],
			[ProductionExpressionUnarySymbol    .instance, ParseNodeExpressionUnary    ],
			[ProductionExpressionExponential    .instance, ParseNodeExpressionBinary   ],
			[ProductionExpressionMultiplicative .instance, ParseNodeExpressionBinary   ],
			[ProductionExpressionAdditive       .instance, ParseNodeExpressionBinary   ],
			[ProductionExpression               .instance, ParseNodeExpression         ],
			[ProductionDeclarationVariable      .instance, ParseNodeDeclarationVariable],
			[ProductionStatementAssignment      .instance, ParseNodeStatementAssignment],
			[ProductionStatement                .instance, ParseNodeStatement          ],
			[ProductionGoal.__0__List           .instance, ParseNodeStatementList      ],
			[ProductionGoal                     .instance, ParseNodeGoal               ],
		])].find(([key]) => rule.production.equals(key)) || [null, ParseNode])[1](rule, children)
	}


	/** The name of the type of this ParseNode. */
	readonly tagname: string = this.rule.production.displayName
	/** The concatenation of the source text of all children. */
	readonly source: string = this.children.map((child) => child.source).join(' ')
	/** The index of the first token in source text. */
	readonly source_index: number = this.children[0].source_index
	/** Zero-based line number of the first token (first line is line 0). */
	readonly line_index: number = this.children[0].line_index
	/** Zero-based column number of the first token (first col is col 0). */
	readonly col_index: number = this.children[0].col_index

	/**
	 * Construct a new ParseNode object.
	 *
	 * @param rule     - The Rule used to create this ParseNode.
	 * @param children - The set of child inputs that creates this ParseNode.
	 */
	protected constructor(
		readonly rule: Rule,
		readonly children: readonly (Token|ParseNode)[],
	) {
	}

	/**
	 * Return a Semantic Node, a node of the Semantic Tree or “decorated/abstract syntax tree”.
	 * @returns a semantic node containing this parse node’s semantics
	 */
	decorate(): SemanticNode {
		return new SemanticNode(this, {'syntactic-name': this.tagname}, this.children.map((c) =>
			(c instanceof ParseNode) ? c.decorate() : new SemanticNode(c, {'syntactic-name': c.tagname})
		))
	}

	/**
	 * @implements Serializable
	 */
	serialize(): string {
		const attributes: Map<string, string> = new Map<string, string>()
		if (!(this instanceof ParseNodeGoal)) {
			attributes.set('line', `${this.line_index + 1}`)
			attributes.set('col' , `${this.col_index  + 1}`)
		}
		attributes.set('source', this.source
			.replace(/\&/g, '&amp;' )
			.replace(/\</g, '&lt;'  )
			.replace(/\>/g, '&gt;'  )
			.replace(/\'/g, '&apos;')
			.replace(/\"/g, '&quot;')
			.replace(/\\/g, '&#x5c;')
			.replace(/\t/g, '&#x09;')
			.replace(/\n/g, '&#x0a;')
			.replace(/\r/g, '&#x0d;')
			.replace(/\u0000/g, '&#x00;')
			.replace(Filebound.SOT, '\u2402') // SYMBOL FOR START OF TEXT
			.replace(Filebound.EOT, '\u2403') // SYMBOL FOR END   OF TEXT
		)
		const contents: string = this.children.map((child) => child.serialize()).join('')
		return `<${this.tagname} ${Util.stringifyAttributes(attributes)}>${contents}</${this.tagname}>`
	}
}



export class ParseNodePrimitiveLiteral extends ParseNode {
	declare children:
		readonly [TokenString|TokenNumber];
	decorate(): SemanticNodeConstant {
		return new SemanticNodeConstant(this.children[0], this.children[0].cook())
	}
}
export class ParseNodeStringTemplate extends ParseNode {
	declare children:
		| readonly [TokenTemplate]
		| readonly [TokenTemplate,                                                        TokenTemplate]
		| readonly [TokenTemplate, ParseNodeExpression,                                   TokenTemplate]
		| readonly [TokenTemplate,                      ParseNodeStringTemplate__0__List, TokenTemplate]
		| readonly [TokenTemplate, ParseNodeExpression, ParseNodeStringTemplate__0__List, TokenTemplate]
	;
	decorate(): SemanticNodeTemplate {
		return new SemanticNodeTemplate(this, (this.children as readonly (TokenTemplate | ParseNodeExpression | ParseNodeStringTemplate__0__List)[]).flatMap((c) => c instanceof Token ? // TODO formatting
			[new SemanticNodeConstant(c, c.cook())]
		: c instanceof ParseNodeStringTemplate__0__List ?
			c.decorate().children
		:
			[c.decorate()]
		))
	}
}
class ParseNodeStringTemplate__0__List extends ParseNode {
	declare children:
		| readonly [                                  TokenTemplate                     ]
		| readonly [                                  TokenTemplate, ParseNodeExpression]
		| readonly [ParseNodeStringTemplate__0__List, TokenTemplate                     ]
		| readonly [ParseNodeStringTemplate__0__List, TokenTemplate, ParseNodeExpression]
	;
	decorate(): SemanticNodeTemplatePartial {
		return new SemanticNodeTemplatePartial(this, (this.children as readonly (TokenTemplate | ParseNodeExpression | ParseNodeStringTemplate__0__List)[]).flatMap((c) =>
			c instanceof Token ? [new SemanticNodeConstant(c, c.cook())] :
			c instanceof ParseNodeExpression ? [c.decorate()] :
			c.decorate().children
		))
	}
}
export class ParseNodeExpressionUnit extends ParseNode {
	declare children:
		[TokenIdentifier] |
		[ParseNodePrimitiveLiteral] |
		[ParseNodeStringTemplate] |
		[TokenPunctuator, ParseNodeExpression, TokenPunctuator];
	decorate(): SemanticExpressionType {
		return (this.children.length === 1) ?
			(this.children[0] instanceof ParseNode) ? this.children[0].decorate() :
				new SemanticNodeIdentifier(this.children[0], this.children[0].cook())
		:
			this.children[1].decorate()
	}
}
export class ParseNodeExpressionUnary extends ParseNode {
	declare children:
		readonly [ParseNodeExpressionUnit] |
		readonly [TokenPunctuator, ParseNodeExpressionUnary];
	decorate(): SemanticExpressionType {
		return (this.children.length === 1) ?
			this.children[0].decorate()
		:
			(this.children[0].source === Punctuator.AFF) ? // `+a` is a no-op
				this.children[1].decorate()
			:
				new SemanticNodeExpression(this, this.children[0].source, [
					this.children[1].decorate(),
				])
	}
}
export class ParseNodeExpressionBinary extends ParseNode {
	declare children:
		readonly [ParseNodeExpressionUnary|ParseNodeExpressionBinary] |
		readonly [ParseNodeExpressionUnary|ParseNodeExpressionBinary, TokenPunctuator, ParseNodeExpressionBinary];
	decorate(): SemanticExpressionType {
		return (this.children.length === 1) ?
			this.children[0].decorate()
		:
			(this.children[1].source === Punctuator.SUB) ? // `a - b` is syntax sugar for `a + -(b)`
				new SemanticNodeExpression(this, Punctuator.ADD, [
					this.children[0].decorate(),
					new SemanticNodeExpression(this.children[2], Punctuator.NEG, [
						this.children[2].decorate(),
					]),
				])
			:
				new SemanticNodeExpression(this, this.children[1].source, [
					this.children[0].decorate(),
					this.children[2].decorate(),
				])
	}
}
export class ParseNodeExpression extends ParseNode {
	declare children:
		readonly [ParseNodeExpressionBinary];
	decorate(): SemanticExpressionType {
		return this.children[0].decorate()
	}
}
export class ParseNodeDeclarationVariable extends ParseNode {
	declare children:
		readonly [TokenKeyword,               TokenIdentifier, TokenPunctuator, ParseNodeExpression, TokenPunctuator] |
		readonly [TokenKeyword, TokenKeyword, TokenIdentifier, TokenPunctuator, ParseNodeExpression, TokenPunctuator];
	decorate(): SemanticNodeDeclaration {
		const is_unfixed: boolean             = this.children[1].source === Keyword.UNFIXED
		const identifier: TokenIdentifier     = this.children[is_unfixed ? 2 : 1] as TokenIdentifier
		const expression: ParseNodeExpression = this.children[is_unfixed ? 4 : 3] as ParseNodeExpression
		return new SemanticNodeDeclaration(this, 'variable', is_unfixed, [
			new SemanticNodeAssignee(identifier, [
				new SemanticNodeIdentifier(identifier, identifier.cook()),
			]),
			new SemanticNodeAssigned(expression, [
				expression.decorate(),
			]),
		])
	}
}
export class ParseNodeStatementAssignment extends ParseNode {
	declare children:
		readonly [TokenIdentifier, TokenPunctuator, ParseNodeExpression, TokenPunctuator];
	decorate(): SemanticNodeAssignment {
		const identifier: TokenIdentifier     = this.children[0]
		const expression: ParseNodeExpression = this.children[2]
		return new SemanticNodeAssignment(this, [
			new SemanticNodeAssignee(identifier, [
				new SemanticNodeIdentifier(identifier, identifier.cook()),
			]),
			new SemanticNodeAssigned(expression, [
				expression.decorate(),
			]),
		])
	}
}
export class ParseNodeStatement extends ParseNode {
	declare children:
		readonly [ParseNodeDeclarationVariable]         |
		readonly [ParseNodeStatementAssignment]         |
		readonly [ParseNodeExpression, TokenPunctuator] |
		readonly [TokenPunctuator];
	decorate(): SemanticStatementType {
		return (this.children.length === 1 && this.children[0] instanceof ParseNode)
			? this.children[0].decorate()
			: (this.children.length === 2)
				? new SemanticNodeStatementExpression(this, [
					this.children[0].decorate(),
				])
				: new SemanticNodeStatementEmpty(this)
	}
}
export class ParseNodeStatementList extends ParseNode {
	declare children:
		readonly [                        ParseNodeStatement] |
		readonly [ParseNodeStatementList, ParseNodeStatement];
	decorate(): SemanticNodeStatementList {
		return new SemanticNodeStatementList(this, this.children.length === 1 ?
			[this.children[0].decorate()]
		: [
			...this.children[0].decorate().children,
			this.children[1].decorate()
		])
	}
}
export class ParseNodeGoal extends ParseNode {
	declare children:
		readonly [TokenFilebound,                         TokenFilebound] |
		readonly [TokenFilebound, ParseNodeStatementList, TokenFilebound];
	decorate(): SemanticNodeNull|SemanticNodeGoal {
		return (this.children.length === 2) ?
			new SemanticNodeNull(this)
		:
			new SemanticNodeGoal(this, [
				this.children[1].decorate()
			])
	}
}
