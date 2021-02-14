import {
	Token,
	TokenFilebound,
	ParseError01,
} from '@chharvey/parser';
import * as assert from 'assert'

import {
	Dev,
	Util,
} from '../../src/core/';
import {
	Punctuator,
	Keyword,
	TOKEN,
	PARSER,
	ParserSolid as Parser,
} from '../../src/parser/';

import {
	assert_arrayLength,
} from '../assert-helpers'
import * as h from '../helpers-parse'



describe('Parser', () => {
	describe('#parse', () => {
		it('throws a ParseError01 when reaching an unexpected token.', () => {
			;[
				`false + /34.56;`,
				`(true)) || null;`,
				`234 null;`,
			].forEach((src) => {
				assert.throws(() => new Parser(src).parse(), ParseError01)
			})
		})

		context('Goal ::= #x02 #x03', () => {
			it('returns only file bounds.', () => {
				const tree: PARSER.ParseNodeGoal = new Parser('').parse()
				assert.strictEqual(tree.children.length, 2)
				tree.children.forEach((child) => assert.ok(child instanceof TokenFilebound))
			})
		})

		context('Statement ::= ";"', () => {
			it('returns a statement with only a punctuator.', () => {
				/*
					<Goal source="␂ ; ␃">
						<FILEBOUND value="true">␂</FILEBOUND>
						<Goal__0__List line="1" col="1" source=";">
							<Statement line="1" col="1" source=";">
								<PUNCTUATOR line="1" col="1" value="7">;</PUNCTUATOR>
							</Statement>
						</Goal__0__List>
						<FILEBOUND value="false">␃</FILEBOUND>
					</Goal>
				*/
				const statement: PARSER.ParseNodeStatement = h.statementFromSource(`;`)
				assert_arrayLength(statement.children, 1)
				const token: PARSER.ParseNodeDeclaration | PARSER.ParseNodeStatementAssignment | Token = statement.children[0];
				assert.ok(token instanceof TOKEN.TokenPunctuator)
				assert.strictEqual(token.source, Punctuator.ENDSTAT)
			})
		})

		Dev.supports('literalCollection') && describe('Word ::= KEYWORD | IDENTIFIER', () => {
			it('makes a Word node.', () => {
				/*
					<Word>
						<KEYWORD>unfixed</KEYWORD> or <IDENTIFIER>foobar</IDENTIFIER>
					</Word>
				*/
				const srcs: string[] = [
					`unfixed`,
					`foobar`,
				];
				assert.deepStrictEqual(srcs.map((src) =>
					h.wordFromString(src).source
				), srcs);
			});
		});

		Dev.supportsAll('typingExplicit', 'literalCollection') && describe('TypeProperty ::= Word ":" Type;', () => {
			it('makes a TypeProperty node.', () => {
				/*
					<TypeProperty>
						<Word source="let">...</Word>
						<PUNCTUATOR>:</PUNCTUATOR>
						<Type source="T">...</Type>
					</TypeProperty>
				*/
				const srcs: Map<string, string> = new Map([
					[`let`,        `str`],
					[`fontWeight`, `int`],
					[`fontStyle`,  `Normal | Italic | Oblique`],
					[`fontSize`,   `float`],
					[`fontFamily`, `str`],
				]);
				assert.deepStrictEqual(
					[...srcs].map(([prop, typ]) => h.typePropertyFromString(`${ prop }: ${ typ }`).children.map((c) => c.source)),
					[...srcs].map(([prop, typ]) => [prop, Punctuator.ISTYPE, typ]),
				);
			});
		});

		Dev.supportsAll('typingExplicit', 'literalCollection') && describe('TypeTupleLiteral ::= "[" ","? Type# ","? "]"', () => {
			/*
				<TypeTupleLiteral>
					<PUNCTUATOR>[</PUNCTUATOR>
					<TypeTupleLiteral__1__List source="T, U | V, W & X!">...</TypeTupleLiteral__1__List>
					<PUNCTUATOR>]</PUNCTUATOR>
				</TypeTupleLiteral>
			*/
			it('with no leading or trailing comma.', () => {
				const tuple: PARSER.ParseNodeTypeTupleLiteral = h.tupleTypeFromString(`[T, U | V, W & X!]`);
				assert_arrayLength(tuple.children, 3);
				assert.deepStrictEqual(
					tuple.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, `T , U | V , W & X !`, Punctuator.BRAK_CLS],
				);
			});
			it('with leading comma.', () => {
				const tuple: PARSER.ParseNodeTypeTupleLiteral = h.tupleTypeFromString(`
					[
						, T
						, U | V
						, W & X!
					]
				`);
				assert_arrayLength(tuple.children, 4);
				assert.deepStrictEqual(
					tuple.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, Punctuator.COMMA, `T , U | V , W & X !`, Punctuator.BRAK_CLS],
				);
			});
			it('with trailing comma.', () => {
				const tuple: PARSER.ParseNodeTypeTupleLiteral = h.tupleTypeFromString(`
					[
						T,
						U | V,
						W & X!,
					]
				`);
				assert_arrayLength(tuple.children, 4);
				assert.deepStrictEqual(
					tuple.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, `T , U | V , W & X !`, Punctuator.COMMA, Punctuator.BRAK_CLS],
				);
			});
			specify('TypeTupleLiteral__1__List ::= TypeTupleLiteral__1__List "," Type', () => {
				/*
					<TypeTupleLiteral__1__List>
						<TypeTupleLiteral__1__List>
							<TypeTupleLiteral__1__List>
								<Type source="T">...</Type>
							</TypeTupleLiteral__1__List>
							<PUNCTUATOR>,</PUNCTUATOR>
							<Type source="U | V">...</Type>
						</TypeTupleLiteral__1__List>
						<PUNCTUATOR>,</PUNCTUATOR>
						<Type source="W & X!">...</Type>
					</TypeTupleLiteral__1__List>
				*/
				const tuple: PARSER.ParseNodeTypeTupleLiteral = h.tupleTypeFromString(`[T, U | V, W & X!]`);
				assert_arrayLength(tuple.children, 3);
				const type_list: PARSER.ParseNodeTypeTupleLiteral__1__List = tuple.children[1];
				h.hashListSources(type_list, `T`, `U | V`, `W & X !`);
			});
		});

		Dev.supportsAll('typingExplicit', 'literalCollection') && describe('TypeRecordLiteral ::= "[" ","? TypeProperty# ","? "]"', () => {
			/*
				<TypeRecordLiteral>
					<PUNCTUATOR>[</PUNCTUATOR>
					<TypeRecordLiteral__1__List source="a: T, b: U | V, c: W & X!">...</TypeRecordLiteral__1__List>
					<PUNCTUATOR>]</PUNCTUATOR>
				</TypeRecordLiteral>
			*/
			it('with no leading or trailing comma.', () => {
				const record: PARSER.ParseNodeTypeRecordLiteral = h.recordTypeFromString(`[a: T, b: U | V, c: W & X!]`);
				assert_arrayLength(record.children, 3);
				assert.deepStrictEqual(
					record.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, `a : T , b : U | V , c : W & X !`, Punctuator.BRAK_CLS],
				);
			});
			it('with leading comma.', () => {
				const record: PARSER.ParseNodeTypeRecordLiteral = h.recordTypeFromString(`
					[
						, a: T
						, b: U | V
						, c: W & X!
					]
				`);
				assert_arrayLength(record.children, 4);
				assert.deepStrictEqual(
					record.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, Punctuator.COMMA, `a : T , b : U | V , c : W & X !`, Punctuator.BRAK_CLS],
				);
			});
			it('with trailing comma.', () => {
				const record: PARSER.ParseNodeTypeRecordLiteral = h.recordTypeFromString(`
					[
						a: T,
						b: U | V,
						c: W & X!,
					]
				`);
				assert_arrayLength(record.children, 4);
				assert.deepStrictEqual(
					record.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, `a : T , b : U | V , c : W & X !`, Punctuator.COMMA, Punctuator.BRAK_CLS],
				);
			});
			specify('TypeRecordLiteral__1__List ::= TypeRecordLiteral__1__List "," TypeProperty', () => {
				/*
					<TypeRecordLiteral__1__List>
						<TypeRecordLiteral__1__List>
							<TypeRecordLiteral__1__List>
								<TypeProperty source="a: T">...</TypeProperty>
							</TypeRecordLiteral__1__List>
							<PUNCTUATOR>,</PUNCTUATOR>
							<TypeProperty source="b: U | V">...</TypeProperty>
						</TypeRecordLiteral__1__List>
						<PUNCTUATOR>,</PUNCTUATOR>
						<TypeProperty source="c: W & X!">...</TypeProperty>
					</TypeRecordLiteral__1__List>
				*/
				const record: PARSER.ParseNodeTypeRecordLiteral = h.recordTypeFromString(`[a: T, b: U | V, c: W & X!]`);
				assert_arrayLength(record.children, 3);
				const property_list: PARSER.ParseNodeTypeRecordLiteral__1__List = record.children[1];
				h.hashListSources(property_list, `a : T`, `b : U | V`, `c : W & X !`);
			});
		});

		Dev.supportsAll('typingExplicit', 'literalCollection') && describe('TypeUnit ::= "[" "]"', () => {
			it('makes a TypeUnit node containing brackets.', () => {
				/*
					<TypeUnit>
						<PUNCTUATOR>[</PUNCTUATOR>
						<PUNCTUATOR>]</PUNCTUATOR>
					</TypeUnit>
				*/
				const type_unit: PARSER.ParseNodeTypeUnit = h.unitTypeFromString(`[]`);
				assert_arrayLength(type_unit.children, 2);
				assert.deepStrictEqual(
					type_unit.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, Punctuator.BRAK_CLS],
				);
			});
		});

		Dev.supports('typingExplicit') && describe('TypeUnit ::= IDENTIFIER', () => {
			it('parses type identifiers.', () => {
				assert.deepStrictEqual([
					`T`,
					`U`,
					`V`,
				].map((src) => h.tokenIdentifierFromTypeString(src).source), [
					`T`,
					`U`,
					`V`,
				]);
			});
		});

		Dev.supports('typingExplicit') && describe('TypeUnit ::= PrimitiveLiteral', () => {
			it('parses NULL, BOOLEAN, INTEGER, FLOAT, or STRING.', () => {
				assert.deepStrictEqual(([
					[`null`,   TOKEN.TokenKeyword],
					[`false`,  TOKEN.TokenKeyword],
					[`true`,   TOKEN.TokenKeyword],
					[`42`,     TOKEN.TokenNumber],
					[`4.2e+1`, TOKEN.TokenNumber],
				] as [string, typeof TOKEN.TokenKeyword | typeof TOKEN.TokenNumber][]).map(([src, tokentype]) => {
					const token: TOKEN.TokenKeyword | TOKEN.TokenNumber | TOKEN.TokenString = h.tokenLiteralFromTypeString(src)
					assert.ok(token instanceof tokentype)
					return token.source
				}), [
					Keyword.NULL,
					Keyword.FALSE,
					Keyword.TRUE,
					'42',
					'4.2e+1',
				]);
			})
		})

		Dev.supports('typingExplicit') && describe('TypeUnit ::= TypeKeyword', () => {
			it('parses keywords `bool`, `int`, `float`, `obj`.', () => {
				assert.deepStrictEqual(([
					`bool`,
					`int`,
					`float`,
					`obj`,
				]).map((src) => h.tokenKeywordFromTypeString(src).source), [
					Keyword.BOOL,
					Keyword.INT,
					Keyword.FLOAT,
					Keyword.OBJ,
				]);
			})
			it('throws when given a non-type keyword.', () => {
				assert.throws(() => h.tokenLiteralFromTypeString(`isnt`), ParseError01)
			})
		})

		Dev.supportsAll('typingExplicit', 'literalCollection') && specify('TypeUnit ::= TypeTupleLiteral', () => {
			h.tupleTypeFromString(`[T, U | V, W & X!]`); // assert does not throw
		});

		Dev.supportsAll('typingExplicit', 'literalCollection') && specify('TypeUnit ::= TypeRecordLiteral', () => {
			h.recordTypeFromString(`[a: T, b: U | V, c: W & X!]`); // assert does not throw
		});

		Dev.supports('typingExplicit') && describe('TypeUnit ::= "(" Type ")"', () => {
			it('makes an TypeUnit node containing a Type node.', () => {
				/*
					<TypeUnit>
						<PUNCTUATOR>(</PUNCTUATOR>
						<Type source="(obj | int) & float">...</Type>
						<PUNCTUATOR>)</PUNCTUATOR>
					</TypeUnit>
				*/
				const type_unit: PARSER.ParseNodeTypeUnit = h.unitTypeFromString(`(obj | int & float)`)
				assert_arrayLength(type_unit.children, 3)
				const [open, typ, close]: readonly [Token, PARSER.ParseNodeType, Token] = type_unit.children
				assert.ok(open  instanceof TOKEN.TokenPunctuator)
				assert.ok(close instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[open.source, typ.source, close.source],
					[Punctuator.GRP_OPN, [
						Keyword.OBJ,
						Punctuator.UNION,
						Keyword.INT,
						Punctuator.INTER,
						Keyword.FLOAT,
					].join(' '), Punctuator.GRP_CLS],
				)
			})
		})

		Dev.supports('typingExplicit') && describe('TypeUnarySymbol ::= TypeUnarySymbol "!"', () => {
			it('makes a ParseNodeTypeUnarySymbol node.', () => {
				/*
					<TypeUnarySymbol>
						<TypeUnarySymbol source="int">...</TypeUnarySymbol>
						<PUNCTUATOR>!</PUNCTUATOR>
					</TypeUnarySymbol>
				*/
				const type_unary: PARSER.ParseNodeTypeUnarySymbol = h.unaryTypeFromString(`int!`)
				assert_arrayLength(type_unary.children, 2)
				const [unary, op]: readonly [PARSER.ParseNodeTypeUnarySymbol, Token] = type_unary.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[unary.source, op.source],
					[Keyword.INT,  Punctuator.ORNULL],
				)
			})
		})

		Dev.supports('typingExplicit') && describe('TypeIntersection ::= TypeIntersection "&" TypeUnarySymbol', () => {
			it('makes a ParseNodeTypeIntersection node.', () => {
				/*
					<TypeIntersection>
						<TypeIntersection source="int">...</TypeIntersection>
						<PUNCTUATOR>&</PUNCTUATOR>
						<TypeUnarySymbol source="float">...</TypeUnarySymbol>
					</TypeIntersection>
				*/
				const type_intersection: PARSER.ParseNodeTypeIntersection = h.intersectionTypeFromString(`int & float`)
				assert_arrayLength(type_intersection.children, 3)
				const [left, op, right]: readonly [PARSER.ParseNodeTypeIntersection, Token, PARSER.ParseNodeTypeUnarySymbol] = type_intersection.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[left.source, op.source,        right.source],
					[Keyword.INT, Punctuator.INTER, Keyword.FLOAT],
				)
			})
		})

		Dev.supports('typingExplicit') && describe('TypeUnion ::= TypeUnion "|" TypeIntersection', () => {
			it('makes a ParseNodeTypeUnion node.', () => {
				/*
					<TypeUnion>
						<TypeUnion source="int">...</TypeUnion>
						<PUNCTUATOR>|</PUNCTUATOR>
						<TypeIntersection source="float">...</TypeIntersection>
					</TypeUnion>
				*/
				const type_union: PARSER.ParseNodeTypeUnion = h.unionTypeFromString(`int | float`)
				assert_arrayLength(type_union.children, 3)
				const [left, op, right]: readonly [PARSER.ParseNodeTypeUnion, Token, PARSER.ParseNodeTypeIntersection] = type_union.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[left.source, op.source,        right.source],
					[Keyword.INT, Punctuator.UNION, Keyword.FLOAT],
				)
			})
		})

		Dev.supports('literalCollection') && describe('Property ::= Word "=" Expression', () => {
			it('makes a Property node.', () => {
				/*
					<Property>
						<Word source="unfixed">...</Word>
						<PUNCTUATOR>=</PUNCTUATOR>
						<Expression source="42">...</Expression>
					</Property>
				*/
				const srcs: string[] = [
					`unfixed`,
					`foobar`,
				];
				assert.deepStrictEqual(
					srcs.map((src) => h.propertyFromString(`${ src } = 42`).children.map((c) => c.source)),
					srcs.map((src) => [src, Punctuator.ASSIGN, `42`]),
				);
			});
		});

		Dev.supports('literalCollection') && describe('Case ::= Expression# "|->" Expression', () => {
			it('makes a Case node.', () => {
				/*
					<Case>
						<Case__0__List>
							<Case__0__List>
								<Expression source="42">...</Expression>
							</Case__0__List>
							<PUNCTUATOR>,</PUNCTUATOR>
							<Expression source="true">...</Expression>
						</Case__0__List>
						<PUNCTUATOR>|-></PUNCTUATOR>
						<Expression source="null || false">...</Expression>
					</Case>
				*/
				const kase: PARSER.ParseNodeCase = h.caseFromString(`42, true |-> null || false`);
				h.hashListSources(kase.children[0], `42`, `true`);
				assert.deepStrictEqual(
					[kase.children[1].source, kase.children[2].source],
					[Punctuator.MAPTO,        `null || false`],
				);
			});
		});

		Dev.supports('literalCollection') && describe('ListLiteral ::= "[" ","? Expression# ","? "]"', () => {
			it('with no leading or trailing comma.', () => {
				/*
					<ListLiteral>
						<PUNCTUATOR>[</PUNCTUATOR>
						<Case__0__List source="42, true, null || false">...</Case__0__List>
						<PUNCTUATOR>]</PUNCTUATOR>
					</ListLiteral>
				*/
				const unit: PARSER.ParseNodeListLiteral = h.listLiteralFromSource(`[42, true, null || false];`);
				assert_arrayLength(unit.children, 3);
				assert.deepStrictEqual(
					unit.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, `42 , true , null || false`, Punctuator.BRAK_CLS],
				);
			});
			it('with leading comma.', () => {
				const unit: PARSER.ParseNodeListLiteral = h.listLiteralFromSource(`
					[
						, 42
						, true
						, null || false
					];
				`);
				assert_arrayLength(unit.children, 4);
				assert.deepStrictEqual(
					unit.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, Punctuator.COMMA, `42 , true , null || false`, Punctuator.BRAK_CLS],
				);
			});
			it('with trailing comma.', () => {
				const unit: PARSER.ParseNodeListLiteral = h.listLiteralFromSource(`
					[
						42,
						true,
						null || false,
					];
				`);
				assert_arrayLength(unit.children, 4);
				assert.deepStrictEqual(
					unit.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, `42 , true , null || false`, Punctuator.COMMA, Punctuator.BRAK_CLS],
				);
			});
			specify('Case__0__List ::= Case__0__List "," Expression', () => {
				/*
					<Case__0__List>
						<Case__0__List>
							<Case__0__List>
								<Expression source="42">...</Expression>
							</Case__0__List>
							<PUNCTUATOR>,</PUNCTUATOR>
							<Expression source="true">...</Expression>
						</Case__0__List>
						<PUNCTUATOR>,</PUNCTUATOR>
						<Expression source="null || false">...</Expression>
					</Case__0__List>
				*/
				const unit: PARSER.ParseNodeListLiteral = h.listLiteralFromSource(`[42, true, null || false];`);
				assert_arrayLength(unit.children, 3);
				h.hashListSources(unit.children[1], `42`, `true`, `null || false`);
			});
		});

		Dev.supports('literalCollection') && describe('RecordLiteral ::= "[" ","? Property# ","? "]"', () => {
			it('with leading comma.', () => {
				/*
					<RecordLiteral>
						<PUNCTUATOR>[</PUNCTUATOR>
						<PUNCTUATOR>,</PUNCTUATOR>
						<RecordLiteral__1__List source="let = true, foobar = 42">...</RecordLiteral__1__List>
						<PUNCTUATOR>]</PUNCTUATOR>
					</RecordLiteral>
				*/
				const unit: PARSER.ParseNodeRecordLiteral = h.recordLiteralFromSource(`
					[
						, let = true
						, foobar = 42
					];
				`);
				assert_arrayLength(unit.children, 4);
				assert.ok(unit.children[2] instanceof PARSER.ParseNodeRecordLiteral__1__List);
				assert.deepStrictEqual(
					unit.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, Punctuator.COMMA, `let = true , foobar = 42`, Punctuator.BRAK_CLS],
				);
			});
			specify('RecordLiteral__1__List ::= RecordLiteral__1__List "," Property', () => {
				/*
					<RecordLiteral__1__List>
						<RecordLiteral__1__List>
							<Property source="let = true">...</Property>
						</RecordLiteral__1__List>
						<PUNCTUATOR>,</PUNCTUATOR>
						<Property source="foobar = 42">...</Property>
					</RecordLiteral__1__List>
				*/
				const unit: PARSER.ParseNodeRecordLiteral = h.recordLiteralFromSource(`[let = true, foobar = 42];`);
				assert_arrayLength(unit.children, 3);
				h.hashListSources(unit.children[1], `let = true`, `foobar = 42`);
			});
		});

		Dev.supports('literalCollection') && describe('MappingLiteral ::= "[" ","? Case# ","? "]"', () => {
			it('with trailing comma.', () => {
				/*
					<MappingLiteral>
						<PUNCTUATOR>[</PUNCTUATOR>
						<MappingLiteral__1__List source="1, 2, 3 |-> null, 4, 5, 6 |-> false, 7, 8 |-> true, 9, 0 |-> 42.0">...</MappingLiteral__1__List>
						<PUNCTUATOR>,</PUNCTUATOR>
						<PUNCTUATOR>]</PUNCTUATOR>
					</MappingLiteral>
				*/
				const unit: PARSER.ParseNodeMappingLiteral = h.mappingLiteralFromSource(`
					[
						1, 2, 3 |-> null,
						4, 5, 6 |-> false,
						7, 8    |-> true,
						9, 0    |-> 42.0,
					];
				`);
				assert_arrayLength(unit.children, 4);
				assert.ok(unit.children[1] instanceof PARSER.ParseNodeMappingLiteral__1__List);
				assert.deepStrictEqual(
					unit.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, `1 , 2 , 3 |-> null , 4 , 5 , 6 |-> false , 7 , 8 |-> true , 9 , 0 |-> 42.0`, Punctuator.COMMA, Punctuator.BRAK_CLS],
				);
			});
			specify('MappingLiteral__1__List ::= MappingLiteral__1__List "," Case', () => {
				/*
					<MappingLiteral__1__List>
						<MappingLiteral__1__List>
							<MappingLiteral__1__List>
								<MappingLiteral__1__List>
									<Case source="1, 2, 3 |-> null">...</Case>
								</MappingLiteral__1__List>
								<PUNCTUATOR>,</PUNCTUATOR>
								<Case source="4, 5, 6 |-> false">...</Case>
							</MappingLiteral__1__List>
							<PUNCTUATOR>,</PUNCTUATOR>
							<Case source="7, 8 |-> true">...</Case>
						</MappingLiteral__1__List>
						<PUNCTUATOR>,</PUNCTUATOR>
						<Case source="9, 0 |-> 42.0">...</Case>
					</MappingLiteral__1__List>
				*/
				const unit: PARSER.ParseNodeMappingLiteral = h.mappingLiteralFromSource(`[1, 2, 3 |-> null, 4, 5, 6 |-> false, 7, 8 |-> true, 9, 0 |-> 42.0];`);
				assert_arrayLength(unit.children, 3);
				h.hashListSources(
					unit.children[1],
					`1 , 2 , 3 |-> null`,
					`4 , 5 , 6 |-> false`,
					`7 , 8 |-> true`,
					`9 , 0 |-> 42.0`,
				);
			});
		});

		Dev.supports('literalCollection') && describe('ExpressionUnit ::= "[" "]"', () => {
			it('makes an ExpressionUnit node containing brackets.', () => {
				/*
					<ExpressionUnit>
						<PUNCTUATOR>[</PUNCTUATOR>
						<PUNCTUATOR>]</PUNCTUATOR>
					</ExpressionUnit>
				*/
				const expression_unit: PARSER.ParseNodeExpressionUnit = h.unitExpressionFromSource(`[];`);
				assert_arrayLength(expression_unit.children, 2);
				assert.deepStrictEqual(
					expression_unit.children.map((c) => c.source),
					[Punctuator.BRAK_OPN, Punctuator.BRAK_CLS],
				);
			});
		});

		context('ExpressionUnit ::= PrimitiveLiteral', () => {
			Dev.supports('variables') && it('parses IDENTIFIER.', () => {
				assert.strictEqual(h.tokenIdentifierFromSource(`ident;`).source, 'ident')
			})
			it('parses NULL, BOOLEAN, INTEGER, FLOAT, or STRING.', () => {
				assert.deepStrictEqual(([
					[`null;`,   TOKEN.TokenKeyword],
					[`false;`,  TOKEN.TokenKeyword],
					[`true;`,   TOKEN.TokenKeyword],
					[`42;`,     TOKEN.TokenNumber],
					[`4.2e+1;`, TOKEN.TokenNumber],
				] as [string, typeof TOKEN.TokenKeyword | typeof TOKEN.TokenNumber][]).map(([src, tokentype]) => {
					const token: TOKEN.TokenKeyword | TOKEN.TokenNumber | TOKEN.TokenString = h.tokenLiteralFromSource(src)
					assert.ok(token instanceof tokentype)
					return token.source
				}), [
					Keyword.NULL,
					Keyword.FALSE,
					Keyword.TRUE,
					'42',
					'4.2e+1',
				]);
			})
		})

		Dev.supports('literalTemplate') && context('ExpressionUnit ::= StringTemplate', () => {
			function stringTemplateParseNode (src: string): string {
				return (((((((((((((new Parser(src)
					.parse()
					.children[1] as PARSER.ParseNodeGoal__0__List)
					.children[0] as PARSER.ParseNodeStatement)
					.children[0] as PARSER.ParseNodeExpression)
					.children[0] as PARSER.ParseNodeExpressionDisjunctive)
					.children[0] as PARSER.ParseNodeExpressionConjunctive)
					.children[0] as PARSER.ParseNodeExpressionEquality)
					.children[0] as PARSER.ParseNodeExpressionComparative)
					.children[0] as PARSER.ParseNodeExpressionAdditive)
					.children[0] as PARSER.ParseNodeExpressionMultiplicative)
					.children[0] as PARSER.ParseNodeExpressionExponential)
					.children[0] as PARSER.ParseNodeExpressionUnarySymbol)
					.children[0] as PARSER.ParseNodeExpressionUnit)
					.children[0] as PARSER.ParseNodeStringTemplate)
					.serialize()
			}
			specify('head, tail.', () => {
				assert.strictEqual(stringTemplateParseNode(Util.dedent(`
					'''head1{{}}tail1''';
				`)), `
					<StringTemplate line="1" col="1" source="&apos;&apos;&apos;head1{{ }}tail1&apos;&apos;&apos;">
						<TEMPLATE line="1" col="1" value="head1">'''head1{{</TEMPLATE>
						<TEMPLATE line="1" col="11" value="tail1">}}tail1'''</TEMPLATE>
					</StringTemplate>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, tail.', () => {
				assert.strictEqual(stringTemplateParseNode(Util.dedent(`
					'''head1{{ '''full1''' }}tail1''';
				`)), `
					<StringTemplate line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}tail1&apos;&apos;&apos;">
						<TEMPLATE line="1" col="1" value="head1">'''head1{{</TEMPLATE>
						<Expression line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<ExpressionAdditive line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
								<ExpressionMultiplicative line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
									<ExpressionExponential line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
										<ExpressionUnarySymbol line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
											<ExpressionUnit line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
												<StringTemplate line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
													<TEMPLATE line="1" col="12" value="full1">'''full1'''</TEMPLATE>
												</StringTemplate>
											</ExpressionUnit>
										</ExpressionUnarySymbol>
									</ExpressionExponential>
								</ExpressionMultiplicative>
							</ExpressionAdditive>
						</Expression>
						<TEMPLATE line="1" col="24" value="tail1">}}tail1'''</TEMPLATE>
					</StringTemplate>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, middle, tail.', () => {
				assert.strictEqual(stringTemplateParseNode(Util.dedent(`
					'''head1{{ '''full1''' }}midd1{{}}tail1''';
				`)), `
					<StringTemplate line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}midd1{{ }}tail1&apos;&apos;&apos;">
						<TEMPLATE line="1" col="1" value="head1">'''head1{{</TEMPLATE>
						<Expression line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<ExpressionAdditive line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
								<ExpressionMultiplicative line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
									<ExpressionExponential line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
										<ExpressionUnarySymbol line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
											<ExpressionUnit line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
												<StringTemplate line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
													<TEMPLATE line="1" col="12" value="full1">'''full1'''</TEMPLATE>
												</StringTemplate>
											</ExpressionUnit>
										</ExpressionUnarySymbol>
									</ExpressionExponential>
								</ExpressionMultiplicative>
							</ExpressionAdditive>
						</Expression>
						<StringTemplate__0__List line="1" col="24" source="}}midd1{{">
							<TEMPLATE line="1" col="24" value="midd1">}}midd1{{</TEMPLATE>
						</StringTemplate__0__List>
						<TEMPLATE line="1" col="33" value="tail1">}}tail1'''</TEMPLATE>
					</StringTemplate>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, middle, expr, tail.', () => {
				assert.strictEqual(stringTemplateParseNode(Util.dedent(`
					'''head1{{ '''full1''' }}midd1{{ '''full2''' }}tail1''';
				`)), `
					<StringTemplate line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos; }}tail1&apos;&apos;&apos;">
						<TEMPLATE line="1" col="1" value="head1">'''head1{{</TEMPLATE>
						<Expression line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<ExpressionAdditive line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
								<ExpressionMultiplicative line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
									<ExpressionExponential line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
										<ExpressionUnarySymbol line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
											<ExpressionUnit line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
												<StringTemplate line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
													<TEMPLATE line="1" col="12" value="full1">'''full1'''</TEMPLATE>
												</StringTemplate>
											</ExpressionUnit>
										</ExpressionUnarySymbol>
									</ExpressionExponential>
								</ExpressionMultiplicative>
							</ExpressionAdditive>
						</Expression>
						<StringTemplate__0__List line="1" col="24" source="}}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos;">
							<TEMPLATE line="1" col="24" value="midd1">}}midd1{{</TEMPLATE>
							<Expression line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
								<ExpressionAdditive line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
									<ExpressionMultiplicative line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
										<ExpressionExponential line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
											<ExpressionUnarySymbol line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
												<ExpressionUnit line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
													<StringTemplate line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
														<TEMPLATE line="1" col="34" value="full2">'''full2'''</TEMPLATE>
													</StringTemplate>
												</ExpressionUnit>
											</ExpressionUnarySymbol>
										</ExpressionExponential>
									</ExpressionMultiplicative>
								</ExpressionAdditive>
							</Expression>
						</StringTemplate__0__List>
						<TEMPLATE line="1" col="46" value="tail1">}}tail1'''</TEMPLATE>
					</StringTemplate>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, middle, expr, middle, tail.', () => {
				assert.strictEqual(stringTemplateParseNode(Util.dedent(`
					'''head1{{ '''full1''' }}midd1{{ '''full2''' }}midd2{{}}tail1''';
				`)), `
					<StringTemplate line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos; }}midd2{{ }}tail1&apos;&apos;&apos;">
						<TEMPLATE line="1" col="1" value="head1">'''head1{{</TEMPLATE>
						<Expression line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<ExpressionAdditive line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
								<ExpressionMultiplicative line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
									<ExpressionExponential line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
										<ExpressionUnarySymbol line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
											<ExpressionUnit line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
												<StringTemplate line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
													<TEMPLATE line="1" col="12" value="full1">'''full1'''</TEMPLATE>
												</StringTemplate>
											</ExpressionUnit>
										</ExpressionUnarySymbol>
									</ExpressionExponential>
								</ExpressionMultiplicative>
							</ExpressionAdditive>
						</Expression>
						<StringTemplate__0__List line="1" col="24" source="}}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos; }}midd2{{">
							<StringTemplate__0__List line="1" col="24" source="}}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos;">
								<TEMPLATE line="1" col="24" value="midd1">}}midd1{{</TEMPLATE>
								<Expression line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
									<ExpressionAdditive line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
										<ExpressionMultiplicative line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
											<ExpressionExponential line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
												<ExpressionUnarySymbol line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
													<ExpressionUnit line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
														<StringTemplate line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
															<TEMPLATE line="1" col="34" value="full2">'''full2'''</TEMPLATE>
														</StringTemplate>
													</ExpressionUnit>
												</ExpressionUnarySymbol>
											</ExpressionExponential>
										</ExpressionMultiplicative>
									</ExpressionAdditive>
								</Expression>
							</StringTemplate__0__List>
							<TEMPLATE line="1" col="46" value="midd2">}}midd2{{</TEMPLATE>
						</StringTemplate__0__List>
						<TEMPLATE line="1" col="55" value="tail1">}}tail1'''</TEMPLATE>
					</StringTemplate>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, middle, expr, middle, expr, tail.', () => {
				assert.strictEqual(stringTemplateParseNode(Util.dedent(`
					'''head1{{ '''full1''' }}midd1{{ '''full2''' }}midd2{{ '''head2{{ '''full3''' }}tail2''' }}tail1''';
				`)), `
					<StringTemplate line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos; }}midd2{{ &apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos; }}tail1&apos;&apos;&apos;">
						<TEMPLATE line="1" col="1" value="head1">'''head1{{</TEMPLATE>
						<Expression line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<ExpressionAdditive line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
								<ExpressionMultiplicative line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
									<ExpressionExponential line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
										<ExpressionUnarySymbol line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
											<ExpressionUnit line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
												<StringTemplate line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
													<TEMPLATE line="1" col="12" value="full1">'''full1'''</TEMPLATE>
												</StringTemplate>
											</ExpressionUnit>
										</ExpressionUnarySymbol>
									</ExpressionExponential>
								</ExpressionMultiplicative>
							</ExpressionAdditive>
						</Expression>
						<StringTemplate__0__List line="1" col="24" source="}}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos; }}midd2{{ &apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos;">
							<StringTemplate__0__List line="1" col="24" source="}}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos;">
								<TEMPLATE line="1" col="24" value="midd1">}}midd1{{</TEMPLATE>
								<Expression line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
									<ExpressionAdditive line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
										<ExpressionMultiplicative line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
											<ExpressionExponential line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
												<ExpressionUnarySymbol line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
													<ExpressionUnit line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
														<StringTemplate line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
															<TEMPLATE line="1" col="34" value="full2">'''full2'''</TEMPLATE>
														</StringTemplate>
													</ExpressionUnit>
												</ExpressionUnarySymbol>
											</ExpressionExponential>
										</ExpressionMultiplicative>
									</ExpressionAdditive>
								</Expression>
							</StringTemplate__0__List>
							<TEMPLATE line="1" col="46" value="midd2">}}midd2{{</TEMPLATE>
							<Expression line="1" col="56" source="&apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos;">
								<ExpressionAdditive line="1" col="56" source="&apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos;">
									<ExpressionMultiplicative line="1" col="56" source="&apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos;">
										<ExpressionExponential line="1" col="56" source="&apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos;">
											<ExpressionUnarySymbol line="1" col="56" source="&apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos;">
												<ExpressionUnit line="1" col="56" source="&apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos;">
													<StringTemplate line="1" col="56" source="&apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos;">
														<TEMPLATE line="1" col="56" value="head2">'''head2{{</TEMPLATE>
														<Expression line="1" col="67" source="&apos;&apos;&apos;full3&apos;&apos;&apos;">
															<ExpressionAdditive line="1" col="67" source="&apos;&apos;&apos;full3&apos;&apos;&apos;">
																<ExpressionMultiplicative line="1" col="67" source="&apos;&apos;&apos;full3&apos;&apos;&apos;">
																	<ExpressionExponential line="1" col="67" source="&apos;&apos;&apos;full3&apos;&apos;&apos;">
																		<ExpressionUnarySymbol line="1" col="67" source="&apos;&apos;&apos;full3&apos;&apos;&apos;">
																			<ExpressionUnit line="1" col="67" source="&apos;&apos;&apos;full3&apos;&apos;&apos;">
																				<StringTemplate line="1" col="67" source="&apos;&apos;&apos;full3&apos;&apos;&apos;">
																					<TEMPLATE line="1" col="67" value="full3">'''full3'''</TEMPLATE>
																				</StringTemplate>
																			</ExpressionUnit>
																		</ExpressionUnarySymbol>
																	</ExpressionExponential>
																</ExpressionMultiplicative>
															</ExpressionAdditive>
														</Expression>
														<TEMPLATE line="1" col="79" value="tail2">}}tail2'''</TEMPLATE>
													</StringTemplate>
												</ExpressionUnit>
											</ExpressionUnarySymbol>
										</ExpressionExponential>
									</ExpressionMultiplicative>
								</ExpressionAdditive>
							</Expression>
						</StringTemplate__0__List>
						<TEMPLATE line="1" col="90" value="tail1">}}tail1'''</TEMPLATE>
					</StringTemplate>
				`.replace(/\n\t*/g, ''))
			})
			it('throws when reaching an orphaned head.', () => {
				assert.throws(() => new Parser(`
					'''A string template head token not followed by a middle or tail {{ 1;
				`).parse(), ParseError01)
			})
			it('throws when reaching an orphaned middle.', () => {
				assert.throws(() => new Parser(`
					2 }} a string template middle token not preceded by a head/middle and not followed by a middle/tail {{ 3;
				`).parse(), ParseError01)
			})
			it('throws when reaching an orphaned tail.', () => {
				assert.throws(() => new Parser(`
					4 }} a string template tail token not preceded by a head or middle''';
				`).parse(), ParseError01)
			})
		})

		Dev.supports('literalCollection') && specify('ExpressionUnit ::= ListLiteral', () => {
			h.listLiteralFromSource(`[, 42, true, null || false,];`); // assert does not throw
		});

		Dev.supports('literalCollection') && specify('ExpressionUnit ::= RecordLiteral', () => {
			h.recordLiteralFromSource(`
				[
					, let = true
					, foobar = 42
					,
				];
			`); // assert does not throw
		});

		Dev.supports('literalCollection') && specify('ExpressionUnit ::= MappingLiteral', () => {
			h.mappingLiteralFromSource(`
				[
					,
					1, 2, 3 |-> null,
					4, 5, 6 |-> false,
					7, 8    |-> true,
					9, 0    |-> 42.0,
				];
			`); // assert does not throw
		});

		context('ExpressionUnit ::= "(" Expression ")"', () => {
			it('makes an ExpressionUnit node containing an Expression node.', () => {
				/*
					<ExpressionUnit>
						<PUNCTUATOR>(</PUNCTUATOR>
						<Expression source="2 + -3">...</Expression>
						<PUNCTUATOR>)</PUNCTUATOR>
					</ExpressionUnit>
				*/
				const expression_unit: PARSER.ParseNodeExpressionUnit = h.unitExpressionFromSource(`(2 + -3);`)
				assert_arrayLength(expression_unit.children, 3)
				const [open, expr, close]: readonly [Token, PARSER.ParseNodeExpression, Token] = expression_unit.children
				assert.ok(open  instanceof TOKEN.TokenPunctuator)
				assert.ok(close instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[open.source,        expr.source, close.source],
					[Punctuator.GRP_OPN, `2 + -3`,    Punctuator.GRP_CLS],
				)
			})
		})

		context('ExpressionUnarySymbol ::= ("!" | "?" | "+" | "-") ExpressionUnarySymbol', () => {
			it('makes a ParseNodeExpressionUnarySymbol node.', () => {
				/*
					<ExpressionUnarySymbol>
						<PUNCTUATOR>-</PUNCTUATOR>
						<ExpressionUnarySymbol source="42">...</ExpressionUnarySymbol>
					</ExpressionUnarySymbol>
				*/
				assert.deepStrictEqual([
					`!false;`,
					`?true;`,
					`- 42;`,
					`--2;`,
				].map((src) => {
					const expression_unary: PARSER.ParseNodeExpressionUnarySymbol = h.unaryExpressionFromSource(src)
					assert_arrayLength(expression_unary.children, 2, 'outer unary expression should have 2 children')
					const [op, operand]: readonly [Token, PARSER.ParseNodeExpressionUnarySymbol] = expression_unary.children
					assert.ok(op instanceof TOKEN.TokenPunctuator)
					assert_arrayLength(operand.children, 1, 'inner unary expression should have 1 child')
					return [operand.source, op.source]
				}), [
					[`false`, Punctuator.NOT],
					[`true`,  Punctuator.EMP],
					[`42`,    Punctuator.NEG],
					[`-2`,    Punctuator.NEG],
				])
			})
		})

		context('ExpressionExponential ::=  ExpressionUnarySymbol "^" ExpressionExponential', () => {
			it('makes a ParseNodeExpressionExponential node.', () => {
				/*
					<ExpressionExponential>
						<ExpressionUnarySymbol source="2">...</ExpressionUnarySymbol>
						<PUNCTUATOR>^</PUNCTUATOR>
						<ExpressionExponential source="-3">...</ExpressionExponential>
					</ExpressionExponential>
				*/
				const expression_exp: PARSER.ParseNodeExpressionExponential = h.exponentialExpressionFromSource(`2 ^ -3;`)
				assert_arrayLength(expression_exp.children, 3, 'exponential expression should have 3 children')
				const [left, op, right]: readonly [PARSER.ParseNodeExpressionUnarySymbol, Token, PARSER.ParseNodeExpressionExponential] = expression_exp.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[left.source, op.source,      right.source],
					['2',         Punctuator.EXP, '-3'],
				)
			})
		})

		context('ExpressionMultiplicative ::= ExpressionMultiplicative ("*" | "/") ExpressionExponential', () => {
			it('makes a ParseNodeExpressionMultiplicative node.', () => {
				/*
					<ExpressionMultiplicative>
						<ExpressionMultiplicative source="2">...</ExpressionMultiplicative>
						<PUNCTUATOR>*</PUNCTUATOR>
						<ExpressionExponential source="-3">...</ExpressionExponential>
					</ExpressionMultiplicative>
				*/
				const expression_mul: PARSER.ParseNodeExpressionMultiplicative = h.multiplicativeExpressionFromSource(`2 * -3;`)
				assert_arrayLength(expression_mul.children, 3, 'multiplicative expression should have 3 children')
				const [left, op, right]: readonly [PARSER.ParseNodeExpressionMultiplicative, Token, PARSER.ParseNodeExpressionExponential] = expression_mul.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[left.source, op.source,      right.source],
					['2',         Punctuator.MUL, '-3'],
				)
			})
		})

		context('ExpressionAdditive ::= ExpressionAdditive ("+" | "-") ExpressionMultiplicative', () => {
			it('makes a ParseNodeExpressionAdditive node.', () => {
				/*
					<ExpressionAdditive>
						<ExpressionAdditive source="2">...</ExpressionAdditive>
						<PUNCTUATOR>+</PUNCTUATOR>
						<ExpressionMultiplicative source="-3">...</ExpressionMultiplicative>
					</ExpressionAdditive>
				*/
				const expression_add: PARSER.ParseNodeExpressionAdditive = h.additiveExpressionFromSource(`2 + -3;`)
				assert_arrayLength(expression_add.children, 3, 'additive expression should have 3 children')
				const [left, op, right]: readonly [PARSER.ParseNodeExpressionAdditive, Token, PARSER.ParseNodeExpressionMultiplicative] = expression_add.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[left.source, op.source,      right.source],
					['2',         Punctuator.ADD, '-3'],
				)
			})
		})

		context('ExpressionComparative ::= ExpressionComparative ("<" | ">" | "<=" | ">=" | "!<" | "!>") ExpressionAdditive', () => {
			it('makes a ParseNodeExpressionComparative node.', () => {
				/*
					<ExpressionComparative>
						<ExpressionComparative source="2">...</ExpressionComparative>
						<PUNCTUATOR>&lt;</PUNCTUATOR>
						<ExpressionAdditive source="-3">...</ExpressionAdditive>
					</ExpressionComparative>
				*/
				const expression_compare: PARSER.ParseNodeExpressionComparative = h.comparativeExpressionFromSource(`2 < -3;`)
				assert_arrayLength(expression_compare.children, 3, 'comparative expression should have 3 children')
				const [left, op, right]: readonly [PARSER.ParseNodeExpressionComparative, Token, PARSER.ParseNodeExpressionAdditive] = expression_compare.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[left.source, op.source,     right.source],
					['2',         Punctuator.LT, '-3'],
				)
			})
			it('allows chaining of `<` and `>`.', () => {
				const expression_compare: PARSER.ParseNodeExpressionComparative = h.comparativeExpressionFromSource(`2 < 3 > 4;`)
				assert_arrayLength(expression_compare.children, 3, 'comparative expression should have 3 children')
				const [left, op, right]: readonly [PARSER.ParseNodeExpressionComparative, Token, PARSER.ParseNodeExpressionAdditive] = expression_compare.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[left.source, op.source,     right.source],
					['2 < 3',     Punctuator.GT, '4'],
				)
			})
		})

		context('ExpressionEquality ::= ExpressionEquality ("is" | "isnt" | "==" | "!=") ExpressionComparative', () => {
			it('makes a ParseNodeExpressionEquality node.', () => {
				/*
					<ExpressionEquality>
						<ExpressionEquality source="2">...</ExpressionEquality>
						<PUNCTUATOR>is</PUNCTUATOR>
						<ExpressionComparative source="-3">...</ExpressionComparative>
					</ExpressionEquality>
				*/
				assert.deepStrictEqual([
					`2 is -3;`,
					`2 == -3;`,
				].map((src, i) => {
					const expression_eq: PARSER.ParseNodeExpressionEquality = h.equalityExpressionFromSource(src)
					assert_arrayLength(expression_eq.children, 3, 'equality expression should have 3 children')
					const [left, op, right]: readonly [PARSER.ParseNodeExpressionEquality, Token, PARSER.ParseNodeExpressionComparative] = expression_eq.children
					assert.ok(op instanceof [TOKEN.TokenKeyword, TOKEN.TokenPunctuator][i])
					return [left.source, op.source, right.source]
				}), [
					['2', Keyword.IS,    '-3'],
					['2', Punctuator.EQ, '-3'],
				])
			})
		})

		context('ExpressionConjunctive ::= ExpressionConjunctive ("&&" | "!&") ExpressionEquality', () => {
			it('makes a ParseNodeExpressionConjunctive node.', () => {
				/*
					<ExpressionConjunctive>
						<ExpressionConjunctive source="2">...</ExpressionConjunctive>
						<PUNCTUATOR>&&</PUNCTUATOR>
						<ExpressionAdditive source="-3">...</ExpressionAdditive>
					</ExpressionConjunctive>
				*/
				const expression_conj: PARSER.ParseNodeExpressionConjunctive = h.conjunctiveExpressionFromSource(`2 && -3;`)
				assert_arrayLength(expression_conj.children, 3, 'conjunctive expression should have 3 children')
				const [left, op, right]: readonly [PARSER.ParseNodeExpressionConjunctive, Token, PARSER.ParseNodeExpressionEquality] = expression_conj.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[left.source, op.source,      right.source],
					['2',         Punctuator.AND, '-3'],
				)
			})
		})

		context('ExpressionDisjunctive ::= ExpressionDisjunctive ("||" | "!|") ExpressionConjunctive', () => {
			it('makes a ParseNodeExpressionDisjunctive node.', () => {
				/*
					<ExpressionDisjunctive>
						<ExpressionDisjunctive source="2">...</ExpressionDisjunctive>
						<PUNCTUATOR>||</PUNCTUATOR>
						<ExpressionConjunctive source="-3">...</ExpressionConjunctive>
					</ExpressionDisjunctive>
				*/
				const expression_disj: PARSER.ParseNodeExpressionDisjunctive = h.disjunctiveExpressionFromSource(`2 || -3;`)
				assert_arrayLength(expression_disj.children, 3, 'disjunctive expression should have 3 children')
				const [left, op, right]: readonly [PARSER.ParseNodeExpressionDisjunctive, Token, PARSER.ParseNodeExpressionConjunctive] = expression_disj.children
				assert.ok(op instanceof TOKEN.TokenPunctuator)
				assert.deepStrictEqual(
					[left.source, op.source,     right.source],
					['2',         Punctuator.OR, '-3'],
				)
			})
		})

		context('ExpressionConditional ::= "if" Expression "then" Expression "else" Expression', () => {
			it('makes a ParseNodeExpressionConditional node.', () => {
				/*
					<ExpressionConditional>
						<KEYWORD>if</KEYWORD>
						<Expression source="true">...</Expression>
						<KEYWORD>then</KEYWORD>
						<Expression source="2">...</Expression>
						<KEYWORD>else</KEYWORD>
						<Expression source="3">...</Expression>
					</ExpressionConditional>
				*/
				const expression_cond: PARSER.ParseNodeExpressionConditional = h.conditionalExpressionFromSource(`
					if true then 2 else 3;
				`)
				const
					[_if,   condition,                  _then, consequent,                 _else, alternative]: readonly
					[Token, PARSER.ParseNodeExpression, Token, PARSER.ParseNodeExpression, Token, PARSER.ParseNodeExpression] = expression_cond.children
				assert.ok(_if   instanceof TOKEN.TokenKeyword)
				assert.ok(_then instanceof TOKEN.TokenKeyword)
				assert.ok(_else instanceof TOKEN.TokenKeyword)
				assert.deepStrictEqual(
					[_if.source, condition.source, _then.source, consequent.source, _else.source, alternative.source],
					[Keyword.IF, `true`,           Keyword.THEN, `2`,               Keyword.ELSE, `3`],
				)
			})
		})

		Dev.supportsAll('variables', 'typingExplicit') && describe('DeclarationVariable', () => {
			/*
				<Statement>
					<DeclarationVariable>
						<KEYWORD>let</KEYWORD>
						<KEYWORD>unfixed</KEYWORD>
						<IDENTIFIER>the_answer</IDENTIFIER>
						<PUNCTUATOR>:</PUNCTUATOR>
						<Type source="int | float">...</Type>
						<PUNCTUATOR>=</PUNCTUATOR>
						<Expression source="21 * 2">...</Expression>
						<PUNCTUATOR>;</PUNCTUATOR>
					</DeclarationVariable>
				</Statement>
			*/
			it('makes a ParseNodeDeclarationVariable node with 7 children (not unfixed).', () => {
				const decl: PARSER.ParseNodeDeclarationVariable = h.variableDeclarationFromSource(`
					let  the_answer:  int | float =  21  *  2;
				`)
				assert_arrayLength(decl.children, 7)
				assert.deepStrictEqual(decl.children.map((child) => child.source), [
					'let', 'the_answer', ':', 'int | float', '=', '21 * 2', ';',
				])
			})
			it('makes a ParseNodeDeclarationVariable node with 8 children (unfixed).', () => {
				const decl: PARSER.ParseNodeDeclarationVariable = h.variableDeclarationFromSource(`
					let  unfixed  the_answer:  int!  =  21  *  2;
				`)
				assert_arrayLength(decl.children, 8)
				assert.deepStrictEqual(decl.children.map((child) => child.source), [
					'let', 'unfixed', 'the_answer', ':', 'int !', '=', '21 * 2', ';',
				])
			})
		})

		Dev.supportsAll('variables', 'typingExplicit') && describe('DeclarationType', () => {
			/*
				<Statement>
					<DeclarationType>
						<KEYWORD>type</KEYWORD>
						<IDENTIFIER>T</IDENTIFIER>
						<PUNCTUATOR>=</PUNCTUATOR>
						<Type source="int | float">...</Type>
						<PUNCTUATOR>;</PUNCTUATOR>
					</DeclarationType>
				</Statement>
			*/
			it('makes a ParseNodeDeclarationType node.', () => {
				const decl: PARSER.ParseNodeDeclarationType = h.typeDeclarationFromSource(`
					type  T  =  int | float;
				`);
				assert.deepStrictEqual(decl.children.map((child) => child.source), [
					'type', 'T', '=', 'int | float', ';',
				]);
			});
		});

		Dev.supports('variables') && describe('StatementAssignment', () => {
			/*
				<Statement>
					<StatementAssignment>
						<IDENTIFIER>this_answer</IDENTIFIER>
						<PUNCTUATOR>=</PUNCTUATOR>
						<Expression source="that_answer - 40">...</Expression>
						<PUNCTUATOR>;</PUNCTUATOR>
					</StatementAssignment>
				</Statement>
			*/
			it('makes a ParseNodeStatementAssignment node.', () => {
				const stmt: PARSER.ParseNodeStatement = h.statementFromSource(`this_answer  =  that_answer  -  40;`)
				assert_arrayLength(stmt.children, 1)
				const decl: Token | PARSER.ParseNodeDeclaration | PARSER.ParseNodeStatementAssignment = stmt.children[0];
				assert.ok(decl instanceof PARSER.ParseNodeStatementAssignment)
				assert.deepStrictEqual(decl.children.map((child) => child.source), [
					'this_answer', '=', 'that_answer - 40', ';',
				])
			})
		})

		context('Goal__0__List ::= Goal__0__List Statement', () => {
			it('parses multiple statements.', () => {
				/*
					<Goal>
						<FILEBOUND.../>...</FILEBOUND>
						<Goal__0__List>
							<Goal__0__List>
								<Statement source="42 ;">...</Statement>
							</Goal__0__List>
							<Statement source="420 ;"/>...</Statement>
						</Goal__0__List>
						<FILEBOUND.../>...</FILEBOUND>
					</Goal>
				*/
				const goal: PARSER.ParseNodeGoal = new Parser(`42; 420;`).parse()
				assert_arrayLength(goal.children, 3, 'goal should have 3 children')
				const stat_list: PARSER.ParseNodeGoal__0__List = goal.children[1]
				assert_arrayLength(stat_list.children, 2, 'stat_list should have 2 children')
				const stat0: PARSER.ParseNodeStatement = (() => {
					const stat_list_sub: PARSER.ParseNodeGoal__0__List = stat_list.children[0]
					assert_arrayLength(stat_list_sub.children, 1)
					return stat_list_sub.children[0]
				})()
				const stat1: PARSER.ParseNodeStatement = stat_list.children[1]
				assert.strictEqual(stat0.source, '42 ;')
				assert.strictEqual(stat1.source, '420 ;')
			})
		})
	})
})
