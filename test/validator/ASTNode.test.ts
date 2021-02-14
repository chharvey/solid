import * as assert from 'assert'
import * as xjs from 'extrajs'

import {
	SolidConfig,
	CONFIG_DEFAULT,
	Dev,
} from '../../src/core/';
import {
	Operator,
} from '../../src/enum/Operator.enum';
import {
	ReferenceError01,
	ReferenceError02,
	ReferenceError03,
	AssignmentError01,
	AssignmentError10,
	TypeError01,
	TypeError03,
	NanError01,
} from '../../src/error/';
import {
	Decorator,
	Validator,
	AST,
	SolidLanguageType,
	SolidTypeConstant,
	SolidObject,
	SolidNull,
	SolidBoolean,
	Int16,
	Float64,
	SolidString,
} from '../../src/validator/'
import {
	Builder,
	InstructionNone,
	InstructionExpression,
	InstructionConst,
	InstructionUnop,
	InstructionBinopArithmetic,
	InstructionBinopEquality,
	InstructionBinopLogical,
	InstructionCond,
	InstructionStatement,
	InstructionModule,
} from '../../src/builder/'
import {
	typeConstInt,
	typeConstFloat,
	instructionConstInt,
	instructionConstFloat,
} from '../helpers'
import {
	unitTypeFromString,
	unaryTypeFromString,
	intersectionTypeFromString,
	unionTypeFromString,
	variableDeclarationFromSource,
} from '../helpers-parse'
import {
	variableFromSource,
	operationFromSource,
	statementExpressionFromSource,
	constantFromSource,
	goalFromSource,
} from '../helpers-semantic'



describe('ASTNodeSolid', () => {
	describe('#build', () => {
		context('SemanticGoal ::= ()', () => {
			it('returns InstructionNone.', () => {
				const src: string = ``;
				const instr: InstructionNone | InstructionModule = goalFromSource(src).build(new Builder(src));
				assert.ok(instr instanceof InstructionNone)
			})
		})

		describe('ASTNodeStatementExpression', () => {
			it('returns InstructionNone for empty statement expression.', () => {
				const src: string = `;`;
				const instr: InstructionNone | InstructionStatement = statementExpressionFromSource(src)
					.build(new Builder(src))
				assert.ok(instr instanceof InstructionNone)
			})
			it('returns InstructionStatement for nonempty statement expression.', () => {
				const src: string = `42 + 420;`;
				const builder: Builder = new Builder(src);
				const stmt: AST.ASTNodeStatementExpression = statementExpressionFromSource(src);
				assert.deepStrictEqual(
					stmt.build(builder),
					new InstructionStatement(0n, operationFromSource(src).build(builder)),
				)
			})
			specify('multiple statements.', () => {
				const src: string = `42; 420;`;
				const generator: Builder = new Builder(src);
				goalFromSource(src).children.forEach((stmt, i) => {
					assert.ok(stmt instanceof AST.ASTNodeStatementExpression);
					assert.deepStrictEqual(
						stmt.build(generator),
						new InstructionStatement(BigInt(i), constantFromSource(stmt.source).build(generator)),
					)
				})
			})
		})

		context('ASTNodeConstant', () => {
			it('returns InstructionConst.', () => {
				assert.deepStrictEqual([
					'null;',
					'false;',
					'true;',
					'0;',
					'+0;',
					'-0;',
					'42;',
					'+42;',
					'-42;',
					'0.0;',
					'+0.0;',
					'-0.0;',
					'-4.2e-2;',
				].map((src) => constantFromSource(src).build(new Builder(src))), [
					instructionConstInt(0n),
					instructionConstInt(0n),
					instructionConstInt(1n),
					instructionConstInt(0n),
					instructionConstInt(0n),
					instructionConstInt(0n),
					instructionConstInt(42n),
					instructionConstInt(42n),
					instructionConstInt(-42n),
					instructionConstFloat(0),
					instructionConstFloat(0),
					instructionConstFloat(-0),
					instructionConstFloat(-0.042),
				])
			})
		})

		context('ASTNodeOperation', () => {
			specify('with constant folding on.', () => {
				const nodes: readonly [string, AST.ASTNodeOperation][] = [
					`!null;`,
					`!false;`,
					`!true;`,
					`!42;`,
					`!4.2;`,
					`!0;`,
					`!0.0;`,
					`?null;`,
					`?false;`,
					`?true;`,
					`?42;`,
					`?4.2;`,
					`?0;`,
					`?0.0;`,
					`42 + 420;`,
					`42 - 420;`,
					`3.0e1 - 201.0e-1;`,
					`3 * 2.1;`,
					` 126 /  3;`,
					`-126 /  3;`,
					` 126 / -3;`,
					`-126 / -3;`,
					` 200 /  3;`,
					` 200 / -3;`,
					`-200 /  3;`,
					`-200 / -3;`,
					`42 == 420;`,
					`42 != 420;`,
					`4.2 == 42;`,
					`42 != 42.0;`,
					`true is 1;`,
					`true == 1;`,
					`null is false;`,
					`null == false;`,
					`false == 0.0;`,
					`false is 0.0;`,
					`0.0 is 0;`,
					`42 && 420;`,
					`4.2 || -420;`,
					`null && 201.0e-1;`,
					`true && 201.0e-1;`,
					`false || null;`,
					`if true  then false   else 2;`,
					`if false then 3.0     else null;`,
					`if true  then 2       else 3.0;`,
					`if false then 2 + 3.0 else 1.0 * 2;`,
					`42 ^ 2 * 420;`,
					`2 ^ 15 + 2 ^ 14;`,
					`-(2 ^ 14) - 2 ^ 15;`,
					`2 * 3 + 5;`,
					`2 * 3 + 5.0;`,
					`-(5) ^ +(2 * 3);`,
				].map((src) => [src, operationFromSource(src)]);
				assert.deepStrictEqual(
					nodes.map(([src,  node]) => node.build(new Builder(src))),
					nodes.map(([_src, node]) => {
						const assess: SolidObject | null = node.assess();
						assert.ok(assess);
						return InstructionConst.fromAssessment(assess);
					}),
					'produces `InstructionConst.new(ASTNodeOperation#assess)`',
				)
			}).timeout(10_000);
			context('with constant folding off.', () => {
				const folding_off: SolidConfig = {
					...CONFIG_DEFAULT,
					compilerOptions: {
						...CONFIG_DEFAULT.compilerOptions,
						constantFolding: false,
					},
				}
				function buildOperations(tests: ReadonlyMap<string, InstructionExpression>): void {
					assert.deepStrictEqual(
						[...tests.keys()].map((src) => operationFromSource(src, folding_off).build(new Builder(src, folding_off))),
						[...tests.values()],
					)
				}
				specify('SemanticOperation[operator: NOT | EMP] ::= SemanticConstant', () => {
					buildOperations(new Map<string, InstructionUnop>([
						[`!null;`,  new InstructionUnop(Operator.NOT, instructionConstInt(0n))],
						[`!false;`, new InstructionUnop(Operator.NOT, instructionConstInt(0n))],
						[`!true;`,  new InstructionUnop(Operator.NOT, instructionConstInt(1n))],
						[`!42;`,    new InstructionUnop(Operator.NOT, instructionConstInt(42n))],
						[`!4.2;`,   new InstructionUnop(Operator.NOT, instructionConstFloat(4.2))],
						[`?null;`,  new InstructionUnop(Operator.EMP, instructionConstInt(0n))],
						[`?false;`, new InstructionUnop(Operator.EMP, instructionConstInt(0n))],
						[`?true;`,  new InstructionUnop(Operator.EMP, instructionConstInt(1n))],
						[`?42;`,    new InstructionUnop(Operator.EMP, instructionConstInt(42n))],
						[`?4.2;`,   new InstructionUnop(Operator.EMP, instructionConstFloat(4.2))],
					]))
				})
				specify('SemanticOperation[operator: NEG] ::= SemanticConstant', () => {
					buildOperations(new Map<string, InstructionUnop>([
						[`-(4);`,   new InstructionUnop(Operator.NEG, instructionConstInt(4n))],
						[`-(4.2);`, new InstructionUnop(Operator.NEG, instructionConstFloat(4.2))],
					]))
				})
				specify('SemanticOperation[operator: ADD | MUL] ::= SemanticConstant SemanticConstant', () => {
					buildOperations(new Map([
						[`42 + 420;`, new InstructionBinopArithmetic(Operator.ADD, instructionConstInt(42n),   instructionConstInt(420n))],
						[`3 * 2.1;`,  new InstructionBinopArithmetic(Operator.MUL, instructionConstFloat(3.0), instructionConstFloat(2.1))],
					]))
				})
				specify('SemanticOperation[operator: DIV] ::= SemanticConstant SemanticConstant', () => {
					buildOperations(xjs.Map.mapValues(new Map([
						[' 126 /  3;', [ 126n,  3n]],
						['-126 /  3;', [-126n,  3n]],
						[' 126 / -3;', [ 126n, -3n]],
						['-126 / -3;', [-126n, -3n]],
						[' 200 /  3;', [ 200n,  3n]],
						[' 200 / -3;', [ 200n, -3n]],
						['-200 /  3;', [-200n,  3n]],
						['-200 / -3;', [-200n, -3n]],
					]), ([a, b]) => new InstructionBinopArithmetic(
						Operator.DIV,
						instructionConstInt(a),
						instructionConstInt(b),
					)))
				})
				specify('SemanticOperation[operator: IS | EQ] ::= SemanticConstant SemanticConstant', () => {
					assert.deepStrictEqual([
						`42 == 420;`,
						`4.2 is 42;`,
						`42 is 4.2;`,
						`4.2 == 42;`,
						`true is 1;`,
						`true == 1;`,
						`null is false;`,
						`null == false;`,
						`false == 0.0;`,
					].map((src) => operationFromSource(src, folding_off).build(new Builder(src, folding_off))), [
						new InstructionBinopEquality(
							Operator.EQ,
							instructionConstInt(42n),
							instructionConstInt(420n),
						),
						new InstructionBinopEquality(
							Operator.IS,
							instructionConstFloat(4.2),
							instructionConstInt(42n),
						),
						new InstructionBinopEquality(
							Operator.IS,
							instructionConstInt(42n),
							instructionConstFloat(4.2),
						),
						new InstructionBinopEquality(
							Operator.EQ,
							instructionConstFloat(4.2),
							instructionConstFloat(42.0),
						),
						new InstructionBinopEquality(
							Operator.IS,
							instructionConstInt(1n),
							instructionConstInt(1n),
						),
						new InstructionBinopEquality(
							Operator.EQ,
							instructionConstInt(1n),
							instructionConstInt(1n),
						),
						new InstructionBinopEquality(
							Operator.IS,
							instructionConstInt(0n),
							instructionConstInt(0n),
						),
						new InstructionBinopEquality(
							Operator.EQ,
							instructionConstInt(0n),
							instructionConstInt(0n),
						),
						new InstructionBinopEquality(
							Operator.EQ,
							instructionConstFloat(0.0),
							instructionConstFloat(0.0),
						),
					])
				})
				describe('SemanticOperation[operator: AND | OR] ::= SemanticConstant SemanticConstant', () => {
					it('returns InstructionBinopLogical.', () => {
						assert.deepStrictEqual([
							`42 && 420;`,
							`4.2 || -420;`,
							`null && 201.0e-1;`,
							`true && 201.0e-1;`,
							`false || null;`,
						].map((src) => operationFromSource(src, folding_off).build(new Builder(src, folding_off))), [
							new InstructionBinopLogical(
								0n,
								Operator.AND,
								instructionConstInt(42n),
								instructionConstInt(420n),
							),
							new InstructionBinopLogical(
								0n,
								Operator.OR,
								instructionConstFloat(4.2),
								instructionConstFloat(-420.0),
							),
							new InstructionBinopLogical(
								0n,
								Operator.AND,
								instructionConstFloat(0.0),
								instructionConstFloat(20.1),
							),
							new InstructionBinopLogical(
								0n,
								Operator.AND,
								instructionConstFloat(1.0),
								instructionConstFloat(20.1),
							),
							new InstructionBinopLogical(
								0n,
								Operator.OR,
								instructionConstInt(0n),
								instructionConstInt(0n),
							),
						])
					})
					it('counts internal variables correctly.', () => {
						const src: string = `1 && 2 || 3 && 4;`
						assert.deepStrictEqual(
							operationFromSource(src, folding_off).build(new Builder(src, folding_off)),
							new InstructionBinopLogical(
								0n,
								Operator.OR,
								new InstructionBinopLogical(
									1n,
									Operator.AND,
									instructionConstInt(1n),
									instructionConstInt(2n),
								),
								new InstructionBinopLogical(
									2n,
									Operator.AND,
									instructionConstInt(3n),
									instructionConstInt(4n),
								),
							),
						)
					})
				})
				specify('ExpressionConditional ::= "if" Expression "then" Expression "else" Expression;', () => {
					buildOperations(xjs.Map.mapValues(new Map([
						[`if true  then false   else 2;`,       [new Int16(1n), new Int16(0n),    new Int16(2n)]],
						[`if false then 3.0     else null;`,    [new Int16(0n), new Float64(3.0), new Float64(0.0)]],
						[`if true  then 2       else 3.0;`,     [new Int16(1n), new Float64(2.0), new Float64(3.0)]],
					]), ([cond, cons, alt]) => new InstructionCond(
						new InstructionConst(cond),
						new InstructionConst(cons),
						new InstructionConst(alt),
					)))
				})
				specify('compound expression.', () => {
					buildOperations(new Map([
						[`42 ^ 2 * 420;`, new InstructionBinopArithmetic(
							Operator.MUL,
							new InstructionBinopArithmetic(
								Operator.EXP,
								instructionConstInt(42n),
								instructionConstInt(2n),
							),
							instructionConstInt(420n),
						)],
						[`2 * 3.0 + 5;`, new InstructionBinopArithmetic(
							Operator.ADD,
							new InstructionBinopArithmetic(
								Operator.MUL,
								instructionConstFloat(2.0),
								instructionConstFloat(3.0),
							),
							instructionConstFloat(5.0),
						)],
					]))
				})
			})
			context('with constant folding off, int coercion off.', () => {
				const folding_coercion_off: SolidConfig = {
					...CONFIG_DEFAULT,
					compilerOptions: {
						...CONFIG_DEFAULT.compilerOptions,
						constantFolding: false,
						intCoercion: false,
					},
				}
				describe('SemanticOperation[operator: EQ]', () => {
					it('does not coerce operands into floats.', () => {
						assert.deepStrictEqual([
							`42 == 420;`,
							`4.2 == 42;`,
							`42 == 4.2;`,
							`null == 0.0;`,
							`false == 0.0;`,
							`true == 1.0;`,
						].map((src) => operationFromSource(src, folding_coercion_off).build(new Builder(src, folding_coercion_off))), [
							[instructionConstInt(42n),   instructionConstInt(420n)],
							[instructionConstFloat(4.2), instructionConstInt(42n)],
							[instructionConstInt(42n),   instructionConstFloat(4.2)],
							[instructionConstInt(0n),    instructionConstFloat(0.0)],
							[instructionConstInt(0n),    instructionConstFloat(0.0)],
							[instructionConstInt(1n),    instructionConstFloat(1.0)],
						].map(([left, right]) => new InstructionBinopEquality(Operator.EQ, left, right)))
					})
				})
			})
		})
	})


	Dev.supports('variables') && describe('#varCheck', () => {
		describe('ASTNodeTypeAlias', () => {
			it('throws if the validator does not contain a record for the identifier.', () => {
				goalFromSource(`
					type T = int;
					type U = float | T;
				`).varCheck(); // assert does not throw
				assert.throws(() => goalFromSource(`
					type U = float | T;
				`).varCheck(), ReferenceError01);
			});
			it.skip('throws when there is a temporal dead zone.', () => {
				assert.throws(() => goalFromSource(`
					T;
					type T = int;
				`).varCheck(), ReferenceError02);
			});
			it('throws if was declared as a value variable.', () => {
				assert.throws(() => goalFromSource(`
					let FOO: int = 42;
					type T = FOO | float;
				`).varCheck(), ReferenceError03);
			});
		});
		describe('ASTNodeConstant', () => {
			it('never throws.', () => {
				constantFromSource(`42;`).varCheck();
			});
		});
		describe('ASTNodeVariable', () => {
			it('throws if the validator does not contain a record for the identifier.', () => {
				goalFromSource(`
					let unfixed i: int = 42;
					i;
				`).varCheck(); // assert does not throw
				assert.throws(() => variableFromSource(`i;`).varCheck(), ReferenceError01);
			});
			it.skip('throws when there is a temporal dead zone.', () => {
				assert.throws(() => goalFromSource(`
					i;
					let unfixed i: int = 42;
				`).varCheck(), ReferenceError02);
			});
			it('throws if it was declared as a type alias.', () => {
				assert.throws(() => goalFromSource(`
					type FOO = int;
					42 || FOO;
				`).varCheck(), ReferenceError03);
			});
		});
		describe('ASTNodeDeclarationVariable', () => {
			it('throws if the validator already contains a record for the variable.', () => {
				assert.throws(() => goalFromSource(`
					let i: int = 42;
					let i: int = 43;
				`).varCheck(), AssignmentError01);
				assert.throws(() => goalFromSource(`
					type FOO = float;
					let FOO: int = 42;
				`).varCheck(), AssignmentError01);
			});
		});
		describe('ASTNodeDeclarationType', () => {
			it('throws if the validator already contains a record for the symbol.', () => {
				assert.throws(() => goalFromSource(`
					type T = int;
					type T = float;
				`).varCheck(), AssignmentError01);
				assert.throws(() => goalFromSource(`
					let FOO: int = 42;
					type FOO = float;
				`).varCheck(), AssignmentError01);
			});
		});
		describe('ASTNodeAssignee', () => {
			it('throws if the variable is not unfixed.', () => {
				goalFromSource(`
					let unfixed i: int = 42;
					i = 43;
				`).varCheck(); // assert does not throw
				assert.throws(() => goalFromSource(`
					let i: int = 42;
					i = 43;
				`).varCheck(), AssignmentError10);
			});
			it('always throws for type alias reassignment.', () => {
				assert.throws(() => goalFromSource(`
					type T = 42;
					T = 43;
				`).varCheck(), ReferenceError03);
			});
		});
	});


	describe('#typeCheck', () => {
		describe('ASTNodeDeclarationVariable', () => {
			it('checks the assigned expression’s type against the variable assignee’s type.', () => {
				const src: string = `let  the_answer:  int | float =  21  *  2;`
				const decl: AST.ASTNodeDeclarationVariable = Decorator.decorate(variableDeclarationFromSource(src));
				decl.typeCheck();
			})
			it('throws when the assigned expression’s type is not compatible with the variable assignee’s type.', () => {
				const src: string = `let  the_answer:  null =  21  *  2;`
				const decl: AST.ASTNodeDeclarationVariable = Decorator.decorate(variableDeclarationFromSource(src));
				assert.throws(() => decl.typeCheck(), TypeError03);
			})
			it('with int coersion on, allows assigning ints to floats.', () => {
				const src: string = `let x: float = 42;`
				const decl: AST.ASTNodeDeclarationVariable = Decorator.decorate(variableDeclarationFromSource(src));
				decl.typeCheck();
			})
			it('with int coersion off, throws when assigning int to float.', () => {
				const src: string = `let x: float = 42;`
				const decl: AST.ASTNodeDeclarationVariable = Decorator.decorate(variableDeclarationFromSource(src));
				assert.throws(() => decl.typeCheck(new Validator({
					...CONFIG_DEFAULT,
					compilerOptions: {
						...CONFIG_DEFAULT.compilerOptions,
						intCoercion: false,
					},
				})), TypeError03);
			})
		})
	})


	describe('ASTNodeType', () => {
		describe('#assess', () => {
			it('computes the value of constant null, boolean, or number types.', () => {
				assert.deepStrictEqual([
					`null`,
					`false`,
					`true`,
					`42`,
					`4.2e+3`,
				].map((src) => Decorator.decorate(unitTypeFromString(src)).assess()), [
					SolidNull,
					SolidBoolean.FALSETYPE,
					SolidBoolean.TRUETYPE,
					new SolidTypeConstant(new Int16(42n)),
					new SolidTypeConstant(new Float64(4.2e+3)),
				])
			})
			it('computes the value of a type alias.', () => {
				const validator: Validator = new Validator();
				const goal: AST.ASTNodeGoal = goalFromSource(`
					type T = int;
					type U = T;
				`);
				goal.varCheck(validator);
				assert.deepStrictEqual(
					((goal
						.children[1] as AST.ASTNodeDeclarationType)
						.children[1] as AST.ASTNodeTypeAlias)
						.assess(validator),
					Int16,
				);
			});
			it('computes the value of keyword type.', () => {
				assert.deepStrictEqual([
					'bool',
					'int',
					'float',
					'obj',
				].map((src) => Decorator.decorate(unitTypeFromString(src)).assess()), [
					SolidBoolean,
					Int16,
					Float64,
					SolidObject,
				])
			})
			it('computes the value of a nullified (ORNULL) type.', () => {
				assert.deepStrictEqual(
					Decorator.decorate(unaryTypeFromString(`int!`)).assess(),
					Int16.union(SolidNull),
				)
			})
			it('computes the value of AND and OR operators', () => {
				assert.deepStrictEqual(
					Decorator.decorate(intersectionTypeFromString(`obj & 3`)).assess(),
					SolidObject.intersect(typeConstInt(3n)),
				)
				assert.deepStrictEqual(
					Decorator.decorate(unionTypeFromString(`4.2 | int`)).assess(),
					typeConstFloat(4.2).union(Int16),
				)
			})
		})
	})


	context('ASTNodeExpression', () => {
		describe('#type', () => {
			function typeOperations(tests: ReadonlyMap<string, SolidObject>): void {
				return assert.deepStrictEqual(
					[...tests.keys()].map((src) => operationFromSource(src).type()),
					[...tests.values()].map((result) => new SolidTypeConstant(result)),
				);
			}
			context('with constant folding off, int coercion off.', () => {
				const folding_coercion_off: SolidConfig = {
					...CONFIG_DEFAULT,
					compilerOptions: {
						...CONFIG_DEFAULT.compilerOptions,
						constantFolding: false,
						intCoercion: false,
					},
				}
				function typeOfOperationFromSource(src: string): SolidLanguageType {
					return operationFromSource(src, folding_coercion_off).type(new Validator(folding_coercion_off));
				}
				describe('ASTNodeOperationBinaryArithmetic', () => {
					it('returns `Integer` if both operands are ints.', () => {
						assert.deepStrictEqual(typeOfOperationFromSource(`7 * 3;`), Int16);
					})
					it('returns `Float` if both operands are floats.', () => {
						assert.deepStrictEqual(typeOfOperationFromSource(`7.0 - 3.0;`), Float64);
					})
					it('throws TypeError for invalid type operations.', () => {
						assert.throws(() => typeOfOperationFromSource(`7.0 + 3;`), TypeError01);
					});
				})
				describe('ASTNodeOperationBinaryComparative', () => {
					it('returns `Boolean` if both operands are of the same numeric type.', () => {
						assert.deepStrictEqual(typeOfOperationFromSource(`7 < 3;`), SolidBoolean);
						assert.deepStrictEqual(typeOfOperationFromSource(`7.0 >= 3.0;`), SolidBoolean);
					})
					it('throws TypeError if operands have different types.', () => {
						assert.throws(() => typeOfOperationFromSource(`7.0 <= 3;`), TypeError01);
					})
				})
				describe('ASTNodeOperationBinaryEquality[operator=EQ]', () => {
					it('returns `false` if operands are of different numeric types.', () => {
						assert.deepStrictEqual(typeOfOperationFromSource(`7 == 7.0;`), SolidBoolean.FALSETYPE);
					})
					it('returns `false` if operands are of disjoint types in general.', () => {
						assert.deepStrictEqual(typeOfOperationFromSource(`7 == null;`), SolidBoolean.FALSETYPE);
					})
				})
			})
			context('with constant folding on, with int coersion on.', () => {
				context('ASTNodeConstant', () => {
					it('returns a constant Null type for ASTNodeConstant with null value.', () => {
						assert.ok(constantFromSource(`null;`).type().equals(SolidNull));
					})
					it('returns a constant Boolean type for ASTNodeConstant with bool value.', () => {
						assert.deepStrictEqual([
							`false;`,
							`true;`,
						].map((src) => constantFromSource(src).type()), [
							SolidBoolean.FALSETYPE,
							SolidBoolean.TRUETYPE,
						])
					})
					it('returns a constant Integer type for ASTNodeConstant with integer value.', () => {
						assert.deepStrictEqual(constantFromSource(`42;`).type(), new SolidTypeConstant(new Int16(42n)));
					})
					it('returns a constant Float type for ASTNodeConstant with float value.', () => {
						assert.deepStrictEqual(constantFromSource(`4.2e+1;`).type(), new SolidTypeConstant(new Float64(42.0)));
					})
					it('returns `String` for ASTNodeConstant with string value.', () => {
						;[
							...(Dev.supports('literalString') ? [
								constantFromSource(`'42';`),
							] : []),
							...(Dev.supports('literalTemplate') ? [
								(goalFromSource(`'''42''';`)
									.children[0] as AST.ASTNodeStatementExpression)
									.children[0] as AST.ASTNodeTemplate,
								(goalFromSource(`'''the answer is {{ 7 * 3 * 2 }} but what is the question?''';`)
									.children[0] as AST.ASTNodeStatementExpression)
									.children[0] as AST.ASTNodeTemplate,
							] : []),
						].forEach((node) => {
							assert.strictEqual(node.type(), SolidString)
						})
					})
				})
				context('ASTNodeOperationBinaryArithmetic', () => {
					it('returns a constant Integer type for any operation of integers.', () => {
						assert.deepStrictEqual(operationFromSource(`7 * 3 * 2;`).type(), new SolidTypeConstant(new Int16(7n * 3n * 2n)));
					})
					it('returns a constant Float type for any operation of mix of integers and floats.', () => {
						assert.deepStrictEqual(operationFromSource(`3.0 * 2.7;`).type(), new SolidTypeConstant(new Float64(3.0 * 2.7)));
						assert.deepStrictEqual(operationFromSource(`7 * 3.0 * 2;`).type(), new SolidTypeConstant(new Float64(7 * 3.0 * 2)));
					})
				})
			})
			context('with constant folding off, with int coersion on.', () => {
				const folding_off: SolidConfig = {
					...CONFIG_DEFAULT,
					compilerOptions: {
						...CONFIG_DEFAULT.compilerOptions,
						constantFolding: false,
					},
				}
				context('ASTNodeOperationBinaryArithmetic', () => {
					it('returns Integer for integer arithmetic.', () => {
						const node: AST.ASTNodeOperation = operationFromSource(`(7 + 3) * 2;`, folding_off);
						assert.deepStrictEqual(
							[node.type(new Validator(folding_off)), node.children.length],
							[Int16,                                 2],
						)
						assert.deepStrictEqual(
							[node.children[0].type(new Validator(folding_off)), node.children[1].type(new Validator(folding_off))],
							[Int16,                                             Int16],
						)
					})
					it('returns Float for float arithmetic.', () => {
						const node: AST.ASTNodeOperation = operationFromSource(`7 * 3.0 ^ 2;`, folding_off);
						assert.deepStrictEqual(
							[node.type(new Validator(folding_off)), node.children.length],
							[Float64,                               2],
						)
						assert.deepStrictEqual(
							[node.children[0].type(new Validator(folding_off)), node.children[1].type(new Validator(folding_off))],
							[Int16,                                             Float64],
						)
					})
				})
				it('allows coercing of ints to floats if there are any floats.', () => {
					assert.deepStrictEqual(operationFromSource(`7.0 > 3;`) .type(new Validator(folding_off)), SolidBoolean);
					assert.deepStrictEqual(operationFromSource(`7 == 7.0;`).type(new Validator(folding_off)), SolidBoolean);
				})
				describe('ASTNodeOperationBinaryEquality[operator=IS]', () => {
					it('returns `false` if operands are of different numeric types.', () => {
						assert.deepStrictEqual(operationFromSource(`7 is 7.0;`, folding_off).type(new Validator(folding_off)), SolidBoolean.FALSETYPE);
					})
				})
			})
			Dev.supports('variables') && it('returns Unknown for undeclared variables.', () => {
				// NOTE: a reference error will be thrown at the variable-checking stage
				assert.strictEqual(variableFromSource(`x;`).type(), SolidLanguageType.UNKNOWN);
			});
			it('returns a constant Boolean type for boolean unary operation of anything.', () => {
				typeOperations(xjs.Map.mapValues(new Map([
					[`!false;`,  true],
					[`!true;`,   false],
					[`!null;`,   true],
					[`!42;`,     false],
					[`!4.2e+1;`, false],
					[`?false;`,  true],
					[`?true;`,   false],
					[`?null;`,   true],
					[`?42;`,     false],
					[`?4.2e+1;`, false],
				]), (v) => SolidBoolean.fromBoolean(v)))
			})
			it('computes type for equality and comparison.', () => {
				typeOperations(xjs.Map.mapValues(new Map([
					[`2 < 3;`,    true],
					[`2 > 3;`,    false],
					[`2 <= 3;`,   true],
					[`2 >= 3;`,   false],
					[`2 !< 3;`,   false],
					[`2 !> 3;`,   true],
					[`2 is 3;`,   false],
					[`2 isnt 3;`, true],
					[`2 == 3;`,   false],
					[`2 != 3;`,   true],
					[`0 is -0;`,     true],
					[`0 == -0;`,     true],
					[`0.0 is 0;`,    false],
					[`0.0 == 0;`,    true],
					[`0.0 is -0;`,   false],
					[`0.0 == -0;`,   true],
					[`-0.0 is 0;`,   false],
					[`-0.0 == 0;`,   true],
					[`-0.0 is 0.0;`, false],
					[`-0.0 == 0.0;`, true],
				]), (v) => SolidBoolean.fromBoolean(v)))
			})
			it('computes type for AND and OR.', () => {
				typeOperations(new Map<string, SolidObject>([
					[`null  && false;`, SolidNull.NULL],
					[`false && null;`,  SolidBoolean.FALSE],
					[`true  && null;`,  SolidNull.NULL],
					[`false && 42;`,    SolidBoolean.FALSE],
					[`4.2   && true;`,  SolidBoolean.TRUE],
					[`null  || false;`, SolidBoolean.FALSE],
					[`false || null;`,  SolidNull.NULL],
					[`true  || null;`,  SolidBoolean.TRUE],
					[`false || 42;`,    new Int16(42n)],
					[`4.2   || true;`,  new Float64(4.2)],
				]))
			})
			it('throws for arithmetic operation of non-numbers.', () => {
				[
					`null + 5;`,
					`5 * null;`,
					`false - 2;`,
					`2 / true;`,
					`null ^ false;`,
					...(Dev.supports('literalString') ? [`'hello' + 5;`] : []),
				].forEach((src) => {
					assert.throws(() => operationFromSource(src).type(), TypeError01);
				})
			})
			it('throws for comparative operation of non-numbers.', () => {
				assert.throws(() => operationFromSource(`7.0 <= null;`).type(), TypeError01);
			});
			describe('ASTNodeOperationTernary', () => {
				context('with constant folding on', () => {
					it('computes type for for conditionals', () => {
						typeOperations(new Map<string, SolidObject>([
							[`if true then false else 2;`,          SolidBoolean.FALSE],
							[`if false then 3.0 else null;`,        SolidNull.NULL],
							[`if true then 2 else 3.0;`,            new Int16(2n)],
							[`if false then 2 + 3.0 else 1.0 * 2;`, new Float64(2.0)],
						]))
					})
				})
				it('throws when condition is not boolean.', () => {
					assert.throws(() => operationFromSource(`if 2 then true else false;`).type(), TypeError01);
				})
			})
		})

		describe('#assess', () => {
			function assessOperations(tests: Map<string, SolidObject>): void {
				return assert.deepStrictEqual(
					[...tests.keys()].map((src) => operationFromSource(src).assess()),
					[...tests.values()],
				);
			}
			it('computes the value of constant null or boolean expression.', () => {
				assert.deepStrictEqual([
					'null;',
					'false;',
					'true;',
				].map((src) => constantFromSource(src).assess()), [
					SolidNull.NULL,
					SolidBoolean.FALSE,
					SolidBoolean.TRUE,
				]);
			})
			it('computes the value of a constant float expression.', () => {
				assert.deepStrictEqual(`
					55.  -55.  033.  -033.  2.007  -2.007
					91.27e4  -91.27e4  91.27e-4  -91.27e-4
					-0.  -0.0  6.8e+0  6.8e-0  0.0e+0  -0.0e-0
				`.trim().replace(/\n\t+/g, '  ').split('  ').map((src) => constantFromSource(`${ src };`).assess()), [
					55, -55, 33, -33, 2.007, -2.007,
					91.27e4, -91.27e4, 91.27e-4, -91.27e-4,
					-0, -0, 6.8, 6.8, 0, -0,
				].map((v) => new Float64(v)));
			})

			Dev.supports('variables') && describe('ASTNodeVariable', () => {
				it('assesses the value of a fixed variable.', () => {
					const validator: Validator = new Validator();
					const goal: AST.ASTNodeGoal = goalFromSource(`
						let x: int = 21 * 2;
						x;
					`);
					goal.varCheck(validator);
					goal.typeCheck(validator);
					assert.ok(!(goal.children[0] as AST.ASTNodeDeclarationVariable).unfixed);
					assert.deepStrictEqual(
						((goal
							.children[1] as AST.ASTNodeStatementExpression)
							.children[0] as AST.ASTNodeExpression)
							.assess(validator),
						new Int16(42n),
					);
				});
				it('returns null for an unfixed variable.', () => {
					const validator: Validator = new Validator();
					const goal: AST.ASTNodeGoal = goalFromSource(`
						let unfixed x: int = 21 * 2;
						x;
					`);
					goal.varCheck(validator);
					goal.typeCheck(validator);
					assert.ok((goal.children[0] as AST.ASTNodeDeclarationVariable).unfixed);
					assert.deepStrictEqual(
						((goal
							.children[1] as AST.ASTNodeStatementExpression)
							.children[0] as AST.ASTNodeExpression)
							.assess(validator),
						null,
					);
				});
				it('returns null for an uncomputable fixed variable.', () => {
					const validator: Validator = new Validator();
					const goal: AST.ASTNodeGoal = goalFromSource(`
						let unfixed x: int = 21 * 2;
						let y: int = x / 2;
						y;
					`);
					goal.varCheck(validator);
					goal.typeCheck(validator);
					assert.ok(!(goal.children[1] as AST.ASTNodeDeclarationVariable).unfixed);
					assert.deepStrictEqual(
						((goal
							.children[2] as AST.ASTNodeStatementExpression)
							.children[0] as AST.ASTNodeExpression)
							.assess(validator),
						null,
					);
				});
			});

			it('computes the value of a logical negation of anything.', () => {
				assessOperations(new Map([
					[`!false;`,  SolidBoolean.TRUE],
					[`!true;`,   SolidBoolean.FALSE],
					[`!null;`,   SolidBoolean.TRUE],
					[`!0;`,      SolidBoolean.FALSE],
					[`!42;`,     SolidBoolean.FALSE],
					[`!0.0;`,    SolidBoolean.FALSE],
					[`!-0.0;`,   SolidBoolean.FALSE],
					[`!4.2e+1;`, SolidBoolean.FALSE],
				]))
			})
			it('computes the value of emptiness of anything.', () => {
				assessOperations(new Map([
					[`?false;`,  SolidBoolean.TRUE],
					[`?true;`,   SolidBoolean.FALSE],
					[`?null;`,   SolidBoolean.TRUE],
					[`?0;`,      SolidBoolean.TRUE],
					[`?42;`,     SolidBoolean.FALSE],
					[`?0.0;`,    SolidBoolean.TRUE],
					[`?-0.0;`,   SolidBoolean.TRUE],
					[`?4.2e+1;`, SolidBoolean.FALSE],
				]))
			})
			it('computes the value of an integer operation of constants.', () => {
				assessOperations(xjs.Map.mapValues(new Map([
					[`42 + 420;`,           42 + 420],
					[`42 - 420;`,           42 + -420],
					[` 126 /  3;`,          Math.trunc( 126 /  3)],
					[`-126 /  3;`,          Math.trunc(-126 /  3)],
					[` 126 / -3;`,          Math.trunc( 126 / -3)],
					[`-126 / -3;`,          Math.trunc(-126 / -3)],
					[` 200 /  3;`,          Math.trunc( 200 /  3)],
					[` 200 / -3;`,          Math.trunc( 200 / -3)],
					[`-200 /  3;`,          Math.trunc(-200 /  3)],
					[`-200 / -3;`,          Math.trunc(-200 / -3)],
					[`42 ^ 2 * 420;`,       (42 ** 2 * 420) % (2 ** 16)],
					[`2 ^ 15 + 2 ^ 14;`,    -(2 ** 14)],
					[`-(2 ^ 14) - 2 ^ 15;`, 2 ** 14],
					[`-(5) ^ +(2 * 3);`,    (-(5)) ** +(2 * 3)],
				]), (val) => new Int16(BigInt(val))))
			})
			it('overflows integers properly.', () => {
				assert.deepStrictEqual([
					`2 ^ 15 + 2 ^ 14;`,
					`-(2 ^ 14) - 2 ^ 15;`,
				].map((src) => operationFromSource(src).assess()), [
					new Int16(-(2n ** 14n)),
					new Int16(2n ** 14n),
				])
			})
			it('computes the value of a float operation of constants.', () => {
				assessOperations(new Map<string, SolidObject>([
					[`3.0e1 - 201.0e-1;`,     new Float64(30 - 20.1)],
					[`3 * 2.1;`,     new Float64(3 * 2.1)],
				]))
			})
			it('should throw when performing an operation that does not yield a valid number.', () => {
				assert.throws(() => operationFromSource(`-4 ^ -0.5;`).assess(), NanError01)
			})
			it('computes the value of comparison operators.', () => {
				assessOperations(xjs.Map.mapValues(new Map([
					[`3 <  3;`,     false],
					[`3 >  3;`,     false],
					[`3 <= 3;`,     true],
					[`3 >= 3;`,     true],
					[`5.2 <  7.0;`, true],
					[`5.2 >  7.0;`, false],
					[`5.2 <= 7.0;`, true],
					[`5.2 >= 7.0;`, false],
					[`5.2 <  9;`, true],
					[`5.2 >  9;`, false],
					[`5.2 <= 9;`, true],
					[`5.2 >= 9;`, false],
					[`5 <  9.2;`, true],
					[`5 >  9.2;`, false],
					[`5 <= 9.2;`, true],
					[`5 >= 9.2;`, false],
					[`3.0 <  3;`, false],
					[`3.0 >  3;`, false],
					[`3.0 <= 3;`, true],
					[`3.0 >= 3;`, true],
					[`3 <  3.0;`, false],
					[`3 >  3.0;`, false],
					[`3 <= 3.0;`, true],
					[`3 >= 3.0;`, true],
				]), (val) => SolidBoolean.fromBoolean(val)))
			})
			it('computes the value of IS and EQ operators.', () => {
				assessOperations(xjs.Map.mapValues(new Map([
					[`null is null;`, true],
					[`null == null;`, true],
					[`null is 5;`,    false],
					[`null == 5;`,    false],
					[`true is 1;`,    false],
					[`true == 1;`,    false],
					[`true is 1.0;`,  false],
					[`true == 1.0;`,  false],
					[`true is 5.1;`,  false],
					[`true == 5.1;`,  false],
					[`true is true;`, true],
					[`true == true;`, true],
					[`3.0 is 3;`,     false],
					[`3.0 == 3;`,     true],
					[`3 is 3.0;`,     false],
					[`3 == 3.0;`,     true],
					[`0.0 is 0.0;`,   true],
					[`0.0 == 0.0;`,   true],
					[`0.0 is -0.0;`,  false],
					[`0.0 == -0.0;`,  true],
					[`0 is -0;`,     true],
					[`0 == -0;`,     true],
					[`0.0 is 0;`,    false],
					[`0.0 == 0;`,    true],
					[`0.0 is -0;`,   false],
					[`0.0 == -0;`,   true],
					[`-0.0 is 0;`,   false],
					[`-0.0 == 0;`,   true],
					[`-0.0 is 0.0;`, false],
					[`-0.0 == 0.0;`, true],
				]), (val) => SolidBoolean.fromBoolean(val)))
			}).timeout(10_000);
			it('computes the value of AND and OR operators.', () => {
				assessOperations(new Map<string, SolidObject>([
					[`null && 5;`,     SolidNull.NULL],
					[`null || 5;`,     new Int16(5n)],
					[`5 && null;`,     SolidNull.NULL],
					[`5 || null;`,     new Int16(5n)],
					[`5.1 && true;`,   SolidBoolean.TRUE],
					[`5.1 || true;`,   new Float64(5.1)],
					[`3.1 && 5;`,      new Int16(5n)],
					[`3.1 || 5;`,      new Float64(3.1)],
					[`false && null;`, SolidBoolean.FALSE],
					[`false || null;`, SolidNull.NULL],
				]))
			})
			it('computes the value of a conditional expression.', () => {
				assessOperations(new Map<string, SolidObject>([
					[`if true then false else 2;`,          SolidBoolean.FALSE],
					[`if false then 3.0 else null;`,        SolidNull.NULL],
					[`if true then 2 else 3.0;`,            new Int16(2n)],
					[`if false then 2 + 3.0 else 1.0 * 2;`, new Float64(2.0)],
				]))
			})
		})
	})
})
