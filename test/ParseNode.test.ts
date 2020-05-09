import * as assert from 'assert'

import Util   from '../src/class/Util.class'
import Parser from '../src/class/Parser.class'
import {
	SemanticNodeNull,
	SemanticNodeGoal,
	SemanticNodeStatementList,
	SemanticNodeStatementExpression,
	SemanticNodeTemplate,
} from '../src/class/SemanticNode.class'



describe('ParseNode', () => {
	describe('#decorate', () => {
		context('Goal ::= #x02 #x03', () => {
			it('makes a SemanticNodeNull node.', () => {
				assert.strictEqual(new Parser('').parse().decorate().tagname, 'Null')
			})
		})

		context('Statement ::= ";"', () => {
			it('makes a StatementEmpty node.', () => {
				const semanticnode: SemanticNodeNull|SemanticNodeGoal = new Parser(';').parse().decorate()
				assert.ok(semanticnode instanceof SemanticNodeGoal)
				assert.strictEqual(semanticnode.serialize(), `
					<Goal source="␂ ; ␃">
						<StatementList line="1" col="1" source=";">
							<StatementEmpty line="1" col="1" source=";"/>
						</StatementList>
					</Goal>
				`.replace(/\n\t*/g, ''))
			})
		})

		context('ExpressionUnit ::= PrimitiveLiteral', () => {
			it('makes a SemanticNodeConstant node.', () => {
				assert.strictEqual(new Parser('42;').parse().decorate().serialize(), `
					<Goal source="␂ 42 ; ␃">
						<StatementList line="1" col="1" source="42 ;">
							<StatementExpression line="1" col="1" source="42 ;">
								<Constant line="1" col="1" source="42" value="42"/>
							</StatementExpression>
						</StatementList>
					</Goal>
				`.replace(/\n\t*/g, ''))
			})
		})

		context('ExpressionUnit ::= StringTemplate', () => {
			const stringTemplateSemanticNode = (goal: SemanticNodeGoal): SemanticNodeTemplate => ((goal
				.children[0] as SemanticNodeStatementList)
				.children[0] as SemanticNodeStatementExpression)
				.children[0] as SemanticNodeTemplate
			specify('head, tail.', () => {
				assert.strictEqual(stringTemplateSemanticNode(new Parser(Util.dedent(`
					'''head1{{}}tail1''';
				`)).parse().decorate() as SemanticNodeGoal).serialize(), `
					<Template line="1" col="1" source="&apos;&apos;&apos;head1{{ }}tail1&apos;&apos;&apos;">
						<Constant line="1" col="1" source="&apos;&apos;&apos;head1{{" value="head1"/>
						<Constant line="1" col="11" source="}}tail1&apos;&apos;&apos;" value="tail1"/>
					</Template>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, tail.', () => {
				assert.strictEqual(stringTemplateSemanticNode(new Parser(Util.dedent(`
					'''head1{{ '''full1''' }}tail1''';
				`)).parse().decorate() as SemanticNodeGoal).serialize(), `
					<Template line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}tail1&apos;&apos;&apos;">
						<Constant line="1" col="1" source="&apos;&apos;&apos;head1{{" value="head1"/>
						<Template line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<Constant line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;" value="full1"/>
						</Template>
						<Constant line="1" col="24" source="}}tail1&apos;&apos;&apos;" value="tail1"/>
					</Template>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, middle, tail.', () => {
				assert.strictEqual(stringTemplateSemanticNode(new Parser(Util.dedent(`
					'''head1{{ '''full1''' }}midd1{{}}tail1''';
				`)).parse().decorate() as SemanticNodeGoal).serialize(), `
					<Template line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}midd1{{ }}tail1&apos;&apos;&apos;">
						<Constant line="1" col="1" source="&apos;&apos;&apos;head1{{" value="head1"/>
						<Template line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<Constant line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;" value="full1"/>
						</Template>
						<Constant line="1" col="24" source="}}midd1{{" value="midd1"/>
						<Constant line="1" col="33" source="}}tail1&apos;&apos;&apos;" value="tail1"/>
					</Template>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, middle, expr, tail.', () => {
				assert.strictEqual(stringTemplateSemanticNode(new Parser(Util.dedent(`
					'''head1{{ '''full1''' }}midd1{{ '''full2''' }}tail1''';
				`)).parse().decorate() as SemanticNodeGoal).serialize(), `
					<Template line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos; }}tail1&apos;&apos;&apos;">
						<Constant line="1" col="1" source="&apos;&apos;&apos;head1{{" value="head1"/>
						<Template line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<Constant line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;" value="full1"/>
						</Template>
						<Constant line="1" col="24" source="}}midd1{{" value="midd1"/>
						<Template line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
							<Constant line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;" value="full2"/>
						</Template>
						<Constant line="1" col="46" source="}}tail1&apos;&apos;&apos;" value="tail1"/>
					</Template>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, middle, expr, middle, tail.', () => {
				assert.strictEqual(stringTemplateSemanticNode(new Parser(Util.dedent(`
					'''head1{{ '''full1''' }}midd1{{ '''full2''' }}midd2{{}}tail1''';
				`)).parse().decorate() as SemanticNodeGoal).serialize(), `
					<Template line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos; }}midd2{{ }}tail1&apos;&apos;&apos;">
						<Constant line="1" col="1" source="&apos;&apos;&apos;head1{{" value="head1"/>
						<Template line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<Constant line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;" value="full1"/>
						</Template>
						<Constant line="1" col="24" source="}}midd1{{" value="midd1"/>
						<Template line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
							<Constant line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;" value="full2"/>
						</Template>
						<Constant line="1" col="46" source="}}midd2{{" value="midd2"/>
						<Constant line="1" col="55" source="}}tail1&apos;&apos;&apos;" value="tail1"/>
					</Template>
				`.replace(/\n\t*/g, ''))
			})
			specify('head, expr, middle, expr, middle, expr, tail.', () => {
				assert.strictEqual(stringTemplateSemanticNode(new Parser(Util.dedent(`
					'''head1{{ '''full1''' }}midd1{{ '''full2''' }}midd2{{ '''head2{{ '''full3''' }}tail2''' }}tail1''';
				`)).parse().decorate() as SemanticNodeGoal).serialize(), `
					<Template line="1" col="1" source="&apos;&apos;&apos;head1{{ &apos;&apos;&apos;full1&apos;&apos;&apos; }}midd1{{ &apos;&apos;&apos;full2&apos;&apos;&apos; }}midd2{{ &apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos; }}tail1&apos;&apos;&apos;">
						<Constant line="1" col="1" source="&apos;&apos;&apos;head1{{" value="head1"/>
						<Template line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;">
							<Constant line="1" col="12" source="&apos;&apos;&apos;full1&apos;&apos;&apos;" value="full1"/>
						</Template>
						<Constant line="1" col="24" source="}}midd1{{" value="midd1"/>
						<Template line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;">
							<Constant line="1" col="34" source="&apos;&apos;&apos;full2&apos;&apos;&apos;" value="full2"/>
						</Template>
						<Constant line="1" col="46" source="}}midd2{{" value="midd2"/>
						<Template line="1" col="56" source="&apos;&apos;&apos;head2{{ &apos;&apos;&apos;full3&apos;&apos;&apos; }}tail2&apos;&apos;&apos;">
							<Constant line="1" col="56" source="&apos;&apos;&apos;head2{{" value="head2"/>
							<Template line="1" col="67" source="&apos;&apos;&apos;full3&apos;&apos;&apos;">
								<Constant line="1" col="67" source="&apos;&apos;&apos;full3&apos;&apos;&apos;" value="full3"/>
							</Template>
							<Constant line="1" col="79" source="}}tail2&apos;&apos;&apos;" value="tail2"/>
						</Template>
						<Constant line="1" col="90" source="}}tail1&apos;&apos;&apos;" value="tail1"/>
					</Template>
				`.replace(/\n\t*/g, ''))
			})
		})

		context('ExpressionUnit ::= "(" Expression ")"', () => {
			it('returns the inner Expression node.', () => {
				assert.strictEqual(new Parser('(2 + -3);').parse().decorate().serialize(), `
					<Goal source="␂ ( 2 + -3 ) ; ␃">
						<StatementList line="1" col="1" source="( 2 + -3 ) ;">
							<StatementExpression line="1" col="1" source="( 2 + -3 ) ;">
								<Expression line="1" col="2" source="2 + -3" operator="+">
									<Constant line="1" col="2" source="2" value="2"/>
									<Constant line="1" col="6" source="-3" value="-3"/>
								</Expression>
							</StatementExpression>
						</StatementList>
					</Goal>
				`.replace(/\n\t*/g, ''))
			})
		})

		context('ExpressionUnarySymbol ::= "-" ExpressionUnarySymbol', () => {
			it('makes a SemanticNodeExpression node with 1 child.', () => {
				assert.strictEqual(new Parser('- 42;').parse().decorate().serialize(), `
					<Goal source="␂ - 42 ; ␃">
						<StatementList line="1" col="1" source="- 42 ;">
							<StatementExpression line="1" col="1" source="- 42 ;">
								<Expression line="1" col="1" source="- 42" operator="-">
									<Constant line="1" col="3" source="42" value="42"/>
								</Expression>
							</StatementExpression>
						</StatementList>
					</Goal>
				`.replace(/\n\t*/g, ''))
			})
		})

		context('ExpressionExponential', () => {
			it('makes a SemanticNodeExpression node with 2 children.', () => {
				assert.strictEqual(new Parser('2 ^ -3;').parse().decorate().serialize(), `
					<Goal source="␂ 2 ^ -3 ; ␃">
						<StatementList line="1" col="1" source="2 ^ -3 ;">
							<StatementExpression line="1" col="1" source="2 ^ -3 ;">
								<Expression line="1" col="1" source="2 ^ -3" operator="^">
									<Constant line="1" col="1" source="2" value="2"/>
									<Constant line="1" col="5" source="-3" value="-3"/>
								</Expression>
							</StatementExpression>
						</StatementList>
					</Goal>
				`.replace(/\n\t*/g, ''))
			})
		})

		context('ExpressionMultiplicative ::= ExpressionMultiplicative "*" ExpressionExponential', () => {
			it('makes a SemanticNodeExpression node with 2 children.', () => {
				assert.strictEqual(new Parser('2 * -3;').parse().decorate().serialize(), `
					<Goal source="␂ 2 * -3 ; ␃">
						<StatementList line="1" col="1" source="2 * -3 ;">
							<StatementExpression line="1" col="1" source="2 * -3 ;">
								<Expression line="1" col="1" source="2 * -3" operator="*">
									<Constant line="1" col="1" source="2" value="2"/>
									<Constant line="1" col="5" source="-3" value="-3"/>
								</Expression>
							</StatementExpression>
						</StatementList>
					</Goal>
				`.replace(/\n\t*/g, ''))
			})
		})

		context('ExpressionAdditive ::= ExpressionAdditive "+" ExpressionMultiplicative', () => {
			it('makes a SemanticNodeExpression node with 2 children.', () => {
				assert.strictEqual(new Parser('2 + -3;').parse().decorate().serialize(), `
					<Goal source="␂ 2 + -3 ; ␃">
						<StatementList line="1" col="1" source="2 + -3 ;">
							<StatementExpression line="1" col="1" source="2 + -3 ;">
								<Expression line="1" col="1" source="2 + -3" operator="+">
									<Constant line="1" col="1" source="2" value="2"/>
									<Constant line="1" col="5" source="-3" value="-3"/>
								</Expression>
							</StatementExpression>
						</StatementList>
					</Goal>
				`.replace(/\n\t*/g, ''))
			})
		})

		context('ExpressionAdditive ::= ExpressionAdditive "-" ExpressionMultiplicative', () => {
			it('makes a SemanticNodeExpression with the `+` operator and negates the 2nd operand.', () => {
				assert.strictEqual(new Parser('2 - 3;').parse().decorate().serialize(), `
					<Goal source="␂ 2 - 3 ; ␃">
						<StatementList line="1" col="1" source="2 - 3 ;">
							<StatementExpression line="1" col="1" source="2 - 3 ;">
								<Expression line="1" col="1" source="2 - 3" operator="+">
									<Constant line="1" col="1" source="2" value="2"/>
									<Expression line="1" col="5" source="3" operator="-">
										<Constant line="1" col="5" source="3" value="3"/>
									</Expression>
								</Expression>
							</StatementExpression>
						</StatementList>
					</Goal>
				`.replace(/\n\t*/g, ''))
			})
		})

		context('DeclarationVariable, StatementAssignment', () => {
			it('makes SemanticNodeDeclaration and SemanticNodeAssignment nodes.', () => {
				assert.strictEqual(new Parser(Util.dedent(`
					let unfixed the_answer = 42;
					let \`the £ answer\` = the_answer * 10;
					the_answer = the_answer - \\z14;
				`)).parse().decorate().serialize(), `
					<Goal source="␂ let unfixed the_answer = 42 ; let \`the £ answer\` = the_answer * 10 ; the_answer = the_answer - &#x5c;z14 ; ␃">
						<StatementList line="1" col="1" source="let unfixed the_answer = 42 ; let \`the £ answer\` = the_answer * 10 ; the_answer = the_answer - &#x5c;z14 ;">
							<Declaration line="1" col="1" source="let unfixed the_answer = 42 ;" type="variable" unfixed="true">
								<Assignee line="1" col="13" source="the_answer">
									<Identifier line="1" col="13" source="the_answer" id="128"/>
								</Assignee>
								<Assigned line="1" col="26" source="42">
									<Constant line="1" col="26" source="42" value="42"/>
								</Assigned>
							</Declaration>
							<Declaration line="2" col="1" source="let \`the £ answer\` = the_answer * 10 ;" type="variable" unfixed="false">
								<Assignee line="2" col="5" source="\`the £ answer\`">
									<Identifier line="2" col="5" source="\`the £ answer\`" id="129"/>
								</Assignee>
								<Assigned line="2" col="22" source="the_answer * 10">
									<Expression line="2" col="22" source="the_answer * 10" operator="*">
										<Identifier line="2" col="22" source="the_answer" id="128"/>
										<Constant line="2" col="35" source="10" value="10"/>
									</Expression>
								</Assigned>
							</Declaration>
							<Assignment line="3" col="1" source="the_answer = the_answer - &#x5c;z14 ;">
								<Assignee line="3" col="1" source="the_answer">
									<Identifier line="3" col="1" source="the_answer" id="128"/>
								</Assignee>
								<Assigned line="3" col="14" source="the_answer - &#x5c;z14">
									<Expression line="3" col="14" source="the_answer - &#x5c;z14" operator="+">
										<Identifier line="3" col="14" source="the_answer" id="128"/>
										<Expression line="3" col="27" source="&#x5c;z14" operator="-">
											<Constant line="3" col="27" source="&#x5c;z14" value="40"/>
										</Expression>
									</Expression>
								</Assigned>
							</Assignment>
						</StatementList>
					</Goal>
				`.replace(/\n\t*/g, ''))
			})
		})
	})
})
