import * as assert from 'assert'

import {
	SolidConfig,
	CONFIG_DEFAULT,
} from '../src/core/';
import {
	assert_arrayLength,
} from './assert-helpers'
import {
	ParserSolid as Parser,
} from '../src/parser/';
import {
	Decorator,
	AST,
} from '../src/validator/'



export function constantFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): AST.ASTNodeConstant {
	const statement: AST.ASTNodeStatementExpression = statementExpressionFromSource(src, config);
	assert_arrayLength(statement.children, 1, 'semantic statement should have 1 child')
	const expression: AST.ASTNodeExpression = statement.children[0];
	assert.ok(expression instanceof AST.ASTNodeConstant);
	return expression
}
export function variableFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): AST.ASTNodeVariable {
	const statement: AST.ASTNodeStatementExpression = statementExpressionFromSource(src, config);
	assert_arrayLength(statement.children, 1, 'semantic statement should have 1 child');
	const expression: AST.ASTNodeExpression = statement.children[0];
	assert.ok(expression instanceof AST.ASTNodeVariable);
	return expression;
}
export function operationFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): AST.ASTNodeOperation {
	const statement: AST.ASTNodeStatementExpression = statementExpressionFromSource(src, config);
	assert_arrayLength(statement.children, 1, 'semantic statement should have 1 child')
	const expression: AST.ASTNodeExpression = statement.children[0];
	assert.ok(expression instanceof AST.ASTNodeOperation);
	return expression
}
export function statementExpressionFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): AST.ASTNodeStatementExpression {
	const goal: AST.ASTNodeGoal = goalFromSource(src, config);
	assert_arrayLength(goal.children, 1, 'semantic goal should have 1 child')
	const statement: AST.ASTNodeStatement = goal.children[0] as AST.ASTNodeStatement;
	assert.ok(statement instanceof AST.ASTNodeStatementExpression);
	return statement
}
export function variableDeclarationFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): AST.ASTNodeDeclarationVariable {
	const goal: AST.ASTNodeGoal = goalFromSource(src, config);
	assert_arrayLength(goal.children, 1, 'semantic goal should have 1 child');
	const statement: AST.ASTNodeStatement = goal.children[0] as AST.ASTNodeStatement;
	assert.ok(statement instanceof AST.ASTNodeDeclarationVariable);
	return statement;
}
export function typeDeclarationFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): AST.ASTNodeDeclarationType {
	const goal: AST.ASTNodeGoal = goalFromSource(src, config);
	assert_arrayLength(goal.children, 1, 'semantic goal should have 1 child');
	const statement: AST.ASTNodeStatement = goal.children[0] as AST.ASTNodeStatement;
	assert.ok(statement instanceof AST.ASTNodeDeclarationType);
	return statement;
}
export function assignmentFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): AST.ASTNodeAssignment {
	const goal: AST.ASTNodeGoal = goalFromSource(src, config);
	assert_arrayLength(goal.children, 1, 'semantic goal should have 1 child');
	const statement: AST.ASTNodeStatement = goal.children[0] as AST.ASTNodeStatement;
	assert.ok(statement instanceof AST.ASTNodeAssignment);
	return statement;
}
export function goalFromSource(src: string, config: SolidConfig = CONFIG_DEFAULT): AST.ASTNodeGoal {
	return Decorator.decorate(new Parser(src, config).parse());
}
