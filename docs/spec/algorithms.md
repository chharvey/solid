# Abstract Algorithms
This chapter lists and defines common abstract algorithms used throughout this specification.



## VarCheck
Performs the definite assignment piece during semantic analysis.



## TypeCheck
Performs the type-checking piece during semantic analysis.



## ToBoolean
Returns an associated [boolean value](./data-types#boolean), `true` or `false`, with a Solid Language Value.
```
Boolean ToBoolean(Object value) :=
	1. *If* `TypeOf(value)` is `Null`:
		1. *Return:* `false`.
	2. *If* `TypeOf(value)` is `Boolean`:
		1. *Return:* `value`.
	3. *Return:* `true`.
```



## Identical
Compares two objects and returns whether they are the exact same object.
```
Boolean Identical(Object a, Object b) :=
	1. *If* `a` is the value `null` and `b` is the value `null`:
		1. *Return:* `true`.
	2. *If* `a` is the value `false` *and* `b` is the value `false`:
		1. *Return:* `true`.
	3. *If* `a` is the value `true` *and* `b` is the value `true`:
		1. *Return:* `true`.
	4. *If* `a` is of type `Integer` *and* `b` is of type `Integer`:
		1. If `a` and `b` have the same bitwise encoding:
			1. *Return:* `true`.
	5. *If* `a` is of type `Float` *and* `b` is of type `Float`:
		1. If `a` and `b` have the same bitwise encoding:
			1. *Return:* `true`.
	// 6. *If* `a` is of type `String` *and* `b` is of type `String`:
	// 	1. If `a` and `b` are exactly the same sequence of code units
	// 		(same length and same code units at corresponding indices):
	// 		1. *Return:* `true`.
	// 7. *If* `a` and `b` are the same object:
	// 	1. *Return:* `true`.
	8. Return `false`.
```



## Equal
Compares two objects and returns whether they are considered “equal” by some definition.
```
Boolean Equal(Object a, Object b) :=
	1. *If* `Identical(a, b)` is `true`:
		1. *Return:* `true`.
	2. *If* `a` is of type `Number` *and* `b` is of type `Number`:
		1. *If* `a` is of type `Float` *or* `b` is of type `Float`:
			1. *Return:* `Equal(Float(a), Float(b))`.
	3. *If* `a` is of type `Float` *and* `b` is of type `Float`:
		1. If `a` is `0.0` *and* `b` is `-0.0`:
			1. *Return:* `true`.
		2. If `a` is `-0.0` *and* `b` is `0.0`:
			1. *Return:* `true`.
	// 3. TODO: custom equality operators
	4. Return `false`.
```



## PerformBinaryArithmetic
Performs a binary arithmetic operation.
```
Number! PerformBinaryArithmetic(Text op, Number operand0, Number operand1) :=
	1. *If* `op` is `EXP`:
		1. *Let* `result` be the power, `operand0 ^ operand1`,
			obtained by raising `operand0` (the base) to `operand1` (the exponent).
		2. *Return:* `result`.
	2. *Else If* `op` is `MUL`:
		1. *Let* `result` be the product, `operand0 * operand1`,
			obtained by multiplying `operand0` (the multiplicand) by `operand1` (the multiplier).
		2. *Return:* `result`.
	3. *Else If* `op` is `DIV`:
		1. *Let* `result` be the quotient, `operand0 / operand1`,
			obtained by dividing `operand0` (the dividend) by `operand1` (the divisor).
		2. *Return:* `result`.
	4. *Else If* `op` is `ADD`:
		1. *Let* `result` be the sum, `operand0 + operand1`,
			obtained by adding `operand0` (the augend) to `operand1` (the addend).
		2. *Return:* `result`.
	5. *Throw:* a new TypeError01.
```



## PerformBinaryCompare
Performs a binary comparison operation.
```
Boolean! PerformBinaryCompare(Text op, Number operand0, Number operand1) :=
	1. *If* `op` is `LT`:
		1. *If* `operand0` is strictly less than `operand1`:
			1. *Return:* `true`.
		2. *Return:* `false`.
	2. *Else If* `op` is `GT`:
		1. *If* `operand1` is strictly less than `operand0`:
			1. *Return:* `true`.
		2. *Return:* `false`.
	3. *Else If* `op` is `LE`:
		1. *If* `operand0` is equal to `operand1`:
			1. *Return:* `true`.
		2. *If* `operand0` is strictly less than `operand1`:
			1. *Return:* `true`.
		3. *Return:* `false`.
	4. *Else If* `op` is `GE`:
		1. *If* `operand0` is equal to `operand1`:
			1. *Return:* `true`.
		2. *If* `operand1` is strictly less than `operand0`:
			1. *Return:* `true`.
		3. *Return:* `false`.
	5. *Throw:* a new TypeError01.
```
