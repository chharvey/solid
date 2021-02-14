# Expressions and Operators
This chapter describes operator syntax, semantics, and precedence in expressions and types.



## Value Operators


### Summary Table
In the table below, the horizontal ellipsis character `…` represents an allowed expression.

<table>
	<thead>
		<tr>
			<th>Precedence<br/><small>(1 is highest)</small></th>
			<th>Operator Name</th>
			<th>Arity &amp; Position</th>
			<th>Grouping</th>
			<th>Symbols</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<th>1</th>
			<td>Grouping</td>
			<td>unary wrap</td>
			<td>(n/a)</td>
			<td><code>( … )</code></td>
		</tr>
		<tr>
			<th rowspan="4">2</th>
			<td>Logical Negation</td>
			<td rowspan="4">unary prefix</td>
			<td rowspan="4">right-to-left</td>
			<td><code>! …</code></td>
		</tr>
		<tr>
			<td>Emptiness</td>
			<td><code>? …</code></td>
		</tr>
		<tr>
			<td>Mathematical Affirmation</td>
			<td><code>+ …</code></td>
		</tr>
		<tr>
			<td>Mathematical Negation</td>
			<td><code>- …</code></td>
		</tr>
		<tr>
			<th>3</th>
			<td>Exponentiation</td>
			<td>binary infix</td>
			<td>right-to-left</td>
			<td><code>… ^ …</code></td>
		</tr>
		<tr>
			<th rowspan="2">4</th>
			<td>Multiplication</td>
			<td rowspan="2">binary infix</td>
			<td rowspan="2">left-to-right</td>
			<td><code>… * …</code></td>
		</tr>
		<tr>
			<td>Division</td>
			<td><code>… / …</code></td>
		</tr>
		<tr>
			<th rowspan="2">5</th>
			<td>Addition</td>
			<td rowspan="2">binary infix</td>
			<td rowspan="2">left-to-right</td>
			<td><code>… + …</code></td>
		</tr>
		<tr>
			<td>Subtraction</td>
			<td><code>… - …</code></td>
		</tr>
		<tr>
			<th rowspan="6">6</th>
			<td>Less Than</td>
			<td rowspan="6">binary infix</td>
			<td rowspan="6">left-to-right</td>
			<td><code>… < …</code></td>
		</tr>
		<tr>
			<td>Greater Than</td>
			<td><code>… > …</code></td>
		</tr>
		<tr>
			<td>Less Than or Equal To</td>
			<td><code>… <= …</code></td>
		</tr>
		<tr>
			<td>Greater Than or Equal To</td>
			<td><code>… >= …</code></td>
		</tr>
		<tr>
			<td>Not Less Than</td>
			<td><code>… !< …</code></td>
		</tr>
		<tr>
			<td>Not Greater Than</td>
			<td><code>… !> …</code></td>
		</tr>
		<tr>
			<th rowspan="4">7</th>
			<td>Identity</td>
			<td rowspan="4">binary infix</td>
			<td rowspan="4">left-to-right</td>
			<td><code>… is …</code></td>
		</tr>
		<tr>
			<td>Non-Identity</td>
			<td><code>… isnt …</code></td>
		</tr>
		<tr>
			<td>Equality</td>
			<td><code>… == …</code></td>
		</tr>
		<tr>
			<td>Non-Equality</td>
			<td><code>… != …</code></td>
		</tr>
		<tr>
			<th rowspan="2">8</th>
			<td>Conjunction</td>
			<td rowspan="2">binary infix</td>
			<td rowspan="2">left-to-right</td>
			<td><code>… && …</code></td>
		</tr>
		<tr>
			<td>Alternative Denial</td>
			<td><code>… !& …</code></td>
		</tr>
		<tr>
			<th rowspan="2">9</th>
			<td>Disjunction</td>
			<td rowspan="2">binary infix</td>
			<td rowspan="2">left-to-right</td>
			<td><code>… || …</code></td>
		</tr>
		<tr>
			<td>Joint Denial</td>
			<td><code>… !| …</code></td>
		</tr>
		<tr>
			<th>10</th>
			<td>Conditional</td>
			<td>ternary infix</td>
			<td>n/a</td>
			<td><code>if … then … else …</code></td>
		</tr>
	</tbody>
</table>

The **Grouping** column indicates the direction of application of *grouping symbols*,
where none are present.
For example, exponentiation is grouped right-to-left, so the ambiguous expression
`a ^ b ^ c` is interpreted as `a ^ (b ^ c)`, and not as `(a ^ b) ^ c`.
Operations that have the same precedence, like multiplication and division,
are grouped together:
`a / b * c` is interpreted left-to-right as `(a / b) * c`.

Sometimes you might see the above notion expressed as the term “associativity”,
but in this documentation we use “grouping” instead to differentiate it from
the mathematical definition of “associativity”, which has a related meaning.
Some binary operations are truly **associative**, which means that any grouping interpretation,
whether left-to-right or right-to-left, would yield the same mathematical result.
Addition is an example of this. Whether we group *left-to-right* `(a + b) + c`
or *right-to-left* `a + (b + c)`, the output remains the same.
Operations that are associative are indicated as so in their respective sections below.


### Logical Negation, Emptiness
```
`!` <unknown>
`?` <unknown>
```
The **logical negation** operator, `!`, returns the opposite boolean value of the operand’s “logical value”.

A value’s “logical value” is the boolean value that most closely corresponds to that value.
A value is said to be “falsy” if its “logical value” is `false`. Otherwise the value is said to be “truthy”.

| “Falsy” Values | “Truthy” Values |
| -------------- | --------------- |
| `null`         |                 |
| `false`        | `true`          |
|                | all integers (including `0`)
|                | all floats   (including `0.0` and `-0.0`)
|                | all strings  (including `''`)
|                | any other value

The operator `!` logically negates the “logical value” of the operand.
If the value is “falsy”, `true` is produced; otherwise `false` is produced.

The **emptiness operator**, `?`, determines whether a value is considered “empty”.
A value is “empty” if it’s “falsy”, if it’s a zero numeric value (`0`, `0.0`, or `-0.0`), or if it’s an empty string.
In future versions its semantics will be expanded to collections (such as arrays and sets, etc.).


### Mathematical Affirmation, Mathematical Negation
```
`+` <int | float>
`-` <int | float>
```
The **mathematical affirmation** operator, `+`, and
the **mathematical negation** operator, `-`,
are valid only on number types.
The affirmation is a no-op (the number itself is produced),
and the negation computes the additive inverse, or “negation”, of the number.
Any integer base can be used.

These operators can be chained, and when done so, are grouped right-to-left.
For example, `-+-8` is equivalent to `-(+(-8))`.

```
let int_p = 512;
let int_n = -\x200;

+int_p; %== 512
+int_n; %== -512

-int_p; %== -512
-int_n; %== 512
```

Recognize that number tokens can begin with **U+002B PLUS SIGN** or **U+002D HYPHEN-MINUS**,
even if they’re prefixed with a radix.
For example, `-\x200` is lexed as a single token, and not two tokens `-` and `\x200`.
The same is true for `+\x200`.
Even though these tokens’ values are the same as the computed values of
the expressions `-(\x200)` and `+(\x200)`,
this is important to mention because it could affect how we write
[additive expressions](#parsing-additive-expressions).


### Exponentiation
```
<int | float> `^` <int | float>
```
The **exponentiation** operator is valid only on number types.
It produces the result of raising the left-hand operand to the power of the right-hand operand.
Integer bases as well as integers and floats can be mixed.

```
3 ^ 2;    %== 9
2 ^ \b11; %== 8
```

Expressions involving exponentiation can be imprecise.
For example, the mathematical value of *3<sup>-2</sup>* is one-ninth, approximately 0.111111,
which is not an integer. Since integers are truncated, `3 ^ -2` will produce `0`.

Exponentiation is *grouped right-to-left*.
This means that where grouping is ambiguous, the expression is evaluated from right to left.
For example, `a ^ b ^ c` is equivalent to `a ^ (b ^ c)` and not `(a ^ b) ^ c`.
This is consistent with mathematical notation,
where *a<sup>b<sup>c</sup></sup>* is interpreted as *a<sup>(b<sup>c</sup>)</sup>*.

#### Exponentiation: Order of Operations
In mathematics, exponents are applied before negation (which is multiplication).
However, in Solid, [mathematical negation](#mathematical-affirmation-mathematical-negation)
is a unary operator, which is stronger than any binary operator.
**Mathematical negation is not considered multiplication**,
even if it indeed produces the same mathematical result of multiplying by -1.
Therefore, we can end up with confusing syntax such as this:
```
-3 ^ 2
```
While *mathematically*, *&minus;3<sup>2</sup>* is equivalent to *&minus;1&middot;3<sup>2</sup>*,
producing *&minus;9*, the Solid expression `-3 ^ 2`, is *not equivalent*.
Mathematical negation is stronger than exponentiation, so Solid will compute `-3`
first as a unary operation (or, in this case, as a single token),
and then raise that value to the power of `2`, producing `9`.
Writing such an ambiguous syntax could cause developers to scratch their heads
wondering why `-3 ^ 2` is `9`.

As a recommendation,
though expressions like `-3 ^ 2` are well-formed and will produce a numerical result,
it’s best practice to place parentheses where they’ll reduce ambiguity and improve readability.
So if raising `-3` to the power of `2` is intended, the expression is best written
```
(-3) ^ 2
```
On the other hand, if the intention is actually to raise `3` to the power of `2` first,
and then negate, the expression should be written `-(3 ^ 2)` or `-1 * 3 ^ 2`.


### Multiplicative
```
<int | float> `*` <int | float>
<int | float> `/` <int | float>
```
The **multiplication** operator, `*`, and
the **division** operator, `/`,
are valid only on number types.
They produce the respective mathematical product and quotient of the operands.
Integer bases as well as integers and floats can be mixed.

Multiplication is **associative**, which means the following expressions produce the same result,
for any numbers `‹a›`, `‹b›`, and `‹c›`:
```
‹a› * ‹b› * ‹c›
(‹a› * ‹b›) * ‹c›
‹a› * (‹b› * ‹c›)
```

Multiplication and division perform the standard arithmetic operations,
keeping in mind that the result of division `/` on integers are truncated,
and division by `0` will result in an error.
```
\o12 / \q11; % produces `2`
3 / 2;       % produces `1`, since 1.5 gets truncated
4 / 0;       % runtime error
```


### Additive
```
<int | float> `+` <int | float>
<int | float> `-` <int | float>
```
The **addition** operator, `+`, and
the **subtraction** operator, `-`,
are valid only on number types.
They produce the respective mathematical sum and difference of the operands.
Integer bases as well as integers and floats can be mixed.

Addition is **associative**, which means the following expressions produce the same result,
for any numbers `‹a›`, `‹b›`, and `‹c›`:
```
‹a› + ‹b› + ‹c›
(‹a› + ‹b›) + ‹c›
‹a› + (‹b› + ‹c›)
```

Addition and subtraction perform the standard arithmetic operations,
keeping in mind that integer overflow is possible
when going beyond the maximum/minimum integer values.

#### Parsing Additive Expressions
[Previously in this chapter](#mathematical-affirmation-mathematical-negation)
we saw that number tokens can begin with **U+002B PLUS SIGN** or **U+002D HYPHEN-MINUS**.
Since those characters are the same as the additive operator symbols,
this could affect how additive expressions are parsed.

```
3+1
```
In the code above, our intention was to write the sum of `3` and `1`.
The lexer will however produce two number tokens: `3` and `+1`,
since it thinks `+1` is a single token.
This will lead the parser to fail, since a number token cannot follow another number token
in the formal grammar.

To fix the error, we must use whitespace indicate token boundaries.
```
3 + 1
```
Now the lexer produces three tokens: a number `3`, a punctuator `+`, and a number `1`.
The parser receives these tokens and produces the correct expression.
(Note that the code `3+ 1` would be sufficient, but perhaps not as readable.)


### Comparative
```
<int | float> `<`  <int | float>
<int | float> `>`  <int | float>
<int | float> `<=` <int | float>
<int | float> `>=` <int | float>
<int | float> `!<` <int | float>
<int | float> `!>` <int | float>
```
The comparative operators,

- **less than** `<`
- **greater than** `>`
- **less than or equal to** `<=`
- **greater than or equal to** `>=`
- **not less than** `!<`
- **not greater than** `!>`

compare number types in the usual sense. The result is a boolean value.
Integer bases as well as integers and floats can be mixed.

In numerical uses, `!<` is equivalent to `>=`, and `!>` is equivalent to `<=`.
In general, however, this might not hold for future operator overloads.
For instance, if the relational operators were overloaded to mean “subset” for sets,
then `a !< b` (“`a` is not a strict subset of `b`”) does not necessarily mean
that `a >= b` (“`a` is a superset of ”).


### Equality
```
<unknown> `is`   <unknown>
<unknown> `isnt` <unknown>
<unknown> `==`   <unknown>
<unknown> `!=`   <unknown>
```
These operators compare two values.
Any type of operands are valid. The result is a boolean value.
Integer bases as well as integers and floats can be mixed.

The **identity** operator `is` determines whether two operands are the exactly same object.
It produces whether the bitwise representations of both operands are the same,
and whether they exist at the same location in memory.
Primitive values such as `null`, boolean values, number values, and string values
only exist once, so any two of “the same” values will be identical.
For other types, identity and equality might not necessarily be the same:
objects that are considered equal might not be identical.

Per the [IEEE-754-2019] specification, the floating-point values `0.0` and `-0.0` do not have
the same bitwise representation; therefore the expression `0.0 is -0.0` evaluates to `false`.
Floatint-point values and integer values are never identical, so the expression `42 is 42.0` is also `false`.

The **equality** operator `==` determines whether two operands are considered “equal” by some definition,
based on the type of the operands.
For `null` and boolean values, equality is one in the same with identity.
For number values, equality is determined by mathematical quantity, thus `0.0 == -0.0` is `true`.
Mixed number types of the same quantity are equal, so `42 == 42.0` is also `true`.

The non-identity operator `isnt` is simply the logical negation of `is`, and
the non-equality operator `!=` is simply the logical negation of `==`.

All four of these operators are **commutative**, meaning the order of operands does not change the resulting value.
```
‹a› is   ‹b›; % same as `‹b› is   ‹a›`
‹a› isnt ‹b›; % same as `‹b› isnt ‹a›`
‹a› ==   ‹b›; % same as `‹b› ==   ‹a›`
‹a› !=   ‹b›; % same as `‹b› !=   ‹a›`
```
Remember: Expressions are always evaluated from left to right, so side-effects could still be observed.


### Conjunctive
```
<unknown> `&&` <unknown>
<unknown> `!&` <unknown>
```
The **logical conjunction** operator `&&` (”and”) produces the left-hand operand if it is “falsy”;
otherwise it produces the right-hand operand. The operands may be of any type.

The `&&` operator short-circuits, in that evaluation of the right-hand operand does not take place
if it does not need to. If the left-hand operand of an `&&` operation is “falsy”,
then that operand is produced and the right-hand operand is not evaluated.
Short-circuiting can speed up runtime computation if the “simpler” expression is on the left.

Logical conjunction is **associative**, which means the following expressions produce the same result,
for any values `‹a›`, `‹b›`, and `‹c›`:
```
‹a› && ‹b› && ‹c›
(‹a› && ‹b›) && ‹c›
‹a› && (‹b› && ‹c›)
```

The **logical alternative denial** operator `!&` (“nand”) is the logical negation of conjunction.
```
a !& b; % sugar for `!(a && b)`
```


### Disjunctive
```
<unknown> `||` <unknown>
<unknown> `!|` <unknown>
```
The **logical disjunction** operator `||` (“or”) produces the left-hand operand if it is “truthy”;
otherwise it produces the right-hand operand. The operands may be of any type.

The `||` operator short-circuits, in that evaluation of the right-hand operand does not take place
if it does not need to. If the left-hand operand of an `||` operation is “truthy”,
then that operand is produced and the right-hand operand is not evaluated.
Short-circuiting can speed up runtime computation if the “simpler” expression is on the left.

Logical disjunction is **associative**, which means the following expressions produce the same result,
for any values `‹a›`, `‹b›`, and `‹c›`:
```
‹a› || ‹b› || ‹c›
(‹a› || ‹b›) || ‹c›
‹a› || (‹b› || ‹c›)
```

The **logical joint denial** operator `!|` (“nor”) is the logical negation of disjunction.
```
a !| b; % sugar for `!(a || b)`
```


### Conditional
```
`if` <bool> `then` <unknown> `else` <unknown>
```
The conditional operator is a ternary operator that takes three operand expressions:
a condition, a consequent, and an alternative.
The condition must be a boolean expression, and the consequent and alternative may be of any type.
The consequent and alternative expressions are sometimes called “branches”:
the “then branch” and the “else branch” respectively.

The result of the conditional expression is either the consequent or the alterantive,
depending on the value of the condition.
If the condition is true, the consequent is produced, otherwise the alternative is produced.

Evaluation of a conditional expression is short-circuited: Only the produced branch is evaluated.
For example, if the condition evalutes to `false`, then only the alternative is evaluated and then produced;
the consequent does not even get evaluated.
This is meaningful when evaluation of an expression produces side-effects, such as a routine call.
Because one of the branches is not evaluated, its side-effects (if any) will not occur.



## Type Operators


### Summary Table
In the table below, the horizontal ellipsis character `…` represents an allowed expression.
<table>
	<thead>
		<tr>
			<th>Precedence<br/><small>(1 is highest)</small></th>
			<th>Operator Name</th>
			<th>Arity &amp; Position</th>
			<th>Grouping</th>
			<th>Symbols</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<th>1</th>
			<td>Grouping</td>
			<td>unary wrap</td>
			<td>(n/a)</td>
			<td><code>( … )</code></td>
		</tr>
		<tr>
			<th>2</th>
			<td>Nullish</td>
			<td>unary postfix</td>
			<td>left-to-right</td>
			<td><code>… !</code></td>
		</tr>
		<tr>
			<th>3</th>
			<td>Intersection</td>
			<td>binary infix</td>
			<td>left-to-right</td>
			<td><code>… & …</code></td>
		</tr>
		<tr>
			<th>4</th>
			<td>Union</td>
			<td>binary infix</td>
			<td>left-to-right</td>
			<td><code>… | …</code></td>
		</tr>
	</tbody>
</table>


### Nullish
```
<Type> `!`
```
The **nullish** operator creates a [union](#union) of the operand and the `null` type.
```
type T = int!; % equivalent to `type T = int | null;`
```


### Intersection
```
<Type> `&` <Type>
```
The **intersection** operator creates a strict combination of the operands.
```
type T = [foo: bool] & [bar: int];
let v: T = [
	foo = false,
	bar = 42,
];
```


### Union
```
<Type> `|` <Type>
```
The **union** operator creates a type that is either one operand, or the other, or some combination of both.
```
type T = bool | int;
let unfixed v: T = false;
v = 42;
```
