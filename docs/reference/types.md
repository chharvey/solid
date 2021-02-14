# Types
This chapter describes types of values in the Solid Language.

Solid is a strongly-typed language, meaning that types of values are determined at compile-time.
A strong type system can help prevent many runtime errors.

Solid Language Types are described in the [formal specification](../spec/data-types.md#solid-language-types).
This reference takes a more informative approach.



## Never
The Never type, `never`, is the Bottom type in the type hierarchy —
it contains no values and is a subtype of every other type.

The Never type is used to describe the return type of functions that never return,
or the type of an expression that never evaluates.

The Never type is most commonly a result of a type operation that produces the Bottom type,
for example, the intersection of two disjoint types.

There are no values assignble to the Never type.
Currently, there are no expressions assignable to it either, but
future versions of Solid will support expressions of type Never.

Since Never is a subtype of every other type, expressions of type Never
will be accepted anywhere.



## Null
The Null type, `null`, has exactly one value, also called `null`.
The meaning of the `null` value is not specified, but it’s most commonly used as a placeholder
when no other value is appropriate.

The Null type has no supertypes other than [Object](#object) and [Unknown](#unknown).



## Boolean
The Boolean type, `bool`, has two logical values, called `true` and `false`.
These values are used for binary states.



## Integer
Integers, type `int`, are whole numbers, their negatives, and zero.

Integers are written as a series of digits, such as `0123`,
optionally preceded by a negative sign (`-0123`).
`0` and `-0` are identical.

Integers may be written in five other bases in addition to the default base 10:
bases 2, 4, 8, 16, and 36.

Raw Input  | Base | Mathematical Value (in decimal)
---------  | ---- | -------------------------------
`42`       | 10   | 42
`\d42`     | 10   | 42
`\b101010` |  2   | 42
`\q222`    |  4   | 42
`\o52`     |  8   | 42
`\x2a`     | 16   | 42
`\z16`     | 36   | 42

We may also include the underscore as a numeric separator symbol, to visually group digits.
```
\b1_0011_1000_1000;
\q103_2020;
\o11_610;
\d5_000;
\x13_88;
\z3_u_w;
```
The numeric separator cannot appear at the beginning or end of an integer,
nor can it appear consecutively.

Integers can be added, subtracted, and multiplied like normal numbers.
However, when dividing integers, if getting a non-integer value, we will truncate the decimal
(round towards zero). Dividing by zero is an error.
In all operations on integers, bases can be mixed.
```
3 / 2;        %== 1
-3 / 2;       %== -1
\b110 * \q12; %== 36
```



## Float
Floating-point numbers, type `float`, are decimals, which offer finer precision for numerical data than integers do.
(In computers, there are no irrational (non-fractional) numbers, but we approximate them well.)

Floating numbers cannot be declared in any base other than decimal (10).
Exactly one decimal point must be present in a float literal.
```
0.25;
0.5;     % the leading whole number part is required
1.;      % but we can omit the trailing fractional part
0 . 5;   %> Error
```

We can write floats in “scientific-like notation”, such as `6.022e23`.
This represents *6.022 &times; 10<sup>23</sup>*.
This notation consts of the following parts:
- the whole part (an integer)
- a decimal point (`.`)
- the fractional part (the decimal places)
- the symbol `e`
- the exponent part (an integer)

We say “scientific-like notation” because it’s technically not scientific notation:
The coefficient need not be between 1 and 10. `-42.0e-1` is a valid floating-point value.

Floating-point values can be operated on just as integers can.
There is no truncation for division, but dividing by zero still raises an error.

Float values are considered “contageous” in that they “infect” any integers they are operated with.
For example, in the expression `1 + 2.3`, the integer `1` is *coerced* into the float `1.0`,
giving the same result as `1.0 + 2.3`.
If an expression contains *any* float value anywhere, then
*all* the integers in the expression are coerced into floats.



## String
The String type is denoted `str`.



## Object
The Object type, `obj`, is the type of all values, that is, every value is assignable to the Object type.



## Unknown
The Unknown type, `unknown`, is the Top type in the type hierarchy —
it contains every value and expression, and is a supertype of every other type.

The Unknown type is used to describe a value or expression about which nothing is known.
Therefore, the compiler will not assume it has any properties or valid in some operations.

Every value and expression is assignble to the Unknown type.
Currently, since there are no valueless expressions,
the Unknown type is equivalent to the [Object](#object) type.
However, future versions of Solid will support expressions assignable to Unknown
that are not assignable to Object.
