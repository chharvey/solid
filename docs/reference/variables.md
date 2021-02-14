# Variables
This chapter describes declaring and accessing local variables and types.



## Variable Declaration
Values in Solid can be stored in variables and then accessed later.
In order to use a variable, it must be **declared** first,
in what we call a **variable declaration statement**.
We declare variables with the keyword `let`.
```
let my_var: str = 'Hello, world!';
```
When we declare a variable, we must assign it a value, using the **assignment operator** `=`.
In some languages, declaring a variable and at the same time assigning it a value is called **initialization**.
In Solid, this is mandatory — it’s not possible to declare a value without initializing it.
The assignment operator is an equals sign, but it does not represent equality in the mathematical sense.
It means we’re setting the value on the right-hand side to the variable on the left.
In programming terms, we say, “the variable `my_var` is **assigned the value** `'Hello, world!'`”,
or, “the value `'Hello, world!'` is **assigned *to* the variable** `my_var`”.

When we access the variable, we reference the value it’s assigned.
```
my_var; %== 'Hello, world!'
```

Variables can be declared only once within a given scope.
Attempting to declare a new varible with the same name will result in a semantic error.
```
let my_var: str = 'Hello, world!';
let my_var: str = '¡Hola, mundo!'; %> AssignmentError
```
> AssignmentError: Duplicate declaration: `my_var` is already declared.

All **basic** variable names *must* start with an uppercase or lowercase letter or an underscore.
The rest of the basic variable name may include letters, digits, and underscores.
By convention, variables are named in *snake_case*, but it’s not required.
Basic variable names cannot be any of the reserved keywords in the Solid language.
Variable names don’t have to be “basic” — we can write variables with
[Unicode characters](#unicode-identifiers) as well.


### Temporal Dead Zone
Attempting to access an undeclared variable results in a compile-time error:
```
my_other_var; %> ReferenceError
```
> ReferenceError: `my_other_var` is never declared.

Even if the variable is declared further down in the code, we get the same error.
This region of code between where a variable is accessed and where it’s declared
is called a **temporal dead zone**.
Solid does not hoist variables.
```
my_other_var;              %> ReferenceError
%%------------------------
--- TEMPORAL DEAD ZONE ---
------------------------%%
let my_other_var: str = 'Hello, programmer!';
```
> ReferenceError: `my_other_var` is used before it is declared.



## Variable Reassignment
By default, variables are **fixed** in that they cannot be reassigned.
```
let my_var: str = 'Hello, world!';
my_var = '¡Hola, mundo!';          %> AssignmentError
```
> AssignmentError: Reassignment of a fixed variable: `my_var`.

In some programming disciplines this pattern is generally encouraged, because
variables holding different values at different points in runtime could lead to unpredictability.
However, changing a variable’s value is useful in some cases, such as in loops or for storing state.

Therefore, we can declare a variables with the keywords `let unfixed`,
which allows us to assign it a new value later.
```
let unfixed my_var = 'Hello, world!';
my_var;                               %== 'Hello, world!'
my_var = '¡Hola, mundo!';
my_var;                               %== '¡Hola, mundo!'
```
The statement `my_var = '¡Hola, mundo!';` is called a **variable reassignment statement**.
An unfixed variable can be reassigned anywhere in the scope in which it’s visible.


### Pointers
Variables are pointers, which reference preexisting values.
When we access a variable, we reference the value that it points to.
```
let my_var: str = 'Hello, world!';
my_var;                            % references the string `'Hello, world!'`
```

When a variable is assigned another variable, it points to the evaluated value of that variable.
```
let a: int = 42;
let b: int = a;
a;               %== 42
b;               % also `42`
```
If that first variable is ever reassigned, the second variable will keep its pointer
to the original value, until it itself is reassigned.
```
let unfixed a: int = 42;
let unfixed b: int = a;
a;                       %== 42
b;                       % also `42`
a = 420;
a;                       % now `420`
b;                       % still `42`
```



## Unicode Identifiers
Identifiers exist to name variables, among other programming constructs.
**Basic identifiers** take the form of a single upper- or lower-case letter or underscore,
followed by any number of upper- or lower-case letters, digits, and underscores.

This constraint is quite limited and could be a pain point for non-English speakers.
Many other languages have characters in their alphabet that are not allowed by this rule.
Therefore, Solid offers a flexible mechanism for allowing identifiers with
almost any character in the Unicode character set: **Unicode identifiers**.

By wrapping the identifier name with \`back-ticks\`,
we can include non-ASCII letters.
```
let `español`: str = 'Spanish for “Spanish”';
```
In the identifier above, notice the letter `ñ`.
We can access the variable just like any other, as long as we include the name in back-tick delimiters.
```
`español`; %== 'Spanish for “Spanish”'
```

When an identifier is written with back-ticks, it must always be referred to as such,
even if it doesn’t contain “special characters”. The converse is true as well.
```
let `foo`: int = 42;
foo * 2;             %> ReferenceError
let bar: int = 420;
`bar` * 2;           %> ReferenceError
```
> ReferenceError: `foo` is never declared.
>
> ReferenceError: `` `bar` `` is never declared.

This means that the identifiers `foo` and `` `foo` `` can refer to different values.
```
let foo:   int = 42;
let `foo`: int = 420;
```

We can use Unicode identifiers to name variables with words that appear in the set of reserved keywords.
```
let let: int = 42; %> ParseError
```
> ParseError: Unexpeted token `let`.

The reserved keyword `let` cannot be used as an identifier name,
but we can turn it into a Unicode identifier to work around this limitation.
```
let `let`: int = 42;
```

With Unicode identifiers, we can insert almost any character,
including spaces and punctuation symbols.
```
let `Svaret på den ultimata frågan.`: int = 42;
`Svaret på den ultimata frågan.` / 2;           %== 21
```

Unicode identifiers may also contain no characters: The token `` `​` `` is a valid identifier.
```
let ``: str = 'What’s my name?';
```

Note that Unicode identifiers *are not strings*; they’re simply names of declared variables.
Even though Unicode identifiers are not strings, they’re tokenized the same way
that template literals are, with a few differences.

Like template literals,

- Unicode identifiers are not “cooked”, so there are no escape sequences.
	That is, an identifier named `` `1\u{24}2` `` remains exactly as typed;
	it does not become `` `1$2` `` and cannot be accessed as such.
	`` `1\u{24}2` `` and `` `1$2` `` are two different identifiers.
- Line breaks, whitespace, and non-printing characters are allowed in Unicode identifiers,
	however, keep in mind that this might result in very unreadable code.
- The character **U+0003 END OF TEXT** is not allowed in Unicode identifiers
	(or any token for that matter) and will cause a lexical error.

But unlike template literals,

- Unicode identifiers may contain the character sequences `'''` and `{{` in them,
	since they are not delimited by those characters.
- Unicode identifiers must not contain the character `` ` `` **U+0060 GRAVE ACCENT**,
	as that would end the token. There is no way to escape this character.



## Type Declaration
Types can be declared as variables that refer to types at compile-time.
They’re like regular variables, but instead of holding runtime values, they hold types.
The variable that a type is stored in is called a **type alias**.
```
type MyType = int | float;
```
By convention, type aliases are named in *PascalCase*.

Type aliases are initialized when they’re declared, and they’re always fixed — they can never be reassigned.
```
type MyType = int | float;
MyType = int;              % raises a ParseError or ReferenceError (depending on type expression)
```

Type aliases can be declared only once within a given scope.
Attempting to declare a new type alias with the same name will result in a semantic error.
This behavior is identical to that of regular variables.
```
type MyType = str;
type MyType = int; %> AssignmentError
```
> AssignmentError: Duplicate declaration: `MyType` is already declared.


Also like variables, type aliases can create temporal dead zones.
Solid does not hoist type aliases.
*(NOTE: This may change in future versions.)*
```
let my_var: MyType = 'Hello, programmer!'; %> ReferenceError
%%------------------------
--- TEMPORAL DEAD ZONE ---
------------------------%%
type MyType = str;
```
> ReferenceError: `MyType` is used before it is declared.
