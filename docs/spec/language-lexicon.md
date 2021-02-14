# Solid Language: Lexicon
This chapter defines the lexical composition of the Solid programming language.



## Solid Source Code
Solid source text (Solid code) is expressed using characters from the
[Unicode](https://www.unicode.org/) character set.
Solid source text is a sequence of Unicode code points,
values ranging from U+0000 to U+10FFFF (including surrogate code points).
Not all code points are permitted everywhere;
the next section explicitly defines these permissions.

When stored and transmitted, Solid source text should be encoded and decoded via the
[UTF-8](https://tools.ietf.org/html/rfc3629) transmission format.

Solid programs are often stored in text files, which, for editing convenience, are organized into lines.
These lines are typically separated by some combination of the characters
**U+000D CARRIAGE RETURN (CR)** and **U+000A LINE FEED (LF)**.
For example, it is common for Windows systems to represent a newline as a CR–LF pair,
whereas on Unix-based systems the representation is a single LF.


### Line Normalization
To simplify the tasks of external applications and to delineate file bounds,
the Solid compiler **normalizes** all line breaks in a source file on input, before parsing.
This line normalization consists of three steps:

1. Prepend the file with a **U+0002 START OF TEXT** (“SOT”) character followed by an LF.
2. Replace any two-character sequence CR–LF and any single CR not followed by an LF
	with a single LF character.
3. Append the file with an LF followed by a **U+0003 END OF TEXT** (“EOT”) character.

Note that if the source file already contains an LF at the end,
the last step will result in an extra LF character preceding the EOT.
This does not matter, however, since additional whitespace does not affect parsing.
Even though Solid is a [whitespace-independent language](#whitespace),
line break normalization is important to the compilation process,
during which line and column numbers of any invalid source input might be reported.


### Abstract Operation: UTF16Encoding
The abstract operation **UTF16Encoding** encodes a code point using the UTF-16 encoding algorithm.
```
Sequence<RealNumber> UTF16Encoding(RealNumber n) :=
	1. *If* `n` is less than 0 or greater than \x10ffff:
		1. Throw a ParseError.
	2. *If* `n` is less than or equal to \xffff:
		1. *Return:* [n].
	3. *Let* `d` be `n - \x10000`.
	4. *Let* `cu1` be the integer quotient of `d / \x400`.
	5. *Let* `cu2` be the integer remainder of `d / \x400`.
	6. *Return:* [cu1 + \xd800, cu2 + \xdc00].
```


### Static Semantics: CodePoint
The **CodePoint** of a character is the integer index of its placement in the Unicode character set.
A code point is *not* a code unit. A code point is simply Unicode’s index of a character,
whereas a code unit is the [UTF-16-encoded](#abstract-operation-utf16encoding) value of that code point.
```
CodePoint([#x00-#x10ffff]) -> RealNumber
	:=; // TO BE DESCRIBED
```


### Static Semantics: TokenWorth
The [**TokenWorth**](./grammar/tokenworth.ebnf) attribute grammar assigns a
[Solid Specification Value](./data-types#solid-specification-types)
to a Token produced by the Tokenizer piece of the Solid compiler.


### Static Semantics: DigitCount
The **DigitCount** attribute grammar gives the [number](./data-types.md#real-integer-numbers) of
numeric (non-separator) digits in a digit sequence.
```
DigitCount(DigitSequenceDec :::= [0-9]) -> RealNumber
	:= 1;
DigitCount(DigitSequenceDec :::= DigitSequenceDec "_"? [0-9]) -> RealNumber
	:= DigitCount(DigitSequenceDec) + DigitCount([0-9]);
```



## Token Formation
After line break normalization,
the source text of a Solid file is converted into a sequence of input elements, called tokens.
The source text is scanned from left to right, repeatedly taking the longest possible
sequence of code points as the next token.
The lexical structure of Solid describes what sequence of characters form valid tokens,
which form the lowest-level building blocks of the language.

There are a small number of token types, each of which have a specific purpose.

1. [File Bounds](#file-bounds)
1. [Whitespace](#whitespace)
1. [Punctuators](#punctuators)
1. [Keywords](#keywords)
1. [Identifiers](#identifiers)
1. [Numbers](#numbers)
1. [String Literals](#string-literals)
1. [Template Literals](#template-literals)
1. [Comments](#comments)


### File Bounds
File bound tokens are tokens that consist of exactly 1 character:
either **U+0002 START OF TEXT**, or **U+0003 END OF TEXT**.


### Whitespace
Whitespace tokens consist of combinations of the following characters.
Any consecutive sequence of these characters is put into a single whitespace token.

Code Point | Name                 | Block       | Category
---------- | -------------------- | ----------- | ---------------------
U+0020     | SPACE                | Basic Latin | Separator, Space [Zs]
U+0009     | CHARACTER TABULATION | Basic Latin | Other, Control [Cc]
U+000A     | LINE FEED (LF)       | Basic Latin | Other, Control [Cc]

Solid is whitespace-independent.
This means that a programmer should be able to add more whitespace where whitespace already exists,
and remove any amount of whitespace from existing whitespace (but not remove all of it)
without affecting the syntax or semantics of the program.
Whitespace tokens are not sent to the parser for syntactic analysis.

The Solid lexical grammar does not currently recognize the following characters as whitespace:

Code Point | Name                      | Block                       | Category
---------- | ------------------------- | --------------------------- | -------------------------
U+000B     | LINE TABULATION           | Basic Latin                 | Other, Control [Cc]
U+000C     | FORM FEED (FF)            | Basic Latin                 | Other, Control [Cc]
U+000D     | CARRIAGE RETURN (CR)      | Basic Latin                 | Other, Control [Cc]
U+00A0     | NO-BREAK SPACE            | Latin-1 Supplement          | Separator, Space [Zs]
U+1680     | OGHAM SPACE MARK          | Latin-1 Supplement          | Separator, Space [Zs]
U+2000     | EN QUAD                   | Latin-1 Supplement          | Separator, Space [Zs]
U+2001     | EM QUAD                   | Latin-1 Supplement          | Separator, Space [Zs]
U+2002     | EN SPACE                  | Latin-1 Supplement          | Separator, Space [Zs]
U+2003     | EM SPACE                  | Latin-1 Supplement          | Separator, Space [Zs]
U+2004     | THREE-PER-EM SPACE        | Latin-1 Supplement          | Separator, Space [Zs]
U+2005     | FOUR-PER-EM SPACE         | Latin-1 Supplement          | Separator, Space [Zs]
U+2006     | SIX-PER-EM SPACE          | Latin-1 Supplement          | Separator, Space [Zs]
U+2007     | FIGURE SPACE              | Latin-1 Supplement          | Separator, Space [Zs]
U+2008     | PUNCTUATION SPACE         | Latin-1 Supplement          | Separator, Space [Zs]
U+2009     | THIN SPACE                | Latin-1 Supplement          | Separator, Space [Zs]
U+200A     | HAIR SPACE                | Latin-1 Supplement          | Separator, Space [Zs]
U+200B     | ZERO WIDTH SPACE          | General Punctuation         | Other, Format [Cf]
U+2028     | LINE SEPARATOR            | General Punctuation         | Separator, Line [Zl]
U+2029     | PARAGRAPH SEPARATOR       | General Punctuation         | Separator, Paragraph [Zp]
U+202F     | NARROW NO-BREAK SPACE     | General Punctuation         | Separator, Space [Zs]
U+205F     | MEDIUM MATHEMATICAL SPACE | General Punctuation         | Separator, Space [Zs]
U+3000     | IDEOGRAPHIC SPACE         | CJK Symbols and Punctuation | Separator, Space [Zs]


### Punctuators
Punctuators are non-alphanumeric characters in the ASCII character set, or spans of such characters,
that add to the semantics of the Solid language.
Some punctuators are operators, which perform computations on values, and
some punctuators are delimiters, which separate certain code constructs from each other or group them together.

#### TokenWorth (Punctuators)
The Token Worth of a Punctuator token is the unique [real integer number](./data-types.md#real-integer-numbers) ID
that distinguishes the punctuator from the other punctuators.
Token Worth quantities for punctuators are predetermined by the lexical grammar and are independent of any instance program.
Token Worth quantities for punctuators are integers ranging from *0* to *127* (inclusive).

This specification uses the term “ID” to refer to the identification,
as not to be confused with [Identifier tokens](#identifiers).


### Keywords
Keywords are sequences of alphanumeric characters reserved by the Solid language
and enumerated in the lexical grammar.
Keywords convey certain semantics to the compiler and to programmers.

#### TokenWorth (Keywords)
The Token Worth of a Keyword token is the unique [real integer number](./data-types.md#real-integer-numbers) ID
that distinguishes the keyword from the other keywords.
Token Worth quantities for keywords are predetermined by the lexical grammar and are independent of any instance program.
Token Worth quantities for keywords are integers ranging from *128* to *255* (inclusive).


### Identifiers
Identifiers are sequences of alphanumeric characters that do not match the [Keyword](#keywords) production.
Identifiers are author-defined and point to values in a program.

Lexically, identifiers have two forms: basic identifiers and Unicode identifiers.
Basic identifiers must start with an alphabetic character or an underscore,
and thereafter may contain more alphanumeric characters or underscores.
Unicode identifiers are enclosed in back-ticks (`` `…` `` **U+0060 GRAVE ACCENT**),
and may contain any number of characters from the Unicode character set.

#### TokenWorth (Identifiers)
The Token Worth of an Identifier token is the unique [real integer number](./data-types.md#real-integer-numbers) ID
that distinguishes the identifier from other identifiers within a given program.
Token Worth quantities for identifiers are integers strictly greater than *255*.


### Numbers
Numbers are literal constants that represent numeric mathematical values.
Currently, only positive and negative (and zero) integers are supported.

#### TokenWorth (Numbers)
The Token Worth of a number token is the [real number](./data-types.md#realnumber) that the token represents.

There is a many-to-one relationship between tokens and Token Worth quantities.
For example, both the tokens containing `0042` and `+42`
have the same Token Worth: the integer *42*.


### String Literals
String tokens are sequences of Unicode characters enclosed in delimiters.
Strings are snippets of textual data.

#### TokenWorth (Strings)
The Token Worth of a String token is a [sequence](./data-types.md#sequence)
of UTF-16-encoded code units computed by the various parts of the token.

A **code unit** is a [real integer number](./data-types.md#real-integer-numbers)
representing one character or part of a character in a string.
In the [UTF-16 encoding](#abstract-operation-utf16encoding),
characters in the Unicode character set are represented by either one or two code units.

There is a many-to-one relationship between tokens and Token Worth quantities.
For example, both the tokens containing `'ABC'` and `'\u{41}\u{42}\u{43}'`
have the same Token Worth: the sequence of code units *[65, 66, 67]*.


### Template Literals
Template tokens are almost exactly like string tokens, except that
they use different delimiters, and their “cooked” values are computed differently.
Template literal tokens can be combined together in
specific ways determined by the formal syntactic grammar.

#### TokenWorth (Templates)
The Token Worth of a Template token is the analogue of the Token Worth of a String token.


### Comments
Comments are tokens of arbitrary text,
mainly used to add human-readable language to code
or to provide other types of annotations.
Comment tokens are not sent to the Solid parser.

#### Line Comments
Line comments begin with `%` (**U+0025 PERCENT SIGN**).
The compiler will ignore all source text starting from `%` and onward,
up to and including the next line break (**U+000A LINE FEED (LF)**).

#### Multiline Comments
Multiline comments are contained in the delimiters `%% %%`,
and may contain line breaks. Nesting is not possible.

##### Documentation Comments
Documentation comments are multiline comments, but they use the delimiters `%%% %%`.
The extra percent sign may signal to a separate parser that
the comment documents the code structure that follows it.
