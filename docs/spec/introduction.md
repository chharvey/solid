# Introduction
This chapter introduces and provides an overview of the Solid programming language.



## What is Solid?
Solid is a general-purpose, multi-paradigm programming language that combines
the power of a static compiler,
the efficiency of asynchronous execution, and
the logic of mathematical concepts.

The Solid Project includes this Solid Language Specification,
a Solid language compiler, and a collection of built-in objects (the “core library”) in Solid.



## Scope
This specification defines the Solid programming language and compiler,
developed by [Chris Harvey](https://github.com/chharvey).
Solid started off as a personal side project, and it is by no means an industry standard.
However, plans for Solid’s growth are ambitious.

Solid is a new programming language inspired by similar languages such as
[Java](https://docs.oracle.com/javase/specs/) and [TypeScript](https://www.typescriptlang.org/).
It was developed to address some of the pitfalls of the most popular languages, as well as
to incorporate more mathematically-focused concepts and programming best practices.

While this document is a rigorous and comprehensive specification of the Solid programming language,
an [informal language guide] (link pending) for developers programming with Solid is available.
There, readers will encounter a more practical approach,
including an introductory “Hello, world” tutorial.

The Solid Project’s source code, including this specification, is stored in the official
Solid repository, hosted on [GitHub](https://github.com/chharvey/solid/).
The Solid Project is open-sourced and licensed by the
[AGPL 3.0](https://choosealicense.com/licenses/agpl-3.0/) license,
the text of which can be found under the project’s root directory.
Contributions to this project are welcome in the form of GitHub Issues and Pull Requests.
All contributors are expected to adhere strictly to this license and
to read the contribution guidelines thoroughly before contributing.

The Solid language and compiler aim to be developed together and incrementally,
following the [Semantic Versioning](https://semver.org/) specification to track version numbers.

The **zeroth** edition of this specification is in pre-release, and not fully stable.
New features are added by minor version number, with similar features grouped by theme.
A detailed changelog can be found on GitHub.



## Design Goals
The Solid language is developed with the following design goals in mind.

1. **Easy to Learn:** Solid source code should feel familiar, with common
	syntax structures and semantics.
	Developers shouldn’t need in-depth knowledge of the Solid language or its applications
	to write good programs.

1. **Easy to Read:** One with sufficient programming background should be able to look at
	a snippet of Solid code and understand what it does.
	Clear and explicit code is prioritized over quirky shorthand syntax,
	and more understandable APIs are preferred over unreadable abbreviations or symbols.
	Solid code should be self-explanatory.

1. **Easy to Write:** Constructs and conventions should be consistent and predictable.
	Developers should be able to write Solid code easily without having to memorize
	tricky hacks or continually “look it up” in reference manuals.

1. **Fun to Use:** Developing in Solid should be fun and not feel like a chore.
	Solid should open itself to third-party tools like IDEs and text editor extensions
	to assist developers in their work.
	Compile-time and run-time errors shouldn’t feel overwhelming or discouraging.
	Installation, setup, and starting a new project should be effort-free.
	Solid should work “right out of the box”.

1. **Configuration over Convention:** Solid should be highly configurable, so that developers
	can pick and choose the features they want, and discard the ones they don’t need.
	(Non-default) configuration and imports should be explicit.
	Convention should only be prioritized across the entire Solid ecosystem,
	and when it is, it should be well documented.

1. **Community-Driven:** The direction in which Solid evolves should be driven
	by a wide and diverse community.
	The Solid Project will be open-sourced, and will be written *in Solid itself*,
	so that Solid users can directly contribute to its development.



## Attributes of Solid
The following attributes describe the Solid language as a whole.

- Solid is [General-Purpose](#general-purpose).
- Solid is [High-Level](#high-level).
- Solid is [Multi-Paradigm](#multi-paradigm).
- Solid is [Compiled](#compiled).
- Solid is [Strongly Typed](#strongly-typed).
- Solid is [Immutable](#immutable).
- Solid is [Asynchronous](#asynchronous).


### General-Purpose
Solid is a **general-purpose** language. It may be used in a variety of environments
to solve a variety of problems.
The Solid core library contains a minimal number of general-use functions,
which serve as building blocks that developers may combine to build higher-order
or more specific applications.
Solid aims to cater to as wide a community as possible,
and not to become a language of just a niche group.


### High-Level
Solid is a **high-level** language, allowing developers to focus on *what* they want to do,
and not necessarily worry about *how* they want the machine do it.
Solid is designed with a focus on clean and recognizable code,
and not “clever tricks” that can be used to optimize runtime performance.
(After all, that’s the compiler’s job.)

Low-level operations such as bitwise operators, pointers, and memory management are obscured.
The goal isn’t to be restrictive, but instead to encourage developers
to work with larger structures and think in abstract terms.


### Multi-Paradigm
Solid combines multiple paradigms of programming style, roughly consisting of:

- 40% [Object-Oriented](#object-oriented)
- 30% [Functional](#functional)
- 20% [Procedural](#procedural)
- 10% [Ontology](#ontology)

Overall, this makes Solid about 60% imperative and 40% declarative.
Of course, the percentages above are very subjective,
and programmers can use any language styles they prefer.

#### Object-Oriented
Solid takes most of its ideas from [object-oriented](https://en.wikipedia.org/wiki/Object-oriented_programming)
languages such as [C++](https://isocpp.org/) and [Smalltalk](http://www.smalltalk.org/).
At the highest levels of a program are classes and objects, which have states and behaviors
and communicate to each other through methods and interfaces.
The class hierarchy and polymorphism are used to define more fine-grained properties of objects.
Customized behavior, for example, is first and foremost achieved via inheritance and subclassing
instead of object/data manipulation.

#### Functional
Solid contains concepts in Lambda Calculus and [functional](https://en.wikipedia.org/wiki/Functional_programming)
languages such as [Common Lisp](https://lisp-lang.org/) and [Haskell](https://www.haskell.org/).
Functions are first-class citizens just like other data types, and are pure by default:
they cannot modify externals or have side effects, and they must return a value
(unless explicitly defined otherwise).
Error-handling is functional:
errors are not typically thrown and caught like they are in imperative languages;
rather, they’re returned in the output of the function.
Solid exhibits abstract concepts such as referential transparency, recursion,
function composition, function currying, and eta-conversion.

#### Procedural
Solid allows a [procedural](https://en.wikipedia.org/wiki/Procedural_programming) writing style
when developers need to be more dictatorial in their approach.
Method definitions and statement blocks contain several procedures to be executed
in a particular order (whether it be sequential or concurrent).
Control flow statements offer more fine-grained control of the order of execution.

#### Ontology
Solid’s type system is significantly expressive.
Types are static and declarative, and relations between types are expressed
[ontologically](https://en.wikipedia.org/wiki/Ontology_language),
allowing developers to be explicit in the kinds of data they work with.



### Compiled
Solid is a **compiled** language, rather than an **interpreted** one.
This means source code is transformed into different code that a machine runs.
The compilation process does take time (depending on how big the program is),
but the time is worth the performance benefit.
After a Solid program is compiled once, it can be run many times very quickly.
This contrasts with an interpreted language, which must be interpreted every time it runs.


### Strongly Typed
Solid has a robust **manifest** type system in which types are declared by hand.
Solid’s compiler can infer types that are not declared manually,
but type inference takes the strictest route.
A strong and explicit type system prevents common runtime errors and allows developers
to think about the shape of their data while writing code.

Solid’s type system is **structural**, as opposed to **nominal**,
meaning types are compared based on their shape (the properties they have),
rather than on their names (where they exist in the type hierarchy).
This compositional typing allows functions to be flexible with data being passed around.
Structural typing can be overridden by nominal typing
in cases where developers want to ensure that only the type hierarchy is considered.


### Immutable
Solid encourages thinking with **immutable** rather than **stateful** data structures.
By default, variables and parameters cannot be reassigned or mutated,
and methods on a class are not allowed to reassign or mutate the fields of that class.
The approach is to use lightweight objects that have short lifespans;
if properties need to change, objects can be killed and replaced with new objects
with updated properties, a practice that can help reduce memory leaks.
This default immutable behavior can be overridden
by declaring objects as unfixed and/or mutable.


### Asynchronous
Solid is almost completely **asynchronous**.
All functions (except class constructors) run in a non-blocking manner,
so the runtime is never unnecessarily idly waiting by for other processes to run.
Therefore, working with functions is easier in Solid than in languages that offer both
blocking and non-blocking.
Since everything is run asynchronously,
their results will always have to be **awaited** before being used.
Solid’s clean syntax makes this easy to do,
making for a more consistent and streamlined workflow.
