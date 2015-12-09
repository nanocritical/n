# The N Programming Language v0.1

# Introduction

N is a general-purpose language designed with systems programming in mind,
suitable for both low-level and high-level programming. N is designed to
achieve comparable performance to C, with increased safety, greater
conciseness and ease of use.

N can be used in a freestanding environment, althoug parts of the standard
library assume the presence of a heap and may therefore not be available in a
resource-constrained environment.


# Source code representation

Source code is Unicode text encoded in UTF-8, not canonicalized. Most of the
code is actually formed of printable ASCII characters only, with the exception
of string literals which can contain arbitrary code points.


# Literals

Literals are text representations of values. Literals are always given a type
by the compiler, derived from an explicitly type annotation or through type
inference, and the interpretation of a literal is dependent on that type.


# Types

N supports several kinds of types: references, slices, structures,
enumerations, unions, functions and interfaces.

## Void

Void is the return type of functions returning no value. It generally does not
need to appear explicitly in code.

## Bool

Takes either the value true or false.

## Numeric types

All the numeric types defined in this section are distinct and require explicit
conversions.

The following architecture-independent sized numeric types are defined:

	U8
	U16
	U32
	U64

	I8
	I16
	I32
	I64

	Float32
	Float64

	Complex32
	Complex64

The following integer types are also defined in n.builtins. They are platform
dependent, and their precision is between 32 and 64 bits.

	Uint
	Int

They should be large enough to allow indexing the largest datasets that may be
handled on the target platform. For example, they should be 64-bit on 32-bit
Linux (where support for large files over 4GB in size is common). They might
however be 32-bit on a small embedded device with very limited memory and
storage.

Uint and Int are the preferred integer types in most code. On all desktop and
server target platforms, Uint and Int are strictly larger than 32 bits (that
usually means 64 bit, or the mantissa of a double -- 53 bit).

We stress that developers should be able to think of Uint and Int as "large
enough" for most intent and purposes and most platforms should simply define
them to be 64 bit integer.

On all integer types (e.g. U32, Uint), standard arithmetic operations do not
overflow or underflow. It is a runtime error if they do.

The unsigned sized integers types are also equipped with additional, explicit
overflow arithmetic operations (U8, U16, U32, U64).


## Reference types

A reference of type t points to the location of a value of type t in memory.
There are multiple kinds of references, built from a set of possible attribute:

	Local, Counted Local, Counted Global
	Const, Mutable, Mercurial
	Optional

A reference can be local or global. A local reference is known by the compiler
to be accessed concurrently by a single execution thread, at most. A global
reference may be in use concurrently by multiple execution threads. A global
reference cannot be dereferenced directly: a local reference may be derived
from it first. Note that the terms local or global do not refer to the lexical
scope of the reference: it is defined with respect to the set of threads that
may access the referenced value concurrently, over a given section of the
program, as observable at compile time.

A reference can be constant, mutable, or mercurial. A constant reference allows
reading the value it refers to -- this is the default. A mutable reference also
allows mutating the value it refers to. A mercurial reference also allows
mercurially mutating the value it refers to. Mutation and mercurial mutation
are defined later in this document.

A reference can be non-optional or optional. A non-optional reference always
refers to a valid value -- this is the default. An optional reference does not
always point to a valid value, in which case it is nil.

	*t	Local reference to t
	!t	Mutable local reference to t
	#t	Mercurial local reference to t
	?*t	Optional local reference to t
	?!t	Optional mutable local reference to t
	?#t	Optional mercurial local reference to t

	@t	Counted reference to t
	@!t	Mutable counted reference to t
	@#t	Mercurial counted reference to t
	?@t	Optional counted reference to t
	?@!t	Optional mutable counted reference to t
	?@#t	Optional mercurial counted reference to t

	&t	Global counted reference to t
	&!t	Mutable global counted reference to t
	&#t	Mercurial global counted reference to t
	?&t	Optional global counted reference to t
	?&!t	Optional mutable global counted reference to t
	?&#t	Optional mercurial global counted reference to t

The compiler performs lifetime and escape analysis of references and enforces
correctness rules relative to the use of references.

Local references are uncounted and offer the compile-time guarantee that the
value they point to is valid for at least as long as the reference exists.

References counting is handled automatically by the compiler.

### Memory ownernship

A local reference does not own the memory it points to, but it does offer the
static guarantee that the location pointed to is valid.

A counted reference, whether local or global, is the owner of the memory it
points to.

To use an uncounted local reference as a local counted reference, it must be
bridged.

	let refc_x = Unsafe_acquire ref_x
	...
	Unsafe_release refc_x

`Unsafe_release` checks: (i) that the count is `1`, ensuring that the bridged
reference hasn't escaped; (ii) it doesn't free the memory, as `ref_x` must
remain valid.  If the count is not `1`, the program aborts.

### Lifetime

An anonymous variable (as in `_ = expr`) has the same lifetime as a named
variable.


### Reference compatibility

A number of conversions between references are allowed. The rules below apply
recursively:

	*t	->	?*t
	!t	->	*t, ?!t
	#t	->	!t, ?#t
	?t	->	*t (if statically known to be non-nil)
	?!t	->	!t (if statically known to be non-nil)
	?#t	->	#t (if statically known to be non-nil)

Additionally, local counted references can be used as (uncounted) local
references. The compiler inserts an implicit increment of the reference count
before the use, and it inserts an implicit decrement of the reference
count after the last possible use of the derived (uncounted) local reference.


## Slice types

A slice is a reference to a contiguous area of memory that contains a known
number of elements of a given type. Slices do not own the memory they point to.
A slice `s` has an element count, `s.Count`.

	let s = {true false true}
	assert s.Count == 3

Slices also have a capacity, which extends past the end of the slice. The 

	0 .. Count	Initialized memory
	Count .. Cap	Zeroed memory

Through a slice, local references can be obtained to the elements in the memory
area referenced by the slice, using indexes `0` to `s.Count-1`. It is illegal
to index an element outside of this range: it will result in a compilation
error if possible, or a runtime error otherwise.

	s.[0]
	s.[s.Count - 1]

Slices can be const or mutable. Through a const slice, only const references to
the elements can be obtained. Through a mutable slice, mercurial references can
be obtained.

	-- Types added for emphasis:
	let s:[]Uint = {0 1 2}:[]Uint
	fmt.Prn s
	let p:*Uint = s.[0]
	fmt.Prn p
	let q:#Uint = s![0]
	q# = 42
	fmt.Prn s

`Slice T` and `Mutable_slice T` are defined in `lib/n/builtins/builtins.n`.

### Slice compatibility

Mutable slices can be used as const slices:

	[!]t	->	[]t

### Slicing

Slicing a slice `s` creates a new slice that refers to a contiguous subset of
the memory region referred to by `s`. To create a sub-slice, a bounds
expression is used as index:

	s.[beg .. end] -- const sub-slice
	s![beg .. end] -- mutable sub-slice

Unlike a range expression, a bounds expression can ommit its beginning (lower
bound, inclusive), its end (upper bound, exclusive), or both.  If `beg` is
ommitted, `0` is assumed. If `end` is ommitted, `s.Count` is assumed.

	s.[..]		s.[0 .. s.Count]
	s.[.. 1]	s.[0 .. 1]
	s.[1 ..]	s.[1 .. s.Count]

Slicing can extend past the end `s.Count` of a slice, up to its capacity
`s.Cap`.

It is illegal to specify bounds outside of the range `0 .. s.Cap` in a slicing
expression: doing so will result in a compilation error if possible, or a
runtime error otherwise.

Both a slice and its sub-slice refer to the same memory, and both allow
modifying its content.

	let s = {0 1 2}:[]Uint
	let a:[]Uint = s.[1 .. 3] -- refers to elements {1 2}
	let b:[!]Uint = s.[1 .. 3]
	b![0]# = 42

	fmt.Prn s -- Prints {0 42 2}


### Aliasing

Two distinct slices of the same type `t` may point to the same areas of memory.

However, if `u` is distinct from `t`, `[]u` and `[]t` may not alias.


## String type

`String` is a special-purpose type, similar to `[]U8`, but designed to
facilitate common text string operations. Like `[]U8`, strings are immutable,
have reference semantics, and do not own the memory they point to.

Like uncounted local references and slices, strings have a local lifetime. They
are cheap to copy or pass as arguments. The compiler performs escape analysis
on strings and enforces their lifetime properties.

As with slices, strings have a counterpart type which is mutable, has value
semantics, and owns its memory: `Stringbuf`.

`String` is defined in `lib/n/builtins/string.n`.


### String encoding

No encoding is assumed.

#### Commentary

While the standard library offers facilities to handle Unicode encoding
(especially UTF-8), it is not possible in general to give each string valu in a
program a particular encoding. Some strings do not have a well-defined encoding
at all, some strings contain encoding errors but must be preserved as-is (maybe
they're just being copied around, or handled as opaque byte sequences).


## Array types

An array is a sequence of elements of a single type. The length of the array is
part of its type.

	[32]U8
	[1000]*Float64

UNIMPLEMENTED


## Struct types


### Destructor

Dtor must be idempotent. It must be possible to call a destructor multiple
times on the same object. In particular, it must be possible to call a
destructor on a zero value.


## Enum types

### Implementation notes

Implementations may use a bitmask representation of the enumeration or union
tags to implement matching of enum values, without however affecting the ABI.


## Union types


## Function types


## Interface types


## Type inference


# Declaration and scopes


## Label scopes (try)


## Blank identifier


## Predeclared identifier


## Exported identifier


## Type declaration


## Enum declaration


## Union declaration


## Environment declaration

N does not have global variables. There is however a facility to manage
globally accessible state, with a mechanism that bears ressemblance to
dynamically scoped variables in other languages.

An environment is an instance of an intf. The instance is declared with a
globalenv declaration.

	-- logging.n
	globalenv Log:`Log

	intf `Log =
		met# Error msg:String
	
	struct Log =

	Store met# Error msg:String
		Eprn msg

	-- server.n
	import hostname

	fun foo within logging.Log
		Log#Error "bad"

A module that wants to use an environment throughout can also use a toplevel
within declaration:

	within logging.Log

	fun Print
		Log#Error "bad"

A toplevel within declaration is equivalent to adding a within declaration to
all functions and methods defined within the file, with the exception of
prototypes in interface definitions.

Without a within declaration, a global environment cannot be used.

A global environment form a stack. A different implementation of an environment
can be pushed on the stack to temporarily change its behavior.

A localenv is the same concept, but each execution threads sees a separate
instance of the environment.


## Function declaration

### Named arguments

### Vararg list of arguments


## Method declaration


## Interface declaration


# Generics


# Example declarations

## Example declarations in interfaces



# Expressions


## Primary expressions

## Accessor expressions

## Range expression

## Index and slice expressions

## Compile time isa test

## Calls

## Operators

## Operator precedence

### Unary and postfix operators disambiguation

When parsing an expression, the parser uses the following rule to disambiguate
the tokens that can be both unary operators or binary operators, listed below:

	+ - ov+ ov- ! # *

At the beginning of an expression, it is a prefix unary operator. At the end of
an expression, it is a postfix unary operator.

In the middle of an expression, the spaces surrounding the operator determine
how the operator must be interpreted. If there are spaces on both sides, or on
neither sides, then it is a binary operator. If there are spaces before but
none after, it is a prefix unary operator. If there are spaces after but none
before, it is a postfix unary operator.

	-1		Prefix unary
	ptr!		Post unary
	(foo p)?	Post unary
	{-1 0}  	Prefix unary
	{p?}		Postfix unary
	a+1		Binary
	a + 1		Binary
	foo a +1	Prefix unary
	foo a+ 1  	Error, '+' is not a valid postfix operator

## Arithmetic operators

## Overflow arithmetic operators

## Bitwise operators

## Comparison operators

## Logical operators

## Associativity and precedence

N forces the use of parentheses to clarify the evaluation order of expressions
in all but the most common and widely-understood cases.

	Expression		Interpretation
	a * b + 1 - 0		((a * b) + 1) - 0
	a/b * c			(a / b) * c
	(a | b) + c		As-is, parentheses are required
	a + b == c * d		(a + b) == (c * d)
	a | b == c + d		(a | b) == (c + d)
	(a ov<< b) bw<<	c	As-is, parentheses are required

## Builtin functions

### Sizeof, Alignof and Offsetof

### Assert, pre, post, and invariant

### Likely and Unlikely

	fun Likely expr:Bool = Bool
	fun Unlikely expr:Bool = Bool

Likely indicates that an expression is likely to evaluate to true. Unlikely
indicates that an expression is unlikely to evaluate to true. These functions
have otherwise no effect and return their argument unmodified.

These indications provide the compiler with branch prediction information.

	if Likely foobar
		return 0

By style convention, Likely and Unlikely should be placed on the outside of an
expression.

	if Likely <- not foobar
	if not Likely foobar

While equivalent to the first, the second form may be confusing to careless
reader.


## Order of evaluation


# Statements

## Value declaration

## Let block

## Assignment

## If statement

## While statement

## For statement

## Foreach statement

## Match statement

## Return statement

## Break statement

## Continue statement

## Try catch statement

## Throw statement

## Except statement

## Drop and Fatal functions


# Modules

## Source file naming

## Import declaration

## Export declaration

## Entry point

# Compilation dependencies

## Compilation stage

Each module belongs to a compilation stage. Module `b` belongs to the
compilation stage of module `a` if both `a` has an inline dependency to `b`,
and `b` has an inline dependency to `a`. This allows some modules to have
circular dependencies among themselves, while still enforcing namespace
separation and export restrictions.

For instance, the `n.builtins`, `n.io` and `n.fmt` modules belong to the same
compilation stage (along with other modules), as they are all defined in terms
of one another. However, `n.net.http` belongs to a different compilation stage
entirely.

Dependencies among compilation stages form a tree (by construction, stages
cannot have circular dependencies with one another).

A compilation stage can be built once all the compilation stages it depends on
are built.  This allows certain compilation stages from being built
independently, and in parallel, of one another.


# Errors

N uses error codes returned by functions. The language provides extensive
support to handle errors in a consistent, systematic, and concise manner.

Errors are important values and cannot be easily ignored: they must be
explicitly handled. An error can be returned to the caller (`return`,
`except`), it can branch the execution flow (`throw`, `except`), it can lead to
the program aborting (`fatal`), or it can be dropped explicitly (`drop`).

The compiler instrements error handling code automatically, to facilitate the
tracking and analysis of any error generated. The creation, forwarding, and
dropping of an error can be traced, monitored, and logged.

## Returning errors

Functions that return Error among multiple return values must obey an important
rule regarding these values. In a return tuple of the form:

	x1:t1, ..., xn:tn, err:Error, y1:s1, ..., yn:sn

with `t1` ... `tn` arbitrary types, and the types `s1` ... `sn` all different
from Error.

When `err != OK`, the elements of the tuple after `err`, starting with `y1`,
must be set to their zero-value. For instance, this code is illegal: 

	return 1, INVAL, 2

However, it is legal to return

	return 1, INVAL, {}

Many functions return an error value in first position, as they are unable to
return any other useful information in case of an error.

The intent of this rule is (i) to standardize the use of errors in multiple
return values, and (ii) to make the use of `except` predictable and safe.

## except

The `except` keyword can be used to conditionally return from a function, when
an error is not OK.

	except = foobar

Is equivalent to:

	let err = foobar
	if err != OK
		return err

`except` can also be used in the destruct pattern of a `let` or `var` statement:

	let except, f = fs.Open filename

`except` can only be used in functions with single return values, or in
functions that return multiple values with a single Error in first position. In
the latter case, the values after the first are all set to their zero value,
even if they are named return values and were set to a non-zero value at some
point before the `except`.


## try .. catch ..

When the error handling for a series of statements can be centralized, a `try ..
catch ..` block can be used.

	try
		let except, f = fs.Open filename flags={CREATE|WRITE|TRUNCATE}
		_, except = f!Write "A=".Bytes
		_, except = f!Write a
		_, except = f!Write "B=".Bytes
		_, except = f!Write b
	catch err
		Log#Error "failed to save state in '%s': %s" filename err

In some cases, it is useful to handle multiple sources of errors separately,
and multiple catch clauses can be used, each identified by a label.

	try
		let except SRC, f = fs.Open filename1={READ|WRITE}
		let content, except = f!Read buf

		let except DST, g = fs.Open filename2 flags={CREATE|WRITE|TRUNCATE}
		_, except DST = g!Write content
		except DST = g!Datasync

		except SRC = f!Truncate 0

	catch SRC err
		Log#Error "failed to read/clear state '%s': %s" filename1 err
	catch DST err
		Log#Error "failed to write state to '%s': %s" filename2 err

In a `try .. catch ..`, `except` can be used as usual, but instead of returning
from the function, the execution jumps to a catch clause. If the catch clause
has a label, the label must be used as first argument to except.

	except = ...
	except LABEL = ...

In a `try .. catch ..`, a throw statement can be used to jump (unconditionally)
to a catch clause. If the catch clause has a label, the label must be used as
first argument in the throw statement.

	throw err
	throw LABEL err

Inside the `try` block, `except` and `throw` are compiled to gotos. Despite the
name, `try .. catch ..` are not related to exceptions.



# Semantic constraints

## Principle

Semantic constraints are annotations: they do not affect optimizations and code
generation, and it must be possible to successfully compile the program after
eliding all constraints and disabling constraints validation.

## Syntax

### Atoms

### Asserts

### Pre conditions

### Post conditions

### Invariants

### Claims

## Automated proofs

## Manual proofs

## Assisted proofs
