# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 b:S
.R
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_018.md:1:1:1:2
UNEXPECTED STATEMENT - fuzz_crash_018.md:2:1:2:3
UNDECLARED TYPE - fuzz_crash_018.md:1:5:1:6
DECLARATION HAS NO VALUE - fuzz_crash_018.md:1:3:1:6
# PROBLEMS
── ✗ unexpected statement ──────────────────────────────── fuzz_crash_018.md:1:1

I was parsing a statement, and this token cannot start a statement here.

0 b:S
^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found 0 here.

── ✗ unexpected statement ──────────────────────────────── fuzz_crash_018.md:2:1

I was parsing a statement, and this token cannot start a statement here.

.R
^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found .R here.
Names that start with uppercase letters are used for tags, type names, and
mod names in Roc.

── ✗ undeclared type ───────────────────────────────────── fuzz_crash_018.md:1:5

The type S is not declared in this scope.

0 b:S
    ^

── ● declaration has no value ──────────────────────────── fuzz_crash_018.md:1:3

This declaration has a type annotation but no implementation.

0 b:S
  ^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

# TOKENS
~~~zig
Int,LowerIdent,OpColon,UpperIdent,
DotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "b")
			(ty (name "S")))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
b : S
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "b"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
