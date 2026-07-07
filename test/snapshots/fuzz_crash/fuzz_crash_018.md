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

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  0 b:S                                                                     │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_018.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `0` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  .R                                                                        │
 │  ‾‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_018.md:2:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.R` here.
    Names that start with uppercase letters are used for tags, type names, and
    module names in Roc.


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `S` is not declared in this scope. ─────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  0 b:S                                                                     │
 │      ‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_018.md:1:5 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  0 b:S                                                                     │
 │    ‾‾‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_018.md:1:3 ┘

    Add a value body here, or put hosted functions in a platform type module so
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
	(type-module)
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
		(e-anno-only)
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
