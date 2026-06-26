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
PARSE ERROR - fuzz_crash_018.md:1:1:1:2
PARSE ERROR - fuzz_crash_018.md:2:1:2:3
UNDECLARED TYPE - fuzz_crash_018.md:1:5:1:6
DECLARATION HAS NO VALUE - fuzz_crash_018.md:1:3:1:6
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  0 b:S                                                                     │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_018.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  .R                                                                        │
 │  ‾‾                                                                        │
 └───────────────────────────────────────────────────── fuzz_crash_018.md:2:1 ┘

    This is an unexpected parsing error. Please check your syntax.


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
