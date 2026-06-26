# META
~~~ini
description=formatter instability with leading newline
type=file
~~~
# SOURCE
~~~roc

b:r
~~~
# EXPECTED
DECLARATION HAS NO VALUE - fuzz_crash_079.md:2:1:2:4
# PROBLEMS

┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  b:r                                                                       │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_079.md:2:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "b")
			(ty-var (raw "r")))))
~~~
# FORMATTED
~~~roc

b : r
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "b"))
		(e-anno-only)
		(annotation
			(ty-rigid-var (name "r")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "r")))
	(expressions
		(expr (type "r"))))
~~~
