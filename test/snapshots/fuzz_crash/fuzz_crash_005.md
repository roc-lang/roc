# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_005.md:1:1:1:5
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  modu                                                                      │
 │  ‾‾‾‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_005.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `modu` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.

# TOKENS
~~~zig
LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
