# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
*import B as
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_038.md:1:1:1:2
EXPECTED IMPORT ALIAS - fuzz_crash_038.md:1:2:1:8
# PROBLEMS

┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  *import B as                                                              │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_038.md:1:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `*` here.


┌───────────────────────┐
│ EXPECTED IMPORT ALIAS ├─ I was parsing an import alias, and I expected an ──┐
└┬──────────────────────┘  uppercase mod name after `as`.                  │
 │                                                                            │
 │  *import B as                                                              │
 │   ‾‾‾‾‾‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_038.md:1:2 ┘

    Import aliases rename mods, so they must start with an uppercase letter.

    For example:
        import Json.Decode as Decode

    I found `import` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.

# TOKENS
~~~zig
OpStar,KwImport,UpperIdent,KwAs,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_upper_name_after_import_as"))))
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
