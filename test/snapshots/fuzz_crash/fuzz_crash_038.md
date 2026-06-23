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
PARSE ERROR - fuzz_crash_038.md:1:1:1:2
PARSE ERROR - fuzz_crash_038.md:1:2:1:8
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  *import B as                                                              │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_038.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: ───────────────────────────────────┐
└┬────────────┘  expected_upper_name_after_import_as                          │
 │                                                                            │
 │  *import B as                                                              │
 │   ‾‾‾‾‾‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_038.md:1:2 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
OpStar,KwImport,UpperIdent,KwAs,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
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
