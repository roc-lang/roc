# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}|0
as s|||0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_062.md:1:8:1:9
PARSE ERROR - fuzz_crash_062.md:1:9:1:10
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: exposed_item_unexpected_token ─────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  module[}|0                                                                │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_062.md:1:8 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: header_expected_close_square ──────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  module[}|0                                                                │
 │          ‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_062.md:1:9 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
KwModule,OpenSquare,CloseCurly,OpBar,Int,
KwAs,LowerIdent,OpBar,OpBar,OpBar,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "header_expected_close_square"))
	(statements))
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
