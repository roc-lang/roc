# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}('
)
~~~
# EXPECTED
UNCLOSED SINGLE QUOTE - fuzz_crash_039.md:1:10:1:11
PARSE ERROR - fuzz_crash_039.md:1:8:1:9
PARSE ERROR - fuzz_crash_039.md:1:9:1:10
# PROBLEMS

┌───────────────────────┐
│ UNCLOSED SINGLE QUOTE ├─ This single-quoted literal is missing a closing ───┐
└┬──────────────────────┘  quote.                                             │
 │                                                                            │
 │  module[}('                                                                │
 │           ‾                                                                │
 └──────────────────────────────────────────────────── fuzz_crash_039.md:1:10 ┘



┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: exposed_item_unexpected_token ─────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  module[}('                                                                │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_039.md:1:8 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: header_expected_close_square ──────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  module[}('                                                                │
 │          ‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_039.md:1:9 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
KwModule,OpenSquare,CloseCurly,NoSpaceOpenRound,MalformedSingleQuote,
CloseRound,
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
