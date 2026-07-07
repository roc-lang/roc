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
EXPECTED EXPOSED NAME - fuzz_crash_039.md:1:8:1:9
EXPECTED CLOSING BRACKET - fuzz_crash_039.md:1:9:1:10
# PROBLEMS

┌───────────────────────┐
│ UNCLOSED SINGLE QUOTE ├─ This single-quoted literal is missing a closing ───┐
└┬──────────────────────┘  quote.                                             │
 │                                                                            │
 │  module[}('                                                                │
 │           ‾                                                                │
 └──────────────────────────────────────────────────── fuzz_crash_039.md:1:10 ┘



┌───────────────────────┐
│ EXPECTED EXPOSED NAME ├─ I was parsing an exposing list, and I expected ────┐
└┬──────────────────────┘  an exposed name.                                   │
 │                                                                            │
 │  module[}('                                                                │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_039.md:1:8 ┘

    Exposing lists contain lowercase values, uppercase types or tags, and
    `Type.*` entries.

    For example:
        module [main, Result, Result.*]

    I found `}` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────────┐
│ EXPECTED CLOSING BRACKET ├─ I was parsing a header exposing list, and I ────┐
└┬─────────────────────────┘  expected a closing `]`.                         │
 │                                                                            │
 │  module[}('                                                                │
 │          ‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_039.md:1:9 ┘

    Close the list after the final exposed name.

    For example:
        module [main, helper]

    I found `(` here.

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
