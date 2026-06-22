# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
0"
}
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_060.md:1:2:1:3
PARSE ERROR - fuzz_crash_060.md:1:1:1:2
PARSE ERROR - fuzz_crash_060.md:1:2:1:3
PARSE ERROR - fuzz_crash_060.md:1:3:1:3
PARSE ERROR - fuzz_crash_060.md:1:3:1:3
PARSE ERROR - fuzz_crash_060.md:2:1:2:2
# PROBLEMS
                                                             ┌─────────────────┐
┌─ This string is missing a closing quote. ──────────────────┤ UNCLOSED STRING │
│                                                            └────────────────┬┘
│                                                                             │
│  0"                                                                         │
│   ‾                                                                         │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_060.md:1:2

                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  0"                                                                         │
│  ‾                                                                          │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_060.md:1:1

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  0"                                                                         │
│   ‾                                                                         │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_060.md:1:2

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  0"                                                                         │
│    ‾                                                                        │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_060.md:1:3

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  0"                                                                         │
│    ‾                                                                        │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_060.md:1:3

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  }                                                                          │
│  ‾                                                                          │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_060.md:2:1

    This is an unexpected parsing error. Please check your syntax.
# TOKENS
~~~zig
Int,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
