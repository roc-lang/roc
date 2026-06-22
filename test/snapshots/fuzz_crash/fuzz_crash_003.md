# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
= "te
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_003.md:1:3:1:6
PARSE ERROR - fuzz_crash_003.md:1:1:1:2
PARSE ERROR - fuzz_crash_003.md:1:3:1:4
PARSE ERROR - fuzz_crash_003.md:1:4:1:6
PARSE ERROR - fuzz_crash_003.md:1:6:1:6
# PROBLEMS
                                                             ┌─────────────────┐
┌─ This string is missing a closing quote. ──────────────────┤ UNCLOSED STRING │
│                                                            └────────────────┬┘
│                                                                             │
│  = "te                                                                      │
│    ‾‾‾                                                                      │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_003.md:1:3

                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  = "te                                                                      │
│  ‾                                                                          │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_003.md:1:1

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  = "te                                                                      │
│    ‾                                                                        │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_003.md:1:3

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  = "te                                                                      │
│     ‾‾                                                                      │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_003.md:1:4

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  = "te                                                                      │
│       ‾                                                                     │
└─────────────────────────────────────────────────────────────────────────────┘
    fuzz_crash_003.md:1:6

    This is an unexpected parsing error. Please check your syntax.
# TOKENS
~~~zig
OpAssign,StringStart,StringPart,StringEnd,
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
