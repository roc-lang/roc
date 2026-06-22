# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
Fli/main.roc" }

Pair(a, b+ : (
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_021.md:1:13:1:16
PARSE ERROR - fuzz_crash_021.md:1:4:1:5
PARSE ERROR - fuzz_crash_021.md:1:5:1:9
PARSE ERROR - fuzz_crash_021.md:1:9:1:13
PARSE ERROR - fuzz_crash_021.md:1:13:1:14
PARSE ERROR - fuzz_crash_021.md:1:14:1:16
PARSE ERROR - fuzz_crash_021.md:1:16:1:16
PARSE ERROR - fuzz_crash_021.md:3:1:3:5
# PROBLEMS
                                                             ┌─────────────────┐
┌─ This string is missing a closing quote. ──────────────────┤ UNCLOSED STRING │
│                                                            └────────────────┬┘
│                                                                             │
│  Fli/main.roc" }                                                            │
│              ‾‾‾                                                            │
└───────────────────────────────────────────────────── fuzz_crash_021.md:1:13 ┘

                                                                 ┌─────────────┐
┌─ Type applications require parentheses around their type ──────┤ PARSE ERROR │
│  arguments.                                                    └────────────┬┘
│                                                                             │
│  Fli/main.roc" }                                                            │
│     ‾                                                                       │
└────────────────────────────────────────────────────── fuzz_crash_021.md:1:4 ┘

    I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  Fli/main.roc" }                                                            │
│      ‾‾‾‾                                                                   │
└────────────────────────────────────────────────────── fuzz_crash_021.md:1:5 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  Fli/main.roc" }                                                            │
│          ‾‾‾‾                                                               │
└────────────────────────────────────────────────────── fuzz_crash_021.md:1:9 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  Fli/main.roc" }                                                            │
│              ‾                                                              │
└───────────────────────────────────────────────────── fuzz_crash_021.md:1:13 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  Fli/main.roc" }                                                            │
│               ‾‾                                                            │
└───────────────────────────────────────────────────── fuzz_crash_021.md:1:14 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  Fli/main.roc" }                                                            │
│                 ‾                                                           │
└───────────────────────────────────────────────────── fuzz_crash_021.md:1:16 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: ────────────────────────────────────┤ PARSE ERROR │
│  expected_ty_anno_close_round_or_comma                         └────────────┬┘
│                                                                             │
│  Pair(a, b+ : (                                                             │
│  ‾‾‾‾                                                                       │
└────────────────────────────────────────────────────── fuzz_crash_021.md:3:1 ┘

    This is an unexpected parsing error. Please check your syntax.
# TOKENS
~~~zig
UpperIdent,OpSlash,LowerIdent,NoSpaceDotLowerIdent,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,OpPlus,OpColon,OpenRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_ty_anno_close_round_or_comma"))))
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
