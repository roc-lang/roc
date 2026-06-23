# META
~~~ini
description=An empty module with multiline exposes and comments
type=snippet
~~~
# SOURCE
~~~roc
	[ # Comment After exposes open
		something, # Comment after exposed item
		SomeType, # Comment after final exposed item
	]
~~~
# EXPECTED
PARSE ERROR - module_multiline_with_comments.md:1:2:1:3
PARSE ERROR - module_multiline_with_comments.md:2:3:2:12
PARSE ERROR - module_multiline_with_comments.md:2:12:2:13
PARSE ERROR - module_multiline_with_comments.md:3:11:3:12
PARSE ERROR - module_multiline_with_comments.md:4:2:4:3
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  [ # Comment After exposes open                                            │
 │  ‾                                                                         │
 └───────────────────────────────────── module_multiline_with_comments.md:1:2 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  something, # Comment after exposed item                                   │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └───────────────────────────────────── module_multiline_with_comments.md:2:3 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  something, # Comment after exposed item                                   │
 │           ‾                                                                │
 └──────────────────────────────────── module_multiline_with_comments.md:2:12 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  SomeType, # Comment after final exposed item                              │
 │          ‾                                                                 │
 └──────────────────────────────────── module_multiline_with_comments.md:3:11 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ]                                                                         │
 │  ‾                                                                         │
 └───────────────────────────────────── module_multiline_with_comments.md:4:2 ┘

    This is an unexpected parsing error. Please check your syntax.

# TOKENS
~~~zig
OpenSquare,
LowerIdent,Comma,
UpperIdent,Comma,
CloseSquare,
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
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
# Comment After exposes open
# Comment after final exposed item
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
