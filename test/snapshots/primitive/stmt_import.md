# META
~~~ini
description=A primitive
type=snippet
~~~
# SOURCE
~~~roc
import json.Json [foo, BAR]
~~~
# EXPECTED
PARSE ERROR - stmt_import.md:1:18:1:19
PARSE ERROR - stmt_import.md:1:19:1:22
PARSE ERROR - stmt_import.md:1:22:1:23
PARSE ERROR - stmt_import.md:1:27:1:28
# PROBLEMS
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  import json.Json [foo, BAR]                                                │
│                   ‾                                                         │
└─────────────────────────────────────────────────────────────────────────────┘
    stmt_import.md:1:18

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  import json.Json [foo, BAR]                                                │
│                    ‾‾‾                                                      │
└─────────────────────────────────────────────────────────────────────────────┘
    stmt_import.md:1:19

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  import json.Json [foo, BAR]                                                │
│                       ‾                                                     │
└─────────────────────────────────────────────────────────────────────────────┘
    stmt_import.md:1:22

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ Type applications require parentheses around their type ──────┤ PARSE ERROR │
│  arguments.                                                    └────────────┬┘
│                                                                             │
│  import json.Json [foo, BAR]                                                │
│                            ‾                                                │
└─────────────────────────────────────────────────────────────────────────────┘
    stmt_import.md:1:27

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
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,OpenSquare,LowerIdent,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
import json.Json
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (module "json.Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
