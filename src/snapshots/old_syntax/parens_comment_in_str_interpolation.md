# META
~~~ini
description=parens_comment_in_str_interpolation
type=expr
~~~
# SOURCE
~~~roc
"${(S#
)}"
~~~
~~~
# EXPECTED
PARSE ERROR - parens_comment_in_str_interpolation.md:2:1:2:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**parens_comment_in_str_interpolation.md:2:1:2:3:**
```roc
)}"
```
^^


**INVALID INTERPOLATION**
This string interpolation is not valid.
String interpolation should use the format: "text $(expression) more text"

# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:2),OpenStringInterpolation(1:2-1:4),NoSpaceOpenRound(1:4-1:5),UpperIdent(1:5-1:6),Newline(1:7-1:7),
CloseRound(2:1-2:2),CloseStringInterpolation(2:2-2:3),StringPart(2:3-2:3),StringEnd(2:3-2:4),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-2.4
	(e-string-part @1.2-1.2 (raw ""))
	(e-malformed @2.1-2.3 (reason "expected_expr_close_round_or_comma"))
	(e-string-part @2.3-2.3 (raw "")))
~~~
# FORMATTED
~~~roc
"${
	
}"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-2.4
	(e-literal @1.2-1.2 (string ""))
	(e-runtime-error (tag "invalid_string_interpolation"))
	(e-literal @2.3-2.3 (string "")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "Str"))
~~~
