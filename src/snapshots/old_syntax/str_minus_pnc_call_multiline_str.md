# META
~~~ini
description=str_minus_pnc_call_multiline_str
type=expr
~~~
# SOURCE
~~~roc
"" -""""""()
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:2),StringEnd(1:2-1:3),OpUnaryMinus(1:4-1:5),MultilineStringStart(1:5-1:8),StringPart(1:8-1:8),MultilineStringEnd(1:8-1:11),NoSpaceOpenRound(1:11-1:12),CloseRound(1:12-1:13),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.3
	(e-string-part @1.2-1.2 (raw "")))
~~~
# FORMATTED
~~~roc
""
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.3
	(e-literal @1.2-1.2 (string "")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Str"))
~~~
