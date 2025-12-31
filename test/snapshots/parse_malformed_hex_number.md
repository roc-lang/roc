# META
~~~ini
description=Malformed hex number (0x without digits)
type=expr
~~~
# SOURCE
~~~roc
0x
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - parse_malformed_hex_number.md:1:1:1:3
# PROBLEMS
NIL
# TOKENS
~~~zig
MalformedNumberNoDigits,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed)
~~~
