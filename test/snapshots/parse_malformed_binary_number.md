# META
~~~ini
description=Malformed binary number (0b without digits)
type=expr
~~~
# SOURCE
~~~roc
0b
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - parse_malformed_binary_number.md:1:1:1:3
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
