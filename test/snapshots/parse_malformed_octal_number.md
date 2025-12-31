# META
~~~ini
description=Malformed octal number (0o without digits)
type=expr
~~~
# SOURCE
~~~roc
0o
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - parse_malformed_octal_number.md:1:1:1:3
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
