# META
~~~ini
description=body_block_string_apply_string
type=expr
~~~
# SOURCE
~~~roc
t="""" """""
S
~~~
# EXPECTED
UNDEFINED VARIABLE - body_block_string_apply_string.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:2-1:3),MultilineStringStart(1:3-1:6),StringPart(1:6-1:8),MultilineStringEnd(1:8-1:11),StringStart(1:11-1:12),StringPart(1:12-1:12),StringEnd(1:12-1:13),Newline(1:1-1:1),
UpperIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "t"))
~~~
# FORMATTED
~~~roc
t
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
