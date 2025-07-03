# META
~~~ini
description=newline_in_type_alias_application
type=expr
~~~
# SOURCE
~~~roc
A:A
 A
p
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:1-1:1),
UpperIdent(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "A"))
~~~
# FORMATTED
~~~roc
A
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "A") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[A]a"))
~~~
