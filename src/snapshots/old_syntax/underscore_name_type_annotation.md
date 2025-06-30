# META
~~~ini
description=underscore_name_type_annotation fail
type=expr
~~~
# SOURCE
~~~roc
A
:_h
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:1-1:1),
OpColon(2:1-2:2),NamedUnderscore(2:2-2:4),EndOfFile(2:4-2:4),
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
(e-tag @1.1-1.2 (ext-var 73) (name "A") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[A]*"))
~~~
