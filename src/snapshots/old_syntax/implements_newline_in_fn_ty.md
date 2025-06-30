# META
~~~ini
description=implements_newline_in_fn_ty
type=expr
~~~
# SOURCE
~~~roc
A implements q:U
 e->p
d
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),KwImplements(1:3-1:13),LowerIdent(1:14-1:15),OpColon(1:15-1:16),UpperIdent(1:16-1:17),Newline(1:1-1:1),
LowerIdent(2:2-2:3),OpArrow(2:3-2:5),LowerIdent(2:5-2:6),Newline(1:1-1:1),
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
(e-tag @1.1-1.2 (ext-var 73) (name "A") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[A]*"))
~~~
