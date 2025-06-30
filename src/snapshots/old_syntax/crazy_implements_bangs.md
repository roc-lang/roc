# META
~~~ini
description=crazy_implements_bangs
type=expr
~~~
# SOURCE
~~~roc
P:=p#
  implements[]
n
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColonEqual(1:2-1:4),LowerIdent(1:4-1:5),Newline(1:6-1:6),
KwImplements(2:3-2:13),OpenSquare(2:13-2:14),CloseSquare(2:14-2:15),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "P"))
~~~
# FORMATTED
~~~roc
P
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (ext-var 73) (name "P") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[P]*"))
~~~
