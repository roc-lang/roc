# META
~~~ini
description=nested_when_comment_in_pat
type=expr
~~~
# SOURCE
~~~roc
when 6 is
O#
 B->when 6 is
1->O
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
UpperIdent(2:1-2:2),Newline(2:3-2:3),
UpperIdent(3:2-3:3),OpArrow(3:3-3:5),LowerIdent(3:5-3:9),Int(3:10-3:11),LowerIdent(3:12-3:14),Newline(1:1-1:1),
Int(4:1-4:2),OpArrow(4:2-4:4),UpperIdent(4:4-4:5),EndOfFile(4:5-4:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (qaul "") (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
