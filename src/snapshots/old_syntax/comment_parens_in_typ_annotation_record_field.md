# META
~~~ini
description=comment_parens_in_typ_annotation_record_field
type=expr
~~~
# SOURCE
~~~roc
i:{t:(J#
)}
A
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),OpenCurly(1:3-1:4),LowerIdent(1:4-1:5),OpColon(1:5-1:6),NoSpaceOpenRound(1:6-1:7),UpperIdent(1:7-1:8),Newline(1:9-1:9),
CloseRound(2:1-2:2),CloseCurly(2:2-2:3),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "i"))
~~~
# FORMATTED
~~~roc
i
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
