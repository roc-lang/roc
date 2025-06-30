# META
~~~ini
description=multiline_str_pnc_apply_in_assignment_record_access_newline
type=expr
~~~
# SOURCE
~~~roc
i=""""""().1
p
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:2-1:3),MultilineStringStart(1:3-1:6),StringPart(1:6-1:6),MultilineStringEnd(1:6-1:9),NoSpaceOpenRound(1:9-1:10),CloseRound(1:10-1:11),NoSpaceDotInt(1:11-1:13),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
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
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
