# META
~~~ini
description=Simple when expression
type=expr
~~~
# SOURCE
~~~roc
when x is
    Ok(value) -> value
    Err(msg) -> msg
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
UpperIdent(2:5-2:7),NoSpaceOpenRound(2:7-2:8),LowerIdent(2:8-2:13),CloseRound(2:13-2:14),OpArrow(2:15-2:17),LowerIdent(2:18-2:23),Newline(1:1-1:1),
UpperIdent(3:5-3:8),NoSpaceOpenRound(3:8-3:9),LowerIdent(3:9-3:12),CloseRound(3:12-3:13),OpArrow(3:14-3:16),LowerIdent(3:17-3:20),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
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
