# META
~~~ini
description=pattern_with_as_parens
type=expr
~~~
# SOURCE
~~~roc
when t is
    Ok ({} as d)->S
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
UpperIdent(2:5-2:7),OpenRound(2:8-2:9),OpenCurly(2:9-2:10),CloseCurly(2:10-2:11),KwAs(2:12-2:14),LowerIdent(2:15-2:16),CloseRound(2:16-2:17),OpArrow(2:17-2:19),UpperIdent(2:19-2:20),EndOfFile(2:20-2:20),
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
