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
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,LowerIdent,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-ident (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-lookup-local
	(p-assign (ident "when")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
