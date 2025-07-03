# META
~~~ini
description=pnc_apply_comment_after_newline
type=expr
~~~
# SOURCE
~~~roc
i(i,
)t
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),NoSpaceOpenRound(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),Newline(1:1-1:1),
CloseRound(2:1-2:2),LowerIdent(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-apply @1.1-2.2
	(e-ident @1.1-1.2 (qaul "") (raw "i"))
	(e-ident @1.3-1.4 (qaul "") (raw "i")))
~~~
# FORMATTED
~~~roc
i(
	i,
)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-2.2
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "a"))
~~~
