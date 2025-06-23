# META
~~~ini
description=Dot access expression
type=expr
~~~
# SOURCE
~~~roc
list.map(fn)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `list` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `fn` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),NoSpaceDotLowerIdent(1:5-1:9),NoSpaceOpenRound(1:9-1:10),LowerIdent(1:10-1:12),CloseRound(1:12-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(field_access (1:1-1:13)
	(binop (1:1-1:13)
		"list"
		(ident (1:1-1:5) "" "list")
		(apply (1:5-1:13)
			(ident (1:5-1:9) "" ".map")
			(ident (1:10-1:12) "" "fn"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_dot_access (1:1-1:13)
	(e_runtime_error (1:1-1:5) "ident_not_in_scope")
	"map"
	(e_runtime_error (1:10-1:12) "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr 76 (type "*"))
~~~