# META
~~~ini
description=Dot access expression
type=expr
~~~
# SOURCE
~~~roc
list.map(fn)
~~~
# EXPECTED
UNDEFINED VARIABLE - can_dot_access.md:1:1:1:5
UNDEFINED VARIABLE - can_dot_access.md:1:10:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `list` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_dot_access.md:1:1:1:5:**
```roc
list.map(fn)
```
^^^^


**UNDEFINED VARIABLE**
Nothing is named `fn` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_dot_access.md:1:10:1:12:**
```roc
list.map(fn)
```
         ^^


# TOKENS
~~~zig
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-field-access
	(e-ident (raw "list"))
	(e-apply
		(e-ident (raw "map"))
		(e-ident (raw "fn"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access (field "map")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope")))
	(args
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
