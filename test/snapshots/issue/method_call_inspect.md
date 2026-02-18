# META
~~~ini
description=Method call syntax with .inspect() should produce e_dot_access with args
type=expr
~~~
# SOURCE
~~~roc
x.inspect()
~~~
# EXPECTED
UNDEFINED VARIABLE - method_call_inspect.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**method_call_inspect.md:1:1:1:2:**
```roc
x.inspect()
```
^


# TOKENS
~~~zig
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-field-access
	(e-ident (raw "x"))
	(e-apply
		(e-ident (raw "inspect"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access (field "inspect")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope")))
	(args))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
