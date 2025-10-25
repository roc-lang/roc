# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
main! = |_| True.not()
~~~
# EXPECTED
UNDEFINED VARIABLE - builtins.md:1:13:1:21
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `not` in this scope.
Is there an `import` or `exposing` missing up-top?

**builtins.md:1:13:1:21:**
```roc
main! = |_| True.not()
```
            ^^^^^^^^


# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "True.not")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "_arg -> Error"))))
~~~
