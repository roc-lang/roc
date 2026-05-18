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
DOES NOT EXIST - builtins.md:1:13:1:21
# PROBLEMS
**DOES NOT EXIST**
`True.not` does not exist.

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
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-call
				(e-runtime-error (tag "qualified_ident_does_not_exist"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "_arg -> Error"))))
~~~
