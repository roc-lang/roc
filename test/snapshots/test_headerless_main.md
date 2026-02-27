# META
~~~ini
description=Headerless file with main function
type=file
~~~
# SOURCE
~~~roc
x = 5
main! = |_| x
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-ident (raw "x"))))))
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
				(p-underscore)))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-underscore))))))
	(d-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "x")))
			(e-lambda
				(args
					(p-underscore))
				(e-lookup-local
					(p-assign (ident "x")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => Try({  }, [..])"))
		(patt (type "Dec"))
		(patt (type "_arg -> Dec")))
	(expressions
		(expr (type "Str => Try({  }, [..])"))
		(expr (type "Dec"))
		(expr (type "_arg -> Dec"))))
~~~
