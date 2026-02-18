# META
~~~ini
description=Debug as function argument
type=snippet
~~~
# SOURCE
~~~roc
foo = |f| f(dbg 42)
bar = |f| f(dbg(42))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,KwDbg,Int,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,KwDbg,NoSpaceOpenRound,Int,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-apply
					(e-ident (raw "f"))
					(e-dbg
						(e-int (raw "42"))))))
		(s-decl
			(p-ident (raw "bar"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-apply
					(e-ident (raw "f"))
					(e-dbg
						(e-tuple
							(e-int (raw "42")))))))))
~~~
# FORMATTED
~~~roc
foo = |f| f(dbg 42)
bar = |f| f(dbg (42))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-call
				(e-lookup-local
					(p-assign (ident "f")))
				(e-dbg
					(e-num (value "42"))))))
	(d-let
		(p-assign (ident "bar"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-call
				(e-lookup-local
					(p-assign (ident "f")))
				(e-dbg
					(e-num (value "42")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({} -> a) => a"))
		(patt (type "({} -> a) => a")))
	(expressions
		(expr (type "({} -> a) => a"))
		(expr (type "({} -> a) => a"))))
~~~
