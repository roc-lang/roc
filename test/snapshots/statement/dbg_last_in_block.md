# META
~~~ini
description=Debug as last expression in block should return {}
type=snippet
~~~
# SOURCE
~~~roc
main = || {
    dbg "hello"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
KwDbg,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "main"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-dbg
							(e-string
								(e-string-part (raw "hello"))))))))))
~~~
# FORMATTED
~~~roc
main = || {
	dbg "hello"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-lambda
			(args)
			(e-block
				(e-dbg
					(e-string
						(e-literal (string "hello"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) => {}")))
	(expressions
		(expr (type "({}) => {}"))))
~~~
