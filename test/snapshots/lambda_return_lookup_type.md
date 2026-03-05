# META
~~~ini
description=Lambda return type should match variable lookup type, not Bool
type=mono
~~~
# SOURCE
~~~roc
x = ["hello"]
f = |_| x
y = f(0)
~~~
# MONO
~~~roc
x : List(Str)
x = ["hello"]

f : _arg -> List(Str)
f = |_| x

y : List(Str)
y = f(0)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenSquare,StringStart,StringPart,StringEnd,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-list
				(e-string
					(e-string-part (raw "hello")))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-underscore))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "y"))
			(e-apply
				(e-ident (raw "f"))
				(e-int (raw "0"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-list
			(elems
				(e-string
					(e-literal (string "hello"))))))
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-underscore))
			(e-lookup-local
				(p-assign (ident "x")))))
	(d-let
		(p-assign (ident "y"))
		(e-call
			(e-lookup-local
				(p-assign (ident "f")))
			(e-num (value "0")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Str)"))
		(patt (type "_arg -> List(Str)"))
		(patt (type "List(Str)")))
	(expressions
		(expr (type "List(Str)"))
		(expr (type "_arg -> List(Str)"))
		(expr (type "List(Str)"))))
~~~
