# META
~~~ini
description=For expression stmt
type=statement
~~~
# SOURCE
~~~roc
for x in ["a", "b", "c"] {
  result = result + x
} 
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwFor,LowerIdent,KwIn,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-for
	(p-ident (raw "x"))
	(e-list
		(e-string
			(e-string-part (raw "a")))
		(e-string
			(e-string-part (raw "b")))
		(e-string
			(e-string-part (raw "c"))))
	(e-block
		(statements
			(s-decl
				(p-ident (raw "result"))
				(e-binop (op "+")
					(e-ident (raw "result"))
					(e-ident (raw "x")))))))
~~~
# FORMATTED
~~~roc
for x in ["a", "b", "c"] {
	result = result + x
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-for
		(p-assign (ident "x"))
		(e-list
			(elems
				(e-string
					(e-literal (string "a")))
				(e-string
					(e-literal (string "b")))
				(e-string
					(e-literal (string "c")))))
		(e-block
			(s-let
				(p-assign (ident "result"))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "result")))
					(e-lookup-local
						(p-assign (ident "x")))))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
