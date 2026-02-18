# META
~~~ini
description=Test str.to_utf8() method call syntax
type=snippet
~~~
# SOURCE
~~~roc
bytes : List(U8)
bytes = "hello".to_utf8()

expect bytes == [104, 101, 108, 108, 111]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
KwExpect,LowerIdent,OpEquals,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "bytes")
			(ty-apply
				(ty (name "List"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "bytes"))
			(e-field-access
				(e-string
					(e-string-part (raw "hello")))
				(e-apply
					(e-ident (raw "to_utf8")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "bytes"))
				(e-list
					(e-int (raw "104"))
					(e-int (raw "101"))
					(e-int (raw "108"))
					(e-int (raw "108"))
					(e-int (raw "111")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "bytes"))
		(e-dot-access (field "to_utf8")
			(receiver
				(e-string
					(e-literal (string "hello"))))
			(args))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "U8") (builtin)))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "bytes")))
			(e-list
				(elems
					(e-num (value "104"))
					(e-num (value "101"))
					(e-num (value "108"))
					(e-num (value "108"))
					(e-num (value "111")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(U8)")))
	(expressions
		(expr (type "List(U8)"))))
~~~
