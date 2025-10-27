# META
~~~ini
description=the int test platform
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires {} { multiplyInts : I64, I64 -> I64 }
    exposes []
    packages {}
    provides { multiplyInts: "multiplyInts" }

multiplyInts : I64, I64 -> I64
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(rigids)
		(ty-record
			(anno-record-field (name "multiplyInts")
				(ty-fn
					(ty (name "I64"))
					(ty (name "I64"))
					(ty (name "I64")))))
		(exposes)
		(packages)
		(provides
			(record-field (name "multiplyInts")
				(e-string
					(e-string-part (raw "multiplyInts"))))))
	(statements
		(s-type-anno (name "multiplyInts")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))
				(ty (name "I64"))))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {} { multiplyInts : I64, I64 -> I64 }
	exposes []
	packages {}
	provides { multiplyInts: "multiplyInts" }

multiplyInts : I64, I64 -> I64
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "multiplyInts"))
		(e-not-implemented)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin)))))
	(s-let
		(p-assign (ident "multiplyInts"))
		(e-not-implemented)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Signed64)), Num(Int(Signed64)) -> Num(Int(Signed64))")))
	(expressions
		(expr (type "Num(Int(Signed64)), Num(Int(Signed64)) -> Num(Int(Signed64))"))))
~~~
