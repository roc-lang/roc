# META
~~~ini
description=Unset an optional field with `name: _` in a record update and a record construction
type=snippet
~~~
# SOURCE
~~~roc
b : { opt ?: Str }
b = { opt: "hello!" }

c = { ..b, opt: _ }

d = { opt: _, other: 1 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpQuestion,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,Underscore,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Underscore,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "b")
			(ty-record
				(anno-record-field (name "opt") (optional true)
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "b"))
			(e-record
				(field (field "opt")
					(e-string
						(e-string-part (raw "hello!"))))))
		(s-decl
			(p-ident (raw "c"))
			(e-record
				(ext
					(e-ident (raw "b")))
				(field (field "opt") (unset true))))
		(s-decl
			(p-ident (raw "d"))
			(e-record
				(field (field "opt") (unset true))
				(field (field "other")
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "b"))
		(e-record
			(fields
				(field (name "opt")
					(e-string
						(e-literal (string "hello!"))))))
		(annotation
			(ty-record
				(field (field "opt") (optional true)
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "c"))
		(e-record
			(ext
				(e-lookup-local
					(p-assign (ident "b"))))
			(fields)
			(unsets
				(unset-field (name "opt")))))
	(d-let
		(p-assign (ident "d"))
		(e-record
			(fields
				(field (name "other")
					(e-num (value "1"))))
			(unsets
				(unset-field (name "opt"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ opt ?: Str }"))
		(patt (type "{ opt ?: Str }"))
		(patt (type "{ other: Dec }")))
	(expressions
		(expr (type "{ opt ?: Str }"))
		(expr (type "{ opt ?: Str }"))
		(expr (type "{ other: Dec }"))))
~~~
