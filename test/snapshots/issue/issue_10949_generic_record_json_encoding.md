# META
~~~ini
description=A record built from unannotated lambda parameters derives an encoder when the call site supplies concrete field types
type=snippet
~~~
# SOURCE
~~~roc
# repro for https://github.com/roc-lang/roc/issues/10949
to_json = |a, b| Json.to_str({ a, b })

result = to_json("hi", {})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,OpenCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "to_json"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-apply
					(e-ident (raw "Json.to_str"))
					(e-record
						(field (field "a"))
						(field (field "b"))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "to_json"))
				(e-string
					(e-string-part (raw "hi")))
				(e-record)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "to_json"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-call (constraint-fn-var 259)
				(e-lookup-external
					(builtin))
				(e-record
					(fields
						(field (name "a")
							(e-lookup-local
								(p-assign (ident "a"))))
						(field (name "b")
							(e-lookup-local
								(p-assign (ident "b")))))))))
	(d-let
		(p-assign (ident "result"))
		(e-call (constraint-fn-var 284)
			(e-lookup-local
				(p-assign (ident "to_json")))
			(e-string
				(e-literal (string "hi")))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg, _arg2 -> Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "_arg, _arg2 -> Str"))
		(expr (type "Str"))))
~~~
