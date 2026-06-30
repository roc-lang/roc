# META
~~~ini
description=to_str should accept Str
type=snippet
~~~
# SOURCE
~~~roc
function : a -> Str where [ a.to_str : a -> Str ]
function = |convertible| {
    convertible.to_str()
}

value = "my string"

_ = function(value)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "function")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(method (module-of "a") (name "to_str")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "function"))
			(e-lambda
				(args
					(p-ident (raw "convertible")))
				(e-block
					(statements
						(e-method-call (method ".to_str")
							(receiver
								(e-ident (raw "convertible")))
							(args))))))
		(s-decl
			(p-ident (raw "value"))
			(e-string
				(e-string-part (raw "my string"))))
		(s-decl
			(p-underscore)
			(e-apply
				(e-ident (raw "function"))
				(e-ident (raw "value"))))))
~~~
# FORMATTED
~~~roc
function : a -> Str where [a.to_str : a -> Str]
function = |convertible| {
	convertible.to_str()
}

value = "my string"

_ = function(value)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "function"))
		(e-lambda
			(args
				(p-assign (ident "convertible")))
			(e-block
				(e-dispatch-call (method "to_str") (constraint-fn-var 51)
					(receiver
						(e-lookup-local
							(p-assign (ident "convertible"))))
					(args))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "value"))
		(e-string
			(e-literal (string "my string"))))
	(d-let
		(p-underscore)
		(e-call (constraint-fn-var 72)
			(e-lookup-local
				(p-assign (ident "function")))
			(e-lookup-local
				(p-assign (ident "value"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Str where [a.to_str : a -> Str]"))
		(patt (type "Str")))
	(expressions
		(expr (type "a -> Str where [a.to_str : a -> Str]"))
		(expr (type "Str"))
		(expr (type "Str"))))
~~~
