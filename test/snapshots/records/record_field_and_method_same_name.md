# META
~~~ini
description=Record fields and attached methods with the same name use disjoint syntax and IR
type=snippet
~~~
# SOURCE
~~~roc
Thing := { f : I64 -> I64 }.{
    f : Thing, I64 -> I64
    f = |_, value| value - 1
}

thing : Thing
thing = { f: |value| value + 1 }

from_field : I64
from_field = (thing.f)(10)

from_method : I64
from_method = thing.f(10)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,Comma,LowerIdent,OpBar,LowerIdent,OpBinaryMinus,Int,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Thing")
				(args))
			(ty-record
				(anno-record-field (name "f")
					(ty-fn
						(ty (name "I64"))
						(ty (name "I64")))))
			(associated
				(s-type-anno (name "f")
					(ty-fn
						(ty (name "Thing"))
						(ty (name "I64"))
						(ty (name "I64"))))
				(s-decl
					(p-ident (raw "f"))
					(e-lambda
						(args
							(p-underscore)
							(p-ident (raw "value")))
						(e-binop (op "-")
							(e-ident (raw "value"))
							(e-int (raw "1")))))))
		(s-type-anno (name "thing")
			(ty (name "Thing")))
		(s-decl
			(p-ident (raw "thing"))
			(e-record
				(field (field "f")
					(e-lambda
						(args
							(p-ident (raw "value")))
						(e-binop (op "+")
							(e-ident (raw "value"))
							(e-int (raw "1")))))))
		(s-type-anno (name "from_field")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "from_field"))
			(e-apply
				(e-tuple
					(e-field-access
						(e-ident (raw "thing"))
						(e-ident (raw "f"))))
				(e-int (raw "10"))))
		(s-type-anno (name "from_method")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "from_method"))
			(e-method-call (method ".f")
				(receiver
					(e-ident (raw "thing")))
				(args
					(e-int (raw "10")))))))
~~~
# FORMATTED
~~~roc
Thing := { f : I64 -> I64 }.{
	f : Thing, I64 -> I64
	f = |_, value| value - 1
}

thing : Thing
thing = { f: |value| value + 1 }

from_field : I64
from_field = (thing.f)(10)

from_method : I64
from_method = thing.f(10)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "record_field_and_method_same_name.Thing.f"))
		(e-lambda
			(args
				(p-underscore)
				(p-assign (ident "value")))
			(e-dispatch-call (method "minus") (constraint-fn-var 243)
				(receiver
					(e-lookup-local
						(p-assign (ident "value"))))
				(args
					(e-num (value "1")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Thing") (local))
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin)))))
	(d-let
		(p-assign (ident "thing"))
		(e-record
			(fields
				(field (name "f")
					(e-lambda
						(args
							(p-assign (ident "value")))
						(e-dispatch-call (method "plus") (constraint-fn-var 273)
							(receiver
								(e-lookup-local
									(p-assign (ident "value"))))
							(args
								(e-num (value "1"))))))))
		(annotation
			(ty-lookup (name "Thing") (local))))
	(d-let
		(p-assign (ident "from_field"))
		(e-call (constraint-fn-var 322)
			(e-field-access (field "f")
				(receiver
					(e-lookup-local
						(p-assign (ident "thing")))))
			(e-num (value "10")))
		(annotation
			(ty-lookup (name "I64") (builtin))))
	(d-let
		(p-assign (ident "from_method"))
		(e-dispatch-call (method "f") (constraint-fn-var 346)
			(receiver
				(e-lookup-local
					(p-assign (ident "thing"))))
			(args
				(e-num (value "10"))))
		(annotation
			(ty-lookup (name "I64") (builtin))))
	(s-nominal-decl
		(ty-header (name "Thing"))
		(ty-record
			(field (field "f")
				(ty-fn (effectful false)
					(ty-lookup (name "I64") (builtin))
					(ty-lookup (name "I64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Thing, I64 -> I64"))
		(patt (type "Thing"))
		(patt (type "I64"))
		(patt (type "I64")))
	(type_decls
		(nominal (type "Thing")
			(ty-header (name "Thing"))))
	(expressions
		(expr (type "Thing, I64 -> I64"))
		(expr (type "Thing"))
		(expr (type "I64"))
		(expr (type "I64"))))
~~~
